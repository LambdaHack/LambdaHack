-- | The FFI-coverage battery: each @foreign import javascript@ the wasm
-- build reaches is called at least once with a known input and checked
-- against a known output (@docs/wasm-frontend-unified-plan.md@, 0.3).
--
-- The file-backend cases go through "Game.LambdaHack.Common.File"'s
-- exports, so they run over @WasmFile.hs@ under wasm, against the
-- @localStorage@ stub and the real lz-string that @ts-src/run-wasm-test.mjs@
-- installs, and natively over @HSFile.hs@ in a scratch directory, so that
-- a drift between the backends fails @cabal test@ or @make test-wasm@.
-- The frame and overlay cases are wasm-only: they drive @Wasm.hs@'s
-- @js_submitFrame@ and @js_submitOverlays@ through 'Wasm.startup'
-- and 'fdisplay' and read back what the driver's @lhSubmitFrame@
-- and @lhSubmitOverlays@ stubs recorded. The reactor's exports, which this
-- command binary cannot exercise, are @ts-src/run-wasm-game.mjs@'s to cover.
module FfiCoverageUnitTests (ffiCoverageUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Exception as Ex
import qualified Data.Text as T
import           Data.Version (Version, makeVersion)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Game.LambdaHack.Common.File as File

#ifdef USE_WASMFILE
import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Text.Encoding (decodeLatin1)
import           GHC.Wasm.Prim (fromJSString, toJSString)
#else
import qualified Data.ByteString.Char8 as BS
import           System.Directory
  (createDirectory, getTemporaryDirectory, removeDirectoryRecursive)
import           System.FilePath ((</>))
import           System.IO.Error (isAlreadyExistsError)
#endif

#ifdef USE_WASM
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend
  (encodeFrameLayout, layOutFrame)
import           Game.LambdaHack.Client.UI.Frontend.Common (RawFrontend (..))
import qualified Game.LambdaHack.Client.UI.Frontend.Wasm as Wasm
import           Game.LambdaHack.Client.UI.PointUI (PointUI (..))
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Definition.Color as Color

import UnitTestHelpers (multiFontClientOptions)
#endif

ffiCoverageUnitTests :: TestTree
ffiCoverageUnitTests = testGroup "ffiCoverageUnitTests" $
  [ testCase "encodeEOF then strictDecodeEOF returns the value saved" $
      withScratch $ \path -> do
        File.encodeEOF (path "roundTrip") version payload
        (v, p) <- File.strictDecodeEOF (path "roundTrip")
        v @?= version
        p @?= payload
  , testCase "doesFileExist is False before a save and True after" $
      withScratch $ \path -> do
        File.doesFileExist (path "exists") >>= (@?= False)
        File.encodeEOF (path "exists") version payload
        File.doesFileExist (path "exists") >>= (@?= True)
  , testCase "renameFile moves the contents and removes the old name" $
      withScratch $ \path -> do
        File.tryWriteFile (path "renameFrom") $ T.pack contents
        File.renameFile (path "renameFrom") (path "renameTo")
        File.readFile (path "renameTo") >>= (@?= contents)
        File.doesFileExist (path "renameFrom") >>= (@?= False)
  , testCase "tryWriteFile returns from a refused write, writing nothing" $
      withScratch $ \path -> do
        File.tryWriteFile (refused path) $ T.pack contents
        File.doesFileExist (refused path) >>= (@?= False)
  , testCase "strictDecodeEOF rejects a file whose EOF marker is damaged" $
      withScratch $ \path -> do
        File.encodeEOF (path "damageSource") version payload
        damageMarker (path "damageSource") (path "damaged")
        -- The undamaged original still decodes, so the rejection below
        -- is the marker's alone.
        (_, p) <- File.strictDecodeEOF (path "damageSource")
        p @?= payload
        r <- Ex.try (File.strictDecodeEOF (path "damaged")
                       :: IO (Version, Payload))
        case r of
          Left (e :: Ex.SomeException) ->
            assertBool ("rejected as corrupted, not otherwise: " ++ show e) $
              "corrupted file" `isInfixOf` show e
          Right _ -> assertFailure "a damaged EOF marker was decoded"
  ]
  ++ wasmFileTests
  ++ frontendTests

-- | The stored form of a save, and none natively: under wasm a save is
-- stored as the string its 'String' path stored, so that a save one build
-- makes another loads.
wasmFileTests :: [TestTree]
#ifdef USE_WASMFILE
wasmFileTests =
  [ testCase "a save is stored as the String path stored it" $
      withScratch $ \path -> do
        File.encodeEOF (path "stored") version payload
        stored <- File.readFile (path "stored")
        let t = decodeLatin1 $ LBS.toStrict
                $ encode (version, (encode payload, "OK" :: String))
        expected <- File.compressToUTF16 $ toJSString $ T.unpack t
        stored @?= fromJSString expected
  ]
#else
wasmFileTests = []
#endif

-- | The frame and overlay cases, and none natively, where no wasm frontend
-- is linked. Kept a binding of its own, as @Spec.hs@ keeps
-- @sdlFontsetTests@, so that the argument of 'testGroup' is the same shape
-- in both builds.
frontendTests :: [TestTree]
#ifdef USE_WASM
frontendTests = [submitFrameTest, submitOverlaysTest]
#else
frontendTests = []
#endif

type Payload = (Int, String, [Int])

-- | A save with a character outside Latin-1, whose encoded bytes
-- @WasmFile.hs@ carries as Latin-1 characters before compressing.
payload :: Payload
payload = (42, "save \x1F600 here", [1 .. 100])

version :: Version
version = makeVersion [0, 3]

contents :: String
contents = "renamed contents"

-- | Run a case's file operations in a fresh scratch space, naming each file
-- through the function given: natively a temporary directory, removed
-- afterwards; under wasm a key prefix, the backend there being the
-- driver's in-memory @localStorage@, so each case names its files apart.
withScratch :: ((String -> FilePath) -> IO a) -> IO a
#ifdef USE_WASMFILE
withScratch k = k ("ffiCoverage/" ++)
#else
withScratch k = do
  tmp <- getTemporaryDirectory
  Ex.bracket (fresh tmp (0 :: Int)) removeDirectoryRecursive (k . (</>))
 where
  fresh tmp n = do
    let dir = tmp </> ("LambdaHack-ffiCoverage-" ++ show n)
    r <- Ex.try (createDirectory dir) :: IO (Either Ex.IOException ())
    case r of
      Right () -> return dir
      Left e | isAlreadyExistsError e -> fresh tmp (n + 1)
             | otherwise -> Ex.throwIO e
#endif

-- | A file name the backend refuses to write: natively one in a directory
-- that does not exist; under wasm a key under the prefix the driver's
-- @localStorage@ refuses with the error a full browser storage throws.
refused :: (String -> FilePath) -> FilePath
#ifdef USE_WASMFILE
refused path = "quotaExceeded/" ++ path "refused"
#else
refused path = path "absent" </> "refused"
#endif

-- | Copy an 'File.encodeEOF' save to a new name with the last character of
-- its @OK@ EOF marker changed, so that the copy decodes structurally and
-- only the marker check can reject it.
damageMarker :: FilePath -> FilePath -> IO ()
#ifdef USE_WASMFILE
damageMarker from to = do
  raw <- File.readFile from
  plain <- fromJSString <$> File.decompressFromUTF16 (toJSString raw)
  case reverse plain of
    'K' : rest -> do
      damaged <- File.compressToUTF16 $ toJSString $ reverse $ 'X' : rest
      -- Under wasm 'File.tryWriteFile' stores its text as the item itself.
      File.tryWriteFile to $ T.pack $ fromJSString damaged
    _ -> assertFailure "the save does not end in the EOF marker"
#else
damageMarker from to = do
  bytes <- BS.readFile from
  case BS.unsnoc bytes of
    Just (rest, 'K') -> BS.writeFile to $ BS.snoc rest 'X'
    _ -> assertFailure "the save does not end in the EOF marker"
#endif

#ifdef USE_WASM
-- | @Wasm.hs@'s @js_submitFrame@, reached through 'Wasm.startup' and
-- 'fdisplay': the driver's @lhSubmitFrame@ stub records one call per frame,
-- with the screen's width and height and the cell word it reads at the
-- address given, which matches the fixture only at the frame's first cell.
submitFrameTest :: TestTree
submitFrameTest =
  testCase "js_submitFrame passes each frame's address and size, once" $ do
    let coscreen = emptyScreenContent {rwidth = 7, rheight = 3}
        cell = Color.attrChar2ToW32 Color.BrRed
        frameOf c =
          let blank = blankSingleFrame coscreen
          in blank {singleArray =
                      singleArray blank PointArray.// [(Point 0 0, cell c)]}
    rf <- startupUnderTest coscreen
    n <- js_frameCount
    fdisplay rf $ frameOf 'A'
    fdisplay rf $ frameOf 'B'
    js_frameCount >>= (@?= n + 2)
    forM_ (zip [n ..] "AB") $ \(i, c) -> do
      js_frameWidth i >>= (@?= 7)
      js_frameHeight i >>= (@?= 3)
      js_frameFirstCell i >>= (@?= fromEnum (Color.attrCharW32 $ cell c))

-- | The frontend the frame and overlay cases run on, on options whose fontset
-- resolves, as 'Wasm.startup' requires.
startupUnderTest :: ScreenContent -> IO RawFrontend
startupUnderTest coscreen = Wasm.startup coscreen multiFontClientOptions

-- | What the driver's @lhSubmitFrame@ stub recorded, call by call.
foreign import javascript unsafe "globalThis.lhSubmitFrameCalls.length"
  js_frameCount :: IO Int

foreign import javascript unsafe "globalThis.lhSubmitFrameCalls[$1].w"
  js_frameWidth :: Int -> IO Int

foreign import javascript unsafe "globalThis.lhSubmitFrameCalls[$1].h"
  js_frameHeight :: Int -> IO Int

foreign import javascript unsafe "globalThis.lhSubmitFrameCalls[$1].first"
  js_frameFirstCell :: Int -> IO Int

-- | @Wasm.hs@'s @js_submitOverlays@, reached as the frame case reaches
-- @js_submitFrame@: the driver's @lhSubmitOverlays@ stub records no call
-- for a frame without overlays and one for a frame with them, made before
-- that frame's, with the screen's width and the words it reads
-- at the address given, which are those 'encodeFrameLayout' writes.
submitOverlaysTest :: TestTree
submitOverlaysTest =
  testCase "js_submitOverlays passes a frame's overlays, before the frame" $ do
    let coscreen = emptyScreenContent {rwidth = 7, rheight = 3}
        blank = blankSingleFrame coscreen
        line = map $ Color.attrChar2ToW32 Color.BrRed
        overlaid = blank { singlePropOverlay = [(PointUI 2 1, line "ab")]
                         , singleSquareOverlay = [(PointUI 0 0, line "s")]
                         , singleMonoOverlay = [(PointUI 3 2, line "m")] }
        expected = map fromEnum $ encodeFrameLayout $ layOutFrame 7 overlaid
    rf <- startupUnderTest coscreen
    n <- js_overlaysCount
    frames <- js_frameCount
    fdisplay rf blank
    js_overlaysCount >>= (@?= n)
    fdisplay rf overlaid
    js_overlaysCount >>= (@?= n + 1)
    js_overlaysWidth n >>= (@?= 7)
    js_overlaysFramesBefore n >>= (@?= frames + 1)
    len <- js_overlaysLength n
    mapM (js_overlaysWord n) [0 .. len - 1] >>= (@?= expected)

-- | What the driver's @lhSubmitOverlays@ stub recorded, call by call.
foreign import javascript unsafe "globalThis.lhSubmitOverlaysCalls.length"
  js_overlaysCount :: IO Int

foreign import javascript unsafe "globalThis.lhSubmitOverlaysCalls[$1].w"
  js_overlaysWidth :: Int -> IO Int

foreign import javascript unsafe
  "globalThis.lhSubmitOverlaysCalls[$1].frames"
  js_overlaysFramesBefore :: Int -> IO Int

foreign import javascript unsafe
  "globalThis.lhSubmitOverlaysCalls[$1].words.length"
  js_overlaysLength :: Int -> IO Int

foreign import javascript unsafe
  "globalThis.lhSubmitOverlaysCalls[$1].words[$2]"
  js_overlaysWord :: Int -> Int -> IO Int
#endif
