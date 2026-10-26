-- | Determinism goldens (@docs/wasm-frontend-unified-plan.md@, the determinism
-- goldens practice): digests, each compared with a committed literal, in
-- two families. The frame family digests every request a client sends the
-- frontend while a scripted player opens one of the dialogs players read
-- --- help, history, the main menu, an item's description --- or looks at
-- the HUD frame beneath them. Each script runs twice, under the single-font
-- setup the terminal frontends have and under the multi-font one SDL and the
-- browser have, and each case's golden fails when the requests its script
-- sends under that setup change. The state family digests the state a crawl
-- game or a battle game ends in, the AI playing it for a fixed number of
-- frames at the difficulty and with the seeds of the Makefile's @benchCrawl@
-- or @benchBattle@. A legitimate change is the new digest the failure prints,
-- pasted over the old one. The perturbations that prove each family can fail
-- are the mutants in @tools/mutants-deep.py@ whose labels begin @goldens:@.
--
-- Each case name begins with the @fe-golden@ series marker.
module DeterminismGoldenUnitTests (determinismGoldenUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary (encode, put)
import           Data.Binary.Put (Put, putWord8, runPut)
import           Data.Bits (xor)
import qualified Data.ByteString.Lazy as BL
import qualified Data.EnumMap.Strict as EM
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word64)
import           Options.Applicative
  (defaultPrefs, execParserPure, handleParseResult)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf (printf)

import           Game.LambdaHack.Client.MonadClient (MonadClient)
import           Game.LambdaHack.Client.UI.Frame (ColorMode (..))
import           Game.LambdaHack.Client.UI.FrameM (promptGetKey)
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI (MonadClientUI (..))
import           Game.LambdaHack.Client.UI.Msg (MsgClassShow (..))
import           Game.LambdaHack.Client.UI.MsgM (msgAdd)
import           Game.LambdaHack.Client.UI.PointUI (PointUI (..))
import           Game.LambdaHack.Client.UI.SessionUI (SessionUI (..))
import           Game.LambdaHack.Common.ClientOptions (ClientOptions)
import           Game.LambdaHack.Definition.Defs
  (CStore (..), ItemDialogMode (..))
import           Game.LambdaHack.Server (serverOptionsPI)

import TieKnot (tieKnot)
import UnitTestHelpers

determinismGoldenUnitTests :: TestTree
determinismGoldenUnitTests = testGroup "determinismGoldenUnitTests" $ concat
  [ frameGolden hudScript 0xc903da55cccbbada 0xc903da55cccbbada
  , frameGolden helpScript 0xdfcea6da3d931836 0x2cd4b9ef354ea8de
  -- The browser builds offer no key to dump the history to a file, so the
  -- history screen's prompt lists one key fewer there (allHistoryHuman), and
  -- their goldens are those `make test-wasm` prints.
#if !defined(USE_JSFILE) && !defined(USE_WASMFILE)
  , frameGolden historyScript 0xaf37da5db75d7fb0 0x25eea5689396c8e0
#else
  , frameGolden historyScript 0x3a766f7350d32486 0xf3e785f52f3f62e6
#endif
  , frameGolden mainMenuScript 0xa32e124d169cae09 0x4bcf461cd770978a
  , frameGolden itemScript 0xf9cb38ca86a7f36c 0xf76941846095dcda
  , [ stateGolden "crawl" 1 "SMGen 123 123" "SMGen 123 125" 0x6e31649e74658d4a
    , stateGolden "battle" 3 "SMGen 127 123" "SMGen 127 125" 0x2b62e6d39badeee9 ]
  ]

-- | What a scripted player does: the keys the frontend answers with,
-- in order, then ESC once they run out; a change to the fixture; and
-- the action, run by hero A as the pointman.
data Script = Script
  { scriptName    :: String
  , scriptKeys    :: [K.KM]
  , scriptFixture :: CliState -> CliState
  , scriptAction  :: forall m. (MonadClient m, MonadClientUI m) => m ()
  }

-- | The HUD frame alone: the map and the status lines, and no text
-- a font setup lays out, so its two digests agree.
hudScript :: Script
hudScript = Script
  { scriptName = "the HUD frame"
  , scriptKeys = []
  , scriptFixture = id
  , scriptAction = void $ promptGetKey ColorFull EM.empty False []
  }

-- | The first three pages of help.
helpScript :: Script
helpScript = Script
  { scriptName = "help"
  , scriptKeys = [K.spaceKM, K.spaceKM]
  , scriptFixture = id
  , scriptAction = void $ dispatchCmd HumanCmd.Help
  }

-- | The message history, of two messages.
historyScript :: Script
historyScript = Script
  { scriptName = "history"
  , scriptKeys = []
  , scriptFixture = id
  , scriptAction = do
      msgAdd MsgActionAlert "A message to show."
      msgAdd MsgActionAlert "Another one."
      void $ dispatchCmd HumanCmd.AllHistory
  }

-- | The main menu.
mainMenuScript :: Script
mainMenuScript = Script
  { scriptName = "the main menu"
  , scriptKeys = []
  , scriptFixture = id
  , scriptAction = void $ dispatchCmd HumanCmd.MainMenu
  }

-- | The equipment menu, and the description of the item it lists.
itemScript :: Script
itemScript = Script
  { scriptName = "an item's description"
  , scriptKeys = [K.returnKM]
  , scriptFixture = withEqpItems [(testActorId, testItemId2)]
  , scriptAction =
      void $ dispatchCmd $ HumanCmd.ChooseItemMenu (MStore CEqp)
  }

-- | The script's two cases, under the single-font and the multi-font
-- setup, each against its golden digest.
frameGolden :: Script -> Word64 -> Word64 -> [TestTree]
frameGolden script singleFont multiFont =
  [ frameCase "single font" stubClientOptions singleFont
  , frameCase "multi-font" multiFontClientOptions multiFont ]
 where
  frameCase setup options golden =
    testCase ("fe-golden: " ++ scriptName script ++ ", " ++ setup) $ do
      records <- runScript script options
      Digest (fnv1a $ runPut $ mapM_ putRecord records) @?= Digest golden

-- | Every request the action sends the frontend, on 'frameCliState'.
runScript :: Script -> ClientOptions -> IO [FrontRecord]
runScript Script{..} options = do
  (chanF, getRecords) <- recordingFchanFrontend scriptKeys
  let cliS = scriptFixture $ frameCliState options
      action = do
        updateClientLeader testActorId
        scriptAction
  void $ executorCli action
    cliS {cliSession = (\sess -> sess {schanF = chanF}) <$> cliSession cliS}
  getRecords

-- | A request as bytes: a tag, then its contents, a frame being its
-- screen array and its three overlays, each line at its position.
putRecord :: FrontRecord -> Put
putRecord = \case
  RecordFrame frame -> putWord8 0 >> putFrame frame
  RecordDelay k -> putWord8 1 >> put k
  RecordKey kms frame -> putWord8 2 >> put kms >> putFrame frame
  RecordPressed -> putWord8 3
  RecordDiscardKey -> putWord8 4
  RecordResetKeys -> putWord8 5
  RecordShutdown -> putWord8 6
  RecordPrintScreen -> putWord8 7
 where
  putFrame RealisedFrame{realisedArray, realisedOverlays = (prop, sq, mono)} =
    do put $ U.toList realisedArray
       mapM_ (put . map (\(PointUI x y, al) -> ((x, y), al))) [prop, sq, mono]

-- | The state a game of the given mode ends in, started anew at the given
-- difficulty with the given dungeon and main seeds, and played by the AI for a
-- fixed number of frames.
stateGolden :: String -> Int -> String -> String -> Word64 -> TestTree
stateGolden mode difficulty dungeonRng mainRng golden =
  testCase ("fe-golden: the state a " ++ mode ++ " game ends in") $ do
    let args = words "--noAnim --maxFps 100000 --frontendNull --benchmark"
               ++ words "--stopAfterFrames 500 --automateAll --keepAutomated"
               ++ [ "--newGame", show difficulty, "--gameMode", mode
                  , "--setDungeonRng", dungeonRng, "--setMainRng", mainRng ]
    serverOptions <-
      handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
    finalState <- tieKnot serverOptions
    Digest (fnv1a $ encode finalState) @?= Digest golden

-- | 64-bit FNV-1a, the digest fold both golden families share.
fnv1a :: BL.ByteString -> Word64
fnv1a = BL.foldl' (\h b -> (h `xor` into @Word64 b) * 0x100000001b3)
                  0xcbf29ce484222325

-- | A digest, shown as the hexadecimal literal a golden is written in.
newtype Digest = Digest Word64
  deriving Eq

instance Show Digest where
  show (Digest w) = printf "0x%016x" w
