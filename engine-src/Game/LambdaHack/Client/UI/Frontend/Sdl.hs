-- | Text frontend based on SDL2.
module Game.LambdaHack.Client.UI.Frontend.Sdl
  ( startup, frontendName
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , FontAtlas, FrontendSession(..), startupFun, shutdown, forceShutdown
  , display, drawFrame, printScreen, modTranslate, keyTranslate, colorToRGBA
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import           Data.IORef
import qualified Data.Text as T
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word32, Word8)
import           Foreign.C.String (withCString)
import           Foreign.C.Types (CInt)
import           Foreign.Ptr (nullPtr)
import           Foreign.Storable (peek)
import           System.Directory
import           System.Exit (die, exitSuccess)
import           System.FilePath

import qualified SDL
import qualified SDL.Font as TTF
import           SDL.Input.Keyboard.Codes
import qualified SDL.Internal.Types
import qualified SDL.Raw.Basic as SDL (logSetAllPriority)
import qualified SDL.Raw.Enum
import qualified SDL.Raw.Event
import qualified SDL.Raw.Types
import qualified SDL.Raw.Video
import qualified SDL.Vect as Vect

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.File
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Content.TileKind (floorSymbol)
import qualified Game.LambdaHack.Definition.Color as Color

-- These are needed until SDL is fixed and all our devs move
-- to the fixed version:
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           SDL.Internal.Exception (throwIfNull)
import qualified SDL.Raw.Event as Raw
import           Unsafe.Coerce (unsafeCoerce)
--import qualified SDL.Raw.Enum as Raw

type FontAtlas = EM.EnumMap Color.AttrCharW32 SDL.Texture

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { swindow          :: SDL.Window
  , srenderer        :: SDL.Renderer
  , squareFont       :: TTF.Font
  , squareFontSize   :: Int
  , mapFontIsBitmap  :: Bool
  , spropFont        :: Maybe TTF.Font
  , sboldFont        :: Maybe TTF.Font
  , smonoFont        :: Maybe TTF.Font
  , squareAtlas      :: IORef FontAtlas
  , smonoAtlas       :: IORef FontAtlas
  , sbasicTexture    :: IORef SDL.Texture
  , stexture         :: IORef SDL.Texture
  , spreviousFrame   :: IORef SingleFrame
  , sforcedShutdown  :: IORef Bool
  , scontinueSdlLoop :: IORef Bool
  , sframeQueue      :: MVar SingleFrame
  , sframeDrawn      :: MVar ()
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "sdl"

-- | Set up and start the main loop providing input and output.
--
-- Because of Windows and OS X, SDL2 needs to be on a bound thread,
-- so we can't avoid the communication overhead of bound threads.
startup :: ScreenContent -> ClientOptions -> IO RawFrontend
startup coscreen soptions = startupBound $ startupFun coscreen soptions

startupFun :: ScreenContent -> ClientOptions -> MVar RawFrontend -> IO ()
startupFun coscreen soptions@ClientOptions{..} rfMVar = do
 SDL.initialize [SDL.InitEvents]
 -- lowest: pattern SDL_LOG_PRIORITY_VERBOSE = (1) :: LogPriority
 -- our default: pattern SDL_LOG_PRIORITY_ERROR = (5) :: LogPriority
 SDL.logSetAllPriority $ toEnum $ fromMaybe 5 slogPriority
 TTF.initialize
 let title = T.pack $ fromJust stitle
     chosenFontsetID = fromJust schosenFontset
 -- Unlike @error@, @die@ does not move savefiles aside.
 chosenFontset <- case lookup chosenFontsetID sfontsets of
   Nothing -> die $ "Fontset not defined in config file"
                    `showFailure` chosenFontsetID
   Just fs -> return fs
     -- If some auxiliary fonts are equal and at the same size, this wastefully
     -- opens them many times. However, native builds are efficient enough
     -- and slow machines should use the most frugal case (only square font)
     -- in which no waste occurs and all rendering is aided with an atlas.
 let findFontFile t =
       if T.null t
       then return Nothing
       else case lookup t sfonts of
         Nothing -> die $ "Font not defined in config file" `showFailure` t
         Just (FontProportional fname fsize fhint) -> do
           sdlFont <- loadFontFile fname fsize
           setHintMode sdlFont fhint
           -- TODO: when SDL_ttf can do it, check that not a bitmap font
           realSize <- TTF.height sdlFont
           let !_A = assert (realSize > 0) ()  -- sanity
           return $ Just (sdlFont, realSize)
         Just (FontMonospace fname fsize fhint) -> do
           sdlFont <- loadFontFile fname fsize
           setHintMode sdlFont fhint
           isFontMono <- TTF.isMonospace sdlFont
           realSize <- TTF.height sdlFont
           let !_A = assert (isFontMono && realSize > 0) ()  -- sanity
           return $ Just (sdlFont, realSize)
         Just (FontMapScalable fname fsize fhint cellSizeAdd) -> do
           sdlFont <- loadFontFile fname fsize
           setHintMode sdlFont fhint
           isFontMono <- TTF.isMonospace sdlFont
           realSize <- TTF.height sdlFont
           let !_A = assert (isFontMono && realSize > 0) ()  -- sanity
           return $ Just (sdlFont, realSize + cellSizeAdd)
         Just (FontMapBitmap fname cellSizeAdd) -> do
           sdlFont <- loadFontFile fname 0  -- size ignored for bitmap fonts
           isFontMono <- TTF.isMonospace sdlFont
           realSize <- TTF.height sdlFont
           let !_A = assert (isFontMono && realSize > 0) ()  -- sanity
           return $ Just (sdlFont, realSize + cellSizeAdd)
     loadFontFile fname fsize = do
       let fontFileName = T.unpack fname
           fontSize = round $ fromJust sallFontsScale * intToDouble fsize
       if isRelative fontFileName
       then do
         case lookup fontFileName $ rFontFiles coscreen of
           Nothing -> fail $ "Font file not supplied with the game: "
                             ++ fontFileName
                             ++ " within "
                             ++ show (map fst $ rFontFiles coscreen)
           Just bs -> TTF.decode bs fontSize
       else do
         fontFileExists <- doesFileExist fontFileName
         if not fontFileExists
         then fail $ "Font file does not exist: " ++ fontFileName
         else TTF.load fontFileName fontSize
     setHintMode _ HintingHeavy = return ()  -- default
     setHintMode sdlFont HintingLight = TTF.setHinting sdlFont TTF.Light
 (squareFont, squareFontSize, mapFontIsBitmap) <-
   if fromJust sallFontsScale == 1.0 then do
     mfontMapBitmap <- findFontFile $ fontMapBitmap chosenFontset
     case mfontMapBitmap of
       Just (sdlFont, size) -> return (sdlFont, size, True)
       Nothing -> do
         mfontMapScalable <- findFontFile $ fontMapScalable chosenFontset
         case mfontMapScalable of
           Just (sdlFont, size) -> return (sdlFont, size, False)
           Nothing -> die "Neither bitmap nor scalable map font defined"
   else do
     mfontMapScalable <- findFontFile $ fontMapScalable chosenFontset
     case mfontMapScalable of
        Just (sdlFont, size) -> return (sdlFont, size, False)
        Nothing -> die "Scaling requested but scalable map font not defined"
 let halfSize = squareFontSize `div` 2
     boxSize = 2 * halfSize  -- map font determines cell size for all others
 -- Real size of these fonts ignored.
 spropFont <- fst <$$> findFontFile (fontPropRegular chosenFontset)
 sboldFont <- fst <$$> findFontFile (fontPropBold chosenFontset)
 smonoFont <- fst <$$> findFontFile (fontMono chosenFontset)
 let !_A =
       assert
         (isJust spropFont && isJust sboldFont && isJust smonoFont
          || isNothing spropFont && isNothing sboldFont && isNothing smonoFont
          `blame` "Either all auxiliary fonts should be defined or none"
          `swith` chosenFontset) ()
 -- The hacky log priority 0 tells SDL frontend to init and quit at once,
 -- for testing on CIs without graphics access.
 if slogPriority == Just 0 then do
  rf <- createRawFrontend coscreen (\_ -> return ()) (return ())
  putMVar rfMVar rf
  maybe (return ()) TTF.free spropFont
  maybe (return ()) TTF.free sboldFont
  maybe (return ()) TTF.free smonoFont
  TTF.free squareFont
  TTF.quit
  SDL.quit
 else do
  -- The code below fails without access to a graphics system.
  SDL.initialize [SDL.InitVideo]
  -- This cursor size if fine for default size and Full HD 1.5x size.
  let (cursorAlpha, cursorBW) = cursorXhair
  xhairCursor <-
    createCursor cursorBW cursorAlpha (SDL.V2 32 27) (SDL.P (SDL.V2 13 13))
  SDL.activeCursor SDL.$= xhairCursor
--  xhairCursor <-
--    throwIfNull "SDL.Input.Mouse.createSystemCursor" "SDL_createSystemCursor"
--    $ Raw.createSystemCursor Raw.SDL_SYSTEM_CURSOR_CROSSHAIR
--  SDL.activeCursor SDL.$= unsafeCoerce xhairCursor
  let screenV2 = SDL.V2 (toEnum $ rwidth coscreen * boxSize)
                        (toEnum $ rheight coscreen * boxSize)
      windowConfig = SDL.defaultWindow
        { SDL.windowInitialSize = screenV2
        , SDL.windowMode = case fromMaybe NotFullscreen sfullscreenMode of
            ModeChange -> SDL.Fullscreen
            BigBorderlessWindow -> SDL.FullscreenDesktop
            NotFullscreen -> SDL.Windowed }
      rendererConfig = SDL.RendererConfig
        { rendererType          = if sbenchmark
                                  then SDL.AcceleratedRenderer
                                  else SDL.AcceleratedVSyncRenderer
        , rendererTargetTexture = True
        }
  swindow <- SDL.createWindow title windowConfig
  srenderer <- SDL.createRenderer swindow (-1) rendererConfig
  unless (fromMaybe NotFullscreen sfullscreenMode == NotFullscreen) $
    -- This is essential to preserve game map aspect ratio in fullscreen, etc.,
    -- if the aspect ratios of video mode and game map view don't match.
    SDL.rendererLogicalSize srenderer SDL.$= Just screenV2
  let clearScreen = do
        -- Display black screen ASAP to hide any garbage. This is also needed
        -- to clear trash on the margins in fullscreen. No idea why the double
        -- calls are needed, sometimes. Perhaps it's double-buffered.
        SDL.rendererRenderTarget srenderer SDL.$= Nothing
        SDL.clear srenderer  -- clear the backbuffer
        SDL.present srenderer
        SDL.clear srenderer  -- clear the other half of the double buffer?
        SDL.present srenderer
  clearScreen
  let initTexture = do
        texture <- SDL.createTexture srenderer SDL.ARGB8888
                                     SDL.TextureAccessTarget screenV2
        SDL.rendererRenderTarget srenderer SDL.$= Just texture
        SDL.rendererDrawBlendMode srenderer SDL.$= SDL.BlendNone
        SDL.rendererDrawColor srenderer SDL.$= blackRGBA
        SDL.clear srenderer  -- clear the texture
        return texture
  basicTexture <- initTexture
  sbasicTexture <- newIORef basicTexture
  texture <- initTexture
  stexture <- newIORef texture
  squareAtlas <- newIORef EM.empty
  smonoAtlas <- newIORef EM.empty
  spreviousFrame <- newIORef $ blankSingleFrame coscreen
  sforcedShutdown <- newIORef False
  scontinueSdlLoop <- newIORef True
  sframeQueue <- newEmptyMVar
  sframeDrawn <- newEmptyMVar
  let sess = FrontendSession{..}
  rfWithoutPrintScreen <-
    createRawFrontend coscreen (display sess) (shutdown sess)
  let rf = rfWithoutPrintScreen {fprintScreen = printScreen sess}
  putMVar rfMVar rf
  let pointTranslate :: forall i. (Enum i) => Vect.Point Vect.V2 i -> PointUI
      pointTranslate (SDL.P (SDL.V2 x y)) =
        PointUI (fromEnum x `div` halfSize) (fromEnum y `div` boxSize)
      redraw = do
        -- Textures may be trashed and even invalid, especially on Windows.
        atlas <- readIORef squareAtlas
        writeIORef squareAtlas EM.empty
        monoAtlas <- readIORef smonoAtlas
        writeIORef smonoAtlas EM.empty
        oldBasicTexture <- readIORef sbasicTexture
        newBasicTexture <- initTexture
        oldTexture <- readIORef stexture
        newTexture <- initTexture
        mapM_ SDL.destroyTexture $ EM.elems atlas
        mapM_ SDL.destroyTexture $ EM.elems monoAtlas
        SDL.destroyTexture oldBasicTexture
        SDL.destroyTexture oldTexture
        writeIORef sbasicTexture newBasicTexture
        writeIORef stexture newTexture
        -- To clear the margins in fullscreen:
        clearScreen
        -- To overwrite each char:
        prevFrame <- readIORef spreviousFrame
        writeIORef spreviousFrame $ blankSingleFrame coscreen
        drawFrame coscreen soptions sess prevFrame
        SDL.pumpEvents
        SDL.Raw.Event.flushEvents minBound maxBound
      loopSDL :: IO ()
      loopSDL = do
        me <- SDL.pollEvent  -- events take precedence over frames
        case me of
          Nothing -> do
            mfr <- tryTakeMVar sframeQueue
            case mfr of
              Just fr -> do
                -- Some SDL2 (OpenGL) backends are very thread-unsafe,
                -- so we need to ensure we draw on the same (bound) OS thread
                -- that initialized SDL, hence we have to poll frames.
                drawFrame coscreen soptions sess fr
                putMVar sframeDrawn ()  -- signal that drawing ended
              Nothing -> threadDelay $ if sbenchmark then 150 else 15000
                           -- 60 polls per second, so keyboard snappy enough;
                           -- max 6000 FPS when benchmarking
          Just e -> handleEvent e
        continueSdlLoop <- readIORef scontinueSdlLoop
        if continueSdlLoop
        then loopSDL
        else do
          maybe (return ()) TTF.free spropFont
          maybe (return ()) TTF.free sboldFont
          maybe (return ()) TTF.free smonoFont
          TTF.free squareFont
          TTF.quit
          SDL.destroyRenderer srenderer
          SDL.destroyWindow swindow
          SDL.quit
          forcedShutdown <- readIORef sforcedShutdown
          when forcedShutdown
            exitSuccess  -- not in the main thread, so no exit yet, see "Main"
      handleEvent e = case SDL.eventPayload e of
        SDL.KeyboardEvent keyboardEvent
          | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed -> do
            let sym = SDL.keyboardEventKeysym keyboardEvent
                ksm = SDL.keysymModifier sym
                shiftPressed = SDL.keyModifierLeftShift ksm
                               || SDL.keyModifierRightShift ksm
                key = keyTranslate shiftPressed $ SDL.keysymKeycode sym
                modifier = modTranslate ksm
                modifierNoShift = case modifier of  -- to prevent S-!, etc.
                  K.Shift -> K.NoModifier
                  K.ControlShift -> K.Control
                  K.AltShift -> K.Alt
                  _ -> modifier
            p <- SDL.getAbsoluteMouseLocation
            when (key == K.Esc) $ resetChanKey (fchanKey rf)
            saveKMP rf modifierNoShift key (pointTranslate p)
        SDL.MouseButtonEvent mouseButtonEvent
          | SDL.mouseButtonEventMotion mouseButtonEvent == SDL.Released -> do
            modifier <- modTranslate <$> SDL.getModState
            let key = case SDL.mouseButtonEventButton mouseButtonEvent of
                  SDL.ButtonLeft -> K.LeftButtonRelease
                  SDL.ButtonMiddle -> K.MiddleButtonRelease
                  SDL.ButtonRight -> K.RightButtonRelease
                  _ -> K.LeftButtonRelease  -- any other is spare left
                p = SDL.mouseButtonEventPos mouseButtonEvent
            saveKMP rf modifier key (pointTranslate p)
        SDL.MouseWheelEvent mouseWheelEvent -> do
          modifier <- modTranslate <$> SDL.getModState
          let SDL.V2 _ y = SDL.mouseWheelEventPos mouseWheelEvent
              mkey = case (compare y 0, SDL.mouseWheelEventDirection
                                          mouseWheelEvent) of
                (EQ, _) -> Nothing
                (LT, SDL.ScrollNormal) -> Just K.WheelSouth
                (GT, SDL.ScrollNormal) -> Just K.WheelNorth
                (LT, SDL.ScrollFlipped) -> Just K.WheelNorth
                (GT, SDL.ScrollFlipped) -> Just K.WheelSouth
          p <- SDL.getAbsoluteMouseLocation
          maybe (return ())
                (\key -> saveKMP rf modifier key (pointTranslate p)) mkey
        SDL.WindowClosedEvent{} -> forceShutdown sess
        SDL.QuitEvent -> forceShutdown sess
        SDL.WindowRestoredEvent{} -> redraw  -- e.g., unminimize
        SDL.WindowExposedEvent{} -> redraw  -- needed on Windows
        SDL.WindowResizedEvent{} -> do
          -- Eome window managers apparently are able to resize.
          SDL.showSimpleMessageBox Nothing SDL.Warning
            "Windows resize detected"
            "Please resize the game and/or make it fullscreen via 'allFontsScale' and 'fullscreenMode' settings in the 'config.ui.ini' file. Resizing fonts via generic scaling algorithms gives poor results."
          redraw
        -- Probably not needed, because no textures nor their content lost:
        -- SDL.WindowShownEvent{} -> redraw
        _ -> return ()
  loopSDL

-- | Copied from SDL2 and fixed (packed booleans are needed).
--
-- Create a cursor using the specified bitmap data and mask (in MSB format,
-- packed). Width must be a multiple of 8.
--
--
createCursor :: MonadIO m
             => VS.Vector Word8 -- ^ Whether this part of the cursor is black. Use bit 1 for white and bit 0 for black.
             -> VS.Vector Word8 -- ^ Whether or not pixels are visible. Use bit 1 for visible and bit 0 for transparent.
             -> Vect.V2 CInt -- ^ The width and height of the cursor.
             -> Vect.Point Vect.V2 CInt -- ^ The X- and Y-axis location of the upper left corner of the cursor relative to the actual mouse position
             -> m SDL.Cursor
createCursor dta msk (Vect.V2 w h) (Vect.P (Vect.V2 hx hy)) =
    liftIO . fmap unsafeCoerce $
        throwIfNull "SDL.Input.Mouse.createCursor" "SDL_createCursor" $
            VS.unsafeWith dta $ \unsafeDta ->
            VS.unsafeWith msk $ \unsafeMsk ->
                Raw.createCursor unsafeDta unsafeMsk w h hx hy

-- Ignores bits after the last 8 multiple.
boolListToWord8List :: [Bool] -> [Word8]
boolListToWord8List =
  let i True multiple = multiple
      i False _ = 0
  in \case
    b1 : b2 : b3 : b4 : b5 : b6 : b7 : b8 : rest ->
      i b1 128 + i b2 64 + i b3 32 + i b4 16 + i b5 8 + i b6 4 + i b7 2 + i b8 1
      : boolListToWord8List rest
    _ -> []

cursorXhair :: (VS.Vector Word8, VS.Vector Word8)  -- alpha, BW
cursorXhair =
  let charToBool '.' = (True, True)  -- visible black
      charToBool '#' = (True, False)  -- visible white
      charToBool _ = (False, False)  -- transparent white
      toVS = VS.fromList . boolListToWord8List
  in toVS *** toVS $ unzip $ map charToBool $ concat

    [ "            ...                 "
    , "            .#.                 "
    , "        ..  .#.  ..             "
    , "      ..##  .#.  ##..           "
    , "     .##    .#.    ##.          "
    , "    .#      .#.      #.         "
    , "   .#       .#.       #.        "
    , "   .#       ...       #.        "
    , "  .#                   #.       "
    , "  .#                   #.       "
    , "                                "
    , "             .                  "
    , "........    .#.    ........     "
    , ".######.   .###.   .######.     "
    , "........    .#.    ........     "
    , "             .                  "
    , "                                "
    , "  .#                   #.       "
    , "  .#                   #.       "
    , "   .#       ...       #.        "
    , "   .#       .#.       #.        "
    , "    .#      .#.      #.         "
    , "     .##    .#.    ##.          "
    , "      ..##  .#.  ##..           "
    , "        ..  .#.  ..             "
    , "            .#.                 "
    , "            ...                 " ]

shutdown :: FrontendSession -> IO ()
shutdown FrontendSession{..} = writeIORef scontinueSdlLoop False

forceShutdown :: FrontendSession -> IO ()
forceShutdown sess@FrontendSession{..} = do
  writeIORef sforcedShutdown True
  shutdown sess

-- | Add a frame to be drawn.
display :: FrontendSession  -- ^ frontend session data
        -> SingleFrame      -- ^ the screen frame to draw
        -> IO ()
display FrontendSession{..} curFrame = do
  continueSdlLoop <- readIORef scontinueSdlLoop
  if continueSdlLoop then do
    putMVar sframeQueue curFrame
    -- Wait until the frame is drawn.
    takeMVar sframeDrawn
  else do
    forcedShutdown <- readIORef sforcedShutdown
    when forcedShutdown $
      -- When there's a forced shutdown, ignore displaying one frame
      -- and don't occupy the CPU creating new ones and moving on with the game
      -- (possibly also saving the new game state, surprising the player),
      -- but delay the server and client thread(s) for a long time
      -- and let the SDL-init thread clean up and exit via @exitSuccess@
      -- to avoid exiting via "thread blocked".
      threadDelay 50000

drawFrame :: ScreenContent    -- ^ e.g., game screen size
          -> ClientOptions    -- ^ client options
          -> FrontendSession  -- ^ frontend session data
          -> SingleFrame      -- ^ the screen frame to draw
          -> IO ()
drawFrame coscreen ClientOptions{..} sess@FrontendSession{..} curFrame = do
  prevFrame <- readIORef spreviousFrame
  let halfSize = squareFontSize `div` 2
      boxSize = 2 * halfSize
      tt2Square = Vect.V2 (toEnum boxSize) (toEnum boxSize)
      vp :: Int -> Int -> Vect.Point Vect.V2 CInt
      vp x y = Vect.P $ Vect.V2 (toEnum x) (toEnum y)
      drawHighlight !col !row !color = do
        SDL.rendererDrawColor srenderer SDL.$= colorToRGBA color
        let rect = SDL.Rectangle (vp (col * boxSize) (row * boxSize)) tt2Square
        SDL.drawRect srenderer $ Just rect
        SDL.rendererDrawColor srenderer SDL.$= blackRGBA
          -- reset back to black
      chooseAndDrawHighlight !col !row !bg = do
-- Rectangle drawing is broken in SDL 2.0.16
-- (https://github.com/LambdaHack/LambdaHack/issues/281)
-- and simple workarounds fail with old SDL, e.g., four lines instead of
-- a rectangle, so we have to manually erase the broken rectangles
-- instead of depending on glyphs overwriting them fully.
       let workaroundOverwriteHighlight = do
             let rect = SDL.Rectangle (vp (col * boxSize) (row * boxSize))
                                      tt2Square
             SDL.drawRect srenderer $ Just rect
       case bg of
        Color.HighlightNone -> workaroundOverwriteHighlight
        Color.HighlightBackground -> workaroundOverwriteHighlight
        Color.HighlightNoneCursor -> workaroundOverwriteHighlight
        _ -> drawHighlight col row $ Color.highlightToColor bg
-- workarounds end
      -- This also frees the surface it gets.
      scaleSurfaceToTexture :: Int -> SDL.Surface -> IO SDL.Texture
      scaleSurfaceToTexture xsize textSurfaceRaw = do
        Vect.V2 sw sh <- SDL.surfaceDimensions textSurfaceRaw
        let width = min xsize $ fromEnum sw
            height = min boxSize $ fromEnum sh
            xsrc = max 0 (fromEnum sw - width) `div` 2
            ysrc = max 0 (fromEnum sh - height) `divUp` 2
            srcR = SDL.Rectangle (vp xsrc ysrc)
                                 (Vect.V2 (toEnum width) (toEnum height))
            xtgt = (xsize - width) `divUp` 2
            ytgt = (boxSize - height) `div` 2
            tgtR = vp xtgt ytgt
            tt2 = Vect.V2 (toEnum xsize) (toEnum boxSize)
        textSurface <- SDL.createRGBSurface tt2 SDL.ARGB8888
        SDL.surfaceFillRect textSurface Nothing blackRGBA
        -- We crop surface rather than texture to set the resulting
        -- texture as @TextureAccessStatic@ via @createTextureFromSurface@,
        -- which otherwise we wouldn't be able to do.
        void $ SDL.surfaceBlit textSurfaceRaw (Just srcR)
                               textSurface (Just tgtR)
        SDL.freeSurface textSurfaceRaw
        textTexture <- SDL.createTextureFromSurface srenderer textSurface
        SDL.freeSurface textSurface
        return textTexture
      -- This also frees the surface it gets.
      scaleSurfaceToTextureProp :: Int -> Int -> SDL.Surface -> Bool
                                -> IO (Int, SDL.Texture)
      scaleSurfaceToTextureProp x row textSurfaceRaw allSpace = do
        Vect.V2 sw sh <- SDL.surfaceDimensions textSurfaceRaw
        let widthRaw = fromEnum sw
            remainingWidth = rwidth coscreen * boxSize - x
            width | widthRaw <= remainingWidth = widthRaw
                  | allSpace = remainingWidth
                  | otherwise = remainingWidth - boxSize
            height = min boxSize $ fromEnum sh
            xsrc = 0
            ysrc = max 0 (fromEnum sh - height) `divUp` 2
            srcR = SDL.Rectangle (vp xsrc ysrc)
                                 (Vect.V2 (toEnum width) (toEnum height))
            xtgt = 0
            ytgt = (boxSize - height) `div` 2
            tgtR = vp xtgt ytgt
            tt2Prop = Vect.V2 (toEnum width) (toEnum boxSize)
        textSurface <- SDL.createRGBSurface tt2Prop SDL.ARGB8888
        SDL.surfaceFillRect textSurface Nothing blackRGBA
        -- We crop surface rather than texture to set the resulting
        -- texture as @TextureAccessStatic@ via @createTextureFromSurface@,
        -- which otherwise we wouldn't be able to do.
        -- This is not essential for proportional font, for which we have
        -- no texture atlas, but it's consistent with other fonts
        -- and the bottleneck is the square font, anyway.
        void $ SDL.surfaceBlit textSurfaceRaw (Just srcR)
                               textSurface (Just tgtR)
        SDL.freeSurface textSurfaceRaw
        textTexture <- SDL.createTextureFromSurface srenderer textSurface
        SDL.freeSurface textSurface
        when (width /= widthRaw && not allSpace) $
          setSquareChar (rwidth coscreen - 1) row Color.trimmedLineAttrW32
        return (width, textTexture)
      -- <https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf_42.html#SEC42>
      -- Note that @Point@ here refers to screen coordinates with square font
      -- (as @PointSquare@ normally should) and not game map coordinates.
      -- See "Game.LambdaHack.Client.UI.Frame" for explanation of this
      -- irregularity.
      setMapChar :: PointI -> (Word32, Word32) -> IO Int
      setMapChar !i (!w, !wPrev) =
        if w == wPrev
        then return $! i + 1
        else do
          let Point{..} = toEnum i
          setSquareChar px py (Color.AttrCharW32 w)
          return $! i + 1
      drawMonoOverlay :: OverlaySpace -> IO ()
      drawMonoOverlay =
        mapM_ (\(PointUI x y, al) ->
                 let lineCut = take (2 * rwidth coscreen - x) al
                 in drawMonoLine x y lineCut)
      drawMonoLine :: Int -> Int -> AttrString -> IO ()
      drawMonoLine _ _ [] = return ()
      drawMonoLine x row (w : rest) = do
        setMonoChar x row w
        drawMonoLine (x + 1) row rest
      setMonoChar :: Int -> Int -> Color.AttrCharW32 -> IO ()
      setMonoChar !x !row !w = do
        atlas <- readIORef smonoAtlas
        let Color.AttrChar{acAttr=Color.Attr{fg=fgRaw, bg}, acChar} =
              Color.attrCharFromW32 w
            fg | even row && fgRaw == Color.White = Color.AltWhite
               | otherwise = fgRaw
            ac = Color.attrChar2ToW32 fg acChar
            !_A = assert (bg `elem` [ Color.HighlightNone
                                    , Color.HighlightNoneCursor ]) ()
        textTexture <- case EM.lookup ac atlas of
          Nothing -> do
            textSurfaceRaw <-
              TTF.shadedGlyph (fromJust smonoFont) (colorToRGBA fg)
                              blackRGBA acChar
            textTexture <- scaleSurfaceToTexture halfSize textSurfaceRaw
            writeIORef smonoAtlas $ EM.insert ac textTexture atlas
            return textTexture
          Just textTexture -> return textTexture
        let tt2Mono = Vect.V2 (toEnum halfSize) (toEnum boxSize)
            tgtR = SDL.Rectangle (vp (x * halfSize) (row * boxSize)) tt2Mono
        SDL.copy srenderer textTexture Nothing (Just tgtR)
      drawSquareOverlay :: OverlaySpace -> IO ()
      drawSquareOverlay =
        mapM_ (\(pUI, al) ->
                 let PointSquare col row = uiToSquare pUI
                     lineCut = take (rwidth coscreen - col) al
                 in drawSquareLine col row lineCut)
      drawSquareLine :: Int -> Int -> AttrString -> IO ()
      drawSquareLine _ _ [] = return ()
      drawSquareLine col row (w : rest) = do
        setSquareChar col row w
        drawSquareLine (col + 1) row rest
      setSquareChar :: Int -> Int -> Color.AttrCharW32 -> IO ()
      setSquareChar !col !row !w = do
        atlas <- readIORef squareAtlas
        let Color.AttrChar{ acAttr=Color.Attr{fg=fgRaw, bg}
                          , acChar=acCharRaw } =
              Color.attrCharFromW32 w
            fg | even row && fgRaw == Color.White = Color.AltWhite
               | otherwise = fgRaw
            ac = if bg == Color.HighlightBackground
                 then w
                 else Color.attrChar2ToW32 fg acCharRaw
        textTexture <- case EM.lookup ac atlas of
          Nothing -> do
            -- Make all visible floors bold (no bold font variant for 16x16x,
            -- so only the dot can be bold).
            let acChar = if not (Color.isBright fg)
                            && acCharRaw == floorSymbol  -- '\x00B7'
                         then if mapFontIsBitmap
                              then '\x0007'
                              else '\x22C5'
                         else acCharRaw
                background = if bg == Color.HighlightBackground
                             then greyRGBA
                             else blackRGBA
            textSurfaceRaw <- TTF.shadedGlyph squareFont (colorToRGBA fg)
                                              background acChar
            textTexture <- scaleSurfaceToTexture boxSize textSurfaceRaw
            writeIORef squareAtlas $ EM.insert ac textTexture atlas
            return textTexture
          Just textTexture -> return textTexture
        let tgtR = SDL.Rectangle (vp (col * boxSize) (row * boxSize)) tt2Square
        SDL.copy srenderer textTexture Nothing (Just tgtR)
        -- Potentially overwrite a portion of the glyph.
        chooseAndDrawHighlight col row bg
      drawPropOverlay :: OverlaySpace -> IO ()
      drawPropOverlay =
        mapM_ (\(PointUI x y, al) ->
                 drawPropLine (x * halfSize) y al)
      drawPropLine :: Int -> Int -> AttrString -> IO ()
      drawPropLine _ _ [] = return ()
      drawPropLine x _ _ | x >= (rwidth coscreen - 1) * boxSize =
        -- This chunk starts at $ sign or beyond so, for KISS, reject it.
        return ()
      drawPropLine x row (w : rest) = do
        let isSpace = (== Color.spaceAttrW32)
            Color.AttrChar{acAttr=Color.Attr{fg=fgRaw, bg}} =
              Color.attrCharFromW32
              $ if isSpace w
                then case filter (not . isSpace) rest of
                  w2 : _ -> w2
                  [] -> w
                else w
            sameAttr ac = Color.fgFromW32 ac == fgRaw
                          || isSpace ac  -- matches all colours
            (sameRest, otherRest) = span sameAttr rest
            !_A = assert (bg `elem` [ Color.HighlightNone
                                    , Color.HighlightNoneCursor ]) ()
            fg | even row && fgRaw == Color.White = Color.AltWhite
               | otherwise = fgRaw
            t = T.pack $ map Color.charFromW32 $ w : sameRest
        width <- drawPropChunk x row fg t
        drawPropLine (x + width) row otherRest
      drawPropChunk :: Int -> Int -> Color.Color -> T.Text -> IO Int
      drawPropChunk x row fg t = do
        let font = if fg >= Color.White && fg /= Color.BrBlack
                   then spropFont
                   else sboldFont
            allSpace = T.all Char.isSpace t
        textSurfaceRaw <- TTF.shaded (fromJust font) (colorToRGBA fg)
                                     blackRGBA t
        (width, textTexture) <-
          scaleSurfaceToTextureProp x row textSurfaceRaw allSpace
        let tgtR = SDL.Rectangle (vp x (row * boxSize))
                                 (Vect.V2 (toEnum width) (toEnum boxSize))
        -- Potentially overwrite some of the screen.
        SDL.copy srenderer textTexture Nothing (Just tgtR)
        SDL.destroyTexture textTexture
        return width
  let arraysEqual = singleArray curFrame == singleArray prevFrame
      overlaysEqual =
        singleMonoOverlay curFrame == singleMonoOverlay prevFrame
        && singleSquareOverlay curFrame == singleSquareOverlay prevFrame
        && singlePropOverlay curFrame == singlePropOverlay prevFrame
  basicTexture <- readIORef sbasicTexture  -- previous content still present
  unless arraysEqual $ do
    SDL.rendererRenderTarget srenderer SDL.$= Just basicTexture
    U.foldM'_ setMapChar 0 $ U.zip (PointArray.avector $ singleArray curFrame)
                                   (PointArray.avector $ singleArray prevFrame)
  unless (arraysEqual && overlaysEqual) $ do
    texture <- readIORef stexture
    SDL.rendererRenderTarget srenderer SDL.$= Just texture
    SDL.copy srenderer basicTexture Nothing Nothing  -- overwrite last content
    -- Mono overlay rendered last, because more likely to come after
    -- the proportional one and so to have a warning message about overrun
    -- that needs to be overlaid on top of the proportional overlay.
    drawPropOverlay $ singlePropOverlay curFrame
    drawSquareOverlay $ singleSquareOverlay curFrame
    drawMonoOverlay $ singleMonoOverlay curFrame
    writeIORef spreviousFrame curFrame
    SDL.rendererRenderTarget srenderer SDL.$= Nothing
    SDL.copy srenderer texture Nothing Nothing  -- overwrite the backbuffer
    SDL.present srenderer
    -- We can't print screen in @display@ due to thread-unsafety.
    when sprintEachScreen $ printScreen sess

-- It can't seem to cope with SDL_PIXELFORMAT_INDEX8, so we are stuck
-- with huge bitmaps.
printScreen :: FrontendSession -> IO ()
printScreen FrontendSession{..} = do
  dataDir <- appDataDir
  tryCreateDir dataDir
  tryCreateDir $ dataDir </> "screenshots"
  utcTime <- getCurrentTime
  timezone <- getTimeZone utcTime
  let unspace = map $ \c -> case c of  -- prevent the need for backquoting
        ' ' -> '_'
        ':' -> '.'
        _ -> c
      dateText = unspace $ take 25 $ show $ utcToLocalTime timezone utcTime
      fileName = dataDir </> "screenshots" </> "prtscn" <> dateText <.> "bmp"
      SDL.Internal.Types.Renderer renderer = srenderer
  Vect.V2 sw sh <- SDL.get $ SDL.windowSize swindow
  ptrOut <- SDL.Raw.Video.createRGBSurface 0 sw sh 32 0 0 0 0
  surfaceOut <- peek ptrOut
  void $ SDL.Raw.Video.renderReadPixels
    renderer
    nullPtr
    SDL.Raw.Enum.SDL_PIXELFORMAT_ARGB8888
    (SDL.Raw.Types.surfacePixels surfaceOut)
    (sw * 4)
  withCString fileName $ \fileNameCString ->
    void $! SDL.Raw.Video.saveBMP ptrOut fileNameCString
  SDL.Raw.Video.freeSurface ptrOut

-- | Translates modifiers to our own encoding.
modTranslate :: SDL.KeyModifier -> K.Modifier
modTranslate m =
  modifierTranslate
    (SDL.keyModifierLeftCtrl m || SDL.keyModifierRightCtrl m)
    (SDL.keyModifierLeftShift m || SDL.keyModifierRightShift m)
    (SDL.keyModifierLeftAlt m
     || SDL.keyModifierRightAlt m
     || SDL.keyModifierAltGr m
     || SDL.keyModifierLeftGUI m
     || SDL.keyModifierRightGUI m)
    False

keyTranslate :: Bool -> SDL.Keycode -> K.Key
keyTranslate shiftPressed n = case n of
  KeycodeEscape     -> K.Esc
  KeycodeReturn     -> K.Return
  KeycodeBackspace  -> K.BackSpace
  KeycodeTab        -> if shiftPressed then K.BackTab else K.Tab
  KeycodeSpace      -> K.Space
  KeycodeExclaim -> K.Char '!'
  KeycodeQuoteDbl -> K.Char '"'
  KeycodeHash -> K.Char '#'
  KeycodePercent -> K.Char '%'
  KeycodeDollar -> K.Char '$'
  KeycodeAmpersand -> K.Char '&'
  KeycodeQuote -> if shiftPressed then K.Char '"' else K.Char '\''
  KeycodeLeftParen -> K.Char '('
  KeycodeRightParen -> K.Char ')'
  KeycodeAsterisk -> K.Char '*'
  KeycodePlus -> K.Char '+'
  KeycodeComma -> if shiftPressed then K.Char '<' else K.Char ','
  KeycodeMinus -> if shiftPressed then K.Char '_' else K.Char '-'
  KeycodePeriod -> if shiftPressed then K.Char '>' else K.Char '.'
  KeycodeSlash -> if shiftPressed then K.Char '?' else K.Char '/'
  Keycode1 -> if shiftPressed then K.Char '!' else K.Char '1'
  Keycode2 -> if shiftPressed then K.Char '@' else K.Char '2'
  Keycode3 -> if shiftPressed then K.Char '#' else K.Char '3'
  Keycode4 -> if shiftPressed then K.Char '$' else K.Char '4'
  Keycode5 -> if shiftPressed then K.Char '%' else K.Char '5'
  Keycode6 -> if shiftPressed then K.Char '^' else K.Char '6'
  Keycode7 -> if shiftPressed then K.Char '&' else K.Char '7'
  Keycode8 -> if shiftPressed then K.Char '*' else K.Char '8'
  Keycode9 -> if shiftPressed then K.Char '(' else K.Char '9'
  Keycode0 -> if shiftPressed then K.Char ')' else K.Char '0'
  KeycodeColon -> K.Char ':'
  KeycodeSemicolon -> if shiftPressed then K.Char ':' else K.Char ';'
  KeycodeLess -> K.Char '<'
  KeycodeEquals -> if shiftPressed then K.Char '+' else K.Char '='
  KeycodeGreater -> K.Char '>'
  KeycodeQuestion -> K.Char '?'
  KeycodeAt -> K.Char '@'
  KeycodeLeftBracket -> if shiftPressed then K.Char '{' else K.Char '['
  KeycodeBackslash -> if shiftPressed then K.Char '|' else K.Char '\\'
  KeycodeRightBracket -> if shiftPressed then K.Char '}' else K.Char ']'
  KeycodeCaret -> K.Char '^'
  KeycodeUnderscore -> K.Char '_'
  KeycodeBackquote -> if shiftPressed then K.Char '~' else K.Char '`'
  Keycode 167      -> if shiftPressed then K.Char '~' else K.Char '`'
    -- on some keyboards the key below ESC is paragraph and its scancode is 167
    -- and moreover SDL sometimes gives this code even on normal keyboards
  KeycodeUp         -> K.Up
  KeycodeDown       -> K.Down
  KeycodeLeft       -> K.Left
  KeycodeRight      -> K.Right
  KeycodeHome       -> K.Home
  KeycodeEnd        -> K.End
  KeycodePageUp     -> K.PgUp
  KeycodePageDown   -> K.PgDn
  KeycodeInsert     -> K.Insert
  KeycodeDelete     -> K.Delete
  KeycodePrintScreen -> K.PrintScreen
  KeycodeClear -> K.Begin
  KeycodeKPClear -> K.Begin
  KeycodeKPDivide   -> if shiftPressed then K.Char '?' else K.Char '/'
                         -- KP and normal are merged here
  KeycodeKPMultiply -> K.Char '*'  -- KP and normal are merged here
  KeycodeKPMinus    -> K.Char '-'  -- KP and normal are merged here
  KeycodeKPPlus     -> K.Char '+'  -- KP and normal are merged here
  KeycodeKPEnter    -> K.Return
  KeycodeKPEquals   -> K.Return  -- in case of some funny layouts
  KeycodeKP1 -> if shiftPressed then K.KP '1' else K.End
  KeycodeKP2 -> if shiftPressed then K.KP '2' else K.Down
  KeycodeKP3 -> if shiftPressed then K.KP '3' else K.PgDn
  KeycodeKP4 -> if shiftPressed then K.KP '4' else K.Left
  KeycodeKP5 -> if shiftPressed then K.KP '5' else K.Begin
  KeycodeKP6 -> if shiftPressed then K.KP '6' else K.Right
  KeycodeKP7 -> if shiftPressed then K.KP '7' else K.Home
  KeycodeKP8 -> if shiftPressed then K.KP '8' else K.Up
  KeycodeKP9 -> if shiftPressed then K.KP '9' else K.PgUp
  KeycodeKP0 -> if shiftPressed then K.KP '0' else K.Insert
  KeycodeKPPeriod -> K.Char '.'  -- dot and comma are merged here
  KeycodeKPComma  -> K.Char '.'  -- to sidestep national standards
  KeycodeF1       -> K.Fun 1
  KeycodeF2       -> K.Fun 2
  KeycodeF3       -> K.Fun 3
  KeycodeF4       -> K.Fun 4
  KeycodeF5       -> K.Fun 5
  KeycodeF6       -> K.Fun 6
  KeycodeF7       -> K.Fun 7
  KeycodeF8       -> K.Fun 8
  KeycodeF9       -> K.Fun 9
  KeycodeF10      -> K.Fun 10
  KeycodeF11      -> K.Fun 11
  KeycodeF12      -> K.Fun 12
  KeycodeLCtrl    -> K.DeadKey
  KeycodeLShift   -> K.DeadKey
  KeycodeLAlt     -> K.DeadKey
  KeycodeLGUI     -> K.DeadKey
  KeycodeRCtrl    -> K.DeadKey
  KeycodeRShift   -> K.DeadKey
  KeycodeRAlt     -> K.DeadKey
  KeycodeRGUI     -> K.DeadKey
  KeycodeMode     -> K.DeadKey
  KeycodeNumLockClear -> K.DeadKey
  KeycodeUnknown  -> K.Unknown "KeycodeUnknown"
  _ -> let i = fromEnum $ unwrapKeycode n
       in if | 97 <= i && i <= 122
               && shiftPressed -> K.Char $ Char.chr $ i - 32
             | 32 <= i && i <= 126 -> K.Char $ Char.chr i
             | otherwise -> K.Unknown $ show n


sDL_ALPHA_OPAQUE :: Word8
sDL_ALPHA_OPAQUE = 255

blackRGBA :: SDL.V4 Word8
blackRGBA = SDL.V4 0 0 0 sDL_ALPHA_OPAQUE

-- A third of @colorToRGBA Color.BrBlack@ to compensate for the use
-- as background (high area) as opposed to glyphs (usually small area).
greyRGBA :: SDL.V4 Word8
greyRGBA = SDL.V4 0x25 0x1F 0x1F sDL_ALPHA_OPAQUE

-- This code is sadly duplicated from "Game.LambdaHack.Definition.Color".
colorToRGBA :: Color.Color -> SDL.V4 Word8
colorToRGBA Color.Black     = blackRGBA
colorToRGBA Color.Red       = SDL.V4 0xD5 0x05 0x05 sDL_ALPHA_OPAQUE
colorToRGBA Color.Green     = SDL.V4 0x05 0x9D 0x05 sDL_ALPHA_OPAQUE
colorToRGBA Color.Brown     = SDL.V4 0xCA 0x4A 0x05 sDL_ALPHA_OPAQUE
colorToRGBA Color.Blue      = SDL.V4 0x05 0x56 0xF4 sDL_ALPHA_OPAQUE
colorToRGBA Color.Magenta   = SDL.V4 0xAF 0x0E 0xAF sDL_ALPHA_OPAQUE
colorToRGBA Color.Cyan      = SDL.V4 0x05 0x96 0x96 sDL_ALPHA_OPAQUE
colorToRGBA Color.White     = SDL.V4 0xB8 0xBF 0xCB sDL_ALPHA_OPAQUE
colorToRGBA Color.AltWhite  = SDL.V4 0xC4 0xBE 0xB1 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrBlack   = SDL.V4 0x6F 0x5F 0x5F sDL_ALPHA_OPAQUE
colorToRGBA Color.BrRed     = SDL.V4 0xFF 0x55 0x55 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrGreen   = SDL.V4 0x65 0xF1 0x36 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrYellow  = SDL.V4 0xEB 0xD6 0x42 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrBlue    = SDL.V4 0x4D 0x98 0xF4 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrMagenta = SDL.V4 0xFF 0x77 0xFF sDL_ALPHA_OPAQUE
colorToRGBA Color.BrCyan    = SDL.V4 0x52 0xF4 0xE5 sDL_ALPHA_OPAQUE
colorToRGBA Color.BrWhite   = SDL.V4 0xFF 0xFF 0xFF sDL_ALPHA_OPAQUE
