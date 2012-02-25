-- | Text frontend based on Gtk.
module Game.LambdaHack.Display.Gtk
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , pushFrame, nextEvent
    -- * Frontend administration tools
  , frontendName, startup, shutdown
  ) where

import Control.Monad
import Control.Monad.STM
import Control.Monad.Reader
import Control.Concurrent hiding (Chan)
import Control.Concurrent.STM.TChan
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk hiding (Point)
import qualified Data.List as L
import Data.IORef
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

import qualified Game.LambdaHack.Key as K (Key(..), keyTranslate, Modifier(..))
import qualified Game.LambdaHack.Color as Color

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { sview       :: TextView                    -- ^ the widget to draw to
  , stags       :: M.Map Color.Attr TextTag    -- ^ text color tags for fg/bg
  , schanKey    :: TChan (K.Key, K.Modifier)   -- ^ channel for keyboard input
  , schanScreen :: TChan Color.SingleFrame     -- ^ channel for screen output
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "gtk"

-- | Starts the main program loop using the frontend input and output.
startup :: String -> (FrontendSession -> IO ()) -> IO ()
startup configFont k = do
  -- Init GUI.
  unsafeInitGUIForThreadedRTS
  w <- windowNew
  ttt <- textTagTableNew
  -- Text attributes.
  stags <- fmap M.fromList $
             mapM (\ ak -> do
                      tt <- textTagNew Nothing
                      textTagTableAdd ttt tt
                      doAttr tt ak
                      return (ak, tt))
               [ Color.Attr{fg, bg}
               | fg <- [minBound..maxBound], bg <- Color.legalBG ]
  -- Text buffer.
  tb <- textBufferNew (Just ttt)
  textBufferSetText tb (unlines (replicate 25 (replicate 80 ' ')))
  -- Create text view. TODO: use GtkLayout or DrawingArea instead of TextView?
  sview <- textViewNewWithBuffer tb
  containerAdd w sview
  textViewSetEditable sview False
  textViewSetCursorVisible sview False
  -- Set the font specified in config, if any.
  f <- fontDescriptionFromString configFont
  widgetModifyFont sview (Just f)
  -- Prepare font chooser dialog.
  currentfont <- newIORef f
  sview `on` buttonPressEvent $ do
    but <- eventButton
    liftIO $ case but of
      RightButton -> do
        fsd <- fontSelectionDialogNew "Choose font"
        cf  <- readIORef currentfont  -- TODO: "Terminus,Monospace" fails
        fds <- fontDescriptionToString cf
        fontSelectionDialogSetFontName fsd fds
        fontSelectionDialogSetPreviewText fsd "eee...@.##+##"
        resp <- dialogRun fsd
        when (resp == ResponseOk) $ do
          fn <- fontSelectionDialogGetFontName fsd
          case fn of
            Just fn' -> do
              fd <- fontDescriptionFromString fn'
              writeIORef currentfont fd
              widgetModifyFont sview (Just fd)
            Nothing  -> return ()
        widgetDestroy fsd
        return True
      _ -> return False
  -- Modify default colours.
  let black = Color minBound minBound minBound  -- Color.defBG == Color.Black
      white = Color 0xC500 0xBC00 0xB800        -- Color.defFG == Color.White
  widgetModifyBase sview StateNormal black
  widgetModifyText sview StateNormal white
  -- Set up the channel for keyboard input.
  schanKey <- newTChanIO
  -- Set up the channel for drawing frames.
  schanScreen <- newTChanIO
  let sess = FrontendSession{..}
  -- Fork the game logic thread.
  forkIO $ k sess
  -- Fork the frame drawing thread.
  forkIO $ waitForFrames sess
  -- Fill the keyboard channel.
  sview `on` keyPressEvent $ do
    n <- eventKeyName
    mods <- eventModifier
    liftIO $ do
      unless (deadKey n) $ atomically $ do
        -- Key pressed. Drop all the old frames.
        flushChan schanScreen
        let !key = K.keyTranslate n
            !modifier = modifierTranslate mods
        void $ writeTChan schanKey (key, modifier)
      return True
  -- Set up the quit handler and widgets.
  onDestroy w mainQuit
  widgetShowAll w
  -- Wait with showing the window until there's anything to draw.
  yield
  -- Show the window.
  mainGUI

-- | Shuts down the frontend cleanly.
shutdown :: FrontendSession -> IO ()
shutdown _ = mainQuit

-- | Output to the screen via the frontend.
display :: FrontendSession    -- ^ frontend session data
        -> Color.SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display _ Color.NoFrame = return ()
display FrontendSession{sview, stags} Color.SingleFrame{..} = do
  tb <- textViewGetBuffer sview
  let attrs = L.zip [0..] $ L.map (L.map fst) sflevel
      chars = L.map (BS.pack . L.map snd) sflevel
      bs    = [BS.pack sfTop, BS.pack "\n", BS.unlines chars, BS.pack sfBottom]
  textBufferSetByteString tb (BS.concat bs)
  mapM_ (setTo tb stags 0) attrs

setTo :: TextBuffer -> M.Map Color.Attr TextTag -> Int -> (Int, [Color.Attr])
      -> IO ()
setTo _  _   _  (_,  [])         = return ()
setTo tb tts lx (ly, attr:attrs) = do
  ib <- textBufferGetIterAtLineOffset tb (ly + 1) lx
  ie <- textIterCopy ib
  let setIter :: Color.Attr -> Int -> [Color.Attr] -> IO ()
      setIter previous repetitions [] = do
        textIterForwardChars ie repetitions
        when (previous /= Color.defaultAttr) $
          textBufferApplyTag tb (tts M.! previous) ib ie
      setIter previous repetitions (a:as)
        | a == previous =
            setIter a (repetitions + 1) as
        | otherwise = do
            textIterForwardChars ie repetitions
            when (previous /= Color.defaultAttr) $
              textBufferApplyTag tb (tts M.! previous) ib ie
            textIterForwardChars ib repetitions
            setIter a 1 as
  setIter attr 1 attrs

flushChan :: TChan a -> STM ()
flushChan chan = do
  b <- isEmptyTChan chan
  unless b $ do
    _ <- readTChan chan
    flushChan chan

-- TODO: configure
-- | Maximal frames per second.
maxFps :: Int
maxFps = 10

-- TODO: perhaps rewrite all with no STM, but a single MVar.
-- | Wait on the channel and draw frames on demand.
waitForFrames :: FrontendSession -> IO ()
waitForFrames sess@FrontendSession{schanScreen, schanKey} = do
  -- Wait until frame received.
  mframe <- atomically $ readTChan schanScreen
  -- Don't wait until frame drawn.
  case mframe of
    Color.NoFrame -> return ()
    frame   -> postGUIAsync $ display sess frame
  b <- atomically $ isEmptyTChan schanKey
  -- Don't delay if the user acts (and drops old frames, if there were any).
  when b $ threadDelay $ 1000000 `div` maxFps
  waitForFrames sess

-- | Input key via the frontend.
nextEvent :: FrontendSession -> IO (K.Key, K.Modifier)
nextEvent FrontendSession{schanKey} = do
  km <- atomically $ readTChan schanKey
  return km

-- | Add a game screen frame to the drawing channel queue.
pushFrame :: FrontendSession -> Color.SingleFrame -> IO ()
pushFrame FrontendSession{schanScreen} mframe = do
  atomically $ writeTChan schanScreen mframe
  -- Help GTK to start drawing it ASAP.
  -- TODO: instead wait until any frame (not necessarily this one)
  -- is fully evaluated and is being drawn. Or better, do anything
  -- only during the delay.
  yield

-- | Tells a dead key.
deadKey :: String -> Bool
deadKey x = case x of
  "Shift_R"          -> True
  "Shift_L"          -> True
  "Control_L"        -> True
  "Control_R"        -> True
  "Super_L"          -> True
  "Super_R"          -> True
  "Menu"             -> True
  "Alt_L"            -> True
  "Alt_R"            -> True
  "ISO_Level2_Shift" -> True
  "ISO_Level3_Shift" -> True
  "ISO_Level2_Latch" -> True
  "ISO_Level3_Latch" -> True
  "Num_Lock"         -> True
  "Caps_Lock"        -> True
  _                  -> False

-- | Translates modifiers to our own encoding.
modifierTranslate :: [Modifier] -> K.Modifier
modifierTranslate mods =
  if Control `elem` mods then K.Control else K.NoModifier

doAttr :: TextTag -> Color.Attr -> IO ()
doAttr tt attr@Color.Attr{fg, bg}
  | attr == Color.defaultAttr = return ()
  | fg == Color.defFG = set tt [textTagBackground := Color.colorToRGB bg]
  | bg == Color.defBG = set tt [textTagForeground := Color.colorToRGB fg]
  | otherwise         = set tt [textTagForeground := Color.colorToRGB fg,
                                textTagBackground := Color.colorToRGB bg]
