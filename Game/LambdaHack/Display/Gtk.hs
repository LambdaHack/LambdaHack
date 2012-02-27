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
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception (finally)
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk hiding (Point)
import qualified Data.List as L
import Data.IORef
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

import Game.LambdaHack.Utils.LQueue
import qualified Game.LambdaHack.Key as K (Key(..), keyTranslate, Modifier(..))
import qualified Game.LambdaHack.Color as Color

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { sview       :: TextView                    -- ^ the widget to draw to
  , stags       :: M.Map Color.Attr TextTag    -- ^ text color tags for fg/bg
  , schanKey    :: Chan (K.Key, K.Modifier)    -- ^ channel for keyboard input
  , schanScreen :: LQueue Color.SingleFrame    -- ^ channel for screen output
  , slastFull   :: IORef (Color.SingleFrame, Bool)
      -- ^ most recent not empty, not repeated frame and if any followed
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "gtk"

-- | Spawns the gtk input and output thread, which spawns all the other
-- required threads. We crate a separate thread for gtk to minimize
-- communication with the heavy main thread. The other threads have to be
-- spawned after gtk is initialized, because they call @postGUIAsync@,
-- and need 'sview' and 'stags'.
startup :: String -> (FrontendSession -> IO ()) -> IO ()
startup configFont k = do
  mv <- newEmptyMVar
  -- Fork the gtk input and output thread.
  void $ forkIO (runGtk configFont k `finally` putMVar mv ())
  takeMVar mv

-- | Sets up and starts the main GTK loop providing input and output.
runGtk :: String ->  (FrontendSession -> IO ()) -> IO ()
runGtk configFont k = do
  -- Init GUI.
  unsafeInitGUIForThreadedRTS
  -- Text attributes.
  ttt <- textTagTableNew
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
  -- Create text view. TODO: use GtkLayout or DrawingArea instead of TextView?
  sview <- textViewNewWithBuffer tb
  textViewSetEditable sview False
  textViewSetCursorVisible sview False
  -- Set up the channel for keyboard input.
  schanKey <- newChan
  -- Set up the channel for drawing frames.
  schanScreen <- newLQueue
  -- Create the session record.
  slastFull <- newIORef (Color.NoFrame, False)
  let sess = FrontendSession{..}
  -- Fork the game logic thread.
  forkIO $ k sess
   -- Install a function that periodically draws a frame from a queue, if any.
  timeoutAdd (drawFrame sess) (1000 `div` maxFps)
  -- Fill the keyboard channel.
  sview `on` keyPressEvent $ do
    n <- eventKeyName
    mods <- eventModifier
    let !key = K.keyTranslate n
        !modifier = modifierTranslate mods
    liftIO $ do
      unless (deadKey n) $ do
        -- Key pressed. Drop all the old frames.
        clearLQueue schanScreen
        -- Store the key in the channel.
        writeChan schanKey (key, modifier)
      return True
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
  -- Set up the main window.
  w <- windowNew
  containerAdd w sview
  onDestroy w mainQuit
  widgetShowAll w
  -- Show the window.
  mainGUI

-- | Shuts down the frontend cleanly.
shutdown :: FrontendSession -> IO ()
shutdown _ = mainQuit

-- | Output to the screen via the frontend.
display :: FrontendSession    -- ^ frontend session data
        -> Color.SingleFrame  -- ^ the screen frame to draw
        -> IO Bool
display _ Color.NoFrame = return True  -- timeout requested
display FrontendSession{sview, stags} Color.SingleFrame{..} = do  -- new frame
  tb <- textViewGetBuffer sview
  let attrs = L.zip [0..] $ L.map (L.map fst) sflevel
      chars = L.map (BS.pack . L.map snd) sflevel
      bs    = [BS.pack sfTop, BS.pack "\n", BS.unlines chars, BS.pack sfBottom]
  textBufferSetByteString tb (BS.concat bs)
  mapM_ (setTo tb stags 0) attrs
  return True

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

-- TODO: configure
-- | Maximal frames per second.
maxFps :: Int
maxFps = 30

-- | Draw a frame from a channel, if any.
drawFrame :: FrontendSession -> IO Bool
drawFrame sess@FrontendSession{schanScreen} = do
  -- Try to get a frame. Don't block.
  mframe <- tryReadLQueue schanScreen
  maybe (return True) (display sess) mframe

-- | Input key via the frontend.
nextEvent :: FrontendSession -> IO (K.Key, K.Modifier)
nextEvent FrontendSession{schanKey} = do
  km <- readChan schanKey
  return km

-- | Add a game screen frame to the frame drawing channel.
pushFrame :: FrontendSession -> Color.SingleFrame -> IO ()
pushFrame FrontendSession{schanScreen, slastFull} frame = do
  (lastFrame, anyFollowed) <- readIORef slastFull
  let nextFrame =
        if frame == lastFrame
        then Color.NoFrame  -- no sense repeating
        else frame
  unless (nextFrame == Color.NoFrame && anyFollowed) $ do  -- old news
    writeLQueue schanScreen nextFrame
    if nextFrame == Color.NoFrame
      then writeIORef slastFull (lastFrame, True)
      else writeIORef slastFull (nextFrame, False)

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
