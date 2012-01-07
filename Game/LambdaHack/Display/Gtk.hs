-- | Text frontend based on Gtk.
module Game.LambdaHack.Display.Gtk
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , display, nextEvent
    -- * Frontend administration tools
  , frontendName, startup, shutdown
  ) where

import Control.Monad
import Control.Concurrent
import Graphics.UI.Gtk.Gdk.Events  -- TODO: replace, deprecated
import Graphics.UI.Gtk
import qualified Data.List as L
import Data.IORef
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

import Game.LambdaHack.Area
import Game.LambdaHack.Loc
import Game.LambdaHack.Geometry
import qualified Game.LambdaHack.Keys as K (Key(..), keyTranslate)
import qualified Game.LambdaHack.Color as Color

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { sview :: TextView                  -- ^ the widget to draw to
  , stags :: M.Map Color.Attr TextTag  -- ^ text tags for fore/back colour pairs
  , schan :: Chan String               -- ^ the channel that carries input
  }

-- | The name of the frontend for the user's information.
frontendName :: String
frontendName = "gtk"

-- | Starts the main program loop using the frontend input and output.
startup :: (FrontendSession -> IO ()) -> IO ()
startup k = do
  -- initGUI
  unsafeInitGUIForThreadedRTS
  w <- windowNew
  ttt <- textTagTableNew
  -- text attributes
  stags <- fmap M.fromList $
             mapM (\ ak -> do
                      tt <- textTagNew Nothing
                      textTagTableAdd ttt tt
                      doAttr tt ak
                      return (ak, tt))
               [ (f, b) | f <- [minBound..maxBound], b <- Color.legalBG ]
  -- text buffer
  tb <- textBufferNew (Just ttt)
  textBufferSetText tb (unlines (replicate 25 (replicate 80 ' ')))
  -- create text view, TODO: use GtkLayout or DrawingArea instead of TextView?
  sview <- textViewNewWithBuffer tb
  containerAdd w sview
  textViewSetEditable sview False
  textViewSetCursorVisible sview False
  -- font
  f <- fontDescriptionNew
  fontDescriptionSetFamily f "Monospace"
  fontDescriptionSetSize f 12
  widgetModifyFont sview (Just f)
  currentfont <- newIORef f
  let buttonPressHandler e = case e of
        Button { Graphics.UI.Gtk.Gdk.Events.eventButton = RightButton } -> do
          fsd <- fontSelectionDialogNew "Choose font"
          cf  <- readIORef currentfont
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
  onButtonPress sview buttonPressHandler
  -- modify default colours
  let black = Color minBound minBound minBound  -- Color.defBG == Color.Black
      white = Color 0xC500 0xBC00 0xB800        -- Color.defFG == Color.White
  widgetModifyBase sview StateNormal black
  widgetModifyText sview StateNormal white
  -- set up the channel for communication
  schan <- newChan
  forkIO $ k FrontendSession{..}
  -- fill the channel
  onKeyPress sview
    (\ e -> do
        writeChan schan (Graphics.UI.Gtk.Gdk.Events.eventKeyName e)
        return True)
  -- set quit handler
  onDestroy w mainQuit
  -- start it up
  widgetShowAll w
  yield
  mainGUI

-- | Shuts down the frontend cleanly.
shutdown :: FrontendSession -> IO ()
shutdown _ = mainQuit

-- | Output to the screen via the frontend.
display :: Area                         -- ^ the size of the drawn area
        -> FrontendSession              -- ^ current session data
        -> (Loc -> (Color.Attr, Char))  -- ^ the content of the screen
        -> String                       -- ^ an extra line to show at the top
        -> String                       -- ^ an extra line to show at the bottom
        -> IO ()
display (x0, y0, x1, y1) FrontendSession{sview, stags} f msg status =
  postGUIAsync $ do
    tb <- textViewGetBuffer sview
    let xsize  = x1 - x0 + 1
        fLine y = let (as, cs) = unzip [ f (toLoc xsize (x, y))
                                       | x <- [x0..x1] ]
                  in ((y, as), BS.pack cs)
        memo  = L.map fLine [y0..y1]
        attrs = L.map fst memo
        chars = L.map snd memo
        bs    = [BS.pack msg, BS.pack "\n", BS.unlines chars, BS.pack status]
    textBufferSetByteString tb (BS.concat bs)
    mapM_ (setTo tb stags x0) attrs

setTo :: TextBuffer -> M.Map Color.Attr TextTag -> X -> (Y, [Color.Attr])
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

-- | Input key via the frontend.
nextEvent :: FrontendSession -> IO K.Key
nextEvent sess = do
  e <- readUndeadChan (schan sess)
  return (K.keyTranslate e)

-- | Reads until a non-dead key encountered.
readUndeadChan :: Chan String -> IO String
readUndeadChan ch = do
  x <- readChan ch
  if dead x then readUndeadChan ch else return x
 where
  dead x = case x of
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

doAttr :: TextTag -> Color.Attr -> IO ()
doAttr tt (fg, bg)
  | (fg, bg) == Color.defaultAttr = return ()
  | fg == Color.defFG = set tt [textTagBackground := Color.colorToRGB bg]
  | bg == Color.defBG = set tt [textTagForeground := Color.colorToRGB fg]
  | otherwise         = set tt [textTagForeground := Color.colorToRGB fg,
                                textTagBackground := Color.colorToRGB bg]
