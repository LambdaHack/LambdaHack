module Display.Gtk
  (displayId, startup, shutdown, display, nextEvent, Session) where

import Control.Monad
import Control.Concurrent
import Graphics.UI.Gtk.Gdk.Events  -- TODO: replace, deprecated
import Graphics.UI.Gtk
import qualified Data.List as L
import Data.IORef
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

import Geometry
import qualified Keys as K (Key(..), keyTranslate)
import qualified Color

displayId :: String
displayId = "gtk"

data Session = Session
  { schan :: Chan String
  , stags :: M.Map Color.Attr TextTag
  , sview :: TextView
  }

startup :: (Session -> IO ()) -> IO ()
startup k =
  do
    -- initGUI
    unsafeInitGUIForThreadedRTS
    w <- windowNew

    ttt <- textTagTableNew
    -- text attributes
    tts <- fmap M.fromList $
           mapM (\ ak -> do
                           tt <- textTagNew Nothing
                           textTagTableAdd ttt tt
                           doAttr tt ak
                           return (ak, tt))
                [ (f, b) | f <- [minBound..maxBound], b <- Color.legalBG ]

    -- text buffer
    tb <- textBufferNew (Just ttt)
    textBufferSetText tb (unlines (replicate 25 (replicate 80 ' ')))

    -- create text view
    tv <- textViewNewWithBuffer tb
    containerAdd w tv
    textViewSetEditable tv False
    textViewSetCursorVisible tv False

    -- font
    f <- fontDescriptionNew
    fontDescriptionSetFamily f "Monospace"
    fontDescriptionSetSize f 12
    widgetModifyFont tv (Just f)
    currentfont <- newIORef f
    onButtonPress tv (\ e -> case e of
                               Button { Graphics.UI.Gtk.Gdk.Events.eventButton = RightButton } ->
                                 do
                                   fsd <- fontSelectionDialogNew "Choose font"
                                   cf  <- readIORef currentfont
                                   fds <- fontDescriptionToString cf
                                   fontSelectionDialogSetFontName fsd fds
                                   fontSelectionDialogSetPreviewText fsd "+##@##-...|"
                                   resp <- dialogRun fsd
                                   when (resp == ResponseOk) $
                                     do
                                       fn <- fontSelectionDialogGetFontName fsd
                                       case fn of
                                         Just fn' -> do
                                                       fd <- fontDescriptionFromString fn'
                                                       writeIORef currentfont fd
                                                       widgetModifyFont tv (Just fd)
                                         Nothing  -> return ()
                                   widgetDestroy fsd
                                   return True
                               _ -> return False)

    let black = Color minBound minBound minBound  -- Color.defBG == Color.Black
        white = Color 0xC500 0xBC00 0xB800        -- Color.defFG == Color.White
    widgetModifyBase tv StateNormal black
    widgetModifyText tv StateNormal white

    ec <- newChan
    forkIO $ k (Session ec tts tv)

    onKeyPress tv
      (\ e -> do
          writeChan ec (Graphics.UI.Gtk.Gdk.Events.eventKeyName e)
          return True)

    onDestroy w mainQuit -- set quit handler
    widgetShowAll w
    yield
    mainGUI

shutdown :: Session -> IO ()
shutdown _ = mainQuit

display :: Area -> Session -> (Loc -> (Color.Attr, Char)) -> String -> String
           -> IO ()
display (x0, y0, x1, y1) session f msg status =
  postGUIAsync $ do
    tb <- textViewGetBuffer (sview session)
    let fLine y = let (as, cs) = unzip [ f (toLoc (y, x)) | x <- [x0..x1] ]
                  in  ((y, as), BS.pack cs)
        memo  = L.map fLine [y0..y1]
        attrs = L.map fst memo
        chars = L.map snd memo
        bs    = [BS.pack msg, BS.pack "\n", BS.unlines chars, BS.pack status]
    textBufferSetByteString tb (BS.concat bs)
    mapM_ (setTo tb (stags session) x0) attrs

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

-- | reads until a non-dead key encountered
readUndeadChan :: Chan String -> IO String
readUndeadChan ch =
  do
    x <- readChan ch
    if dead x then readUndeadChan ch else return x
      where
        dead x =
          case x of
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

nextEvent :: Session -> IO K.Key
nextEvent session =
  do
    e <- readUndeadChan (schan session)
    return (K.keyTranslate e)

doAttr :: TextTag -> Color.Attr -> IO ()
doAttr tt (fg, bg)
  | (fg, bg) == Color.defaultAttr = return ()
  | fg == Color.defFG = set tt [textTagBackground := Color.colorToRGB bg]
  | bg == Color.defBG = set tt [textTagForeground := Color.colorToRGB fg]
  | otherwise         = set tt [textTagForeground := Color.colorToRGB fg,
                                textTagBackground := Color.colorToRGB bg]
