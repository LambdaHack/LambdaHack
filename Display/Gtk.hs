module Display.Gtk
  (Display.Gtk.Color, Attr, startup, shutdown,
   display, nextEvent, setBG, setFG, Session,
   yellow, blue, magenta, red, green, attr) where

import Control.Monad
import Control.Concurrent
import Graphics.UI.Gtk hiding (Attr)
import Data.List as L
import Data.IORef
import Data.Map as M

import Geometry

data Session =
  Session {
    schan :: Chan String,
    stags :: Map AttrKey TextTag,
    sview :: TextView }

doAttr :: TextTag -> AttrKey -> IO ()
doAttr tt (BG Blue)    = set tt [ textTagBackground := "#0000CC" ]
doAttr tt (BG Magenta) = set tt [ textTagBackground := "#CC00CC" ]
doAttr tt (BG Green)   = set tt [ textTagBackground := "#00CC00" ]
doAttr tt (BG Red)     = set tt [ textTagBackground := "#CC0000" ]
doAttr tt (FG Red)     = set tt [ textTagForeground := "#FF0000" ]
doAttr tt (FG Yellow)  = set tt [ textTagForeground := "#CCCC00" ]
doAttr tt _            = return ()

startup :: (Session -> IO ()) -> IO ()
startup k =
  do
    initGUI
    w <- windowNew

    ttt <- textTagTableNew
    -- text attributes
    tts <- fmap M.fromList $
           mapM (\ c -> do
                          tt <- textTagNew Nothing
                          textTagTableAdd ttt tt
                          doAttr tt c
                          return (c,tt))
                [ x | c <- [minBound .. maxBound], x <- [FG c, BG c]]

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
    widgetModifyFont tv (Just f)
    currentfont <- newIORef f
    onButtonPress tv (\ e -> case e of
                               Button { eventButton = RightButton } ->
                                 do
                                   fsd <- fontSelectionDialogNew "Choose font"
                                   cf <- readIORef currentfont
                                   fd <- fontDescriptionToString cf
                                   fontSelectionDialogSetFontName fsd fd
                                   fontSelectionDialogSetPreviewText fsd "+##@##-...|"
                                   response <- dialogRun fsd
                                   when (response == ResponseOk) $
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

    let black = Color minBound minBound minBound
    let white = Color maxBound maxBound maxBound
    widgetModifyBase tv StateNormal black
    widgetModifyText tv StateNormal white

    ec <- newChan 
    forkIO $ k (Session ec tts tv)
    
    onKeyPress tv (\ e -> writeChan ec (eventKeyName e) >> yield >> return True)

    idleAdd (yield >> return True) priorityDefaultIdle
    onDestroy w mainQuit -- set quit handler
    widgetShowAll w
    yield
    mainGUI

shutdown _ = mainQuit

display :: Area -> Session -> (Loc -> (Attr, Char)) -> String -> String -> IO ()
display ((y0,x0),(y1,x1)) session f msg status =
  do
    sbuf <- textViewGetBuffer (sview session)
    ttt <- textBufferGetTagTable sbuf
    tb <- textBufferNew (Just ttt)
    let text = unlines [ [ snd (f (y,x)) | x <- [x0..x1] ] | y <- [y0..y1] ]
    textBufferSetText tb (msg ++ "\n" ++ text ++ status)
    sequence_ [ setTo tb (stags session) (y,x) a | 
                y <- [y0..y1], x <- [x0..x1], let loc = (y,x), let (a,c) = f (y,x) ]
    textViewSetBuffer (sview session) tb

setTo :: TextBuffer -> Map AttrKey TextTag -> Loc -> Attr -> IO ()
setTo tb tts (ly,lx) a =
  do
    ib <- textBufferGetIterAtLineOffset tb (ly+1) lx
    ie <- textIterCopy ib
    textIterForwardChar ie
    mapM_ (\ c -> textBufferApplyTag tb (tts ! c) ib ie) a

nextEvent :: Session -> IO String
nextEvent session = readChan (schan session)

setBG c = (BG c :)
setFG c = (FG c :)
blue    = Blue
magenta = Magenta
red     = Red
yellow  = Yellow
green   = Green
attr    = []

type Attr = [AttrKey]

data AttrKey =
    FG AttrColor
  | BG AttrColor
  deriving (Eq, Ord)

type Color = AttrColor

data AttrColor =
    Blue
  | Magenta
  | Red
  | Green
  | Yellow
  deriving (Eq, Ord, Enum, Bounded)
