module Display.Gtk
  (startup, shutdown,
   display, nextEvent, setBG, setFG, Session, blue, magenta, attr) where

import Control.Concurrent
import Graphics.UI.Gtk hiding (Attr)
import Data.List as L

import Level

data Session =
  Session {
    schan :: Chan String,
    sbuf  :: TextBuffer }

startup :: (Session -> IO ()) -> IO ()
startup k =
  do
    initGUI
    w <- windowNew

    -- text attributes
    tt <- textTagNew Nothing
    ttt <- textTagTableNew
    textTagTableAdd ttt tt

    -- text buffer
    tb <- textBufferNew (Just ttt)

    -- create text view
    tv <- textViewNewWithBuffer tb
    containerAdd w tv
    textViewSetEditable tv False
    textViewSetCursorVisible tv False

    -- font
    f <- fontDescriptionNew
    fontDescriptionSetFamily f "Monospace"
    widgetModifyFont tv (Just f)
    let black = Color minBound minBound minBound
    let white = Color maxBound maxBound maxBound
    widgetModifyBase tv StateNormal black
    widgetModifyText tv StateNormal white

    ec <- newChan 
    forkIO $ k (Session ec tb)
    
    onKeyPress tv (\ e -> writeChan ec (eventKeyName e) >> yield >> return True)

    idleAdd (yield >> return True) priorityDefaultIdle
    onDestroy w mainQuit -- set quit handler
    widgetShowAll w
    yield
    mainGUI

shutdown _ = mainQuit

display :: Area -> Session -> (Loc -> (Attr, Char)) -> String -> IO ()
display ((y0,x0),(y1,x1)) session f status =
  let img = unlines $
            L.map (L.map (\ (x,y) -> let (a,c) = f (y,x) in c))
            [ [ (x,y) | x <- [x0..x1] ] | y <- [y0..y1] ]
  in  textBufferSetText (sbuf session) (img ++ status)

nextEvent :: Session -> IO String
nextEvent session = readChan (schan session)

setBG   = id
setFG   = id
blue    = id
magenta = id
attr    = ()

type Attr = ()
