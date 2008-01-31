module Display.Gtk
  (startup, shutdown,
   display, nextEvent, setBG, setFG, Session, blue, magenta, attr) where

import Control.Monad
import Control.Concurrent
import Graphics.UI.Gtk hiding (Attr)
import Data.List as L

import Level

data Session =
  Session {
    schan :: Chan String,
    stag  :: TextTag,
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
    set tt [ textTagBackground := "#FF0000" ]

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
    let black = Color minBound minBound minBound
    let white = Color maxBound maxBound maxBound
    widgetModifyBase tv StateNormal black
    widgetModifyText tv StateNormal white

    ec <- newChan 
    forkIO $ k (Session ec tt tb)
    
    onKeyPress tv (\ e -> writeChan ec (eventKeyName e) >> yield >> return True)

    idleAdd (yield >> return True) priorityDefaultIdle
    onDestroy w mainQuit -- set quit handler
    widgetShowAll w
    yield
    mainGUI

shutdown _ = mainQuit

display :: Area -> Session -> (Loc -> (Attr, Char)) -> String -> IO ()
display ((y0,x0),(y1,x1)) session f status =
{-
  let img = unlines $
            L.map (L.map (\ (x,y) -> let (a,c) = f (y,x) in c))
            [ [ (x,y) | x <- [x0..x1] ] | y <- [y0..y1] ]
  in  textBufferSetText (sbuf session) (img ++ status)
-}
  do
    ttt <- textBufferGetTagTable (sbuf session)
    tb <- textBufferNew (Just ttt)
    sequence_ [ setTo tb (stag session) (y,x) c a | y <- [y0..y1], x <- [x0..x1], let loc = (y,x), let (a,c) = f (y,x) ]
    textBufferSetText (sbuf session) ""
    iter <- textBufferGetEndIter (sbuf session)
    is <- textBufferGetStartIter tb
    ie <- textBufferGetEndIter tb
    textBufferInsertRange (sbuf session) iter is ie

setTo :: TextBuffer -> TextTag -> Loc -> Char -> Bool -> IO ()
setTo tb tt (ly,lx) x blue =
  do
    iter <- textBufferGetEndIter tb
    when (lx == 0 && ly /= 0) $ textBufferInsert tb iter "\n"
    iter <- textBufferGetEndIter tb
    textBufferInsert tb iter [x]
    when blue $
      do
        ie <- textBufferGetEndIter tb
        ib <- textIterCopy ie
        textIterBackwardChar ib
        textBufferApplyTag tb tt ib ie

nextEvent :: Session -> IO String
nextEvent session = readChan (schan session)

setBG   = id
setFG   = id
blue    = const True
magenta = id
attr    = False

type Attr = Bool
