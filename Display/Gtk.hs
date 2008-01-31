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
    stagb :: TextTag,
    stagm :: TextTag,
    sview :: TextView }

startup :: (Session -> IO ()) -> IO ()
startup k =
  do
    initGUI
    w <- windowNew

    -- text attributes
    ttb <- textTagNew Nothing
    ttm <- textTagNew Nothing
    ttt <- textTagTableNew
    textTagTableAdd ttt ttb
    textTagTableAdd ttt ttm
    set ttb [ textTagBackground := "#0000CC" ]
    set ttm [ textTagBackground := "#CC00CC" ]

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
    forkIO $ k (Session ec ttb ttm tv)
    
    onKeyPress tv (\ e -> writeChan ec (eventKeyName e) >> yield >> return True)

    idleAdd (yield >> return True) priorityDefaultIdle
    onDestroy w mainQuit -- set quit handler
    widgetShowAll w
    yield
    mainGUI

shutdown _ = mainQuit

display :: Area -> Session -> (Loc -> (Attr, Char)) -> String -> String -> IO ()
display ((y0,x0),(y1,x1)) session f msg status =
{-
  let img = unlines $
            L.map (L.map (\ (x,y) -> let (a,c) = f (y,x) in c))
            [ [ (x,y) | x <- [x0..x1] ] | y <- [y0..y1] ]
  in  textBufferSetText (sbuf session) (img ++ status)
-}
  do
    sbuf <- textViewGetBuffer (sview session)
    ttt <- textBufferGetTagTable sbuf
    tb <- textBufferNew (Just ttt)
    let text = unlines [ [ snd (f (y,x)) | x <- [x0..x1] ] | y <- [y0..y1] ]
    textBufferSetText tb (msg ++ "\n" ++ text ++ status)
    sequence_ [ setTo tb (stagb session) (stagm session) (y,x) c a | y <- [y0..y1], x <- [x0..x1], let loc = (y,x), let (a,c) = f (y,x) ]
    textViewSetBuffer (sview session) tb
{-
    textBufferSetText sbuf ""
    iter <- textBufferGetEndIter tb
    textBufferInsert tb iter ("\n" ++ status)
    iter <- textBufferGetEndIter sbuf
    is <- textBufferGetStartIter tb
    ie <- textBufferGetEndIter tb
    textBufferInsertRange sbuf iter is ie
-}

setTo :: TextBuffer -> TextTag -> TextTag -> Loc -> Char -> (Maybe Bool) -> IO ()
setTo tb ttb ttm (ly,lx) x bg =
  do
    case bg of
      Nothing -> return ()
      Just x  ->
        do
          ib <- textBufferGetIterAtLineOffset tb ly lx
          ie <- textIterCopy ib
          textIterForwardChar ie
          textBufferApplyTag tb (if x then ttb else ttm) ib ie

nextEvent :: Session -> IO String
nextEvent session = readChan (schan session)

setBG   = id
setFG   = id
blue    = const (Just True)
magenta = const (Just False) 
attr    = Nothing 

type Attr = Maybe Bool
