module Display.Vty
  (displayId, startup, shutdown,
   display, nextEvent, setBold, setBG, setFG, Session,
   white, black, yellow, blue, magenta, red, green, attr, Attr) where

import Graphics.Vty as V
import Data.List as L
import Data.Char
import qualified Data.ByteString as BS

import Geometry
import Keys as K

displayId = "vty"

type Session = V.Vty

startup :: (Session -> IO ()) -> IO ()
startup k = V.mkVty >>= k

display :: Area -> Session -> (Loc -> (Attr, Char)) -> String -> String -> IO ()
display ((y0,x0),(y1,x1)) vty f msg status =
    let img = (foldr (<->) V.empty_image .
               L.map (foldr (<|>) V.empty_image .
                      L.map (\ (x,y) -> let (a,c) = f (y,x) in char a c)))
              [ [ (x,y) | x <- [x0..x1] ] | y <- [y0..y1] ]
    in  V.update vty (pic_for_image
         (utf8_bytestring attr (BS.pack (L.map (fromIntegral . ord) (toWidth (x1 - x0 + 1) msg))) <->
          img <-> 
          utf8_bytestring attr (BS.pack (L.map (fromIntegral . ord) (toWidth (x1 - x0 + 1) status)))))

toWidth :: Int -> String -> String
toWidth n x = take n (x ++ repeat ' ')

keyTranslate :: V.Event -> Maybe K.Key
keyTranslate e =
  case e of
    V.EvKey KEsc []         -> Just K.Esc
    V.EvKey KEnter []       -> Just K.Return
    V.EvKey KUp []          -> Just K.Up
    V.EvKey KDown []        -> Just K.Down
    V.EvKey KLeft []        -> Just K.Left
    V.EvKey KRight []       -> Just K.Right
    V.EvKey KHome []        -> Just K.Home
    V.EvKey KPageUp []      -> Just K.PgUp
    V.EvKey KEnd []         -> Just K.End
    V.EvKey KPageDown []    -> Just K.PgDn
    V.EvKey KBegin []       -> Just K.Begin
    V.EvKey (KASCII c) []   -> Just (K.Char c)
    _                       -> Nothing

nextEvent :: Session -> IO K.Key
nextEvent session =
  do
    e <- V.next_event session
    maybe (nextEvent session) return (keyTranslate e)

attr = def_attr

setBold a = with_style a bold

setBG c a = with_back_color a c

setFG c a = with_fore_color a c
