module Display.Vty
  (displayId, startup, shutdown,
   display, nextEvent, setBold, setBG, setFG, Session,
   white, black, yellow, blue, magenta, red, green, attr, Attr) where

import Graphics.Vty as V
import Data.List as L
import Data.Char
import qualified Data.ByteString as BS

import Geometry

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

nextEvent :: Session -> IO String
nextEvent session =
  do
    e <- V.next_event session
    case e of
      V.EvKey (KASCII '<') [] -> return "less"
      V.EvKey (KASCII '>') [] -> return "greater"
      V.EvKey (KASCII '.') [] -> return "period"
      V.EvKey (KASCII ':') [] -> return "colon"
      V.EvKey (KASCII ',') [] -> return "comma"
      V.EvKey (KASCII ' ') [] -> return "space"
      V.EvKey (KASCII '?') [] -> return "question"
      V.EvKey (KASCII '*') [] -> return "asterisk"
      V.EvKey (KASCII c) []   -> return [c]
      V.EvKey KEsc []         -> return "Escape"
      V.EvKey KEnter []       -> return "Return"
      _                       -> nextEvent session

attr = def_attr

setBold a = with_style a bold

setBG c a = with_back_color a c

setFG c a = with_fore_color a c
