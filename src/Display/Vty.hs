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

-- | translates the vty key code to the standard GTK key code
keyTranslate :: V.Event -> String
keyTranslate e =
  case e of
    V.EvKey (KASCII '<') [] -> "less"
    V.EvKey (KASCII '>') [] -> "greater"
    V.EvKey (KASCII '.') [] -> "period"
    V.EvKey (KASCII ':') [] -> "colon"
    V.EvKey (KASCII ',') [] -> "comma"
    V.EvKey (KASCII ' ') [] -> "space"
    V.EvKey (KASCII '?') [] -> "question"
    V.EvKey (KASCII '*') [] -> "asterisk"
    V.EvKey KEsc []         -> "Escape"
    V.EvKey KEnter []       -> "Return"
    V.EvKey KUp []          -> "KP_Up"
    V.EvKey KDown []        -> "KP_Down"
    V.EvKey KLeft []        -> "KP_Left"
    V.EvKey KRight []       -> "KP_Right"
    V.EvKey KHome []        -> "KP_Home"
    V.EvKey KPageUp []      -> "KP_Page_Up"
    V.EvKey KEnd []         -> "KP_End"
    V.EvKey KPageDown []    -> "KP_Page_Down"
    V.EvKey KBegin []       -> "KP_Begin"
    V.EvKey (KASCII c) []   -> [c]
    _ -> []

nextEvent :: Session -> IO String
nextEvent session =
  do
    e <- V.next_event session
    let s = keyTranslate e in
      if L.null s
      then nextEvent session
      else return s

attr = def_attr

setBold a = with_style a bold

setBG c a = with_back_color a c

setFG c a = with_fore_color a c
