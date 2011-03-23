module Display.Vty
  (displayId, startup, shutdown,
   display, nextEvent, setBG, setFG, attr, Session,
   black, red, green, yellow, blue, magenta, cyan, white,
   bright_black, bright_red, bright_green, bright_yellow,
   bright_blue, bright_magenta, bright_cyan, bright_white,
   Attr, AttrColor) where

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
    V.EvKey KEsc []          -> Just K.Esc
    V.EvKey KEnter []        -> Just K.Return
    V.EvKey (KASCII '\t') [] -> Just K.Tab
    V.EvKey KUp []           -> Just K.Up
    V.EvKey KDown []         -> Just K.Down
    V.EvKey KLeft []         -> Just K.Left
    V.EvKey KRight []        -> Just K.Right
    V.EvKey KHome []         -> Just K.Home
    V.EvKey KPageUp []       -> Just K.PgUp
    V.EvKey KEnd []          -> Just K.End
    V.EvKey KPageDown []     -> Just K.PgDn
    V.EvKey KBegin []        -> Just K.Begin
    -- No KP_ keys in vty; see https://github.com/coreyoconnor/vty/issues/8
    -- For now, movement keys are more important than hero selection:
    V.EvKey (KASCII c) []
      | c `elem` ['1'..'9']  -> Just (K.KP c)
      | otherwise            -> Just (K.Char c)
    _                        -> Nothing
--  _                        -> Just (K.Dbg $ show e)

nextEvent :: Session -> IO K.Key
nextEvent session =
  do
    e <- V.next_event session
    maybe (nextEvent session) return (keyTranslate e)

-- A hack to get bright colors via the bold attribute. Depending on terminal
-- settings this is needed or not and the characters really get bold or not.
-- HCurses does this by default, but vty refuses to get crazy.
isBright c = c `elem` [bright_black, bright_red, bright_green, bright_yellow,
                       bright_blue, bright_magenta, bright_cyan, bright_white]
hack c a = if isBright c then with_style a bold else a
setFG c a = hack c $ with_fore_color a c
setBG c a = hack c $ with_back_color a c

attr = def_attr { attr_fore_color = SetTo white,
                  attr_back_color = SetTo black }

type AttrColor = Color
