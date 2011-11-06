module Display.Vty
  (displayId, startup, shutdown, display, nextEvent, Session) where

import Graphics.Vty
import qualified Data.List as L
import qualified Data.ByteString.Char8 as BS

import Geometry
import qualified Keys as K (Key(..))
import qualified Color

displayId :: String
displayId = "vty"

type Session = Vty

startup :: (Session -> IO ()) -> IO ()
startup k = mkVty >>= k

display :: Area -> Session -> (Loc -> (Color.Attr, Char)) -> String -> String
           -> IO ()
display (x0, y0, x1, y1) vty f msg status =
  let img = (foldr (<->) empty_image .
             L.map (foldr (<|>) empty_image .
                    L.map (\ (x, y) -> let (a, c) = f (toLoc (x1 + 1) (x, y))
                                       in  char (setAttr a) c)))
            [ [ (x, y) | x <- [x0..x1] ] | y <- [y0..y1] ]
  in  update vty (pic_for_image
       (utf8_bytestring (setAttr Color.defaultAttr)
        (BS.pack (toWidth (x1 - x0 + 1) msg)) <->
        img <->
        utf8_bytestring (setAttr Color.defaultAttr)
        (BS.pack (toWidth (x1 - x0 + 1) status))))

toWidth :: Int -> String -> String
toWidth n x = take n (x ++ repeat ' ')

keyTranslate :: Event -> K.Key
keyTranslate e =
  case e of
    EvKey KEsc []          -> K.Esc
    EvKey KEnter []        -> K.Return
    EvKey (KASCII '\t') [] -> K.Tab
    EvKey KUp []           -> K.Up
    EvKey KDown []         -> K.Down
    EvKey KLeft []         -> K.Left
    EvKey KRight []        -> K.Right
    EvKey KHome []         -> K.Home
    EvKey KPageUp []       -> K.PgUp
    EvKey KEnd []          -> K.End
    EvKey KPageDown []     -> K.PgDn
    EvKey KBegin []        -> K.Begin
    -- No KP_ keys in vty; see https://github.com/coreyoconnor/vty/issues/8
    -- For now, movement keys are more important than hero selection:
    EvKey (KASCII c) []
      | c `elem` ['1'..'9']  -> K.KP c
      | otherwise            -> K.Char c
    _                        -> K.Unknown (show e)

nextEvent :: Session -> IO K.Key
nextEvent session =
  do
    e <- next_event session
    return (keyTranslate e)

-- A hack to get bright colors via the bold attribute. Depending on terminal
-- settings this is needed or not and the characters really get bold or not.
-- HSCurses does this by default, but in Vty you have to request the hack.
hack :: Color.Color -> Attr -> Attr
hack c a = if Color.isBright c then with_style a bold else a

setAttr :: (Color.Color, Color.Color) -> Attr
setAttr (fg, bg) =
-- This optimization breaks display for white background terminals:
--  if (fg, bg) == Color.defaultAttr
--  then def_attr
--  else
    hack fg $ hack bg $
      def_attr { attr_fore_color = SetTo (aToc fg),
                 attr_back_color = SetTo (aToc bg) }

aToc :: Color.Color -> Color
aToc Color.Black     = black
aToc Color.Red       = red
aToc Color.Green     = green
aToc Color.Yellow    = yellow
aToc Color.Blue      = blue
aToc Color.Magenta   = magenta
aToc Color.Cyan      = cyan
aToc Color.White     = white
aToc Color.BrBlack   = bright_black
aToc Color.BrRed     = bright_red
aToc Color.BrGreen   = bright_green
aToc Color.BrYellow  = bright_yellow
aToc Color.BrBlue    = bright_blue
aToc Color.BrMagenta = bright_magenta
aToc Color.BrCyan    = bright_cyan
aToc Color.BrWhite   = bright_white
