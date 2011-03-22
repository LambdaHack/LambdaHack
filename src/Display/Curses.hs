module Display.Curses
  (displayId, startup, shutdown,
   display, nextEvent, setBG, setFG, attr, Session,
   black, red, green, yellow, blue, magenta, cyan, white,
   bright_black, bright_red, bright_green, bright_yellow,
   bright_blue, bright_magenta, bright_cyan, bright_white,
   Display.Curses.Attr, AttrColor) where

import UI.HSCurses.Curses as C hiding (setBold)
import qualified UI.HSCurses.CursesHelper as C
import Data.List as L
import Data.Map as M
import Data.Char
import qualified Data.ByteString as BS

import Geometry
import Keys as K

displayId = "curses"

data Session =
  Session
    { win :: Window,
      styles :: Map (Maybe AttrColor, Maybe AttrColor) C.CursesStyle }

startup :: (Session -> IO ()) -> IO ()
startup k =
  do
    C.start
    C.startColor
    cursSet CursorInvisible
    nr <- colorPairs
    let s = [ ((f,b), C.Style (toFColor f) (toBColor b))
            | f <- Nothing : L.map Just [minBound..maxBound],
              b <- Nothing : L.map Just [minBound..maxBound] ]
    let (ks, vs) = unzip (tail s)  -- drop the Nothing/Nothing combo
    ws <- C.convertStyles (take (nr - 1) vs)
    k (Session C.stdScr (M.fromList (zip ks ws)))

shutdown :: Session -> IO ()
shutdown w = C.end

display :: Area -> Session -> (Loc -> (Display.Curses.Attr, Char)) -> String -> String -> IO ()
display ((y0,x0),(y1,x1)) (Session { win = w, styles = s }) f msg status =
  do
    erase
    mvWAddStr w 0 0 msg
    sequence_ [ let (a,c) = f (y,x) in C.setStyle (findWithDefault C.defaultCursesStyle a s) >> mvWAddStr w (y+1) x [c]
              | x <- [x0..x1], y <- [y0..y1] ]
    mvWAddStr w (y1+2) 0 status
    refresh
{-
    in  V.update vty (Pic NoCursor
         ((renderBS attr (BS.pack (L.map (fromIntegral . ord) (toWidth (x1-x0+1) msg)))) <->
          img <->
          (renderBS attr (BS.pack (L.map (fromIntegral . ord) (toWidth (x1-x0+1) status))))))
-}

{-
toWidth :: Int -> String -> String
toWidth n x = take n (x ++ repeat ' ')
-}

keyTranslate :: C.Key -> Maybe K.Key
keyTranslate e =
  case e of
    C.KeyChar '\ESC' -> Just K.Esc
    C.KeyExit        -> Just K.Esc
    C.KeyChar '\n'   -> Just K.Return
    C.KeyChar '\r'   -> Just K.Return
    C.KeyEnter       -> Just K.Return
    C.KeyChar '\t'   -> Just K.Tab
    C.KeyUp          -> Just K.Up
    C.KeyDown        -> Just K.Down
    C.KeyLeft        -> Just K.Left
    C.KeyRight       -> Just K.Right
    C.KeyHome        -> Just K.Home
    C.KeyPPage       -> Just K.PgUp
    C.KeyEnd         -> Just K.End
    C.KeyNPage       -> Just K.PgDn
    C.KeyBeg         -> Just K.Begin
    C.KeyB2          -> Just K.Begin
    C.KeyClear       -> Just K.Begin
    -- No KP_ keys in hscurses and they do not seem actively maintained.
    -- For now, movement keys are more important than hero selection:
    C.KeyChar c
      | c `elem` ['1'..'9'] -> Just (K.KP c)
      | otherwise           -> Just (K.Char c)
    _                -> Nothing
--  _                -> Just (K.Dbg $ show e)

nextEvent :: Session -> IO K.Key
nextEvent session =
  do
    e <- C.getKey refresh
    maybe (nextEvent session) return (keyTranslate e)

type Attr = (Maybe AttrColor, Maybe AttrColor)

setFG c (_, b) = (Just c, b)
setBG c (f, _) = (f, Just c)
attr = (Nothing, Nothing)

data AttrColor =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BrBlack
  | BrRed
  | BrGreen
  | BrYellow
  | BrBlue
  | BrMagenta
  | BrCyan
  | BrWhite
  deriving (Show, Eq, Ord, Enum, Bounded)

toFColor :: Maybe AttrColor -> C.ForegroundColor
toFColor (Just Black)     = C.BlackF
toFColor (Just Red)       = C.DarkRedF
toFColor (Just Green)     = C.DarkGreenF
toFColor (Just Yellow)    = C.BrownF
toFColor (Just Blue)      = C.DarkBlueF
toFColor (Just Magenta)   = C.PurpleF
toFColor (Just Cyan)      = C.DarkCyanF
toFColor (Just White)     = C.WhiteF
toFColor (Just BrBlack)   = C.GreyF
toFColor (Just BrRed)     = C.RedF
toFColor (Just BrGreen)   = C.GreenF
toFColor (Just BrYellow)  = C.YellowF
toFColor (Just BrBlue)    = C.BlueF
toFColor (Just BrMagenta) = C.MagentaF
toFColor (Just BrCyan)    = C.CyanF
toFColor (Just BrWhite)   = C.BrightWhiteF
toFColor Nothing          = C.DefaultF

toBColor :: Maybe AttrColor -> C.BackgroundColor
toBColor (Just Black)     = C.BlackB
toBColor (Just Red)       = C.DarkRedB
toBColor (Just Green)     = C.DarkGreenB
toBColor (Just Yellow)    = C.BrownB
toBColor (Just Blue)      = C.DarkBlueB
toBColor (Just Magenta)   = C.PurpleB
toBColor (Just Cyan)      = C.DarkCyanB
toBColor (Just White)     = C.WhiteB
toBColor (Just _)         = C.DefaultB  -- a limitation of curses
toBColor Nothing          = C.DefaultB

black   = Black
red     = Red
green   = Green
yellow  = Yellow
blue    = Blue
magenta = Magenta
cyan    = Cyan
white   = White

bright_black   = BrBlack
bright_red     = BrRed
bright_green   = BrGreen
bright_yellow  = BrYellow
bright_blue    = BrBlue
bright_magenta = BrMagenta
bright_cyan    = BrCyan
bright_white   = BrWhite
