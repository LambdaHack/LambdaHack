module Display.Curses
  (displayId, startup, shutdown,
   display, nextEvent, setBG, setFG, setBold, Session,
   white, black, yellow, blue, magenta, red, green, attr, Display.Curses.Attr) where

import UI.HSCurses.Curses as C hiding (setBold)
import qualified UI.HSCurses.CursesHelper as C
import Data.List as L
import Data.Map as M
import Data.Char
import qualified Data.ByteString as BS

import Geometry

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

-- | translates the hcurses key code to the standard GTK key code
keyTranslate :: C.Key -> String
keyTranslate e =
  case e of
    C.KeyChar '<' -> "less"
    C.KeyChar '>' -> "greater"
    C.KeyChar '.' -> "period"
    C.KeyChar ':' -> "colon"
    C.KeyChar ',' -> "comma"
    C.KeyChar ' ' -> "space"
    C.KeyChar '?' -> "question"
    C.KeyChar '*' -> "asterisk"
    C.KeyChar '\ESC' -> "Escape"
    C.KeyExit        -> "Escape"
    C.KeyChar '\n'   -> "Return"
    C.KeyChar '\r'   -> "Return"
    C.KeyEnter       -> "Return"
    C.KeyUp    -> "KP_Up"
    C.KeyDown  -> "KP_Down"
    C.KeyLeft  -> "KP_Left"
    C.KeyRight -> "KP_Right"
    C.KeyHome  -> "KP_Home"
    C.KeyPPage -> "KP_Page_Up"
    C.KeyEnd   -> "KP_End"
    C.KeyNPage -> "KP_Page_Down"
    C.KeyBeg   -> "KP_Begin"
    C.KeyB2    -> "KP_Begin"
    C.KeyChar c   -> [c]
    _ -> []

nextEvent :: Session -> IO String
nextEvent session =
  do
    e <- C.getKey refresh
    let s = keyTranslate e in
      if L.null s
      then nextEvent session
      else return s

type Attr = (Maybe AttrColor, Maybe AttrColor)

attr = (Nothing, Nothing)

data AttrColor = White | Black | Yellow | Blue | Magenta | Red | Green 
  deriving (Show, Eq, Ord, Enum, Bounded)

toFColor :: Maybe AttrColor -> C.ForegroundColor
toFColor (Just White)    = C.WhiteF
toFColor (Just Black)    = C.BlackF
toFColor (Just Yellow)   = C.BrownF
toFColor (Just Blue)     = C.DarkBlueF
toFColor (Just Magenta)  = C.PurpleF
toFColor (Just Red)      = C.DarkRedF
toFColor (Just Green)    = C.DarkGreenF
toFColor Nothing         = C.DefaultF

toBColor :: Maybe AttrColor -> C.BackgroundColor
toBColor (Just White)    = C.WhiteB
toBColor (Just Black)    = C.BlackB
toBColor (Just Yellow)   = C.BrownB
toBColor (Just Blue)     = C.DarkBlueB
toBColor (Just Magenta)  = C.PurpleB
toBColor (Just Red)      = C.DarkRedB
toBColor (Just Green)    = C.DarkGreenB
toBColor Nothing         = C.DefaultB

white   = White
black   = Black
yellow  = Yellow
blue    = Blue
magenta = Magenta
red     = Red
green   = Green

setBold (f, b) = (f, b)
setFG c (_, b) = (Just c, b)
setBG c (f, _) = (f, Just c)
