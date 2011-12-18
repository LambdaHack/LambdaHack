module Game.LambdaHack.Display.Curses
  ( displayId, startup, shutdown, display, nextEvent, Session
  ) where

import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as C
import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad

import Game.LambdaHack.Area
import Game.LambdaHack.Loc
import qualified Game.LambdaHack.Keys as K (Key(..))
import qualified Game.LambdaHack.Color as Color

displayId :: String
displayId = "curses"

data Session = Session
  { win :: C.Window
  , styles :: M.Map (Color.Color, Color.Color) C.CursesStyle
  }

startup :: (Session -> IO ()) -> IO ()
startup k = do
  C.start
  C.cursSet C.CursorInvisible
  let s = [ ((f, b), C.Style (toFColor f) (toBColor b))
          | f <- [minBound..maxBound],
            -- No more color combinations possible: 16*4, 64 is max.
            b <- Color.legalBG ]
  nr <- C.colorPairs
  when (nr < L.length s) $
    C.end >>
    error ("Terminal has too few color pairs (" ++ show nr ++ "). Giving up.")
  let (ks, vs) = unzip s
  ws <- C.convertStyles vs
  let styleMap = M.fromList (zip ks ws)
  k (Session C.stdScr styleMap)

shutdown :: Session -> IO ()
shutdown _ = C.end

display :: Area -> Int -> Session
        -> (Loc -> (Color.Attr, Char)) -> String -> String
        -> IO ()
display (x0, y0, x1, y1) width (Session { win = w, styles = s })
        f msg status = do
  -- let defaultStyle = C.defaultCursesStyle
  -- Terminals with white background require this:
  let defaultStyle = s M.! Color.defaultAttr
      xsize  = x1 - x0 + 1
  C.erase
  C.setStyle defaultStyle
  C.mvWAddStr w 0 0 (toWidth width msg)
  C.mvWAddStr w (y1 + 2) 0 (toWidth (width - 1) status)
  -- TODO: the following does not work in standard xterm window,
  -- due to the curses historical limitations.
  -- C.mvWAddStr w (y1 + 2) (width - 1) " "
  sequence_ [ C.setStyle (M.findWithDefault defaultStyle a s)
              >> C.mvWAddStr w (y + 1) x [c]
            | x <- [x0..x1], y <- [y0..y1],
              let (a, c) = f (toLoc xsize (x, y)) ]
  C.refresh

toWidth :: Int -> String -> String
toWidth n x = take n (x ++ repeat ' ')

keyTranslate :: C.Key -> K.Key
keyTranslate e =
  case e of
    C.KeyChar '\ESC' -> K.Esc
    C.KeyExit        -> K.Esc
    C.KeyChar '\n'   -> K.Return
    C.KeyChar '\r'   -> K.Return
    C.KeyEnter       -> K.Return
    C.KeyChar '\t'   -> K.Tab
    C.KeyUp          -> K.Up
    C.KeyDown        -> K.Down
    C.KeyLeft        -> K.Left
    C.KeySLeft       -> K.Left
    C.KeyRight       -> K.Right
    C.KeySRight      -> K.Right
    C.KeyHome        -> K.Home
    C.KeyPPage       -> K.PgUp
    C.KeyEnd         -> K.End
    C.KeyNPage       -> K.PgDn
    C.KeyBeg         -> K.Begin
    C.KeyB2          -> K.Begin
    C.KeyClear       -> K.Begin
    -- No KP_ keys; see https://github.com/skogsbaer/hscurses/issues/10
    -- Movement keys are more important than hero selection, so preferring them:
    C.KeyChar c
      | c `elem` ['1'..'9'] -> K.KP c
      | otherwise           -> K.Char c
    _                       -> K.Unknown (show e)

nextEvent :: Session -> IO K.Key
nextEvent _session = do
  e <- C.getKey C.refresh
  return (keyTranslate e)
--  case keyTranslate e of
--    Unknown _ -> nextEvent session
--    k -> return k

toFColor :: Color.Color -> C.ForegroundColor
toFColor Color.Black     = C.BlackF
toFColor Color.Red       = C.DarkRedF
toFColor Color.Green     = C.DarkGreenF
toFColor Color.Yellow    = C.BrownF
toFColor Color.Blue      = C.DarkBlueF
toFColor Color.Magenta   = C.PurpleF
toFColor Color.Cyan      = C.DarkCyanF
toFColor Color.White     = C.WhiteF
toFColor Color.BrBlack   = C.GreyF
toFColor Color.BrRed     = C.RedF
toFColor Color.BrGreen   = C.GreenF
toFColor Color.BrYellow  = C.YellowF
toFColor Color.BrBlue    = C.BlueF
toFColor Color.BrMagenta = C.MagentaF
toFColor Color.BrCyan    = C.CyanF
toFColor Color.BrWhite   = C.BrightWhiteF

toBColor :: Color.Color -> C.BackgroundColor
toBColor Color.Black     = C.BlackB
toBColor Color.Red       = C.DarkRedB
toBColor Color.Green     = C.DarkGreenB
toBColor Color.Yellow    = C.BrownB
toBColor Color.Blue      = C.DarkBlueB
toBColor Color.Magenta   = C.PurpleB
toBColor Color.Cyan      = C.DarkCyanB
toBColor Color.White     = C.WhiteB
toBColor _               = C.BlackB  -- a limitation of curses
