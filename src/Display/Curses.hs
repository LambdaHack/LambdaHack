module Display.Curses
  (displayId, startup, shutdown,
   display, nextEvent, setBG, setFG, attr, Session) where

import UI.HSCurses.Curses as C hiding (setBold)
import qualified UI.HSCurses.CursesHelper as C
import Data.List as L
import Data.Map as M
import Data.Char
import qualified Data.ByteString as BS
import Control.Monad
import Data.Maybe

import Geometry
import Keys as K
import qualified Attr

displayId = "curses"

data Session =
  Session
    { win :: Window,
      styles :: Map (Attr.Color, Attr.Color) C.CursesStyle }

startup :: (Session -> IO ()) -> IO ()
startup k =
  do
    C.start
    cursSet CursorInvisible
    let s = [ ((f,b), C.Style (toFColor f) (toBColor b))
            | f <- [minBound..maxBound],
              -- No more color combinations possible: 16*4, 64 is max.
              b <- [Attr.Black, Attr.White, Attr.Blue, Attr.Magenta ] ]
    nr <- colorPairs
    when (nr < L.length s) $
      C.end >>
      error ("Terminal has too few color pairs (" ++ show nr ++ "). Giving up.")
    let (ks, vs) = unzip s
    ws <- C.convertStyles vs
    let styleMap = M.fromList (zip ks ws)
    k (Session C.stdScr styleMap)

shutdown :: Session -> IO ()
shutdown w = C.end

display :: Area -> Session -> (Loc -> (Display.Curses.Attr, Char)) -> String -> String -> IO ()
display ((y0,x0),(y1,x1)) (Session { win = w, styles = s }) f msg status =
  do
    -- let defaultStyle = C.defaultCursesStyle
    -- Terminals with white background require this and more:
    let defaultStyle = s ! (Attr.defFG, Attr.defBG)
        canonical (c, d) = (fromMaybe Attr.defFG c, fromMaybe Attr.defBG d)
    C.erase
    mvWAddStr w 0 0 msg
    sequence_ [ let (a,c) = f (y,x) in C.setStyle (findWithDefault defaultStyle (canonical a) s) >> mvWAddStr w (y+1) x [c]
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

type Attr = (Maybe Attr.Color, Maybe Attr.Color)

setFG c (_, b) = (Just c, b)
setBG c (f, _) = (f, Just c)
attr = (Nothing, Nothing)

toFColor :: Attr.Color -> C.ForegroundColor
toFColor Attr.Black     = C.BlackF
toFColor Attr.Red       = C.DarkRedF
toFColor Attr.Green     = C.DarkGreenF
toFColor Attr.Yellow    = C.BrownF
toFColor Attr.Blue      = C.DarkBlueF
toFColor Attr.Magenta   = C.PurpleF
toFColor Attr.Cyan      = C.DarkCyanF
toFColor Attr.White     = C.WhiteF
toFColor Attr.BrBlack   = C.GreyF
toFColor Attr.BrRed     = C.RedF
toFColor Attr.BrGreen   = C.GreenF
toFColor Attr.BrYellow  = C.YellowF
toFColor Attr.BrBlue    = C.BlueF
toFColor Attr.BrMagenta = C.MagentaF
toFColor Attr.BrCyan    = C.CyanF
toFColor Attr.BrWhite   = C.BrightWhiteF

toBColor :: Attr.Color -> C.BackgroundColor
toBColor Attr.Black     = C.BlackB
toBColor Attr.Red       = C.DarkRedB
toBColor Attr.Green     = C.DarkGreenB
toBColor Attr.Yellow    = C.BrownB
toBColor Attr.Blue      = C.DarkBlueB
toBColor Attr.Magenta   = C.PurpleB
toBColor Attr.Cyan      = C.DarkCyanB
toBColor Attr.White     = C.WhiteB
toBColor _              = C.BlackB  -- a limitation of curses
