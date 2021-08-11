-- | Text frontend based on HSCurses. This frontend is not fully supported
-- due to the limitations of the curses library (keys, colours, last character
-- of the last line).
module Game.LambdaHack.Client.UI.Frontend.Curses
  (
#ifdef USE_CURSES
-- to molify doctest, but don't break stylish-haskell parsing
   startup, frontendName
#endif
  ) where

#ifdef USE_CURSES
import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent.Async
import           Data.Char (chr, ord)
import qualified Data.Map.Strict as M
import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as C

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Definition.Color as Color

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { swin    :: C.Window  -- ^ the window to draw to
  , sstyles :: M.Map (Color.Color, Color.Color) C.CursesStyle
      -- ^ map from fore/back colour pairs to defined curses styles
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "curses"

-- | Starts the main program loop using the frontend input and output.
startup :: ScreenContent -> ClientOptions -> IO RawFrontend
startup coscreen _soptions = do
  C.start
  void $ C.cursSet C.CursorInvisible
  let s = [ ((fg, bg), C.Style (toFColor fg) (toBColor bg))
          | -- Almost no more color combinations possible: 15*4, 64 is max.
            fg <- Color.legalFgCol
          , bg <- [Color.Black, Color.Blue, Color.White, Color.BrBlack] ]
  nr <- C.colorPairs
  when (nr < length s) $
    C.end >> error ("terminal has too few color pairs" `showFailure` nr)
  let (ks, vs) = unzip s
  ws <- C.convertStyles vs
  let swin = C.stdScr
      sstyles = M.fromDistinctAscList (zip ks ws)
      sess = FrontendSession{..}
  rf <- createRawFrontend coscreen (display coscreen sess) shutdown
  let storeKeys :: IO ()
      storeKeys = do
        K.KM{..} <- keyTranslate <$> C.getKey C.refresh
        saveKMP rf modifier key (PointUI 0 0)
        storeKeys
  void $ async storeKeys
  return $! rf

shutdown :: IO ()
shutdown = C.end

-- | Output to the screen via the frontend.
display :: ScreenContent
        -> FrontendSession
        -> SingleFrame
        -> IO ()
display coscreen FrontendSession{..} SingleFrame{singleArray} = do
  -- let defaultStyle = C.defaultCursesStyle
  -- Terminals with white background require this:
  let defaultStyle = sstyles M.! (Color.defFG, Color.Black)
  C.erase
  C.setStyle defaultStyle
  -- We need to remove the last character from the status line,
  -- because otherwise it would overflow a standard size xterm window,
  -- due to the curses historical limitations.
  let sf = chunk $ map Color.attrCharFromW32
                 $ PointArray.toListA singleArray
      level = init sf ++ [init $ last sf]
      nm = zip [0..] $ map (zip [0..]) level
      chunk [] = []
      chunk l = let (ch, r) = splitAt (rwidth coscreen) l
                in ch : chunk r
  sequence_ [ C.setStyle (M.findWithDefault defaultStyle acAttr2 sstyles)
              >> C.mvWAddStr swin y x [acChar]
            | (y, line) <- nm
            , (x, Color.AttrChar{acAttr=Color.Attr{..}, ..}) <- line
            , let acAttr2 = case bg of
                    Color.HighlightNone -> (fg, Color.Black)
                    Color.HighlightGreen -> (fg, Color.Black)  -- too few bgs
                    Color.HighlightBlue ->
                      if fg /= Color.Blue
                      then (fg, Color.Blue)
                      else (fg, Color.BrBlack)
                    Color.HighlightBrown -> (fg, Color.Black)
                    Color.HighlightCyan -> (fg, Color.Black)
                    Color.HighlightGrey ->
                      if fg /= Color.BrBlack
                      then (fg, Color.BrBlack)
                      else (fg, Color.defFG)
                    Color.HighlightWhite -> (fg, Color.Black)
                    Color.HighlightMagenta -> (fg, Color.Black)
                    Color.HighlightRed ->
                      if fg /= Color.Red
                      then (fg, Color.Red)
                      else (fg, Color.defFG)
                    Color.HighlightYellow -> (Color.Black, Color.defFG)
                    Color.HighlightYellowAim -> (Color.Black, Color.defFG)
                    Color.HighlightRedAim ->
                      if fg /= Color.Red
                      then (fg, Color.Red)
                      else (fg, Color.defFG)
                    Color.HighlightNoneCursor -> (fg, Color.Black)
                    Color.HighlightBackground -> (fg, Color.BrBlack) ]
  C.refresh

keyTranslate :: C.Key -> K.KM
keyTranslate e = (\(key, modifier) -> K.KM modifier key) $
  case e of
    C.KeyChar '\ESC' -> (K.Esc,     K.NoModifier)
    C.KeyExit        -> (K.Esc,     K.NoModifier)
    C.KeyChar '\n'   -> (K.Return,  K.NoModifier)
    C.KeyChar '\r'   -> (K.Return,  K.NoModifier)
    C.KeyEnter       -> (K.Return,  K.NoModifier)
    C.KeyChar ' '    -> (K.Space,   K.NoModifier)
    C.KeyChar '\t'   -> (K.Tab,     K.NoModifier)
    C.KeyBTab        -> (K.BackTab, K.NoModifier)
    C.KeyBackspace   -> (K.BackSpace, K.NoModifier)
    C.KeyUp          -> (K.Up,      K.NoModifier)
    C.KeyDown        -> (K.Down,    K.NoModifier)
    C.KeyLeft        -> (K.Left,    K.NoModifier)
    C.KeySLeft       -> (K.Left,    K.NoModifier)
    C.KeyRight       -> (K.Right,   K.NoModifier)
    C.KeySRight      -> (K.Right,   K.NoModifier)
    C.KeyHome        -> (K.Home,    K.NoModifier)
    C.KeyEnd         -> (K.End,     K.NoModifier)
    C.KeyPPage       -> (K.PgUp,    K.NoModifier)
    C.KeyNPage       -> (K.PgDn,    K.NoModifier)
    C.KeyBeg         -> (K.Begin,   K.NoModifier)
    C.KeyB2          -> (K.Begin,   K.NoModifier)
    C.KeyClear       -> (K.Begin,   K.NoModifier)
    C.KeyIC          -> (K.Insert,  K.NoModifier)
    -- No KP_ keys; see <https://github.com/skogsbaer/hscurses/issues/10>
    C.KeyChar c
      -- This case needs to be considered after Tab, since, apparently,
      -- on some terminals ^i == Tab and Tab is more important for us.
      | ord '\^A' <= ord c && ord c <= ord '\^Z' ->
        -- Alas, only lower-case letters.
        (K.Char $ chr $ ord c - ord '\^A' + ord 'a', K.Control)
        -- Movement keys are more important than leader picking,
        -- so disabling the latter and interpreting the keypad numbers
        -- as movement:
      | c `elem` ['1'..'9'] -> (K.KP c, K.NoModifier)
      | otherwise           -> (K.Char c, K.NoModifier)
    _                       -> (K.Unknown (show e), K.NoModifier)

toFColor :: Color.Color -> C.ForegroundColor
toFColor Color.Black     = C.BlackF
toFColor Color.Red       = C.DarkRedF
toFColor Color.Green     = C.DarkGreenF
toFColor Color.Brown     = C.BrownF
toFColor Color.Blue      = C.DarkBlueF
toFColor Color.Magenta   = C.PurpleF
toFColor Color.Cyan      = C.DarkCyanF
toFColor Color.White     = C.WhiteF
toFColor Color.AltWhite  = C.WhiteF
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
toBColor Color.Brown     = C.BrownB
toBColor Color.Blue      = C.DarkBlueB
toBColor Color.Magenta   = C.PurpleB
toBColor Color.Cyan      = C.DarkCyanB
toBColor Color.White     = C.WhiteB
toBColor _               = C.BlackB  -- a limitation of curses
#endif
