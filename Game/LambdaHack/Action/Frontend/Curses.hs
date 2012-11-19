-- | Text frontend based on HSCurses.
module Game.LambdaHack.Action.Frontend.Curses
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , display, nextEvent, promptGetAnyKey
    -- * Frontend administration tools
  , frontendName, startup
  ) where

import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as C
import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad

import Game.LambdaHack.Utils.Assert
import qualified Game.LambdaHack.Key as K (Key(..),  Modifier(..))
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Animation (SingleFrame(..))

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { swin    :: C.Window  -- ^ the window to draw to
  , sstyles :: M.Map Color.Attr C.CursesStyle
      -- ^ map from fore/back colour pairs to defined curses styles
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "curses"

-- | Starts the main program loop using the frontend input and output.
startup :: String -> (FrontendSession -> IO ()) -> IO ()
startup _ k = do
  C.start
--  C.keypad C.stdScr False  -- TODO: may help to fix xterm keypad on Ubuntu
  void $ C.cursSet C.CursorInvisible
  let s = [ (Color.Attr{fg, bg}, C.Style (toFColor fg) (toBColor bg))
          | fg <- [minBound..maxBound],
            -- No more color combinations possible: 16*4, 64 is max.
            bg <- Color.legalBG ]
  nr <- C.colorPairs
  when (nr < L.length s) $
    C.end >>
    (assert `failure`
       "Terminal has too few color pairs (" ++ show nr ++ "). Giving up.")
  let (ks, vs) = unzip s
  ws <- C.convertStyles vs
  let styleMap = M.fromList (zip ks ws)
  k (FrontendSession C.stdScr styleMap)
  C.end

-- | Output to the screen via the frontend.
display :: FrontendSession          -- ^ frontend session data
        -> Bool
        -> Bool
        -> Maybe SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display _ _ _ Nothing = return ()
display FrontendSession{..}  _ _ (Just SingleFrame{..}) = do
  -- let defaultStyle = C.defaultCursesStyle
  -- Terminals with white background require this:
  let defaultStyle = sstyles M.! Color.defaultAttr
  C.erase
  C.setStyle defaultStyle
  C.mvWAddStr swin 0 0 sfTop
  -- We need to remove the last character from the status line,
  -- because otherwise it would overflow a standard size xterm window,
  -- due to the curses historical limitations.
  C.mvWAddStr swin (L.length sfLevel + 1) 0 (L.init sfBottom)
  let nm = L.zip [0..] $ L.map (L.zip [0..]) sfLevel
  sequence_ [ C.setStyle (M.findWithDefault defaultStyle acAttr sstyles)
              >> C.mvWAddStr swin (y + 1) x [acChar]
            | (y, line) <- nm, (x, Color.AttrChar{..}) <- line ]
  C.refresh

-- | Input key via the frontend.
nextEvent :: FrontendSession -> Maybe Bool -> IO (K.Key, K.Modifier)
nextEvent _sess _ = do
  e <- C.getKey C.refresh
  return (keyTranslate e, K.NoModifier)

-- | Display a prompt, wait for any key.
promptGetAnyKey :: FrontendSession -> SingleFrame
             -> IO (K.Key, K.Modifier)
promptGetAnyKey sess frame = do
  display sess True True $ Just frame
  nextEvent sess Nothing

keyTranslate :: C.Key -> K.Key
keyTranslate e =
  case e of
    C.KeyChar '\ESC' -> K.Esc
    C.KeyExit        -> K.Esc
    C.KeyChar '\n'   -> K.Return
    C.KeyChar '\r'   -> K.Return
    C.KeyEnter       -> K.Return
    C.KeyChar ' '    -> K.Space
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
    -- TODO: try to get the Control modifier from the escape gibberish
    -- and use Control-keypad for KP_ movement.
    -- Movement keys are more important than hero selection,
    -- so disabling the latter and interpreting the keypad numbers as movement:
    C.KeyChar c
      | c `elem` ['1'..'9'] -> K.KP c
      | otherwise           -> K.Char c
    _                       -> K.Unknown (show e)

toFColor :: Color.Color -> C.ForegroundColor
toFColor Color.Black     = C.BlackF
toFColor Color.Red       = C.DarkRedF
toFColor Color.Green     = C.DarkGreenF
toFColor Color.Brown     = C.BrownF
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
toBColor Color.Brown     = C.BrownB
toBColor Color.Blue      = C.DarkBlueB
toBColor Color.Magenta   = C.PurpleB
toBColor Color.Cyan      = C.DarkCyanB
toBColor Color.White     = C.WhiteB
toBColor _               = C.BlackB  -- a limitation of curses
