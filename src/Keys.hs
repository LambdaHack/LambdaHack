module Keys where

import Prelude hiding (Left, Right)

import Geometry hiding (Up, Down)
import Data.Maybe
import Data.List as L
import Data.Map as M

-- | Library-independent datatype to represent keys.
data Key =
    Esc
  | Return
  | Tab
  | PgUp
  | PgDn
  | Left
  | Right
  | Up
  | Down
  | End
  | Begin
  | Home
  | KP Char        -- ^ a keypad key for a character (digits and operators)
  | Char Char      -- ^ a single printable character
  | Unknown String -- ^ an unknown key, collected to warn the user later
  deriving (Ord, Eq)

showKey :: Key -> String
showKey (Char ' ') = "<space>"  -- warnings about "command ( )" look wrong
showKey (Char c) = [c]
showKey Esc      = "ESC"  -- these three are common and terse abbreviations
showKey Return   = "RET"
showKey Tab      = "TAB"
showKey PgUp     = "<page-up>"
showKey PgDn     = "<page-down>"
showKey Left     = "<left>"
showKey Right    = "<right>"
showKey Up       = "<up>"
showKey Down     = "<down>"
showKey End      = "<end>"
showKey Begin    = "<begin>"
showKey Home     = "<home>"
showKey (KP c)   = "<KeyPad " ++ [c] ++ ">"
showKey (Unknown s) = s

instance Show Key where
  show = showKey

-- | Maps a keypad movement key to the canonical form.
-- Hard-coded not to bloat config files.
canonMoveKey :: Key -> Key
canonMoveKey e =
  case e of
    KP '8' -> Char 'K'
    KP '2' -> Char 'J'
    KP '4' -> Char 'H'
    KP '6' -> Char 'L'
    KP '7' -> Char 'Y'
    KP '9' -> Char 'U'
    KP '1' -> Char 'B'
    KP '3' -> Char 'N'
    KP '5' -> Char '.'
    Up     -> Char 'k'
    Down   -> Char 'j'
    Left   -> Char 'h'
    Right  -> Char 'l'
    Home   -> Char 'y'
    PgUp   -> Char 'u'
    End    -> Char 'b'
    PgDn   -> Char 'n'
    Begin  -> Char '.'
    k      -> k

-- | Configurable event handler for the direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as open/close.
handleDirection :: Key -> (Dir -> a) -> a -> a
handleDirection e h k =
  case e of
    Char 'k' -> h up
    Char 'j' -> h down
    Char 'h' -> h left
    Char 'l' -> h right
    Char 'y' -> h upleft
    Char 'u' -> h upright
    Char 'b' -> h downleft
    Char 'n' -> h downright
    _          -> k

-- | Configurable event handler for the upper direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as open/close.
handleUDirection :: Key -> (Dir -> a) -> a -> a
handleUDirection e h k =
  case e of
    Char 'K' -> h up
    Char 'J' -> h down
    Char 'H' -> h left
    Char 'L' -> h right
    Char 'Y' -> h upleft
    Char 'U' -> h upright
    Char 'B' -> h downleft
    Char 'N' -> h downright
    _          -> k

-- | Translate key from a GTK string description to our internal key type.
-- To be used, in particular, for the macros in the config file.
keyTranslate :: String -> Key
keyTranslate "less"          = Char '<'
keyTranslate "greater"       = Char '>'
keyTranslate "period"        = Char '.'
keyTranslate "colon"         = Char ':'
keyTranslate "comma"         = Char ','
keyTranslate "space"         = Char ' '
keyTranslate "question"      = Char '?'
keyTranslate "dollar"        = Char '$'
keyTranslate "asterisk"      = Char '*'
keyTranslate "KP_Multiply"   = Char '*'
keyTranslate "slash"         = Char '/'
keyTranslate "KP_Divide"     = Char '/'
keyTranslate "underscore"    = Char '_'
keyTranslate "Escape"        = Esc
keyTranslate "Return"        = Return
keyTranslate "Tab"           = Tab
keyTranslate "KP_Up"         = Up
keyTranslate "KP_Down"       = Down
keyTranslate "KP_Left"       = Left
keyTranslate "KP_Right"      = Right
keyTranslate "KP_Home"       = Home
keyTranslate "KP_End"        = End
keyTranslate "KP_Page_Up"    = PgUp
keyTranslate "KP_Page_Down"  = PgDn
keyTranslate "KP_Begin"      = Begin
keyTranslate "KP_Enter"      = Return
keyTranslate ['K','P','_',c] = KP c
keyTranslate [c]             = Char c
keyTranslate s               = Unknown s

-- | Maps a key to the canonical key for the command it denotes.
-- Takes into account the keypad and any macros from a config file.
-- Macros cannot depend on each other, but they can on canonMoveKey.
-- This has to be fully evaluated to catch errors in macro definitions early.
macroKey :: [(String, String)] -> M.Map Key Key
macroKey section =
  let trans k = case keyTranslate k of
                  Unknown s -> error $ "unknown macro key " ++ s
                  kt -> kt
      trMacro (from, to) = let fromTr = trans from
                               !toTr  = canonMoveKey $ trans to
                           in  if fromTr == toTr
                               then error $ "degenerate alias for " ++ show toTr
                               else (fromTr, toTr)
  in  M.fromList $ L.map trMacro section
