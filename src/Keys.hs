module Keys where

import Prelude hiding (Left, Right)

import Geometry hiding (Up, Down)

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
  | KP Char    -- ^ a keypad key for a character (digits and operators)
  | Char Char  -- ^ a single printable character
  | Dbg String -- ^ an unknown key, collected for debugging
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
showKey (Dbg s)  = s

-- | maps a key to the canonical key for the command it denotes
canonicalKey :: Key -> Key
canonicalKey e =
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
    k        -> k

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
