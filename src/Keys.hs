module Keys where

import Prelude hiding (Left, Right)

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
showKey (Char c) = [c]
showKey Esc      = "<escape>"
showKey Return   = "<return>"
showKey Tab      = "<tab>"
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
