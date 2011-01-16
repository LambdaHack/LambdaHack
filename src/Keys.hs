module Keys where

import Prelude hiding (Left, Right)

-- | Library-independent datatype to represent keys.
data Key =
    Esc
  | Return
  | PgUp
  | PgDn
  | Left
  | Right
  | Up
  | Down
  | End
  | Begin
  | Home
  | Char Char    -- ^ a single printable character
  deriving (Ord, Eq)

showKey :: Key -> String
showKey (Char c) = [c]
showKey Esc      = "<escape>"
showKey Return   = "<return>"
showKey PgUp     = "<page-up>"
showKey PgDn     = "<page-down>"
showKey Left     = "<left>"
showKey Right    = "<right>"
showKey Up       = "<up>"
showKey Down     = "<down>"
showKey End      = "<end>"
showKey Home     = "<home>"
