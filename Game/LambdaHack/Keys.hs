-- | Frontend-independent keyboard input operations.
module Game.LambdaHack.Keys
  ( Key(..), handleDir, moveBinding, keyTranslate
  ) where

import Prelude hiding (Left, Right)
import qualified Data.List as L
import Data.Char

import Game.LambdaHack.PointXY
import Game.LambdaHack.Vector

-- TODO: if the file grows much larger, split it and move a part to Utils/

-- | Frontend-independent datatype to represent keys.
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
  | KP !Char        -- ^ a keypad key for a character (digits and operators)
  | Char !Char      -- ^ a single printable character
  | Unknown !String -- ^ an unknown key, collected to warn the user later
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

dirViChar :: [Char]
dirViChar = ['y', 'k', 'u', 'l', 'n', 'j', 'b', 'h']

dirViMoveKey :: [Key]
dirViMoveKey = map Char dirViChar

dirViRunKey :: [Key]
dirViRunKey = map (Char . toUpper) dirViChar

dirMoveKey :: [Key]
dirMoveKey = [Home, Up, PgUp, Right, PgDn, Down, End, Left]

dirRunKey :: [Key]
dirRunKey = map KP ['7', '8', '9', '6', '3', '2', '1', '4']

-- | Configurable event handler for the direction keys.
-- Used for directed commands such as close door.
handleDir :: X -> Key -> (Vector -> a) -> a -> a
handleDir lxsize e h k =
  let mvs = moves lxsize
      assocs = zip dirViMoveKey mvs ++ zip dirMoveKey mvs
  in maybe k h (L.lookup e assocs)

-- | Binding of both sets of movement keys.
moveBinding :: ((X -> Vector) -> a) -> ((X -> Vector) -> a)
            -> [(Key, (String, a))]
moveBinding move run =
  let assign f (key, dir) = (key, ("", f dir))
  in map (assign move) (zip dirViMoveKey movesWidth) ++
     map (assign move) (zip dirMoveKey movesWidth) ++
     map (assign run) (zip dirViRunKey movesWidth) ++
     map (assign run) (zip dirRunKey movesWidth)

-- | Translate key from a GTK string description to our internal key type.
-- To be used, in particular, for the command bindings and macros
-- in the config file.
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
keyTranslate "minus"         = Char '-'
keyTranslate "KP_Subtract"   = Char '-'
keyTranslate "bracketleft"   = Char '['
keyTranslate "bracketright"  = Char ']'
keyTranslate "braceleft"     = Char '{'
keyTranslate "braceright"    = Char '}'
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
