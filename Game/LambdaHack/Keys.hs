module Game.LambdaHack.Keys where

import Prelude hiding (Left, Right)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Data.Maybe

import Game.LambdaHack.Geometry
import Game.LambdaHack.Dir

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

dirChars :: [Char]
dirChars = ['y', 'k', 'u', 'l', 'n', 'j', 'b', 'h']

dirKeys :: [Key]
dirKeys = map Char dirChars

dirUKeys :: [Key]
dirUKeys = map (Char . toUpper) dirChars

-- | Maps a keypad movement key to the canonical form.
-- Hard-coded not to bloat config files.
canonMoveKey :: Key -> Key
canonMoveKey =
  let assocs = (Begin, Char '.') : (KP '5', Char '.') :
        zip [Home, Up, PgUp, Right, PgDn, Down, End, Left] dirKeys ++
        zip (map KP ['7', '8', '9', '6', '3', '2', '1', '4']) dirUKeys
      m = M.fromList assocs
  in \ e -> fromMaybe e (M.lookup e m)

-- | Configurable event handler for the direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as close door.
handleDirection :: X -> Key -> (Dir -> a) -> a -> a
handleDirection lxsize e h k =
  let assocs = zip dirKeys (moves lxsize)
  in maybe k h (L.lookup e assocs)

-- | Configurable event handler for the upper direction keys.
handleUDirection :: X -> Key -> (Dir -> a) -> a -> a
handleUDirection lxsize e h k =
  let assocs = zip dirUKeys (moves lxsize)
  in maybe k h (L.lookup e assocs)

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
