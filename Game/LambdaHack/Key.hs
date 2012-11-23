-- | Frontend-independent keyboard input operations.
module Game.LambdaHack.Key
  ( Key(..), handleDir, dirAllMoveKey
  , moveBinding, keyTranslate, Modifier(..), showKM
  ) where

import Prelude hiding (Left, Right)
import qualified Data.List as L
import qualified Data.Char as Char

import Game.LambdaHack.PointXY
import Game.LambdaHack.Vector

-- TODO: if the file grows much larger, split it and move a part to Utils/

-- | Frontend-independent datatype to represent keys.
data Key =
    Esc
  | Return
  | Space
  | Tab
  | BackTab
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
  | Unknown !String -- ^ an unknown key, registered to warn the user
  deriving (Ord, Eq)

-- | Our own encoding of modifiers. Incomplete.
data Modifier =
    Control
  | NoModifier
  deriving (Ord, Eq)

-- Common and terse names for keys.
showKey :: Key -> String
showKey (Char c) = [c]
showKey Esc      = "ESC"
showKey Return   = "RET"
showKey Space    = "SPACE"
showKey Tab      = "TAB"
showKey BackTab  = "SHIFT-TAB"
showKey PgUp     = "PGUP"
showKey PgDn     = "PGDOWN"
showKey Left     = "LEFT"
showKey Right    = "RIGHT"
showKey Up       = "UP"
showKey Down     = "DOWN"
showKey End      = "END"
showKey Begin    = "BEGIN"
showKey Home     = "HOME"
showKey (KP c)   = "KEYPAD(" ++ [c] ++ ")"
showKey (Unknown s) = s

-- | Show a key with a modifier, if any.
showKM :: (Key, Modifier) -> String
showKM (key, Control) = "CTRL-" ++ showKey key
showKM (key, NoModifier) = showKey key

instance Show Key where
  show = showKey

dirViChar :: [Char]
dirViChar = ['y', 'k', 'u', 'l', 'n', 'j', 'b', 'h']

dirViMoveKey :: [Key]
dirViMoveKey = map Char dirViChar

dirMoveKey :: [Key]
dirMoveKey = [Home, Up, PgUp, Right, PgDn, Down, End, Left]

dirAllMoveKey :: [Key]
dirAllMoveKey = dirViMoveKey ++ dirMoveKey

dirViRunKey :: [Key]
dirViRunKey = map (Char . Char.toUpper) dirViChar

dirRunKey :: [Key]
dirRunKey = map KP dirNums

_dirAllRunKey :: [Key]
_dirAllRunKey = dirViRunKey ++ dirRunKey

dirNums :: [Char]
dirNums = ['7', '8', '9', '6', '3', '2', '1', '4']

dirHeroKey :: [Key]
dirHeroKey = map Char dirNums

-- | Configurable event handler for the direction keys.
-- Used for directed commands such as close door.
handleDir :: X -> (Key, Modifier) -> (Vector -> a) -> a -> a
handleDir lxsize (key, NoModifier) h k =
  let mvs = moves lxsize
      assocs = zip dirAllMoveKey $ mvs ++ mvs
  in maybe k h (L.lookup key assocs)
handleDir _lxsize _ _h k = k

-- TODO: deduplicate
-- | Binding of both sets of movement keys.
moveBinding :: ((X -> Vector) -> a) -> ((X -> Vector) -> a)
            -> [((Key, Modifier), (String, Bool, a))]
moveBinding move run =
  let assign f (km, dir) = (km, ("", True, f dir))
      rNoModifier = repeat NoModifier
      rControl = repeat Control
  in map (assign move) (zip (zip dirViMoveKey rNoModifier) movesWidth) ++
     map (assign move) (zip (zip dirMoveKey rNoModifier) movesWidth) ++
     map (assign run)  (zip (zip dirViRunKey rNoModifier) movesWidth) ++
     map (assign run)  (zip (zip dirRunKey rNoModifier) movesWidth) ++
     map (assign run)  (zip (zip dirMoveKey rControl) movesWidth) ++
     map (assign run)  (zip (zip dirRunKey rControl) movesWidth) ++
     map (assign run)  (zip (zip dirHeroKey rControl) movesWidth)

-- | Translate key from a GTK string description to our internal key type.
-- To be used, in particular, for the command bindings and macros
-- in the config file.
keyTranslate :: String -> Key
keyTranslate "less"          = Char '<'
keyTranslate "greater"       = Char '>'
keyTranslate "period"        = Char '.'
keyTranslate "colon"         = Char ':'
keyTranslate "comma"         = Char ','
keyTranslate "question"      = Char '?'
keyTranslate "dollar"        = Char '$'
keyTranslate "asterisk"      = Char '*'
keyTranslate "KP_Multiply"   = Char '*'
keyTranslate "slash"         = Char '/'
keyTranslate "KP_Divide"     = Char '/'
keyTranslate "underscore"    = Char '_'
keyTranslate "minus"         = Char '-'
keyTranslate "KP_Subtract"   = Char '-'
keyTranslate "plus"          = Char '+'
keyTranslate "KP_Add"        = Char '+'
keyTranslate "bracketleft"   = Char '['
keyTranslate "bracketright"  = Char ']'
keyTranslate "braceleft"     = Char '{'
keyTranslate "braceright"    = Char '}'
keyTranslate "Escape"        = Esc
keyTranslate "Return"        = Return
keyTranslate "space"         = Space
keyTranslate "Tab"           = Tab
keyTranslate "ISO_Left_Tab"  = BackTab
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
