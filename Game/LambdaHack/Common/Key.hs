-- | Frontend-independent keyboard input operations.
module Game.LambdaHack.Common.Key
  ( Key(..), handleDir, dirAllMoveKey
  , moveBinding, keyTranslate, Modifier(..), KM(..), showKM, escKey
  ) where

import Data.Binary
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (Left, Right)

import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Common.VectorXY

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
  | KP !Char      -- ^ a keypad key for a character (digits and operators)
  | Char !Char    -- ^ a single printable character
  | Unknown !Text -- ^ an unknown key, registered to warn the user
  deriving (Ord, Eq)

instance Binary Key where
  put Esc         = putWord8 0
  put Return      = putWord8 1
  put Space       = putWord8 2
  put Tab         = putWord8 3
  put BackTab     = putWord8 4
  put PgUp        = putWord8 5
  put PgDn        = putWord8 6
  put Left        = putWord8 7
  put Right       = putWord8 8
  put Up          = putWord8 9
  put Down        = putWord8 10
  put End         = putWord8 11
  put Begin       = putWord8 12
  put Home        = putWord8 13
  put (KP c)      = putWord8 14 >> put c
  put (Char c)    = putWord8 15 >> put c
  put (Unknown s) = putWord8 16 >> put s
  get = do
    tag <- getWord8
    case tag of
      0  -> return Esc
      1  -> return Return
      2  -> return Space
      3  -> return Tab
      4  -> return BackTab
      5  -> return PgUp
      6  -> return PgDn
      7  -> return Left
      8  -> return Right
      9  -> return Up
      10 -> return Down
      11 -> return End
      12 -> return Begin
      13 -> return Home
      14 -> fmap KP get
      15 -> fmap Char get
      16 -> fmap Unknown get
      _ -> fail "no parse (Key)"

-- | Our own encoding of modifiers. Incomplete.
data Modifier =
    NoModifier
  | Control
  deriving (Ord, Eq)

instance Binary Modifier where
  put NoModifier = putWord8 0
  put Control    = putWord8 1
  get = do
    tag <- getWord8
    case tag of
      0  -> return NoModifier
      1  -> return Control
      _ -> fail "no parse (Modifier)"

data KM = KM {modifier :: !Modifier, key :: !Key}
  deriving (Ord, Eq)

instance Show KM where
  show = T.unpack . showKM

instance Binary KM where
  put KM {..} = do
    put key
    put modifier
  get = do
    key <- get
    modifier <- get
    return KM {..}

-- Common and terse names for keys.
showKey :: Key -> Text
showKey (Char c) = T.singleton c
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
showKey (KP c)   = "KEYPAD(" <> T.singleton c <> ")"
showKey (Unknown s) = s

-- | Show a key with a modifier, if any.
showKM :: KM -> Text
showKM KM{modifier=Control, key} = "CTRL-" <> showKey key
showKM KM{modifier=NoModifier, key} = showKey key

escKey :: KM
escKey = KM {modifier = NoModifier, key = Esc}

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
handleDir :: X -> KM -> (Vector -> a) -> a -> a
handleDir lxsize KM{modifier=NoModifier, key} h k =
  let mvs = moves lxsize
      assocs = zip dirAllMoveKey $ mvs ++ mvs
  in maybe k h (lookup key assocs)
handleDir _lxsize _ _h k = k

-- TODO: deduplicate
-- | Binding of both sets of movement keys.
moveBinding :: (VectorXY -> a) -> (VectorXY -> a)
            -> [(KM, a)]
moveBinding move run =
  let assign f (km, dir) = (km, f dir)
      rNoModifier = repeat NoModifier
      rControl = repeat Control
  in map (assign move) (zip (zipWith KM rNoModifier dirViMoveKey) movesXY) ++
     map (assign move) (zip (zipWith KM rNoModifier dirMoveKey) movesXY) ++
     map (assign run)  (zip (zipWith KM rNoModifier dirViRunKey) movesXY) ++
     map (assign run)  (zip (zipWith KM rNoModifier dirRunKey) movesXY) ++
     map (assign run)  (zip (zipWith KM rControl dirMoveKey) movesXY) ++
     map (assign run)  (zip (zipWith KM rControl dirRunKey) movesXY) ++
     map (assign run)  (zip (zipWith KM rControl dirHeroKey ) movesXY)

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
keyTranslate "equal"         = Char '='
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
keyTranslate s               = Unknown $ T.pack s
