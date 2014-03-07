{-# LANGUAGE DeriveGeneric #-}
-- | Frontend-independent keyboard input operations.
module Game.LambdaHack.Common.Key
  ( Key(..), handleDir, dirAllKey
  , moveBinding, mkKM, keyTranslate, Modifier(..), KM(..), showKM, escKey
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude hiding (Left, Right)

import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Vector

-- TODO: if the file grows much larger, split it and move a part to Utils/

-- | Frontend-independent datatype to represent keys.
data Key =
    Esc
  | Return
  | Space
  | Tab
  | BackTab
  | BackSpace
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
  deriving (Read, Ord, Eq, Generic)

instance Binary Key

-- | Our own encoding of modifiers. Incomplete.
data Modifier =
    NoModifier
  | Control
  deriving (Read, Ord, Eq, Generic)

instance Binary Modifier

data KM = KM {modifier :: !Modifier, key :: !Key}
  deriving (Read, Ord, Eq, Generic)

instance Show KM where
  show = T.unpack . showKM

instance Binary KM

-- Common and terse names for keys.
showKey :: Key -> Text
showKey (Char c) = T.singleton c
showKey Esc      = "ESC"
showKey Return   = "RET"
showKey Space    = "SPACE"
showKey Tab      = "TAB"
showKey BackTab  = "SHIFT-TAB"
showKey BackSpace = "BACKSPACE"
showKey Up       = "UP"
showKey Down     = "DOWN"
showKey Left     = "LEFT"
showKey Right    = "RIGHT"
showKey Home     = "HOME"
showKey End      = "END"
showKey PgUp     = "PGUP"
showKey PgDn     = "PGDOWN"
showKey Begin    = "BEGIN"
showKey (KP c)   = "KEYPAD_" <> T.singleton c
showKey (Unknown s) = s

-- | Show a key with a modifier, if any.
showKM :: KM -> Text
showKM KM{modifier=Control, key} = "CTRL-" <> showKey key
showKM KM{modifier=NoModifier, key} = showKey key

escKey :: KM
escKey = KM {modifier = NoModifier, key = Esc}

dirKeypadKey :: [Key]
dirKeypadKey = [Home, Up, PgUp, Right, PgDn, Down, End, Left]

dirKeypadShiftChar :: [Char]
dirKeypadShiftChar = ['7', '8', '9', '6', '3', '2', '1', '4']

dirKeypadShiftKey :: [Key]
dirKeypadShiftKey = map KP dirKeypadShiftChar

dirLaptopKey :: [Key]
dirLaptopKey = map Char ['7', '8', '9', 'o', 'l', 'k', 'j', 'u']

dirLaptopShiftKey :: [Key]
dirLaptopShiftKey = map Char ['&', '*', '(', 'O', 'L', 'K', 'J', 'U']

dirViChar :: [Char]
dirViChar = ['y', 'k', 'u', 'l', 'n', 'j', 'b', 'h']

dirViKey :: [Key]
dirViKey = map Char dirViChar

dirViShiftKey :: [Key]
dirViShiftKey = map (Char . Char.toUpper) dirViChar

dirAllKey :: Bool -> Bool -> [Key]
dirAllKey configVi configLaptop = dirKeypadKey ++ if configVi
                                                  then dirViKey
                                                  else if configLaptop
                                                       then dirLaptopKey
                                                       else []

-- | Configurable event handler for the direction keys.
-- Used for directed commands such as close door.
handleDir :: Bool -> Bool -> KM -> (Vector -> a) -> a -> a
handleDir configVi configLaptop KM{modifier=NoModifier, key} h k =
  let assocs = zip (dirAllKey configVi configLaptop) $ moves ++ moves
  in maybe k h (lookup key assocs)
handleDir _ _ _ _ k = k

-- | Binding of both sets of movement keys.
moveBinding :: Bool -> Bool -> (Vector -> a) -> (Vector -> a)
            -> [(KM, a)]
moveBinding configVi configLaptop move run =
  let assign f (km, dir) = (km, f dir)
      mapMove modifier keys =
        map (assign move) (zip (zipWith KM (repeat modifier) keys) moves)
      mapRun modifier keys =
        map (assign run) (zip (zipWith KM (repeat modifier) keys) moves)
      dirOtherKey | configVi = dirViKey
                  | configLaptop = dirLaptopKey
                  | otherwise = []
      dirOtherShiftKey | configVi = dirViShiftKey
                       | configLaptop = dirLaptopShiftKey
                       | otherwise = []
  in mapMove NoModifier dirKeypadKey
     ++ mapMove NoModifier dirOtherKey
     ++ mapRun NoModifier dirKeypadShiftKey
     ++ mapRun NoModifier dirOtherShiftKey
     ++ mapRun Control dirKeypadKey
     ++ mapRun Control dirKeypadShiftKey
     ++ mapRun Control (map Char dirKeypadShiftChar)

mkKM :: String -> KM
mkKM s = let mkKey sk =
               case keyTranslate sk of
                 Unknown _ ->
                   assert `failure` "unknown key" `twith` s
                 key -> key
         in case s of
           ('C':'T':'R':'L':'-':rest) -> KM {key=mkKey rest, modifier=Control}
           _ -> KM {key=mkKey s, modifier=NoModifier}

-- | Translate key from a GTK string description to our internal key type.
-- To be used, in particular, for the command bindings and macros
-- in the config file.
keyTranslate :: String -> Key
keyTranslate "less"          = Char '<'
keyTranslate "greater"       = Char '>'
keyTranslate "period"        = Char '.'
keyTranslate "colon"         = Char ':'
keyTranslate "semicolon"     = Char ';'
keyTranslate "comma"         = Char ','
keyTranslate "question"      = Char '?'
keyTranslate "dollar"        = Char '$'
keyTranslate "parenleft"     = Char '('
keyTranslate "parenright"    = Char ')'
keyTranslate "asterisk"      = Char '*'
keyTranslate "KP_Multiply"   = KP '*'
keyTranslate "slash"         = Char '/'
keyTranslate "KP_Divide"     = Char '/'
keyTranslate "backslash"     = Char '\\'
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
keyTranslate "ampersand"     = Char '&'
keyTranslate "apostrophe"    = Char '\''
keyTranslate "Escape"        = Esc
keyTranslate "Return"        = Return
keyTranslate "space"         = Space
keyTranslate "Tab"           = Tab
keyTranslate "ISO_Left_Tab"  = BackTab
keyTranslate "BackSpace"     = BackSpace
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
