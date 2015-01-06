{-# LANGUAGE DeriveGeneric #-}
-- | Frontend-independent keyboard input operations.
module Game.LambdaHack.Client.Key
  ( Key(..), showKey, handleDir, dirAllKey
  , moveBinding, mkKM, keyTranslate
  , Modifier(..), KM(..), toKM, showKM
  , escKM, spaceKM, returnKM, pgupKM, pgdnKM, leftButtonKM, rightButtonKM
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude hiding (Left, Right)

import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector

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
  | Insert
  | Delete
  | Home
  | KP !Char      -- ^ a keypad key for a character (digits and operators)
  | Char !Char    -- ^ a single printable character
  | LeftButtonPress    -- ^ left mouse button pressed
  | MiddleButtonPress  -- ^ middle mouse button pressed
  | RightButtonPress   -- ^ right mouse button pressed
  | Unknown !Text -- ^ an unknown key, registered to warn the user
  deriving (Read, Ord, Eq, Generic)

instance Binary Key

-- | Our own encoding of modifiers. Incomplete.
data Modifier =
    NoModifier
  | Shift
  | Control
  | Alt
  deriving (Read, Ord, Eq, Generic)

instance Binary Modifier

data KM = KM { key      :: !Key
             , modifier :: !Modifier
             , pointer  :: !Point }
  deriving (Read, Ord, Eq, Generic)

instance Show KM where
  show = T.unpack . showKM

instance Binary KM

toKM :: Modifier -> Key -> KM
toKM modifier key = KM{pointer=dummyPoint, ..}

-- Common and terse names for keys.
showKey :: Key -> Text
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
showKey Insert   = "INSERT"
showKey Delete   = "DELETE"
showKey (KP c)   = "KEYPAD_" <> T.singleton c
showKey (Char c) = T.singleton c
showKey LeftButtonPress = "LEFT-BUTTON"
showKey MiddleButtonPress = "MIDDLE-BUTTON"
showKey RightButtonPress = "RIGHT-BUTTON"
showKey (Unknown s) = s

-- | Show a key with a modifier, if any.
showKM :: KM -> Text
showKM KM{modifier=Shift, key} = "SHIFT-" <> showKey key
showKM KM{modifier=Control, key} = "CTRL-" <> showKey key
showKM KM{modifier=Alt, key} = "ALT-" <> showKey key
showKM KM{modifier=NoModifier, key} = showKey key

escKM :: KM
escKM = toKM NoModifier Esc

spaceKM :: KM
spaceKM = toKM NoModifier Space

returnKM :: KM
returnKM = toKM NoModifier Return

pgupKM :: KM
pgupKM = toKM NoModifier PgUp

pgdnKM :: KM
pgdnKM = toKM NoModifier PgDn

leftButtonKM :: KM
leftButtonKM = toKM NoModifier LeftButtonPress

rightButtonKM :: KM
rightButtonKM = toKM NoModifier RightButtonPress

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

dirMoveNoModifier :: Bool -> Bool -> [Key]
dirMoveNoModifier configVi configLaptop =
  dirKeypadKey ++ if configVi then dirViKey
                  else if configLaptop then dirLaptopKey
                  else []

dirRunNoModifier :: Bool -> Bool -> [Key]
dirRunNoModifier configVi configLaptop =
  dirKeypadShiftKey ++ if configVi then dirViShiftKey
                       else if configLaptop then dirLaptopShiftKey
                       else []

dirRunControl :: [Key]
dirRunControl = dirKeypadKey
                ++ dirKeypadShiftKey
                ++ (map Char dirKeypadShiftChar)

dirAllKey :: Bool -> Bool -> [Key]
dirAllKey configVi configLaptop =
  dirMoveNoModifier configVi configLaptop
  ++ dirRunNoModifier configVi configLaptop
  ++ dirRunControl

-- | Configurable event handler for the direction keys.
-- Used for directed commands such as close door.
handleDir :: Bool -> Bool -> KM -> (Vector -> a) -> a -> a
handleDir configVi configLaptop KM{modifier=NoModifier, key} h k =
  let assocs = zip (dirAllKey configVi configLaptop) $ cycle moves
  in maybe k h (lookup key assocs)
handleDir _ _ _ _ k = k

-- | Binding of both sets of movement keys.
moveBinding :: Bool -> Bool -> (Vector -> a) -> (Vector -> a)
            -> [(KM, a)]
moveBinding configVi configLaptop move run =
  let assign f (km, dir) = (km, f dir)
      mapMove modifier keys =
        map (assign move) (zip
          (zipWith toKM (repeat modifier) keys) moves)
      mapRun modifier keys =
        map (assign run) (zip
          (zipWith toKM (repeat modifier) keys) moves)
  in mapMove NoModifier (dirMoveNoModifier configVi configLaptop)
     ++ mapRun NoModifier (dirRunNoModifier configVi configLaptop)
     ++ mapRun Control dirRunControl

mkKM :: String -> KM
mkKM s = let mkKey sk =
               case keyTranslate sk of
                 Unknown _ -> assert `failure` "unknown key" `twith` s
                 key -> key
         in case s of
           ('S':'H':'I':'F':'T':'-':rest) -> toKM Shift (mkKey rest)
           ('C':'T':'R':'L':'-':rest) -> toKM Control (mkKey rest)
           ('A':'L':'T':'-':rest) -> toKM Alt (mkKey rest)
           _ -> toKM NoModifier (mkKey s)

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
keyTranslate "KP_Divide"     = KP '/'
keyTranslate "bar"           = Char '|'
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
keyTranslate "at"            = Char '@'
keyTranslate "apostrophe"    = Char '\''
keyTranslate "Escape"        = Esc
keyTranslate "Return"        = Return
keyTranslate "space"         = Space
keyTranslate "Tab"           = Tab
keyTranslate "ISO_Left_Tab"  = BackTab
keyTranslate "BackSpace"     = BackSpace
keyTranslate "Up"            = Up
keyTranslate "KP_Up"         = Up
keyTranslate "Down"          = Down
keyTranslate "KP_Down"       = Down
keyTranslate "Left"          = Left
keyTranslate "KP_Left"       = Left
keyTranslate "Right"         = Right
keyTranslate "KP_Right"      = Right
keyTranslate "Home"          = Home
keyTranslate "KP_Home"       = Home
keyTranslate "End"           = End
keyTranslate "KP_End"        = End
keyTranslate "Page_Up"       = PgUp
keyTranslate "KP_Page_Up"    = PgUp
keyTranslate "Prior"         = PgUp
keyTranslate "KP_Prior"      = PgUp
keyTranslate "Page_Down"     = PgDn
keyTranslate "KP_Page_Down"  = PgDn
keyTranslate "Next"          = PgDn
keyTranslate "KP_Next"       = PgDn
keyTranslate "Begin"         = Begin
keyTranslate "KP_Begin"      = Begin
keyTranslate "Clear"         = Begin
keyTranslate "KP_Clear"      = Begin
keyTranslate "Center"        = Begin
keyTranslate "KP_Center"     = Begin
keyTranslate "Insert"        = Insert
keyTranslate "KP_Insert"     = Insert
keyTranslate "Delete"        = Delete
keyTranslate "KP_Delete"     = Delete
keyTranslate "KP_Enter"      = Return
keyTranslate "LeftButtonPress" = LeftButtonPress
keyTranslate "MiddleButtonPress" = MiddleButtonPress
keyTranslate "RightButtonPress" = RightButtonPress
keyTranslate ['K','P','_',c] = KP c
keyTranslate [c]             = Char c
keyTranslate s               = Unknown $ T.pack s
