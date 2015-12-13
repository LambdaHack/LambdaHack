{-# LANGUAGE DeriveGeneric #-}
-- | Frontend-independent keyboard input operations.
module Game.LambdaHack.Client.Key
  ( Key(..), showKey, handleDir, dirAllKey
  , moveBinding, mkKM, keyTranslate, keyTranslateWeb
  , Modifier(..), KM(..), toKM, showKM
  , escKM, spaceKM, returnKM
  , pgupKM, pgdnKM, upKM, downKM, homeKM, endKM, backspaceKM
  , leftButtonKM, rightButtonKM, deadKM
  ) where

import Control.DeepSeq
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
  | DeadKey
  deriving (Read, Ord, Eq, Generic)

instance Binary Key

instance NFData Key

-- | Our own encoding of modifiers. Incomplete.
data Modifier =
    NoModifier
  | Shift
  | Control
  | Alt
  deriving (Read, Ord, Eq, Generic)

instance Binary Modifier

instance NFData Modifier

data KM = KM { key      :: !Key
             , modifier :: !Modifier
             , pointer  :: !(Maybe Point) }
  deriving (Read, Ord, Eq, Generic)

instance NFData KM

instance Show KM where
  show = T.unpack . showKM

instance Binary KM

toKM :: Modifier -> Key -> KM
toKM modifier key = KM{pointer=Nothing, ..}

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
showKey Insert   = "INS"
showKey Delete   = "DELETE"
showKey (KP c)   = "KEYPAD_" <> T.singleton c
showKey (Char c) = T.singleton c
showKey LeftButtonPress = "LEFT-BUTTON"
showKey MiddleButtonPress = "MIDDLE-BUTTON"
showKey RightButtonPress = "RIGHT-BUTTON"
showKey (Unknown s) = s
showKey DeadKey      = "DEADKEY"

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

upKM :: KM
upKM = toKM NoModifier Up

downKM :: KM
downKM = toKM NoModifier Down

homeKM :: KM
homeKM = toKM NoModifier Home

endKM :: KM
endKM = toKM NoModifier End

backspaceKM :: KM
backspaceKM = toKM NoModifier BackSpace

leftButtonKM :: KM
leftButtonKM = toKM NoModifier LeftButtonPress

rightButtonKM :: KM
rightButtonKM = toKM NoModifier RightButtonPress

deadKM :: KM
deadKM = toKM NoModifier DeadKey

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
                ++ map Char dirKeypadShiftChar

dirRunShift :: [Key]
dirRunShift = dirRunControl

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
        map (assign move) (zip (map (toKM modifier) keys) $ cycle moves)
      mapRun modifier keys =
        map (assign run) (zip (map (toKM modifier) keys) $ cycle moves)
  in mapMove NoModifier (dirMoveNoModifier configVi configLaptop)
     ++ mapRun NoModifier (dirRunNoModifier configVi configLaptop)
     ++ mapRun Control dirRunControl
     ++ mapRun Shift dirRunShift

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
keyTranslate "asciitilde"    = Char '~'
keyTranslate "exclam"        = Char '!'
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
-- dead keys
keyTranslate "Shift_L"          = DeadKey
keyTranslate "Shift_R"          = DeadKey
keyTranslate "Control_L"        = DeadKey
keyTranslate "Control_R"        = DeadKey
keyTranslate "Super_L"          = DeadKey
keyTranslate "Super_R"          = DeadKey
keyTranslate "Menu"             = DeadKey
keyTranslate "Alt_L"            = DeadKey
keyTranslate "Alt_R"            = DeadKey
keyTranslate "ISO_Level2_Shift" = DeadKey
keyTranslate "ISO_Level3_Shift" = DeadKey
keyTranslate "ISO_Level2_Latch" = DeadKey
keyTranslate "ISO_Level3_Latch" = DeadKey
keyTranslate "Num_Lock"         = DeadKey
keyTranslate "Caps_Lock"        = DeadKey
-- numeric keypad
keyTranslate ['K','P','_',c] = KP c
-- standard characters
keyTranslate [c]             = Char c
keyTranslate s               = Unknown $ T.pack s


-- | Translate key from a Web API string description
-- (https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key#Key_values)
-- to our internal key type. To be used in web frontends.
-- Currently only the "Key values on Linux (GTK)" table taken into account.
-- TODO: KEY_LOCATION_NUMPAD
keyTranslateWeb :: String -> Key
keyTranslateWeb "Backspace"  = BackSpace
keyTranslateWeb "Tab"        = Tab
keyTranslateWeb "Clear"      = Begin
keyTranslateWeb "Enter"      = Return
keyTranslateWeb "Esc"        = Esc
keyTranslateWeb "Escape"     = Esc
keyTranslateWeb "Del"        = Delete
keyTranslateWeb "Delete"     = Delete
keyTranslateWeb "Home"       = Home
keyTranslateWeb "Up"         = Up
keyTranslateWeb "ArrowUp"    = Up
keyTranslateWeb "Down"       = Down
keyTranslateWeb "ArrowDown"  = Down
keyTranslateWeb "Left"       = Left
keyTranslateWeb "ArrowLeft"  = Left
keyTranslateWeb "Right"      = Right
keyTranslateWeb "ArrowRight" = Right
keyTranslateWeb "PageUp"     = PgUp
keyTranslateWeb "PageDown"   = PgDn
keyTranslateWeb "End"        = End
keyTranslateWeb "Insert"     = Insert
keyTranslateWeb "space"      = Space
keyTranslateWeb "Equals"     = Char '='
keyTranslateWeb "Multiply"   = KP '*'
keyTranslateWeb "Add"        = Char '+'
keyTranslateWeb "Subtract"   = Char '-'
keyTranslateWeb "Divide"     = KP '/'
-- dead keys
keyTranslateWeb "Dead"        = DeadKey
keyTranslateWeb "Shift"       = DeadKey
keyTranslateWeb "Control"     = DeadKey
keyTranslateWeb "Meta"        = DeadKey
keyTranslateWeb "Menu"        = DeadKey
keyTranslateWeb "ContextMenu" = DeadKey
keyTranslateWeb "Alt"         = DeadKey
keyTranslateWeb "AltGraph"    = DeadKey
keyTranslateWeb "Num_Lock"    = DeadKey
keyTranslateWeb "CapsLock"    = DeadKey
-- browser/webkit quirks
keyTranslateWeb ['\ESC']     = Esc
keyTranslateWeb [' ']        = Space
keyTranslateWeb ['\n']       = Return
keyTranslateWeb ['\r']       = Return
keyTranslateWeb ['\t']       = Tab
-- standard characters
keyTranslateWeb [c]          = Char c
keyTranslateWeb s            = Unknown $ T.pack s

-- TODO: with Shift?
-- keyTranslateWeb "ISO_Left_Tab"  = BackTab
