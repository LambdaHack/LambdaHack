{-# LANGUAGE DeriveGeneric #-}
-- | Frontend-independent keyboard input operations.
module Game.LambdaHack.Client.UI.Key
  ( Key(..), Modifier(..), KM(..), KMP(..)
  , showKey, showKM
  , escKM, controlEscKM, spaceKM, safeSpaceKM, undefinedKM, returnKM
  , pgupKM, pgdnKM, wheelNorthKM, wheelSouthKM
  , upKM, downKM, leftKM, rightKM
  , homeKM, endKM, backspaceKM, controlP
  , leftButtonReleaseKM, middleButtonReleaseKM, rightButtonReleaseKM
  , dirAllKey, handleDir, moveBinding, mkKM, mkChar
  , keyTranslate, keyTranslateWeb
  , dirMoveNoModifier, dirRunNoModifier, dirRunControl, dirRunShift
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , dirKeypadKey, dirKeypadShiftChar, dirKeypadShiftKey
  , dirLeftHandKey, dirLeftHandShiftKey
  , dirViChar, dirViKey, dirViShiftKey
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude hiding (Left, Right)

import           Control.DeepSeq
import           Data.Binary
import qualified Data.Char as Char
import           GHC.Generics (Generic)

import Game.LambdaHack.Client.UI.PointUI
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
  | PrintScreen
  | Home
  | KP Char      -- ^ a keypad key for a character (digits and operators)
  | Char Char    -- ^ a single printable character
  | Fun Int      -- ^ function key
  | LeftButtonPress    -- ^ left mouse button pressed
  | MiddleButtonPress  -- ^ middle mouse button pressed
  | RightButtonPress   -- ^ right mouse button pressed
  | LeftButtonRelease    -- ^ left mouse button released
  | MiddleButtonRelease  -- ^ middle mouse button released
  | RightButtonRelease   -- ^ right mouse button released
  | WheelNorth  -- ^ mouse wheel rotated north
  | WheelSouth  -- ^ mouse wheel rotated south
  | Unknown String -- ^ an unknown key, registered to warn the user
  | DeadKey
  deriving (Ord, Eq, Generic)

instance Binary Key

instance NFData Key

-- | Our own encoding of modifiers.
data Modifier =
    NoModifier
  | ControlShift
  | AltShift
  | Shift
  | Control
  | Alt
  deriving (Show, Ord, Eq, Generic)

instance Binary Modifier

instance NFData Modifier

-- | Key and modifier.
data KM = KM { modifier :: Modifier
             , key      :: Key }
  deriving (Ord, Eq, Generic)

instance Binary KM

instance NFData KM

instance Show KM where
  show = showKM

-- | Key, modifier and position of mouse pointer.
data KMP = KMP { kmpKeyMod  :: KM
               , kmpPointer :: PointUI }

-- | Common and terse names for keys.
showKey :: Key -> String
showKey Esc      = "ESC"
showKey Return   = "RET"
showKey Space    = "SPACE"
showKey Tab      = "TAB"
showKey BackTab  = "S-TAB"
showKey BackSpace = "BACKSPACE"
showKey Up       = "UP"
showKey Down     = "DOWN"
showKey Left     = "LEFT"
showKey Right    = "RIGHT"
showKey Home     = "HOME"
showKey End      = "END"
showKey PgUp     = "PGUP"
showKey PgDn     = "PGDN"
showKey Begin    = "BEGIN"
showKey Insert   = "INS"
showKey Delete   = "DEL"
showKey PrintScreen = "PRTSCR"
showKey (KP c)   = "KP_" ++ [c]
showKey (Char c) = [c]
showKey (Fun n) = "F" ++ show n
showKey LeftButtonPress = "LMB-PRESS"
showKey MiddleButtonPress = "MMB-PRESS"
showKey RightButtonPress = "RMB-PRESS"
showKey LeftButtonRelease = "LMB"
showKey MiddleButtonRelease = "MMB"
showKey RightButtonRelease = "RMB"
showKey WheelNorth = "WHEEL-UP"
showKey WheelSouth = "WHEEL-DN"
showKey (Unknown s) = "'" ++ s ++ "'"
showKey DeadKey      = "DEADKEY"

-- | Show a key with a modifier, if any.
showKM :: KM -> String
showKM KM{modifier=NoModifier, key} = showKey key
showKM KM{modifier=ControlShift, key} = "C-S-" ++ showKey key
showKM KM{modifier=AltShift, key} = "A-S-" ++ showKey key
showKM KM{modifier=Shift, key} = "S-" ++ showKey key
showKM KM{modifier=Control, key} = "C-" ++ showKey key
showKM KM{modifier=Alt, key} = "A-" ++ showKey key

escKM :: KM
escKM = KM NoModifier Esc

controlEscKM :: KM
controlEscKM = KM Control Esc

spaceKM :: KM
spaceKM = KM NoModifier Space

safeSpaceKM :: KM
safeSpaceKM = KM NoModifier $ Unknown "SAFE_SPACE"

undefinedKM :: KM
undefinedKM = KM NoModifier $ Unknown "UNDEFINED KEY"

returnKM :: KM
returnKM = KM NoModifier Return

pgupKM :: KM
pgupKM = KM NoModifier PgUp

pgdnKM :: KM
pgdnKM = KM NoModifier PgDn

wheelNorthKM :: KM
wheelNorthKM = KM NoModifier WheelNorth

wheelSouthKM :: KM
wheelSouthKM = KM NoModifier WheelSouth

upKM :: KM
upKM = KM NoModifier Up

downKM :: KM
downKM = KM NoModifier Down

leftKM :: KM
leftKM = KM NoModifier Left

rightKM :: KM
rightKM = KM NoModifier Right

homeKM :: KM
homeKM = KM NoModifier Home

endKM :: KM
endKM = KM NoModifier End

backspaceKM :: KM
backspaceKM = KM NoModifier BackSpace

controlP :: KM
controlP = KM Control (Char 'P')

leftButtonReleaseKM :: KM
leftButtonReleaseKM = KM NoModifier LeftButtonRelease

middleButtonReleaseKM :: KM
middleButtonReleaseKM = KM NoModifier MiddleButtonRelease

rightButtonReleaseKM :: KM
rightButtonReleaseKM = KM NoModifier RightButtonRelease

dirKeypadKey :: [Key]
dirKeypadKey = [Home, Up, PgUp, Right, PgDn, Down, End, Left]

dirKeypadShiftChar :: [Char]
dirKeypadShiftChar = ['7', '8', '9', '6', '3', '2', '1', '4']

dirKeypadShiftKey :: [Key]
dirKeypadShiftKey = map KP dirKeypadShiftChar

dirLeftHandKey :: [Key]
dirLeftHandKey = map Char ['q', 'w', 'e', 'd', 'c', 'x', 'z', 'a']

dirLeftHandShiftKey :: [Key]
dirLeftHandShiftKey = map Char ['Q', 'W', 'E', 'D', 'C', 'X', 'Z', 'A']

dirViChar :: [Char]
dirViChar = ['y', 'k', 'u', 'l', 'n', 'j', 'b', 'h']

dirViKey :: [Key]
dirViKey = map Char dirViChar

dirViShiftKey :: [Key]
dirViShiftKey = map (Char . Char.toUpper) dirViChar

dirMoveNoModifier :: Bool -> Bool -> [Key]
dirMoveNoModifier uVi uLeftHand =
  dirKeypadKey ++ (if uVi then dirViKey else [])
               ++ (if uLeftHand then dirLeftHandKey else [])

dirRunNoModifier :: Bool -> Bool -> [Key]
dirRunNoModifier uVi uLeftHand =
  dirKeypadShiftKey ++ (if uVi then dirViShiftKey else [])
                    ++ (if uLeftHand then dirLeftHandShiftKey else [])

dirRunControl :: [Key]
dirRunControl = dirKeypadKey
                ++ dirKeypadShiftKey
                ++ map Char dirKeypadShiftChar

dirRunShift :: [Key]
dirRunShift = dirRunControl

dirAllKey :: Bool -> Bool -> [Key]
dirAllKey uVi uLeftHand =
  dirMoveNoModifier uVi uLeftHand
  ++ dirRunNoModifier uVi uLeftHand
  ++ dirRunControl

-- | Configurable event handler for the direction keys.
-- Used for directed commands such as close door.
handleDir :: [Key] -> KM -> Maybe Vector
handleDir dirKeys KM{modifier=NoModifier, key} =
  let assocs = zip dirKeys $ cycle moves
  in lookup key assocs
handleDir _ _ = Nothing

-- | Binding of both sets of movement keys, vi and laptop.
moveBinding :: Bool -> Bool -> (Vector -> a) -> (Vector -> a)
            -> [(KM, a)]
moveBinding uVi uLeftHand move run =
  let assign f (km, dir) = (km, f dir)
      mapMove modifier keys =
        map (assign move) (zip (map (KM modifier) keys) $ cycle moves)
      mapRun modifier keys =
        map (assign run) (zip (map (KM modifier) keys) $ cycle moves)
  in mapMove NoModifier (dirMoveNoModifier uVi uLeftHand)
     ++ mapRun NoModifier (dirRunNoModifier uVi uLeftHand)
     ++ mapRun Control dirRunControl
     ++ mapRun Shift dirRunShift

mkKM :: String -> KM
mkKM s = let mkKey sk =
               case keyTranslate sk of
                 Unknown _ -> error $ "unknown key" `showFailure` s
                 key -> key
         in case s of
           'C':'-':'S':'-':rest -> KM ControlShift (mkKey rest)
           'S':'-':'C':'-':rest -> KM ControlShift (mkKey rest)
           'A':'-':'S':'-':rest -> KM AltShift (mkKey rest)
           'S':'-':'A':'-':rest -> KM AltShift (mkKey rest)
           'S':'-':rest -> KM Shift (mkKey rest)
           'C':'-':rest -> KM Control (mkKey rest)
           'A':'-':rest -> KM Alt (mkKey rest)
           _ -> KM NoModifier (mkKey s)

mkChar :: Char -> KM
mkChar c = KM NoModifier $ Char c

-- | Translate key from a GTK string description to our internal key type.
-- To be used, in particular, for the command bindings and macros
-- in the config file.
--
-- See <https://github.com/twobob/gtk-/blob/master/gdk/keynames.txt>
keyTranslate :: String -> Key
keyTranslate "less"          = Char '<'
keyTranslate "greater"       = Char '>'
keyTranslate "period"        = Char '.'
keyTranslate "colon"         = Char ':'
keyTranslate "semicolon"     = Char ';'
keyTranslate "comma"         = Char ','
keyTranslate "question"      = Char '?'
keyTranslate "numbersign"    = Char '#'
keyTranslate "dollar"        = Char '$'
keyTranslate "parenleft"     = Char '('
keyTranslate "parenright"    = Char ')'
keyTranslate "asterisk"      = Char '*'  -- KP and normal are merged here
keyTranslate "KP_Multiply"   = Char '*'
keyTranslate "slash"         = Char '/'
keyTranslate "KP_Divide"     = Char '/'
keyTranslate "bar"           = Char '|'
keyTranslate "backslash"     = Char '\\'
keyTranslate "asciicircum"   = Char '^'
keyTranslate "underscore"    = Char '_'
keyTranslate "minus"         = Char '-'
keyTranslate "KP_Subtract"   = Char '-'  -- KP and normal are merged here
keyTranslate "plus"          = Char '+'
keyTranslate "KP_Add"        = Char '+'  -- KP and normal are merged here
keyTranslate "equal"         = Char '='
keyTranslate "bracketleft"   = Char '['
keyTranslate "bracketright"  = Char ']'
keyTranslate "braceleft"     = Char '{'
keyTranslate "braceright"    = Char '}'
keyTranslate "caret"         = Char '^'
keyTranslate "ampersand"     = Char '&'
keyTranslate "at"            = Char '@'
keyTranslate "asciitilde"    = Char '~'
keyTranslate "grave"         = Char '`'
keyTranslate "exclam"        = Char '!'
keyTranslate "apostrophe"    = Char '\''
keyTranslate "quotedbl"      = Char '"'
keyTranslate "Escape"        = Esc
keyTranslate "ESC"           = Esc
keyTranslate "Return"        = Return
keyTranslate "RET"           = Return
keyTranslate "space"         = Space
keyTranslate "SPACE"         = Space
keyTranslate "Tab"           = Tab
keyTranslate "TAB"           = Tab
keyTranslate "BackTab"       = BackTab
keyTranslate "ISO_Left_Tab"  = BackTab
keyTranslate "BackSpace"     = BackSpace
keyTranslate "BACKSPACE"     = BackSpace
keyTranslate "Up"            = Up
keyTranslate "UP"            = Up
keyTranslate "KP_Up"         = Up
keyTranslate "Down"          = Down
keyTranslate "DOWN"          = Down
keyTranslate "KP_Down"       = Down
keyTranslate "Left"          = Left
keyTranslate "LEFT"          = Left
keyTranslate "KP_Left"       = Left
keyTranslate "Right"         = Right
keyTranslate "RIGHT"         = Right
keyTranslate "KP_Right"      = Right
keyTranslate "Home"          = Home
keyTranslate "HOME"          = Home
keyTranslate "KP_Home"       = Home
keyTranslate "End"           = End
keyTranslate "END"           = End
keyTranslate "KP_End"        = End
keyTranslate "Page_Up"       = PgUp
keyTranslate "PGUP"          = PgUp
keyTranslate "KP_Page_Up"    = PgUp
keyTranslate "Prior"         = PgUp
keyTranslate "KP_Prior"      = PgUp
keyTranslate "Page_Down"     = PgDn
keyTranslate "PGDN"          = PgDn
keyTranslate "KP_Page_Down"  = PgDn
keyTranslate "Next"          = PgDn
keyTranslate "KP_Next"       = PgDn
keyTranslate "Begin"         = Begin
keyTranslate "BEGIN"         = Begin
keyTranslate "KP_Begin"      = Begin
keyTranslate "Clear"         = Begin
keyTranslate "KP_Clear"      = Begin
keyTranslate "Center"        = Begin
keyTranslate "KP_Center"     = Begin
keyTranslate "Insert"        = Insert
keyTranslate "INS"           = Insert
keyTranslate "KP_Insert"     = Insert
keyTranslate "Delete"        = Delete
keyTranslate "DEL"           = Delete
keyTranslate "KP_Delete"     = Delete
keyTranslate "KP_Enter"      = Return
keyTranslate "F1"            = Fun 1
keyTranslate "F2"            = Fun 2
keyTranslate "F3"            = Fun 3
keyTranslate "F4"            = Fun 4
keyTranslate "F5"            = Fun 5
keyTranslate "F6"            = Fun 6
keyTranslate "F7"            = Fun 7
keyTranslate "F8"            = Fun 8
keyTranslate "F9"            = Fun 9
keyTranslate "F10"           = Fun 10
keyTranslate "F11"           = Fun 11
keyTranslate "F12"           = Fun 12
keyTranslate "LeftButtonPress" = LeftButtonPress
keyTranslate "LMB-PRESS" = LeftButtonPress
keyTranslate "MiddleButtonPress" = MiddleButtonPress
keyTranslate "MMB-PRESS" = MiddleButtonPress
keyTranslate "RightButtonPress" = RightButtonPress
keyTranslate "RMB-PRESS" = RightButtonPress
keyTranslate "LeftButtonRelease" = LeftButtonRelease
keyTranslate "LMB" = LeftButtonRelease
keyTranslate "MiddleButtonRelease" = MiddleButtonRelease
keyTranslate "MMB" = MiddleButtonRelease
keyTranslate "RightButtonRelease" = RightButtonRelease
keyTranslate "RMB" = RightButtonRelease
keyTranslate "WheelNorth"    = WheelNorth
keyTranslate "WHEEL-UP"      = WheelNorth
keyTranslate "WheelSouth"    = WheelSouth
keyTranslate "WHEEL-DN"      = WheelSouth
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
keyTranslate "Meta_L"           = DeadKey
keyTranslate "Meta_R"           = DeadKey
keyTranslate "ISO_Level2_Shift" = DeadKey
keyTranslate "ISO_Level3_Shift" = DeadKey
keyTranslate "ISO_Level2_Latch" = DeadKey
keyTranslate "ISO_Level3_Latch" = DeadKey
keyTranslate "Num_Lock"         = DeadKey
keyTranslate "NumLock"          = DeadKey
keyTranslate "Caps_Lock"        = DeadKey
keyTranslate "CapsLock"         = DeadKey
keyTranslate "VoidSymbol"       = DeadKey
-- numeric keypad
keyTranslate ['K','P','_',c] = KP c
-- standard characters
keyTranslate [c]             = Char c
keyTranslate s               = Unknown s

-- | Translate key from a Web API string description
-- (<https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key#Key_values>)
-- to our internal key type. To be used in web frontends.
-- The argument says whether Shift is pressed.
keyTranslateWeb :: String -> Bool -> Key
keyTranslateWeb "1"          True = KP '1'
keyTranslateWeb "2"          True = KP '2'
keyTranslateWeb "3"          True = KP '3'
keyTranslateWeb "4"          True = KP '4'
keyTranslateWeb "5"          True = KP '5'
keyTranslateWeb "6"          True = KP '6'
keyTranslateWeb "7"          True = KP '7'
keyTranslateWeb "8"          True = KP '8'
keyTranslateWeb "9"          True = KP '9'
keyTranslateWeb "End"        True = KP '1'
keyTranslateWeb "ArrowDown"  True = KP '2'
keyTranslateWeb "PageDown"   True = KP '3'
keyTranslateWeb "ArrowLeft"  True = KP '4'
keyTranslateWeb "Begin"      True = KP '5'
keyTranslateWeb "Clear"      True = KP '5'
keyTranslateWeb "ArrowRight" True = KP '6'
keyTranslateWeb "Home"       True = KP '7'
keyTranslateWeb "ArrowUp"    True = KP '8'
keyTranslateWeb "PageUp"     True = KP '9'
keyTranslateWeb "Backspace"  _ = BackSpace
keyTranslateWeb "Tab"        True = BackTab
keyTranslateWeb "Tab"        False = Tab
keyTranslateWeb "BackTab"    _ = BackTab
keyTranslateWeb "Begin"      _ = Begin
keyTranslateWeb "Clear"      _ = Begin
keyTranslateWeb "Enter"      _ = Return
keyTranslateWeb "Esc"        _ = Esc
keyTranslateWeb "Escape"     _ = Esc
keyTranslateWeb "Del"        _ = Delete
keyTranslateWeb "Delete"     _ = Delete
keyTranslateWeb "Home"       _ = Home
keyTranslateWeb "Up"         _ = Up
keyTranslateWeb "ArrowUp"    _ = Up
keyTranslateWeb "Down"       _ = Down
keyTranslateWeb "ArrowDown"  _ = Down
keyTranslateWeb "Left"       _ = Left
keyTranslateWeb "ArrowLeft"  _ = Left
keyTranslateWeb "Right"      _ = Right
keyTranslateWeb "ArrowRight" _ = Right
keyTranslateWeb "PageUp"     _ = PgUp
keyTranslateWeb "PageDown"   _ = PgDn
keyTranslateWeb "End"        _ = End
keyTranslateWeb "Insert"     _ = Insert
keyTranslateWeb "space"      _ = Space
keyTranslateWeb "Equals"     _ = Char '='
keyTranslateWeb "Multiply"   _ = Char '*'  -- KP and normal are merged here
keyTranslateWeb "*"          _ = Char '*'
keyTranslateWeb "Add"        _ = Char '+'  -- KP and normal are merged here
keyTranslateWeb "Subtract"   _ = Char '-'  -- KP and normal are merged here
keyTranslateWeb "Divide"     True = Char '?'
keyTranslateWeb "Divide"     False = Char '/' -- KP and normal are merged here
keyTranslateWeb "/"          True = Char '?'
keyTranslateWeb "/"          False = Char '/' -- KP and normal are merged here
keyTranslateWeb "Decimal"    _ = Char '.'  -- dot and comma are merged here
keyTranslateWeb "Separator"  _ = Char '.'  -- to sidestep national standards
keyTranslateWeb "F1"         _ = Fun 1
keyTranslateWeb "F2"         _ = Fun 2
keyTranslateWeb "F3"         _ = Fun 3
keyTranslateWeb "F4"         _ = Fun 4
keyTranslateWeb "F5"         _ = Fun 5
keyTranslateWeb "F6"         _ = Fun 6
keyTranslateWeb "F7"         _ = Fun 7
keyTranslateWeb "F8"         _ = Fun 8
keyTranslateWeb "F9"         _ = Fun 9
keyTranslateWeb "F10"        _ = Fun 10
keyTranslateWeb "F11"        _ = Fun 11
keyTranslateWeb "F12"        _ = Fun 12
-- dead keys
keyTranslateWeb "Dead"        _ = DeadKey
keyTranslateWeb "Shift"       _ = DeadKey
keyTranslateWeb "Control"     _ = DeadKey
keyTranslateWeb "Meta"        _ = DeadKey
keyTranslateWeb "Menu"        _ = DeadKey
keyTranslateWeb "ContextMenu" _ = DeadKey
keyTranslateWeb "Alt"         _ = DeadKey
keyTranslateWeb "AltGraph"    _ = DeadKey
keyTranslateWeb "Num_Lock"    _ = DeadKey
keyTranslateWeb "NumLock"     _ = DeadKey
keyTranslateWeb "Caps_Lock"   _ = DeadKey
keyTranslateWeb "CapsLock"    _ = DeadKey
keyTranslateWeb "Win"         _ = DeadKey
-- browser quirks
keyTranslateWeb "Unidentified" _ = Begin  -- hack for Firefox
keyTranslateWeb ['\ESC']     _ = Esc
keyTranslateWeb [' ']        _ = Space
keyTranslateWeb ['\n']       _ = Return
keyTranslateWeb ['\r']       _ = DeadKey
keyTranslateWeb ['\t']       _ = Tab
-- standard characters
keyTranslateWeb [c]          _ = Char c
keyTranslateWeb s            _ = Unknown s
