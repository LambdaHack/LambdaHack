{-# LANGUAGE DeriveGeneric #-}
-- | Frontend-independent keyboard input operations.
module Game.LambdaHack.Client.Key
  ( Key(..), showKey, handleDir, dirAllKey
  , moveBinding, mkKM, keyTranslate, keyTranslateWeb
  , Modifier(..), KM(..), showKM
  , escKM, spaceKM, safeSpaceKM, returnKM
  , pgupKM, pgdnKM, wheelNorthKM, wheelSouthKM
  , upKM, downKM, leftKM, rightKM
  , homeKM, endKM, backspaceKM
  , leftButtonReleaseKM
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude hiding (Alt, Left, Right)

import Control.DeepSeq
import Data.Binary
import qualified Data.Char as Char
import qualified Data.Text as T
import GHC.Generics (Generic)

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
  | LeftButtonRelease    -- ^ left mouse button released
  | MiddleButtonRelease  -- ^ middle mouse button released
  | RightButtonRelease   -- ^ right mouse button released
  | LeftDblClick       -- ^ left mouse button double click
  | WheelNorth  -- ^ mouse wheel rotated north
  | WheelSouth  -- ^ mouse wheel rotated south
  | Unknown !Text -- ^ an unknown key, registered to warn the user
  | DeadKey
  deriving (Ord, Eq, Generic)

instance Binary Key

instance NFData Key

-- | Our own encoding of modifiers.
data Modifier =
    NoModifier
  | Shift
  | Control
  | Alt
  deriving (Ord, Eq, Generic)

instance Binary Modifier

instance NFData Modifier

data KM = KM { modifier :: !Modifier
             , key      :: !Key }
  deriving (Ord, Eq, Generic)

instance Binary KM

instance NFData KM

instance Show KM where
  show = T.unpack . showKM

-- Common and terse names for keys.
showKey :: Key -> Text
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
showKey Delete   = "DELETE"
showKey (KP c)   = "KP_" <> T.singleton c
showKey (Char c) = T.singleton c
showKey LeftButtonPress = "LMB-PRESS"
showKey MiddleButtonPress = "MMB-PRESS"
showKey RightButtonPress = "RMB-PRESS"
showKey LeftButtonRelease = "LMB"
showKey MiddleButtonRelease = "MMB"
showKey RightButtonRelease = "RMB"
showKey LeftDblClick = "LMB-DBLCLICK"
showKey WheelNorth = "WHEEL-NORTH"
showKey WheelSouth = "WHEEL-SOUTH"
showKey (Unknown s) = "'" <> s <> "'"
showKey DeadKey      = "DEADKEY"

-- | Show a key with a modifier, if any.
showKM :: KM -> Text
showKM KM{modifier=Shift, key} = "S-" <> showKey key
showKM KM{modifier=Control, key} = "C-" <> showKey key
showKM KM{modifier=Alt, key} = "A-" <> showKey key
showKM KM{modifier=NoModifier, key} = showKey key

escKM :: KM
escKM = KM NoModifier Esc

spaceKM :: KM
spaceKM = KM NoModifier Space

safeSpaceKM :: KM
safeSpaceKM = KM NoModifier $ Unknown "Space"

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

leftButtonReleaseKM :: KM
leftButtonReleaseKM = KM NoModifier LeftButtonRelease

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
  dirKeypadKey ++ if | configVi -> dirViKey
                     | configLaptop -> dirLaptopKey
                     | otherwise -> []

dirRunNoModifier :: Bool -> Bool -> [Key]
dirRunNoModifier configVi configLaptop =
  dirKeypadShiftKey ++ if | configVi -> dirViShiftKey
                          | configLaptop -> dirLaptopShiftKey
                          | otherwise -> []

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
handleDir :: Bool -> Bool -> KM -> Maybe Vector
handleDir configVi configLaptop KM{modifier=NoModifier, key} =
  let assocs = zip (dirAllKey configVi configLaptop) $ cycle moves
  in lookup key assocs
handleDir _ _ _ = Nothing

-- | Binding of both sets of movement keys.
moveBinding :: Bool -> Bool -> (Vector -> a) -> (Vector -> a)
            -> [(KM, a)]
moveBinding configVi configLaptop move run =
  let assign f (km, dir) = (km, f dir)
      mapMove modifier keys =
        map (assign move) (zip (map (KM modifier) keys) $ cycle moves)
      mapRun modifier keys =
        map (assign run) (zip (map (KM modifier) keys) $ cycle moves)
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
           'S':'H':'I':'F':'T':'-':rest -> KM Shift (mkKey rest)
           'C':'T':'R':'L':'-':rest -> KM Control (mkKey rest)
           'A':'L':'T':'-':rest -> KM Alt (mkKey rest)
           _ -> KM NoModifier (mkKey s)

-- | Translate key from a GTK string description to our internal key type.
-- To be used, in particular, for the command bindings and macros
-- in the config file.
--
-- See https://github.com/twobob/gtk-/blob/master/gdk/keyname-table.h
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
keyTranslate "asterisk"      = Char '*'  -- for latop movement keys
keyTranslate "KP_Multiply"   = KP '*'    -- for keypad aiming
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
keyTranslate "grave"         = Char '`'
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
keyTranslate "LeftButtonRelease" = LeftButtonRelease
keyTranslate "MiddleButtonRelease" = MiddleButtonRelease
keyTranslate "RightButtonRelease" = RightButtonRelease
keyTranslate "LeftDblClick"  = LeftDblClick
keyTranslate "WheelNorth"    = WheelNorth
keyTranslate "WheelSouth"    = WheelSouth
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
keyTranslate "Caps_Lock"        = DeadKey
keyTranslate "VoidSymbol"       = DeadKey
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
keyTranslateWeb :: String -> Bool -> Key
keyTranslateWeb "Backspace"  _ = BackSpace
keyTranslateWeb "Tab"        _ = Tab
keyTranslateWeb "BackTab"    _ = BackTab
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
keyTranslateWeb "Multiply"   False = Char '*'  -- for latop movement keys
keyTranslateWeb "Multiply"   True = KP '*'     -- for keypad aiming
keyTranslateWeb "Add"        _ = Char '+'
keyTranslateWeb "Subtract"   _ = Char '-'
keyTranslateWeb "Divide"     False = Char '/'
keyTranslateWeb "Divide"     True = KP '/'
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
keyTranslateWeb "CapsLock"    _ = DeadKey
keyTranslateWeb "Win"         _ = DeadKey
-- browser/webkit quirks
keyTranslateWeb ['\ESC']     _ = Esc
keyTranslateWeb [' ']        _ = Space
keyTranslateWeb ['\n']       _ = Return
keyTranslateWeb ['\r']       _ = Return
keyTranslateWeb ['\t']       _ = Tab
-- standard characters
keyTranslateWeb [c]          _ = Char c
keyTranslateWeb s            _ = Unknown $ T.pack s
