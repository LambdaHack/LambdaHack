module Keys where

import Prelude hiding (Left, Right)

import Geometry hiding (Up, Down)
import Data.Maybe
import Data.List as L
import Data.Map as M

-- | Library-independent datatype to represent keys.
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
  | KP Char    -- ^ a keypad key for a character (digits and operators)
  | Char Char  -- ^ a single printable character
  | Dbg String -- ^ an unknown key, collected for debugging
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
showKey (Dbg s)  = s

-- | Maps a keypad movement key to the canonical form.
-- Hard-coded not to bloat config files.
canonMoveKey :: Key -> Key
canonMoveKey e =
  case e of
    KP '8' -> Char 'K'
    KP '2' -> Char 'J'
    KP '4' -> Char 'H'
    KP '6' -> Char 'L'
    KP '7' -> Char 'Y'
    KP '9' -> Char 'U'
    KP '1' -> Char 'B'
    KP '3' -> Char 'N'
    KP '5' -> Char '.'
    Up     -> Char 'k'
    Down   -> Char 'j'
    Left   -> Char 'h'
    Right  -> Char 'l'
    Home   -> Char 'y'
    PgUp   -> Char 'u'
    End    -> Char 'b'
    PgDn   -> Char 'n'
    Begin  -> Char '.'
    k      -> k

-- | Configurable event handler for the direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as open/close.
handleDirection :: Key -> (Dir -> a) -> a -> a
handleDirection e h k =
  case e of
    Char 'k' -> h up
    Char 'j' -> h down
    Char 'h' -> h left
    Char 'l' -> h right
    Char 'y' -> h upleft
    Char 'u' -> h upright
    Char 'b' -> h downleft
    Char 'n' -> h downright
    _          -> k

-- | Configurable event handler for the upper direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as open/close.
handleUDirection :: Key -> (Dir -> a) -> a -> a
handleUDirection e h k =
  case e of
    Char 'K' -> h up
    Char 'J' -> h down
    Char 'H' -> h left
    Char 'L' -> h right
    Char 'Y' -> h upleft
    Char 'U' -> h upright
    Char 'B' -> h downleft
    Char 'N' -> h downright
    _          -> k

-- | Translate key from a GTK string description to our internal key type.
-- To be used, in particular, for the macros in the config file.
keyTranslate :: String -> Maybe Key
keyTranslate "less"          = Just (Char '<')
keyTranslate "greater"       = Just (Char '>')
keyTranslate "period"        = Just (Char '.')
keyTranslate "colon"         = Just (Char ':')
keyTranslate "comma"         = Just (Char ',')
keyTranslate "space"         = Just (Char ' ')
keyTranslate "question"      = Just (Char '?')
keyTranslate "dollar"        = Just (Char '$')
keyTranslate "asterisk"      = Just (Char '*')
keyTranslate "KP_Multiply"   = Just (Char '*')
keyTranslate "slash"         = Just (Char '/')
keyTranslate "KP_Divide"     = Just (Char '/')
keyTranslate "underscore"    = Just (Char '_')
keyTranslate "Escape"        = Just Esc
keyTranslate "Return"        = Just Return
keyTranslate "Tab"           = Just Tab
keyTranslate "KP_Up"         = Just Up
keyTranslate "KP_Down"       = Just Down
keyTranslate "KP_Left"       = Just Left
keyTranslate "KP_Right"      = Just Right
keyTranslate "KP_Home"       = Just Home
keyTranslate "KP_End"        = Just End
keyTranslate "KP_Page_Up"    = Just PgUp
keyTranslate "KP_Page_Down"  = Just PgDn
keyTranslate "KP_Begin"      = Just Begin
keyTranslate "KP_Enter"      = Just Return
keyTranslate ['K','P','_',c] = Just (KP c)
keyTranslate [c]             = Just (Char c)
keyTranslate _               = Nothing
-- keyTranslate e               = Just (Dbg $ show e)

-- | Maps a key to the canonical key for the command it denotes.
-- Takes into account the keypad and any macros from a config file.
-- Macros cannot depend on each other, but they can on canonMoveKey.
macroKey :: [(String, String)] -> Key -> Key
macroKey section =
  let trans k = fromMaybe (error $ "unknown macro key " ++ k) (keyTranslate k)
      trMacro (from, to) = (trans from, canonMoveKey $ trans to)
      macros  = M.fromList $ L.map trMacro section
  in  \ e -> case M.lookup e macros of
               Just key -> key
               Nothing  -> canonMoveKey e
