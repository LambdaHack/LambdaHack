-- | Text frontend based on Vty.
module Game.LambdaHack.Frontend.Vty
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , fdisplay, fpromptGetKey
    -- * Frontend administration tools
  , frontendName, startup
  ) where

import qualified Data.List as L
import Data.Text.Encoding (encodeUtf8)
import Graphics.Vty
import qualified Graphics.Vty as Vty

import Game.LambdaHack.Common.Animation (DebugModeCli (..), SingleFrame (..),
                                         overlayOverlay)
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Common.Msg

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { svty      :: !Vty  -- internal vty session
  , sdebugCli :: !DebugModeCli  -- ^ client configuration
      -- ^ Configuration of the frontend session.
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "vty"

-- | Starts the main program loop using the frontend input and output.
startup :: DebugModeCli -> (FrontendSession -> IO ()) -> IO ()
startup sdebugCli k = do
  svty <- mkVty
  k FrontendSession{..}
  Vty.shutdown svty

-- | Output to the screen via the frontend.
fdisplay :: FrontendSession    -- ^ frontend session data
         -> Bool
         -> Maybe SingleFrame  -- ^ the screen frame to draw
         -> IO ()
fdisplay _ _ Nothing = return ()
fdisplay FrontendSession{svty} _ (Just rawSF) =
  let SingleFrame{sfLevel, sfBottom} = overlayOverlay rawSF
      img = (foldr (<->) empty_image
             . L.map (foldr (<|>) empty_image
                      . L.map (\ Color.AttrChar{..} ->
                                char (setAttr acAttr) acChar)))
            sfLevel
      pic = pic_for_image $
              img <->
              utf8_bytestring (setAttr Color.defAttr) (encodeUtf8 sfBottom)
  in update svty pic

-- | Input key via the frontend.
nextEvent :: FrontendSession -> IO K.KM
nextEvent sess@FrontendSession{svty, sdebugCli=DebugModeCli{snoMore}} =
  if snoMore then return K.escKey
  else do
    e <- next_event svty
    case e of
      EvKey n mods -> do
        let key = keyTranslate n
            modifier = modifierTranslate mods
        return K.KM {key, modifier}
      _ -> nextEvent sess

-- | Display a prompt, wait for any key.
fpromptGetKey :: FrontendSession -> SingleFrame -> IO K.KM
fpromptGetKey sess frame = do
  fdisplay sess True $ Just frame
  nextEvent sess

keyTranslate :: Key -> K.Key
keyTranslate n =
  case n of
    KEsc          -> K.Esc
    KEnter        -> K.Return
    (KASCII ' ')  -> K.Space
    (KASCII '\t') -> K.Tab
    KBackTab      -> K.BackTab
    KUp           -> K.Up
    KDown         -> K.Down
    KLeft         -> K.Left
    KRight        -> K.Right
    KHome         -> K.Home
    KPageUp       -> K.PgUp
    KEnd          -> K.End
    KPageDown     -> K.PgDn
    KBegin        -> K.Begin
    (KASCII c)    -> K.Char c
    _             -> K.Unknown (showT n)

-- | Translates modifiers to our own encoding.
modifierTranslate :: [Modifier] -> K.Modifier
modifierTranslate mods =
  if MCtrl `elem` mods then K.Control else K.NoModifier

-- A hack to get bright colors via the bold attribute. Depending on terminal
-- settings this is needed or not and the characters really get bold or not.
-- HSCurses does this by default, but in Vty you have to request the hack.
hack :: Color.Color -> Attr -> Attr
hack c a = if Color.isBright c then with_style a bold else a

setAttr :: Color.Attr -> Attr
setAttr Color.Attr{fg, bg} =
-- This optimization breaks display for white background terminals:
--  if (fg, bg) == Color.defAttr
--  then def_attr
--  else
  hack fg $ hack bg $
    def_attr { attr_fore_color = SetTo (aToc fg)
             , attr_back_color = SetTo (aToc bg) }

aToc :: Color.Color -> Color
aToc Color.Black     = black
aToc Color.Red       = red
aToc Color.Green     = green
aToc Color.Brown     = yellow
aToc Color.Blue      = blue
aToc Color.Magenta   = magenta
aToc Color.Cyan      = cyan
aToc Color.White     = white
aToc Color.BrBlack   = bright_black
aToc Color.BrRed     = bright_red
aToc Color.BrGreen   = bright_green
aToc Color.BrYellow  = bright_yellow
aToc Color.BrBlue    = bright_blue
aToc Color.BrMagenta = bright_magenta
aToc Color.BrCyan    = bright_cyan
aToc Color.BrWhite   = bright_white
