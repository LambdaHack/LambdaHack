-- | Text frontend based on Vty.
module Game.LambdaHack.Client.UI.Frontend.Vty
  ( startup, frontendName
  ) where

import Control.Concurrent.Async
import Control.Monad
import Data.Default
import Graphics.Vty
import qualified Graphics.Vty as Vty

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Msg

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { svty :: !Vty  -- ^ internal vty session
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "vty"

-- | Starts the main program loop using the frontend input and output.
startup :: DebugModeCli -> IO RawFrontend
startup _sdebugCli = do
  svty <- mkVty def
  let sess = FrontendSession{..}
  rf <- createRawFrontend (display sess) (Vty.shutdown svty)
  let storeKeys :: IO ()
      storeKeys = do
        km <- nextEvent0 sess
        saveKM rf km
        storeKeys
  void $ async storeKeys
  return $! rf

-- | Input key via the frontend.
nextEvent0 :: FrontendSession -> IO K.KM
nextEvent0 sess@FrontendSession{svty} = do
  e <- nextEvent svty  -- blocks here, so no polling
  case e of
    EvKey n mods -> do
      let !key = keyTranslate n
          !modifier = modTranslate mods
          !pointer = Nothing
      return $! K.KM{..}
    _ -> nextEvent0 sess

-- | Output to the screen via the frontend.
display :: FrontendSession    -- ^ frontend session data
        -> SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display FrontendSession{svty} rawSF =
  let SingleFrame{sfLevel} = overlayOverlay rawSF
      img = (foldr (<->) emptyImage
             . map (foldr (<|>) emptyImage
                      . map (\Color.AttrChar{..} ->
                               char (setAttr acAttr) acChar)))
            $ map decodeLine sfLevel
      pic = picForImage img
  in update svty pic

-- TODO: Ctrl-m is RET
keyTranslate :: Key -> K.Key
keyTranslate n =
  case n of
    KEsc          -> K.Esc
    KEnter        -> K.Return
    (KChar ' ')   -> K.Space
    (KChar '\t')  -> K.Tab
    KBackTab      -> K.BackTab
    KBS           -> K.BackSpace
    KUp           -> K.Up
    KDown         -> K.Down
    KLeft         -> K.Left
    KRight        -> K.Right
    KHome         -> K.Home
    KEnd          -> K.End
    KPageUp       -> K.PgUp
    KPageDown     -> K.PgDn
    KBegin        -> K.Begin
    KCenter       -> K.Begin
    KIns          -> K.Insert
    -- Ctrl-Home and Ctrl-End are the same in vty as Home and End
    -- on some terminals so we have to use 1--9 for movement instead of
    -- leader change.
    (KChar c)
      | c `elem` ['1'..'9'] -> K.KP c  -- movement, not leader change
      | otherwise           -> K.Char c
    _             -> K.Unknown (tshow n)

-- | Translates modifiers to our own encoding.
modTranslate :: [Modifier] -> K.Modifier
modTranslate mods =
  modifierTranslate
    (MCtrl `elem` mods) (MShift `elem` mods) (MAlt `elem` mods) False

-- A hack to get bright colors via the bold attribute. Depending on terminal
-- settings this is needed or not and the characters really get bold or not.
-- HSCurses does this by default, but in Vty you have to request the hack.
hack :: Color.Color -> Attr -> Attr
hack c a = if Color.isBright c then withStyle a bold else a

setAttr :: Color.Attr -> Attr
setAttr Color.Attr{fg, bg} =
-- This optimization breaks display for white background terminals:
--  if (fg, bg) == Color.defAttr
--  then def_attr
--  else
  hack fg $ hack bg $
    defAttr { attrForeColor = SetTo (aToc fg)
            , attrBackColor = SetTo (aToc bg) }

aToc :: Color.Color -> Color
aToc Color.Black     = black
aToc Color.Red       = red
aToc Color.Green     = green
aToc Color.Brown     = yellow
aToc Color.Blue      = blue
aToc Color.Magenta   = magenta
aToc Color.Cyan      = cyan
aToc Color.White     = white
aToc Color.BrBlack   = brightBlack
aToc Color.BrRed     = brightRed
aToc Color.BrGreen   = brightGreen
aToc Color.BrYellow  = brightYellow
aToc Color.BrBlue    = brightBlue
aToc Color.BrMagenta = brightMagenta
aToc Color.BrCyan    = brightCyan
aToc Color.BrWhite   = brightWhite
