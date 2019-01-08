-- | Text frontend based on Vty.
module Game.LambdaHack.Client.UI.Frontend.Vty
  ( startup, frontendName
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.Concurrent.Async
import           Graphics.Vty
import qualified Graphics.Vty as Vty

import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import qualified Game.LambdaHack.Common.Color as Color
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | Session data maintained by the frontend.
newtype FrontendSession = FrontendSession
  { svty :: Vty  -- ^ internal vty session
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "vty"

-- | Starts the main program loop using the frontend input and output.
startup :: ScreenContent -> ClientOptions -> IO RawFrontend
startup coscreen _soptions = do
  svty <- mkVty mempty
  let sess = FrontendSession{..}
  rf <- createRawFrontend coscreen (display coscreen sess) (Vty.shutdown svty)
  let storeKeys :: IO ()
      storeKeys = do
        e <- nextEvent svty  -- blocks here, so no polling
        case e of
          EvKey n mods ->
            saveKMP rf (modTranslate mods) (keyTranslate n) originPoint
          _ -> return ()
        storeKeys
  void $ async storeKeys
  return $! rf

-- | Output to the screen via the frontend.
display :: ScreenContent
        -> FrontendSession
        -> SingleFrame
        -> IO ()
display coscreen FrontendSession{svty} SingleFrame{singleFrame} =
  let img = foldr (<->) emptyImage
            . map (foldr (<|>) emptyImage
                     . map (\w -> char (setAttr $ Color.attrFromW32 w)
                                       (Color.charFromW32 w)))
            $ chunk $ PointArray.toListA singleFrame
      pic = picForImage img
      chunk [] = []
      chunk l = let (ch, r) = splitAt (rwidth coscreen) l
                in ch : chunk r
  in update svty pic

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
    -- C-Home and C-End are the same in vty as Home and End
    -- on some terminals so we have to use 1--9 for movement instead of
    -- leader change.
    (KChar c)
      | c `elem` ['1'..'9'] -> K.KP c  -- movement, not leader change
      | otherwise           -> K.Char c
    _             -> K.Unknown (show n)

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
setAttr Color.Attr{..} =
-- This optimization breaks display for white background terminals:
--  if (fg, bg) == Color.defAttr
--  then def_attr
--  else
  let (fg1, bg1) = case bg of
        Color.HighlightNone -> (fg, Color.Black)
        Color.HighlightRed -> (Color.Black, Color.defFG)
        Color.HighlightBlue ->
          if fg /= Color.Blue
          then (fg, Color.Blue)
          else (fg, Color.BrBlack)
        Color.HighlightYellow ->
          if fg /= Color.Brown
          then (fg, Color.Brown)
          else (fg, Color.defFG)
        Color.HighlightGrey ->
          if fg /= Color.BrBlack
          then (fg, Color.BrBlack)
          else (fg, Color.defFG)
        _ -> (fg, Color.Black)
  in hack fg1 $ hack bg1 $
       defAttr { attrForeColor = SetTo (aToc fg1)
               , attrBackColor = SetTo (aToc bg1) }

aToc :: Color.Color -> Color
aToc Color.Black     = black
aToc Color.Red       = red
aToc Color.Green     = green
aToc Color.Brown     = yellow
aToc Color.Blue      = blue
aToc Color.Magenta   = magenta
aToc Color.Cyan      = cyan
aToc Color.White     = white
aToc Color.AltWhite  = white
aToc Color.BrBlack   = brightBlack
aToc Color.BrRed     = brightRed
aToc Color.BrGreen   = brightGreen
aToc Color.BrYellow  = brightYellow
aToc Color.BrBlue    = brightBlue
aToc Color.BrMagenta = brightMagenta
aToc Color.BrCyan    = brightCyan
aToc Color.BrWhite   = brightWhite
