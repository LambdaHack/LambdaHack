-- | Text frontend based on Vty.
module Game.LambdaHack.Display.Vty
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , display, nextEvent
    -- * Frontend administration tools
  , frontendName, startup, shutdown
  ) where

import Graphics.Vty hiding (shutdown)
import qualified Graphics.Vty as Vty
import qualified Data.List as L
import qualified Data.ByteString.Char8 as BS

import Game.LambdaHack.Area
import Game.LambdaHack.PointXY
import qualified Game.LambdaHack.Key as K (Key(..))
import qualified Game.LambdaHack.Color as Color

-- | Session data maintained by the frontend.
type FrontendSession = Vty

-- | The name of the frontend.
frontendName :: String
frontendName = "vty"

-- | Starts the main program loop using the frontend input and output.
startup :: (FrontendSession -> IO ()) -> IO ()
startup k = mkVty >>= k

-- | Shuts down the frontend cleanly.
shutdown :: FrontendSession -> IO ()
shutdown = Vty.shutdown

-- | Output to the screen via the frontend.
display :: Area             -- ^ the size of the drawn area
        -> FrontendSession  -- ^ current session data
        -> (PointXY -> (Color.Attr, Char))
                            -- ^ the content of the screen
        -> String           -- ^ an extra line to show at the top
        -> String           -- ^ an extra line to show at bottom
        -> IO ()
display (x0, y0, x1, y1) vty f msg status =
  let img = (foldr (<->) empty_image .
             L.map (foldr (<|>) empty_image .
                    L.map (\ (x, y) -> let (a, c) = f (PointXY (x, y))
                                       in char (setAttr a) c)))
            [ [ (x, y) | x <- [x0..x1] ] | y <- [y0..y1] ]
      pic = pic_for_image $
              utf8_bytestring (setAttr Color.defaultAttr) (BS.pack msg)
              <-> img <->
              utf8_bytestring (setAttr Color.defaultAttr) (BS.pack status)
  in update vty pic

-- | Input key via the frontend.
nextEvent :: FrontendSession -> IO K.Key
nextEvent sess = do
  e <- next_event sess
  return (keyTranslate e)

keyTranslate :: Event -> K.Key
keyTranslate e =
  case e of
    EvKey KEsc []          -> K.Esc
    EvKey KEnter []        -> K.Return
    EvKey (KASCII '\t') [] -> K.Tab
    EvKey KUp []           -> K.Up
    EvKey KDown []         -> K.Down
    EvKey KLeft []         -> K.Left
    EvKey KRight []        -> K.Right
    EvKey KHome []         -> K.Home
    EvKey KPageUp []       -> K.PgUp
    EvKey KEnd []          -> K.End
    EvKey KPageDown []     -> K.PgDn
    EvKey KBegin []        -> K.Begin
    -- No KP_ keys in vty; see https://github.com/coreyoconnor/vty/issues/8
    -- For now, movement keys are more important than hero selection:
    EvKey (KASCII c) []
      | c `elem` ['1'..'9']  -> K.KP c
      | otherwise            -> K.Char c
    _                        -> K.Unknown (show e)

-- A hack to get bright colors via the bold attribute. Depending on terminal
-- settings this is needed or not and the characters really get bold or not.
-- HSCurses does this by default, but in Vty you have to request the hack.
hack :: Color.Color -> Attr -> Attr
hack c a = if Color.isBright c then with_style a bold else a

setAttr :: Color.Attr -> Attr
setAttr Color.Attr{fg, bg} =
-- This optimization breaks display for white background terminals:
--  if (fg, bg) == Color.defaultAttr
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
