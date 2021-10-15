-- | Text frontend based on Vty.
module Game.LambdaHack.Client.UI.Frontend.Vty
  (
#ifdef USE_VTY
-- to molify doctest, but don't break stylish-haskell parsing
   startup, frontendName
#endif
  ) where

#ifdef USE_VTY
import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent.Async
import           Data.Char (chr, ord)
import qualified System.Console.ANSI as ANSI
import qualified System.IO as SIO

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Content.TileKind (floorSymbol)
import qualified Game.LambdaHack.Definition.Color as Color

-- No session data maintained by this frontend

-- | The name of the frontend.
frontendName :: String
frontendName = "vty"

-- | Starts the main program loop using the frontend input and output.
startup :: ScreenContent -> ClientOptions -> IO RawFrontend
startup coscreen _soptions = do
  rf <- createRawFrontend coscreen (display coscreen) shutdown
  let storeKeys :: IO ()
      storeKeys = do
        c <- SIO.getChar  -- blocks here, so no polling
        let K.KM{..} = keyTranslate c
        saveKMP rf modifier key (PointUI 0 0)
        storeKeys
  SIO.hSetBuffering SIO.stdin SIO.NoBuffering
  SIO.hSetBuffering SIO.stderr $ SIO.BlockBuffering $
    Just $ 2 * rwidth coscreen * rheight coscreen
  void $ async storeKeys
  return $! rf

shutdown :: IO ()
shutdown = SIO.hFlush SIO.stdout >> SIO.hFlush SIO.stderr

-- | Output to the screen via the frontend.
display :: ScreenContent
        -> SingleFrame
        -> IO ()
display _coscreen SingleFrame{singleArray} = do
  ANSI.hHideCursor SIO.stderr
  let f Point{..} w = do
        ANSI.hSetCursorPosition SIO.stderr py px
        let acChar = squashChar $ Color.charFromW32 w
            (fg, bg) = setAttr $ Color.attrFromW32 w
        ANSI.hSetSGR SIO.stderr [uncurry (ANSI.SetColor ANSI.Foreground)
                                 $ colorTranslate fg]
        ANSI.hSetSGR SIO.stderr [uncurry (ANSI.SetColor ANSI.Background)
                                 $ colorTranslate bg]
{-
This is dubious, because I can't foce bright background colour with that,
only bright foregrouns. And I have at least one bright backround: bright black.
        -- A hack to get bright colors via the bold attribute.
        -- Depending on terminal settings this is needed or not
        -- and the characters really get bold or not.
        -- HSCurses does this by default, in Vty you have to request the hack,
        -- with ANSI we probably need it as well.
        ANSI.hSetSGR SIO.stderr [ANSI.SetConsoleIntensity
                                 $ if Color.isBright fg
                                   then ANSI.BoldIntensity
                                   else ANSI.NormalIntensity]
-}
        SIO.hPutStr SIO.stderr [acChar]
  PointArray.imapMA_ f singleArray
  let Point{..} = PointArray.maxIndexByA (comparing Color.bgFromW32) singleArray
  ANSI.hSetCursorPosition SIO.stderr py px
  ANSI.hShowCursor SIO.stderr
  SIO.hFlush SIO.stderr

keyTranslate :: Char -> K.KM
keyTranslate e = (\(key, modifier) -> K.KM modifier key) $
  case e of
    '\ESC' -> (K.Esc,     K.NoModifier)
    '\n'   -> (K.Return,  K.NoModifier)
    '\r'   -> (K.Return,  K.NoModifier)
    ' '    -> (K.Space,   K.NoModifier)
    '\t'   -> (K.Tab,     K.NoModifier)
    c | ord '\^A' <= ord c && ord c <= ord '\^Z' ->
        -- Alas, only lower-case letters.
        (K.Char $ chr $ ord c - ord '\^A' + ord 'a', K.Control)
        -- Movement keys are more important than leader picking,
        -- so disabling the latter and interpreting the keypad numbers
        -- as movement:
      | c `elem` ['1'..'9'] -> (K.KP c,              K.NoModifier)
      | otherwise           -> (K.Char c,            K.NoModifier)

setAttr :: Color.Attr -> (Color.Color, Color.Color)
setAttr Color.Attr{..} =
  let (fg1, bg1) = case bg of
        Color.HighlightNone -> (fg, Color.Black)
        Color.HighlightGreen ->
          if fg /= Color.Green
          then (fg, Color.Green)
          else (fg, Color.BrBlack)
        Color.HighlightBlue ->
          if fg /= Color.Blue
          then (fg, Color.Blue)
          else (fg, Color.BrBlack)
        Color.HighlightBrown ->
          if fg /= Color.Blue
          then (fg, Color.Brown)
          else (fg, Color.BrBlack)
        Color.HighlightCyan ->
          if fg /= Color.Blue
          then (fg, Color.Cyan)
          else (fg, Color.BrBlack)
        Color.HighlightGrey ->
          if fg /= Color.BrBlack
          then (fg, Color.BrBlack)
          else (fg, Color.defFG)
        Color.HighlightWhite -> (fg, Color.Magenta)
        Color.HighlightMagenta -> (fg, Color.Magenta)
        Color.HighlightRed ->
          if fg /= Color.Red
          then (fg, Color.Red)
          else (fg, Color.defFG)
        Color.HighlightYellow -> (fg, Color.Black)  -- cursor used instead
        Color.HighlightYellowAim -> (Color.Black, Color.defFG)
        Color.HighlightRedAim ->
          if fg /= Color.Red
          then (fg, Color.Red)
          else (fg, Color.defFG)
        Color.HighlightNoneCursor -> (fg, Color.Black)
        Color.HighlightBackground -> (fg, Color.BrBlack)
  in (fg1, bg1)

colorTranslate :: Color.Color -> (ANSI.ColorIntensity, ANSI.Color)
colorTranslate Color.Black     = (ANSI.Dull, ANSI.Black)
colorTranslate Color.Red       = (ANSI.Dull, ANSI.Red)
colorTranslate Color.Green     = (ANSI.Dull, ANSI.Green)
colorTranslate Color.Brown     = (ANSI.Dull, ANSI.Yellow)
colorTranslate Color.Blue      = (ANSI.Dull, ANSI.Blue)
colorTranslate Color.Magenta   = (ANSI.Dull, ANSI.Magenta)
colorTranslate Color.Cyan      = (ANSI.Dull, ANSI.Cyan)
colorTranslate Color.White     = (ANSI.Dull, ANSI.White)
colorTranslate Color.AltWhite  = (ANSI.Dull, ANSI.White)
colorTranslate Color.BrBlack   = (ANSI.Vivid, ANSI.Black)
colorTranslate Color.BrRed     = (ANSI.Vivid, ANSI.Red)
colorTranslate Color.BrGreen   = (ANSI.Vivid, ANSI.Green)
colorTranslate Color.BrYellow  = (ANSI.Vivid, ANSI.Yellow)
colorTranslate Color.BrBlue    = (ANSI.Vivid, ANSI.Blue)
colorTranslate Color.BrMagenta = (ANSI.Vivid, ANSI.Magenta)
colorTranslate Color.BrCyan    = (ANSI.Vivid, ANSI.Cyan)
colorTranslate Color.BrWhite   = (ANSI.Vivid, ANSI.White)

squashChar :: Char -> Char
squashChar c = if c == floorSymbol then '.' else c
#endif
