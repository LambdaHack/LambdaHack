-- | Text frontend based on ANSI (via ansi-terminal).
module Game.LambdaHack.Client.UI.Frontend.ANSI
  ( startup, frontendName
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent.Async
import           Data.Char (chr, ord)
import qualified Data.Text as T
import qualified System.Console.ANSI as ANSI
import qualified System.IO as SIO

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Content.TileKind (floorSymbol)
import qualified Game.LambdaHack.Definition.Color as Color

-- No session data maintained by this frontend

-- | The name of the frontend.
frontendName :: String
frontendName = "ANSI"

-- | Starts the main program loop using the frontend input and output.
startup :: ScreenContent -> IO RawFrontend
startup coscreen@ScreenContent{rwidth, rheight} = do
  myx <- ANSI.getTerminalSize
  case myx of
    Just (y, x) | x < rwidth || y < rheight ->
      error $ T.unpack
            $ "The terminal is too small. It should have"
              <+> tshow rwidth
              <+> "columns and"
              <+> tshow rheight
              <+> "rows, but is has"
              <+> tshow x
              <+> "columns and"
              <+> tshow y
              <+> "rows. Resize it and run the program again."
    _ -> do
      rf <- createRawFrontend coscreen (display coscreen) shutdown
      let storeKeys :: IO ()
          storeKeys = do
            c <- SIO.getChar  -- blocks here, so no polling
            s <- do
              if c == '\ESC' then do
                ready <- SIO.hReady SIO.stdin
                if ready then do
                  c2 <- SIO.getChar
                  case c2 of
                    '\ESC' -> return [c]
                    '[' -> keycodeInput [c, c2]
                    '0' -> keycodeInput [c, c2]  -- not handled, but reported
                    _ -> return [c, c2]  -- Alt modifier
                else return [c]
              else return [c]
            let K.KM{..} = keyTranslate s
            saveKMP rf modifier key (PointUI 0 0)
            storeKeys
          keycodeInput :: String -> IO String
          keycodeInput inputSoFar = do
            ready <- SIO.hReady SIO.stdin
            if ready then do
              c <- SIO.getChar
              if ord '@' <= ord c && ord c <= ord '~'  -- terminator
              then return $ inputSoFar ++ [c]
              else keycodeInput  $ inputSoFar ++ [c]
            else return inputSoFar
      SIO.hSetBuffering SIO.stdin SIO.NoBuffering
      SIO.hSetBuffering SIO.stderr $ SIO.BlockBuffering $
        Just $ 2 * rwidth * rheight
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
  -- Do not trash people's terminals when interrupted:
  ANSI.hSetSGR SIO.stderr [uncurry (ANSI.SetColor ANSI.Foreground)
                           $ colorTranslate Color.White]
  ANSI.hSetSGR SIO.stderr [uncurry (ANSI.SetColor ANSI.Background)
                           $ colorTranslate Color.Black]
  SIO.hFlush SIO.stderr

-- This is contrived, because we don't want to depend on libraries
-- that read and interpret terminfo or similar on different architectures.
keyTranslate :: String -> K.KM
keyTranslate e = (\(key, modifier) -> K.KM modifier key) $
  case e of
    "\ESC" -> (K.Esc, K.NoModifier)  -- equals @^[@
    '\ESC' : '[' : rest -> keycodeTranslate rest
    ['\ESC', c] -> (K.Char c, K.Alt)
    "\BS"  -> (K.BackSpace, K.NoModifier)
    "\n"   -> (K.Return, K.NoModifier)
    "\r"   -> (K.Return, K.NoModifier)
    " "    -> (K.Space, K.NoModifier)
    "\t"   -> (K.Tab, K.NoModifier)  -- apparently equals @\^I@ and @\HT@
    [c] | ord '\^A' <= ord c && ord c <= ord '\^Z' ->
          -- Alas, only lower-case letters.
          (K.Char $ chr $ ord c - ord '\^A' + ord 'a', K.Control)
        | -- On (some) terminal emulators Shift-keypad direction produces
          -- the same code as number keys. A sensible workaround for that
          -- is using Control for running, but it's not clear how portable
          -- this is, so we do not rely on this exclusively. Since movement
          -- keys are more important than leader picking, we are disabling
          -- the latter and interpreting the keypad numbers as movement.
          --
          -- BTW, S-KP_5 and C-KP_5 are still not correctly handled
          -- on some terminals, so this may be the biggest portability problem.
          c `elem` ['1'..'9'] -> (K.KP c, K.NoModifier)
        | otherwise           -> (K.Char c, K.NoModifier)
    _ -> (K.Unknown e, K.NoModifier)

-- From https://en.wikipedia.org/wiki/ANSI_escape_code#Terminal_input_sequences
keycodeTranslate :: String -> (K.Key, K.Modifier)
keycodeTranslate e =
  case e of
    "1~" -> (K.Home, K.NoModifier)
    "2~" -> (K.Insert, K.NoModifier)
    "3~" -> (K.Delete, K.NoModifier)
    "4~" -> (K.End , K.NoModifier)
    "5~" -> (K.PgUp, K.NoModifier)
    "6~" -> (K.PgDn, K.NoModifier)
    "7~" -> (K.Home, K.NoModifier)
    "8~" -> (K.End, K.NoModifier)
    "9~" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "10~" -> (K.Fun 0, K.NoModifier)
    "11~" -> (K.Fun 1, K.NoModifier)
    "12~" -> (K.Fun 2, K.NoModifier)
    "13~" -> (K.Fun 3, K.NoModifier)
    "14~" -> (K.Fun 4, K.NoModifier)
    "15~" -> (K.Fun 5, K.NoModifier)
    "17~" -> (K.Fun 6, K.NoModifier)
    "18~" -> (K.Fun 7, K.NoModifier)
    "19~" -> (K.Fun 8, K.NoModifier)
    "20~" -> (K.Fun 9, K.NoModifier)
    "21~" -> (K.Fun 10, K.NoModifier)
    "22~" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "23~" -> (K.Fun 11, K.NoModifier)
    "24~" -> (K.Fun 12, K.NoModifier)
    "25~" -> (K.Fun 13, K.NoModifier)
    "26~" -> (K.Fun 14, K.NoModifier)
    "27~" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "28~" -> (K.Fun 15, K.NoModifier)
    "29~" -> (K.Fun 16, K.NoModifier)
    "30~" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "32~" -> (K.Fun 18 , K.NoModifier)
    "33~" -> (K.Fun 19 , K.NoModifier)
    "34~" -> (K.Fun 20 , K.NoModifier)
    "35~" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)

    "A" -> (K.Up, K.NoModifier)
    "B" -> (K.Down, K.NoModifier)
    "C" -> (K.Right, K.NoModifier)
    "D" -> (K.Left, K.NoModifier)
    "E" -> (K.Begin, K.NoModifier)
    "F" -> (K.End, K.NoModifier)
    "G" -> (K.KP '5', K.NoModifier)
    "H" -> (K.Home, K.NoModifier)
    "I" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "J" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "K" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "L" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "M" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "N" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "O" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "1P" -> (K.Fun 1, K.NoModifier)
    "1Q" -> (K.Fun 2, K.NoModifier)
    "1R" -> (K.Fun 3, K.NoModifier)
    "1S" -> (K.Fun 4, K.NoModifier)
    "T" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "U" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "V" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "W" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "X" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "Y" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)
    "Z" -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)

--    "r" -> (K.Begin, K.NoModifier)
--    "u" -> (K.Begin, K.NoModifier)

    _ -> (K.Unknown $ "\\ESC[" ++ e, K.NoModifier)

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
