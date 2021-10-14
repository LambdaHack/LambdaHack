-- | Line terminal text frontend based on stdin/stdout, intended for logging
-- tests, but may be used on a teletype terminal, or with keyboard and printer.
module Game.LambdaHack.Client.UI.Frontend.Teletype
  ( startup, frontendName
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent.Async
import           Data.Char (chr, ord)
import qualified System.IO as SIO

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Content.TileKind (floorSymbol)
import qualified Game.LambdaHack.Definition.Color as Color

-- No session data maintained by this frontend

-- | The name of the frontend.
frontendName :: String
frontendName = "teletype"

-- | Set up the frontend input and output.
startup :: ScreenContent -> IO RawFrontend
startup coscreen = do
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
display coscreen SingleFrame{singleArray} = do
  let f w l =
        let acCharRaw = Color.charFromW32 w
            acChar = if acCharRaw == floorSymbol then '.' else acCharRaw
        in acChar : l
      levelChar = chunk $ PointArray.foldrA f [] singleArray
      chunk [] = []
      chunk l = let (ch, r) = splitAt (rwidth coscreen) l
                in ch : chunk r
  SIO.hPutStr SIO.stderr $ unlines levelChar
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
