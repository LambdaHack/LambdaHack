-- | Text frontend based on stdin/stdout, intended for bots.
module Game.LambdaHack.Client.UI.Frontend.Std
  ( startup, frontendName
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, ord)
import qualified System.IO as SIO

import qualified Control.Concurrent.STM as STM
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color

-- No session data maintained by this frontend

-- | The name of the frontend.
frontendName :: String
frontendName = "std"

-- | Set up the frontend input and output.
startup :: DebugModeCli -> IO RawFrontend
startup _sdebugCli = do
  rf <- createRawFrontend display shutdown
  let storeKeys :: IO ()
      storeKeys = do
        c <- nextEvent  -- blocks here, so no polling
        let !km = keyTranslate c
        -- Store the key in the channel.
        STM.atomically $ STM.writeTQueue (fchanKey rf) km
        -- Instantly show any frame waiting for display.
        void $ tryPutMVar (fshowNow rf) ()
        storeKeys
  void $ async storeKeys
  return $! rf

shutdown :: IO ()
shutdown = SIO.hFlush SIO.stdout >> SIO.hFlush SIO.stderr

-- | Output to the screen via the frontend.
display :: SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display rawSF =
  let SingleFrame{sfLevel} = overlayOverlay rawSF
      bs = map (BS.pack . map Color.acChar . decodeLine) sfLevel ++ [BS.empty]
  in mapM_ BS.putStrLn bs

-- | Input key via the frontend.
nextEvent :: IO Char
nextEvent = do
  l <- BS.hGetLine SIO.stdin
  return $! case BS.uncons l of
    Nothing -> '\n'  -- empty line counts as RET
    Just (hd, _) -> hd

keyTranslate :: Char -> K.KM
keyTranslate e = (\(key, modifier) -> K.toKM modifier key) $
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
