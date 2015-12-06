-- | Text frontend based on stdin/stdout, intended for bots.
module Game.LambdaHack.Client.UI.Frontend.Std
  ( startup, frontendName
  ) where

import Control.Concurrent
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, ord)
import qualified System.IO as SIO

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color

-- | No session data needs to be maintained by this frontend.
data FrontendSession = FrontendSession
  { skeyPressed :: !(MVar ())
  , sdebugCli   :: !DebugModeCli  -- ^ client configuration
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "std"

-- | Starts the main program loop using the frontend input and output.
startup :: DebugModeCli -> IO RawFrontend
startup sdebugCli = do
  skeyPressed <- newEmptyMVar
  let sess = FrontendSession{..}
      rf = RawFrontend
        { fdisplay = display sess
        , fpromptGetKey = promptGetKey sess
        , fshutdown = shutdown
        , fkeyPressed = skeyPressed
        }
  return $! rf

shutdown :: IO ()
shutdown = SIO.hFlush SIO.stdout >> SIO.hFlush SIO.stderr

-- | Output to the screen via the frontend.
display :: FrontendSession    -- ^ frontend session data
        -> SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display _ rawSF =
  let SingleFrame{sfLevel} = overlayOverlay rawSF
      bs = map (BS.pack . map Color.acChar . decodeLine) sfLevel ++ [BS.empty]
  in mapM_ BS.putStrLn bs

-- | Input key via the frontend.
nextEvent :: FrontendSession -> IO K.KM
nextEvent FrontendSession{..} = do
  l <- BS.hGetLine SIO.stdin
  void $ tryPutMVar skeyPressed ()
  let c = case BS.uncons l of
        Nothing -> '\n'  -- empty line counts as RET
        Just (hd, _) -> hd
  return $! keyTranslate c

-- | Display a prompt, wait for any key.
promptGetKey :: FrontendSession -> SingleFrame -> IO K.KM
promptGetKey sess frame = do
  display sess frame
  nextEvent sess

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
