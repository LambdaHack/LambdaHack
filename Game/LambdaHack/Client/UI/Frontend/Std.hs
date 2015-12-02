-- | Text frontend based on stdin/stdout, intended for bots.
module Game.LambdaHack.Client.UI.Frontend.Std
  ( startup, frontendName
  ) where

import Control.Concurrent
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, ord)
import Data.IORef
import qualified System.IO as SIO

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color

-- | No session data needs to be maintained by this frontend.
data FrontendSession = FrontendSession
  { sescPressed :: !(IORef Bool)
  , sdebugCli   :: !DebugModeCli  -- ^ client configuration
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "std"

-- | Starts the main program loop using the frontend input and output.
startup :: DebugModeCli -> MVar RawFrontend -> IO ()
startup sdebugCli rfMVar = do
  sescPressed <- newIORef False
  fautoYesRef <- newIORef $ not $ sdisableAutoYes sdebugCli
  let sess = FrontendSession{..}
      rf = RawFrontend
        { fdisplay = display sess
        , fpromptGetKey = promptGetKey sess
        , fsyncFrames = syncFrames sess
        , fescPressed = sescPressed
        , fautoYesRef
        }
  putMVar rfMVar rf
  -- TODO: (SIO.hFlush SIO.stdout >> SIO.hFlush SIO.stderr)

-- | Output to the screen via the frontend.
display :: FrontendSession    -- ^ frontend session data
         -> Maybe SingleFrame  -- ^ the screen frame to draw
         -> IO ()
display _ Nothing = return ()
display _ (Just rawSF) =
  let SingleFrame{sfLevel} = overlayOverlay rawSF
      bs = map (BS.pack . map Color.acChar . decodeLine) sfLevel ++ [BS.empty]
  in mapM_ BS.putStrLn bs

-- | Input key via the frontend.
nextEvent :: IO K.KM
nextEvent = do
  l <- BS.hGetLine SIO.stdin
  let c = case BS.uncons l of
        Nothing -> '\n'  -- empty line counts as RET
        Just (hd, _) -> hd
  return $! keyTranslate c

syncFrames :: FrontendSession -> IO ()
syncFrames _ = return ()

-- | Display a prompt, wait for any key.
promptGetKey :: FrontendSession -> SingleFrame -> IO K.KM
promptGetKey sess frame = do
  display sess $ Just frame
  nextEvent

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
