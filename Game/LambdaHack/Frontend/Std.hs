-- | Text frontend based on stdin/stdout, intended for bots.
module Game.LambdaHack.Frontend.Std
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , display, promptGetAnyKey
    -- * Frontend administration tools
  , frontendName, startup, sdebugCli
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, ord)
import qualified Data.List as L
import Data.Text.Encoding (encodeUtf8)
import qualified System.IO as SIO

import Game.LambdaHack.Common.Animation (DebugModeCli (..), SingleFrame (..))
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Key as K

-- | No session data needs to be maintained by this frontend.
data FrontendSession = FrontendSession
  { sdebugCli :: !DebugModeCli  -- ^ client configuration
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "std"

-- | Starts the main program loop using the frontend input and output.
startup :: DebugModeCli -> (FrontendSession -> IO ()) -> IO ()
startup sdebugCli k = k FrontendSession{..}

-- | Output to the screen via the frontend.
display :: FrontendSession    -- ^ frontend session data
        -> Bool
        -> Maybe SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display _ _ Nothing = return ()
display _ _ (Just SingleFrame{..}) =
  let chars = L.map (BS.pack . L.map Color.acChar) sfLevel
      bs = [encodeUtf8 sfTop, BS.empty] ++ chars ++ [encodeUtf8 sfBottom, BS.empty]
  in mapM_ BS.putStrLn bs

-- | Input key via the frontend.
nextEvent :: FrontendSession -> IO K.KM
nextEvent FrontendSession{sdebugCli=DebugModeCli{snoMore}} =
  if snoMore then return K.escKey
  else do
    l <- BS.hGetLine SIO.stdin
    let c = case BS.uncons l of
          Nothing -> '\n'  -- empty line counts as RET
          Just (hd, _) -> hd
    return $! keyTranslate c

-- | Display a prompt, wait for any key.
promptGetAnyKey :: FrontendSession -> SingleFrame
                -> IO K.KM
promptGetAnyKey sess frame = do
  display sess True $ Just frame
  nextEvent sess

keyTranslate :: Char -> K.KM
keyTranslate e = (\(key, modifier) -> K.KM {..}) $
  case e of
    '\ESC' -> (K.Esc,     K.NoModifier)
    '\n'   -> (K.Return,  K.NoModifier)
    '\r'   -> (K.Return,  K.NoModifier)
    ' '    -> (K.Space,   K.NoModifier)
    '\t'   -> (K.Tab,     K.NoModifier)
    c | ord '\^A' <= ord c && ord c <= ord '\^Z' ->
        -- Alas, only lower-case letters.
        (K.Char $ chr $ ord c - ord '\^A' + ord 'a', K.Control)
        -- Movement keys are more important than hero selection,
        -- so disabling the latter and interpreting the keypad numbers
        -- as movement:
      | c `elem` ['1'..'9'] -> (K.KP c,              K.NoModifier)
      | otherwise           -> (K.Char c,            K.NoModifier)
