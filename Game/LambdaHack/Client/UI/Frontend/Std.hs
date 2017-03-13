-- | Text frontend based on stdin/stdout, intended for bots.
module Game.LambdaHack.Client.UI.Frontend.Std
  ( startup, frontendName
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , shutdown, display
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent.Async
import Data.Char (chr, ord)
import qualified System.IO as SIO

import qualified Game.LambdaHack.Client.UI.Key as K
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

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
        l <- SIO.hGetLine SIO.stdin  -- blocks here, so no polling
        let c = case l of
              [] -> '\n'  -- empty line counts as RET
              hd : _ -> hd
            K.KM{..} = keyTranslate c
        saveKMP rf modifier key originPoint
        storeKeys
  void $ async storeKeys
  return $! rf

shutdown :: IO ()
shutdown = SIO.hFlush SIO.stdout >> SIO.hFlush SIO.stderr

-- | Output to the screen via the frontend.
display :: SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display SingleFrame{singleFrame} =
  let f w l = Color.charFromW32 w : l
      levelChar = chunk $ PointArray.foldrA f [] singleFrame
      lxsize = fst normalLevelBound + 1
      chunk [] = []
      chunk l = let (ch, r) = splitAt lxsize l
                in ch : chunk r
  in SIO.hPutStrLn SIO.stderr $ unlines levelChar

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
