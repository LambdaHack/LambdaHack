-- | Text frontend based on stdin/stdout, intended for bots.
module Game.LambdaHack.Action.Frontend.Std
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , display, nextEvent, promptGetAnyKey
    -- * Frontend administration tools
  , frontendName, startup
  ) where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BS
import qualified System.IO as SIO
import Data.Text.Encoding (encodeUtf8)

import qualified Game.LambdaHack.Key as K (Key(..),  Modifier(..))
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Animation (SingleFrame(..))

-- | No session data needs to be maintained by this frontend.
type FrontendSession = ()

-- | The name of the frontend.
frontendName :: String
frontendName = "std"

-- | Starts the main program loop using the frontend input and output.
startup :: String -> (FrontendSession -> IO ()) -> IO ()
startup _ k = k ()

-- | Output to the screen via the frontend.
display :: FrontendSession          -- ^ frontend session data
        -> Bool
        -> Bool
        -> Maybe SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display _ _ _ Nothing = return ()
display _ _ _ (Just SingleFrame{..}) =
  let chars = L.map (BS.pack . L.map Color.acChar) sfLevel
      bs = [encodeUtf8 sfTop, BS.empty] ++ chars ++ [encodeUtf8 sfBottom, BS.empty]
  in mapM_ BS.putStrLn bs

-- | Input key via the frontend.
nextEvent :: FrontendSession -> Maybe Bool -> IO (K.Key, K.Modifier)
nextEvent sess mb = do
  e <- BS.hGet SIO.stdin 1
  let c = BS.head e
  if c == '\n'  -- let \n mark the end of input, for human players
    then nextEvent sess mb
    else return (keyTranslate c, K.NoModifier)

-- | Display a prompt, wait for any key.
promptGetAnyKey :: FrontendSession -> SingleFrame
             -> IO (K.Key, K.Modifier)
promptGetAnyKey sess frame = do
  display sess True True $ Just frame
  nextEvent sess Nothing

-- HACK: Special translation that block commands the bots should not use
-- and multiplies some other commands.
keyTranslate :: Char -> K.Key
keyTranslate e =
  case e of
    -- Translate some special keys (use vi keys to move).
    '\ESC' -> K.Esc
    '\n'   -> K.Return
    '\r'   -> K.Return
    '\t'   -> K.Tab
    --  For bots: disable purely UI commands.
    'P'    -> K.Char 'U'
    'V'    -> K.Char 'Y'
    'O'    -> K.Char 'J'
    'I'    -> K.Char 'L'
    'R'    -> K.Char 'K'
    '?'    -> K.Char 'N'
    -- For bots: don't let them give up, write files, procrastinate.
    'Q'    -> K.Char 'H'
    'X'    -> K.Char 'B'
    'D'    -> K.Return
    '.'    -> K.Return
    -- For bots (assuming they go from '0' to 'z'): major commands.
    '<'    -> K.Char 'q'  -- ban ascending to speed up descending
    '>'    -> K.Char '>'
    'c'    -> K.Char 'c'
    'd'    -> K.Char 'r'  -- don't let bots drop stuff
    'g'    -> K.Char 'g'
    'i'    -> K.Char 'i'
    'o'    -> K.Char 'o'
    'q'    -> K.Char 'q'
    'r'    -> K.Char 'r'
    't'    -> K.Char 'g'  -- tagetting is too hard, so don't throw
    'z'    -> K.Char 'g'  -- and don't zap
    'p'    -> K.Char 'g'  -- and don't project
    'a'    -> K.Esc       -- and don't apply
    -- For bots: minor commands. Targeting is too hard, so don't do it.
    '*'    -> K.Char 'c'
    '/'    -> K.Char 'c'
    '['    -> K.Char 'g'
    ']'    -> K.Char 'g'
    '{'    -> K.Char 'g'
    '}'    -> K.Char 'g'
    -- Hack for bots: dump config at the start.
    ' '    -> K.Char 'D'
    -- Movement and hero selection.
    c | c `elem` "kjhlyubnKJHLYUBN" -> K.Char c
    c | c `elem` ['0'..'9'] -> K.Char c
    _      -> K.Char '>'  -- try hard to descend
