-- | Text frontend based on stdin/stdout, intended for bots.
module Game.LambdaHack.Display.Std
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , display, nextEvent
    -- * Frontend administration tools
  , frontendName, startup, shutdown
  ) where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BS
import qualified System.IO as SIO

import Game.LambdaHack.Area
import Game.LambdaHack.PointXY
import qualified Game.LambdaHack.Key as K (Key(..))
import qualified Game.LambdaHack.Color as Color

-- | No session data needs to be maintained by this frontend.
type FrontendSession = ()

-- | The name of the frontend.
frontendName :: String
frontendName = "std"

-- | Starts the main program loop using the frontend input and output.
startup :: (FrontendSession -> IO ()) -> IO ()
startup k = k ()

-- | Shuts down the frontend cleanly. Nothing to be done in this case.
shutdown :: FrontendSession -> IO ()
shutdown _ = return ()

-- | Output to the screen via the frontend.
display :: Area             -- ^ the size of the drawn area
        -> FrontendSession  -- ^ current session data
        -> (PointXY -> (Color.Attr, Char))
                            -- ^ the content of the screen
        -> String           -- ^ an extra line to show at the top
        -> String           -- ^ an extra line to show at the bottom
        -> IO ()
display (x0, y0, x1, y1) _sess f msg status =
  let xsize  = x1 - x0 + 1
      g y x  = if x > x1
               then Nothing
               else Just (snd (f (PointXY (x, y))), x + 1)
      fl y   = fst $ BS.unfoldrN xsize (g y) x0
      level  = L.map fl [y0..y1]
      screen = [BS.pack msg] ++ level ++ [BS.pack status, BS.empty]
  in mapM_ BS.putStrLn screen

-- | Input key via the frontend.
nextEvent :: FrontendSession -> IO K.Key
nextEvent sess = do
  e <- BS.hGet SIO.stdin 1
  let c = BS.head e
  if c == '\n'  -- let \n mark the end of input, for human players
    then nextEvent sess
    else return $ keyTranslate c

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
    'a'    -> K.Char 'g'  -- and don't apply
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
