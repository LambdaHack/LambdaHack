module Game.LambdaHack.Display.Std
  ( frontendName, startup, shutdown, display, nextEvent, FrontendSession
  ) where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BS
import qualified System.IO as SIO

import Game.LambdaHack.Area
import Game.LambdaHack.Loc
import qualified Game.LambdaHack.Keys as K (Key(..))
import qualified Game.LambdaHack.Color as Color

frontendName :: String
frontendName = "std"

type FrontendSession = ()

startup :: (FrontendSession -> IO ()) -> IO ()
startup k = k ()

shutdown :: FrontendSession -> IO ()
shutdown _session = return ()

display :: Area -> Int -> FrontendSession
        -> (Loc -> (Color.Attr, Char)) -> String -> String
        -> IO ()
display (x0, y0, x1, y1) _width _session f msg status =
  let xsize  = x1 - x0 + 1
      g y x  = if x > x1
               then Nothing
               else Just (snd (f (toLoc (x1 + 1) (x, y))), x + 1)
      fl y   = fst $ BS.unfoldrN xsize (g y) x0
      level  = L.map fl [y0..y1]
      screen = [BS.pack msg] ++ level ++ [BS.pack status, BS.empty]
  in mapM_ BS.putStrLn screen

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

nextEvent :: FrontendSession -> IO K.Key
nextEvent _session = do
  e <- BS.hGet SIO.stdin 1
  let c = BS.head e
  if c == '\n'  -- let \n mark the end of input, for human players
    then nextEvent _session
    else return $ keyTranslate c
