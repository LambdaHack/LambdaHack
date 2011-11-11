module Game.LambdaHack.Display.Std
  ( displayId, startup, shutdown, display, nextEvent, Session
  ) where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BS
import qualified System.IO as SIO

import Game.LambdaHack.Area
import Game.LambdaHack.Loc
import qualified Game.LambdaHack.Keys as K (Key(..))
import qualified Game.LambdaHack.Color as Color

displayId :: String
displayId = "std"

type Session = ()

startup :: (Session -> IO ()) -> IO ()
startup k = k ()

shutdown :: Session -> IO ()
shutdown _session = return ()

display :: Area -> Session -> (Loc -> (Color.Attr, Char)) -> String -> String
           -> IO ()
display (x0, y0, x1, y1) _session f msg status =
  let size   = x1 - x0 + 1
      g y x  = if x > x1
               then Nothing
               else Just (snd (f (toLoc (x1 + 1) (x, y))), x + 1)
      fl y   = fst $ BS.unfoldrN size (g y) x0
      level  = L.map fl [y0..y1]
      screen = [BS.pack msg] ++ level ++ [BS.pack status, BS.empty]
  in mapM_ BS.putStrLn screen

keyTranslate :: Char -> K.Key
keyTranslate e =
  case e of
    -- For human players: translate some special keys (use vi keys to move).
    '\ESC' -> K.Esc
    '\n'   -> K.Return
    '\r'   -> K.Return
    '\t'   -> K.Tab
    -- For bots: (assuming they go from '0' to 'z').
    '<'    -> K.Char 'q'  -- ban ascending to speed up descending
    '>'    -> K.Char 'r'
    '*'    -> K.Char 'g'  -- targetting is too hard
    '/'    -> K.Char 'c'  -- targetting is too hard
    'c'    -> K.Char 'c'
    'g'    -> K.Char 'g'
    'd'    -> K.Char 'g'  -- don't let bots drop stuff
    'i'    -> K.Char 'i'
    'q'    -> K.Char 'q'
    'r'    -> K.Char 'r'
    't'    -> K.Char 'g'  -- tagetting is too hard, so don't throw
    'a'    -> K.Char 'g'  -- and don't aim
    -- For bots: don't let them give up, write files, procrastinate.
    '.'    -> K.Return
    'Q'    -> K.Return
    'X'    -> K.Return
    --  For bots: disable purely UI commands.
    'R'    -> K.Char 'K'
    'O'    -> K.Char 'J'
    'T'    -> K.Char 'H'
    'I'    -> K.Char 'L'
    'V'    -> K.Char 'Y'
    'P'    -> K.Char 'U'
    'D'    -> K.Char 'B'
    '?'    -> K.Char 'N'
    -- Hack for bots: dump config at the start.
    ' '    -> K.Char 'D'
    -- Movement and hero selection.
    c | c `elem` "kjhlyubnKJHLYUBN" -> K.Char c
    c | c `elem` ['0'..'9'] -> K.Char c
    _      -> K.Char '>'  -- try hard to descend

nextEvent :: Session -> IO K.Key
nextEvent _session = do
  e <- BS.hGet SIO.stdin 1
  let c = BS.head e
  if c == '\n'  -- let \n mark the end of input, for human players
    then nextEvent _session
    else return $ keyTranslate c
