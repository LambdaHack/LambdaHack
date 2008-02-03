module State where

import Control.Monad
import Data.Binary

import Monster

type Time = Int

data State = State
               { splayer :: Monster,
                 stime   :: Time
               }
  deriving Show

defaultState ploc =
  State
    (Monster Player 10 Nothing ploc)
    0

updatePlayer :: State -> (Monster -> Monster) -> State
updatePlayer s f = s { splayer = f (splayer s) }

instance Binary State where
  put (State player time) =
    do
      put player
      put time
  get = liftM2 State get get


