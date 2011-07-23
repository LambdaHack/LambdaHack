module MonsterState where

import Data.Map as M

import Level
import Monster
import State

getActor :: State -> Actor -> Monster
getActor (State { slevel = lvl }) a = lmonsters lvl ! a

updateActor :: (Monster -> Monster) ->        -- the update
               Actor ->                       -- who to update
               State ->
               State
updateActor f a state =
  updateLevel (updateMonsters (M.adjust f a)) state
