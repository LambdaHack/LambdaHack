module Actor where

import Level
import Monster

data Actor = AMonster Int  -- offset in monster list
           | APlayer
  deriving (Show, Eq)

getActor :: Level -> Player -> Actor -> Monster
getActor lvl p (AMonster n) = lmonsters lvl !! n
getActor lvl p APlayer      = p

updateActor :: (Monster -> Monster) ->                  -- the update
               (Monster -> Level -> Player -> IO a) ->  -- continuation
               Actor ->                                 -- who to update
               Level -> Player -> IO a                  -- transformed continuation
updateActor f k (AMonster n) lvl p = 
  let (m,ms) = updateMonster f n (lmonsters lvl)
  in  k m (updateMonsters lvl (const ms)) p
updateActor f k APlayer      lvl p = k p lvl (f p)

updateMonster :: (Monster -> Monster) -> Int -> [Monster] -> (Monster, [Monster])
updateMonster f n ms =
  case splitAt n ms of
    (pre, x : post) -> let m = f x in (m, pre ++ [m] ++ post)
    xs              -> error "updateMonster"


