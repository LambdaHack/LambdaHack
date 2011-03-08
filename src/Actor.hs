module Actor where

import qualified Data.IntMap as IM

import Level
import Monster
import State

data Actor = AHero Int     -- ^ hero serial number
           | AMonster Int  -- ^ offset in monster list
           | APlayer       -- ^ currently player-controlled hero
  deriving (Show, Eq)

getActor :: State -> Actor -> Movable
getActor (State { slevel = lvl, splayer = p }) a =
  case a of
    AHero n    -> if n == heroNumber p then p else lheroes lvl IM.! n
    AMonster n -> lmonsters lvl !! n
    APlayer    -> p

updateActor :: (Movable -> Movable) ->        -- the update
               (Movable -> State -> IO a) ->  -- continuation
               Actor ->                       -- who to update
               State -> IO a                  -- transformed continuation
updateActor f k (AHero n) state =
  let s = updateAnyHero f n state
  in case findHeroLevel n state of
       Just (_, h) -> k h s
       Nothing     -> error "updateActor(Hero)"
updateActor f k (AMonster n) state@(State { slevel = lvl, splayer = p }) =
  let (m,ms) = updateMonster f n (lmonsters lvl)
  in  k m (updateLevel (updateMonsters (const ms)) state)
updateActor f k APlayer      state@(State { slevel = lvl, splayer = p }) =
  k p (updatePlayer f state)

updateMonster :: (Monster -> Monster) -> Int -> [Monster] ->
                 (Monster, [Monster])
updateMonster f n ms =
  case splitAt n ms of
    (pre, x : post) -> let m = f x
                           mtimeChanged = mtime x /= mtime m
                       in (m, if mtimeChanged then snd (insertMonster m (pre ++ post))
                                              else pre ++ [m] ++ post)
    xs              -> error "updateMonster"
