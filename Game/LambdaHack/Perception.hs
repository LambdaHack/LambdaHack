module Game.LambdaHack.Perception
  ( Perceptions, totalVisible, debugTotalReachable, perception
  , actorReachesLoc, actorReachesActor
  ) where

import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad

import Game.LambdaHack.Loc
import Game.LambdaHack.State
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.FOV
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Content.TileKind

newtype PerceptionReachable = PerceptionReachable
  { preachable :: IS.IntSet
  }

newtype PerceptionVisible = PerceptionVisible
  { pvisible :: IS.IntSet
  }

-- Heroes share visibility and only have separate reachability.
-- The pplayer field is void if player not on the current level,
-- or if the player controls a blind monster. Right now, the field is used only
-- for player-controlled monsters on the current level.
data Perceptions = Perceptions
  { pplayer :: Maybe PerceptionReachable
  , pheroes :: IM.IntMap PerceptionReachable
  , ptotal  :: PerceptionVisible
  }

totalVisible, debugTotalReachable :: Perceptions -> IS.IntSet
totalVisible = pvisible . ptotal
debugTotalReachable per =
  let lpers = maybeToList (pplayer per) ++ IM.elems (pheroes per)
  in IS.unions (map preachable lpers)

actorReachesLoc :: ActorId -> Loc -> Perceptions -> Maybe ActorId -> Bool
actorReachesLoc actor loc per pl =
  let tryHero = case actor of
                  AMonster _ -> Nothing
                  AHero i -> do
                    hper <- IM.lookup i (pheroes per)
                    return $ loc `IS.member` preachable hper
      tryPl   = do -- the case for a monster under player control
                   guard $ Just actor == pl
                   pper <- pplayer per
                   return $ loc `IS.member` preachable pper
      tryAny  = tryHero `mplus` tryPl
  in fromMaybe False tryAny  -- assume not visible, if no perception found

-- Not quite correct if FOV not symmetric (Shadow).
actorReachesActor :: ActorId -> ActorId -> Loc -> Loc
                  -> Perceptions -> Maybe ActorId
                  -> Bool
actorReachesActor actor1 actor2 loc1 loc2 per pl =
  actorReachesLoc actor1 loc2 per pl ||
  actorReachesLoc actor2 loc1 per pl

perception :: Kind.COps -> State -> Perceptions
perception cops@Kind.COps{cotile}
           state@State{ splayer = pl
                      , sconfig  = config
                      , ssensory = sensory } =
  let lvl@Level{lheroes = hs} = slevel state
      mode   = Config.get config "engine" "fovMode"
      radius = let r = Config.get config "engine" "fovRadius"
               in if r < 1
                  then error $ "FOV radius is " ++ show r ++ ", should be >= 1"
                  else r
      -- Perception for a player-controlled monster on the current level.
      mLocPer =
        if isAMonster pl && memActor pl state
        then let m = getPlayerBody state
             in Just (bloc m, computeReachable cops radius mode sensory m lvl)
        else Nothing
      (mLoc, mPer) = (fmap fst mLocPer, fmap snd mLocPer)
      pers = IM.map (\ h -> computeReachable cops radius mode sensory h lvl) hs
      locs = IM.map bloc hs
      lpers = maybeToList mPer ++ IM.elems pers
      reachable = PerceptionReachable $ IS.unions (map preachable lpers)
      -- TODO: Instead of giving the monster a light source, alter vision.
      playerControlledMonsterLight = maybeToList mLoc
      lights = IS.fromList $ playerControlledMonsterLight ++ IM.elems locs
      visible = computeVisible cotile reachable lvl lights
  in  Perceptions { pplayer = mPer
                  , pheroes = pers
                  , ptotal  = visible
                  }

-- | A location can be directly lit by an ambient shine or a portable
-- light source, e.g,, carried by a hero. (Only lights of radius 0
-- are considered for now.) A location is visible if it's directly lit
-- or adjacent to one that is directly lit _and_ reachable. The last condition
-- approximates being on the same side of obstacles as the light source.
-- The approximation is not exact for multiple heroes, but the discrepancy
-- can be attributed to deduction based on combined vague visual hints,
-- e.g., if I don't see the reachable light seen by another hero,
-- there must be a wall in-between. Stray rays indicate doors,
-- moving shadows indicate monsters, etc.).
computeVisible :: Kind.Ops TileKind -> PerceptionReachable -> Level -> IS.IntSet
               -> PerceptionVisible
computeVisible cops (PerceptionReachable reachable)
               lvl@Level{lxsize, lysize} lights' =
  let lights = IS.intersection lights' reachable  -- optimization
      litDirectly loc = Tile.isLit cops (lvl `at` loc) || loc `IS.member` lights
      l_and_R loc = litDirectly loc && loc `IS.member` reachable
      lit loc =
        let srds = surroundings lxsize lysize loc
        in litDirectly loc || L.any l_and_R srds
  in PerceptionVisible $ IS.filter lit reachable

-- | Reachable are all fields on an unblocked path from the hero position.
-- The player's own position is considred reachable by him.
computeReachable :: Kind.COps -> Int -> String -> SensoryMode
                 -> Actor -> Level -> PerceptionReachable
computeReachable Kind.COps{cotile, coactor=Kind.Ops{okind}}
                 radius mode sensory actor lvl =
  let fovMode m =
        if not $ asight $ okind $ bkind m
        then Blind
        else
          -- terrible, temporary hack
          case sensory of
            Vision 3 -> Digital radius
            Vision 2 -> Permissive
            Vision 1 -> Shadow
            Smell    -> Blind
            _        ->
              -- this is not a hack
              case mode of
                "permissive" -> Permissive
                "digital"    -> Digital radius
                "shadow"     -> Shadow
                _            -> error $ "Unknown FOV mode: " ++ show mode
      ploc = bloc actor
  in PerceptionReachable $
       IS.insert ploc $ IS.fromList $ fullscan (fovMode actor) ploc cotile lvl
