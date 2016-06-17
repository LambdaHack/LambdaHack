{-# LANGUAGE CPP #-}
-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Server.Fov
  ( dungeonPerception, fidLidPerception
  , PersLit, litInDungeon
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , PerceptionReachable(..), PerceptionDynamicLit(..)
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Lazy as EML
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Server.FovDigital
import Game.LambdaHack.Server.State

-- | Visually reachable positions (light passes through them to the actor).
-- The list may contain (many) repetitions.
newtype PerceptionReachable = PerceptionReachable
    {preachable :: [Point]}
  deriving Show

-- | All positions lit by dynamic lights on a level. Shared by all factions.
-- The list may contain (many) repetitions.
newtype PerceptionDynamicLit = PerceptionDynamicLit
    {pdynamicLit :: [Point]}
  deriving Show

-- | The cache of FOV information for a level, such as sight, smell
-- and light radiuses for each actor and bitmaps of clear and lit positions.
type PersLit = EML.EnumMap LevelId ( EM.EnumMap FactionId [(Actor, FovCache3)]
                                   , PointArray.Array Bool
                                   , PointArray.Array Bool )

-- | Calculate faction's perception of a level.
levelPerception :: [(Actor, FovCache3)]
                -> PointArray.Array Bool -> PointArray.Array Bool -> Level
                -> Perception
levelPerception actorEqpBody clearPs litPs Level{lxsize, lysize} =
  let -- Dying actors included, to let them see their own demise.
      ourR = preachable . reachableFromActor clearPs
      totalReachable = PerceptionReachable $ concatMap ourR actorEqpBody
      -- All non-projectile actors feel adjacent positions,
      -- even dark (for easy exploration). Projectiles rely on cameras.
      pAndVicinity p = p : vicinity lxsize lysize p
      gatherVicinities = concatMap (pAndVicinity . bpos . fst)
      nocteurs = filter (not . bproj . fst) actorEqpBody
      nocto = gatherVicinities nocteurs
      ptotal = visibleOnLevel totalReachable litPs nocto
      -- TODO: handle smell radius < 2, that is only under the actor
      -- Projectiles can potentially smell, too.
      canSmellAround FovCache3{fovSmell} = fovSmell >= 2
      smellers = filter (canSmellAround . snd) actorEqpBody
      smells = gatherVicinities smellers
      -- No smell stored in walls and under other actors.
      canHoldSmell p = clearPs PointArray.! p
      psmell = PerceptionVisible $ ES.fromList $ filter canHoldSmell smells
  in Perception ptotal psmell

-- | Calculate faction's perception of a level based on the lit tiles cache.
fidLidPerception :: PersLit -> FactionId -> LevelId -> Level
                 -> Perception
fidLidPerception persLit fid lid lvl =
  let (bodyMap, clearPs, litPs) = persLit EML.! lid
      actorEqpBody = EM.findWithDefault [] fid bodyMap
  in levelPerception actorEqpBody clearPs litPs lvl

-- | Calculate perception of a faction.
factionPerception :: PersLit -> FactionId -> State -> FactionPers
factionPerception persLit fid s =
  EM.mapWithKey (fidLidPerception persLit fid) $ sdungeon s

-- | Calculate the perception of the whole dungeon.
dungeonPerception :: State -> StateServer -> Pers
dungeonPerception s ser =
  let persLit = litInDungeon s ser
      f fid _ = factionPerception persLit fid s
  in EM.mapWithKey f $ sfactionD s

-- | Compute positions visible (reachable and seen) by the party.
-- A position can be directly lit by an ambient shine or by a weak, portable
-- light source, e.g,, carried by an actor. A reachable and lit position
-- is visible. Additionally, positions directly adjacent to an actor are
-- assumed to be visible to him (through sound, touch, noctovision, whatever).
visibleOnLevel :: PerceptionReachable -> PointArray.Array Bool -> [Point]
               -> PerceptionVisible
visibleOnLevel PerceptionReachable{preachable} litPs nocto =
  let isVisible = (litPs PointArray.!)
  in PerceptionVisible $ ES.fromList $ nocto ++ filter isVisible preachable

-- | Compute positions reachable by the actor. Reachable are all fields
-- on a visually unblocked path from the actor position.
reachableFromActor :: PointArray.Array Bool -> (Actor, FovCache3)
                   -> PerceptionReachable
reachableFromActor clearPs (body, FovCache3{fovSight}) =
  let radius = min (fromIntegral $ bcalm body `div` (5 * oneM)) fovSight
  in PerceptionReachable $ fullscan clearPs radius (bpos body)

-- | Compute all dynamically lit positions on a level, whether lit by actors
-- or floor items. Note that an actor can be blind, in which case he doesn't see
-- his own light (but others, from his or other factions, possibly do).
litByItems :: PointArray.Array Bool -> [(Point, Int)]
           -> PerceptionDynamicLit
litByItems clearPs allItems =
  let litPos :: (Point, Int) -> [Point]
      litPos (p, light) = fullscan clearPs light p
  in PerceptionDynamicLit $ concatMap litPos allItems

-- | Compute all lit positions in the dungeon.
litInDungeon :: State -> StateServer -> PersLit
litInDungeon s ser =
  let Kind.COps{cotile} = scops s
      processIid3 (FovCache3 sightAcc smellAcc lightAcc) (iid, (k, _)) =
        let FovCache3{..} =
              EM.findWithDefault emptyFovCache3 iid $ sItemFovCache ser
        in FovCache3 (k * fovSight + sightAcc)
                     (k * fovSmell + smellAcc)
                     (k * fovLight + lightAcc)
      processBag3 bag acc = foldl' processIid3 acc $ EM.assocs bag
      itemsInActors :: Level -> EM.EnumMap FactionId [(Actor, FovCache3)]
      itemsInActors lvl =
        let processActor aid =
              let b = getActorBody aid s
                  sslOrgan = processBag3 (borgan b) emptyFovCache3
                  ssl = processBag3 (beqp b) sslOrgan
              in (bfid b, [(b, ssl)])
            asLid = map processActor $ concat $ EM.elems $ lprio lvl
        in EM.fromListWith (++) asLid
      processIid lightAcc (iid, (k, _)) =
        let FovCache3{fovLight} =
              EM.findWithDefault emptyFovCache3 iid $ sItemFovCache ser
        in k * fovLight + lightAcc
      processBag bag acc = foldl' processIid acc $ EM.assocs bag
      lightOnFloor :: Level -> [(Point, Int)]
      lightOnFloor lvl =
        let processPos (p, bag) = (p, processBag bag 0)
        in map processPos $ EM.assocs $ lfloor lvl  -- lembed are hidden
      -- Note that an actor can be blind,
      -- in which case he doesn't see his own light
      -- (but others, from his or other factions, possibly do).
      litOnLevel :: Level -> ( EM.EnumMap FactionId [(Actor, FovCache3)]
                             , PointArray.Array Bool
                             , PointArray.Array Bool )
      litOnLevel lvl@Level{ltile} =
        let bodyMap = itemsInActors lvl
            allBodies = concat $ EM.elems bodyMap
            clearTiles = PointArray.mapA (Tile.isClear cotile) ltile
            blockFromBody (b, _) =
              if bproj b then Nothing else Just (bpos b, False)
            -- TODO: keep it in server state and update when tiles change
            -- and actors are born/move/die. Actually, do this for PersLit.
            blockingActors = mapMaybe blockFromBody allBodies
            clearPs = clearTiles PointArray.// blockingActors
            litTiles = PointArray.mapA (Tile.isLit cotile) ltile
            actorLights = map (\(b, FovCache3{fovLight}) -> (bpos b, fovLight))
                              allBodies
            floorLights = lightOnFloor lvl
            -- If there is light both on the floor and carried by actor,
            -- only the stronger light is taken into account.
            -- This is rare, so no point optimizing away the double computation.
            allLights = floorLights ++ actorLights
            litDynamic = pdynamicLit $ litByItems clearPs allLights
            litPs = litTiles PointArray.// map (\p -> (p, True)) litDynamic
        in (bodyMap, clearPs, litPs)
      litLvl (lid, lvl) = (lid, litOnLevel lvl)
  in EML.fromDistinctAscList $ map litLvl $ EM.assocs $ sdungeon s

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view. The Field of View
-- algorithm to use is passed in the second argument.
-- The actor's own position is considred reachable by him.
fullscan :: PointArray.Array Bool  -- ^ the array with clear points
         -> Int        -- ^ scanning radius
         -> Point      -- ^ position of the spectator
         -> [Point]
fullscan clearPs radius spectatorPos =
  if | radius <= 0 -> []
     | radius == 1 -> [spectatorPos]
     | otherwise -> spectatorPos :
          mapTr (\B{..} -> trV   bx  (-by))  -- quadrant I
       ++ mapTr (\B{..} -> trV   by    bx)   -- II (we rotate counter-clockwise)
       ++ mapTr (\B{..} -> trV (-bx)   by)   -- III
       ++ mapTr (\B{..} -> trV (-by) (-bx))  -- IV
 where
  mapTr :: (Bump -> Point) -> [Point]
  {-# INLINE mapTr #-}
  mapTr tr = map tr $ scan (radius - 1) (isCl . tr)

  isCl :: Point -> Bool
  {-# INLINE isCl #-}
  isCl = (clearPs PointArray.!)

  -- This function is cheap, so no problem it's called twice
  -- for each point: once with @isCl@, once via @concatMap@.
  trV :: X -> Y -> Point
  {-# INLINE trV #-}
  trV x y = shift spectatorPos $ Vector x y
