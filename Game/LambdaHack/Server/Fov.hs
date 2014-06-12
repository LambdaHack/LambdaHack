-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/LambdaHack/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Server.Fov
  ( dungeonPerception, levelPerception
  , PersLit, litInDungeon
  ) where

import Control.Arrow (second)
import qualified Data.EnumMap.Lazy as EML
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Server.Fov.Common
import qualified Game.LambdaHack.Server.Fov.Digital as Digital
import qualified Game.LambdaHack.Server.Fov.Permissive as Permissive
import qualified Game.LambdaHack.Server.Fov.Shadow as Shadow
import Game.LambdaHack.Server.State

-- | Visually reachable position (light passes through them to the actor).
newtype PerceptionReachable = PerceptionReachable
    {preachable :: [Point]}
  deriving Show

-- | All lit positions on a level.
newtype PerceptionLit = PerceptionLit
    {plit :: ES.EnumSet Point}
  deriving Show

type PersLit = EML.EnumMap LevelId PerceptionLit

-- | Calculate perception of the level.
levelPerception :: PerceptionLit -> FovMode -> FactionId
                -> LevelId -> Level -> State -> StateServer
                -> Perception
levelPerception litHere fovMode fid lid lvl@Level{lxsize, lysize} s ser =
  let cops@Kind.COps{cotile} = scops s
      -- Dying actors included, to let them see their own demise.
      ours = filter (not . bproj . snd) $ actorAssocs (== fid) lid s
      cR (aid, b) = preachable $ reachableFromActor cops fovMode lvl aid b s ser
      totalReachable = PerceptionReachable $ concatMap cR ours
      pAndVicinity p = p : vicinity lxsize lysize p
      noctoBodies = map (\(aid, b) -> (pAndVicinity $ bpos b, aid)) ours
      nocto = concat $ map fst noctoBodies
      ptotal = visibleOnLevel cotile totalReachable litHere nocto lvl
      canSmell aid =
        let eqpAssocs =
              fullAssocs cops (sdisco ser) (sdiscoAE ser) aid [CEqp] s
            bodyAssocs =
              fullAssocs cops (sdisco ser) (sdiscoAE ser) aid [CBody] s
            radius = strongestBodyEqp strongestSmellRadius eqpAssocs bodyAssocs
        in radius > 0
      -- TODO: We assume smell FOV radius is always 1, regardless of vision
      -- radius of the actor (and whether he can see at all).
      -- Instead, use the smell radius.
      -- TODO: filter out tiles that are solid and so can't hold smell.
      psmell = PerceptionVisible $ ES.fromList
               $ concat $ map fst $ filter (canSmell . snd) noctoBodies
  in Perception ptotal psmell

-- | Calculate perception of a faction.
factionPerception :: FovMode -> FactionId -> State -> StateServer -> PersLit
                  -> FactionPers
factionPerception fovMode fid s ser persLit =
  let lvlPer lid lvl = let lit = persLit EML.! lid
                       in levelPerception lit fovMode fid lid lvl s ser
  in EM.mapWithKey lvlPer $ sdungeon s

-- | Calculate the perception of the whole dungeon.
dungeonPerception :: FovMode -> State -> StateServer -> Pers
dungeonPerception fovMode s ser =
  let persLit = litInDungeon fovMode s
      f fid _ = factionPerception fovMode fid s ser persLit
  in EM.mapWithKey f $ sfactionD s

-- | Compute positions visible (reachable and seen) by the party.
-- A position can be directly lit by an ambient shine or by a weak, portable
-- light source, e.g,, carried by an actor. A reachable and lit position
-- is visible. Additionally, positions directly adjacent to an actor are
-- assumed to be visible to him (through sound, touch, noctovision, whatever).
visibleOnLevel :: Kind.Ops TileKind -> PerceptionReachable
               -> PerceptionLit -> [Point] -> Level
               -> PerceptionVisible
visibleOnLevel cotile PerceptionReachable{preachable} PerceptionLit{plit}
               nocto lvl =
  let isVisible pos = Tile.isLit cotile (lvl `at` pos) || pos `ES.member` plit
  in PerceptionVisible $ ES.fromList $ nocto ++ filter isVisible preachable

-- | Compute positions reachable by the actor. Reachable are all fields
-- on a visually unblocked path from the actor position.
reachableFromActor :: Kind.COps -> FovMode -> Level -> ActorId -> Actor
                   -> State -> StateServer
                   -> PerceptionReachable
reachableFromActor cops@Kind.COps{cotile}
                   fovMode lvl aid body s ser =
  let eqpAssocs =
        fullAssocs cops (sdisco ser) (sdiscoAE ser) aid [CEqp] s
      bodyAssocs =
        fullAssocs cops (sdisco ser) (sdiscoAE ser) aid [CBody] s
      radius = 1 +  -- all actors feel adjacent positions (for easy exploration)
               strongestBodyEqp strongestSightRadius eqpAssocs bodyAssocs
  in PerceptionReachable $ fullscan cotile fovMode radius (bpos body) lvl

litByItems :: FovMode -> Level -> Point -> State
           -> [(ItemId, (Item, KisOn))]
           -> [Point]
litByItems fovMode lvl p s iis =
  let Kind.COps{cotile} = scops s
  in case strongestLight True $ map (second itemNoDisco) iis of
    (radius, _) : _ ->
      let scan = fullscan cotile fovMode radius p lvl
      -- Optimization: filter out positions that already have ambient light.
      in filter (\pos -> not $ Tile.isLit cotile $ lvl `at` pos) scan
    [] -> []

-- | Compute all lit positions on a level.
litOnLevel :: FovMode -> LevelId -> Level -> State -> PerceptionLit
litOnLevel fovMode lid lvl s =
  let -- Compute positions lit by the actor. Note that the actor can be blind
      -- or a projectile, in which case he doesn't see his own light
      -- (but others, from his or other factions, possibly do).
      eqpAssocs b = bagAssocsK s $ beqp b
      bodyAssocs b = bagAssocsK s $ bbody b
      allAssocs b = eqpAssocs b ++ bodyAssocs b
      liActor b = litByItems fovMode lvl (bpos b) s (allAssocs b)
      litFromActors = concatMap liActor $ actorList (const True) lid s
      -- Compute positions lit by floor items.
      litFloorBag (p, bag) = litByItems fovMode lvl p s (bagAssocsK s bag)
      litFromFloor = concatMap litFloorBag $ EM.assocs $ lfloor lvl
  in PerceptionLit $ ES.fromList $ litFromActors ++ litFromFloor

-- | Compute all lit positions in the dungeon
litInDungeon :: FovMode -> State -> PersLit
litInDungeon fovMode s =
  let litLvl (lid, lvl) = (lid, litOnLevel fovMode lid lvl s)
  in EML.fromDistinctAscList $ map litLvl $ EM.assocs $ sdungeon s

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view. The Field of View
-- algorithm to use is passed in the second argument.
-- The actor's own position is considred reachable by him.
fullscan :: Kind.Ops TileKind  -- ^ tile content, determines clear tiles
         -> FovMode            -- ^ scanning mode
         -> Int                -- ^ scanning radius
         -> Point              -- ^ position of the spectator
         -> Level              -- ^ the map that is scanned
         -> [Point]
fullscan cotile fovMode r spectatorPos lvl = spectatorPos :
  if r <= 0
  then []
  else case fovMode of
    Shadow ->
      concatMap (\tr -> map tr (Shadow.scan (isCl . tr) 1 (0, 1))) tr8
    Permissive ->
      concatMap (\tr -> map tr (Permissive.scan (isCl . tr))) tr4
    Digital ->
      concatMap (\tr -> map tr (Digital.scan r (isCl . tr))) tr4
 where
  isCl :: Point -> Bool
  isCl = Tile.isClear cotile . (lvl `at`)

  -- This function is cheap, so no problem it's called twice
  -- for each point: once with @isCl@, once via @concatMap@.
  trV :: X -> Y -> Point
  {-# INLINE trV #-}
  trV x y = shift spectatorPos $ Vector x y

  -- | The translation, rotation and symmetry functions for octants.
  tr8 :: [(Distance, Progress) -> Point]
  tr8 =
    [ \(p, d) -> trV   p    d
    , \(p, d) -> trV (-p)   d
    , \(p, d) -> trV   p  (-d)
    , \(p, d) -> trV (-p) (-d)
    , \(p, d) -> trV   d    p
    , \(p, d) -> trV (-d)   p
    , \(p, d) -> trV   d  (-p)
    , \(p, d) -> trV (-d) (-p)
    ]

  -- | The translation and rotation functions for quadrants.
  tr4 :: [Bump -> Point]
  tr4 =
    [ \B{..} -> trV   bx  (-by)  -- quadrant I
    , \B{..} -> trV   by    bx   -- II (we rotate counter-clockwise)
    , \B{..} -> trV (-bx)   by   -- III
    , \B{..} -> trV (-by) (-bx)  -- IV
    ]
