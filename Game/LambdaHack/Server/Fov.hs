-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/kosmikus/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Server.Fov
  ( dungeonPerception, levelPerception, fullscan
  ) where

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Server.Fov.Common
import qualified Game.LambdaHack.Server.Fov.Digital as Digital
import qualified Game.LambdaHack.Server.Fov.Permissive as Permissive
import qualified Game.LambdaHack.Server.Fov.Shadow as Shadow

newtype PerceptionReachable = PerceptionReachable
    {preachable :: [Point]}
  deriving Show

-- | Calculate perception of the level.
levelPerception :: Kind.COps -> FovMode -> FactionId
                -> LevelId -> Level -> State
                -> Perception
levelPerception cops@Kind.COps{cotile, coactor=Kind.Ops{okind}}
                configFov fid lid lvl@Level{lxsize, lysize} s =
  let hs = filter (not . bproj) $ actorList (== fid) lid s
      cR b = preachable $ computeReachable cops configFov lvl b
      totalReachable = PerceptionReachable $ concatMap cR hs
      -- TODO: give actors light sources explicitly or alter vision.
      pAndVicinity p = p : vicinity lxsize lysize p
      lightsBodies = map (\b -> (pAndVicinity $ bpos b, b)) hs
      light = concat $ map fst lightsBodies
      ptotal = computeVisible cotile totalReachable lvl light
      canSmell b = asmell $ okind $ bkind b
      -- We assume smell FOV radius is always 1, regardless of vision
      -- radius of the actor (if he can see at all).
      psmell = PerceptionVisible $ ES.fromList
               $ concat $ map fst $ filter (canSmell . snd) lightsBodies
  in Perception ptotal psmell

-- | Calculate perception of a faction.
factionPerception :: Kind.COps -> FovMode -> FactionId -> State
                  -> FactionPers
factionPerception cops configFov fid s =
  EM.mapWithKey (\lid lvl -> levelPerception cops configFov fid lid lvl s)
                $ sdungeon s

-- | Calculate the perception of the whole dungeon.
dungeonPerception :: Kind.COps -> FovMode -> State -> Pers
dungeonPerception cops configFov s =
  let f fid _ = factionPerception cops configFov fid s
  in EM.mapWithKey f $ sfactionD s

-- | A position can be directly lit by an ambient shine or a weak, portable
-- light source, e.g,, carried by a hero. (Only lights of radius 0
-- are considered for now and it's assumed they do not reveal hero's position.
-- TODO: change this to be radius 1 noctovision and introduce stronger
-- light sources that show more but make the hero visible.)
-- A position is visible if it's reachable and either directly lit
-- or adjacent to one that is at once directly lit and reachable.
-- The last condition approximates being
-- on the same side of obstacles as the light source.
-- The approximation is not exact for multiple heroes, but the discrepancy
-- can be attributed to deduction based on combined vague visual hints,
-- e.g., if I don't see the reachable light seen by another hero,
-- there must be a wall in-between. Stray rays indicate doors,
-- moving shadows indicate monsters, etc.
computeVisible :: Kind.Ops TileKind -> PerceptionReachable
               -> Level -> [Point] -> PerceptionVisible
computeVisible cotile PerceptionReachable{preachable} lvl lights =
  let isVisible pos = Tile.isLit cotile (lvl `at` pos)
  in PerceptionVisible $ ES.fromList $ lights ++ filter isVisible preachable

-- | Reachable are all fields on a visually unblocked path
-- from the hero position.
computeReachable :: Kind.COps -> FovMode -> Level -> Actor
                 -> PerceptionReachable
computeReachable Kind.COps{cotile, coactor=Kind.Ops{okind}}
                 configFov lvl body =
  let sight = asight $ okind $ bkind body
      fovMode = if sight then configFov else Blind
      ppos = bpos body
      scan = fullscan cotile fovMode ppos lvl
  in PerceptionReachable scan

-- | Perform a full scan for a given position. Returns the positions
-- that are currently in the field of view. The Field of View
-- algorithm to use, passed in the second argument, is set in the config file.
-- The actor's own position is considred reachable by him.
fullscan :: Kind.Ops TileKind  -- ^ tile content, determines clear tiles
         -> FovMode            -- ^ scanning mode
         -> Point              -- ^ position of the spectator
         -> Level              -- ^ the map that is scanned
         -> [Point]
fullscan cotile fovMode spectatorPos lvl = spectatorPos :
  case fovMode of
    Shadow ->
      concatMap (\tr -> map tr (Shadow.scan (isCl . tr) 1 (0, 1))) tr8
    Permissive ->
      concatMap (\tr -> map tr (Permissive.scan (isCl . tr))) tr4
    Digital r ->
      concatMap (\tr -> map tr (Digital.scan r (isCl . tr))) tr4
    Blind ->  -- all actors feel adjacent positions (for easy exploration)
      let radiusOne = 1
      in concatMap (\tr -> map tr (Digital.scan radiusOne (isCl . tr))) tr4
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
