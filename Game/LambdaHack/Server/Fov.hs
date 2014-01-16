-- | Field Of View scanning with a variety of algorithms.
-- See <https://github.com/kosmikus/LambdaHack/wiki/Fov-and-los>
-- for discussion.
module Game.LambdaHack.Server.Fov
  ( dungeonPerception, levelPerception
  , fullscan, FovMode(..)
  ) where

import Control.Arrow (second)
import Data.Binary
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
import Game.LambdaHack.Common.VectorXY
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Server.Fov.Common
import qualified Game.LambdaHack.Server.Fov.Digital as Digital
import qualified Game.LambdaHack.Server.Fov.Permissive as Permissive
import qualified Game.LambdaHack.Server.Fov.Shadow as Shadow

newtype PerceptionReachable = PerceptionReachable
    {preachable :: ES.EnumSet Point}
  deriving Show

-- | Calculate perception of the level.
levelPerception :: Kind.COps -> State -> FovMode -> FactionId
                -> LevelId -> Level
                -> Perception
levelPerception cops@Kind.COps{cotile} s configFov fid lid
                lvl@Level{lxsize, lysize} =
  let hs = actorNotProjAssocs (== fid) lid s
      reas = map (second $ computeReachable cops configFov lvl) hs
      lreas = map (preachable . snd) reas
      totalRea = PerceptionReachable $ ES.unions lreas
      -- TODO: give actors light sources explicitly or alter vision.
      pAndVicinity p = p : vicinity lxsize lysize p
      lights = ES.fromList $ concatMap (pAndVicinity . bpos . snd) hs
      ptotal = computeVisible cotile totalRea lvl lights
      g = PerceptionVisible . ES.intersection (pvisible ptotal) . preachable
      perActor = EM.map g $ EM.fromList reas  -- reas is not sorted
      psmell = smellFromActors cops s perActor
  in Perception {..}

-- | Calculate perception of a faction.
factionPerception :: Kind.COps -> FovMode -> State -> FactionId
                  -> FactionPers
factionPerception cops configFov s fid =
  EM.mapWithKey (levelPerception cops s configFov fid) $ sdungeon s

-- | Calculate the perception of the whole dungeon.
dungeonPerception :: Kind.COps -> FovMode -> State -> Pers
dungeonPerception cops configFov s =
  let f fid _ = factionPerception cops configFov s fid
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
               -> Level -> ES.EnumSet Point -> PerceptionVisible
computeVisible cotile PerceptionReachable{preachable} lvl lights =
  let isV = isVisible cotile lvl lights
  in PerceptionVisible $ ES.filter isV preachable

isVisible :: Kind.Ops TileKind -> Level -> ES.EnumSet Point -> Point -> Bool
isVisible cotile lvl lights pos =
  Tile.isLit cotile (lvl `at` pos)
  || pos `ES.member` lights

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
  in PerceptionReachable $ ES.fromList scan
       -- Let's hope the list construction is optimized away completely.
       -- If so, there's no point switching to vectors or constructing
       -- the set earlier. We can't apply @fromDistinctAscList@,
       -- which is cheaper, because the list is not sorted.

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

  -- This function is very cheap, so no problem it's called twice
  -- for each point: once with @isCl@, once via @concatMap@.
  trV :: X -> Y -> Point
  {-# INLINE trV #-}
  trV x y = shift spectatorPos $ toVector $ VectorXY x y

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

-- TODO: should Blind really be a FovMode, or a modifier? Let's decide
-- when other similar modifiers are added.
-- | Field Of View scanning mode.
data FovMode =
    Shadow        -- ^ restrictive shadow casting
  | Permissive    -- ^ permissive FOV
  | Digital !Int  -- ^ digital FOV with the given radius
  | Blind         -- ^ only feeling out adjacent tiles by touch
  deriving (Show, Read)

instance Binary FovMode where
  put Shadow      = putWord8 0
  put Permissive  = putWord8 1
  put (Digital r) = putWord8 2 >> put r
  put Blind       = putWord8 3
  get = do
    tag <- getWord8
    case tag of
      0 -> return Shadow
      1 -> return Permissive
      2 -> fmap Digital get
      3 -> return Blind
      _ -> fail "no parse (FovMode)"
