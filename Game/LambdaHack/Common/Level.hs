-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Common.Level
  ( -- * Dungeon
    LevelId, AbsDepth, Dungeon, ascendInBranch
    -- * The @Level@ type and its components
  , Level(..), ItemFloor, ActorMap, TileMap, SmellMap
    -- * Level query
  , at, accessible, accessibleUnknown, accessibleDir
  , knownLsecret, isSecretPos, isChancePos, hideTile
  , findPoint, findPos, findPosTry, findPosTry2
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.Bits as Bits
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import qualified Game.LambdaHack.Common.KindOps as KindOps
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Content.TileKind (TileKind, isUknownSpace)

-- | The complete dungeon is a map from level names to levels.
type Dungeon = EM.EnumMap LevelId Level

-- | Levels in the current branch, @k@ levels shallower than the current.
ascendInBranch :: Dungeon -> Bool -> LevelId -> [LevelId]
ascendInBranch dungeon up lid =
  -- Currently there is just one branch, so the computation is simple.
  let (minD, maxD) =
        case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> assert `failure` "null dungeon" `twith` dungeon
      ln = max minD $ min maxD $ toEnum $ fromEnum lid + if up then 1 else -1
  in case EM.lookup ln dungeon of
    Just _ | ln /= lid -> [ln]
    _ | ln == lid -> []
    _ -> ascendInBranch dungeon up ln  -- jump over gaps

-- | Items located on map tiles.
type ItemFloor = EM.EnumMap Point ItemBag

-- | Items located on map tiles.
type ActorMap = EM.EnumMap Point [ActorId]

-- | Tile kinds on the map.
type TileMap = PointArray.GArray Word16 (Kind.Id TileKind)

-- | Current smell on map tiles.
type SmellMap = EM.EnumMap Point Time

-- | A view on single, inhabited dungeon level. "Remembered" fields
-- carry a subset of the info in the client copies of levels.
data Level = Level
  { ldepth      :: !AbsDepth   -- ^ absolute depth of the level
  , lfloor      :: !ItemFloor  -- ^ remembered items lying on the floor
  , lembed      :: !ItemFloor  -- ^ items embedded in the tile
  , lactor      :: !ActorMap   -- ^ seen actors at positions on the level
  , ltile       :: !TileMap    -- ^ remembered level map
  , lxsize      :: !X          -- ^ width of the level
  , lysize      :: !Y          -- ^ height of the level
  , lsmell      :: !SmellMap   -- ^ remembered smells on the level
  , ldesc       :: !Text       -- ^ level description
  , lstair      :: !([Point], [Point])
                               -- ^ positions of (up, down) stairs
  , lseen       :: !Int        -- ^ currently remembered clear tiles
  , lclear      :: !Int        -- ^ total number of initially clear tiles
  , ltime       :: !Time       -- ^ local time on the level (possibly frozen)
  , lactorCoeff :: !Int        -- ^ the lower, the more monsters spawn
  , lactorFreq  :: !(Freqs ItemKind)
                               -- ^ frequency of spawned actors; [] for clients
  , litemNum    :: !Int        -- ^ number of initial items, 0 for clients
  , litemFreq   :: !(Freqs ItemKind)
                               -- ^ frequency of initial items; [] for clients
  , lsecret     :: !Int        -- ^ secret tile seed
  , lhidden     :: !Int        -- ^ secret tile density
  , lescape     :: ![Point]    -- ^ positions of IK.Escape tiles
  }
  deriving (Show, Eq)

assertSparseItems :: ItemFloor -> ItemFloor
assertSparseItems m =
  assert (EM.null (EM.filter EM.null m)
          `blame` "null floors found" `twith` m) m

assertSparseActors :: ActorMap -> ActorMap
assertSparseActors m =
  assert (EM.null (EM.filter null m)
          `blame` "null actor lists found" `twith` m) m

-- | Query for tile kinds on the map.
at :: Level -> Point -> Kind.Id TileKind
{-# INLINE at #-}
at Level{ltile} p = ltile PointArray.! p

-- | Check whether one position is accessible from another.
-- Precondition: the two positions are next to each other.
accessible :: Kind.COps -> Level -> Point -> Bool
accessible Kind.COps{coTileSpeedup} lvl tpos =
  Tile.isWalkable coTileSpeedup $ lvl `at` tpos

-- | Check whether one position is accessible from another,
-- but additionally treating unknown tiles as walkable.
-- Precondition: the two positions are next to each other.
accessibleUnknown :: Kind.COps -> Level -> Point -> Bool
accessibleUnknown Kind.COps{coTileSpeedup} lvl tpos =
  let tt = lvl `at` tpos
  in Tile.isWalkable coTileSpeedup tt || isUknownSpace tt

-- | Check whether actors can move from a position along a unit vector.
accessibleDir :: Kind.COps -> Level -> Point -> Vector -> Bool
accessibleDir cops lvl spos dir = accessible cops lvl $ spos `shift` dir

knownLsecret :: Level -> Bool
knownLsecret lvl = lsecret lvl /= 0

isSecretPos :: Level -> Point -> Bool
isSecretPos lvl (Point x y) =
  lhidden lvl > 0
  && (lsecret lvl `Bits.rotateR` x `Bits.xor` y + x) `mod` lhidden lvl == 0

isChancePos :: Int -> Int -> Point -> Bool
isChancePos oneInChance lsecret (Point x y) =
  oneInChance > 0
  && (lsecret `Bits.rotateR` x `Bits.xor` y + x) `mod` oneInChance == 0

hideTile :: Kind.COps -> Level -> Point -> Kind.Id TileKind
hideTile Kind.COps{cotile} lvl p =
  let t = lvl `at` p
      ht = Tile.hideAs cotile t
  in if isSecretPos lvl p then ht else t

-- | Find a random position on the map satisfying a predicate.
findPoint :: X -> Y -> (Point -> Maybe Point) -> Rnd Point
findPoint x y f =
  let search = do
        pxy <- randomR (0, (x - 1) * (y - 1))
        let pos = PointArray.punindex x pxy
        case f pos of
          Just p -> return p
          Nothing -> search
  in search

-- | Find a random position on the map satisfying a predicate.
findPos :: TileMap -> (Point -> Kind.Id TileKind -> Bool) -> Rnd Point
findPos ltile p =
  let (x, y) = PointArray.sizeA ltile
      search = do
        pxy <- randomR (0, (x - 1) * (y - 1))
        let tile = KindOps.Id $ ltile `PointArray.accessI` pxy
            pos = PointArray.punindex x pxy
        if p pos tile
        then return $! pos
        else search
  in search

-- | Try to find a random position on the map satisfying
-- conjunction of the mandatory and an optional predicate.
-- If the permitted number of attempts is not enough,
-- try again the same number of times without the next optional predicate,
-- and fall back to trying as many times, as needed, with only the mandatory
-- predicate.
findPosTry :: Int                                  -- ^ the number of tries
           -> TileMap                              -- ^ look up in this map
           -> (Point -> Kind.Id TileKind -> Bool)  -- ^ mandatory predicate
           -> [Point -> Kind.Id TileKind -> Bool]  -- ^ optional predicates
           -> Rnd Point
{-# INLINE findPosTry #-}
findPosTry numTries ltile m r = findPosTry2 numTries ltile m [] undefined r

findPosTry2 :: Int                                  -- ^ the number of tries
            -> TileMap                              -- ^ look up in this map
            -> (Point -> Kind.Id TileKind -> Bool)  -- ^ mandatory predicate
            -> [Point -> Kind.Id TileKind -> Bool]  -- ^ optional predicates
            -> (Point -> Kind.Id TileKind -> Bool)  -- ^ good to have predicate
            -> [Point -> Kind.Id TileKind -> Bool]  -- ^ worst case predicates
            -> Rnd Point
findPosTry2 numTries ltile m0 l g r = assert (numTries > 0) $
  let (x, y) = PointArray.sizeA ltile
      accomodate fallback _ [] = fallback  -- fallback needs to be non-strict
      accomodate fallback m (hd : tl) =
        let search 0 = accomodate fallback m tl
            search !k = do
              pxy <- randomR (0, (x - 1) * (y - 1))
              let tile = KindOps.Id $ ltile `PointArray.accessI` pxy
                  pos = PointArray.punindex x pxy
              if m pos tile && hd pos tile
              then return $! pos
              else search (k - 1)
        in search numTries
  in accomodate (accomodate (findPos ltile m0) m0 r)
                -- @pos@ or @tile@ not always needed, so not strict
                (\pos tile -> m0 pos tile && g pos tile)
                l

instance Binary Level where
  put Level{..} = do
    put ldepth
    put (assertSparseItems lfloor)
    put (assertSparseItems lembed)
    put (assertSparseActors lactor)
    put ltile
    put lxsize
    put lysize
    put lsmell
    put ldesc
    put lstair
    put lseen
    put lclear
    put ltime
    put lactorCoeff
    put lactorFreq
    put litemNum
    put litemFreq
    put lsecret
    put lhidden
    put lescape
  get = do
    ldepth <- get
    lfloor <- get
    lembed <- get
    lactor <- get
    ltile <- get
    lxsize <- get
    lysize <- get
    lsmell <- get
    ldesc <- get
    lstair <- get
    lseen <- get
    lclear <- get
    ltime <- get
    lactorCoeff <- get
    lactorFreq <- get
    litemNum <- get
    litemFreq <- get
    lsecret <- get
    lhidden <- get
    lescape <- get
    return $! Level{..}
