{-# LANGUAGE TypeFamilies #-}
-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Common.Level
  ( -- * Dungeon
    Dungeon, dungeonBounds, ascendInBranch, whereTo
    -- * The @Level@ type and its components
  , ItemFloor, BigActorMap, ProjectileMap, TileMap, SmellMap, Level(..)
    -- * Component updates
  , updateFloor, updateEmbed, updateBigMap, updateProjMap
  , updateTile, updateEntry, updateSmell
    -- * Level query
  , at
  , posToBigLvl, occupiedBigLvl, posToProjsLvl, occupiedProjLvl, posToAidsLvl
  , findPosTry, findPosTry2, nearbyFreePoints
    -- * Misc
  , sortEmbeds
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , EntryMap
  , nearbyPassablePoints, assertSparseItems, assertSparseProjectiles
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.CaveKind (CaveKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.PlaceKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Definition.Defs

-- | The complete dungeon is a map from level identifiers to levels.
type Dungeon = EM.EnumMap LevelId Level

dungeonBounds :: Dungeon -> (LevelId, LevelId)
dungeonBounds dungeon
  | Just ((s, _), _) <- EM.minViewWithKey dungeon
  , Just ((e, _), _) <- EM.maxViewWithKey dungeon
  = (s, e)
dungeonBounds dungeon = error $ "empty dungeon" `showFailure` dungeon

-- | Levels in the current branch, one level up (or down) from the current.
ascendInBranch :: Dungeon -> Bool -> LevelId -> [LevelId]
ascendInBranch dungeon up lid =
  -- Currently there is just one branch, so the computation is simple.
  let (minD, maxD) = dungeonBounds dungeon
      ln = max minD $ min maxD $ toEnum $ fromEnum lid + if up then 1 else -1
  in case EM.lookup ln dungeon of
    Just _ | ln /= lid -> [ln]
    _ | ln == lid -> []
    _ -> ascendInBranch dungeon up ln  -- jump over gaps

-- | Compute the level identifier and stair position on the new level,
-- after a level change.
--
-- We assume there is never a staircase up and down at the same position.
whereTo :: LevelId             -- ^ level of the stairs
        -> Point               -- ^ position of the stairs
        -> Bool                -- ^ optional forced direction
        -> Dungeon             -- ^ current game dungeon
        -> [(LevelId, Point)]  -- ^ possible destinations
whereTo lid pos up dungeon =
  let lvl = dungeon EM.! lid
      li = case elemIndex pos $ fst $ lstair lvl of
        Just ifst -> assert up [ifst]
        Nothing -> case elemIndex pos $ snd $ lstair lvl of
          Just isnd -> assert (not up) [isnd]
          Nothing ->
            let forcedPoss = (if up then fst else snd) (lstair lvl)
            in [0 .. length forcedPoss - 1]  -- for ascending via, e.g., spells
  in case ascendInBranch dungeon up lid of
    [] -> []  -- spell fizzles
    ln : _ -> let lvlDest = dungeon EM.! ln
                  stairsDest = (if up then snd else fst) (lstair lvlDest)
                  posAtIndex i = case drop i stairsDest of
                    [] -> error $ "not enough stairs:" `showFailure` (ln, i + 1)
                    p : _ -> (ln, p)
              in map posAtIndex li

-- | Items located on map tiles.
type ItemFloor = EM.EnumMap Point ItemBag

-- | Big actors located on map tiles.
type BigActorMap = EM.EnumMap Point ActorId

-- | Collections of projectiles located on map tiles.
type ProjectileMap = EM.EnumMap Point [ActorId]

-- | Tile kinds on the map.
type TileMap = PointArray.Array (ContentId TileKind)

-- | Current smell on map tiles.
type SmellMap = EM.EnumMap Point Time

-- | Entries of places on the map.
type EntryMap = EM.EnumMap Point PlaceEntry

-- | A view on single, inhabited dungeon level. "Remembered" fields
-- carry a subset of the info in the client copies of levels.
data Level = Level
  { lkind   :: ContentId CaveKind
                          -- ^ the kind of cave the level is an instance of
  , ldepth  :: Dice.AbsDepth
                          -- ^ absolute depth of the level
  , lfloor  :: ItemFloor  -- ^ remembered items lying on the floor
  , lembed  :: ItemFloor  -- ^ remembered items embedded in the tile
  , lbig    :: BigActorMap
                          -- ^ seen big (non-projectile) actors at positions
                          --   on the level;
                          --   could be recomputed at resume, but small enough
  , lproj   :: ProjectileMap
                          -- ^ seen projectiles at positions on the level;
                          --   could be recomputed at resume
  , ltile   :: TileMap    -- ^ remembered level map
  , lentry  :: EntryMap   -- ^ room entrances on the level
  , larea   :: Area       -- ^ area of the level
  , lsmell  :: SmellMap   -- ^ remembered smells on the level
  , lstair  :: ([Point], [Point])
                          -- ^ positions of (up, down) stairs
  , lescape :: [Point]    -- ^ positions of IK.Escape tiles
  , lseen   :: Int        -- ^ currently remembered clear tiles
  , lexpl   :: Int        -- ^ total number of explorable tiles
  , ltime   :: Time       -- ^ local time on the level (possibly frozen)
  , lnight  :: Bool       -- ^ whether the level is covered in darkness
  }
  deriving (Show, Eq)

assertSparseItems :: ItemFloor -> ItemFloor
assertSparseItems m =
  assert (EM.null (EM.filter EM.null m)
          `blame` "null floors found" `swith` m) m

assertSparseProjectiles :: ProjectileMap -> ProjectileMap
assertSparseProjectiles m =
  assert (EM.null (EM.filter null m)
          `blame` "null projectile lists found" `swith` m) m

updateFloor :: (ItemFloor -> ItemFloor) -> Level -> Level
updateFloor f lvl = lvl {lfloor = f (lfloor lvl)}

updateEmbed :: (ItemFloor -> ItemFloor) -> Level -> Level
updateEmbed f lvl = lvl {lembed = f (lembed lvl)}

updateBigMap :: (BigActorMap -> BigActorMap) -> Level -> Level
updateBigMap f lvl = lvl {lbig = f (lbig lvl)}

updateProjMap :: (ProjectileMap -> ProjectileMap) -> Level -> Level
updateProjMap f lvl = lvl {lproj = f (lproj lvl)}

updateTile :: (TileMap -> TileMap) -> Level -> Level
updateTile f lvl = lvl {ltile = f (ltile lvl)}

updateEntry :: (EntryMap -> EntryMap) -> Level -> Level
updateEntry f lvl = lvl {lentry = f (lentry lvl)}

updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl {lsmell = f (lsmell lvl)}

-- | Query for tile kinds on the map.
at :: Level -> Point -> ContentId TileKind
{-# INLINE at #-}
at Level{ltile} p = ltile PointArray.! p

posToBigLvl :: Point -> Level -> Maybe ActorId
{-# INLINE posToBigLvl #-}
posToBigLvl pos lvl = EM.lookup pos $ lbig lvl

occupiedBigLvl :: Point -> Level -> Bool
{-# INLINE occupiedBigLvl #-}
occupiedBigLvl pos lvl = pos `EM.member` lbig lvl

posToProjsLvl :: Point -> Level -> [ActorId]
{-# INLINE posToProjsLvl #-}
posToProjsLvl pos lvl = EM.findWithDefault [] pos $ lproj lvl

occupiedProjLvl :: Point -> Level -> Bool
{-# INLINE occupiedProjLvl #-}
occupiedProjLvl pos lvl = pos `EM.member` lproj lvl

posToAidsLvl :: Point -> Level -> [ActorId]
{-# INLINE posToAidsLvl #-}
posToAidsLvl pos lvl = maybeToList (posToBigLvl pos lvl)
                       ++ posToProjsLvl pos lvl

-- | Try to find a random position on the map satisfying
-- conjunction of the mandatory and an optional predicate.
-- If the permitted number of attempts is not enough,
-- try again the same number of times without the next optional predicate,
-- and fall back to trying with only the mandatory predicate.
findPosTry :: Int                                    -- ^ the number of tries
           -> Level                                  -- ^ look up in this level
           -> (Point -> ContentId TileKind -> Bool)  -- ^ mandatory predicate
           -> [Point -> ContentId TileKind -> Bool]  -- ^ optional predicates
           -> Rnd (Maybe Point)
{-# INLINE findPosTry #-}
findPosTry numTries lvl m = findPosTry2 numTries lvl m [] undefined

findPosTry2 :: Int                                    -- ^ the number of tries
            -> Level                                  -- ^ look up in this level
            -> (Point -> ContentId TileKind -> Bool)  -- ^ mandatory predicate
            -> [Point -> ContentId TileKind -> Bool]  -- ^ optional predicates
            -> (Point -> ContentId TileKind -> Bool)  -- ^ good to have pred.
            -> [Point -> ContentId TileKind -> Bool]  -- ^ worst case predicates
            -> Rnd (Maybe Point)
{-# INLINE findPosTry2 #-}
findPosTry2 numTries Level{ltile, larea} m0 l g r =
  assert (numTries > 0) $
  let (Point x0 y0, xspan, yspan) = spanArea larea
      accomodate :: Rnd (Maybe Point)
                 -> (Point -> ContentId TileKind -> Bool)
                 -> [Point -> ContentId TileKind -> Bool]
                 -> Rnd (Maybe Point)
      {-# INLINE accomodate #-}
      accomodate fallback m = go
       where
        go :: [Point -> ContentId TileKind -> Bool]
           -> Rnd (Maybe Point)
        go [] = fallback
        go (hd : tl) = search numTries
         where
          search 0 = go tl
          search !k = do
            pxyRelative <- randomR0 (xspan * yspan - 1)
            -- Here we can't use @fromEnum@ and/or work with the @Int@
            -- representation, because the span is different than @rXmax@.
            let Point{..} = punindex xspan pxyRelative
                pos = Point (x0 + px) (y0 + py)
                tile = ltile PointArray.! pos
            if m pos tile && hd pos tile
            then return $ Just pos
            else search (k - 1)
      rAndOnceOnlym0 = r ++ [\_ _ -> True]
  in accomodate (accomodate (return Nothing) m0 rAndOnceOnlym0)
                -- @pos@ and @tile@ not always needed, so not strict;
                -- the function arguments determine that thanks to inlining.
                (\pos tile -> m0 pos tile && g pos tile)
                l

-- | Generate a list of all passable points on (connected component of)
-- the level in the order of path distance from the starting position (BFS).
-- The starting position needn't be passable and is always included.
nearbyPassablePoints :: COps -> Level -> Point -> [Point]
nearbyPassablePoints cops@COps{corule=RuleContent{rXmax, rYmax}} lvl start =
  let passable p = Tile.isEasyOpen (coTileSpeedup cops) (lvl `at` p)
      passableVic p = filter passable $ vicinityBounded rXmax rYmax p
      siftSingle :: Point
                 -> (ES.EnumSet Point, [Point])
                 -> (ES.EnumSet Point, [Point])
      siftSingle current (seen, sameDistance) =
        if current `ES.member` seen
        then (seen, sameDistance)
        else (ES.insert current seen, current : sameDistance)
      siftVicinity :: Point
                   -> (ES.EnumSet Point, [Point])
                   -> (ES.EnumSet Point, [Point])
      siftVicinity current seenAndSameDistance =
        let vic = passableVic current
        in foldr siftSingle seenAndSameDistance vic
      siftNearby :: (ES.EnumSet Point, [Point]) -> [Point]
      siftNearby (seen, sameDistance) =
        sameDistance
        ++ case foldr siftVicinity (seen, []) sameDistance of
             (_, []) -> []
             (seen2, sameDistance2) -> siftNearby (seen2, sameDistance2)
  in siftNearby (ES.singleton start, [start])

nearbyFreePoints :: COps -> Level -> (ContentId TileKind -> Bool) -> Point
                 -> [Point]
nearbyFreePoints cops lvl f start =
  let good p = f (lvl `at` p)
               && Tile.isWalkable (coTileSpeedup cops) (lvl `at` p)
               && null (posToAidsLvl p lvl)
  in filter good $ nearbyPassablePoints cops lvl start

-- We ignore stray embeds, not mentioned in the tile kind.
-- OTOH, some of those mentioned may be used up and so not in the bag
-- and it's OK.
sortEmbeds :: COps -> ContentId TileKind -> [(IK.ItemKind, (ItemId, ItemQuant))]
           -> [(ItemId, ItemQuant)]
sortEmbeds COps{cotile} tk embedKindList =
  let grpList = Tile.embeddedItems cotile tk
      -- Greater or equal 0 to also cover template UNKNOWN items
      -- not yet identified by the client.
      f grp (itemKind, _) = fromMaybe 0 (lookup grp $ IK.ifreq itemKind) >= 0
  in map snd $ mapMaybe (\grp -> find (f grp) embedKindList) grpList

instance Binary Level where
  put Level{..} = do
    put lkind
    put ldepth
    put (assertSparseItems lfloor)
    put (assertSparseItems lembed)
    put lbig
    put (assertSparseProjectiles lproj)
    put ltile
    put lentry
    put larea
    put lsmell
    put lstair
    put lescape
    put lseen
    put lexpl
    put ltime
    put lnight
  get = do
    lkind <- get
    ldepth <- get
    lfloor <- get
    lembed <- get
    lbig <- get
    lproj <- get
    ltile <- get
    lentry <- get
    larea <- get
    lsmell <- get
    lstair <- get
    lescape <- get
    lseen <- get
    lexpl <- get
    ltime <- get
    lnight <- get
    return $! Level{..}
