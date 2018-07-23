{-# LANGUAGE TypeFamilies #-}
-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Common.Level
  ( -- * Dungeon
    LevelId, Dungeon
  , dungeonBounds, ascendInBranch, whereTo
    -- * The @Level@ type and its components
  , ItemFloor, ActorMap, TileMap, SmellMap, Level(..)
    -- * Component updates
  , updateFloor, updateEmbed, updateActorMap, updateTile, updateSmell
    -- * Level query
  , at, findPos, findPosTry, findPosTry2
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , assertSparseItems, assertSparseActors
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.ContentData
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.CaveKind (CaveKind)
import           Game.LambdaHack.Content.PlaceKind (PlaceKind)
import           Game.LambdaHack.Content.TileKind (TileKind)

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
whereTo :: LevelId    -- ^ level of the stairs
        -> Point      -- ^ position of the stairs
        -> Maybe Bool -- ^ optional forced direction
        -> Dungeon    -- ^ current game dungeon
        -> (LevelId, Point)
                      -- ^ destination level and the pos of its receiving stairs
whereTo lid pos mup dungeon =
  let lvl = dungeon EM.! lid
      (up, i) = case elemIndex pos $ fst $ lstair lvl of
        Just ifst -> (True, ifst)
        Nothing -> case elemIndex pos $ snd $ lstair lvl of
          Just isnd -> (False, isnd)
          Nothing -> case mup of
            Just forcedUp -> (forcedUp, 0)  -- for ascending via, e.g., spells
            Nothing -> error $ "no stairs at" `showFailure` (lid, pos)
      !_A = assert (maybe True (== up) mup) ()
  in case ascendInBranch dungeon up lid of
    [] | isJust mup -> (lid, pos)  -- spell fizzles
    [] -> error $ "no dungeon level to go to" `showFailure` (lid, pos)
    ln : _ -> let lvlDest = dungeon EM.! ln
                  stairsDest = (if up then snd else fst) (lstair lvlDest)
              in if length stairsDest < i + 1
                 then error $ "no stairs at index" `showFailure` (lid, pos)
                 else (ln, stairsDest !! i)

-- | Items located on map tiles.
type ItemFloor = EM.EnumMap Point ItemBag

-- | Items located on map tiles.
type ActorMap = EM.EnumMap Point [ActorId]

-- | Tile kinds on the map.
type TileMap = PointArray.Array (ContentId TileKind)

-- | Current smell on map tiles.
type SmellMap = EM.EnumMap Point Time

-- | A view on single, inhabited dungeon level. "Remembered" fields
-- carry a subset of the info in the client copies of levels.
data Level = Level
  { lkind   :: ContentId CaveKind
                          -- ^ the kind of cave the level is an instance of
  , ldepth  :: Dice.AbsDepth
                          -- ^ absolute depth of the level
  , lfloor  :: ItemFloor  -- ^ remembered items lying on the floor
  , lembed  :: ItemFloor  -- ^ remembered items embedded in the tile
  , lactor  :: ActorMap   -- ^ seen actors at positions on the level;
                          --   could be recomputed at resume, but small enough
  , ltile   :: TileMap    -- ^ remembered level map
  , lentry  :: EM.EnumMap Point (ContentId PlaceKind)
                          -- ^ room entrances on the level
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

assertSparseActors :: ActorMap -> ActorMap
assertSparseActors m =
  assert (EM.null (EM.filter null m)
          `blame` "null actor lists found" `swith` m) m

updateFloor :: (ItemFloor -> ItemFloor) -> Level -> Level
updateFloor f lvl = lvl {lfloor = f (lfloor lvl)}

updateEmbed :: (ItemFloor -> ItemFloor) -> Level -> Level
updateEmbed f lvl = lvl {lembed = f (lembed lvl)}

updateActorMap :: (ActorMap -> ActorMap) -> Level -> Level
updateActorMap f lvl = lvl {lactor = f (lactor lvl)}

updateTile :: (TileMap -> TileMap) -> Level -> Level
updateTile f lvl = lvl {ltile = f (ltile lvl)}

updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl {lsmell = f (lsmell lvl)}

-- | Query for tile kinds on the map.
at :: Level -> Point -> ContentId TileKind
{-# INLINE at #-}
at Level{ltile} p = ltile PointArray.! p

-- | Find a random position on the map satisfying a predicate.
findPos :: Level -> (Point -> ContentId TileKind -> Bool) -> Rnd Point
findPos Level{ltile, larea} p =
  let (Point x0 y0, xspan, yspan) = spanArea larea
      search = do
        pxyRelative <- randomR (0, xspan * yspan - 1)
        let Point{..} = PointArray.punindex xspan pxyRelative
            pos = Point (x0 + px) (y0 + py)
            tile = ltile PointArray.! pos
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
           -> Level                                -- ^ look up in this level
           -> (Point -> ContentId TileKind -> Bool)  -- ^ mandatory predicate
           -> [Point -> ContentId TileKind -> Bool]  -- ^ optional predicates
           -> Rnd Point
{-# INLINE findPosTry #-}
findPosTry numTries lvl m = findPosTry2 numTries lvl m [] undefined

findPosTry2 :: Int                                  -- ^ the number of tries
            -> Level                                -- ^ look up in this level
            -> (Point -> ContentId TileKind -> Bool)  -- ^ mandatory predicate
            -> [Point -> ContentId TileKind -> Bool]  -- ^ optional predicates
            -> (Point -> ContentId TileKind -> Bool)  -- ^ good to have pred.
            -> [Point -> ContentId TileKind -> Bool]  -- ^ worst case predicates
            -> Rnd Point
findPosTry2 numTries lvl@Level{ltile, larea} m0 l g r = assert (numTries > 0) $
  let (Point x0 y0, xspan, yspan) = spanArea larea
      accomodate fallback _ [] = fallback  -- fallback needs to be non-strict
      accomodate fallback m (hd : tl) =
        let search 0 = accomodate fallback m tl
            search !k = do
              pxyRelative <- randomR (0, xspan * yspan - 1)
              let Point{..} = PointArray.punindex xspan pxyRelative
                  pos = Point (x0 + px) (y0 + py)
                  tile = ltile PointArray.! pos
              if m pos tile && hd pos tile
              then return $! pos
              else search (k - 1)
        in search numTries
  in accomodate (accomodate (findPos lvl m0) m0 r)
                -- @pos@ or @tile@ not always needed, so not strict
                (\pos tile -> m0 pos tile && g pos tile)
                l

instance Binary Level where
  put Level{..} = do
    put lkind
    put ldepth
    put (assertSparseItems lfloor)
    put (assertSparseItems lembed)
    put (assertSparseActors lactor)
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
    lactor <- get
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
