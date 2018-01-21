-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Common.Level
  ( -- * Dungeon
    LevelId, AbsDepth, Dungeon
  , ascendInBranch, whereTo
    -- * The @Level@ type and its components
  , ItemFloor, ActorMap, TileMap, SmellMap, Level(..)
    -- * Component updates
  , updateFloor, updateEmbed, updateActorMap, updateTile, updateSmell
    -- * Level query
  , at, findPoint, findPos, findPosTry, findPosTry2
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
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.CaveKind (CaveKind)
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import           Game.LambdaHack.Content.TileKind (TileKind)

-- | The complete dungeon is a map from level identifiers to levels.
type Dungeon = EM.EnumMap LevelId Level

-- | Levels in the current branch, one level up (or down) from the current.
ascendInBranch :: Dungeon -> Bool -> LevelId -> [LevelId]
ascendInBranch dungeon up lid =
  -- Currently there is just one branch, so the computation is simple.
  let (minD, maxD) =
        case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> error $ "null dungeon" `showFailure` dungeon
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
type TileMap = PointArray.GArray Word16 (ContentId TileKind)

-- | Current smell on map tiles.
type SmellMap = EM.EnumMap Point Time

-- | A view on single, inhabited dungeon level. "Remembered" fields
-- carry a subset of the info in the client copies of levels.
data Level = Level
  { lkind       :: ContentId CaveKind
                              -- ^ the kind of cave the level is an instance of
  , ldepth      :: AbsDepth   -- ^ absolute depth of the level
  , lfloor      :: ItemFloor  -- ^ remembered items lying on the floor
  , lembed      :: ItemFloor  -- ^ remembered items embedded in the tile
  , lactor      :: ActorMap   -- ^ seen actors at positions on the level
  , ltile       :: TileMap    -- ^ remembered level map
  , lxsize      :: X          -- ^ width of the level
  , lysize      :: Y          -- ^ height of the level
  , lsmell      :: SmellMap   -- ^ remembered smells on the level
  , lstair      :: ([Point], [Point])
                              -- ^ positions of (up, down) stairs
  , lescape     :: [Point]    -- ^ positions of IK.Escape tiles
  , lseen       :: Int        -- ^ currently remembered clear tiles
  , lexplorable :: Int        -- ^ total number of explorable tiles
  , ltime       :: Time       -- ^ local time on the level (possibly frozen)
  , lactorCoeff :: Int        -- ^ the lower, the more monsters spawn
  , lactorFreq  :: Freqs ItemKind
                              -- ^ frequency of spawned actors; [] for clients
  , litemNum    :: Int        -- ^ number of initial items, 0 for clients
  , litemFreq   :: Freqs ItemKind
                              -- ^ frequency of initial items; [] for clients
  , lnight      :: Bool       -- ^ whether the level is covered in darkness
  , lname       :: Text       -- ^ level name
  , ldesc       :: Text       -- ^ level description
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
findPos :: TileMap -> (Point -> ContentId TileKind -> Bool) -> Rnd Point
findPos ltile p =
  let (x, y) = PointArray.sizeA ltile
      search = do
        pxy <- randomR (0, (x - 1) * (y - 1))
        let tile = ContentId $ ltile `PointArray.accessI` pxy
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
           -> (Point -> ContentId TileKind -> Bool)  -- ^ mandatory predicate
           -> [Point -> ContentId TileKind -> Bool]  -- ^ optional predicates
           -> Rnd Point
{-# INLINE findPosTry #-}
findPosTry numTries ltile m = findPosTry2 numTries ltile m [] undefined

findPosTry2 :: Int                                  -- ^ the number of tries
            -> TileMap                              -- ^ look up in this map
            -> (Point -> ContentId TileKind -> Bool)  -- ^ mandatory predicate
            -> [Point -> ContentId TileKind -> Bool]  -- ^ optional predicates
            -> (Point -> ContentId TileKind -> Bool)  -- ^ good to have pred.
            -> [Point -> ContentId TileKind -> Bool]  -- ^ worst case predicates
            -> Rnd Point
findPosTry2 numTries ltile m0 l g r = assert (numTries > 0) $
  let (x, y) = PointArray.sizeA ltile
      accomodate fallback _ [] = fallback  -- fallback needs to be non-strict
      accomodate fallback m (hd : tl) =
        let search 0 = accomodate fallback m tl
            search !k = do
              pxy <- randomR (0, (x - 1) * (y - 1))
              let tile = ContentId $ ltile `PointArray.accessI` pxy
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
    put lkind
    put ldepth
    put (assertSparseItems lfloor)
    put (assertSparseItems lembed)
    put (assertSparseActors lactor)
    put ltile
    put lxsize
    put lysize
    put lsmell
    put lstair
    put lescape
    put lseen
    put lexplorable
    put ltime
    put lactorCoeff
    put lactorFreq
    put litemNum
    put litemFreq
    put lnight
    put lname
    put ldesc
  get = do
    lkind <- get
    ldepth <- get
    lfloor <- get
    lembed <- get
    lactor <- get
    ltile <- get
    lxsize <- get
    lysize <- get
    lsmell <- get
    lstair <- get
    lescape <- get
    lseen <- get
    lexplorable <- get
    ltime <- get
    lactorCoeff <- get
    lactorFreq <- get
    litemNum <- get
    litemFreq <- get
    lnight <- get
    lname <- get
    ldesc <- get
    return $! Level{..}
