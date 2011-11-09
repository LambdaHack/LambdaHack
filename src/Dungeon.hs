module Dungeon
  ( Dungeon, LevelConfig
  , defaultLevelConfig, normalLevelBound
  , rogueRoom, bigRoom, noiseRoom
  , fromList, currentFirst, adjust, (!)
  ) where

import Control.Monad
import Data.Binary
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Ratio

import Geometry
import Area
import AreaRnd
import Loc
import Level
import Item
import Random
import Content.TileKind
import Content.ItemKind
import WorldLoc
import Tile  -- TODO: qualified
import qualified Kind

-- | The complete dungeon is a map from level names to levels.
-- We usually store all but the current level in this data structure.
newtype Dungeon = Dungeon{dungeonLevelMap :: M.Map LevelId Level}
  deriving Show

instance Binary Dungeon where
  put dng = put (M.assocs (dungeonLevelMap dng))
  get = liftM fromList get

-- | Create a dungeon from a list of levels.
fromList :: [(LevelId, Level)] -> Dungeon
fromList = Dungeon . M.fromList

-- | Association list corresponding to the dungeon.
-- Starts at the supplied level id (usually the current level)
-- to try and keep the dungeon lazy when performing searches.
currentFirst :: LevelId -> Dungeon -> [(LevelId, Level)]
currentFirst slevel (Dungeon m) =
  (slevel, m M.! slevel)
  : L.filter ((/= slevel) . fst) (M.assocs m)

adjust :: (Level -> Level) -> LevelId -> Dungeon -> Dungeon
adjust f ln (Dungeon m) = Dungeon (M.adjust f ln m)

(!) :: Dungeon -> LevelId -> Level
(!) (Dungeon m) slid = m M.! slid

type Corridor = [(X, Y)]
type Room = Area

-- | Create a random room according to given parameters.
mkRoom :: Int ->      -- ^ border columns
          (X, Y) ->    -- ^ minimum size
          Area ->     -- ^ this is an area, not the room itself
          Rnd Room    -- ^ this is the upper-left and lower-right corner of the room
mkRoom bd (xm, ym) (x0, y0, x1, y1) =
  do
    (rx0, ry0) <- xyInArea (x0 + bd, y0 + bd, x1 - bd - xm + 1, y1 - bd - ym + 1)
    (rx1, ry1) <- xyInArea (rx0 + xm - 1, ry0 + ym - 1, x1 - bd, y1 - bd)
    return (rx0, ry0, rx1, ry1)

-- | Create a no-room, i.e., a single corridor field.
mkNoRoom :: Int ->      -- ^ border columns
            Area ->     -- ^ this is an area, not the room itself
            Rnd Room    -- ^ this is the upper-left and lower-right corner of the room
mkNoRoom bd (x0, y0, x1, y1) =
  do
    (rx, ry) <- xyInArea (x0 + bd, y0 + bd, x1 - bd, y1 - bd)
    return (rx, ry, rx, ry)

type LMap = M.Map (X, Y) (Kind.Id TileKind)

digCorridors :: Corridor -> LMap
digCorridors (p1:p2:ps) =
  M.union corPos (digCorridors (p2:ps))
  where
    corXY  = fromTo p1 p2
    corPos = M.fromList $ L.zip corXY (repeat Tile.floorDarkId)
digCorridors _ = M.empty

mergeCorridor :: Kind.Id TileKind -> Kind.Id TileKind -> Kind.Id TileKind
mergeCorridor _ t | Tile.isWalkable t = t
mergeCorridor _ t | Tile.isUnknown t  = Tile.floorDarkId
mergeCorridor _ _                     = Tile.openingId

-- | Create a level consisting of only one room. Optionally, insert some walls.
buildLevel :: (LevelConfig -> Rnd LMap) -> LevelConfig -> Bool
             -> Rnd Level
buildLevel buildCave cfg@(LevelConfig{levelBound = (sx, sy)}) isLast =
  do
    lm1 <- buildCave cfg
    let lmap1 = listArrayCfg cfg lm1
    -- Roll locations of the stairs.
    su <- findLoc lmap1 (const Tile.isBoring)
    sd <- findLocTry 2000 lmap1
            (\ l t -> l /= su && Tile.isBoring t)
            (\ l _ -> distance (sx + 1) su l >= minStairsDistance cfg)
    is <- rollItems cfg lmap1 su sd
    let lm2 = M.insert (fromLoc (sx + 1) su) Tile.stairsLightUpId lm1
        lm3 = if isLast
              then lm2
              else M.insert (fromLoc (sx + 1) sd) Tile.stairsLightDownId lm2
    let lmap3 = listArrayCfg cfg lm3
        level = Level emptyParty (sx + 1) (sy + 1) emptyParty IM.empty IM.empty (IM.fromList is) lmap3 (unknownTileMap cfg) "bigroom" (su, sd)
    return level

-- | For a bigroom level: Create a level consisting of only one, empty room.
bigRoom :: LevelConfig -> Bool -> Rnd Level
bigRoom = buildLevel caveEmpty

-- | For a noiseroom level: Create a level consisting of only one room
-- with randomly distributed pillars.
noiseRoom :: LevelConfig -> Bool -> Rnd Level
noiseRoom = buildLevel caveNoise

caveEmpty :: LevelConfig -> Rnd LMap
caveEmpty LevelConfig{levelBound = (sx, sy)} =
  return $ digRoom True (1, 1, sx-1, sy-1) M.empty

caveNoise :: LevelConfig -> Rnd LMap
caveNoise cfg@(LevelConfig{levelBound = (sx, sy)}) = do
  em <- caveEmpty cfg
  nri <- 100 *~ nrItems cfg
  lxy <- replicateM nri $ xyInArea (1, 1, sx - 1, sy - 1)
  let insertRock lm xy = M.insert xy Tile.wallId lm
  return $ L.foldl' insertRock em lxy

-- | If the room has size 1, it is at most a start of a corridor.
digRoom :: Bool -> Room -> LMap -> LMap
digRoom dl (x0, y0, x1, y1) lmap
  | x0 == x1 && y0 == y1 = lmap
  | otherwise =
  let floorDL = if dl then Tile.floorLightId else Tile.floorDarkId
      rm =
        [ ((x, y), floorDL) | x <- [x0..x1], y <- [y0..y1] ]
        ++ [ ((x, y), Tile.wallId)
           | x <- [x0-1, x1+1], y <- [y0..y1] ]
        ++ [ ((x, y), Tile.wallId)
           | x <- [x0-1..x1+1], y <- [y0-1, y1+1] ]
  in M.union (M.fromList rm) lmap

data LevelConfig = LevelConfig
  { levelGrid         :: Rnd (X, Y)
  , minRoomSize       :: Rnd (X ,Y)
  , darkRoomChance    :: Rnd Bool
  , border            :: Int         -- must be at least 2!
  , levelBound        :: (X, Y)      -- lower right point; TODO: change to size or rename to 'bound'
  , extraConnects     :: (X, Y) -> Int
      -- relative to grid (in fact a range, because of duplicate connects)
  , noRooms           :: (X, Y) -> Rnd Int
      -- range, relative to grid
  , minStairsDistance :: Int         -- must not be too large
  , doorChance        :: Rnd Bool
  , doorOpenChance    :: Rnd Bool
  , doorSecretChance  :: Rnd Bool
  , doorSecretMax     :: Int
  , nrItems           :: Rnd Int     -- range
  , depth             :: Int         -- general indicator of difficulty
  }

normalLevelBound :: (X, Y)
normalLevelBound = (79, 22)

defaultLevelConfig :: Int -> LevelConfig
defaultLevelConfig d =
  LevelConfig {
    levelGrid         = do
                          x <- randomR (3, 5)
                          y <- randomR (2, 4)
                          return (x, y),
    minRoomSize       = return (2, 2),
    darkRoomChance    = chance $ 1%((22 - (2 * fromIntegral d)) `max` 2),
    border            = 2,
    levelBound        = normalLevelBound,
    extraConnects     = \ (x, y) -> (x * y) `div` 3,
    noRooms           = \ (x, y) -> randomR (0, (x * y) `div` 3),
    minStairsDistance = 30,
    doorChance        = chance $ 2%3,
    doorOpenChance    = chance $ 1%10,
    doorSecretChance  = chance $ 1%4,
    doorSecretMax     = 15,
    nrItems           = randomR (5, 10),
    depth             = d
  }

_largeLevelConfig :: Int -> LevelConfig
_largeLevelConfig d =
  (defaultLevelConfig d) {
    levelGrid         = return (10, 7),
    levelBound        = (231, 77),
    extraConnects     = const 10
  }

-- | Create a "normal" dungeon level. Takes a configuration in order
-- to tweak all sorts of data.

{-
Level is generated by an algorithm inspired by the original Rogue,
as follows:

  * The available area is divided into a 3 by 3 grid
    where each of the 9 grid cells has approximately the same size.

  * In each of the 9 grid cells one room is placed at a random location.
    The minimum size of a room is 2 by 2 floor tiles. A room is surrounded
    by walls, and the walls still have to fit into the assigned grid cells.

  * Rooms that are on horizontally or vertically adjacent grid cells
    may be connected by a corridor. Corridors consist of 3 segments of straight
    lines (either "horizontal, vertical, horizontal" or "vertical, horizontal,
    vertical"). They end in openings in the walls of the room they connect.
    It is possible that one or two of the 3 segments have length 0, such that
    the resulting corridor is L-shaped or even a single straight line.

  * Corridors are generated randomly in such a way that at least every room
    on the grid is connected, and a few more might be. It is not sufficient
    to always connect all adjacent rooms.

  * Stairs up and down are placed. Stairs are always located in two different
    randomly chosen rooms.
-}

rogueRoom :: LevelConfig -> Bool -> Rnd Level
rogueRoom cfg@(LevelConfig {levelBound}) isLast =
  do
    lgrid    <- levelGrid cfg
    lminroom <- minRoomSize cfg
    let (sx, sy) = levelBound
        gs = grid lgrid (0, 0, sx, sy)
    -- grid locations of "no-rooms"
    nrnr <- noRooms cfg lgrid
    nr   <- replicateM nrnr (do
                               let (x, y) = lgrid
                               xg <- randomR (0, x-1)
                               yg <- randomR (0, y-1)
                               return (xg, yg))
    rs0 <- mapM (\ (i, r) -> do
                              r' <- if i `elem` nr
                                      then mkNoRoom (border cfg) r
                                      else mkRoom (border cfg) lminroom r
                              return (i, r')) gs
    let rooms :: [Area]
        rooms = L.map snd rs0
    dlrooms <- mapM (\ r -> darkRoomChance cfg >>= \ c -> return (r, not c)) rooms :: Rnd [(Area, Bool)]
    let rs = M.fromList rs0
    connects <- connectGrid lgrid
    addedConnects <- replicateM (extraConnects cfg lgrid) (randomConnection lgrid)
    let allConnects = L.nub (addedConnects ++ connects)
    cs <- mapM
           (\ (p0, p1) -> do
                           let r0 = rs M.! p0
                               r1 = rs M.! p1
                           connectRooms r0 r1) allConnects
    let lrooms = L.foldr (\ (r, dl) m -> digRoom dl r m) M.empty dlrooms
        lcorridors = M.unions (L.map digCorridors cs)
        lrocks = emptyLMap levelBound
        lm = M.union (M.unionWith mergeCorridor lcorridors lrooms) lrocks
    -- convert openings into doors
    (dlmap, secretMap) <- do
      let f (l, le) o@((x, y), t) =
                  case t of
                    _ | Tile.isOpening t ->
                      do
                        -- openings have a certain chance to be doors;
                        -- doors have a certain chance to be open; and
                        -- closed doors have a certain chance to be
                        -- secret
                        rb <- doorChance cfg
                        ro <- doorOpenChance cfg
                        if not rb
                          then return (o : l, le)
                          else if ro
                               then return (((x, y), Tile.doorOpenId) : l, le)
                               else do
                                 rsc <- doorSecretChance cfg
                                 if not rsc
                                   then return (((x, y), Tile.doorClosedId) : l, le)
                                   else do
                                     rs1 <- randomR (doorSecretMax cfg `div` 2,
                                                     doorSecretMax cfg)
                                     return (((x, y), Tile.doorSecretId) : l, IM.insert (toLoc (sx + 1) (x, y)) (SecretStrength rs1) le)
                    _ -> return (o : l, le)
      (l, le) <- foldM f ([], IM.empty) (M.toList lm)
      return (M.fromList l, le)
    let unknown = unknownTileMap cfg
        lmapd = listArrayCfg cfg dlmap
    -- locations of the stairs
    su <- findLoc lmapd (const Tile.isBoring)
    sd <- findLocTry 2000 lmapd
            (const Tile.isBoring)
            (\ l _ -> distance (sx + 1) su l >= minStairsDistance cfg)
    -- determine number of items, items and locations for the items
    is <- rollItems cfg lmapd su sd
    let lm2 = if isLast
              then dlmap
              else M.update (\ t ->
                              Just $ if isLit t
                                     then Tile.stairsLightDownId
                                     else Tile.stairsDarkDownId)
                   (fromLoc (sx + 1) sd) dlmap
        lm3 = M.update (\ t ->
                         Just $ if isLit t
                                then Tile.stairsLightUpId
                                else Tile.stairsDarkUpId)
              (fromLoc (sx + 1) su) lm2
        -- generate map and level from the data
        meta = show allConnects
        lmap3 = listArrayCfg cfg lm3
    return $
      Level emptyParty (sx + 1) (sy + 1) emptyParty IM.empty secretMap (IM.fromList is) lmap3 unknown meta (su, sd)

rollItems :: LevelConfig -> TileMap -> Loc -> Loc
             -> Rnd [(Loc, ([Item], [Item]))]
rollItems cfg@LevelConfig{levelBound = (sx, _)} lmap ploc sd =
  do
    nri <- nrItems cfg
    replicateM nri $
      do
        item <- newItem (depth cfg)
        l <- case jname (Kind.getKind (ikind item)) of
               "sword" ->
                 -- swords generated close to monsters; MUAHAHAHA
                 findLocTry 2000 lmap
                   (\ l t -> l /= ploc && l /= sd && Tile.isBoring t)
                   (\ l _ -> distance (sx + 1) ploc l > 30)
               _ -> findLoc lmap
                      (\ l t -> l /= ploc && l /= sd && Tile.isBoring t)
        return (l,([item], []))

emptyLMap :: (X, Y) -> LMap
emptyLMap (mx, my) =
  M.fromList [ ((x, y), Tile.wallId) | x <- [0..mx], y <- [0..my] ]

listArrayCfg :: LevelConfig -> LMap -> TileMap
listArrayCfg cfg@LevelConfig{levelBound = (sx, _)} lmap =
  Kind.listArray (zeroLoc, toLoc (sx + 1) (levelBound cfg))
    (M.elems $ M.mapKeys (\ (x, y) -> (y, x)) lmap)

unknownTileMap :: LevelConfig -> TileMap
unknownTileMap cfg@LevelConfig{levelBound = (sx, _)} =
  Kind.listArray (zeroLoc, toLoc (sx + 1) (levelBound cfg))
    (repeat Tile.unknownId)
