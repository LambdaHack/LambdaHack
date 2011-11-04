module Dungeon
  (Dungeon, LevelConfig,
   defaultLevelConfig, normalLevelSize,
   rogueRoom, bigRoom, noiseRoom,
   putDungeonLevel, getDungeonLevel, fromList, toList, adjust
   )
  where

import Control.Monad
import qualified System.Random as R

import Data.Binary
import qualified Data.Map as M
import qualified Data.List as L
import Data.Ratio

import Geometry
import GeometryRnd
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
newtype Dungeon = Dungeon (M.Map LevelId Level)
  deriving Show

instance Binary Dungeon where
  put dng = put (toList dng)
  get = liftM fromList get

-- | Create a dungeon from a list of levels.
fromList :: [(LevelId, Level)] -> Dungeon
fromList = Dungeon . M.fromList

toList :: Dungeon -> [(LevelId, Level)]
toList (Dungeon m) = M.assocs m

adjust :: (Level -> Level) -> LevelId -> Dungeon -> Dungeon
adjust f ln (Dungeon m) = Dungeon (M.adjust f ln m)

-- | Extract a level from a dungeon.
getDungeonLevel :: LevelId -> Dungeon -> (Level, Dungeon)
getDungeonLevel ln (Dungeon dng) = (dng M.! ln, Dungeon (M.delete ln dng))

-- | Put a level into a dungeon.
putDungeonLevel :: Level -> Dungeon -> Dungeon
putDungeonLevel lvl (Dungeon dng) = Dungeon (M.insert (lname lvl) lvl dng)

type Corridor = [Loc]
type Room = Area

-- | Create a random room according to given parameters.
mkRoom :: Int ->      -- ^ border columns
          (Y,X) ->    -- ^ minimum size
          Area ->     -- ^ this is an area, not the room itself
          Rnd Room    -- ^ this is the upper-left and lower-right corner of the room
mkRoom bd (ym,xm) (x0, y0, x1, y1) =
  do
    (ry0,rx0) <- locInArea (x0 + bd, y0 + bd, x1 - bd - xm + 1, y1 - bd - ym + 1)
    (ry1,rx1) <- locInArea (rx0 + xm - 1, ry0 + ym - 1, x1 - bd, y1 - bd)
    return (rx0, ry0, rx1, ry1)

-- | Create a no-room, i.e., a single corridor field.
mkNoRoom :: Int ->      -- ^ border columns
            Area ->     -- ^ this is an area, not the room itself
            Rnd Room    -- ^ this is the upper-left and lower-right corner of the room
mkNoRoom bd (x0, y0, x1, y1) =
  do
    (ry,rx) <- locInArea (x0 + bd, y0 + bd, x1 - bd, y1 - bd)
    return (rx, ry, rx, ry)

data HV = Horiz | Vert
  deriving (Eq, Show, Bounded)

fromHV :: HV -> Bool
fromHV Horiz = True
fromHV Vert  = False

toHV :: Bool -> HV
toHV True  = Horiz
toHV False = Vert

instance R.Random HV where
  randomR (a,b0) g = case R.randomR (fromHV a, fromHV b0) g of
                      (b, g') -> (toHV b, g')
  random = R.randomR (minBound, maxBound)

type LMap = M.Map Loc (Kind.Id TileKind)

-- | Create a corridor, either horizontal or vertical, with
-- a possible intermediate part that is in the opposite direction.
mkCorridor :: HV -> (Loc,Loc) -> Area -> Rnd [Loc] {- straight sections of the corridor -}
mkCorridor hv ((y0,x0),(y1,x1)) b =
  do
    (ry,rx) <- findLocInArea b (const True)
    -- (ry,rx) is intermediate point the path crosses
    -- hv decides whether we start in horizontal or vertical direction
    case hv of
      Horiz -> return [(y0,x0),(y0,rx),(y1,rx),(y1,x1)]
      Vert  -> return [(y0,x0),(ry,x0),(ry,x1),(y1,x1)]

-- | Try to connect two rooms with a corridor.
-- The condition passed to mkCorridor is tricky; there might not always
-- exist a suitable intermediate point if the rooms are allowed to be close
-- together ...
connectRooms :: Area -> Area -> Rnd [Loc]
connectRooms sa@(_, _, sx1, sy1) ta@(tx0, ty0, _, _) =
  do
    (sy,sx) <- locInArea sa
    (ty,tx) <- locInArea ta
    let xok = sx1 < tx0 - 3
    let xarea = normalizeArea (sx1+2, sy, tx0-2, ty)
    let yok = sy1 < ty0 - 3
    let yarea = normalizeArea (sx, sy1+2, tx, ty0-2)
    let xyarea = normalizeArea (sx1+2, sy1+2, tx0-2, ty0-2)
    (hv,area) <- if xok && yok then fmap (\ hv -> (hv,xyarea)) (binaryChoice Horiz Vert)
                 else if xok   then return (Horiz,xarea)
                               else return (Vert,yarea)
    mkCorridor hv ((sy,sx),(ty,tx)) area

digCorridors :: Corridor -> LMap
digCorridors (p1:p2:ps) =
  M.union corPos (digCorridors (p2:ps))
  where
    corLoc = fromTo p1 p2
    corPos = M.fromList $ L.zip corLoc (repeat Tile.floorDarkId)
digCorridors _ = M.empty

mergeCorridor :: Kind.Id TileKind -> Kind.Id TileKind -> Kind.Id TileKind
mergeCorridor _ t | Tile.isWalkable t = t
mergeCorridor _ t | Tile.isUnknown t  = Tile.floorDarkId
mergeCorridor _ _                     = Tile.openingId

-- | Create a level consisting of only one room. Optionally, insert some walls.
emptyRoom :: (Level -> Rnd (LMap -> LMap)) -> LevelConfig -> LevelId -> LevelId
             -> Rnd Level
emptyRoom addRocksRnd cfg@(LevelConfig { levelSize = (sy,sx) }) nm lastNm =
  do
    let lm1 = digRoom True (1, 1, sx-1, sy-1) (emptyLMap (sy,sx))
        unknown = unknownLAMap (sy,sx)
        lvl = Level
                nm emptyParty (sy,sx) emptyParty M.empty M.empty M.empty (Kind.listArray ((0, 0), (sy, sx)) (M.elems lm1)) unknown "" ((0, 0), (0, 0))
    -- locations of the stairs
    su <- findLoc lvl (const Tile.isBoring)
    sd <- findLoc lvl (\ l t -> Tile.isBoring t
                                && distance (su,l) > minStairsDistance cfg)
    is <- rollItems cfg lvl su
    let lm2 = if nm == lastNm
              then lm1
              else M.insert sd Tile.stairsLightDownId lm1
        lm3 = M.insert su Tile.stairsLightUpId lm2
    addRocks <- addRocksRnd lvl
    let lm4 = addRocks lm3
        level = Level
                  nm emptyParty (sy,sx) emptyParty M.empty M.empty (M.fromList is) (Kind.listArray ((0, 0), (sy, sx)) (M.elems lm4)) unknown "bigroom" (su, sd)
    return level

-- | For a bigroom level: Create a level consisting of only one, empty room.
bigRoom :: LevelConfig -> LevelId -> LevelId -> Rnd Level
bigRoom = emptyRoom (\ _lvl -> return id)

-- | For a noiseroom level: Create a level consisting of only one room
-- with randomly distributed pillars.
noiseRoom :: LevelConfig -> LevelId -> LevelId -> Rnd Level
noiseRoom cfg =
  let addRocks lvl = do
        rs <- rollPillars cfg lvl
        let insertRock lm l =
              case lm M.! l of
                t | Tile.isBoring t -> M.insert l Tile.wallId lm
                _ -> lm
        return $ \ lm -> L.foldl' insertRock lm rs
  in  emptyRoom addRocks cfg

data LevelConfig = LevelConfig
  { levelGrid         :: Rnd (Y,X)
  , minRoomSize       :: Rnd (Y,X)
  , darkRoomChance    :: Rnd Bool
  , border            :: Int         -- must be at least 2!
  , levelSize         :: (Y,X)       -- lower right point; TODO: change to size or rename to 'bound'
  , extraConnects     :: (Y,X) -> Int
      -- relative to grid (in fact a range, because of duplicate connects)
  , noRooms           :: (Y,X) -> Rnd Int
      -- range, relative to grid
  , minStairsDistance :: Int         -- must not be too large
  , doorChance        :: Rnd Bool
  , doorOpenChance    :: Rnd Bool
  , doorSecretChance  :: Rnd Bool
  , doorSecretMax     :: Int
  , nrItems           :: Rnd Int     -- range
  , depth             :: Int         -- general indicator of difficulty
  }

normalLevelSize :: (Y,X)
normalLevelSize = (22,79)

defaultLevelConfig :: Int -> LevelConfig
defaultLevelConfig d =
  LevelConfig {
    levelGrid         = do
                          y <- randomR (2,4)
                          x <- randomR (3,5)
                          return (y,x),
    minRoomSize       = return (2,2),
    darkRoomChance    = chance $ 1%((22 - (2 * fromIntegral d)) `max` 2),
    border            = 2,
    levelSize         = normalLevelSize,
    extraConnects     = \ (y,x) -> (y*x) `div` 3,
    noRooms           = \ (y,x) -> randomR (0,(y*x) `div` 3),
    minStairsDistance = 676,
    doorChance        = chance $ 2%3,
    doorOpenChance    = chance $ 1%10,
    doorSecretChance  = chance $ 1%4,
    doorSecretMax     = 15,
    nrItems           = randomR (5,10),
    depth             = d
  }

_largeLevelConfig :: Int -> LevelConfig
_largeLevelConfig d =
  (defaultLevelConfig d) {
    levelGrid         = return (7,10),
    levelSize         = (77,231),
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

rogueRoom :: LevelConfig -> LevelId -> LevelId -> Rnd Level
rogueRoom cfg nm lastNm =
  do
    lgrid    <- levelGrid cfg
    lminroom <- minRoomSize cfg
    let (ly, lx) = levelSize cfg
        gs = grid lgrid (0, 0, lx, ly)
    -- grid locations of "no-rooms"
    nrnr <- noRooms cfg lgrid
    nr   <- replicateM nrnr (do
                               let (y,x) = lgrid
                               yg <- randomR (0,y-1)
                               xg <- randomR (0,x-1)
                               return (yg,xg))
    rs0 <- mapM (\ (i,r) -> do
                              r' <- if i `elem` nr
                                      then mkNoRoom (border cfg) r
                                      else mkRoom (border cfg) lminroom r
                              return (i,r')) gs
    let rooms :: [Area]
        rooms = L.map snd rs0
    dlrooms <- mapM (\ r -> darkRoomChance cfg >>= \ c -> return (r, not c)) rooms :: Rnd [(Area, Bool)]
    let rs = M.fromList rs0
    connects <- connectGrid lgrid
    addedConnects <- replicateM (extraConnects cfg lgrid) (randomConnection lgrid)
    let allConnects = L.nub (addedConnects ++ connects)
    cs <- mapM
           (\ (p0,p1) -> do
                           let r0 = rs M.! p0
                               r1 = rs M.! p1
                           connectRooms r0 r1) allConnects
    let lrooms = L.foldr (\ (r, dl) m -> digRoom dl r m) M.empty dlrooms
        lcorridors = M.unions (L.map digCorridors cs)
        lrocks = emptyLMap (levelSize cfg)
        lm = M.union (M.unionWith mergeCorridor lcorridors lrooms) lrocks
    -- convert openings into doors
    (dlmap, secretMap) <- do
      let f (l, le) o@((y, x), t) =
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
                               then return (((y,x), Tile.doorOpenId) : l, le)
                               else do
                                 rsc <- doorSecretChance cfg
                                 if not rsc
                                   then return (((y,x), Tile.doorClosedId) : l, le)
                                   else do
                                     rs1 <- randomR (doorSecretMax cfg `div` 2,
                                                     doorSecretMax cfg)
                                     return (((y,x), Tile.doorSecretId) : l, M.insert (y, x) rs1 le)
                    _ -> return (o : l, le)
      (l, le) <- foldM f ([], M.empty) (M.toList lm)
      return (M.fromList l, le)
    let unknown = unknownLAMap (levelSize cfg)
        lvl = Level nm emptyParty (levelSize cfg) emptyParty
                M.empty secretMap M.empty (Kind.listArray ((0, 0), levelSize cfg) (M.elems dlmap)) unknown "" ((0, 0), (0, 0))
    -- locations of the stairs
    su <- findLoc lvl (const Tile.isBoring)
    sd <- findLocTry 1000 lvl
            (const Tile.isBoring)
            (\ l _ -> distance (su,l) > minStairsDistance cfg)
    -- determine number of items, items and locations for the items
    is <- rollItems cfg lvl su
    let lm2 = if nm == lastNm
              then dlmap
              else M.update (\ t ->
                              Just $ if isLit t
                                     then Tile.stairsLightDownId
                                     else Tile.stairsDarkDownId)
                   sd dlmap
        lm3 = M.update (\ t ->
                         Just $ if isLit t
                                then Tile.stairsLightUpId
                                else Tile.stairsDarkUpId)
              su lm2
        -- generate map and level from the data
        meta = show allConnects
    return $
      Level nm emptyParty (levelSize cfg) emptyParty M.empty secretMap (M.fromList is) (Kind.listArray ((0, 0), levelSize cfg) (M.elems lm3)) unknown meta (su, sd)

rollItems :: LevelConfig -> Level -> Loc -> Rnd [(Loc, ([Item], [Item]))]
rollItems cfg lvl ploc =
  do
    nri <- nrItems cfg
    replicateM nri $
      do
        t <- newItem (depth cfg)
        l <- case jname (Kind.getKind (ikind t)) of
               "sword" ->
                 -- swords generated close to monsters; MUAHAHAHA
                 findLocTry 200 lvl
                   (const Tile.isBoring)
                   (\ l _ -> distance (ploc, l) > 400)
               _ -> findLoc lvl (const Tile.isBoring)
        return (l,([t], []))

rollPillars :: LevelConfig -> Level -> Rnd [Loc]
rollPillars cfg lvl =
  do
    nri <- 100 *~ nrItems cfg
    replicateM nri $ findLoc lvl (const Tile.isBoring)

emptyLMap :: (Y, X) -> LMap
emptyLMap (my, mx) =
  M.fromList [ ((y, x), Tile.wallId) | x <- [0..mx], y <- [0..my] ]

unknownLAMap :: (Y, X) -> LAMap
unknownLAMap (my, mx) =
  Kind.listArray ((0, 0), (my, mx)) (repeat Tile.unknownId)

-- | If the room has size 1, it is at most a start of a corridor.
digRoom :: Bool -> Room -> LMap -> LMap
digRoom dl (x0, y0, x1, y1) l
  | y0 == y1 && x0 == x1 = l
  | otherwise =
  let floorDL = if dl then Tile.floorLightId else Tile.floorDarkId
      rm =
        [ ((y, x), floorDL) | x <- [x0..x1], y <- [y0..y1] ]
        ++ [ ((y, x), Tile.wallId)
           | x <- [x0-1, x1+1], y <- [y0..y1] ]
        ++ [ ((y, x), Tile.wallId)
           | x <- [x0-1..x1+1], y <- [y0-1, y1+1] ]
  in M.unionWith const (M.fromList rm) l
