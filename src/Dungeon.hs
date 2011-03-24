module Dungeon where

import Prelude hiding (floor)
import Control.Monad

import Data.Binary
import Data.Map as M
import Data.List as L
import Data.Ratio
import Data.Maybe

import Geometry
import Level
import Item
import Random

-- | The complete dungeon is a map from level names to levels.
-- We usually store all but the current level in this data structure.
data Dungeon = Dungeon (M.Map LevelName Level)
  deriving Show

-- | Create a dungeon from a list of levels.
dungeon :: [Level] -> Dungeon
dungeon = Dungeon . M.fromList . L.map (\ l -> (lname l, l))

-- | Extract a level from a dungeon.
getDungeonLevel :: LevelName -> Dungeon -> (Level, Dungeon)
getDungeonLevel ln (Dungeon dng) = (dng ! ln, Dungeon (M.delete ln dng))

-- | Put a level into a dungeon.
putDungeonLevel :: Level -> Dungeon -> Dungeon
putDungeonLevel lvl (Dungeon dng) = Dungeon (M.insert (lname lvl) lvl dng)

sizeDungeon :: Dungeon -> Int
sizeDungeon (Dungeon dng) = M.size dng

instance Binary Dungeon where
  put (Dungeon dng) = put (M.elems dng)
  get = liftM dungeon get

type Corridor = [(Y,X)]
type Room = Area

-- | Create a random room according to given parameters.
mkRoom :: Int ->      -- ^ border columns
          (Y,X) ->    -- ^ minimum size
          Area ->     -- ^ this is an area, not the room itself
          Rnd Room    -- ^ this is the upper-left and lower-right corner of the room
mkRoom bd (ym,xm) ((y0,x0),(y1,x1)) =
  do
    (ry0,rx0) <- locInArea ((y0 + bd,x0 + bd),(y1 - bd - ym + 1,x1 - bd - xm + 1))
    (ry1,rx1) <- locInArea ((ry0 + ym - 1,rx0 + xm - 1),(y1 - bd,x1 - bd))
    return ((ry0,rx0),(ry1,rx1))

-- | Create a no-room, i.e., a single corridor field.
mkNoRoom :: Int ->      -- ^ border columns
            Area ->     -- ^ this is an area, not the room itself
            Rnd Room    -- ^ this is the upper-left and lower-right corner of the room
mkNoRoom bd ((y0,x0),(y1,x1)) =
  do
    (ry,rx) <- locInArea ((y0 + bd,x0 + bd),(y1 - bd,x1 - bd))
    return ((ry,rx),(ry,rx))

-- | Create a corridor, either horizontal or vertical, with
-- a possible intermediate part that is in the opposite direction.
mkCorridor :: HV -> (Loc,Loc) -> Area -> Rnd [(Y,X)] {- straight sections of the corridor -}
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
connectRooms sa@((sy0,sx0),(sy1,sx1)) ta@((ty0,tx0),(ty1,tx1)) =
  do
    (sy,sx) <- locInArea sa
    (ty,tx) <- locInArea ta
    let xok = sx1 < tx0 - 3
    let xarea = normalizeArea ((sy,sx1+2),(ty,tx0-2))
    let yok = sy1 < ty0 - 3
    let yarea = normalizeArea ((sy1+2,sx),(ty0-2,tx))
    let xyarea = normalizeArea ((sy1+2,sx1+2),(ty0-2,tx0-2))
    (hv,area) <- if xok && yok then fmap (\ hv -> (hv,xyarea)) (binaryChoice Horiz Vert)
                 else if xok   then return (Horiz,xarea)
                               else return (Vert,yarea)
    mkCorridor hv ((sy,sx),(ty,tx)) area

-- | Actually dig a corridor.
digCorridor :: Corridor -> LMap -> LMap
digCorridor (p1:p2:ps) l =
  digCorridor (p2:ps) (M.unionWith corridorUpdate newPos l)
  where
    newPos = M.fromList [ (ps,newTile Corridor) | ps <- fromTo p1 p2 ]
    corridorUpdate _ (Tile (Wall hv) is,u)    = (Tile (Opening hv) is,u)
    corridorUpdate _ (Tile (Opening hv) is,u) = (Tile (Opening hv) is,u)
    corridorUpdate _ (Tile (Floor l) is,u)    = (Tile (Floor l) is,u)
    corridorUpdate (x,u) _                    = (x,u)
digCorridor _ l = l

-- | Create a new tile.
newTile :: Terrain -> (Tile, Tile)
newTile t = (Tile t [], Tile Unknown [])

-- | Create a level consisting of only one room. Optionally, insert some walls.
emptyRoom :: (Level -> Rnd (LMap -> LMap)) -> LevelConfig ->
           LevelName -> Rnd (Maybe (Maybe DungeonLoc) -> Maybe (Maybe DungeonLoc) -> Level, Loc, Loc)
emptyRoom addWallsRnd cfg@(LevelConfig { levelSize = (sy,sx) }) nm =
  do
    let lmap = digRoom Light ((1,1),(sy-1,sx-1)) (emptyLMap (sy,sx))
    let smap = M.fromList [ ((y,x),-100) | y <- [0..sy], x <- [0..sx] ]
    let lvl = Level nm lmEmpty (sy,sx) lmEmpty smap lmap ""
    -- locations of the stairs
    su <- findLoc lvl (const floor)
    sd <- findLoc lvl (\ l t -> floor t
                                && distance (su,l) > minStairsDistance cfg)
    is <- rollItems cfg lvl su
    addWalls <- addWallsRnd lvl
    let addItem lmap (l,it) =
          M.update (\ (t,r) -> Just (t { titems = it : titems t }, r)) l lmap
        flmap lu ld =
          addWalls $
          maybe id (\ l -> M.insert su (newTile (Stairs Light Up   l))) lu $
          maybe id (\ l -> M.insert sd (newTile (Stairs Light Down l))) ld $
          (\lmap -> foldl' addItem lmap is) $
          lmap
        level lu ld = Level nm lmEmpty (sy,sx) lmEmpty smap (flmap lu ld) "bigroom"
    return (level, su, sd)

-- | For a bigroom level: Create a level consisting of only one, empty room.
bigRoom :: LevelConfig ->
           LevelName -> Rnd (Maybe (Maybe DungeonLoc) -> Maybe (Maybe DungeonLoc) -> Level, Loc, Loc)
bigRoom = emptyRoom (\ lvl -> return id)

-- | For a noiseroom level: Create a level consisting of only one room
-- with randomly distributed pillars.
noiseRoom :: LevelConfig ->
             LevelName -> Rnd (Maybe (Maybe DungeonLoc) -> Maybe (Maybe DungeonLoc) -> Level, Loc, Loc)
noiseRoom cfg =
  let addWalls lvl = do
        rs <- rollPillars cfg lvl
        let insertWall lmap l =
              case lmap `at` l of
                Tile (Floor _) [] -> M.insert l (newTile (Wall O)) lmap
                _ -> lmap
        return $ \ lmap -> foldl' insertWall lmap rs
  in  emptyRoom addWalls cfg

data LevelConfig =
  LevelConfig {
    levelGrid         :: Rnd (Y,X),
    minRoomSize       :: Rnd (Y,X),
    darkRoomChance    :: Rnd Bool,
    border            :: Int,       -- must be at least 2!
    levelSize         :: (Y,X),     -- lower right point
    extraConnects     :: (Y,X) -> Int,
                                    -- relative to grid
                                    -- (in fact a range, because of duplicate connects)
    noRooms           :: (Y,X) -> Rnd Int,
                                    -- range, relative to grid
    minStairsDistance :: Int,       -- must not be too large
    doorChance        :: Rnd Bool,
    doorOpenChance    :: Rnd Bool,
    doorSecretChance  :: Rnd Bool,
    doorSecretMax     :: Int,
    nrItems           :: Rnd Int,   -- range
    depth             :: Int        -- general indicator of difficulty
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

largeLevelConfig :: Int -> LevelConfig
largeLevelConfig d =
  (defaultLevelConfig d) {
    levelGrid         = return (7,10),
    levelSize         = (77,231),
    extraConnects     = const 10
  }

-- | Create a "normal" dungeon level. Takes a configuration in order
-- to tweak all sorts of data.
rogueRoom :: LevelConfig ->
         LevelName ->
         Rnd (Maybe (Maybe DungeonLoc) -> Maybe (Maybe DungeonLoc) ->
              Level, Loc, Loc)
rogueRoom cfg nm =
  do
    lgrid    <- levelGrid cfg
    lminroom <- minRoomSize cfg
    let gs = M.toList (grid lgrid ((0,0),levelSize cfg))
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
    let rooms :: [(Loc, Loc)]
        rooms = L.map snd rs0
    dlrooms <- (mapM (\ r -> darkRoomChance cfg >>= \ c -> return (r, toDL (not c))) rooms) :: Rnd [((Loc, Loc), DL)]
    let rs = M.fromList rs0
    connects <- connectGrid lgrid
    addedConnects <- replicateM (extraConnects cfg lgrid) (randomConnection lgrid)
    let allConnects = L.nub (addedConnects ++ connects)
    cs <- mapM
           (\ (p0,p1) -> do
                           let r0 = rs ! p0
                               r1 = rs ! p1
                           connectRooms r0 r1) allConnects
    let smap = M.fromList [ ((y,x),-100) | let (sy,sx) = levelSize cfg,
                                           y <- [0..sy], x <- [0..sx] ]
    let lmap :: LMap
        lmap = foldr digCorridor (foldr (\ (r, dl) m -> digRoom dl r m)
                                        (emptyLMap (levelSize cfg)) dlrooms) cs
    let lvl = Level nm lmEmpty (levelSize cfg) lmEmpty smap lmap ""
    -- convert openings into doors
    dlmap <- fmap M.fromList . mapM
                (\ o@((y,x),(t,r)) ->
                  case t of
                    Tile (Opening hv) _ ->
                      do
                        -- openings have a certain chance to be doors;
                        -- doors have a certain chance to be open; and
                        -- closed doors have a certain chance to be
                        -- secret
                        rb <- doorChance cfg
                        ro <- doorOpenChance cfg
                        rs <- if ro then return Nothing
                                    else do rsc <- doorSecretChance cfg
                                            fmap Just
                                                 (if rsc then randomR (1, doorSecretMax cfg)
                                                         else return 0)
                        if rb
                          then return ((y,x),newTile (Door hv rs))
                          else return o
                    _ -> return o) .
                M.toList $ lmap
    -- locations of the stairs
    su <- findLoc lvl (const floor)
    sd <- findLocTry 1000 lvl
            (const floor)
            (\ l t -> distance (su,l) > minStairsDistance cfg)
    -- determine number of items, items and locations for the items
    is <- rollItems cfg lvl su
    -- generate map and level from the data
    let meta = show allConnects
    return (\ lu ld ->
      let flmap = maybe id (\ l -> M.update (\ (t,r) -> Just $ newTile (Stairs (toDL $ light t) Up   l)) su) lu $
                  maybe id (\ l -> M.update (\ (t,r) -> Just $ newTile (Stairs (toDL $ light t) Down l)) sd) ld $
                  foldr (\ (l,it) f -> M.update (\ (t,r) -> Just (t { titems = it : titems t }, r)) l . f) id is
                  dlmap
      in  Level nm lmEmpty (levelSize cfg) lmEmpty smap flmap meta, su, sd)

rollItems :: LevelConfig -> Level -> Loc -> Rnd [(Loc, Item)]
rollItems cfg lvl ploc =
  do
    nri <- nrItems cfg
    replicateM nri $
      do
        t <- newItem (depth cfg) itemFrequency
        l <- case itype t of
               Sword _ ->
                 -- swords generated close to monsters; MUAHAHAHA
                 findLocTry 200 lvl
                   (const floor)
                   (\ l t -> distance (ploc, l) > 400)
               _ -> findLoc lvl (const floor)
        return (l,t)

rollPillars :: LevelConfig -> Level -> Rnd [Loc]
rollPillars cfg lvl =
  do
    nri <- 100 *~ nrItems cfg
    replicateM nri $
      do
        l <- findLoc lvl (const floor)
        return l

emptyLMap :: (Y,X) -> LMap
emptyLMap (my,mx) = M.fromList [ ((y,x),newTile Rock) | x <- [0..mx], y <- [0..my] ]

-- | If the room has size 1, it is assumed to be a no-room, and a single
-- corridor field will be dug instead of a room.
digRoom :: DL -> Room -> LMap -> LMap
digRoom dl ((y0,x0),(y1,x1)) l
  | y0 == y1 && x0 == x1 =
  M.insert (y0,x0) (newTile Corridor) l
  | otherwise =
  let rm = M.fromList $ [ ((y,x),newTile (Floor dl))   | x <- [x0..x1],     y <- [y0..y1]    ]
                     ++ [ ((y,x),newTile (Wall p))     | (x,y,p) <- [(x0-1,y0-1,UL),(x1+1,y0-1,UR),(x0-1,y1+1,DL),(x1+1,y1+1,DR)] ]
                     ++ [ ((y,x),newTile (Wall p))     | x <- [x0..x1], (y,p) <- [(y0-1,U),(y1+1,D)] ]
                     ++ [ ((y,x),newTile (Wall p))     | (x,p) <- [(x0-1,L),(x1+1,R)],  y <- [y0..y1]    ]
  in M.unionWith const rm l
