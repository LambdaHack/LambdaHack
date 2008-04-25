module Dungeon where

import Prelude hiding (floor)
import Control.Monad

import Data.Map as M
import Data.List as L
import Data.Ratio

import State
import Geometry
import Level
import Monster
import Item
import Random

type Corridor = [(Y,X)]
type Room = Area

mkRoom :: Int ->      {- border columns -}
          (Y,X) ->    {- minimum size -}
          Area ->     {- this is an area, not the room itself -}
          Rnd Room    {- this is the upper-left and lower-right corner of the room -}
mkRoom bd (ym,xm)((y0,x0),(y1,x1)) =
  do
    (ry0,rx0) <- locInArea ((y0+bd,x0+bd),(y1-bd-ym+1,x1-bd-xm+1))
    (ry1,rx1) <- locInArea ((ry0+ym-1,rx0+xm-1),(y1-bd,x1-bd))
    return ((ry0,rx0),(ry1,rx1))

mkCorridor :: HV -> (Loc,Loc) -> Area -> Rnd [(Y,X)] {- straight sections of the corridor -}
mkCorridor hv ((y0,x0),(y1,x1)) b =
  do
    (ry,rx) <- findLocInArea b (const True)
      -- (ry,rx) is intermediate point the path crosses
    -- hv decides whether we start in horizontal or vertical direction
    case hv of
      Horiz -> return [(y0,x0),(y0,rx),(y1,rx),(y1,x1)]
      Vert  -> return [(y0,x0),(ry,x0),(ry,x1),(y1,x1)]

-- the condition passed to mkCorridor is tricky; there might not always
-- exist a suitable intermediate point is the rooms are allowed to be close
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

digCorridor :: Corridor -> LMap -> LMap
digCorridor (p1:p2:ps) l =
  digCorridor (p2:ps) 
    (M.unionWith corridorUpdate (M.fromList [ (ps,newTile Corridor) | ps <- fromTo p1 p2 ]) l)
  where
    corridorUpdate _ (Tile (Wall hv) is,u)    = (Tile (Opening hv) is,u)
    corridorUpdate _ (Tile (Opening hv) is,u) = (Tile (Opening hv) is,u)
    corridorUpdate _ (Tile (Floor l) is,u)    = (Tile (Floor l) is,u)
    corridorUpdate (x,u) _                    = (x,u)
digCorridor _ l = l
  
newTile :: Terrain -> (Tile, Tile)
newTile t = (Tile t [], Tile Unknown [])

bigroom :: LevelConfig -> 
           LevelName -> Rnd (Maybe (Maybe DungeonLoc) -> Maybe (Maybe DungeonLoc) -> Level, Loc, Loc)
bigroom (LevelConfig { levelSize = (sy,sx) }) nm =
  do
    let lmap = digRoom Light ((1,1),(sy-1,sx-1)) (emptyLMap (sy,sx))
    let smap = M.fromList [ ((y,x),-100) | y <- [0..sy], x <- [0..sx] ]
    let lvl = Level nm (sy,sx) [] smap lmap ""
    -- locations of the stairs
    su <- findLoc lvl (const floor)
    sd <- findLoc lvl (\ l t -> floor t && distance (su,l) > 676)
    return $ (\ lu ld ->
      let flmap = maybe id (\ l -> M.insert su (newTile (Stairs Up   l))) lu $
                  maybe id (\ l -> M.insert sd (newTile (Stairs Down l))) ld $
                  lmap
      in  Level nm (sy,sx) [] smap flmap "bigroom", su, sd)

data LevelConfig =
  LevelConfig {
    levelGrid         :: (Y,X),
    minRoomSize       :: (Y,X),
    darkRoomChance    :: Rational,
    border            :: Int,      -- must be at least 2!
    levelSize         :: (Y,X),    -- lower right point
    extraConnects     :: Int,
    minStairsDistance :: Int,      -- must not be too large
    doorChance        :: Rational,
    doorOpenChance    :: Rational,
    doorSecretChance  :: Rational,
    doorSecretMax     :: Int,
    nrItems           :: (Int,Int)
  }
    
defaultLevelConfig :: LevelConfig
defaultLevelConfig =
  LevelConfig {
    levelGrid         = (3,3), -- (7,10), -- (3,3), -- (2,5)
    minRoomSize       = (2,2),
    darkRoomChance    = 1%8,
    border            = 2,
    levelSize         = (22,79), -- (77,231),  -- (22,79),
    extraConnects     = 3,     -- 6
    minStairsDistance = 676,
    doorChance        = 1%2,
    doorOpenChance    = 1%2,
    doorSecretChance  = 1%3,
    doorSecretMax     = 15,
    nrItems           = (3,7)  -- range
  }

largeLevelConfig :: LevelConfig
largeLevelConfig =
  defaultLevelConfig {
    levelGrid         = (7,10),
    levelSize         = (77,231),
    extraConnects     = 10
  }

level :: LevelConfig ->
         LevelName -> Rnd (Maybe (Maybe DungeonLoc) -> Maybe (Maybe DungeonLoc) -> Level, Loc, Loc)
level cfg nm =
  do
    let gs = M.toList (grid (levelGrid cfg) ((0,0),levelSize cfg))
    rs0 <- mapM (\ (i,r) -> do
                              r' <- mkRoom (border cfg) (minRoomSize cfg) r
                              return (i,r')) gs
    let rooms = L.map snd rs0
    dlrooms <- mapM (\ r -> chance (darkRoomChance cfg) >>= \ c -> return (r, toDL (not c))) rooms
    let rs = M.fromList rs0
    connects <- connectGrid (levelGrid cfg)
    addedConnects <- replicateM (extraConnects cfg) (randomConnection (levelGrid cfg))
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
    let lvl = Level nm (levelSize cfg) [] smap lmap "" 
    -- convert openings into doors
    dlmap <- fmap M.fromList . mapM
                (\ o@((y,x),(t,r)) -> 
                  case t of
                    Tile (Opening hv) _ ->
                      do
                        -- chance for doors
                        rb <- chance (doorChance cfg)
                        -- chance for a door to be open
                        ro <- chance (doorOpenChance cfg)
                        rs <- if ro then return Nothing
                                    else do -- chance for a door to be secret
                                            rsc <- chance (doorSecretChance cfg)
                                            fmap Just
                                                 (if rsc then randomR (1, doorSecretMax cfg)
                                                         else return 0)
                        if rb
                          then return ((y,x),newTile (Door hv rs))
                          else return o
                    _ -> return o) .
                M.toList $ lmap
    -- determine number of items, items and locations for the items
    nri <- randomR (nrItems cfg)
    is  <- replicateM nri $
           do
             l <- findLoc lvl (const floor)
             t <- newItem itemFrequency 
             return (l,t)
    -- locations of the stairs
    su <- findLoc lvl (const floor)
    sd <- findLoc lvl (\ l t -> floor t && distance (su,l) > minStairsDistance cfg)
    let meta = show allConnects
    return $ (\ lu ld ->
      let flmap = maybe id (\ l -> M.insert su (newTile (Stairs Up   l))) lu $
                  maybe id (\ l -> M.insert sd (newTile (Stairs Down l))) ld $
                  foldr (\ (l,it) f -> M.update (\ (t,r) -> Just (t { titems = it : titems t }, r)) l . f) id is $
                  dlmap
      in  Level nm (levelSize cfg) [] smap flmap meta, su, sd)

emptyLMap :: (Y,X) -> LMap
emptyLMap (my,mx) = M.fromList [ ((y,x),newTile Rock) | x <- [0..mx], y <- [0..my] ]

digRoom :: DL -> Room -> LMap -> LMap
digRoom dl ((y0,x0),(y1,x1)) l =
  let rm = M.fromList $ [ ((y,x),newTile (Floor dl))   | x <- [x0..x1],     y <- [y0..y1]    ]
                     ++ [ ((y,x),newTile (Wall p)) | (x,y,p) <- [(x0-1,y0-1,UL),(x1+1,y0-1,UR),(x0-1,y1+1,DL),(x1+1,y1+1,DR)] ]
                     ++ [ ((y,x),newTile (Wall p)) | x <- [x0..x1], (y,p) <- [(y0-1,U),(y1+1,D)] ]
                     ++ [ ((y,x),newTile (Wall p)) | (x,p) <- [(x0-1,L),(x1+1,R)],  y <- [y0..y1]    ]
  in M.unionWith const rm l

addMonster :: Level -> Player -> Rnd Level
addMonster lvl@(Level { lmonsters = ms, lmap = lmap })
           player@(Monster { mloc = ploc }) =
  do
    -- TODO: remove the hardcoded chance
    rc <- chance (1 % if L.null ms then 50 else 700)
    if rc
     then do
            -- TODO: new monsters always be generated in a place that isn't
            -- visible by the player (if possible -- not possible for bigrooms)
            sm <- findLoc lvl (\ l t -> floor t && 
                                        not (l `L.elem` L.map mloc (player : ms)) &&
                                        distance (ploc, l) > 400)
            m <- newMonster sm monsterFrequency
            return (updateMonsters lvl (const (m : ms)))
     else return lvl

