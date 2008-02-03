module Dungeon where

import System.Random
import Control.Monad

import Data.Map as M
import Data.List as L

import Level

type Corridor = [(Y,X)]
type Room = Area

mkRoom :: Int ->      {- border columns -}
          (Y,X) ->    {- minimum size -}
          Area ->     {- this is an area, not the room itself -}
          IO Room     {- this is the upper-left and lower-right corner of the room -}
mkRoom bd (ym,xm)((y0,x0),(y1,x1)) =
  do
    (ry0,rx0) <- locInArea ((y0+bd,x0+bd),(y1-bd-ym+1,x1-bd-xm+1))
    (ry1,rx1) <- locInArea ((ry0+ym-1,rx0+xm-1),(y1-bd,x1-bd))
    return ((ry0,rx0),(ry1,rx1))

mkCorridor :: HV -> (Loc,Loc) -> Area -> IO [(Y,X)] {- straight sections of the corridor -}
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
connectRooms :: Area -> Area -> IO [Loc]
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
    (M.unionWith corridorUpdate (M.fromList [ (ps,(Corridor,Unknown)) | ps <- fromTo p1 p2 ]) l)
  where
    corridorUpdate _ (Wall hv,u)    = (Opening hv,u)
    corridorUpdate _ (Opening hv,u) = (Opening hv,u)
    corridorUpdate _ (Floor,u)      = (Floor,u)
    corridorUpdate (x,u) _          = (x,u)
digCorridor _ l = l
  

data LevelConfig =
  LevelConfig {
    levelGrid         :: (Y,X),
    minRoomSize       :: (Y,X),
    border            :: Int,      -- must be at least 2!
    levelSize         :: (Y,X),    -- lower right point
    extraConnects     :: Int,
    minStairsDistance :: Int       -- must not be too large
  }
    
defaultLevelConfig :: LevelConfig
defaultLevelConfig =
  LevelConfig {
    levelGrid         = (3,3), -- (2,5)
    minRoomSize       = (2,2),
    border            = 2,
    levelSize         = (23,79),
    extraConnects     = 3,     -- 6
    minStairsDistance = 676 
  }

level :: LevelConfig ->
         String -> IO (Maybe (Level, Loc) -> Maybe (Level, Loc) -> Level, Loc, Loc)
level cfg nm =
  do
    let gs = M.toList (grid (levelGrid cfg) ((0,0),levelSize cfg))
    rs0 <- mapM (\ (i,r) -> do
                              r' <- mkRoom (border cfg) (minRoomSize cfg) r
                              return (i,r')) gs
    let rooms = L.map snd rs0
    let rs = M.fromList rs0
    connects <- connectGrid (levelGrid cfg)
    addedConnects <- replicateM (extraConnects cfg) (randomConnection (levelGrid cfg))
    let allConnects = L.union addedConnects connects
    cs <- mapM
           (\ (p0,p1) -> do
                           let r0 = rs ! p0
                               r1 = rs ! p1
                           connectRooms r0 r1) allConnects
    let lmap = foldr digCorridor (foldr digRoom (emptyLMap (levelSize cfg)) rooms) cs
    let lvl = Level nm (levelSize cfg) lmap
    -- convert openings into doors
    dlmap <- fmap M.fromList . mapM
                (\ o@((y,x),(t,r)) -> 
                  case t of
                    Opening hv ->
                      do
                        rb <- randomRIO (False,True) -- TODO: make configurable
                        ro <- randomRIO (False,True)
                        if rb
                          then return ((y,x),(Door hv ro,Unknown))
                          else return o
                    _ -> return o) .
                M.toList $ lmap
    su <- findLoc lvl (const (==Floor))
    sd <- findLoc lvl (\ l t -> t == Floor && distance (su,l) > minStairsDistance cfg)
    return $ (\ lu ld ->
      let flmap = M.insert su (Stairs Up lu, Unknown) $
                  M.insert sd (Stairs Down ld, Unknown) $ dlmap
      in  Level nm (levelSize cfg) flmap, su, sd)

emptyLMap :: (Y,X) -> LMap
emptyLMap (my,mx) = M.fromList [ ((y,x),(Rock,Unknown)) | x <- [0..mx], y <- [0..my] ]

digRoom :: Room -> LMap -> LMap
digRoom ((y0,x0),(y1,x1)) l =
  let rm = M.fromList $ [ ((y,x),(Floor,Unknown)) | x <- [x0..x1], y <- [y0..y1] ]
                     ++ [ ((y,x),(Wall Horiz,Unknown)) | x <- [x0-1..x1+1], y <- [y0-1,y1+1] ]
                     ++ [ ((y,x),(Wall Vert,Unknown)) | x <- [x0-1,x1+1], y <- [y0..y1] ]
  in M.unionWith const rm l

