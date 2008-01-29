module Dungeon where

import System.Random

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
    (ry1,rx1) <- locInArea ((ry0+bd+ym-1,rx0+bd+xm-1),(y1-bd,x1-bd))
    return ((ry0,rx0),(ry1,rx1))

mkCorridor :: (Loc,Loc) -> IO [(Y,X)]  {- straight sections of the corridor -}
mkCorridor a@((y0,x0),(y1,x1)) =
  do
    (ry,rx) <- locInArea a
    -- (ry,rx) is intermediate point the path crosses
    hv <- randomRIO (False,True)
    -- hv decides whether we start in horizontal or vertical direction
    if hv then return [(y0,x0),(y0,rx),(y1,rx),(y1,x1)]
          else return [(y0,x0),(ry,x0),(ry,x1),(y1,x1)]

connectRooms :: Area -> Area -> IO [Loc]
connectRooms sa@((sy0,sx0),(sy1,sx1)) ta@((ty0,tx0),(ty1,tx1)) =
  do
    (sy,sx) <- locInArea sa
    (ty,tx) <- locInArea ta
    mkCorridor ((sy,sx),(ty,tx))

digCorridor :: Corridor -> LMap -> LMap
digCorridor (p1:p2:ps) l =
  digCorridor (p2:ps) 
    (M.unionWith corridorUpdate (M.fromList [ (ps,(Corridor,Unknown)) | ps <- fromTo p1 p2 ]) l)
  where
    corridorUpdate _ (Wall _,u)  = (Opening,u)
    corridorUpdate _ (Opening,u) = (Opening,u)
    corridorUpdate _ (Floor,u)   = (Floor,u)
    corridorUpdate (x,u) _       = (x,u)
digCorridor _ l = l
  

{-
gridX = 6
gridY = 6
levelX = 200
levelY = 70
-}
gridX = 3
gridY = 3
minX = 2
minY = 2
levelX = 79
levelY = 23
-- TODO next: generate rooms for each grid point, connect the grid, compute
-- corridors between the rooms as given by the grid connection
level :: String -> IO (Maybe (Level, Loc) -> Maybe (Level, Loc) -> Level, Loc, Loc)
level nm =
  do
    let gs = M.toList (grid (gridY,gridX) ((0,0),(levelY,levelX)))
    rs0 <- mapM (\ (i,r) -> mkRoom 1 (minY,minX) r >>= \ r' -> return (i,r')) gs
    let rooms = L.map snd rs0
    let rs = M.fromList rs0
    connects <- connectGrid (gridY,gridX)
    cs <- mapM
           (\ (p0,p1) -> do
                           let r0 = rs ! p0
                               r1 = rs ! p1
                           connectRooms r0 r1) connects
    let lmap = foldr digCorridor (foldr digRoom (emptyLMap (levelY,levelX)) rooms) cs
    let lvl = Level nm (levelY,levelX) lmap
    su <- findLoc lvl (const (==Floor))
    sd <- findLoc lvl (\ l t -> t == Floor && distance (su,l) > min levelX levelY)
    return $ (\ lu ld ->
      let lmap' = M.insert su (Stairs Up lu, Unknown) $
                  M.insert sd (Stairs Down ld, Unknown) $ lmap
      in  Level nm (levelY,levelX) lmap', su, sd)

emptyLMap :: (Y,X) -> LMap
emptyLMap (my,mx) = M.fromList [ ((y,x),(Rock,Unknown)) | x <- [0..mx], y <- [0..my] ]

digRoom :: Room -> LMap -> LMap
digRoom ((y0,x0),(y1,x1)) l =
  let rm = M.fromList $ [ ((y,x),(Floor,Unknown)) | x <- [x0..x1], y <- [y0..y1] ]
                     ++ [ ((y,x),(Wall Horiz,Unknown)) | x <- [x0-1..x1+1], y <- [y0-1,y1+1] ]
                     ++ [ ((y,x),(Wall Vert,Unknown)) | x <- [x0-1,x1+1], y <- [y0..y1] ]
  in M.unionWith const rm l

