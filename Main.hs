module Main where

import Data.List as L
import Data.Map as M
import Data.Set as S
import Graphics.Vty as V
import System.Random
import Control.Monad

import Level

main :: IO ()
main =
  do
    vty <- V.mkVty
    loop vty 0

display :: Vty -> (Int -> Int -> (Attr, Char)) -> IO ()
display vty f =
    let img = (foldr (<->) V.empty . 
               L.map (foldr (<|>) V.empty . 
                      L.map (\ (x,y) -> let (a,c) = f x y in renderChar a c)))
              [ [ (x,y) | x <- [0..levelX] ] | y <- [0..levelY] ]
    in  V.update vty (Pic NoCursor img)

loop vty i =
  do
    l <- level
    display vty (\ x y -> (attr, head . show $ findWithDefault Unknown (y,x) l))
    -- display vty (\ x y -> (setFG (if (x + y) `mod` 2 == 1 then red else green) attr, head . show $ (x + y + i) `mod` 7))
    e <- V.getEvent vty
    case e of
      V.EvKey KEsc _ -> shutdown vty
      _              -> loop vty (i+1)


mkRoom :: Int ->      {- border columns -}
          (Y,X) ->    {- minimum size -}
          Area ->     {- this is an area, not the room itself -}
          IO Room     {- this is the upper-left and lower-right corner of the room -}
mkRoom bd (ym,xm)((y0,x0),(y1,x1)) =
  do
    (rx0,ry0) <- pointInArea ((y0+bd,x0+bd),(y1-bd-ym+1,x1-bd-xm+1))
    (rx1,ry1) <- pointInArea ((ry0+bd+ym-1,rx0+bd+xm-1),(y1-bd,x1-bd))
    return ((ry0,rx0),(ry1,rx1))

pointInArea :: Area -> IO Loc
pointInArea ((y0,x0),(y1,x1)) =
  do
    rx <- randomRIO (x0,x1)
    ry <- randomRIO (y0,y1)
    return (rx,ry)

grid :: (Y,X) -> ((Y,X),(Y,X)) -> Map (Y,X) ((Y,X),(Y,X))
grid (ny,nx) ((y0,x0),(y1,x1)) =
  let yd = y1 - y0
      xd = x1 - x0
  in M.fromList [ ((y,x), ((y0 + (yd * y `div` ny), x0 + (xd * x `div` nx)),
                           (y0 + (yd * (y + 1) `div` ny - 1), x0 + (xd * (x + 1) `div` nx - 1))))
                | x <- [0..nx-1], y <- [0..ny-1] ]

test :: IO [((Y,X),(Y,X))]
test = mapM (mkRoom 1 (minY,minX) . snd) (M.toList (grid (3,3) ((0,0),(23,79))))

type Corridor = [(Y,X)]

mkCorridor :: ((Y,X),(Y,X)) -> IO [(Y,X)]  {- straight sections of the corridor -}
mkCorridor ((y0,x0),(y1,x1)) =
  do
    rx <- randomRIO (x0,x1)
    ry <- randomRIO (y0,y1)
    -- (ry,rx) is intermediate point the path crosses
    hv <- randomRIO (False,True)
    -- hv decides whether we start in horizontal or vertical direction
    if hv then return [(y0,x0),(y0,rx),(y1,rx),(y1,x1)]
          else return [(y0,x0),(ry,x0),(ry,x1),(y1,x1)]


connectRooms :: Area -> Area -> IO [Loc]
connectRooms ((sy0,sx0),(sy1,sx1)) ((ty0,tx0),(ty1,tx1)) =
  do
    sx <- randomRIO (sx0,sx1)
    sy <- randomRIO (sy0,sy1)
    tx <- randomRIO (tx0,tx1)
    ty <- randomRIO (ty0,ty1)
    mkCorridor ((sy,sx),(ty,tx))

connectGrid :: (Y,X) -> IO [((Y,X),(Y,X))]
connectGrid (ny,nx) =
  do
    let unconnected = S.fromList [ (y,x) | x <- [0..nx-1], y <- [0..ny-1] ]
    -- candidates are neighbors that are still unconnected; we start with
    -- a random choice
    rx <- randomRIO (0,nx-1)
    ry <- randomRIO (0,ny-1)
    let candidates  = S.fromList [ (ry,rx) ]
    connectGrid' (ny,nx) unconnected candidates []

connectGrid' :: (Y,X) -> Set (Y,X) -> Set (Y,X) -> [((Y,X),(Y,X))] -> IO [((Y,X),(Y,X))]
connectGrid' (ny,nx) unconnected candidates acc
  | S.null candidates = return acc
  | otherwise = do
                  r <- randomRIO (0,S.size candidates - 1)
                  let c = S.toList candidates !! r
                  let ns = neighbors ((0,0),(ny-1,nx-1)) c -- potential new candidates
                  let nu = S.delete c unconnected -- new unconnected
                  let (nc,ds) = S.partition (`S.member` nu) ns
                                  -- (new candidates, potential connections)
                  new <- if S.null ds then return id
                                      else do
                                             s <- randomRIO (0,S.size ds - 1)
                                             let d = S.toList ds !! s
                                             return ((c,d) :)
                  connectGrid' (ny,nx) nu
                                       (S.delete c (candidates `S.union` nc)) (new acc)

neighbors :: Area ->        {- size limitation -}
             Loc ->         {- location to find neighbors of -}
             Set Loc
neighbors area (y,x) =
  let cs = [ (y + dy, x + dx) | dy <- [-1..1], dx <- [-1..1], (dx + dy) `mod` 2 == 1 ] 
  in  S.fromList (L.filter (`inside` area) cs)

inside :: Loc -> Area -> Bool
inside (y,x) ((y0,x0),(y1,x1)) = x1 >= x && x >= x0 && y1 >= y && y >= y0

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
level =
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
    return (foldr digCorridor (foldr digRoom emptyLevel rooms) cs)


emptyLevel :: Level
emptyLevel = M.fromList [ ((y,x),Rock) | x <- [0..levelX], y <- [0..levelY] ]

digRoom :: Room -> Level -> Level
digRoom ((y0,x0),(y1,x1)) l =
  let rm = M.fromList [ ((y,x),Floor) | x <- [x0..x1], y <- [y0..y1] ]
  in M.unionWith const rm l

fromTo :: Loc -> Loc -> [Loc]
fromTo (y0,x0) (y1,x1)
  | y0 == y1 = L.map (\ x -> (y0,x)) (fromTo1 x0 x1)
  | x0 == x1 = L.map (\ y -> (y,x0)) (fromTo1 y0 y1)

fromTo1 :: X -> X -> [X]
fromTo1 x0 x1
  | x0 <= x1  = [x0..x1]
  | otherwise = [x0,x0-1..x1]

digCorridor :: Corridor -> Level -> Level
digCorridor (p1:p2:ps) l =
  digCorridor (p2:ps) (M.unionWith corridorUpdate (M.fromList [ (ps,Corridor) | ps <- fromTo p1 p2 ]) l)
    where
      corridorUpdate _ Floor = Floor
      corridorUpdate x _     = x
digCorridor _ l = l
  
