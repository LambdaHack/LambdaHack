module GeometryRnd where

import qualified Data.List as L
import qualified Data.Set as S

import Geometry
import Random

xyInArea :: Area -> Rnd (X, Y)
xyInArea (x0, y0, x1, y1) = do
  rx <- randomR (x0, x1)
  ry <- randomR (y0, y1)
  return (rx, ry)

connectGrid' :: (X, Y) -> S.Set Loc -> S.Set Loc -> [(Loc, Loc)] ->
                Rnd [(Loc, Loc)]
connectGrid' (nx, ny) unconnected candidates acc
  | S.null candidates = return (L.map normalize acc)
  | otherwise = do
      c <- oneOf (S.toList candidates)
      -- potential new candidates:
      let ns = S.fromList $ neighbors (0, 0, nx-1, ny-1) c
          nu = S.delete c unconnected  -- new unconnected
          -- (new candidates, potential connections):
          (nc, ds) = S.partition (`S.member` nu) ns
      new <- if S.null ds
             then return id
             else do
                    d <- oneOf (S.toList ds)
                    return ((c, d) :)
      connectGrid' (nx, ny) nu (S.delete c (candidates `S.union` nc)) (new acc)

connectGrid :: (X, Y) -> Rnd [(Loc, Loc)]
connectGrid (nx, ny) = do
  let unconnected = S.fromList [ toLoc (x, y) | x <- [0..nx-1], y <- [0..ny-1] ]
  -- candidates are neighbors that are still unconnected; we start with
  -- a random choice
  rx <- randomR (0, nx-1)
  ry <- randomR (0, ny-1)
  let candidates  = S.fromList [ toLoc (rx, ry) ]
  connectGrid' (nx, ny) unconnected candidates []

randomConnection :: (X, Y) -> Rnd (Loc, Loc)
randomConnection (nx, ny) = do
  rb  <- randomR (False, True)
  if rb
    then do
           rx  <- randomR (0, nx-2)
           ry  <- randomR (0, ny-1)
           return (normalize (toLoc (rx, ry), toLoc (rx+1, ry)))
    else do
           ry  <- randomR (0, ny-2)
           rx  <- randomR (0, nx-1)
           return (normalize (toLoc (rx, ry), toLoc (rx, ry+1)))
