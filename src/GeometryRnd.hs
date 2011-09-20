module GeometryRnd where

import qualified Data.List as L
import qualified Data.Set as S

import Geometry
import Random

findLocInArea :: Area -> (Loc -> Bool) -> Rnd Loc
findLocInArea a@((y0, x0), (y1, x1)) p = do
  rx <- randomR (x0, x1)
  ry <- randomR (y0, y1)
  let loc = (ry, rx)
  if p loc then return loc else findLocInArea a p

locInArea :: Area -> Rnd Loc
locInArea a = findLocInArea a (const True)

connectGrid' :: (Y,X) -> S.Set Loc -> S.Set Loc -> [(Loc, Loc)] ->
                Rnd [(Loc, Loc)]
connectGrid' (ny, nx) unconnected candidates acc
  | S.null candidates = return (L.map normalize acc)
  | otherwise = do
      c <- oneOf (S.toList candidates)
      -- potential new candidates:
      let ns = S.fromList $ neighbors ((0,0),(ny-1,nx-1)) c
          nu = S.delete c unconnected  -- new unconnected
          -- (new candidates, potential connections):
          (nc, ds) = S.partition (`S.member` nu) ns
      new <- if S.null ds
             then return id
             else do
                    d <- oneOf (S.toList ds)
                    return ((c,d) :)
      connectGrid' (ny,nx) nu (S.delete c (candidates `S.union` nc)) (new acc)

connectGrid :: (Y,X) -> Rnd [(Loc, Loc)]
connectGrid (ny, nx) = do
  let unconnected = S.fromList [ (y, x) | x <- [0..nx-1], y <- [0..ny-1] ]
  -- candidates are neighbors that are still unconnected; we start with
  -- a random choice
  rx <- randomR (0, nx-1)
  ry <- randomR (0, ny-1)
  let candidates  = S.fromList [ (ry, rx) ]
  connectGrid' (ny, nx) unconnected candidates []

randomConnection :: (Y,X) -> Rnd (Loc, Loc)
randomConnection (ny, nx) = do
  rb  <- randomR (False, True)
  if rb
    then do
           rx  <- randomR (0, nx-2)
           ry  <- randomR (0, ny-1)
           return (normalize ((ry, rx), (ry, rx+1)))
    else do
           ry  <- randomR (0, ny-2)
           rx  <- randomR (0, nx-1)
           return (normalize ((ry, rx), (ry+1, rx)))
