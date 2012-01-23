-- | Operations on the "Area" type that involve random numbers.
module Game.LambdaHack.AreaRnd
  ( -- * Picking random points inside areas
    xyInArea, mkRoom, mkVoidRoom
    -- * Choosing connections between areas in a grid
  , connectGrid, randomConnection
    -- * Plotting individual corridors between two areas
  , Corridor, connectPlaces
  ) where

import qualified Data.List as L
import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.PointXY
import Game.LambdaHack.Area
import Game.LambdaHack.Random

-- Picking random points inside areas

-- | Pick a random point within an area.
xyInArea :: Area -> Rnd PointXY
xyInArea (x0, y0, x1, y1) = do
  rx <- randomR (x0, x1)
  ry <- randomR (y0, y1)
  return $ PointXY (rx, ry)

-- | Create a random room according to given parameters.
mkRoom :: (X, Y)    -- ^ minimum size
       -> Area      -- ^ the containing area, not the room itself
       -> Rnd Area
mkRoom (xm, ym) (x0, y0, x1, y1) = do
  let area0 = (x0, y0, x1 - xm + 1, y1 - ym + 1)
  assert (validArea area0 `blame` area0) $ do
    PointXY (rx0, ry0) <- xyInArea area0
    let area1 = (rx0 + xm - 1, ry0 + ym - 1, x1, y1)
    assert (validArea area1 `blame` area1) $ do
      PointXY (rx1, ry1) <- xyInArea area1
      return (rx0, ry0, rx1, ry1)

-- | Create a void room, i.e., a single point area.
mkVoidRoom :: Area     -- ^ the area in which to pick the point
           -> Rnd Area
mkVoidRoom area = assert (validArea area `blame` area) $ do
  PointXY (ry, rx) <- xyInArea area
  return (ry, rx, ry, rx)

-- Choosing connections between areas in a grid

-- | Pick a subset of connections between adjacent areas within a grid until
-- there is only one connected component in the graph of all areas.
connectGrid :: (X, Y) -> Rnd [(PointXY, PointXY)]
connectGrid (nx, ny) = do
  let unconnected = S.fromList [ PointXY (x, y)
                               | x <- [0..nx-1], y <- [0..ny-1] ]
  -- Candidates are neighbours that are still unconnected. We start with
  -- a random choice.
  rx <- randomR (0, nx-1)
  ry <- randomR (0, ny-1)
  let candidates = S.fromList [ PointXY (rx, ry) ]
  connectGrid' (nx, ny) unconnected candidates []

connectGrid' :: (X, Y) -> S.Set PointXY -> S.Set PointXY
             -> [(PointXY, PointXY)]
             -> Rnd [(PointXY, PointXY)]
connectGrid' (nx, ny) unconnected candidates acc
  | S.null candidates = return acc
  | otherwise = do
      c <- oneOf (S.toList candidates)
      -- potential new candidates:
      let ns = S.fromList $ vicinityCardinalXY (0, 0, nx-1, ny-1) c
          nu = S.delete c unconnected  -- new unconnected
          -- (new candidates, potential connections):
          (nc, ds) = S.partition (`S.member` nu) ns
      new <- if S.null ds
             then return id
             else do
               d <- oneOf (S.toList ds)
               return ((c, d) :)
      connectGrid' (nx, ny) nu (S.delete c (candidates `S.union` nc)) (new acc)

-- | Pick a single random connection between adjacent areas within a grid.
randomConnection :: (X, Y) -> Rnd (PointXY, PointXY)
randomConnection (nx, ny) =
  assert (nx > 1 && ny > 0 || nx > 0 && ny > 1 `blame` (nx, ny)) $ do
  rb <- binaryChoice False True
  if rb || not (ny > 1)
    then do
      rx  <- randomR (0, nx-2)
      ry  <- randomR (0, ny-1)
      return (PointXY (rx, ry), PointXY (rx+1, ry))
    else do
      rx  <- randomR (0, nx-1)
      ry  <- randomR (0, ny-2)
      return (PointXY (rx, ry), PointXY (rx, ry+1))

-- Plotting individual corridors between two areas

-- | The choice of horizontal and vertical orientation.
data HV = Horiz | Vert

-- | The coordinates of consecutive fields of a corridor.
type Corridor = [PointXY]

-- | Create a corridor, either horizontal or vertical, with
-- a possible intermediate part that is in the opposite direction.
mkCorridor :: HV            -- ^ orientation of the starting section
           -> PointXY       -- ^ starting point
           -> PointXY       -- ^ ending point
           -> Area          -- ^ the area containing the intermediate point
           -> Rnd Corridor  -- ^ straight sections of the corridor
mkCorridor hv (PointXY (x0, y0)) (PointXY (x1, y1)) b = do
  PointXY (rx, ry) <- xyInArea b
  case hv of
    Horiz -> return $ map PointXY [(x0, y0), (rx, y0), (rx, y1), (x1, y1)]
    Vert  -> return $ map PointXY [(x0, y0), (x0, ry), (x1, ry), (x1, y1)]

-- TODO: assert that sx1 <= tx0, etc.
-- | Try to connect two places with a corridor.
-- Choose entrances at least 4 tiles distant from the edges, if possible.
connectPlaces :: Area -> Area -> Rnd Corridor
connectPlaces sa@(_, _, sx1, sy1) ta@(tx0, ty0, _, _) = do
  let trim (x0, y0, x1, y1) =
        let trim4 (v0, v1) = if v1 - v0 < 8 then (v0, v1) else (v0 + 4, v1 - 4)
            (nx0, nx1) = trim4 (x0, x1)
            (ny0, ny1) = trim4 (y0, y1)
        in (nx0, ny0, nx1, ny1)
  PointXY (sx, sy) <- xyInArea $ trim sa
  PointXY (tx, ty) <- xyInArea $ trim ta
  let xarea = (sx1+2, min sy ty, tx0-2, max sy ty)
      yarea = (sx, sy1+2, tx, ty0-2)
      xyarea = (sx1+2, sy1+2, tx0-2, ty0-2)
  (hv, area) <- if validArea xyarea
                then fmap (\ hv -> (hv, xyarea)) (binaryChoice Horiz Vert)
                else if validArea xarea
                     then return (Horiz, xarea)
                     else return (Vert, normalizeArea yarea)  -- vertical bias
  let (p0, p1) = case hv of
        Horiz -> (PointXY (if trivialArea sa then sx else sx1 + 1, sy),
                  PointXY (if trivialArea ta then tx else tx0 - 1, ty))
        Vert  -> (PointXY (sx, if trivialArea sa then sy else sy1 + 1),
                  PointXY (tx, if trivialArea ta then ty else ty0 - 1))
  -- The condition imposed on mkCorridor are tricky: there might not always
  -- exist a good intermediate point if the places are allowed to be close
  -- together and then we let the intermediate part degenerate.
  mkCorridor hv p0 p1 area
