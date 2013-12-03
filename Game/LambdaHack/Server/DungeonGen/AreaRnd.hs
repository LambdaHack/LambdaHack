-- | Operations on the 'Area' type that involve random numbers.
module Game.LambdaHack.Server.DungeonGen.AreaRnd
  ( -- * Picking points inside areas
    xyInArea, mkRoom, mkVoidRoom
    -- * Choosing connections
  , connectGrid, randomConnection
    -- * Plotting corridors
  , Corridor, connectPlaces
  ) where

import qualified Data.EnumSet as ES

import Game.LambdaHack.Common.Area
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Utils.Assert

-- Picking random points inside areas

-- | Pick a random point within an area.
xyInArea :: Area -> Rnd PointXY
xyInArea (x0, y0, x1, y1) = do
  rx <- randomR (x0, x1)
  ry <- randomR (y0, y1)
  return $ PointXY (rx, ry)

-- | Create a random room according to given parameters.
mkRoom :: (X, Y)    -- ^ minimum size
       -> (X, Y)    -- ^ maximum size
       -> Area      -- ^ the containing area, not the room itself
       -> Rnd Area
mkRoom (xm, ym) (xM, yM) (x0, y0, x1, y1) = do
  assert (xm <= x1 - x0 + 1 && ym <= y1 - y0 + 1) skip
  let area0 = (x0, y0, x1 - xm + 1, y1 - ym + 1)
  assert (validArea area0 `blame` area0) skip
  PointXY (rx0, ry0) <- xyInArea area0
  let sX = rx0 + xm - 1
      sY = ry0 + ym - 1
      eX = min x1 (rx0 + xM - 1)
      eY = min y1 (ry0 + yM - 1)
      area1 = (sX, sY, eX, eY)
  assert (validArea area1 `blame` area1) skip
  PointXY (rx1, ry1) <- xyInArea area1
  return (rx0, ry0, rx1, ry1)

-- | Create a void room, i.e., a single point area within the designated area.
mkVoidRoom :: Area
           -> Rnd Area
mkVoidRoom area = do
  let shrunk = expand (-1) area
      -- Pass corridors closer to the middle of the grid area, if possible.
      core = if validArea shrunk then shrunk else area
  PointXY (ry, rx) <- xyInArea core
  return (ry, rx, ry, rx)

-- Choosing connections between areas in a grid

-- | Pick a subset of connections between adjacent areas within a grid until
-- there is only one connected component in the graph of all areas.
connectGrid :: (X, Y) -> Rnd [(PointXY, PointXY)]
connectGrid (nx, ny) = do
  let unconnected = ES.fromList [ PointXY (x, y)
                                | x <- [0..nx-1], y <- [0..ny-1] ]
  -- Candidates are neighbours that are still unconnected. We start with
  -- a random choice.
  rx <- randomR (0, nx-1)
  ry <- randomR (0, ny-1)
  let candidates = ES.fromList [ PointXY (rx, ry) ]
  connectGrid' (nx, ny) unconnected candidates []

connectGrid' :: (X, Y) -> ES.EnumSet PointXY -> ES.EnumSet PointXY
             -> [(PointXY, PointXY)]
             -> Rnd [(PointXY, PointXY)]
connectGrid' (nx, ny) unconnected candidates acc
  | ES.null candidates = return $ map sortPointXY acc
  | otherwise = do
      c <- oneOf (ES.toList candidates)
      -- potential new candidates:
      let ns = ES.fromList $ vicinityCardinalXY (0, 0, nx-1, ny-1) c
          nu = ES.delete c unconnected  -- new unconnected
          -- (new candidates, potential connections):
          (nc, ds) = ES.partition (`ES.member` nu) ns
      new <- if ES.null ds
             then return id
             else do
               d <- oneOf (ES.toList ds)
               return ((c, d) :)
      connectGrid' (nx, ny) nu
        (ES.delete c (candidates `ES.union` nc)) (new acc)

-- | Pick a single random connection between adjacent areas within a grid.
randomConnection :: (X, Y) -> Rnd (PointXY, PointXY)
randomConnection (nx, ny) =
  assert (nx > 1 && ny > 0 || nx > 0 && ny > 1 `blame` "wrong connection"
                                               `with` (nx, ny)) $ do
  rb <- oneOf [False, True]
  if rb || ny <= 1
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

-- | Try to connect two interiors of places with a corridor.
-- Choose entrances at least 4 or 3 tiles distant from the edges, if the place
-- is big enough. Note that with @pfence == FNone@, the area considered
-- is the strict interior of the place, without the outermost tiles.
connectPlaces :: Area -> Area -> Rnd Corridor
connectPlaces sa@(_, _, sx1, sy1) ta@(tx0, ty0, _, _) = do
  assert (validArea sa && validArea ta `blame` (sa, ta)) skip
  assert (sx1 < tx0 - 1 || sy1 < ty0 - 1 `blame` (sa, ta)) skip
  let trim (x0, y0, x1, y1) =
        let trim4 (v0, v1) | v1 - v0 < 6 = (v0, v1)
                           | v1 - v0 < 8 = (v0 + 3, v1 - 3)
                           | otherwise = (v0 + 4, v1 - 4)
            (nx0, nx1) = trim4 (x0, x1)
            (ny0, ny1) = trim4 (y0, y1)
        in (nx0, ny0, nx1, ny1)
  PointXY (sx, sy) <- xyInArea $ trim sa
  PointXY (tx, ty) <- xyInArea $ trim ta
  let xarea = (sx1+2, min sy ty, tx0-2, max sy ty)
      yarea = (sx, sy1+2, tx, ty0-2)
      xyarea = (sx1+2, sy1+2, tx0-2, ty0-2)
  (hv, area) <- if validArea xyarea
                then fmap (\ hv -> (hv, xyarea)) (oneOf [Horiz, Vert])
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
