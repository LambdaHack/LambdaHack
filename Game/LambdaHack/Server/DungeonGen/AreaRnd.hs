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
import Data.Maybe

import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.VectorXY
import Game.LambdaHack.Server.DungeonGen.Area
import Game.LambdaHack.Utils.Assert

-- Picking random points inside areas

-- | Pick a random point within an area.
xyInArea :: Area -> Rnd PointXY
xyInArea area = do
  let (x0, y0, x1, y1) = fromArea area
  rx <- randomR (x0, x1)
  ry <- randomR (y0, y1)
  return $ PointXY (rx, ry)

-- | Create a random room according to given parameters.
mkRoom :: (X, Y)    -- ^ minimum size
       -> (X, Y)    -- ^ maximum size
       -> Area      -- ^ the containing area, not the room itself
       -> Rnd Area
mkRoom (xm, ym) (xM, yM) area = do
  let (x0, y0, x1, y1) = fromArea area
  assert (xm <= x1 - x0 + 1 && ym <= y1 - y0 + 1) skip
  let a0 = (x0, y0, x1 - xm + 1, y1 - ym + 1)
      area0 = fromMaybe (assert `failure` a0) $ toArea a0
  PointXY (rx0, ry0) <- xyInArea area0
  let sX = rx0 + xm - 1
      sY = ry0 + ym - 1
      eX = min x1 (rx0 + xM - 1)
      eY = min y1 (ry0 + yM - 1)
      a1 = (sX, sY, eX, eY)
      area1 = fromMaybe (assert `failure` a1) $ toArea a1
  PointXY (rx1, ry1) <- xyInArea area1
  let a3 = (rx0, ry0, rx1, ry1)
      area3 = fromMaybe (assert `failure` a3) $ toArea a3
  return area3

-- | Create a void room, i.e., a single point area within the designated area.
mkVoidRoom :: Area -> Rnd Area
mkVoidRoom area = do
  -- Pass corridors closer to the middle of the grid area, if possible.
  let core = fromMaybe area $ shrink area
  pxy <- xyInArea core
  return $ trivialArea pxy

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
                                               `twith` (nx, ny)) $ do
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
connectPlaces :: (Area, Area) -> (Area, Area) -> Rnd Corridor
connectPlaces (sa, so) (ta, to) = do
  let (_, _, sx1, sy1) = fromArea sa
      (_, _, sox1, soy1) = fromArea so
      (tx0, ty0, _, _) = fromArea ta
      (tox0, toy0, _, _) = fromArea to
  assert (sx1 <= tx0  || sy1 <= ty0  `blame` (sa, ta)) skip
  assert (sx1 <= sox1 || sy1 <= soy1 `blame` (sa, so)) skip
  assert (tx0 >= tox0 || ty0 >= toy0 `blame` (ta, to)) skip
  let trim area =
        let (x0, y0, x1, y1) = fromArea area
            trim4 (v0, v1) | v1 - v0 < 6 = (v0, v1)
                           | v1 - v0 < 8 = (v0 + 3, v1 - 3)
                           | otherwise = (v0 + 4, v1 - 4)
            (nx0, nx1) = trim4 (x0, x1)
            (ny0, ny1) = trim4 (y0, y1)
        in fromMaybe (assert `failure` area) $ toArea (nx0, ny0, nx1, ny1)
  PointXY (sx, sy) <- xyInArea $ trim so
  PointXY (tx, ty) <- xyInArea $ trim to
  let hva sarea tarea = do
        let (_, _, zsx1, zsy1) = fromArea sarea
            (ztx0, zty0, _, _) = fromArea tarea
            xa = (zsx1+2, min sy ty, ztx0-2, max sy ty)
            ya = (min sx tx, zsy1+2, max sx tx, zty0-2)
            xya = (zsx1+2, zsy1+2, ztx0-2, zty0-2)
        case toArea xya of
          Just xyarea -> fmap (\hv -> (hv, Just xyarea)) (oneOf [Horiz, Vert])
          Nothing ->
            case toArea xa of
              Just xarea -> return (Horiz, Just xarea)
              Nothing -> return (Vert, toArea ya) -- Vertical bias.
  (hvOuter, areaOuter) <- hva so to
  (hv, area) <- case areaOuter of
    Just arenaOuter -> return (hvOuter, arenaOuter)
    Nothing -> do
      -- TODO: let mkCorridor only pick points on the floor fence
      (hvInner, aInner) <- hva sa ta
      let yell = assert `failure` (sa, so, ta, to, areaOuter, aInner)
          areaInner = fromMaybe yell aInner
      return (hvInner, areaInner)
  -- We cross width one places completely with the corridor, for void
  -- rooms and others (e.g., one-tile wall room then becomes a door, etc.).
  let (p0, p1) = case hv of
        Horiz -> (PointXY (sox1, sy), PointXY (tox0, ty))
        Vert  -> (PointXY (sx, soy1), PointXY (tx, toy0))
  -- The condition imposed on mkCorridor are tricky: there might not always
  -- exist a good intermediate point if the places are allowed to be close
  -- together and then we let the intermediate part degenerate.
  mkCorridor hv p0 p1 area
