-- | Operations on the 'Area' type that involve random numbers.
module Game.LambdaHack.Server.DungeonGen.AreaRnd
  ( -- * Picking points inside areas
    xyInArea, mkVoidRoom, mkRoom, mkFixed
    -- * Choosing connections
  , connectGrid, randomConnection
    -- * Plotting corridors
  , HV(..), Corridor, connectPlaces
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumSet as ES

import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Server.DungeonGen.Area

-- Picking random points inside areas

-- | Pick a random point within an area.
xyInArea :: Area -> Rnd Point
xyInArea area = do
  let (x0, y0, x1, y1) = fromArea area
  rx <- randomR (x0, x1)
  ry <- randomR (y0, y1)
  return $! Point rx ry

-- | Create a void room, i.e., a single point area within the designated area.
mkVoidRoom :: Area -> Rnd Area
mkVoidRoom area = do
  -- Pass corridors closer to the middle of the grid area, if possible.
  let core = fromMaybe area $ shrink area
  pxy <- xyInArea core
  return $! trivialArea pxy

-- | Create a random room according to given parameters.
mkRoom :: (X, Y)    -- ^ minimum size
       -> (X, Y)    -- ^ maximum size
       -> Area      -- ^ the containing area, not the room itself
       -> Rnd Area
mkRoom (xm, ym) (xM, yM) area = do
  let (x0, y0, x1, y1) = fromArea area
      xspan = x1 - x0 + 1
      yspan = y1 - y0 + 1
      aW = (min xm xspan, min ym yspan, min xM xspan, min yM yspan)
      areaW = fromMaybe (assert `failure` aW) $ toArea aW
  Point xW yW <- xyInArea areaW  -- roll size
  let a1 = (x0, y0, max x0 (x1 - xW + 1), max y0 (y1 - yW + 1))
      area1 = fromMaybe (assert `failure` a1) $ toArea a1
  Point rx1 ry1 <- xyInArea area1  -- roll top-left corner
  let a3 = (rx1, ry1, rx1 + xW - 1, ry1 + yW - 1)
      area3 = fromMaybe (assert `failure` a3) $ toArea a3
  return $! area3

-- Doesn't respect minimum sizes, because staircases are specified verbatim,
-- so can't be arbitrarily scaled up.
-- The size may be one more than what maximal size hint requests,
-- but this is safe (limited by area size) and makes up for the rigidity
-- of the fixed room sizes (e.g., that the size is always odd).
mkFixed :: (X, Y)    -- ^ maximum size
        -> Area      -- ^ the containing area, not the room itself
        -> Point     -- ^ the center point
        -> Area
mkFixed (xM, yM) area p@Point{..} =
  let (x0, y0, x1, y1) = fromArea area
      xradius = min ((xM + 1) `div` 2) $ min (px - x0) (x1 - px)
      yradius = min ((yM + 1) `div` 2) $ min (py - y0) (y1 - py)
      a = (px - xradius, py - yradius, px + xradius, py + yradius)
  in fromMaybe (assert `failure` (a, xM, yM, area, p)) $ toArea a

-- Choosing connections between areas in a grid

-- | Pick a subset of connections between adjacent areas within a grid until
-- there is only one connected component in the graph of all areas.
connectGrid :: (X, Y) -> Rnd [(Point, Point)]
connectGrid (nx, ny) = do
  let unconnected = ES.fromDistinctAscList [ Point x y
                                           | y <- [0..ny-1], x <- [0..nx-1] ]
  -- Candidates are neighbours that are still unconnected. We start with
  -- a random choice.
  rx <- randomR (0, nx-1)
  ry <- randomR (0, ny-1)
  let candidates = ES.singleton $ Point rx ry
  connectGrid' (nx, ny) unconnected candidates []

connectGrid' :: (X, Y) -> ES.EnumSet Point -> ES.EnumSet Point
             -> [(Point, Point)]
             -> Rnd [(Point, Point)]
connectGrid' (nx, ny) unconnected candidates acc
  | ES.null candidates = return $! map sortPoint acc
  | otherwise = do
      c <- oneOf (ES.toList candidates)
      -- potential new candidates:
      let ns = ES.fromList $ vicinityCardinal nx ny c
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

-- | Sort the sequence of two points, in the derived lexicographic order.
sortPoint :: (Point, Point) -> (Point, Point)
sortPoint (a, b) | a <= b    = (a, b)
                 | otherwise = (b, a)

-- | Pick a single random connection between adjacent areas within a grid.
randomConnection :: (X, Y) -> Rnd (Point, Point)
randomConnection (nx, ny) =
  assert (nx > 1 && ny > 0 || nx > 0 && ny > 1 `blame` "wrong connection"
                                               `twith` (nx, ny)) $ do
  rb <- oneOf [False, True]
  if rb || ny <= 1
    then do
      rx  <- randomR (0, nx-2)
      ry  <- randomR (0, ny-1)
      return (Point rx ry, Point (rx+1) ry)
    else do
      rx  <- randomR (0, nx-1)
      ry  <- randomR (0, ny-2)
      return (Point rx ry, Point rx (ry+1))

-- Plotting individual corridors between two areas

-- | The choice of horizontal and vertical orientation.
data HV = Horiz | Vert
  deriving Eq

-- | The coordinates of consecutive fields of a corridor.
type Corridor = [Point]

-- | Create a corridor, either horizontal or vertical, with
-- a possible intermediate part that is in the opposite direction.
mkCorridor :: HV            -- ^ orientation of the starting section
           -> Point       -- ^ starting point
           -> Point       -- ^ ending point
           -> Area          -- ^ the area containing the intermediate point
           -> Rnd Corridor  -- ^ straight sections of the corridor
mkCorridor hv (Point x0 y0) (Point x1 y1) b = do
  Point rx ry <- xyInArea b
  return $! map (uncurry Point) $ case hv of
    Horiz -> [(x0, y0), (rx, y0), (rx, y1), (x1, y1)]
    Vert  -> [(x0, y0), (x0, ry), (x1, ry), (x1, y1)]

-- | Try to connect two interiors of places with a corridor.
-- Choose entrances some steps away from the edges, if the place
-- is big enough. Note that with @pfence == FNone@, the area considered
-- is the strict interior of the place, without the outermost tiles.
--
-- The corridor connects the inner areas and the turning point
-- of the corridor (if any) is outside the outer areas.
connectPlaces :: (Area, Area) -> (Area, Area) -> Rnd (Maybe Corridor)
connectPlaces (sa, so) (ta, to) | sa == ta && so == to = return Nothing
connectPlaces (sa, so) (ta, to) = do
  let (_, _, sx1, sy1) = fromArea sa    -- inner area
      (_, _, sox1, soy1) = fromArea so  -- outer area
      (tx0, ty0, _, _) = fromArea ta
      (tox0, toy0, _, _) = fromArea to
  let !_A = assert (sx1 <= tx0  || sy1 <= ty0 `blame` (sa, ta)) ()
      trim area =
        let (x0, y0, x1, y1) = fromArea area
            dx = min 2 $ (x1 - x0) `div` 2
            dy = min 2 $ (y1 - y0) `div` 2
        in fromMaybe (assert `failure` area)
           $ toArea (x0 + dx, y0 + dy, x1 - dx, y1 - dy)
  Point sx sy <- xyInArea $ trim so
  Point tx ty <- xyInArea $ trim to
  let hva d sarea tarea = do
        let (_, _, zsx1, zsy1) = fromArea sarea
            (ztx0, zty0, _, _) = fromArea tarea
            xa = (zsx1+d, min sy ty, ztx0-d, max sy ty)
            ya = (min sx tx, zsy1+d, max sx tx, zty0-d)
            xya = (zsx1+d, zsy1+d, ztx0-d, zty0-d)
        case toArea xya of
          Just xyarea -> Just ([Horiz, Vert], xyarea)
          Nothing -> case toArea xa of
            Just xarea -> Just ([Horiz], xarea) -- Horizontal bias.
            Nothing -> case toArea ya of
              Just yarea -> Just ([Vert], yarea)
              Nothing -> Nothing
      mhvsArea = hva 2 so to `mplus` hva 1 so to `mplus` hva 0 so to
  (hv, area) <- case mhvsArea of
     Nothing -> assert `failure` (sa, so, ta, to, sx, sy, tx, ty)
     Just (hvs, area) -> do
       hv <- oneOf hvs
       return (hv, area)
  let (p0, p1) = case hv of
        Horiz -> (Point sox1 sy, Point tox0 ty)
        Vert  -> (Point sx soy1, Point tx toy0)
  -- The condition imposed on mkCorridor are tricky: there might not always
  -- exist a good intermediate point if the places are allowed to be close
  -- together and then we let the intermediate part degenerate.
  Just <$> mkCorridor hv p0 p1 area
