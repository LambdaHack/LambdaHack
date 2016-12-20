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
connectGrid :: ES.EnumSet Point -> (X, Y) -> Rnd [(Point, Point)]
connectGrid voidPlaces (nx, ny) = do
  let unconnected = ES.fromDistinctAscList [ Point x y
                                           | y <- [0..ny-1], x <- [0..nx-1] ]
  -- Candidates are neighbours that are still unconnected. We start with
  -- a random choice.
  p <- oneOf $ ES.toList $ unconnected ES.\\ voidPlaces
  let candidates = ES.singleton p
  connectGrid' voidPlaces (nx, ny) unconnected candidates []

connectGrid' :: ES.EnumSet Point -> (X, Y)
             -> ES.EnumSet Point -> ES.EnumSet Point
             -> [(Point, Point)]
             -> Rnd [(Point, Point)]
connectGrid' voidPlaces (nx, ny) unconnected candidates !acc
  | unconnected `ES.isSubsetOf` voidPlaces = return acc
  | otherwise = do
      let candidatesBest = candidates ES.\\ voidPlaces
      c <- oneOf $ ES.toList $ if ES.null candidatesBest
                               then candidates
                               else candidatesBest
      -- potential new candidates:
      let ns = ES.fromList $ vicinityCardinal nx ny c
          nu = ES.delete c unconnected  -- new unconnected
          -- (new candidates, potential connections):
          (nc, ds) = ES.partition (`ES.member` nu) ns
      new <- if ES.null ds
             then return id
             else do
               d <- oneOf (ES.toList ds)
               return (sortPoint (c, d) :)
      connectGrid' voidPlaces (nx, ny) nu
        (ES.delete c (candidates `ES.union` nc)) (new acc)

-- | Sort the sequence of two points, in the derived lexicographic order.
sortPoint :: (Point, Point) -> (Point, Point)
sortPoint (a, b) | a <= b    = (a, b)
                 | otherwise = (b, a)

-- | Pick a single random connection between adjacent areas within a grid.
randomConnection :: (X, Y) -> Rnd (Point, Point)
randomConnection (nx, ny) =
  assert (nx > 1 && ny > 0 || nx > 0 && ny > 1 `blame` (nx, ny)) $ do
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
-- There might not always exist a good intermediate point
-- if the places are allowed to be close together
-- and then we let the intermediate part degenerate.
mkCorridor :: HV            -- ^ orientation of the starting section
           -> Point         -- ^ starting point
           -> Bool          -- ^ starting is inside @FGround@ or @FFloor@
           -> Point         -- ^ ending point
           -> Bool          -- ^ ending is inside @FGround@ or @FFloor@
           -> Area          -- ^ the area containing the intermediate point
           -> Rnd Corridor  -- ^ straight sections of the corridor
mkCorridor hv (Point x0 y0) p0floor (Point x1 y1) p1floor area = do
  Point rxRaw ryRaw <- xyInArea area
  let (sx0, sy0, sx1, sy1) = fromArea area
      -- Avoid corridors that run along @FGround@ or @FFloor@ fence.
      rx = if | rxRaw == sx0 + 1 && p0floor -> sx0
              | rxRaw == sx1 - 1 && p1floor -> sx1
              | otherwise -> rxRaw
      ry = if | ryRaw == sy0 + 1 && p0floor -> sy0
              | ryRaw == sy1 - 1 && p1floor -> sy1
              | otherwise -> ryRaw
  return $! map (uncurry Point) $ case hv of
    Horiz -> [(x0, y0), (rx, y0), (rx, y1), (x1, y1)]
    Vert  -> [(x0, y0), (x0, ry), (x1, ry), (x1, y1)]

-- | Try to connect two interiors of places with a corridor.
-- Choose entrances some steps away from the edges, if the place
-- is big enough. Note that with @pfence == FNone@, the inner area considered
-- is the strict interior of the place, without the outermost tiles.
--
-- The corridor connects (touches) the inner areas and the turning point
-- of the corridor (if any) is outside of the outer areas
-- and inside the grid areas.
connectPlaces :: (Area, Area, Area) -> (Area, Area, Area)
              -> Rnd (Maybe Corridor)
connectPlaces (_, _, sg) (_, _, tg) | sg == tg = return Nothing
connectPlaces (sa, so, sg) (ta, to, tg) = do
  let trim area =
        let (x0, y0, x1, y1) = fromArea area
            dx = case (x1 - x0) `div` 2 of
              0 -> 0
              1 -> 1
              2 -> 1
              3 -> 1
              _ -> 3
            dy = case (y1 - y0) `div` 2 of
              0 -> 0
              1 -> 1
              2 -> 1
              3 -> 1
              _ -> 3
        in fromMaybe (assert `failure` area)
           $ toArea (x0 + dx, y0 + dy, x1 - dx, y1 - dy)
  Point sx sy <- xyInArea $ trim sa
  Point tx ty <- xyInArea $ trim ta
  -- If the place (e.g., void place) is trivial (1-tile wide, even with fence),
  -- overwrite it with corridor. The place may not even be built (e.g., void)
  -- and the overwrite ensures connections through it are not broken.
  let (_, _, sax1Raw, say1Raw) = fromArea sa  -- inner area
      (sax1, say1) = if isTrivialArea so
                     then (sax1Raw - 1, say1Raw - 1)
                     else (sax1Raw, say1Raw)
      (tax0Raw, tay0Raw, _, _) = fromArea ta
      (tax0, tay0) = if isTrivialArea to
                     then (tax0Raw + 1, tay0Raw + 1)
                     else (tax0Raw, tay0Raw)
      (_, _, sox1, soy1) = fromArea so  -- outer area
      (tox0, toy0, _, _) = fromArea to
      (sgx0, sgy0, sgx1, sgy1) = fromArea sg  -- grid area
      (tgx0, tgy0, tgx1, tgy1) = fromArea tg
      (hv, area, p0, p1)
        | sgx1 == tgx0 =
          let x0 = if sgy0 <= ty && ty <= sgy1 then sox1 + 1 else sgx1
              x1 = if tgy0 <= sy && sy <= tgy1 then tox0 - 1 else sgx1
          in case toArea (x0, min sy ty, x1, max sy ty) of
            Just a -> (Horiz, a, Point (sax1 + 1) sy, Point (tax0 - 1) ty)
            Nothing -> assert `failure` (sa, so, sg, ta, to, tg, sx, sy, tx, ty)
        | otherwise = assert (sgy1 == tgy0) $
          let y0 = if sgx0 <= tx && tx <= sgx1 then soy1 + 1 else sgy1
              y1 = if tgx0 <= sx && sx <= tgx1 then toy0 - 1 else sgy1
          in case toArea (min sx tx, y0, max sx tx, y1) of
            Just a -> (Vert, a, Point sx (say1 + 1), Point tx (tay0 - 1))
            Nothing -> assert `failure` (sa, so, sg, ta, to, tg, sx, sy, tx, ty)
  Just <$> mkCorridor hv p0 (sa == so) p1 (ta == to) area
