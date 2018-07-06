-- | Operations on the 'Area' type that involve random numbers.
module Game.LambdaHack.Server.DungeonGen.AreaRnd
  ( -- * Picking points inside areas
    mkFixed, pointInArea, findPointInArea, mkVoidRoom, mkRoom
    -- * Choosing connections
  , connectGrid, randomConnection
    -- * Plotting corridors
  , HV(..), Corridor, connectPlaces
  , SpecialArea(..), grid, anchorDown
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , connectGrid', sortPoint, mkCorridor, borderPlace
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.IntSet as IS

import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Misc hiding (xM)
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.PlaceKind

-- Doesn't respect minimum sizes, because staircases are specified verbatim,
-- so can't be arbitrarily scaled up.
-- The size may be one more than what maximal size hint requests,
-- but this is safe (limited by area size) and makes up for the rigidity
-- of the fixed room sizes (e.g., that the size is always odd).
mkFixed :: (X, Y)    -- ^ maximum size
        -> Area      -- ^ the containing area, not the room itself
        -> Point     -- ^ the center point
        -> Area
mkFixed (xMax, yMax) area p@Point{..} =
  let (x0, y0, x1, y1) = fromArea area
      xradius = min ((xMax + 1) `div` 2) $ min (px - x0) (x1 - px)
      yradius = min ((yMax + 1) `div` 2) $ min (py - y0) (y1 - py)
      a = (px - xradius, py - yradius, px + xradius, py + yradius)
  in fromMaybe (error $ "" `showFailure` (a, xMax, yMax, area, p)) $ toArea a

-- | Pick a random point within an area.
pointInArea :: Area -> Rnd Point
pointInArea area = do
  let (Point x0 y0, xspan, yspan) = spanArea area
  pxy <- randomR (0, xspan * yspan - 1)
  let Point{..} = PointArray.punindex xspan pxy
  return $! Point (x0 + px) (y0 + py)

-- | Find a suitable position in the area, based on random points
-- and a predicate.
findPointInArea :: Area -> (Point -> Maybe Point) -> Rnd Point
findPointInArea area f =
  let (Point x0 y0, xspan, yspan) = spanArea area
      search = do
        pxy <- randomR (0, xspan * yspan - 1)
        let Point{..} = PointArray.punindex xspan pxy
            pos = Point (x0 + px) (y0 + py)
        case f pos of
          Just p -> return p
          Nothing -> search
  in search

-- | Create a void room, i.e., a single point area within the designated area.
mkVoidRoom :: Area -> Rnd Area
mkVoidRoom area = do
  -- Pass corridors closer to the middle of the grid area, if possible.
  let core = fromMaybe area $ shrink area
  pxy <- pointInArea core
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
      areaW = fromMaybe (error $ "" `showFailure` aW) $ toArea aW
  Point xW yW <- pointInArea areaW  -- roll size
  let a1 = (x0, y0, max x0 (x1 - xW + 1), max y0 (y1 - yW + 1))
      area1 = fromMaybe (error $ "" `showFailure` a1) $ toArea a1
  Point rx1 ry1 <- pointInArea area1  -- roll top-left corner
  let a3 = (rx1, ry1, rx1 + xW - 1, ry1 + yW - 1)
      area3 = fromMaybe (error $ "" `showFailure` a3) $ toArea a3
  return $! area3

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
  if rb && nx > 1
  then do
    rx <- randomR (0, nx-2)
    ry <- randomR (0, ny-1)
    return (Point rx ry, Point (rx+1) ry)
  else do
    rx <- randomR (0, nx-1)
    ry <- randomR (0, ny-2)
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
  Point rxRaw ryRaw <- pointInArea area
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
connectPlaces :: (Area, Fence, Area) -> (Area, Fence, Area)
              -> Rnd (Maybe Corridor)
connectPlaces (_, _, sg) (_, _, tg) | sg == tg = return Nothing
connectPlaces s3@(sqarea, spfence, sg) t3@(tqarea, tpfence, tg) = do
  let (sa, so) = borderPlace sqarea spfence
      (ta, to) = borderPlace tqarea tpfence
      trim area =
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
        in fromMaybe (error $ "" `showFailure` (area, s3, t3))
           $ toArea (x0 + dx, y0 + dy, x1 - dx, y1 - dy)
  Point sx sy <- pointInArea $ trim sa
  Point tx ty <- pointInArea $ trim ta
  -- If the place (e.g., void place) is trivial (1-tile wide, no fence),
  -- overwrite it with corridor. The place may not even be built (e.g., void)
  -- and the overwrite ensures connections through it are not broken.
  let (_, _, sax1Raw, say1Raw) = fromArea sa  -- inner area
      strivial = isTrivialArea sqarea && spfence == FNone
      (sax1, say1) = if strivial
                     then (sax1Raw - 1, say1Raw - 1)
                     else (sax1Raw, say1Raw)
      (tax0Raw, tay0Raw, _, _) = fromArea ta
      ttrivial = isTrivialArea tqarea && tpfence == FNone
      (tax0, tay0) = if ttrivial
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
            Nothing -> error $ "" `showFailure` (sx, sy, tx, ty, s3, t3)
        | otherwise = assert (sgy1 == tgy0) $
          let y0 = if sgx0 <= tx && tx <= sgx1 then soy1 + 1 else sgy1
              y1 = if tgx0 <= sx && sx <= tgx1 then toy0 - 1 else sgy1
          in case toArea (min sx tx, y0, max sx tx, y1) of
            Just a -> (Vert, a, Point sx (say1 + 1), Point tx (tay0 - 1))
            Nothing -> error $ "" `showFailure` (sx, sy, tx, ty, s3, t3)
      nin p = not $ p `inside` sa || p `inside` ta
      !_A = assert (strivial || ttrivial
                    || allB nin [p0, p1]`blame` (sx, sy, tx, ty, s3, t3)) ()
  cor <- mkCorridor hv p0 (sa == so) p1 (ta == to) area
  let !_A2 = assert (strivial || ttrivial
                     || allB nin cor `blame` (sx, sy, tx, ty, s3, t3)) ()
  return $ Just cor

borderPlace :: Area -> Fence -> (Area, Area)
borderPlace qarea pfence = case pfence of
  FWall -> (qarea, expand qarea)
  FFloor  -> (qarea, qarea)
  FGround -> (qarea, qarea)
  FNone -> case shrink qarea of
    Nothing -> (qarea, qarea)
    Just sr -> (sr, qarea)

data SpecialArea =
    SpecialArea Area
  | SpecialFixed Point (GroupName PlaceKind) Area
  | SpecialMerged SpecialArea Point
  deriving Show

-- | Divide uniformly a larger area into the given number of smaller areas
-- overlapping at the edges.
--
-- When a list of fixed centers (some important points inside)
-- of (non-overlapping) areas is given, incorporate those,
-- with as little disruption, as possible.
grid :: EM.EnumMap Point (GroupName PlaceKind) -> [Point] -> (X, Y) -> Area
     -> ((X, Y), EM.EnumMap Point SpecialArea)
grid fixedCenters boot (nx, ny) area =
  let (x0, y0, x1, y1) = fromArea area
      f z0 z1 n prev (c1 : c2 : rest) =
        let len = c2 - c1 + 1
            cn = len * n `div` (z1 - z0 - 1)
        in if cn < 2
           then let mid1 = (c1 + c2) `div` 2
                    mid2 = (c1 + c2) `divUp` 2
                    mid = if mid1 - prev > 4 then mid1 else mid2
                in (prev, mid, Just c1) : f z0 z1 n mid (c2 : rest)
           else (prev, c1 + len `div` (2 * cn), Just c1)
                : [ ( c1 + len * (2 * z - 1) `div` (2 * cn)
                    , c1 + len * (2 * z + 1) `div` (2 * cn)
                    , Nothing )
                  | z <- [1 .. cn - 1] ]
                ++ f z0 z1 n (c1 + len * (2 * cn - 1) `div` (2 * cn))
                     (c2 : rest)
      f _ z1 _ prev [c1] = [(prev, z1, Just c1)]
      f _ _ _ _ [] = error $ "empty list of centers" `showFailure` fixedCenters
      xcs = IS.toList $ IS.fromList
            $ map px (EM.keys fixedCenters)
              ++ filter (\x -> x >= x0 + 4 && x <= x1 - 4) (map px boot)
      xallCenters = zip [0..] $ f x0 x1 nx x0 xcs
      ycs = IS.toList $ IS.fromList
            $ map py (EM.keys fixedCenters)
              ++ filter (\y -> y >= y0 + 3 && y <= y1 - anchorDown + 1)
                        (map py boot)
      yallCenters = zip [0..] $ f y0 y1 ny y0 ycs
  in ( (length xallCenters, length yallCenters)
     , EM.fromDistinctAscList
         [ ( Point x y
           , case (mcx, mcy) of
               (Just cx, Just cy) ->
                 case EM.lookup (Point cx cy) fixedCenters of
                   Nothing -> SpecialArea sarea
                   Just placeGroup ->
                     SpecialFixed (Point cx cy) placeGroup sarea
               _ -> SpecialArea sarea )
         | (y, (cy0, cy1, mcy)) <- yallCenters
         , (x, (cx0, cx1, mcx)) <- xallCenters
         , let sarea = fromMaybe (error $ "" `showFailure` (x, y))
                       $ toArea (cx0, cy0, cx1, cy1) ] )

anchorDown :: Y
anchorDown = 5  -- not 4, asymmetric vs up, for staircase variety
