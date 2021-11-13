-- | Operations on the 'Area' type that involve random numbers.
module Game.LambdaHack.Server.DungeonGen.AreaRnd
  ( -- * Picking points inside areas
    mkFixed, pointInArea, findPointInArea, mkVoidRoom, mkRoom
    -- * Choosing connections
  , connectGrid, randomConnection
    -- * Plotting corridors
  , HV(..), Corridor, connectPlaces
  , SpecialArea(..), grid
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , connectGrid', sortPoint, mkCorridor, borderPlace
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Functor.Identity (runIdentity)
import qualified Data.IntSet as IS

import Game.LambdaHack.Common.Area
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Core.Random
import Game.LambdaHack.Definition.Defs

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
  pxy <- randomR0 (xspan * yspan - 1)
  let Point{..} = punindex xspan pxy
  return $! Point (x0 + px) (y0 + py)

-- | Find a suitable position in the area, based on random points
-- and a predicate.
findPointInArea :: Area -> (Point -> Maybe Point)
                -> Int -> (Point -> Maybe Point)
                -> Rnd (Maybe Point)
findPointInArea area g gnumTries f =
  let (Point x0 y0, xspan, yspan) = spanArea area
      checkPoint :: Applicative m
                 => (Point -> Maybe Point) -> m (Maybe Point) -> Int
                 -> m (Maybe Point)
      {-# INLINE checkPoint #-}
      checkPoint check fallback pxyRelative =
        let Point{..} = punindex xspan pxyRelative
            pos = Point (x0 + px) (y0 + py)
        in case check pos of
          Just p -> pure $ Just p
          Nothing -> fallback
      gsearch 0 = fsearch (xspan * yspan * 10)
      gsearch count = do
        pxy <- randomR0 (xspan * yspan - 1)
        checkPoint g (gsearch (count - 1)) pxy
      fsearch 0 = return $! runIdentity $ searchAll (xspan * yspan - 1)
      fsearch count = do
        pxy <- randomR0 (xspan * yspan - 1)
        checkPoint f (fsearch (count - 1)) pxy
      searchAll (-1) = pure Nothing
      searchAll pxyRelative =
        checkPoint f (searchAll (pxyRelative - 1)) pxyRelative
  in gsearch gnumTries

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
  p <- oneOf $ ES.elems $ unconnected ES.\\ voidPlaces
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
      c <- oneOf $ ES.elems $ if ES.null candidatesBest
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
               d <- oneOf (ES.elems ds)
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
  if rb && nx > 1 || ny <= 1
  then do
    rx <- randomR0 (nx - 2)
    ry <- randomR0 (ny - 1)
    return (Point rx ry, Point (rx+1) ry)
  else do
    rx <- randomR0 (nx - 1)
    ry <- randomR0 (ny - 2)
    return (Point rx ry, Point rx (ry+1))

-- Plotting individual corridors between two areas

-- | The choice of horizontal and vertical orientation.
data HV = Horiz | Vert
  deriving Eq

-- | The coordinates of consecutive fields of a corridor.
type Corridor = (Point, Point, Point, Point)

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
      -- Avoid corridors that run along @FGround@ or @FFloor@ fence,
      -- unless not possible.
      rx = if | rxRaw == sx0 + 1 && p0floor -> sx0
              | rxRaw == sx1 - 1 && p1floor -> sx1
              | otherwise -> rxRaw
      ry = if | ryRaw == sy0 + 1 && p0floor -> sy0
              | ryRaw == sy1 - 1 && p1floor -> sy1
              | otherwise -> ryRaw
  return $! case hv of
    Horiz -> (Point x0 y0, Point rx y0, Point rx y1, Point x1 y1)
    Vert  -> (Point x0 y0, Point x0 ry, Point x1 ry, Point x1 y1)

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
  let (sa, so, stiny) = borderPlace sqarea spfence
      (ta, to, ttiny) = borderPlace tqarea tpfence
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
  -- If the place (e.g., void place) is slim (at most 2-tile wide, no fence),
  -- overwrite it with corridor. The place may not even be built (e.g., void)
  -- and the overwrite ensures connections through it are not broken.
  let (_, _, sax1Raw, say1Raw) = fromArea sa  -- inner area
      sslim = stiny && spfence == FNone
      (sax1, say1) = if sslim
                     then (sax1Raw - 1, say1Raw - 1)
                     else (sax1Raw, say1Raw)
      (tax0Raw, tay0Raw, _, _) = fromArea ta
      tslim = ttiny && tpfence == FNone
      (tax0, tay0) = if tslim
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
      nin p = not $ inside sa p || inside ta p
      !_A = assert (sslim || tslim
                    || allB nin [p0, p1] `blame` (sx, sy, tx, ty, s3, t3)) ()
  cor@(c1, c2, c3, c4) <- mkCorridor hv p0 (sa == so) p1 (ta == to) area
  let !_A2 = assert (sslim || tslim || allB nin [c1, c2, c3, c4]
                     `blame` (cor, sx, sy, tx, ty, s3, t3)) ()
  return $ Just cor

borderPlace :: Area -> Fence -> (Area, Area, Bool)
borderPlace qarea pfence = case pfence of
  FWall -> (qarea, expand qarea, False)
  FFloor  -> (qarea, qarea, False)
  FGround -> (qarea, qarea, False)
  FNone -> case shrink qarea of
    Nothing -> (qarea, qarea, True)
    Just sr -> (sr, qarea, False)

data SpecialArea =
    SpecialArea Area
  | SpecialFixed Point (Freqs PlaceKind) Area
  | SpecialMerged SpecialArea Point
  deriving Show

-- | Divide uniformly a larger area into the given number of smaller areas
-- overlapping at the edges.
--
-- The list of fixed centers (some important points inside)
-- of (non-overlapping) areas is given. Incorporate those,
-- with as little disruption, as possible.
-- Assume each of four boundaries of the cave are covered by a fixed centre.
grid :: EM.EnumMap Point (Freqs PlaceKind) -> [Point] -> Area -> (X, Y)
     -> ((X, Y), EM.EnumMap Point SpecialArea)
grid fixedCenters boot area cellSize =
  let (x0, y0, x1, y1) = fromArea area
      f zsize z1 n prev (c1 : c2 : rest) =
        let len = c2 - c1
            cn = len * n `div` zsize
        in -- traceShow ( zsize, z1, n, prev, len, cn
           --           , len `div` max 1 (2 * cn) ) $
           if cn < 2
           then let mid1 = (c1 + c2) `div` 2
                    mid2 = (c1 + c2) `divUp` 2
                    mid = if mid1 - prev > 4 then mid1 else mid2
                in (prev, mid, Just c1) : f zsize z1 n mid (c2 : rest)
           else (prev, c1 + len `div` (2 * cn), Just c1)
                : [ ( c1 + len * (2 * z - 1) `div` (2 * cn)
                    , c1 + len * (2 * z + 1) `div` (2 * cn)
                    , Nothing )
                  | z <- [1 .. cn - 1] ]
                ++ f zsize z1 n (c1 + len * (2 * cn - 1) `div` (2 * cn))
                     (c2 : rest)
      f _ z1 _ prev [c1] = [(prev, z1, Just c1)]
      f _ _ _ _ [] = error $ "empty list of centers" `showFailure` fixedCenters
      (xCenters, yCenters) = IS.fromList *** IS.fromList
                             $ unzip $ map (px &&& py) $ EM.keys fixedCenters
      distFromIS is z =
        - minimum (maxBound : map (\i -> abs (i - z)) (IS.toList is))
      xboot = nub $ sortOn (distFromIS xCenters)
              $ filter (`IS.notMember` xCenters) $ map px boot
      yboot = nub $ sortOn (distFromIS yCenters)
              $ filter (`IS.notMember` yCenters) $ map py boot
      -- Don't let boots ignore cell size too much, esp. in small caves.
      xcellsInArea = (x1 - x0 + 1) `div` fst cellSize
      ycellsInArea = (y1 - y0 + 1) `div` snd cellSize
      xbootN = assert (xcellsInArea > 0) $ xcellsInArea - IS.size xCenters
      ybootN = assert (ycellsInArea > 0) $ ycellsInArea - IS.size yCenters
      xset = xCenters `IS.union` IS.fromList (take xbootN xboot)
      yset = yCenters `IS.union` IS.fromList (take ybootN yboot)
      xsize = IS.findMax xset - IS.findMin xset
      ysize = IS.findMax yset - IS.findMin yset
      -- This is precisely how the cave will be divided among places,
      -- if there are no fixed centres except at boot coordinates.
      -- In any case, places, except for at boot points and fixed centres,
      -- are guaranteed at least the rolled minimal size of their
      -- enclosing cell (with one shared fence). Fixed centres are guaranteed
      -- a size between the cave cell size and the one implied by their
      -- placement wrt to cave fence and other fixed centers.
      lgrid = ( xsize `div` fst cellSize
              , ysize `div` snd cellSize )
      xallSegments = zip [0..] $ f xsize x1 (fst lgrid) x0 $ IS.toList xset
      yallSegments = zip [0..] $ f ysize y1 (snd lgrid) y0 $ IS.toList yset
  in -- traceShow (xallSegments, yallSegments) $
     ( (length xallSegments, length yallSegments)
     , EM.fromDistinctAscList
         [ ( Point x y
           , case (mcx, mcy) of
               (Just cx, Just cy) ->
                 case EM.lookup (Point cx cy) fixedCenters of
                   Nothing -> SpecialArea sarea
                   Just placeFreq -> SpecialFixed (Point cx cy) placeFreq sarea
               _ -> SpecialArea sarea )
         | (y, (cy0, cy1, mcy)) <- yallSegments
         , (x, (cx0, cx1, mcx)) <- xallSegments
         , let sarea = fromMaybe (error $ "" `showFailure` (x, y))
                       $ toArea (cx0, cy0, cx1, cy1) ] )
