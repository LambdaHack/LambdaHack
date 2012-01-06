module Game.LambdaHack.AreaRnd where

import qualified Data.List as L
import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Geometry
import Game.LambdaHack.Area
import Game.LambdaHack.Random

xyInArea :: Area -> Rnd (X, Y)
xyInArea (x0, y0, x1, y1) = do
  rx <- randomR (x0, x1)
  ry <- randomR (y0, y1)
  return (rx, ry)

connectGrid' :: (X, Y) -> S.Set (X, Y) -> S.Set (X, Y) -> [((X, Y), (X, Y))]
             -> Rnd [((X, Y), (X, Y))]
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

connectGrid :: (X, Y) -> Rnd [((X, Y), (X, Y))]
connectGrid (nx, ny) = do
  let unconnected = S.fromList [ (x, y) | x <- [0..nx-1], y <- [0..ny-1] ]
  -- candidates are neighbors that are still unconnected; we start with
  -- a random choice
  rx <- randomR (0, nx-1)
  ry <- randomR (0, ny-1)
  let candidates  = S.fromList [ (rx, ry) ]
  connectGrid' (nx, ny) unconnected candidates []

randomConnection :: (X, Y) -> Rnd ((X, Y), (X, Y))
randomConnection (nx, ny) =
  assert (nx > 1 && ny > 0 || nx > 0 && ny > 1 `blame` (nx, ny)) $ do
  rb <- binaryChoice False True
  if rb || not (ny > 1)
    then do
      rx  <- randomR (0, nx-2)
      ry  <- randomR (0, ny-1)
      return (normalize ((rx, ry), (rx+1, ry)))
    else do
      rx  <- randomR (0, nx-1)
      ry  <- randomR (0, ny-2)
      return (normalize ((rx, ry), (rx, ry+1)))

data HV = Horiz | Vert

-- | Create a corridor, either horizontal or vertical, with
-- a possible intermediate part that is in the opposite direction.
mkCorridor :: HV -> ((X, Y), (X, Y)) -> Area
           -> Rnd [(X, Y)]  -- ^ straight sections of the corridor
mkCorridor hv ((x0, y0), (x1, y1)) b = do
  (rx, ry) <- xyInArea b
  -- (rx, ry) is intermediate point the path crosses
  -- hv decides whether we start in horizontal or vertical direction
  case hv of
    Horiz -> return [(x0, y0), (rx, y0), (rx, y1), (x1, y1)]
    Vert  -> return [(x0, y0), (x0, ry), (x1, ry), (x1, y1)]

-- | Try to connect two places with a corridor.
-- Choose entrances at least 4 tiles distant from the edges, if possible.
-- The condition passed to mkCorridor is tricky; there might not always
-- exist a suitable intermediate point if the places are allowed to be close
-- together.
connectPlaces :: Area -> Area -> Rnd [(X, Y)]
connectPlaces sa@(_, _, sx1, sy1) ta@(tx0, ty0, _, _) = do
  let trim (x0, y0, x1, y1) =
        let trim4 (v0, v1) = if v1 - v0 < 8 then (v0, v1) else (v0 + 4, v1 - 4)
            (nx0, nx1) = trim4 (x0, x1)
            (ny0, ny1) = trim4 (y0, y1)
        in (nx0, ny0, nx1, ny1)
  (sx, sy) <- xyInArea $ trim sa
  (tx, ty) <- xyInArea $ trim ta
  let xarea = (sx1+2, min sy ty, tx0-2, max sy ty)
      yarea = (sx, sy1+2, tx, ty0-2)
      xyarea = (sx1+2, sy1+2, tx0-2, ty0-2)
  (hv, area) <- if validArea xyarea
                then fmap (\ hv -> (hv, xyarea)) (binaryChoice Horiz Vert)
                else if validArea xarea
                     then return (Horiz, xarea)
                     else return (Vert, normalizeArea yarea)  -- vertical bias
  let ((x0, y0), (x1, y1)) =
        case hv of
          Horiz -> ((if trivialArea sa then sx else sx1 + 1, sy),
                    (if trivialArea ta then tx else tx0 - 1, ty))
          Vert  -> ((sx, if trivialArea sa then sy else sy1 + 1),
                    (tx, if trivialArea ta then ty else ty0 - 1))
  mkCorridor hv ((x0, y0), (x1, y1)) area
