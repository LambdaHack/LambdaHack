-- | Generation of places from place kinds.
{-# LANGUAGE RankNTypes #-}
module Game.LambdaHack.Place
  ( TileMapXY, Place(..), placeValid, buildFence, digPlace
  ) where

import Data.Binary
import Control.Monad
import qualified Data.Map as M
import qualified Data.List as L

import Game.LambdaHack.Content.PlaceKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Area
import Game.LambdaHack.Geometry
import Game.LambdaHack.Content.TileKind

data Place = Place
  { qkind :: !(Kind.Id PlaceKind)
  , qarea :: !Area
  , qseen :: !Bool
  }
  deriving Show

instance Binary Place where
  put Place{..} = do
    put qkind
    put qarea
    put qseen
  get = do
    qkind <- get
    qarea <- get
    qseen <- get
    return Place{..}

-- | The sparse place and cave map.
type TileMapXY = M.Map (X, Y) (Kind.Id TileKind)

-- | For @CAlternate@ tiling, require the place be comprised
-- of an even number of whole corners, with exactly one square
-- overlap between consecutive coners and no trimming.
-- For other tiling methods, check that the area large enough for tiling
-- the corner twice in each direction, with a possible one row/column overlap..
placeValid :: Area      -- ^ the area to fill
           -> PlaceKind  -- ^ the place kind to construct
           -> Bool
placeValid (x0, y0, x1, y1) PlaceKind{..} =
  let extra = case pfence of
        FWall  -> 1
        FFloor -> -1
        FNone  -> 3
      dx = x1 - x0 + extra
      dy = y1 - y0 + extra
      dxcorner = case ptopLeft of [] -> 0 ; l : _ -> L.length l
      dycorner = L.length ptopLeft
      wholeOverlapped d dcorner = d > 1 && dcorner > 1 &&
                                  (d - 1) `mod` (2 * (dcorner - 1)) == 0
  in case pcover of
    CAlternate -> wholeOverlapped dx dxcorner &&
                  wholeOverlapped dy dycorner
    _          -> dx >= 2 * dxcorner - 1 &&  dy >= 2 * dycorner - 1

buildFence :: Kind.Id TileKind -> Area -> TileMapXY
buildFence wallId (x0, y0, x1, y1) =
  M.fromList $ [ ((x, y), wallId) | x <- [x0-1, x1+1], y <- [y0..y1] ] ++
               [ ((x, y), wallId) | x <- [x0-1..x1+1], y <- [y0-1, y1+1] ]

-- | Construct place of a given kind, with the given floor and wall tiles.
digPlace :: Kind.Id PlaceKind -> PlaceKind
         -> M.Map Char (Kind.Id TileKind)
         -> Kind.Id TileKind -> Kind.Id TileKind -> Kind.Id TileKind
         -> Area
         -> (TileMapXY, Place)
digPlace placeId rk defLegend floorId wallId corId area =
  let (placeArea, fence) = case pfence rk of
        FWall  -> (area, buildFence wallId area)
        FFloor -> (expand area (-1), buildFence corId $ expand area (-1))
        FNone  -> (expand area 1, M.empty)
      legend = M.insert '.' floorId $
               M.insert '#' wallId defLegend
      tmap = M.union (M.map (legend M.!) $ tilePlace placeArea rk) fence
  in (tmap, Place placeId placeArea False)

-- | Create the place by tiling patterns.
tilePlace :: Area                           -- ^ the area to fill
         -> PlaceKind                      -- ^ the place kind to construct
         -> M.Map (X, Y) Char
tilePlace (x0, y0, x1, y1) PlaceKind{..} =
  let dx = x1 - x0 + 1
      dy = y1 - y0 + 1
      fromX (x, y) = L.zip [x..] (repeat y)
      fillInterior :: (forall a. Int -> [a] -> [a]) -> [((X, Y), Char)]
      fillInterior f =
        let tileInterior (y, row) = L.zip (fromX (x0, y)) $ f dx row
            reflected = L.zip [y0..] $ f dy ptopLeft
        in L.concatMap tileInterior reflected
      tileReflect :: Int -> [a] -> [a]
      tileReflect d pat =
        let lstart = L.take (d `divUp` 2) pat
            lend   = L.take (d `div`   2) pat
        in lstart ++ L.reverse lend
      interior = case pcover of
        CAlternate ->
          let tile :: Int -> [a] -> [a]
              tile d pat =
                L.take d (L.cycle $ L.init pat ++ L.init (L.reverse pat))
          in fillInterior tile
        CStretch ->
          let stretch :: Int -> [a] -> [a]
              stretch d pat = tileReflect d (pat ++ L.repeat (L.last pat))
          in fillInterior stretch
        CReflect ->
          let reflect :: Int -> [a] -> [a]
              reflect d pat = tileReflect d (L.cycle pat)
          in fillInterior reflect
  in M.fromList interior
