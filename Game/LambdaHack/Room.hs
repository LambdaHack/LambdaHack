-- | Generation of rooms from room kinds.
{-# LANGUAGE RankNTypes #-}
module Game.LambdaHack.Room
  ( TileMapXY, roomValid, buildFence, digRoom
  ) where

import qualified Data.Map as M
import qualified Data.List as L

import Game.LambdaHack.Geometry
import Game.LambdaHack.Area
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.RoomKind

-- | The sparse room and cave map.
type TileMapXY = M.Map (X, Y) (Kind.Id TileKind)

-- | Check if the area large enough for tiling the corner twice in each
-- diraction, with a possible one tile overlap.
roomValid :: Area      -- ^ the area to fill
          -> RoomKind  -- ^ the room kind to construct
          -> Bool
roomValid (x0, y0, x1, y1) RoomKind{..} =
  let dx = x1 - x0 + if rfence == FNone then 3 else 1
      dy = y1 - y0 + if rfence == FNone then 3 else 1
      dxcorner = case rtopLeft of [] -> 0 ; l : _ -> L.length l
      dycorner = L.length rtopLeft
  in dx >= 2 * dxcorner - 1 &&  dy >= 2 * dycorner - 1

buildFence :: Kind.Id TileKind -> Area -> TileMapXY
buildFence wallId (x0, y0, x1, y1) =
  M.fromList $ [ ((x, y), wallId) | x <- [x0-1, x1+1], y <- [y0..y1] ] ++
               [ ((x, y), wallId) | x <- [x0-1..x1+1], y <- [y0-1, y1+1] ]

-- | Construct room of a given kind, with the given floor and wall tiles.
digRoom :: RoomKind
        -> M.Map Char (Kind.Id TileKind)
        -> Kind.Id TileKind -> Kind.Id TileKind -> Kind.Id TileKind
        -> Area
        -> TileMapXY
digRoom rk defLegend floorId wallId doorId area =
  let (roomArea, fence) = case rfence rk of
        FWall  -> (area, buildFence wallId area)
        FFloor -> (area, buildFence floorId area)
        FNone  -> (expand area 1, M.empty)
      legend = M.insert '.' floorId $
               M.insert '#' wallId $
               M.insert '+' doorId defLegend
  in M.union (M.map (legend M.!) $ tileRoom roomArea rk) fence

-- | Create the room by tiling patterns.
tileRoom :: Area                           -- ^ the area to fill
         -> RoomKind                       -- ^ the room kind to construct
         -> M.Map (X, Y) Char
tileRoom (x0, y0, x1, y1) RoomKind{..} =
  let dx = x1 - x0 + 1
      dy = y1 - y0 + 1
      fromX (x, y) = L.zip [x..] (repeat y)
      fillInterior :: (forall a. Int -> [a] -> [a]) -> [((X, Y), Char)]
      fillInterior f =
        let tileInterior (y, row) = L.zip (fromX (x0, y)) $ f dx row
            reflected = L.zip [y0..] $ f dy rtopLeft
        in L.concatMap tileInterior reflected
      tileReflect :: Int -> [a] -> [a]
      tileReflect d pat =
        let lstart = L.take (d `divUp` 2) pat
            lend   = L.take (d `div`   2) pat
        in lstart ++ L.reverse lend
      interior = case rcover of
        CTile    ->
          let tile :: Int -> [a] -> [a]
              tile d pat = L.take d (L.cycle pat)
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
