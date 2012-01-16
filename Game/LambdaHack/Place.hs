-- | Generation of places from place kinds.
{-# LANGUAGE RankNTypes #-}
module Game.LambdaHack.Place
  ( TileMapXY, Place(..), placeValid, buildFence, addPlace
  ) where

import Data.Binary
import Control.Monad
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Content.PlaceKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Area
import Game.LambdaHack.Geometry
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Random

-- | The parameters with which a place was generated.
data Place = Place
  { qkind        :: !(Kind.Id PlaceKind)
  , qarea        :: !Area
  , qseen        :: !Bool
  , qlegend      :: !String
  , qsolidFence  :: !(Kind.Id TileKind)
  , qhollowFence :: !(Kind.Id TileKind)
  }
  deriving Show

instance Binary Place where
  put Place{..} = do
    put qkind
    put qarea
    put qseen
    put qlegend
    put qsolidFence
    put qhollowFence
  get = do
    qkind <- get
    qarea <- get
    qseen <- get
    qlegend <- get
    qsolidFence <- get
    qhollowFence <- get
    return Place{..}

-- | The map of tile kinds in a cave and any place in a cave.
-- The map is sparse. The default tile that eventually fills the empty spaces
-- is specified in the cave kind specification.
type TileMapXY = M.Map (X, Y) (Kind.Id TileKind)

-- | For @CAlternate@ tiling, require the place be comprised
-- of an even number of whole corners, with exactly one square
-- overlap between consecutive coners and no trimming.
-- For other tiling methods, check that the area large enough for tiling
-- the corner twice in each direction, with a possible one row/column overlap.
placeValid :: Area      -- ^ the area to fill
           -> PlaceKind  -- ^ the place kind to construct
           -> Bool
placeValid r PlaceKind{..} =
  let (x0, y0, x1, y1) = expandFence pfence r
      dx = x1 - x0 + 1
      dy = y1 - y0 + 1
      dxcorner = case ptopLeft of [] -> 0 ; l : _ -> L.length l
      dycorner = L.length ptopLeft
      wholeOverlapped d dcorner = d > 1 && dcorner > 1 &&
                                  (d - 1) `mod` (2 * (dcorner - 1)) == 0
  in case pcover of
    CAlternate -> wholeOverlapped dx dxcorner &&
                  wholeOverlapped dy dycorner
    _          -> dx >= 2 * dxcorner - 1 &&
                  dy >= 2 * dycorner - 1

expandFence :: Fence -> Area -> Area
expandFence fence r = case fence of
  FWall  -> r
  FFloor -> expand r (-1)
  FNone  -> expand r 1

addPlace :: Kind.COps -> Kind.Id TileKind -> Kind.Id TileKind
         -> RollQuad -> Int -> Int -> Area
         -> Rnd (TileMapXY, Place)
addPlace Kind.COps{cotile, coplace=Kind.Ops{okind=pokind, opick=popick}}
         qsolidFence qhollowFence cdarkChance lvl depth
         r = assert (not (trivialArea r) `blame` r) $ do
  dark <- chanceQuad lvl depth cdarkChance
  qkind <- popick "rogue" (placeValid r)
  let kr = pokind qkind
      qlegend = if dark then "darkLegend" else "litLegend"
      qseen = False
      qarea = expandFence (pfence kr) r
      place = assert (validArea qarea `blame` qarea) $
              Place{..}
  legend <- olegend cotile qlegend
  let xlegend = M.insert 'X' qhollowFence legend
  return (digPlace place kr xlegend, place)

olegend :: Kind.Ops TileKind -> String -> Rnd (M.Map Char (Kind.Id TileKind))
olegend Kind.Ops{ofoldrWithKey, opick} group =
  let getSymbols _ tk acc =
        maybe acc (const $ S.insert (tsymbol tk) acc)
          (L.lookup group $ tfreq tk)
      symbols = ofoldrWithKey getSymbols S.empty
      getLegend s acc = do
        m <- acc
        tk <- opick group $ (== s) . tsymbol
        return $ M.insert s tk m
      legend = S.fold getLegend (return M.empty) symbols
  in legend

buildFence :: Kind.Id TileKind -> Area -> TileMapXY
buildFence fenceId (x0, y0, x1, y1) =
  M.fromList $ [ ((x, y), fenceId) | x <- [x0-1, x1+1], y <- [y0..y1] ] ++
               [ ((x, y), fenceId) | x <- [x0-1..x1+1], y <- [y0-1, y1+1] ]

-- | Construct place of a given kind, with the given fence tile.
digPlace :: Place
         -> PlaceKind
         -> M.Map Char (Kind.Id TileKind)
         -> TileMapXY
digPlace Place{..} kr legend =
  let fence = case pfence kr of
        FWall  -> buildFence qsolidFence qarea
        FFloor -> buildFence qhollowFence qarea
        FNone  -> M.empty
  in M.union (M.map (legend M.!) $ tilePlace qarea kr) fence

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
