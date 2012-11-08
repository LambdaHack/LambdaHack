-- | Generation of places from place kinds.
{-# LANGUAGE RankNTypes #-}
module Game.LambdaHack.Place
  ( TileMapXY, Place(..), placeValid, buildFence, buildPlace
  ) where

import Data.Binary
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Content.PlaceKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Area
import Game.LambdaHack.PointXY
import Game.LambdaHack.Misc
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Random
import Game.LambdaHack.Content.CaveKind

-- TODO: use more, rewrite as needed, document each field.
-- | The parameters of a place. Most are immutable and set
-- at the time when a place is generated.
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

-- | The map of tile kinds in a place (and generally anywhere in a cave).
-- The map is sparse. The default tile that eventually fills the empty spaces
-- is specified in the cave kind specification with @cdefaultTile@.
type TileMapXY = M.Map PointXY (Kind.Id TileKind)

-- | For @CAlternate@ tiling, require the place be comprised
-- of an even number of whole corners, with exactly one square
-- overlap between consecutive coners and no trimming.
-- For other tiling methods, check that the area is large enough for tiling
-- the corner twice in each direction, with a possible one row/column overlap.
placeValid :: Area       -- ^ the area to fill
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

-- | Modify available room area according to fence type.
expandFence :: Fence -> Area -> Area
expandFence fence r = case fence of
  FWall  -> r
  FFloor -> expand r (-1)
  FNone  -> expand r 1

-- | Given a few parameters, roll and construct a 'Place' datastructure
-- and fill a cave section acccording to it.
buildPlace :: Kind.COps         -- ^ the game content
           -> CaveKind          -- ^ current cave kind
           -> Kind.Id TileKind  -- ^ fence tile, if fence hollow
           -> Int               -- ^ current level depth
           -> Int               -- ^ maximum depth
           -> Area              -- ^ interior area of the place
           -> Rnd (TileMapXY, Place)
buildPlace Kind.COps{ cotile=cotile@Kind.Ops{opick=opick}
                    , coplace=Kind.Ops{okind=pokind, opick=popick} }
           CaveKind{..} qhollowFence ln depth r
           = assert (not (trivialArea r) `blame` r) $ do
  qsolidFence <- opick cfillerTile (const True)
  dark <- chanceDeep ln depth cdarkChance
  qkind <- popick "rogue" (placeValid r)
  let kr = pokind qkind
      qlegend = if dark then cdarkLegendTile else clitLegendTile
      qseen = False
      qarea = expandFence (pfence kr) r
      place = assert (validArea qarea `blame` qarea) $
              Place{..}
  legend <- olegend cotile qlegend
  let xlegend = M.insert 'X' qhollowFence legend
  return (digPlace place kr xlegend, place)

-- | Roll a legend of a place plan: a map from plan symbols to tile kinds.
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

-- | Construct a fence around an area, with the given tile kind.
buildFence :: Kind.Id TileKind -> Area -> TileMapXY
buildFence fenceId (x0, y0, x1, y1) =
  M.fromList $ [ (PointXY (x, y), fenceId)
               | x <- [x0-1, x1+1], y <- [y0..y1] ] ++
               [ (PointXY (x, y), fenceId)
               | x <- [x0-1..x1+1], y <- [y0-1, y1+1] ]

-- | Construct a place of the given kind, with the given fence tile.
digPlace :: Place                          -- ^ the place parameters
         -> PlaceKind                      -- ^ the place kind
         -> M.Map Char (Kind.Id TileKind)  -- ^ the legend
         -> TileMapXY
digPlace Place{..} kr legend =
  let fence = case pfence kr of
        FWall  -> buildFence qsolidFence qarea
        FFloor -> buildFence qhollowFence qarea
        FNone  -> M.empty
  in M.union (M.map (legend M.!) $ tilePlace qarea kr) fence

-- | Create a place by tiling patterns.
tilePlace :: Area                           -- ^ the area to fill
          -> PlaceKind                      -- ^ the place kind to construct
          -> M.Map PointXY Char
tilePlace (x0, y0, x1, y1) pl@PlaceKind{..} =
  let dx = x1 - x0 + 1
      dy = y1 - y0 + 1
      fromX (x, y) = L.map PointXY $ L.zip [x..] (repeat y)
      fillInterior :: (forall a. Int -> [a] -> [a]) -> [(PointXY, Char)]
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
              tile _ []  = assert `failure` pl
              tile d pat =
                L.take d (L.cycle $ L.init pat ++ L.init (L.reverse pat))
          in fillInterior tile
        CStretch ->
          let stretch :: Int -> [a] -> [a]
              stretch _ []  = assert `failure` pl
              stretch d pat = tileReflect d (pat ++ L.repeat (L.last pat))
          in fillInterior stretch
        CReflect ->
          let reflect :: Int -> [a] -> [a]
              reflect d pat = tileReflect d (L.cycle pat)
          in fillInterior reflect
  in M.fromList interior
