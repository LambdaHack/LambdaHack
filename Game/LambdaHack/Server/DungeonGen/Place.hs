{-# LANGUAGE RankNTypes #-}
-- | Generation of places from place kinds.
module Game.LambdaHack.Server.DungeonGen.Place
  ( TileMapEM, Place(..), placeCheck, buildFenceRnd, buildPlace
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Server.DungeonGen.Area

-- TODO: use more, rewrite as needed, document each field.
-- | The parameters of a place. Most are immutable and set
-- at the time when a place is generated.
data Place = Place
  { qkind        :: !(Kind.Id PlaceKind)
  , qarea        :: !Area
  , qseen        :: !Bool
  , qlegend      :: !Text
  , qsolidFence  :: !(Kind.Id TileKind)
  , qhollowFence :: !(Kind.Id TileKind)
  }
  deriving Show

-- | The map of tile kinds in a place (and generally anywhere in a cave).
-- The map is sparse. The default tile that eventually fills the empty spaces
-- is specified in the cave kind specification with @cdefTile@.
type TileMapEM = EM.EnumMap Point (Kind.Id TileKind)

-- | For @CAlternate@ tiling, require the place be comprised
-- of an even number of whole corners, with exactly one square
-- overlap between consecutive coners and no trimming.
-- For other tiling methods, check that the area is large enough for tiling
-- the corner twice in each direction, with a possible one row/column overlap.
placeCheck :: Area       -- ^ the area to fill
           -> PlaceKind  -- ^ the place kind to construct
           -> Bool
placeCheck r PlaceKind{..} =
  case interiorArea pfence r of
    Nothing -> False
    Just area ->
      let (x0, y0, x1, y1) = fromArea area
          dx = x1 - x0 + 1
          dy = y1 - y0 + 1
          dxcorner = case ptopLeft of [] -> 0 ; l : _ -> T.length l
          dycorner = length ptopLeft
          wholeOverlapped d dcorner = d > 1 && dcorner > 1 &&
                                      (d - 1) `mod` (2 * (dcorner - 1)) == 0
      in case pcover of
        CAlternate -> wholeOverlapped dx dxcorner &&
                      wholeOverlapped dy dycorner
        _          -> dx >= 2 * dxcorner - 1 &&
                      dy >= 2 * dycorner - 1

-- | Calculate interior room area according to fence type, based on the
-- total area for the room and it's fence. This is used for checking
-- if the room fits in the area, for digging up the place and the fence
-- and for deciding if the room is dark or lit later in the dungeon
-- generation process (e.g., for stairs).
interiorArea :: Fence -> Area -> Maybe Area
interiorArea fence r = case fence of
  FWall  -> shrink r
  FFloor -> shrink r
  FNone  -> Just r

-- | Given a few parameters, roll and construct a 'Place' datastructure
-- and fill a cave section acccording to it.
buildPlace :: Kind.COps         -- ^ the game content
           -> CaveKind          -- ^ current cave kind
           -> Bool              -- ^ whether the cave is dark
           -> Kind.Id TileKind  -- ^ dark fence tile, if fence hollow
           -> Kind.Id TileKind  -- ^ lit fence tile, if fence hollow
           -> Int               -- ^ current level depth
           -> Int               -- ^ maximum depth
           -> Area              -- ^ whole area of the place, fence included
           -> Rnd (TileMapEM, Place)
buildPlace Kind.COps{ cotile=cotile@Kind.Ops{opick=opick}
                    , coplace=Kind.Ops{okind=pokind, opick=popick} }
           CaveKind{..} dnight darkCorTile litCorTile ln depth r = do
  qsolidFence <- fmap (fromMaybe $ assert `failure` cfillerTile)
                 $ opick cfillerTile (const True)
  dark <- chanceDice ln depth cdarkChance
  let cave = "rogue"
  qkind <- fmap (fromMaybe $ assert `failure` (cave, r))
           $ popick cave (placeCheck r)
  let qhollowFence = if dark then darkCorTile else litCorTile
      kr = pokind qkind
      qlegend = if dark then clegendDarkTile else clegendLitTile
      qseen = False
      qarea = fromMaybe (assert `failure` (kr, r)) $ interiorArea (pfence kr) r
      place = Place {..}
  legend <- olegend cotile qlegend
  legendLit <- olegend cotile clegendLitTile
  let xlegend = EM.insert 'X' qhollowFence legend
      xlegendLit = EM.insert 'X' qhollowFence legendLit
      cmap = tilePlace qarea kr
      fence = case pfence kr of
        FWall -> buildFence qsolidFence qarea
        FFloor -> buildFence qhollowFence qarea
        FNone -> EM.empty
      (x0, y0, x1, y1) = fromArea qarea
      isEdge (Point x y) = x `elem` [x0, x1] || y `elem` [y0, y1]
      digNight xy c | isEdge xy = xlegendLit EM.! c
                    | otherwise = xlegend EM.! c
      interior = case pfence kr of
        FNone | not dnight -> EM.mapWithKey digNight cmap
        _ -> EM.map (xlegend EM.!) cmap
      tmap = EM.union interior fence
  return (tmap, place)

-- | Roll a legend of a place plan: a map from plan symbols to tile kinds.
olegend :: Kind.Ops TileKind -> Text
        -> Rnd (EM.EnumMap Char (Kind.Id TileKind))
olegend Kind.Ops{ofoldrWithKey, opick} cgroup =
  let getSymbols _ tk acc =
        maybe acc (const $ ES.insert (tsymbol tk) acc)
          (lookup cgroup $ tfreq tk)
      symbols = ofoldrWithKey getSymbols ES.empty
      getLegend s acc = do
        m <- acc
        tk <- fmap (fromMaybe $ assert `failure` (cgroup, s))
              $ opick cgroup $ (== s) . tsymbol
        return $! EM.insert s tk m
      legend = ES.foldr getLegend (return EM.empty) symbols
  in legend

-- | Construct a fence around an area, with the given tile kind.
buildFence :: Kind.Id TileKind -> Area -> TileMapEM
buildFence fenceId area =
  let (x0, y0, x1, y1) = fromArea area
  in EM.fromList $ [ (Point x y, fenceId)
                   | x <- [x0-1, x1+1], y <- [y0..y1] ] ++
                   [ (Point x y, fenceId)
                   | x <- [x0-1..x1+1], y <- [y0-1, y1+1] ]

-- | Construct a fence around an area, with the given tile group.
buildFenceRnd :: Kind.COps -> Text -> Area -> Rnd TileMapEM
buildFenceRnd Kind.COps{cotile=Kind.Ops{opick}} couterFenceTile area = do
  let (x0, y0, x1, y1) = fromArea area
      fenceIdRnd (xf, yf) = do
        let isCorner x y = x `elem` [x0-1, x1+1] && y `elem` [y0-1, y1+1]
            tileGroup | isCorner xf yf = "basic outer fence"
                      | otherwise = couterFenceTile
        fenceId <- fmap (fromMaybe $ assert `failure` tileGroup)
                   $ opick tileGroup (const True)
        return (Point xf yf, fenceId)
      pointList = [ (x, y) | x <- [x0-1, x1+1], y <- [y0..y1] ]
                  ++ [ (x, y) | x <- [x0-1..x1+1], y <- [y0-1, y1+1] ]
  fenceList <- mapM fenceIdRnd pointList
  return $! EM.fromList fenceList

-- TODO: use Text more instead of [Char]?
-- | Create a place by tiling patterns.
tilePlace :: Area                           -- ^ the area to fill
          -> PlaceKind                      -- ^ the place kind to construct
          -> EM.EnumMap Point Char
tilePlace area pl@PlaceKind{..} =
  let (x0, y0, x1, y1) = fromArea area
      xwidth = x1 - x0 + 1
      ywidth = y1 - y0 + 1
      dxcorner = case ptopLeft of
        [] -> assert `failure` (area, pl)
        l : _ -> T.length l
      (dx, dy) = assert (xwidth >= dxcorner && ywidth >= length ptopLeft
                         `blame` (area, pl))
                        (xwidth, ywidth)
      fromX (x2, y2) =
        zipWith (\x y -> Point x y) [x2..] (repeat y2)
      fillInterior :: (forall a. Int -> [a] -> [a]) -> [(Point, Char)]
      fillInterior f =
        let tileInterior (y, row) = zip (fromX (x0, y)) $ f dx row
            reflected = zip [y0..] $ f dy $ map T.unpack ptopLeft
        in concatMap tileInterior reflected
      tileReflect :: Int -> [a] -> [a]
      tileReflect d pat =
        let lstart = take (d `divUp` 2) pat
            lend   = take (d `div`   2) pat
        in lstart ++ reverse lend
      interior = case pcover of
        CAlternate ->
          let tile :: Int -> [a] -> [a]
              tile _ []  = assert `failure` "nothing to tile" `twith` pl
              tile d pat = take d (cycle $ init pat ++ init (reverse pat))
          in fillInterior tile
        CStretch ->
          let stretch :: Int -> [a] -> [a]
              stretch _ []  = assert `failure` "nothing to stretch" `twith` pl
              stretch d pat = tileReflect d (pat ++ repeat (last pat))
          in fillInterior stretch
        CReflect ->
          let reflect :: Int -> [a] -> [a]
              reflect d pat = tileReflect d (cycle pat)
          in fillInterior reflect
  in EM.fromList interior

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
    return $! Place{..}
