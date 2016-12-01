{-# LANGUAGE RankNTypes #-}
-- | Generation of places from place kinds.
module Game.LambdaHack.Server.DungeonGen.Place
  ( TileMapEM, Place(..), placeCheck, buildFenceRnd, buildPlace
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Text as T

import Game.LambdaHack.Common.Frequency
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import Game.LambdaHack.Server.DungeonGen.Area

-- TODO: use more, rewrite as needed, document each field.
-- | The parameters of a place. Most are immutable and set
-- at the time when a place is generated.
data Place = Place
  { qkind    :: !(Kind.Id PlaceKind)
  , qarea    :: !Area
  , qseen    :: !Bool
  , qlegend  :: !(GroupName TileKind)
  , qFWall   :: !(Kind.Id TileKind)
  , qFFloor  :: !(Kind.Id TileKind)
  , qFGround :: !(Kind.Id TileKind)
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
placeCheck r pk@PlaceKind{..} =
  case interiorArea pk r of
    Nothing -> False
    Just area ->
      let (x0, y0, x1, y1) = fromArea area
          dx = x1 - x0 + 1
          dy = y1 - y0 + 1
          dxcorner = case ptopLeft of [] -> 0 ; l : _ -> T.length l
          dycorner = length ptopLeft
          wholeOverlapped d dcorner = d > 1 && dcorner > 1 &&
                                      (d - 1) `mod` (2 * (dcorner - 1)) == 0
          largeEnough = dx >= 2 * dxcorner - 1 && dy >= 2 * dycorner - 1
      in case pcover of
        CAlternate -> wholeOverlapped dx dxcorner &&
                      wholeOverlapped dy dycorner
        CStretch   -> largeEnough
        CReflect   -> largeEnough
        CVerbatim  -> True

-- | Calculate interior room area according to fence type, based on the
-- total area for the room and it's fence. This is used for checking
-- if the room fits in the area, for digging up the place and the fence
-- and for deciding if the room is dark or lit later in the dungeon
-- generation process.
interiorArea :: PlaceKind -> Area -> Maybe Area
interiorArea kr r =
  let requiredForFence = case pfence kr of
        FWall   -> 1
        FFloor  -> 1
        FGround -> 1
        FNone   -> 0
  in case pcover kr of
    CVerbatim ->
      let (x0, y0, x1, y1) = fromArea r
          dx = case ptopLeft kr of
            [] -> assert `failure` kr
            l : _ -> T.length l
          dy = length $ ptopLeft kr
          mx = (x1 - x0 + 1 - dx) `div` 2
          my = (y1 - y0 + 1 - dy) `div` 2
      in if mx < requiredForFence || my < requiredForFence
         then Nothing
         else toArea (x0 + mx, y0 + my, x0 + mx + dx - 1, y0 + my + dy - 1)
    _ -> case requiredForFence of
           0 -> Just r
           1 -> shrink r
           _ -> assert `failure` kr

-- | Given a few parameters, roll and construct a 'Place' datastructure
-- and fill a cave section acccording to it.
buildPlace :: Kind.COps         -- ^ the game content
           -> CaveKind          -- ^ current cave kind
           -> Bool              -- ^ whether the cave is dark
           -> Kind.Id TileKind  -- ^ dark fence tile, if fence hollow
           -> Kind.Id TileKind  -- ^ lit fence tile, if fence hollow
           -> AbsDepth          -- ^ current level depth
           -> AbsDepth          -- ^ absolute depth
           -> Area              -- ^ whole area of the place, fence included
           -> Maybe (GroupName PlaceKind)  -- ^ optional fixed place group
           -> Rnd (TileMapEM, Place)
buildPlace cops@Kind.COps{ cotile=Kind.Ops{opick=opick}
                         , coplace=Kind.Ops{ofoldlGroup'} }
           CaveKind{..} dnight darkCorTile litCorTile
           ldepth@(AbsDepth ld) totalDepth@(AbsDepth depth) r mplaceGroup = do
  qFWall <- fromMaybe (assert `failure` cfillerTile)
            <$> opick cfillerTile (const True)
  dark <- chanceDice ldepth totalDepth cdarkChance
  -- TODO: factor out from here and newItem:
  let findInterval x1y1 [] = (x1y1, (11, 0))
      findInterval !x1y1 ((!x, !y) : rest) =
        if fromIntegral ld * 10 <= x * fromIntegral depth
        then (x1y1, (x, y))
        else findInterval (x, y) rest
      linearInterpolation !dataset =
        -- We assume @dataset@ is sorted and between 0 and 10.
        let ((x1, y1), (x2, y2)) = findInterval (0, 0) dataset
        in ceiling
           $ fromIntegral y1
             + fromIntegral (y2 - y1)
               * (fromIntegral ld * 10 - x1 * fromIntegral depth)
               / ((x2 - x1) * fromIntegral depth)
  let f !placeGroup !q !acc !p !pk !kind =
        let rarity = linearInterpolation (prarity kind)
        in (q * p * rarity, ((pk, kind), placeGroup)) : acc
      g (placeGroup, q) = ofoldlGroup' placeGroup (f placeGroup q) []
      pfreq = case mplaceGroup of
        Nothing -> cplaceFreq
        Just placeGroup -> [(placeGroup, 1)]
      placeFreq = concatMap g pfreq
      checkedFreq = filter (\(_, ((_, kind), _)) -> placeCheck r kind) placeFreq
      freq = toFreq ("buildPlace" <+> tshow (map fst checkedFreq)) checkedFreq
  let !_A = assert (not (nullFreq freq) `blame` (placeFreq, checkedFreq, r)) ()
  ((qkind, kr), _) <- frequency freq
  let qFFloor = if dark then darkCorTile else litCorTile
      qFGround = if dnight then darkCorTile else litCorTile
      qlegend = if dark then clegendDarkTile else clegendLitTile
      qseen = False
      qarea = fromMaybe (assert `failure` (kr, r)) $ interiorArea kr r
      place = Place {..}
  override <- ooverride cops (poverride kr)
  legend <- olegend cops qlegend
  legendLit <- olegend cops clegendLitTile
  let xlegend = EM.union override legend
      xlegendLit = EM.union override legendLit
      cmap = tilePlace qarea kr
      fence = case pfence kr of
        FWall -> buildFence qFWall qarea
        FFloor -> buildFence qFFloor qarea
        FGround -> buildFence qFGround qarea
        FNone -> EM.empty
      (x0, y0, x1, y1) = fromArea qarea
      isEdge (Point x y) = x `elem` [x0, x1] || y `elem` [y0, y1]
      digDay xy c | isEdge xy = xlegendLit EM.! c
                  | otherwise = xlegend EM.! c
      interior = case pfence kr of
        FNone | not dnight -> EM.mapWithKey digDay cmap
        _ -> let lookupLegend x =
                   EM.findWithDefault (assert `failure` (qlegend, x)) x xlegend
             in EM.map lookupLegend cmap
      tmap = EM.union interior fence
  return (tmap, place)

-- | Roll a legend of a place plan: a map from plan symbols to tile kinds.
olegend :: Kind.COps -> GroupName TileKind
        -> Rnd (EM.EnumMap Char (Kind.Id TileKind))
olegend Kind.COps{cotile=Kind.Ops{ofoldlWithKey', opick}} cgroup =
  let getSymbols !acc _ !tk =
        maybe acc (const $ ES.insert (TK.tsymbol tk) acc)
          (lookup cgroup $ TK.tfreq tk)
      symbols = ofoldlWithKey' getSymbols ES.empty
      getLegend s !acc = do
        m <- acc
        tk <- fmap (fromMaybe $ assert `failure` (cgroup, s))
              $ opick cgroup $ (== s) . TK.tsymbol
        return $! EM.insert s tk m
      legend = ES.foldr' getLegend (return EM.empty) symbols
  in legend

ooverride :: Kind.COps -> [(Char, GroupName TileKind)]
          -> Rnd (EM.EnumMap Char (Kind.Id TileKind))
ooverride Kind.COps{cotile=Kind.Ops{opick}} poverride =
  let getLegend (s, cgroup) acc = do
        m <- acc
        tk <- fromMaybe (assert `failure` (cgroup, s))
              <$> opick cgroup (const True)  -- tile symbol ignored
        return $! EM.insert s tk m
      legend = foldr getLegend (return EM.empty) poverride
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
buildFenceRnd :: Kind.COps -> GroupName TileKind -> Area -> Rnd TileMapEM
buildFenceRnd Kind.COps{cotile=Kind.Ops{opick}} couterFenceTile area = do
  let (x0, y0, x1, y1) = fromArea area
      fenceIdRnd (xf, yf) = do
        let isCorner x y = x `elem` [x0-1, x1+1] && y `elem` [y0-1, y1+1]
            tileGroup | isCorner xf yf = "basic outer fence"
                      | otherwise = couterFenceTile
        fenceId <- fromMaybe (assert `failure` tileGroup)
                   <$> opick tileGroup (const True)
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
      fromX (x2, y2) = map (`Point` y2) [x2..]
      fillInterior :: (forall a. Int -> [a] -> [a]) -> [(Point, Char)]
      fillInterior f =
        let tileInterior (y, row) =
              let fx = f dx row
                  xStart = x0 + ((xwidth - length fx) `div` 2)
              in filter ((/= 'X') . snd) $ zip (fromX (xStart, y)) fx
            reflected =
              let fy = f dy $ map T.unpack ptopLeft
                  yStart = y0 + ((ywidth - length fy) `div` 2)
              in zip [yStart..] fy
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
        CVerbatim -> fillInterior $ curry snd
  in EM.fromList interior

instance Binary Place where
  put Place{..} = do
    put qkind
    put qarea
    put qseen
    put qlegend
    put qFWall
    put qFFloor
    put qFGround
  get = do
    qkind <- get
    qarea <- get
    qseen <- get
    qlegend <- get
    qFWall <- get
    qFFloor <- get
    qFGround <- get
    return $! Place{..}
