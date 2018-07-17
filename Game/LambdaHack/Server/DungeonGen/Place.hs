{-# LANGUAGE RankNTypes #-}
-- | Generation of places from place kinds.
module Game.LambdaHack.Server.DungeonGen.Place
  ( Place(..), TileMapEM, buildPlace, isChancePos, buildFenceRnd
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , placeCheck, interiorArea, olegend, ooverride, buildFence, tilePlace
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Bits as Bits
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Text as T

import           Game.LambdaHack.Common.Area
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Frequency
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Content.CaveKind
import           Game.LambdaHack.Content.PlaceKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK

-- | The map of tile kinds in a place (and generally anywhere in a cave).
-- The map is sparse. The default tile that eventually fills the empty spaces
-- is specified in the cave kind specification with @cdefTile@.
type TileMapEM = EM.EnumMap Point (ContentId TileKind)

-- | The parameters of a place. All are immutable and rolled and fixed
-- at the time when a place is generated.
data Place = Place
  { qkind :: ContentId PlaceKind
  , qarea :: Area
  , qmap  :: TileMapEM
  }
  deriving Show

-- | For @CAlternate@ tiling, require the place be comprised
-- of an even number of whole corners, with exactly one square
-- overlap between consecutive coners and no trimming.
-- For other tiling methods, check that the area is large enough for tiling
-- the corner twice in each direction, with a possible one row/column overlap.
placeCheck :: Area       -- ^ the area to fill
           -> PlaceKind  -- ^ the kind of place to construct
           -> Bool
placeCheck r pk@PlaceKind{..} =
  case interiorArea pk r of
    Nothing -> False
    Just area ->
      let (_, xspan, yspan) = spanArea area
          dxcorner = case ptopLeft of [] -> 0 ; l : _ -> T.length l
          dycorner = length ptopLeft
          wholeOverlapped d dcorner = d > 1 && dcorner > 1 &&
                                      (d - 1) `mod` (2 * (dcorner - 1)) == 0
          largeEnough = xspan >= 2 * dxcorner - 1 && yspan >= 2 * dycorner - 1
      in case pcover of
        CAlternate -> wholeOverlapped xspan dxcorner &&
                      wholeOverlapped yspan dycorner
        CStretch   -> largeEnough
        CReflect   -> largeEnough
        CVerbatim  -> True
        CMirror    -> True

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
  in if pcover kr `elem` [CVerbatim, CMirror]
     then let (Point x0 y0, xspan, yspan) = spanArea r
              dx = case ptopLeft kr of
                [] -> error $ "" `showFailure` kr
                l : _ -> T.length l
              dy = length $ ptopLeft kr
              mx = (xspan - dx) `div` 2
              my = (yspan - dy) `div` 2
          in if mx < requiredForFence || my < requiredForFence
             then Nothing
             else toArea (x0 + mx, y0 + my, x0 + mx + dx - 1, y0 + my + dy - 1)
     else case requiredForFence of
       0 -> Just r
       1 -> shrink r
       _ -> error $ "" `showFailure` kr

-- | Given a few parameters, roll and construct a 'Place' datastructure
-- and fill a cave section acccording to it.
buildPlace :: COps                -- ^ the game content
           -> CaveKind            -- ^ current cave kind
           -> Bool                -- ^ whether the cave is dark
           -> ContentId TileKind  -- ^ dark fence tile, if fence hollow
           -> ContentId TileKind  -- ^ lit fence tile, if fence hollow
           -> Dice.AbsDepth       -- ^ current level depth
           -> Dice.AbsDepth       -- ^ absolute depth
           -> Int                 -- ^ secret tile seed
           -> Area                -- ^ whole area of the place, fence included
           -> Maybe (GroupName PlaceKind)  -- ^ optional fixed place group
           -> Rnd Place
buildPlace cops@COps{cotile, coplace}
           CaveKind{..} dnight darkCorTile litCorTile
           ldepth@(Dice.AbsDepth ld) totalDepth@(Dice.AbsDepth depth) dsecret
           r mplaceGroup = do
  qFWall <- fromMaybe (error $ "" `showFailure` cfillerTile)
            <$> opick cotile cfillerTile (const True)
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
      f !placeGroup !q !acc !p !pk !kind =
        let rarity = linearInterpolation (prarity kind)
        in (q * p * rarity, ((pk, kind), placeGroup)) : acc
      g (placeGroup, q) = ofoldlGroup' coplace placeGroup (f placeGroup q) []
      pfreq = case mplaceGroup of
        Nothing -> cplaceFreq
        Just placeGroup -> [(placeGroup, 1)]
      placeFreq = concatMap g pfreq
      checkedFreq = filter (\(_, ((_, kind), _)) -> placeCheck r kind) placeFreq
      freq = toFreq ("buildPlace" <+> tshow (map fst checkedFreq)) checkedFreq
  let !_A = assert (not (nullFreq freq) `blame` (placeFreq, checkedFreq, r)) ()
  ((qkind, kr), _) <- frequency freq
  dark <- if cpassable && pfence kr `elem` [FFloor, FGround]
          then return dnight
          else chanceDice ldepth totalDepth cdarkChance
  let qFFloor = if dark then darkCorTile else litCorTile
      qFGround = if dnight then darkCorTile else litCorTile
      qlegend = if dark then clegendDarkTile else clegendLitTile
      qarea = fromMaybe (error $ "" `showFailure` (kr, r)) $ interiorArea kr r
  (overrideOneIn, override) <- ooverride cops (poverride kr)
  (legendOneIn, legend) <- olegend cops qlegend
  (legendLitOneIn, legendLit) <- olegend cops clegendLitTile
  let xlegend = ( EM.union overrideOneIn legendOneIn
                , EM.union override legend )
      xlegendLit = ( EM.union overrideOneIn legendLitOneIn
                   , EM.union override legendLit )
  cmap <- tilePlace qarea kr
  let fence = case pfence kr of
        FWall -> buildFence qFWall qarea
        FFloor -> buildFence qFFloor qarea
        FGround -> buildFence qFGround qarea
        FNone -> EM.empty
      (x0, y0, x1, y1) = fromArea qarea
      isEdge (Point x y) = x `elem` [x0, x1] || y `elem` [y0, y1]
      digDay xy c | isEdge xy = lookupOneIn xlegendLit xy c
                  | otherwise = lookupOneIn xlegend xy c
      lookupOneIn :: ( EM.EnumMap Char (Int, ContentId TileKind)
                     , EM.EnumMap Char (ContentId TileKind) )
                  -> Point -> Char
                  -> ContentId TileKind
      lookupOneIn (mOneIn, m) xy c = case EM.lookup c mOneIn of
        Just (oneInChance, tk) ->
          if isChancePos oneInChance dsecret xy
          then tk
          else EM.findWithDefault (error $ "" `showFailure` (c, mOneIn, m)) c m
        Nothing -> EM.findWithDefault (error $ "" `showFailure` (c, mOneIn, m))
                                      c m
      interior = case pfence kr of
        FNone | not dnight -> EM.mapWithKey digDay cmap
        _ -> EM.mapWithKey (lookupOneIn xlegend) cmap
      qmap = EM.union interior fence
  return $! Place {..}

isChancePos :: Int -> Int -> Point -> Bool
isChancePos c dsecret (Point x y) =
  c > 0 && (dsecret `Bits.rotateR` x `Bits.xor` y + x) `mod` c == 0

-- | Roll a legend of a place plan: a map from plan symbols to tile kinds.
olegend :: COps -> GroupName TileKind
        -> Rnd ( EM.EnumMap Char (Int, ContentId TileKind)
               , EM.EnumMap Char (ContentId TileKind) )
olegend COps{cotile} cgroup =
  let getSymbols !acc _ !tk =
        maybe acc (const $ ES.insert (TK.tsymbol tk) acc)
              (lookup cgroup $ TK.tfreq tk)
      symbols = ofoldlWithKey' cotile getSymbols ES.empty
      getLegend s !acc = do
        (mOneIn, m) <- acc
        let p f t = TK.tsymbol t == s && f (Tile.kindHasFeature TK.Spice t)
        tk <- fmap (fromMaybe $ error $ "" `showFailure` (cgroup, s))
              $ opick cotile cgroup (p not)
        mtkSpice <- opick cotile cgroup (p id)
        return $! case mtkSpice of
          Nothing -> (mOneIn, EM.insert s tk m)
          Just tkSpice ->
            let n = fromJust (lookup cgroup (TK.tfreq (okind cotile tk)))
                k = fromJust (lookup cgroup (TK.tfreq (okind cotile tkSpice)))
                oneIn = (n + k) `divUp` k
            in (EM.insert s (oneIn, tkSpice) mOneIn, EM.insert s tk m)
      legend = ES.foldr' getLegend (return (EM.empty, EM.empty)) symbols
  in legend

ooverride :: COps -> [(Char, GroupName TileKind)]
          -> Rnd ( EM.EnumMap Char (Int, ContentId TileKind)
                 , EM.EnumMap Char (ContentId TileKind) )
ooverride COps{cotile} poverride =
  let getLegend (s, cgroup) acc = do
        (mOneIn, m) <- acc
        mtkSpice <- opick cotile cgroup (Tile.kindHasFeature TK.Spice)
        tk <- fromMaybe (error $ "" `showFailure` (s, cgroup, poverride))
              <$> opick cotile cgroup (not . Tile.kindHasFeature TK.Spice)
        return $! case mtkSpice of
          Nothing -> (mOneIn, EM.insert s tk m)
          Just tkSpice ->
            let n = fromJust (lookup cgroup (TK.tfreq (okind cotile tk)))
                k = fromJust (lookup cgroup (TK.tfreq (okind cotile tkSpice)))
                oneIn = (n + k) `divUp` k
            in (EM.insert s (oneIn, tkSpice) mOneIn, EM.insert s tk m)
  in foldr getLegend (return (EM.empty, EM.empty)) poverride

-- | Construct a fence around an area, with the given tile kind.
buildFence :: ContentId TileKind -> Area -> TileMapEM
buildFence fenceId area =
  let (x0, y0, x1, y1) = fromArea area
  in EM.fromList $ [ (Point x y, fenceId)
                   | x <- [x0-1, x1+1], y <- [y0..y1] ] ++
                   [ (Point x y, fenceId)
                   | x <- [x0-1..x1+1], y <- [y0-1, y1+1] ]

-- | Construct a fence around an area, with the given tile group.
buildFenceRnd :: COps
              -> GroupName TileKind -> GroupName TileKind
              -> GroupName TileKind -> GroupName TileKind
              -> Area
              -> Rnd TileMapEM
buildFenceRnd COps{cotile}
              cfenceTileN cfenceTileE cfenceTileS cfenceTileW area = do
  let (x0, y0, x1, y1) = fromArea area
      fenceIdRnd couterFenceTile (xf, yf) = do
        let isCorner x y = x `elem` [x0-1, x1+1] && y `elem` [y0-1, y1+1]
            tileGroup | isCorner xf yf = "basic outer fence"
                      | otherwise = couterFenceTile
        fenceId <- fromMaybe (error $ "" `showFailure` tileGroup)
                   <$> opick cotile tileGroup (const True)
        return (Point xf yf, fenceId)
      pointListN = [(x, y0-1) | x <- [x0-1..x1+1]]
      pointListE = [(x1+1, y) | y <- [y0..y1]]
      pointListS = [(x, y1+1) | x <- [x0-1..x1+1]]
      pointListW = [(x0-1, y) | y <- [y0..y1]]
  fenceListN <- mapM (fenceIdRnd cfenceTileN) pointListN
  fenceListE <- mapM (fenceIdRnd cfenceTileE) pointListE
  fenceListS <- mapM (fenceIdRnd cfenceTileS) pointListS
  fenceListW <- mapM (fenceIdRnd cfenceTileW) pointListW
  return $! EM.fromList $ fenceListN ++ fenceListE ++ fenceListS ++ fenceListW

-- | Create a place by tiling patterns.
tilePlace :: Area                           -- ^ the area to fill
          -> PlaceKind                      -- ^ the place kind to construct
          -> Rnd (EM.EnumMap Point Char)
tilePlace area pl@PlaceKind{..} = do
  let (Point x0 y0, xspan, yspan) = spanArea area
      dxcorner = case ptopLeft of
        [] -> error $ "" `showFailure` (area, pl)
        l : _ -> T.length l
      (dx, dy) = assert (xspan >= dxcorner && yspan >= length ptopLeft
                         `blame` (area, pl))
                        (xspan, yspan)
      fromX (x2, y2) = map (`Point` y2) [x2..]
      fillInterior :: (Int -> String -> String)
                   -> (Int -> [String] -> [String])
                   -> [(Point, Char)]
      fillInterior f g =
        let tileInterior (y, row) =
              let fx = f dx row
                  xStart = x0 + ((xspan - length fx) `div` 2)
              in filter ((/= 'X') . snd) $ zip (fromX (xStart, y)) fx
            reflected =
              let gy = g dy $ map T.unpack ptopLeft
                  yStart = y0 + ((yspan - length gy) `div` 2)
              in zip [yStart..] gy
        in concatMap tileInterior reflected
      tileReflect :: Int -> [a] -> [a]
      tileReflect d pat =
        let lstart = take (d `divUp` 2) pat
            lend   = take (d `div`   2) pat
        in lstart ++ reverse lend
  interior <- case pcover of
    CAlternate -> do
      let tile :: Int -> [a] -> [a]
          tile _ []  = error $ "nothing to tile" `showFailure` pl
          tile d pat = take d (cycle $ init pat ++ init (reverse pat))
      return $! fillInterior tile tile
    CStretch -> do
      let stretch :: Int -> [a] -> [a]
          stretch _ []  = error $ "nothing to stretch" `showFailure` pl
          stretch d pat = tileReflect d (pat ++ repeat (last pat))
      return $! fillInterior stretch stretch
    CReflect -> do
      let reflect :: Int -> [a] -> [a]
          reflect d pat = tileReflect d (cycle pat)
      return $! fillInterior reflect reflect
    CVerbatim -> return $! fillInterior (flip const) (flip const)
    CMirror -> do
      mirror1 <- oneOf [id, reverse]
      mirror2 <- oneOf [id, reverse]
      return $! fillInterior (\_ l -> mirror1 l) (\_ l -> mirror2 l)
  return $! EM.fromList interior
