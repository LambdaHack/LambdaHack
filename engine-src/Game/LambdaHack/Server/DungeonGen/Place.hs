{-# LANGUAGE RankNTypes #-}
-- | Generation of places from place kinds.
module Game.LambdaHack.Server.DungeonGen.Place
  ( Place(..), TileMapEM, buildPlace, isChancePos, buildFenceRnd
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , placeCheck, interiorArea, olegend, pover, buildFence, buildFenceMap
  , tilePlace
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Bits as Bits
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Text as T
import           Data.Word (Word32)

import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Content.CaveKind
import           Game.LambdaHack.Content.PlaceKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Server.DungeonGen.AreaRnd

-- | The map of tile kinds in a place (and generally anywhere in a cave).
-- The map is sparse. The default tile that eventually fills the empty spaces
-- is specified in the cave kind specification with @cdefTile@.
type TileMapEM = EM.EnumMap Point (ContentId TileKind)

-- | The parameters of a place. All are immutable and rolled and fixed
-- at the time when a place is generated.
data Place = Place
  { qkind  :: ContentId PlaceKind
  , qarea  :: Area
  , qmap   :: TileMapEM
  , qfence :: TileMapEM
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
           -> Word32              -- ^ secret tile seed
           -> Area                -- ^ whole area of the place, fence included
           -> Maybe Area          -- ^ whole inner area of the grid cell
           -> Freqs PlaceKind     -- ^ optional fixed place freq
           -> Rnd Place
buildPlace cops@COps{coplace, coTileSpeedup}
           kc@CaveKind{..} dnight darkCorTile litCorTile
           levelDepth@(Dice.AbsDepth ldepth)
           totalDepth@(Dice.AbsDepth tdepth)
           dsecret r minnerArea mplaceGroup = do
  let f !q !acc !p !pk !kind =
        let rarity = linearInterpolation ldepth tdepth (prarity kind)
            !fr = q * p * rarity
        in (fr, (pk, kind)) : acc
      g (placeGroup, q) = ofoldlGroup' coplace placeGroup (f q) []
      pfreq = case mplaceGroup of
        [] -> cplaceFreq
        _ -> mplaceGroup
      placeFreq = concatMap g pfreq
      checkedFreq = filter (\(_, (_, kind)) -> placeCheck r kind) placeFreq
      freq = toFreq "buildPlace" checkedFreq
  let !_A = assert (not (nullFreq freq) `blame` (placeFreq, checkedFreq, r)) ()
  (qkind, kr) <- frequency freq
  let smallPattern = pcover kr `elem` [CVerbatim, CMirror]
                     && (length (ptopLeft kr) < 10
                         || T.length (head (ptopLeft kr)) < 10)
  -- Below we apply a heuristics to estimate if there are floor tiles
  -- in the place that are adjacent to floor tiles of the cave and so both
  -- should have the same lit condition.
  -- A false positive is walled staircases in LambdaHack, but it's OK.
  dark <- if cpassable
             && not (dnight && Tile.isLit coTileSpeedup darkCorTile)
                  -- the colonnade can be illuminated just as the trail is
             && (pfence kr `elem` [FFloor, FGround]
                 || pfence kr == FNone && smallPattern)
          then return dnight
          else oddsDice levelDepth totalDepth cdarkOdds
  let qlegend = if dark then clegendDarkTile else clegendLitTile
  rBetter <- case minnerArea of
    Just innerArea | pcover kr `elem` [CVerbatim, CMirror] -> do
      -- A hack: if a verbatim place was rolled, redo computing the area
      -- taking into account that often much smaller portion is taken by place.
      let requiredForFence = case pfence kr of
            FWall   -> 1
            FFloor  -> 1
            FGround -> 1
            FNone   -> 0
          sizeBetter = ( 2 * requiredForFence
                         + T.length (head (ptopLeft kr))
                       , 2 * requiredForFence
                         + length (ptopLeft kr) )
      mkRoom sizeBetter sizeBetter innerArea
    _ -> return r
  let qarea = fromMaybe (error $ "" `showFailure` (kr, r))
              $ interiorArea kr rBetter
      override = if dark then poverrideDark kr else poverrideLit kr
  (overrideOneIn, overDefault) <- pover cops override
  (legendOneIn, legend) <- olegend cops qlegend
  cmap <- tilePlace qarea kr
  let mOneIn :: EM.EnumMap Char (Int, Int, ContentId TileKind)
      mOneIn = EM.union overrideOneIn legendOneIn
      m :: EM.EnumMap Char (ContentId TileKind)
      m = EM.union overDefault legend
      lookupOneIn :: Point -> Char -> ContentId TileKind
      lookupOneIn xy c = case EM.lookup c mOneIn of
        Just (k, n, tk) | isChancePos k n dsecret xy -> tk
        _ -> EM.findWithDefault (error $ "" `showFailure` (c, mOneIn, m)) c m
      qmap = EM.mapWithKey lookupOneIn cmap
  qfence <- buildFence cops kc dnight darkCorTile litCorTile
                       dark (pfence kr) qarea
  return $! Place {..}

isChancePos :: Int -> Int -> Word32 -> Point -> Bool
isChancePos k' n' dsecret (Point x' y') = k' > 0 && n' > 0 &&
  let k = toEnum k'
      n = toEnum n'
      x = toEnum x'
      y = toEnum y'
      z = dsecret `Bits.rotateR` x' `Bits.xor` y + x
  in if k < n
     then z `mod` ((n + k) `divUp` k) == 0
     else z `mod` ((n + k) `divUp` n) /= 0

-- | Roll a legend of a place plan: a map from plan symbols to tile kinds.
olegend :: COps -> GroupName TileKind
        -> Rnd ( EM.EnumMap Char (Int, Int, ContentId TileKind)
               , EM.EnumMap Char (ContentId TileKind) )
olegend COps{cotile} cgroup =
  let getSymbols !acc _ _ !tk = ES.insert (TK.tsymbol tk) acc
      symbols = ofoldlGroup' cotile cgroup getSymbols ES.empty
      getLegend s !acc = do
        (mOneIn, m) <- acc
        let p f t = TK.tsymbol t == s && f (Tile.kindHasFeature TK.Spice t)
        tk <- fmap (fromMaybe $ error $ "" `showFailure` (cgroup, s))
              $ opick cotile cgroup (p not)
        mtkSpice <- opick cotile cgroup (p id)
        return $! case mtkSpice of
          Nothing -> (mOneIn, EM.insert s tk m)
          Just tkSpice ->
            -- Unlikely, but possible that ordinary legend has spice.
            let n = fromMaybe (error $ show cgroup)
                              (lookup cgroup (TK.tfreq (okind cotile tk)))
                k = fromMaybe (error $ show cgroup)
                              (lookup cgroup (TK.tfreq (okind cotile tkSpice)))
            in (EM.insert s (k, n, tkSpice) mOneIn, EM.insert s tk m)
      legend = ES.foldr' getLegend (return (EM.empty, EM.empty)) symbols
  in legend

pover :: COps -> [(Char, GroupName TileKind)]
      -> Rnd ( EM.EnumMap Char (Int, Int, ContentId TileKind)
             , EM.EnumMap Char (ContentId TileKind) )
pover COps{cotile} poverride =
  let getLegend (s, cgroup) acc = do
        (mOneIn, m) <- acc
        mtkSpice <- opick cotile cgroup (Tile.kindHasFeature TK.Spice)
        tk <- fromMaybe (error $ "" `showFailure` (s, cgroup, poverride))
              <$> opick cotile cgroup (not . Tile.kindHasFeature TK.Spice)
        return $! case mtkSpice of
          Nothing -> (mOneIn, EM.insert s tk m)
          Just tkSpice ->
            -- Very likely that overrides have spice.
            let n = fromMaybe (error $ show cgroup)
                              (lookup cgroup (TK.tfreq (okind cotile tk)))
                k = fromMaybe (error $ show cgroup)
                              (lookup cgroup (TK.tfreq (okind cotile tkSpice)))
            in (EM.insert s (k, n, tkSpice) mOneIn, EM.insert s tk m)
  in foldr getLegend (return (EM.empty, EM.empty)) poverride

-- | Construct a fence around a place.
buildFence :: COps -> CaveKind -> Bool
           -> ContentId TileKind -> ContentId TileKind
           -> Bool -> Fence -> Area
           -> Rnd TileMapEM
buildFence COps{cotile} CaveKind{ccornerTile, cwallTile}
           dnight darkCorTile litCorTile dark fence qarea = do
  qFWall <- fromMaybe (error $ "" `showFailure` cwallTile)
            <$> opick cotile cwallTile (const True)
  qFCorner <- fromMaybe (error $ "" `showFailure` ccornerTile)
              <$> opick cotile ccornerTile (const True)
  let qFFloor = if dark then darkCorTile else litCorTile
      qFGround = if dnight then darkCorTile else litCorTile
  return $! case fence of
    FWall -> buildFenceMap qFWall qFCorner qarea
    FFloor -> buildFenceMap qFFloor qFFloor qarea
    FGround -> buildFenceMap qFGround qFGround qarea
    FNone -> EM.empty

-- | Construct a fence around an area, with the given tile kind.
-- Corners have a different kind, e.g., to avoid putting doors there.
buildFenceMap :: ContentId TileKind -> ContentId TileKind -> Area -> TileMapEM
buildFenceMap wallId cornerId area =
  let (x0, y0, x1, y1) = fromArea area
  in EM.fromList $ [ (Point x y, wallId)
                   | x <- [x0-1, x1+1], y <- [y0..y1] ] ++
                   [ (Point x y, wallId)
                   | x <- [x0..x1], y <- [y0-1, y1+1] ] ++
                   [ (Point x y, cornerId)
                   | x <- [x0-1, x1+1], y <- [y0-1, y1+1] ]

-- | Construct a fence around an area, with the given tile group.
buildFenceRnd :: COps
              -> GroupName TileKind -> GroupName TileKind
              -> GroupName TileKind -> GroupName TileKind
              -> Area
              -> Rnd TileMapEM
buildFenceRnd COps{cotile}
              cfenceTileN cfenceTileE cfenceTileS cfenceTileW area = do
  let (x0, y0, x1, y1) = fromArea area
      allTheSame = all (== cfenceTileN) [cfenceTileE, cfenceTileS, cfenceTileW]
      fenceIdRnd couterFenceTile (xf, yf) = do
        let isCorner x y = x `elem` [x0-1, x1+1] && y `elem` [y0-1, y1+1]
            tileGroup | isCorner xf yf && not allTheSame = TK.S_BASIC_OUTER_FENCE
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
