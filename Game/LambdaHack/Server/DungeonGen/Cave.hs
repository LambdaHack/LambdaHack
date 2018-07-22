-- | Generation of caves (not yet inhabited dungeon levels) from cave kinds.
module Game.LambdaHack.Server.DungeonGen.Cave
  ( Cave(..), buildCave
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , pickOpening, digCorridors
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Key (mapWithKeyM)

import           Game.LambdaHack.Common.Area
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.CaveKind
import           Game.LambdaHack.Content.PlaceKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import           Game.LambdaHack.Server.DungeonGen.AreaRnd
import           Game.LambdaHack.Server.DungeonGen.Place

-- | The type of caves (not yet inhabited dungeon levels).
data Cave = Cave
  { dkind  :: ContentId CaveKind  -- ^ the kind of the cave
  , darea  :: Area                -- ^ map area of the cave
  , dmap   :: TileMapEM           -- ^ tile kinds in the cave
  , dnight :: Bool                -- ^ whether the cave is dark
  }
  deriving Show

{- |
Generate a cave using an algorithm inspired by the original Rogue,
as follows (in gross simplification):

* The available area is divided into a grid, e.g, 3 by 3,
  where each of the 9 grid cells has approximately the same size.

* In some of the 9 grid cells a room is placed at a random position
  and with a random size, but larger than the minimum size,
  e.g, 2 by 2 floor tiles.

* Rooms that are on horizontally or vertically adjacent grid cells
  may be connected by a corridor. Corridors consist of 3 segments of straight
  lines (either "horizontal, vertical, horizontal" or "vertical, horizontal,
  vertical"). They end in openings in the walls of the room they connect.
  It is possible that one or two of the 3 segments have length 0, such that
  the resulting corridor is L-shaped or even a single straight line.

* Corridors are generated randomly in such a way that at least every room
  on the grid is connected, and a few more might be. It is not sufficient
  to always connect all adjacent rooms, because not each cell holds a room.
-}
buildCave :: COps                -- ^ content definitions
          -> Dice.AbsDepth       -- ^ depth of the level to generate
          -> Dice.AbsDepth       -- ^ absolute depth
          -> Area                -- ^ map area of the cave
          -> Int                 -- ^ secret tile seed
          -> ContentId CaveKind  -- ^ cave kind to use for generation
          -> (X, Y)              -- ^ the dimensions of the grid of places
          -> EM.EnumMap Point SpecialArea  -- ^ pos of stairs, etc.
          -> [Point]             -- ^ boot positions to be treated as fixed
          -> Rnd Cave
buildCave cops@COps{cocave, coplace, cotile, coTileSpeedup}
          ldepth totalDepth darea dsecret dkind lgr@(gx, gy) gs bootExtra = do
  let kc@CaveKind{..} = okind cocave dkind
  darkCorTile <- fromMaybe (error $ "" `showFailure` cdarkCorTile)
                 <$> opick cotile cdarkCorTile (const True)
  litCorTile <- fromMaybe (error $ "" `showFailure` clitCorTile)
                <$> opick cotile clitCorTile (const True)
  dnight <- chanceDice ldepth totalDepth cnightChance
  let createPlaces = do
        minPlaceSize <- castDiceXY ldepth totalDepth cminPlaceSize
        maxPlaceSize <- castDiceXY ldepth totalDepth cmaxPlaceSize
        let mergeFixed :: EM.EnumMap Point SpecialArea
                       -> (Point, SpecialArea)
                       -> EM.EnumMap Point SpecialArea
            mergeFixed !gs0 (!i, !special) =
              let mergeSpecial ar p2 f =
                    case EM.lookup p2 gs0 of
                      Just (SpecialArea ar2) ->
                        let aSum = sumAreas ar ar2
                            sp = SpecialMerged (f aSum) p2
                        in EM.insert i sp $ EM.delete p2 gs0
                      _ -> gs0
                  mergable :: X -> Y -> Maybe HV
                  mergable x y = case EM.lookup (Point x y) gs0 of
                    Just (SpecialArea ar) ->
                      let (_, xspan, yspan) = spanArea ar
                          isFixed p =
                            p `elem` bootExtra
                            || case gs EM.! p of
                                 SpecialFixed{} -> True
                                 _ -> False
                      in if -- Limit (the aggresive) merging of normal places
                            -- and leave extra place for merging stairs.
                            | any isFixed
                              $ vicinityCardinal gx gy (Point x y) -> Nothing
                            -- Bias: prefer extending vertically.
                            -- Not @-2@, but @-4@, to merge aggressively.
                            | yspan - 4 < snd minPlaceSize -> Just Vert
                            | xspan - 4 < fst minPlaceSize -> Just Horiz
                            | otherwise -> Nothing
                    _ -> Nothing
              in case special of
                SpecialArea ar -> case mergable (px i) (py i) of
                  Nothing -> gs0
                  Just hv -> case hv of
                    -- Bias; vertical minimal sizes are smaller.
                    Vert | py i - 1 >= 0
                           && mergable (px i) (py i - 1) == Just Vert ->
                           mergeSpecial ar i{py = py i - 1} SpecialArea
                    Vert | py i + 1 < gy
                           && mergable (px i) (py i + 1) == Just Vert ->
                           mergeSpecial ar i{py = py i + 1} SpecialArea
                    Horiz | px i - 1 >= 0
                            && mergable (px i - 1) (py i) == Just Horiz ->
                            mergeSpecial ar i{px = px i - 1} SpecialArea
                    Horiz | px i + 1 < gx
                            && mergable (px i + 1) (py i) == Just Horiz ->
                            mergeSpecial ar i{px = px i + 1} SpecialArea
                    _ -> gs0
                SpecialFixed p placeGroup ar ->
                  -- If single merge is sufficient to extend the fixed place
                  -- to full size, and the merge is possible, we perform it.
                  -- An empty inner list signifies some merge is needed,
                  -- but not possible, and then we abort and don't waste space.
                  let (x0, y0, x1, y1) = fromArea ar
                      d = 3  -- arbitrary, matches common content
                      vics :: [[Point]]
                      vics = [ [i {py = py i - 1} | py i - 1 >= 0]  -- possible
                             | py p - y0 < d ]  -- needed
                             ++ [ [i {py = py i + 1} | py i + 1 < gy]
                                | y1 - py p < d ]
                             ++ [ [i {px = px i - 1} | px i - 1 >= 0]
                                | px p - x0 < d ]
                             ++ [ [i {px = px i + 1} | px i + 1 < gx]
                                | x1 - px p < d ]
                  in case vics of
                    [[p2]] -> mergeSpecial ar p2 (SpecialFixed p placeGroup)
                    _ -> gs0
                SpecialMerged{} -> error $ "" `showFailure` (gs, gs0, i)
            gs2 = foldl' mergeFixed gs $ EM.assocs gs
        voidPlaces <- do
          let gridArea = fromMaybe (error $ "" `showFailure` lgr)
                         $ toArea (0, 0, gx - 1, gy - 1)
              voidNum = round $ cmaxVoid * fromIntegral (EM.size gs2)
              isOrdinaryArea p = case p `EM.lookup` gs2 of
                Just SpecialArea{} -> True
                _ -> False
          reps <- replicateM voidNum (pointInArea gridArea)
                    -- repetitions are OK; variance is low anyway
          return $! ES.fromList $ filter isOrdinaryArea reps
        let decidePlace :: Bool
                        -> (TileMapEM, EM.EnumMap Point (Area, Fence, Area))
                        -> (Point, SpecialArea)
                        -> Rnd (TileMapEM, EM.EnumMap Point (Area, Fence, Area))
            decidePlace noVoid (!m, !qls) (!i, !special) =
              case special of
                SpecialArea ar -> do
                  -- Reserved for corridors and the global fence.
                  let innerArea = fromMaybe (error $ "" `showFailure` (i, ar))
                                  $ shrink ar
                      !_A0 = shrink innerArea
                      !_A1 = assert (isJust _A0 `blame` (innerArea, gs, kc)) ()
                  if not noVoid && i `ES.member` voidPlaces
                  then do
                    r <- mkVoidRoom innerArea
                    return (m, EM.insert i (r, FNone, ar) qls)
                  else do
                    r <- mkRoom minPlaceSize maxPlaceSize innerArea
                    Place{..} <-
                      buildPlace cops kc dnight darkCorTile litCorTile
                                 ldepth totalDepth dsecret r Nothing
                    let fence = pfence $ okind coplace qkind
                    return ( EM.union qmap m
                           , EM.insert i (qarea, fence, ar) qls )
                SpecialFixed p@Point{..} placeGroup ar -> do
                  -- Reserved for corridors and the global fence.
                  let innerArea = fromMaybe (error $ "" `showFailure` (i, ar))
                                  $ shrink ar
                      !_A0 = shrink innerArea
                      !_A1 = assert (isJust _A0 `blame` (innerArea, gs2, kc)) ()
                      !_A2 = assert (p `inside` fromJust _A0
                                     `blame` (p, innerArea, gs)) ()
                      r = mkFixed maxPlaceSize innerArea p
                      !_A3 = assert (isJust (shrink r)
                                     `blame` ( r, ar, p, innerArea, gs
                                             , gs2, qls, kc )) ()
                  Place{..} <-
                    buildPlace cops kc dnight darkCorTile litCorTile
                               ldepth totalDepth dsecret r (Just placeGroup)
                  let fence = pfence $ okind coplace qkind
                  return ( EM.union qmap m
                         , EM.insert i (qarea, fence, ar) qls )
                SpecialMerged sp p2 -> do
                  (lplaces, qplaces) <- decidePlace True (m, qls) (i, sp)
                  return (lplaces, EM.insert p2 (qplaces EM.! i) qplaces)
        places <- foldlM' (decidePlace False) (EM.empty, EM.empty)
                  $ EM.assocs gs2
        return (voidPlaces, lgr, places)
  (voidPlaces, lgrid, (lplaces, qplaces)) <- createPlaces
  let lcorridorsFun = do
        connects <- connectGrid voidPlaces lgrid
        addedConnects <- do
          let cauxNum =
                round $ cauxConnects * fromIntegral (fst lgrid * snd lgrid)
          cns <- nub . sort <$> replicateM cauxNum (randomConnection lgrid)
          -- This allows connections through a single void room,
          -- if a non-void room on both ends.
          let notDeadEnd (p, q) =
                if | p `ES.member` voidPlaces ->
                     q `ES.notMember` voidPlaces && sndInCns p
                   | q `ES.member` voidPlaces -> fstInCns q
                   | otherwise -> True
              sndInCns p = any (\(p0, q0) ->
                q0 == p && p0 `ES.notMember` voidPlaces) cns
              fstInCns q = any (\(p0, q0) ->
                p0 == q && q0 `ES.notMember` voidPlaces) cns
          return $! filter notDeadEnd cns
        let allConnects = connects `union` addedConnects
            connectPos :: (Point, Point) -> Rnd (Maybe Corridor)
            connectPos (p0, p1) =
              connectPlaces (qplaces EM.! p0) (qplaces EM.! p1)
        cs <- catMaybes <$> mapM connectPos allConnects
        let pickedCorTile = if dnight then darkCorTile else litCorTile
        return $! EM.unions (map (digCorridors pickedCorTile) cs)
  lcorridors <- lcorridorsFun
  let doorMapFun lpl lcor = do
        -- The hacks below are instead of unionWithKeyM, which is costly.
        let mergeCor _ pl cor = if Tile.isWalkable coTileSpeedup pl
                                then Nothing  -- tile already open
                                else Just (pl, cor)
            intersectionWithKeyMaybe combine =
              EM.mergeWithKey combine (const EM.empty) (const EM.empty)
            interCor = intersectionWithKeyMaybe mergeCor lpl lcor  -- fast
        mapWithKeyM (pickOpening cops kc lplaces litCorTile dsecret)
                    interCor  -- very small
  doorMap <- doorMapFun lplaces lcorridors
  let subArea = fromMaybe (error $ "" `showFailure` kc) $ shrink darea
  fence <- buildFenceRnd cops
                         cfenceTileN cfenceTileE cfenceTileS cfenceTileW subArea
  -- The obscured tile, e.g., scratched wall, stays on the server forever,
  -- only the suspect variant on client gets replaced by this upon searching.
  let sub2Area = fromMaybe (error $ "" `showFailure` kc) $ shrink subArea
      sub3Area = fromMaybe (error $ "" `showFailure` kc) $ shrink sub2Area
      likelySecret = (`inside` sub3Area)
      obscure p t = if isChancePos chidden dsecret p && likelySecret p
                    then Tile.obscureAs cotile $ Tile.buildAs cotile t
                    else return t
  lplacesObscured <- mapWithKeyM obscure lplaces
  let dmap = EM.unions [doorMap, lplacesObscured, lcorridors, fence]
        -- order matters
  return $! Cave {..}

pickOpening :: COps -> CaveKind -> TileMapEM -> ContentId TileKind
            -> Int -> Point -> (ContentId TileKind, ContentId TileKind)
            -> Rnd (ContentId TileKind)
pickOpening COps{cotile, coTileSpeedup}
            CaveKind{cdoorChance, copenChance, chidden}
            lplaces litCorTile dsecret
            pos (pl, cor) = do
  let nicerCorridor =
        if Tile.isLit coTileSpeedup cor then cor
        else -- If any cardinally adjacent walkable room tile is lit,
             -- make the opening lit, as well.
             let roomTileLit p =
                   case EM.lookup p lplaces of
                     Nothing -> False
                     Just tile -> Tile.isWalkable coTileSpeedup tile
                                  && Tile.isLit coTileSpeedup tile
                 vic = vicinityCardinalUnsafe pos
             in if any roomTileLit vic then litCorTile else cor
  -- Openings have a certain chance to be doors and doors have a certain
  -- chance to be open.
  rd <- chance cdoorChance
  if rd then do
    let hidden = Tile.buildAs cotile pl
    doorTrappedId <- Tile.revealAs cotile hidden
    let !_A = assert (Tile.buildAs cotile doorTrappedId == doorTrappedId) ()
    -- Not all solid tiles can hide a door, so @doorTrappedId@ may in fact
    -- not be a door at all, hence the check.
    if Tile.isDoor coTileSpeedup doorTrappedId then do  -- door created
      ro <- chance copenChance
      if ro
      then Tile.openTo cotile doorTrappedId
      else if isChancePos chidden dsecret pos
           then return $! doorTrappedId  -- server will hide it
           else do
             doorOpenId <- Tile.openTo cotile doorTrappedId
             Tile.closeTo cotile doorOpenId
    else return $! doorTrappedId  -- assume this is what content enforces
  else return $! nicerCorridor

digCorridors :: ContentId TileKind -> Corridor -> TileMapEM
digCorridors tile (p1:p2:ps) =
  EM.union corPos (digCorridors tile (p2:ps))
 where
  cor  = fromTo p1 p2
  corPos = EM.fromList $ zip cor (repeat tile)
digCorridors _ _ = EM.empty
