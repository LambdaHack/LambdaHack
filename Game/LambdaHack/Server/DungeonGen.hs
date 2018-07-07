-- | The dungeon generation routine. It creates empty dungeons, without
-- actors and without items, either lying on the floor or embedded inside tiles.
module Game.LambdaHack.Server.DungeonGen
  ( FreshDungeon(..), dungeonGen
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , convertTileMaps, buildTileMap, buildLevel, placeDownStairs, levelFromCave
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import           Data.Tuple
import qualified System.Random as R

import           Game.LambdaHack.Common.Area
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Frequency
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.CaveKind
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.PlaceKind (PlaceKind)
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import           Game.LambdaHack.Server.DungeonGen.AreaRnd
import           Game.LambdaHack.Server.DungeonGen.Cave
import           Game.LambdaHack.Server.DungeonGen.Place

convertTileMaps :: COps -> Bool -> Rnd (ContentId TileKind)
                -> Maybe (Rnd (ContentId TileKind)) -> Area -> TileMapEM
                -> Rnd TileMap
convertTileMaps COps{corule=RuleContent{rXmax, rYmax}, cotile, coTileSpeedup}
                areAllWalkable cdefTile mpickPassable darea ltile = do
  let activeArea = fromMaybe (error $ "" `showFailure` darea) $ shrink darea
      outerId = ouniqGroup cotile "unknown outer fence"
      runCdefTile :: (R.StdGen, Int) -> (ContentId TileKind, (R.StdGen, Int))
      runCdefTile (gen1, pI) =
        if PointArray.punindex rXmax pI `inside` activeArea
        then let (tile, gen2) = St.runState cdefTile gen1
             in (tile, (gen2, pI + 1))
        else (outerId, (gen1, pI + 1))
      runUnfold gen =
        let (gen1, gen2) = R.split gen
        in (PointArray.unfoldrNA rXmax rYmax runCdefTile (gen1, 0), gen2)
  converted0 <- St.state runUnfold
  let converted1 = converted0 PointArray.// EM.assocs ltile
  case mpickPassable of
    _ | areAllWalkable -> return converted1  -- all walkable; passes OK
    Nothing -> return converted1  -- no walkable tiles for filling the map
    Just pickPassable -> do  -- some tiles walkable, so ensure connectivity
      let passes p@Point{..} array =
            Tile.isWalkable coTileSpeedup (array PointArray.! p)
          -- If no point blocks on both ends, then I can eventually go
          -- from bottom to top of the map and from left to right
          -- unless there are disconnected areas inside rooms).
          blocksHorizontal (Point x y) array =
            not (passes (Point (x + 1) y) array
                 || passes (Point (x - 1) y) array)
          blocksVertical (Point x y) array =
            not (passes (Point x (y + 1)) array
                 || passes (Point x (y - 1)) array)
          xeven Point{..} = px `mod` 2 == 0
          yeven Point{..} = py `mod` 2 == 0
          connect included blocks walkableTile array =
            let g p c = if p `inside` activeArea
                           && included p
                           && not (Tile.isEasyOpen coTileSpeedup c)
                           && p `EM.notMember` ltile
                           && blocks p array
                        then walkableTile
                        else c
            in PointArray.imapA g array
      walkable2 <- pickPassable
      let converted2 = connect xeven blocksHorizontal walkable2 converted1
      walkable3 <- pickPassable
      let converted3 = connect yeven blocksVertical walkable3 converted2
      walkable4 <- pickPassable
      let converted4 =
            connect (not . xeven) blocksHorizontal walkable4 converted3
      walkable5 <- pickPassable
      let converted5 =
            connect (not . yeven) blocksVertical walkable5 converted4
      return converted5

buildTileMap :: COps -> Cave -> Rnd TileMap
buildTileMap cops@COps{cotile, cocave}
             Cave{dkind, darea, dmap, dnight} = do
  let CaveKind{cpassable, cdefTile} = okind cocave dkind
      nightCond kt = not (Tile.kindHasFeature TK.Walkable kt)
                     || (if dnight then id else not)
                           (Tile.kindHasFeature TK.Dark kt)
      pickDefTile = fromMaybe (error $ "" `showFailure` cdefTile)
                    <$> opick cotile cdefTile nightCond
      wcond kt = Tile.isEasyOpenKind kt && nightCond kt
      mpickPassable =
        if cpassable
        then Just $ fromMaybe (error $ "" `showFailure` cdefTile)
                    <$> opick cotile cdefTile wcond
        else Nothing
      nwcond kt = not (Tile.kindHasFeature TK.Walkable kt) && nightCond kt
  areAllWalkable <- isNothing <$> opick cotile cdefTile nwcond
  convertTileMaps cops areAllWalkable pickDefTile mpickPassable darea dmap

-- Create a level from a cave.
buildLevel :: COps -> [Point] -> Int -> GroupName CaveKind
           -> Int -> Dice.AbsDepth -> [Point]
           -> Rnd (Level, [Point])
buildLevel cops@COps{cocave, corule}
           boot ln genName minD totalDepth lstairPrev = do
  dkind <- fromMaybe (error $ "" `showFailure` genName)
           <$> opick cocave genName (const True)
  let kc = okind cocave dkind
      -- Simple rule for now: level @ln@ has depth (difficulty) @abs ln@.
      ldepth = Dice.AbsDepth $ abs ln
      (darea, dxspan, dyspan) =
        let (lxPrev, lyPrev) = unzip $ map (px &&& py) lstairPrev
            -- Stairs take some space, hence the first additions.
            -- We reserve space for caves that leave a corridor along boundary.
            lxMin = -4 + minimum (rXmax corule - 1 : lxPrev)
            lxMax = 4 + maximum (0 : lxPrev)
            lyMin = -3 + minimum (rYmax corule - 1 : lyPrev)
            lyMax = 3 + maximum (0 : lyPrev)
            -- Pick minimal cave size that fits all previous stairs.
            xspan = max (lxMax - lxMin + 1) $ cXminSize kc
            yspan = max (lyMax - lyMin + 1) $ cYminSize kc
            x0 = min lxMin
                 $ max (lxMax - xspan + 1)
                 $ (rXmax corule - xspan) `div` 2
            y0 = min lyMin
                 $ max (lyMax - yspan + 1)
                 $ (rYmax corule - yspan) `div` 2
        in ( assert (lxMin >= 0 && lxMax <= rXmax corule - 1
                     && lyMin >= 0 && lyMax <= rYmax corule - 1)
             $ fromMaybe (error $ "" `showFailure` kc)
             $ toArea (x0, y0, x0 + xspan - 1, y0 + yspan - 1)
           , xspan
           , yspan )
  -- Any stairs coming from above are considered extra stairs
  -- and if they don't exceed @extraStairs@,
  -- the amount is filled up with single downstairs.
  -- If they do exceed @extraStairs@, some of them end here.
  extraStairs <- castDice ldepth totalDepth $ cextraStairs kc
  let (abandonedStairs, remainingStairsDown) =
        if ln == minD then (length lstairPrev, 0)
        else let double = min (length lstairPrev) extraStairs
                 single = max 0 $ extraStairs - double
             in (length lstairPrev - double, single)
      (lstairsSingleUp, lstairsDouble) = splitAt abandonedStairs lstairPrev
      lallUpStairs = lstairsDouble ++ lstairsSingleUp
      freq = toFreq ("buildLevel" <+> tshow ln) $ map swap $ cstairFreq kc
      addSingleDown :: [(Point, GroupName PlaceKind)] -> Int
                    -> Rnd [(Point, GroupName PlaceKind)]
      addSingleDown acc 0 = return acc
      addSingleDown acc k = do
        pos <- placeDownStairs kc darea (lallUpStairs ++ map fst acc) boot
        stairGroup <- frequency freq
        addSingleDown ((pos, stairGroup) : acc) (k - 1)
  stairsSingleDown <- addSingleDown [] remainingStairsDown
  let lstairsSingleDown = map fst stairsSingleDown
  fixedStairsDouble <- mapM (\p -> do
    stairGroup <- frequency freq
    return (p, stairGroup)) lstairsDouble
  fixedStairsUp <- mapM (\p -> do
    stairGroup <- frequency freq
    return (p, toGroupName $ tshow stairGroup <+> "up")) lstairsSingleUp
  let fixedStairsDown = map (\(p, t) ->
        (p, toGroupName $ tshow t <+> "down")) stairsSingleDown
      lallStairs = lallUpStairs ++ lstairsSingleDown
  fixedEscape <- case cescapeGroup kc of
                   Nothing -> return []
                   Just escapeGroup -> do
                     epos <- placeDownStairs kc darea lallStairs boot
                     return [(epos, escapeGroup)]
  let lescape = map fst fixedEscape
      fixedCenters = EM.fromList $
        fixedEscape ++ fixedStairsDouble ++ fixedStairsUp ++ fixedStairsDown
      posUp Point{..} = Point (px - 1) py
      posDn Point{..} = Point (px + 1) py
      lstair = ( map posUp $ lstairsSingleUp ++ lstairsDouble
               , map posDn $ lstairsDouble ++ lstairsSingleDown )
  cellSize <- castDiceXY ldepth totalDepth $ ccellSize kc
  -- This is precisely how the cave will be divided among places,
  -- if there are no fixed centres except at boot coordinates.
  -- In any case, places, except for at boot points and fixed centres,
  -- are guaranteed at least the rolled minimal size of their
  -- enclosing cell (with one shared fence). Fixed centres are guaranteed
  -- a size between the cave cell size and the one implied by their
  -- placement wrt to cave boundary and other fixed centers.
  let (xspan, yspan) | couterFenceTile kc /= "basic outer fence" =
                         (dxspan - 2, dyspan - 2)
                     | otherwise = (dxspan, dyspan)
      lgrid =
        -- Size, minus boot points, including boundary tiles.
        let remainingSpan = ( xspan - 1 - 4 - 4
                            , yspan - 1 - 3 - anchorDown + 1 )
        in ( fst remainingSpan `div` fst cellSize
           , snd remainingSpan `div` snd cellSize )
  dsecret <- randomR (1, maxBound)
  cave <- buildCave cops ldepth totalDepth darea dsecret dkind
                    lgrid fixedCenters boot
  cmap <- buildTileMap cops cave
  let lvl = levelFromCave cops cave ldepth cmap lstair lescape
  return (lvl, lstairsDouble ++ lstairsSingleDown)

-- Places yet another staircase (or escape), taking into account only
-- the already existing stairs.
placeDownStairs :: CaveKind -> Area -> [Point] -> [Point] -> Rnd Point
placeDownStairs CaveKind{cminStairDist} darea ps boot = do
  let dist cmin p = all (\pos -> chessDist p pos > cmin) ps
      -- The check is important even for @boot@ points outside @darena@,
      -- because on other levels they will be stair candidates.
      distProj p = all (\pos -> (px pos == px p
                                 || px pos > px p + 5
                                 || px pos < px p - 5)
                                && (py pos == py p
                                    || py pos > py p + 3
                                    || py pos < py p - 3))
                   $ ps ++ boot
      minDist = if length ps >= 3 then 0 else cminStairDist
      (x0, y0, x1, y1) = fromArea darea
      interior = fromMaybe (error $ "" `showFailure` darea)
                 $ toArea (x0 + 9, y0 + 8, x1 - 9, y1 - anchorDown - 4)
      f p@Point{..} =
        if p `inside` interior
        then if dist minDist p && distProj p then Just p else Nothing
        else let nx = if | px < x0 + 9 -> x0 + 4
                         | px > x1 - 9 -> x1 - 4
                         | otherwise -> px
                 ny = if | py < y0 + 8 -> y0 + 3
                         | py > y1 - anchorDown - 4 -> y1 - anchorDown + 1
                         | otherwise -> py
                 np = Point nx ny
             in if dist 0 np && distProj np then Just np else Nothing
  findPointInArea darea f

-- Build rudimentary level from a cave kind.
levelFromCave :: COps -> Cave -> Dice.AbsDepth
              -> TileMap -> ([Point], [Point]) -> [Point]
              -> Level
levelFromCave COps{coTileSpeedup} Cave{..} ldepth ltile lstair lescape =
  let f n t | Tile.isExplorable coTileSpeedup t = n + 1
            | otherwise = n
      lexpl = PointArray.foldlA' f 0 ltile
  in Level
       { lkind = dkind
       , ldepth
       , lfloor = EM.empty
       , lembed = EM.empty
       , lactor = EM.empty
       , ltile
       , larea = darea
       , lsmell = EM.empty
       , lstair
       , lescape
       , lseen = 0
       , lexpl
       , ltime = timeZero
       , lnight = dnight
       }

bootFixedCenters :: RuleContent -> [Point]
bootFixedCenters RuleContent{rXmax, rYmax} =
  [Point 4 3, Point (rXmax - 5) (rYmax - anchorDown)]

-- | Freshly generated and not yet populated dungeon.
data FreshDungeon = FreshDungeon
  { freshDungeon    :: Dungeon        -- ^ maps for all levels
  , freshTotalDepth :: Dice.AbsDepth  -- ^ absolute dungeon depth
  }

-- | Generate the dungeon for a new game.
dungeonGen :: COps -> Caves -> Rnd FreshDungeon
dungeonGen cops caves = do
  let (minD, maxD) | Just ((s, _), _) <- IM.minViewWithKey caves
                   , Just ((e, _), _) <- IM.maxViewWithKey caves
                   = (s, e)
                   | otherwise
                   = error $ "no caves" `showFailure` caves
      freshTotalDepth = assert (signum minD == signum maxD)
                        $ Dice.AbsDepth
                        $ max 10 $ max (abs minD) (abs maxD)
      boot = bootFixedCenters $ corule cops
      buildLvl :: ([(LevelId, Level)], [Point])
               -> (Int, GroupName CaveKind)
               -> Rnd ([(LevelId, Level)], [Point])
      buildLvl (l, ldown) (n, genName) = do
        -- lstairUp for the next level is lstairDown for the current level
        (lvl, ldown2) <-
          buildLevel cops boot n genName minD freshTotalDepth ldown
        return ((toEnum n, lvl) : l, ldown2)
  (levels, _) <- foldlM' buildLvl ([], []) $ reverse $ IM.assocs caves
  let freshDungeon = EM.fromList levels
  return $! FreshDungeon{..}
