-- | The dungeon generation routine. It creates empty dungeons, without
-- actors and without items, either lying on the floor or embedded inside tiles.
module Game.LambdaHack.Server.DungeonGen
  ( FreshDungeon(..), dungeonGen
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , convertTileMaps, buildTileMap, anchorDown, buildLevel
  , snapToStairList, placeDownStairs, levelFromCave
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import           Data.Either (rights)
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.IO as T
import           Data.Tuple
import           System.IO (hFlush, stdout)
import           System.IO.Unsafe (unsafePerformIO)
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
import           Game.LambdaHack.Server.ServerOptions

convertTileMaps :: COps -> Bool -> Rnd (ContentId TileKind)
                -> Maybe (Rnd (ContentId TileKind)) -> Area -> TileMapEM
                -> Rnd TileMap
convertTileMaps COps{corule=RuleContent{rXmax, rYmax}, cotile, coTileSpeedup}
                areAllWalkable cdefTile mpickPassable darea ltile = do
  let outerId = ouniqGroup cotile "unknown outer fence"
      runCdefTile :: (R.StdGen, (Int, [(Point, ContentId TileKind)]))
                  -> ( ContentId TileKind
                     , (R.StdGen, (Int, [(Point, ContentId TileKind)])) )
      runCdefTile (gen1, (pI, assocs)) =
        let p = PointArray.punindex rXmax pI
        in if p `inside` darea
           then case assocs of
             (p2, t2) : rest | p2 == p -> (t2, (gen1, (pI + 1, rest)))
             _ -> let (tile, gen2) = St.runState cdefTile gen1
                  in (tile, (gen2, (pI + 1, assocs)))
           else (outerId, (gen1, (pI + 1, assocs)))
      runUnfold gen =
        let (gen1, gen2) = R.split gen
        in (PointArray.unfoldrNA rXmax rYmax runCdefTile
                                 (gen1, (0, EM.assocs ltile)), gen2)
  converted1 <- St.state runUnfold
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
          activeArea = fromMaybe (error $ "" `showFailure` darea) $ shrink darea
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

anchorDown :: Y
anchorDown = 5  -- not 4, asymmetric vs up, for staircase variety

-- Create a level from a cave.
buildLevel :: COps -> ServerOptions -> Int -> GroupName CaveKind
           -> Int -> Dice.AbsDepth -> [Point]
           -> Rnd (Level, [Point])
buildLevel cops@COps{cocave, corule} serverOptions
           ln genName minD totalDepth lstairPrev = do
  dkind <- fromMaybe (error $ "" `showFailure` genName)
           <$> opick cocave genName (const True)
  let kc = okind cocave dkind
      -- Simple rule for now: level @ln@ has depth (difficulty) @abs ln@.
      ldepth = Dice.AbsDepth $ abs ln
      darea =
        let (lxPrev, lyPrev) = unzip $ map (px &&& py) lstairPrev
            -- Stairs take some space, hence the first additions.
            -- We reserve space for caves that leave a corridor along its fence.
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
        in assert (lxMin >= 0 && lxMax <= rXmax corule - 1
                   && lyMin >= 0 && lyMax <= rYmax corule - 1)
           $ fromMaybe (error $ "" `showFailure` kc)
           $ toArea (x0, y0, x0 + xspan - 1, y0 + yspan - 1)
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
      boot = let (x0, y0, x1, y1) = fromArea darea
             in rights $ map (snapToStairList lallUpStairs)
                             [ Point (x0 + 4) (y0 + 3)
                             , Point (x1 - 4) (y1 - anchorDown + 1) ]
      freq = toFreq ("buildLevel" <+> tshow ln) $ map swap $ cstairFreq kc
      addSingleDown :: [(Point, GroupName PlaceKind)] -> Int
                    -> Rnd [(Point, GroupName PlaceKind)]
      addSingleDown acc 0 = return acc
      addSingleDown acc k = do
        mpos <- placeDownStairs "stairs" False serverOptions ln
                                kc darea (lallUpStairs ++ map fst acc) boot
        case mpos of
          Just pos -> do
            stairGroup <- frequency freq
            addSingleDown ((pos, stairGroup) : acc) (k - 1)
          Nothing -> return acc  -- calling again won't change anything
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
      -- Escapes don't extent to other levels, so corners not harmful
      -- and also escapes should not fail to generate, if possible.
      mepos <- placeDownStairs "escape" True serverOptions ln
                               kc darea lallStairs boot
      case mepos of
        Just epos -> return [(epos, escapeGroup)]
        Nothing -> return []  -- with some luck, there is an escape elsewhere
  let lescape = map fst fixedEscape
      fixedCenters = EM.fromList $
        fixedEscape ++ fixedStairsDouble ++ fixedStairsUp ++ fixedStairsDown
  -- Avoid completely uniform levels (e.g., uniformly merged places).
  bootExtra <- if EM.null fixedCenters then do
                 let lallExits = map fst fixedEscape ++ lallStairs
                 mpointExtra <-
                   placeDownStairs "extra boot" False serverOptions ln
                                   kc darea lallExits boot
                 -- With sane content, @Nothing@ should never appear.
                 return $! maybeToList mpointExtra
               else return []
  let posUp Point{..} = Point (px - 1) py
      posDn Point{..} = Point (px + 1) py
      lstair = ( map posUp $ lstairsSingleUp ++ lstairsDouble
               , map posDn $ lstairsDouble ++ lstairsSingleDown )
  cellSize <- castDiceXY ldepth totalDepth $ ccellSize kc
  let subArea = fromMaybe (error $ "" `showFailure` kc) $ shrink darea
      area = if cfenceApart kc then subArea else darea
      (lgr, gs) = grid fixedCenters (boot ++ bootExtra) area cellSize
  dsecret <- randomR (1, maxBound)
  cave <- buildCave cops ldepth totalDepth darea dsecret dkind lgr gs bootExtra
  cmap <- buildTileMap cops cave
  let lvl = levelFromCave cops cave ldepth cmap lstair lescape
  return (lvl, lstairsDouble ++ lstairsSingleDown)

snapToStairList :: [Point] -> Point -> Either Point Point
snapToStairList [] p = Right p
snapToStairList (pos : rest) p =
  let nx = if px pos > px p + 5 || px pos < px p - 5 then px p else px pos
      ny = if py pos > py p + 3 || py pos < py p - 3 then py p else py pos
      np = Point nx ny
  in if np == pos then Left np else snapToStairList rest np

-- Places yet another staircase (or escape), taking into account only
-- the already existing stairs.
placeDownStairs :: Text -> Bool -> ServerOptions -> Int
                -> CaveKind -> Area -> [Point] -> [Point]
                -> Rnd (Maybe Point)
placeDownStairs object cornerPermitted serverOptions ln
                CaveKind{cminStairDist} darea ps boot = do
  let dist cmin p = all (\pos -> chessDist p pos > cmin) ps
      (x0, y0, x1, y1) = fromArea darea
      -- Stairs in corners often enlarge next caves, so refrain from
      -- generating stairs, if only corner available (escapes special-cased).
      notInCorner Point{..} =
        cornerPermitted
        || px > x0 + 9 && px < x1 - 9
        || py > y0 + 6 && py < y1 - 6
      f p = case snapToStairList ps p of
        Left{} -> Nothing
        Right np -> let nnp = either id id $ snapToStairList boot np
                    in if notInCorner nnp then Just nnp else Nothing
      g p = case f p of
        Just np | dist cminStairDist np -> Just np
        _ -> Nothing
      focusArea = fromMaybe (error $ "" `showFailure` darea)
                  $ toArea (x0 + 4, y0 + 3, x1 - 4, y1 - anchorDown + 1)
  mpos <- findPointInArea focusArea g 100 f
  -- The message fits this debugging level:
  let !_ = if isNothing mpos && sdumpInitRngs serverOptions
           then unsafePerformIO $ do
             T.hPutStrLn stdout $
                "Failed to place" <+> object <+> "on level"
                <+> tshow ln <> ", in" <+> tshow darea
             hFlush stdout
           else ()
  return mpos

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
       , lentry = dentry
       , larea = darea
       , lsmell = EM.empty
       , lstair
       , lescape
       , lseen = 0
       , lexpl
       , ltime = timeZero
       , lnight = dnight
       }

-- | Freshly generated and not yet populated dungeon.
data FreshDungeon = FreshDungeon
  { freshDungeon    :: Dungeon        -- ^ maps for all levels
  , freshTotalDepth :: Dice.AbsDepth  -- ^ absolute dungeon depth
  }

-- | Generate the dungeon for a new game.
dungeonGen :: COps -> ServerOptions -> Caves -> Rnd FreshDungeon
dungeonGen cops serverOptions caves = do
  let (minD, maxD) | Just ((s, _), _) <- IM.minViewWithKey caves
                   , Just ((e, _), _) <- IM.maxViewWithKey caves
                   = (s, e)
                   | otherwise
                   = error $ "no caves" `showFailure` caves
      freshTotalDepth = assert (signum minD == signum maxD)
                        $ Dice.AbsDepth
                        $ max 10 $ max (abs minD) (abs maxD)
      buildLvl :: ([(LevelId, Level)], [Point])
               -> (Int, GroupName CaveKind)
               -> Rnd ([(LevelId, Level)], [Point])
      buildLvl (l, ldown) (n, genName) = do
        -- lstairUp for the next level is lstairDown for the current level
        (lvl, ldown2) <-
          buildLevel cops serverOptions n genName minD freshTotalDepth ldown
        return ((toEnum n, lvl) : l, ldown2)
  (levels, _) <- foldlM' buildLvl ([], []) $ reverse $ IM.assocs caves
  let freshDungeon = EM.fromList levels
  return $! FreshDungeon{..}
