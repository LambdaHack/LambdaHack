{-# LANGUAGE TupleSections #-}
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

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import           Data.Either (rights)
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO (hFlush, stdout)
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Random.SplitMix32 as SM

import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.CaveKind
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.PlaceKind as PK
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal
import           Game.LambdaHack.Server.DungeonGen.AreaRnd
import           Game.LambdaHack.Server.DungeonGen.Cave
import           Game.LambdaHack.Server.DungeonGen.Place
import           Game.LambdaHack.Server.ServerOptions

convertTileMaps :: COps -> Bool -> Rnd (ContentId TileKind)
                -> Maybe (Rnd (ContentId TileKind)) -> Area -> TileMapEM
                -> Rnd TileMap
convertTileMaps COps{corule=RuleContent{rXmax, rYmax}, cotile, coTileSpeedup}
                areAllWalkable cdefTile mpickPassable darea ltile = do
  let outerId = ouniqGroup cotile TK.S_UNKNOWN_OUTER_FENCE
      runCdefTile :: (SM.SMGen, (Int, [(Int, ContentId TileKind)]))
                  -> ( ContentId TileKind
                     , (SM.SMGen, (Int, [(Int, ContentId TileKind)])) )
      runCdefTile (gen1, (pI, assocs)) =
        let p = toEnum pI
        in if p `inside` darea
           then case assocs of
             (p2, t2) : rest | p2 == pI -> (t2, (gen1, (pI + 1, rest)))
             _ -> let (tile, gen2) = St.runState cdefTile gen1
                  in (tile, (gen2, (pI + 1, assocs)))
           else (outerId, (gen1, (pI + 1, assocs)))
      runUnfold gen =
        let (gen1, gen2) = SM.splitSMGen gen
        in (PointArray.unfoldrNA
              rXmax rYmax runCdefTile
              (gen1, (0, IM.assocs $ EM.enumMapToIntMap ltile)), gen2)
  converted1 <- St.state runUnfold
  case mpickPassable of
    _ | areAllWalkable -> return converted1  -- all walkable; passes OK
    Nothing -> return converted1  -- no walkable tiles for filling the map
    Just pickPassable -> do  -- some tiles walkable, so ensure connectivity
      let passes p array =
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
buildTileMap cops@COps{cotile, cocave} Cave{dkind, darea, dmap} = do
  let CaveKind{cpassable, cdefTile} = okind cocave dkind
      pickDefTile = fromMaybe (error $ "" `showFailure` cdefTile)
                    <$> opick cotile cdefTile (const True)
      wcond = Tile.isEasyOpenKind
      mpickPassable =
        if cpassable
        then Just $ fromMaybe (error $ "" `showFailure` cdefTile)
                    <$> opick cotile cdefTile wcond
        else Nothing
      nwcond = not . Tile.kindHasFeature TK.Walkable
  areAllWalkable <- isNothing <$> opick cotile cdefTile nwcond
  convertTileMaps cops areAllWalkable pickDefTile mpickPassable darea dmap

anchorDown :: Y
anchorDown = 5  -- not 4, asymmetric vs up, for staircase variety;
                -- symmetry kept for @cfenceApart@ caves, to save real estate

-- Create a level from a cave.
buildLevel :: COps -> ServerOptions
           -> LevelId -> ContentId CaveKind -> CaveKind -> Int -> Int
           -> Dice.AbsDepth -> [(Point, Text)]
           -> Rnd (Level, [(Point, Text)])
buildLevel cops@COps{coplace, corule=RuleContent{..}} serverOptions
           lid dkind kc doubleDownStairs singleDownStairs
           totalDepth stairsFromUp = do
  let d = if cfenceApart kc then 1 else 0
      -- Simple rule for now: level @lid@ has depth (difficulty) @abs lid@.
      ldepth = Dice.AbsDepth $ abs $ fromEnum lid
      darea =
        let (lxPrev, lyPrev) = unzip $ map ((px &&& py) . fst) stairsFromUp
            -- Stairs take some space, hence the additions.
            lxMin = max 0
                    $ -4 - d + minimum (rXmax - 1 : lxPrev)
            lxMax = min (rXmax - 1)
                    $ 4 + d + maximum (0 : lxPrev)
            lyMin = max 0
                    $ -3 - d + minimum (rYmax - 1 : lyPrev)
            lyMax = min (rYmax - 1)
                    $ 3 + d + maximum (0 : lyPrev)
            -- Pick minimal cave size that fits all previous stairs.
            xspan = max (lxMax - lxMin + 1) $ cXminSize kc
            yspan = max (lyMax - lyMin + 1) $ cYminSize kc
            x0 = min lxMin (rXmax - xspan)
            y0 = min lyMin (rYmax - yspan)
        in fromMaybe (error $ "" `showFailure` kc)
           $ toArea (x0, y0, x0 + xspan - 1, y0 + yspan - 1)
      (lstairsDouble, lstairsSingleUp) = splitAt doubleDownStairs stairsFromUp
      pstairsSingleUp = map fst lstairsSingleUp
      pstairsDouble = map fst lstairsDouble
      pallUpStairs = pstairsDouble ++ pstairsSingleUp
      boot = let (x0, y0, x1, y1) = fromArea darea
             in rights $ map (snapToStairList 0 pallUpStairs)
                             [ Point (x0 + 4 + d) (y0 + 3 + d)
                             , Point (x1 - 4 - d) (y1 - anchorDown + 1) ]
  fixedEscape <- case cescapeFreq kc of
    [] -> return []
    escapeFreq -> do
      -- Escapes don't extend to other levels, so corners not harmful
      -- (actually neither are the other restrictions inherited from stairs
      -- placement, but we respect them to keep a uniform visual layout).
      -- Allowing corners and generating before stars, because they are more
      -- important that stairs (except the first stairs, but they are guaranteed
      -- unless the level has no incoming stairs, but if so, plenty of space).
      mepos <- placeDownStairs "escape" True serverOptions lid
                               kc darea pallUpStairs boot
      case mepos of
        Just epos -> return [(epos, escapeFreq)]
        Nothing -> return []  -- with some luck, there is an escape elsewhere
  let pescape = map fst fixedEscape
      pallUpAndEscape = pescape ++ pallUpStairs
      addSingleDown :: [Point] -> Int -> Rnd [Point]
      addSingleDown acc 0 = return acc
      addSingleDown acc k = do
        mpos <- placeDownStairs "stairs" False serverOptions lid
                                kc darea (pallUpAndEscape ++ acc) boot
        case mpos of
          Just pos -> addSingleDown (pos : acc) (k - 1)
          Nothing -> return acc  -- calling again won't change anything
  pstairsSingleDown <- addSingleDown [] singleDownStairs
  let freqDouble carried =
        filter (\(gn, _) -> carried `elem` T.words (fromGroupName gn))
        $ cstairFreq kc ++ cstairAllowed kc
      fixedStairsDouble = map (second freqDouble) lstairsDouble
      freqUp carried =
        map (first (\gn -> GroupName $ fromGroupName gn <+> "up"))
        $ freqDouble carried
      fixedStairsUp = map (second freqUp) lstairsSingleUp
      freqDown =
        map (first (\gn -> GroupName $ fromGroupName gn <+> "down"))
        $ cstairFreq kc
      fixedStairsDown = map (, freqDown) pstairsSingleDown
      pallExits = pallUpAndEscape ++ pstairsSingleDown
      fixedCenters = EM.fromList $
        fixedEscape ++ fixedStairsDouble ++ fixedStairsUp ++ fixedStairsDown
  -- Avoid completely uniform levels (e.g., uniformly merged places).
  bootExtra <- if EM.null fixedCenters then do
                 mpointExtra <-
                   placeDownStairs "extra boot" False serverOptions lid
                                   kc darea pallExits boot
                 -- With sane content, @Nothing@ should never appear.
                 return $! maybeToList mpointExtra
               else return []
  let posUp Point{..} = Point (px - 1) py
      posDn Point{..} = Point (px + 1) py
      -- This and other places ensure there is always a continuous
      -- staircase from bottom to top. This makes moving between empty
      -- level much less boring. For new levels, it may be blocked by enemies
      -- or not offer enough cover, so other staircases may be preferable.
      lstair = ( map posUp $ pstairsDouble ++ pstairsSingleUp
               , map posDn $ pstairsDouble ++ pstairsSingleDown )
  cellSize <- castDiceXY ldepth totalDepth $ ccellSize kc
  let subArea = fromMaybe (error $ "" `showFailure` kc) $ shrink darea
      area = if cfenceApart kc then subArea else darea
      (lgr, gs) = grid fixedCenters (boot ++ bootExtra) area cellSize
  dsecret <- randomWord32
  cave <- buildCave cops ldepth totalDepth darea dsecret dkind lgr gs bootExtra
  cmap <- buildTileMap cops cave
  -- The bang is needed to prevent caves memory drag until levels used.
  let !lvl = levelFromCave cops cave ldepth cmap lstair pescape
      stairCarried p0 =
        let Place{qkind} = dstairs cave EM.! p0
            freq = map (first $ T.words . tshow)
                       (PK.pfreq $ okind coplace qkind)
            carriedAll = filter (\t -> any (\(ws, _) -> t `elem` ws) freq)
                                rstairWordCarried
        in case carriedAll of
          [t] -> (p0, t)
          _ -> error $ "wrong carried stair word"
                       `showFailure` (freq, carriedAll, kc)
  return (lvl, lstairsDouble ++ map stairCarried pstairsSingleDown)

snapToStairList :: Int -> [Point] -> Point -> Either Point Point
snapToStairList _ [] p = Right p
snapToStairList a (pos : rest) p =
  let nx = if px pos > px p + 5 + a || px pos < px p - 5 - a
           then px p
           else px pos
      ny = if py pos > py p + 3 + a || py pos < py p - 3 - a
           then py p
           else py pos
      np = Point nx ny
  in if np == pos then Left np else snapToStairList a rest np

-- Places yet another staircase (or escape), taking into account only
-- the already existing stairs.
placeDownStairs :: Text -> Bool -> ServerOptions -> LevelId
                -> CaveKind -> Area -> [Point] -> [Point]
                -> Rnd (Maybe Point)
placeDownStairs object cornerPermitted serverOptions lid
                CaveKind{cminStairDist, cfenceApart} darea ps boot = do
  let dist cmin p = all (\pos -> chessDist p pos > cmin) ps
      (x0, y0, x1, y1) = fromArea darea
      -- Stairs in corners often enlarge next caves, so refrain from
      -- generating stairs, if only corner available (escapes special-cased).
      -- The bottom-right corner is exempt, becuase far from messages
      -- Also, avoid generating stairs at all on upper and left margins
      -- to keep subsequent small levels away from messages on top-right.
      rx = 9  -- enough to fit smallest stairs
      ry = 6  -- enough to fit smallest stairs
      wx = x1 - x0 + 1
      wy = y1 - y0 + 1
      notInCorner Point{..} =
        cornerPermitted
        || wx < 3 * rx + 3 || wy < 3 * ry + 3  -- everything is a corner
        || px > x0 + (wx - 3) `div` 3
           && py > y0 + (wy - 3) `div` 3
      f p = case snapToStairList 0 ps p of
        Left{} -> Nothing
        Right np -> let nnp = either id id $ snapToStairList 0 boot np
                    in if notInCorner nnp then Just nnp else Nothing
      g p = case snapToStairList 2 ps p of
        Left{} -> Nothing
        Right np -> let nnp = either id id $ snapToStairList 2 boot np
                    in if notInCorner nnp && dist cminStairDist nnp
                       then Just nnp
                       else Nothing
      focusArea = let d = if cfenceApart then 1 else 0
                  in fromMaybe (error $ "" `showFailure` darea)
                     $ toArea ( x0 + 4 + d, y0 + 3 + d
                              , x1 - 4 - d, y1 - anchorDown + 1 )
  mpos <- findPointInArea focusArea g 300 f
  -- The message fits this debugging level:
  let !_ = if isNothing mpos && sdumpInitRngs serverOptions
           then unsafePerformIO $ do
             T.hPutStrLn stdout $
                "Failed to place" <+> object <+> "on level"
                <+> tshow lid <> ", in" <+> tshow darea
             hFlush stdout
-- Not really expensive, but shouldn't disrupt normal testing nor play.
#ifdef WITH_EXPENSIVE_ASSERTIONS
             error "possible, but unexpected; alarm!"
#endif
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
       , lbig = EM.empty
       , lproj = EM.empty
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
dungeonGen cops@COps{cocave} serverOptions caves = do
  let shuffleSegment :: ([Int], [GroupName CaveKind])
                     -> Rnd [(Int, GroupName CaveKind)]
      shuffleSegment (ns, l) = assert (length ns == length l) $ do
        lShuffled <- shuffle l
        return $! zip ns lShuffled
  cavesShuffled <- mapM shuffleSegment caves
  let cavesFlat = concat cavesShuffled
      keys = map fst cavesFlat
      minD = minimum keys
      maxD = maximum keys
      freshTotalDepth = assert (signum minD == signum maxD)
                        $ Dice.AbsDepth
                        $ max 10 $ max (abs minD) (abs maxD)
      getCaveKindNum :: (Int, GroupName CaveKind)
                     -> Rnd ((LevelId, ContentId CaveKind, CaveKind), Int)
      getCaveKindNum (ln, genName) = do
        dkind <- fromMaybe (error $ "" `showFailure` genName)
                 <$> opick cocave genName (const True)
        let kc = okind cocave dkind
            ldepth = Dice.AbsDepth $ abs ln
        maxStairsNum <- castDice ldepth freshTotalDepth $ cmaxStairsNum kc
        return ((toEnum ln, dkind, kc), maxStairsNum)
  caveKindNums <- mapM getCaveKindNum cavesFlat
  let (caveKinds, caveNums) = unzip caveKindNums
      caveNumNexts = zip caveNums $ drop 1 caveNums ++ [0]
      placeStairs :: ([(Int, Int, Int)], Int)
                  -> (Int, Int)
                  -> ([(Int, Int, Int)], Int)
      placeStairs (acc, nstairsFromUp) (maxStairsNum, maxStairsNumNext) =
        let !_A1 = assert (nstairsFromUp <= maxStairsNum) ()
            -- Any stairs coming from above are kept and if they exceed
            -- @maxStairsNumNext@, the remainder ends here.
            -- If they don't exceed the minimum of @maxStairsNum@
            -- and @maxStairsNumNext@, the difference is filled up
            -- with single downstairs. The computation below maximizes
            -- the number of stairs at the cost of breaking some long
            -- staircases, except for the first one, which is always kept.
            -- Even without this exception, sometimes @maxStairsNum@
            -- could not be reached.
            doubleKept =
              minimum [1, nstairsFromUp, maxStairsNum, maxStairsNumNext]
            nstairsFromUp1 = nstairsFromUp - doubleKept
            maxStairsNum1 = maxStairsNum - doubleKept
            maxStairsNumNext1 = maxStairsNumNext - doubleKept
            singleDownStairs =
              min maxStairsNumNext1 $ maxStairsNum1 - nstairsFromUp1
            remainingNext = maxStairsNumNext1 - singleDownStairs
            doubleDownStairs = doubleKept
                               + min nstairsFromUp1 remainingNext
            !_A2 = assert (singleDownStairs >= 0) ()
            !_A3 = assert (doubleDownStairs >= doubleKept) ()
        in ( (nstairsFromUp, doubleDownStairs, singleDownStairs) : acc
           , doubleDownStairs + singleDownStairs )
      (caveStairs, nstairsFromUpLast) = foldl' placeStairs ([], 0) caveNumNexts
      caveZipped = assert (nstairsFromUpLast == 0)
                   $ zip caveKinds (reverse caveStairs)
      placeCaveKind :: ([(LevelId, Level)], [(Point, Text)])
                     -> ( (LevelId, ContentId CaveKind, CaveKind)
                        , (Int, Int, Int) )
                     -> Rnd ([(LevelId, Level)], [(Point, Text)])
      placeCaveKind (lvls, stairsFromUp)
                    ( (lid, dkind, kc)
                    , (nstairsFromUp, doubleDownStairs, singleDownStairs) ) = do
        let !_A = assert (length stairsFromUp == nstairsFromUp) ()
        (newLevel, ldown2) <-
          -- lstairUp for the next level is lstairDown for the current level
          buildLevel cops serverOptions
                     lid dkind kc doubleDownStairs singleDownStairs
                     freshTotalDepth stairsFromUp
        return ((lid, newLevel) : lvls, ldown2)
  (levels, stairsFromUpLast) <- foldlM' placeCaveKind ([], []) caveZipped
  let freshDungeon = assert (null stairsFromUpLast) $ EM.fromList levels
  return $! FreshDungeon{..}
