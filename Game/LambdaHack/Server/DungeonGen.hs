-- | The unpopulated dungeon generation routine.
module Game.LambdaHack.Server.DungeonGen
  ( FreshDungeon(..), dungeonGen
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , convertTileMaps, placeStairs, buildLevel, levelFromCaveKind, findGenerator
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import qualified System.Random as R

import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.CaveKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import Game.LambdaHack.Server.DungeonGen.Area
import Game.LambdaHack.Server.DungeonGen.Cave
import Game.LambdaHack.Server.DungeonGen.Place

convertTileMaps :: Kind.COps -> Bool
                -> Rnd (Kind.Id TileKind) -> Maybe (Rnd (Kind.Id TileKind))
                -> Int -> Int -> TileMapEM
                -> Rnd TileMap
convertTileMaps Kind.COps{coTileSpeedup} areAllWalkable
                cdefTile mcdefTileWalkable cxsize cysize ltile = do
  let runCdefTile :: R.StdGen -> (Kind.Id TileKind, R.StdGen)
      runCdefTile = St.runState cdefTile
      runUnfold gen =
        let (gen1, gen2) = R.split gen
        in (PointArray.unfoldrNA cxsize cysize runCdefTile gen1, gen2)
  converted0 <- St.state runUnfold
  let converted1 = converted0 PointArray.// EM.assocs ltile
  case mcdefTileWalkable of
    _ | areAllWalkable -> return converted1  -- all walkable; passes OK
    Nothing -> return converted1  -- no walkable tiles for filling the map
    Just cdefTileWalkable -> do  -- some tiles walkable, so ensure connectivity
      -- TODO: perhaps checking connectivity with BFS would be better,
      -- but it's still artibrary how we recover connectivity and we still
      -- need ltile not to break rooms (unless that's a good idea,
      -- but surely it's not for starship hull walls, vaults, fire pits, etc.,
      -- so perhaps all but impenetrable walls is game).
      let passes p@Point{..} array =
            px >= 0 && px <= cxsize - 1
            && py >= 0 && py <= cysize - 1
            && Tile.isWalkable coTileSpeedup (array PointArray.! p)
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
            let g n c = if included n
                           && not (Tile.isWalkable coTileSpeedup c)
                           && n `EM.notMember` ltile
                           && blocks n array
                        then walkableTile
                        else c
            in PointArray.imapA g array
      walkable2 <- cdefTileWalkable
      let converted2 = connect xeven blocksHorizontal walkable2 converted1
      walkable3 <- cdefTileWalkable
      let converted3 = connect yeven blocksVertical walkable3 converted2
      walkable4 <- cdefTileWalkable
      let converted4 =
            connect (not . xeven) blocksHorizontal walkable4 converted3
      walkable5 <- cdefTileWalkable
      let converted5 =
            connect (not . yeven) blocksVertical walkable5 converted4
      return converted5

buildTileMap :: Kind.COps -> Cave -> Rnd TileMap
buildTileMap cops@Kind.COps{ cotile=Kind.Ops{opick}
                           , cocave=Kind.Ops{okind=cokind} }
             Cave{dkind, dmap, dnight} = do
  let CaveKind{cxsize, cysize, cpassable, cdefTile} = cokind dkind
      nightCond kt = not (Tile.kindHasFeature TK.Clear kt)
                     || (if dnight then id else not)
                           (Tile.kindHasFeature TK.Dark kt)
      dcond kt = (cpassable
                  || not (Tile.kindHasFeature TK.Walkable kt))
                 && nightCond kt
      pickDefTile =
        fromMaybe (assert `failure` cdefTile) <$> opick cdefTile dcond
      wcond kt = Tile.kindHasFeature TK.Walkable kt
                 && nightCond kt
      mpickWalkable =
        if cpassable
        then Just
             $ fromMaybe (assert `failure` cdefTile) <$> opick cdefTile wcond
        else Nothing
      nwcond kt = not (Tile.kindHasFeature TK.Walkable kt) && nightCond kt
  areAllWalkable <- isNothing <$> opick cdefTile nwcond
  convertTileMaps cops areAllWalkable
                  pickDefTile mpickWalkable cxsize cysize dmap

-- @ps@ and the result are the points in the middle of starcase
condStairs :: Kind.COps -> TileMap -> [Point] -> Point -> Kind.Id TileKind
           -> Bool
condStairs Kind.COps{coTileSpeedup} cmap ps p !t =
  let (xsize, ysize) = PointArray.sizeA cmap
      -- The border of the dungeon should not be excavated around staircase.
      area = (3, 2, xsize - 4, ysize - 3)
      dist !cmin !p0 _ = all (\ !pos -> chessDist p0 pos > cmin) ps
  in p `inside` area
     && Tile.isWalkable coTileSpeedup t
     && not (Tile.isNoActor coTileSpeedup t)
     && dist 3 p t

placeStairs :: Kind.COps -> TileMap -> CaveKind -> [Point] -> Rnd Point
placeStairs cops@Kind.COps{coTileSpeedup}
            cmap CaveKind{cminStairDist} ps = do
  let dist !cmin !p _ = all (\ !pos -> chessDist p pos > cmin) ps
      ds = [ dist cminStairDist
           , dist (2 * (cminStairDist `div` 3))
           , dist (cminStairDist `div` 2)
           , dist (cminStairDist `div` 3)
           , dist (cminStairDist `div` 5) ]
  findPosTry2 200 cmap
    (condStairs cops cmap ps)
    ds
    (\_ !t -> Tile.isOftenActor coTileSpeedup t)
    ds

type Staircase = (Point, ( Maybe (Kind.Id TileKind)
                         , Maybe (Kind.Id TileKind) ))

-- | Create a level from a cave.
buildLevel :: Kind.COps -> Cave -> Int -> Int -> Int -> AbsDepth
           -> [Point] -> Maybe Bool -> TileMap
           -> Rnd Level
buildLevel cops@Kind.COps{ cotile=Kind.Ops{opick}
                         , cocave=Kind.Ops{okind=cokind}
                         , coTileSpeedup}
           Cave{dkind, dplaces}
           ln minD maxD totalDepth lstairUpRaw escapeFeature cmap = do
  let kc@CaveKind{citemNum, clegendLitTile} = cokind dkind
      fitArea pos = inside pos . fromArea . qarea
      findLegend pos = maybe clegendLitTile qlegend
                       $ find (fitArea pos) dplaces
      hasEscape p = Tile.kindHasFeature (TK.Cause $ IK.Escape p)
      ascendable  = Tile.kindHasFeature (TK.Cause $ IK.Ascend 1)
      descendable = Tile.kindHasFeature (TK.Cause $ IK.Ascend (-1))
      noAsc = ln == maxD
      noDesc = ln == minD
      makeStairs :: Bool -> [Staircase] -> Rnd [Staircase]
      makeStairs moveUp stairs =
        if (if moveUp then noAsc else noDesc) then
          return stairs
        else do
          let cond tk = if moveUp then ascendable tk else descendable tk
              posCur = map fst stairs
          spos <- placeStairs cops cmap kc posCur
          let legend = findLegend spos
          stairId <- fromMaybe (assert `failure` legend) <$> opick legend cond
          return $! stairs ++ [(spos, if moveUp
                                      then (Just stairId, Nothing)
                                      else (Nothing, Just stairId))]
      condS p = Tile.isWalkable coTileSpeedup (cmap PointArray.! p)
                && not (Tile.isNoActor coTileSpeedup (cmap PointArray.! p))
      posUp Point{..} = Point (px - 1) py
      posDn Point{..} = Point (px + 1) py
      lstairUp = map posUp lstairUpRaw  -- prevent drift to the right
      condPair p = condS p || condS (posUp p) || condS (posDn p)
  stairs1 <-
    -- If all stairs can be continued, do that, otherwise shuffle all starcases
    -- (then the levels are separated by a thick layer that mixes up exits).
    -- These are kept first on the lstair, so they match up between levels.
    if all condPair lstairUp
    then do
      let f :: Point -> Rnd Staircase
          f p = do
            let legend = findLegend p
            stairUpId <- fromMaybe (assert `failure` legend)
                         <$> opick legend ascendable
            if noDesc then return (p, (Just stairUpId, Nothing))
            else do
              stairDownId <- fromMaybe (assert `failure` legend)
                             <$> opick legend descendable
              return (p, (Just stairUpId, Just stairDownId))
      mapM f lstairUp
    else return []
  -- If not too many, add extra stairs down.
  let lenStairsDown1 = length $ catMaybes $ map (snd . snd) stairs1
  stairs2 <- if lenStairsDown1 < 3
             then makeStairs False stairs1
             else return stairs1
  -- Fill up stairs up.
  let lenStairsUp2 = length $ catMaybes $ map (fst . snd) stairs2
      nstairUpLeft = length lstairUp - lenStairsUp2
  stairs3 <- foldlM' (\sts _ -> makeStairs True sts) stairs2 [1 .. nstairUpLeft]
  let posTotal = map fst $ stairs3
  escape <- case escapeFeature of
              Nothing -> return []
              Just True -> do
                epos <- placeStairs cops cmap kc posTotal
                let legend = findLegend epos
                upEscape <- fmap (fromMaybe $ assert `failure` legend)
                            $ opick legend $ hasEscape 1
                return [(epos, (Just upEscape, Nothing))]
              Just False -> do
                epos <- placeStairs cops cmap kc posTotal
                let legend = findLegend epos
                downEscape <- fmap (fromMaybe $ assert `failure` legend)
                              $ opick legend $ hasEscape (-1)
                return [(epos, (Nothing, Just downEscape))]
  let vicMoves = map (uncurry Vector)
        [ (-2, -1), (-1, -1), (0, -1), (1, -1), (2, -1), (2, 0)
        , (2, 1), (1, 1), (0, 1), (-1, 1), (-2, 1), (-2, 0) ]
      vicUnsafe :: Point -> [Point]
      vicUnsafe p = [ shift p dxy | dxy <- vicMoves ]
      addVicinity (p, (mUp, mDn)) =
        let pt = if | condS p -> p
                    | condS (posUp p) -> posUp p
                    | otherwise -> assert (condS (posDn p)) $ posDn p
            old = cmap PointArray.! pt
            vic = p : vicUnsafe p
                  ++ if isNothing mUp then [posUp p] else []
                  ++ if isNothing mDn then [posDn p] else []
        in [(posUp p, idUp) | idUp <- maybeToList mUp]
           ++ [(posDn p, idDn) | idDn <- maybeToList mDn]
           ++ map (\p0 -> (p0, old)) vic
      exitTiles = concatMap addVicinity $ stairs3 ++ escape
      ltile = cmap PointArray.// exitTiles
      lstair = ( map (posUp . fst) $ filter (isJust . fst . snd) stairs3
               , map (posDn . fst) $ filter (isJust . snd . snd) stairs3 )
  -- A simple rule for now: level at level @ln@ has depth (difficulty) @abs ln@.
      ldepth = AbsDepth $ abs ln
  litemNum <- castDice ldepth totalDepth citemNum
  lsecret <- randomR (1, maxBound)  -- 0 means unknown
  return $! levelFromCaveKind cops kc ldepth ltile lstair
                              litemNum lsecret (map fst escape)

-- | Build rudimentary level from a cave kind.
levelFromCaveKind :: Kind.COps
                  -> CaveKind -> AbsDepth -> TileMap -> ([Point], [Point])
                  -> Int -> Int -> [Point]
                  -> Level
levelFromCaveKind Kind.COps{coTileSpeedup}
                  CaveKind{ cactorCoeff=lactorCoeff
                          , cactorFreq=lactorFreq
                          , citemFreq=litemFreq
                          , ..
                          }
                  ldepth ltile lstair litemNum lsecret lescape =
  let f n t | Tile.isExplorable coTileSpeedup t = n + 1
            | otherwise = n
      lclear = PointArray.foldlA' f 0 ltile
  in Level
       { ldepth
       , lfloor = EM.empty
       , lembed = EM.empty  -- is populated inside $MonadServer$
       , lactor = EM.empty
       , ltile
       , lxsize = cxsize
       , lysize = cysize
       , lsmell = EM.empty
       , ldesc = cname
       , lstair
       , lseen = 0
       , lclear
       , ltime = timeZero
       , lactorCoeff
       , lactorFreq
       , litemNum
       , litemFreq
       , lsecret
       , lhidden = chidden
       , lescape
       }

findGenerator :: Kind.COps -> AbsDepth
              -> Int -> (GroupName CaveKind, Maybe Bool)
              -> Rnd (Int, TileMap, Cave, Maybe Bool)
findGenerator cops totalDepth n (genName, escapeFeature) = do
  let Kind.COps{cocave=Kind.Ops{opick}} = cops
  ci <- fromMaybe (assert `failure` genName) <$> opick genName (const True)
  -- A simple rule for now: level at level @ln@ has depth (difficulty) @abs ln@.
  let ldepth = AbsDepth $ abs n
  cave <- buildCave cops ldepth totalDepth ci
  cmap <- buildTileMap cops cave
  return (n, cmap, cave, escapeFeature)

-- | Freshly generated and not yet populated dungeon.
data FreshDungeon = FreshDungeon
  { freshDungeon    :: !Dungeon   -- ^ maps for all levels
  , freshTotalDepth :: !AbsDepth  -- ^ absolute dungeon depth
  }

-- | Generate the dungeon for a new game.
dungeonGen :: Kind.COps -> Caves -> Rnd FreshDungeon
dungeonGen cops caves = do
  let (minD, maxD) =
        case (IM.minViewWithKey caves, IM.maxViewWithKey caves) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> assert `failure` "no caves" `twith` caves
      freshTotalDepth = assert (signum minD == signum maxD)
                        $ AbsDepth
                        $ max 10 $ max (abs minD) (abs maxD)
  tileMaps <- mapM (uncurry $ findGenerator cops freshTotalDepth)
                   (IM.assocs caves)
  let buildLvl :: ([Point], [(LevelId, Level)])
               -> (Int, TileMap, Cave, Maybe Bool)
               -> Rnd ([Point], [(LevelId, Level)])
      buildLvl (lstairUp, l) (n, cmap, cave, escapeFeature) = do
        lvl <- buildLevel cops cave n minD maxD freshTotalDepth
               lstairUp escapeFeature cmap
        -- lstairUp for the next level is lstairDown for the current level
        let lstairDown = snd $ lstair lvl
        return (lstairDown, (toEnum n, lvl) : l)
  (lstairUpLast, levels) <- foldlM' buildLvl ([], []) $ reverse tileMaps
  let !_A = assert (null lstairUpLast) ()
  let freshDungeon = EM.fromList levels
  return $! FreshDungeon{..}
