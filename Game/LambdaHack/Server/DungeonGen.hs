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
import Game.LambdaHack.Content.ItemKind (ItemKind)
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

condStairs :: Kind.COps -> Bool -> TileMap -> [Point]
           -> Point -> Kind.Id TileKind
           -> Bool
condStairs Kind.COps{coTileSpeedup} moveUp cmap ps p !t =
  let (xsize, ysize) = PointArray.sizeA cmap
      yUpOffset = if moveUp then -1 else 1
      -- The border of the dungeon should not be excavated around stairs.
      area = (2, 3 + yUpOffset , xsize - 3, ysize - 4 + yUpOffset)
      dist !cmin !p0 _ = all (\ !pos -> chessDist p0 pos > cmin) ps
  in p `inside` area
     && Tile.isWalkable coTileSpeedup t
     && not (Tile.isNoActor coTileSpeedup t)
     && dist 3 p t

placeStairs :: Kind.COps -> Bool -> TileMap -> CaveKind -> [Point]
            -> Rnd Point
placeStairs cops@Kind.COps{coTileSpeedup} moveUp cmap CaveKind{..} ps = do
  let dist !cmin !p _ = all (\ !pos -> chessDist p pos > cmin) ps
      ds = [ dist cminStairDist
           , dist (2 * (cminStairDist `div` 3))
           , dist (cminStairDist `div` 2)
           , dist (cminStairDist `div` 3)
           , dist (cminStairDist `div` 5) ]
  findPosTry2 200 cmap
    (condStairs cops moveUp cmap ps)
    ds
    (\_ !t -> Tile.isOftenActor coTileSpeedup t)
    ds

-- | Create a level from a cave.
buildLevel :: Kind.COps -> Cave
           -> AbsDepth -> LevelId -> LevelId -> LevelId -> AbsDepth
           -> Int -> Maybe Bool
           -> Rnd Level
buildLevel cops@Kind.COps{ cotile=Kind.Ops{opick, okind}
                         , cocave=Kind.Ops{okind=cokind} }
           Cave{..} ldepth ln minD maxD totalDepth nstairUp escapeFeature = do
  let kc@CaveKind{..} = cokind dkind
      fitArea pos = inside pos . fromArea . qarea
      findLegend pos = maybe clegendLitTile qlegend
                       $ find (fitArea pos) dplaces
      hasEscape p = Tile.kindHasFeature (TK.Cause $ IK.Escape p)
      ascendable  = Tile.kindHasFeature $ TK.Cause (IK.Ascend 1)
      descendable = Tile.kindHasFeature $ TK.Cause (IK.Ascend (-1))
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
  cmap <- convertTileMaps cops areAllWalkable
                          pickDefTile mpickWalkable cxsize cysize dmap
  -- We keep two-way stairs separately, in the last component.
  let makeStairs :: Bool -> Bool -> Bool
                 -> ( [(Point, Kind.Id TileKind)]
                    , [(Point, Kind.Id TileKind)]
                    , [(Point, Kind.Id TileKind)] )
                 -> Rnd ( [(Point, Kind.Id TileKind)]
                        , [(Point, Kind.Id TileKind)]
                        , [(Point, Kind.Id TileKind)] )
      makeStairs moveUp noAsc noDesc (!up, !down, !upDown) =
        if (if moveUp then noAsc else noDesc) then
          return (up, down, upDown)
        else do
          let cond tk = (if moveUp then ascendable tk else descendable tk)
                        && (not noAsc || not (ascendable tk))
                        && (not noDesc || not (descendable tk))
              stairsCur = up ++ down ++ upDown
              posCur = nub $ sort $ map fst stairsCur
          spos <- placeStairs cops moveUp cmap kc posCur
          let legend = findLegend spos
          stairId <- fromMaybe (assert `failure` legend) <$> opick legend cond
          let st = (spos, stairId)
              asc = ascendable $ okind stairId
              desc = descendable $ okind stairId
          return $! case (asc, desc) of
                     (True, False) -> (st : up, down, upDown)
                     (False, True) -> (up, st : down, upDown)
                     (True, True)  -> (up, down, st : upDown)
                     (False, False) -> assert `failure` st
  (stairsUp1, stairsDown1, stairsUpDown1) <-
    makeStairs False (ln == maxD) (ln == minD) ([], [], [])
  let !_A = assert (null stairsUp1) ()
  let nstairUpLeft = nstairUp - length stairsUpDown1
  (stairsUp2, stairsDown2, stairsUpDown2) <-
    foldlM' (\sts _ -> makeStairs True (ln == maxD) (ln == minD) sts)
            (stairsUp1, stairsDown1, stairsUpDown1)
            [1 .. nstairUpLeft]
  -- If only a single tile of up-and-down stairs, add one more stairs down.
  (stairsUp, stairsDown, stairsUpDown) <-
    if null (stairsUp2 ++ stairsDown2)
    then makeStairs False True (ln == minD)
           (stairsUp2, stairsDown2, stairsUpDown2)
    else return (stairsUp2, stairsDown2, stairsUpDown2)
  let stairsUpAndUpDown = stairsUp ++ stairsUpDown
  let !_A = assert (length stairsUpAndUpDown == nstairUp) ()
  let stairsTotal = stairsUpAndUpDown ++ stairsDown
      posTotal = nub $ sort $ map fst stairsTotal
  escape <- case escapeFeature of
              Nothing -> return []
              Just True -> do
                epos <- placeStairs cops True cmap kc posTotal
                let legend = findLegend epos
                upEscape <- fmap (fromMaybe $ assert `failure` legend)
                            $ opick legend $ hasEscape 1
                return [(epos, upEscape)]
              Just False -> do
                epos <- placeStairs cops False cmap kc posTotal
                let legend = findLegend epos
                downEscape <- fmap (fromMaybe $ assert `failure` legend)
                              $ opick legend $ hasEscape (-1)
                return [(epos, downEscape)]
  let addVicinity (p, k) = let vic = vicinityUnsafe p
                               old = cmap PointArray.! p
                           in (p, k) : map (\p0 -> (p0, old)) vic
      exits = concatMap addVicinity $ stairsTotal ++ escape
      ltile = cmap PointArray.// exits
      -- We reverse the order in down stairs, to minimize long stair chains.
      lstair = ( map fst $ stairsUp ++ stairsUpDown
               , map fst $ stairsUpDown ++ stairsDown )
  -- traceShow (ln, nstairUp, (stairsUp, stairsDown, stairsUpDown)) skip
  litemNum <- castDice ldepth totalDepth citemNum
  lsecret <- randomR (1, maxBound)  -- 0 means unknown
  return $! levelFromCaveKind cops kc ldepth ltile lstair
                              cactorCoeff cactorFreq litemNum citemFreq
                              lsecret (map fst escape)

-- | Build rudimentary level from a cave kind.
levelFromCaveKind :: Kind.COps
                  -> CaveKind -> AbsDepth -> TileMap -> ([Point], [Point])
                  -> Int -> Freqs ItemKind -> Int -> Freqs ItemKind -> Int -> [Point]
                  -> Level
levelFromCaveKind Kind.COps{coTileSpeedup}
                  CaveKind{..}
                  ldepth ltile lstair lactorCoeff lactorFreq litemNum litemFreq
                  lsecret lescape =
  let lvl = Level
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
        , lclear = 0  -- calculated below
        , ltime = timeZero
        , lactorCoeff
        , lactorFreq
        , litemNum
        , litemFreq
        , lsecret
        , lhidden = chidden
        , lescape
        }
      f n t | Tile.isExplorable coTileSpeedup t = n + 1
            | otherwise = n
      lclear = PointArray.foldlA' f 0 ltile
  in lvl {lclear}

findGenerator :: Kind.COps -> LevelId -> LevelId -> LevelId -> AbsDepth -> Int
              -> (GroupName CaveKind, Maybe Bool)
              -> Rnd Level
findGenerator cops ln minD maxD totalDepth nstairUp
              (genName, escapeFeature) = do
  let Kind.COps{cocave=Kind.Ops{opick}} = cops
  ci <- fromMaybe (assert `failure` genName) <$> opick genName (const True)
  -- A simple rule for now: level at level @ln@ has depth (difficulty) @abs ln@.
  let ldepth = AbsDepth $ abs $ fromEnum ln
  cave <- buildCave cops ldepth totalDepth ci
  buildLevel cops cave ldepth ln minD maxD totalDepth nstairUp escapeFeature

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
      (minId, maxId) = (toEnum minD, toEnum maxD)
      freshTotalDepth = assert (signum minD == signum maxD)
                        $ AbsDepth
                        $ max 10 $ max (abs minD) (abs maxD)
  let gen :: (Int, [(LevelId, Level)]) -> (Int, (GroupName CaveKind, Maybe Bool))
          -> Rnd (Int, [(LevelId, Level)])
      gen (!nstairUp, l) (!n, !caveTB) = do
        let ln = toEnum n
        lvl <- findGenerator cops ln minId maxId freshTotalDepth nstairUp caveTB
        -- nstairUp for the next level is nstairDown for the current level
        let nstairDown = length $ snd $ lstair lvl
        return (nstairDown, (ln, lvl) : l)
  (nstairUpLast, levels) <- foldlM' gen (0, []) $ reverse $ IM.assocs caves
  let !_A = assert (nstairUpLast == 0) ()
  let freshDungeon = EM.fromList levels
  return $! FreshDungeon{..}
