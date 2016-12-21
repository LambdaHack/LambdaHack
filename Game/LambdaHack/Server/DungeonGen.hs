-- | The unpopulated dungeon generation routine.
module Game.LambdaHack.Server.DungeonGen
  ( FreshDungeon(..), dungeonGen
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , convertTileMaps, placeDownStairs, buildLevel, levelFromCaveKind
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Data.Tuple
import qualified System.Random as R

import Game.LambdaHack.Common.Frequency
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.PlaceKind (PlaceKind)
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
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

-- | Create a level from a cave.
buildLevel :: Kind.COps -> Int -> GroupName CaveKind
           -> Int -> AbsDepth -> [(Point, GroupName PlaceKind)]
           -> Rnd (Level, [(Point, GroupName PlaceKind)])
buildLevel cops@Kind.COps{cocave=Kind.Ops{okind=okind, opick}}
           ln genName minD totalDepth lstairPrev = do
  dkind <- fromMaybe (assert `failure` genName) <$> opick genName (const True)
  let kc = okind dkind
      -- Simple rule for now: level @ln@ has depth (difficulty) @abs ln@.
      ldepth = AbsDepth $ abs ln
  -- Any stairs coming from above are considered extra stairs
  -- and if they don't exceed @extraStairs@,
  -- the amount is filled up with single downstairs.
  -- If they do exceed @extraStairs@, some of them end here.
  extraStairs <- castDice ldepth totalDepth $ cextraStairs kc
  let (abandonedStairs, singleStairsDown) =
        if ln == minD then (length lstairPrev, 0)
        else let double = min (length lstairPrev) $ extraStairs
                 single = max 0 $ extraStairs - double
             in (length lstairPrev - double, single)
      (lstairsSingleUp, lstairsDouble) = splitAt abandonedStairs lstairPrev
      allUpStairs = map fst $ lstairsDouble ++ lstairsSingleUp
      addSingleDown :: [(Point, GroupName PlaceKind)] -> Int
                    -> Rnd [(Point, GroupName PlaceKind)]
      addSingleDown acc 0 = return acc
      addSingleDown acc k = do
        pos <- placeDownStairs kc $ allUpStairs ++ map fst acc
        let freq = toFreq ("buildLevel ('" <> tshow ln <> ")")
                   $ map swap $ cstairFreq kc
        stairGroup <- frequency freq
        addSingleDown ((pos, stairGroup) : acc) (k - 1)
  lstairsSingleDown <- addSingleDown [] singleStairsDown
  let fixedStairsDouble = lstairsDouble
      fixedStairsUp = map (\(p, t) ->
        (p, toGroupName $ tshow t <+> "up")) lstairsSingleUp
      fixedStairsDown = map (\(p, t) ->
        (p, toGroupName $ tshow t <+> "down")) lstairsSingleDown
      allStairs = allUpStairs ++ map fst lstairsSingleDown
  fixedEscape <- case cescapeGroup kc of
                   Nothing -> return []
                   Just escapeGroup -> do
                     epos <- placeDownStairs kc allStairs
                     return [(epos, escapeGroup)]
  let lescape = map fst fixedEscape
      fixedCenters = EM.fromList $
        fixedEscape ++ fixedStairsDouble ++ fixedStairsUp ++ fixedStairsDown
      posUp Point{..} = Point (px - 1) py
      posDn Point{..} = Point (px + 1) py
      lstair = ( map (posUp . fst) $ lstairsSingleUp ++ lstairsDouble
               , map (posDn . fst) $ lstairsDouble ++ lstairsSingleDown )
  lsecret <- randomR (1, maxBound)  -- 0 means unknown
  cave <- buildCave cops ldepth totalDepth lsecret dkind fixedCenters
  cmap <- buildTileMap cops cave
  litemNum <- castDice ldepth totalDepth $ citemNum kc
  let lvl = levelFromCaveKind cops kc ldepth cmap lstair
                              litemNum lsecret lescape
  return (lvl, lstairsDouble ++ lstairsSingleDown)

placeDownStairs :: CaveKind -> [Point] -> Rnd Point
placeDownStairs kc@CaveKind{..} ps = do
  let dist cmin p = all (\pos -> chessDist p pos > cmin) ps
      distProj p = all (\pos -> (px pos == px p
                                 || px pos > px p + 5
                                 || px pos < px p - 5)
                                && (py pos == py p
                                    || py pos > py p + 3
                                    || py pos < py p - 3))
                   $ ps ++ bootFixedCenters kc
      minDist = if length ps >= 3 then 0 else cminStairDist
      f p@Point{..} =
        if p `inside` (9, 8, cxsize - 10, cysize - 9)
        then if dist minDist p && distProj p then Just p else Nothing
        else let nx = if | px < 9 -> 4
                         | px > cxsize - 10 -> cxsize - 5
                         | otherwise -> px
                 ny = if | py < 8 -> 3
                         | py > cysize - 9 -> cysize - 4
                         | otherwise -> py
                 np = Point nx ny
             in if dist 0 np && distProj np then Just np else Nothing
  findPoint cxsize cysize f

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
      buildLvl :: ([(LevelId, Level)], [(Point, GroupName PlaceKind)])
               -> (Int, GroupName CaveKind)
               -> Rnd ([(LevelId, Level)], [(Point, GroupName PlaceKind)])
      buildLvl (l, ldown) (n, genName) = do
        -- lstairUp for the next level is lstairDown for the current level
        (lvl, ldown2) <- buildLevel cops n genName minD freshTotalDepth ldown
        return ((toEnum n, lvl) : l, ldown2)
  (levels, _) <- foldlM' buildLvl ([], []) $ reverse $ IM.assocs caves
  let freshDungeon = EM.fromList levels
  return $! FreshDungeon{..}
