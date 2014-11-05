{-# LANGUAGE CPP #-}
-- | The unpopulated dungeon generation routine.
module Game.LambdaHack.Server.DungeonGen
  ( -- * Public API
    FreshDungeon(..), dungeonGen
#ifdef EXPOSE_INTERNAL
  , convertTileMaps, placeStairs, buildLevel, levelFromCaveKind, findGenerator
#endif
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Maybe

import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import Game.LambdaHack.Server.DungeonGen.Area
import Game.LambdaHack.Server.DungeonGen.Cave
import Game.LambdaHack.Server.DungeonGen.Place

convertTileMaps :: Kind.COps
                -> Rnd (Kind.Id TileKind) -> Maybe (Rnd (Kind.Id TileKind))
                -> Int -> Int -> TileMapEM
                -> Rnd TileMap
convertTileMaps Kind.COps{cotile}
                cdefTile mcdefTileWalkable cxsize cysize ltile = do
  let f :: Point -> Rnd (Kind.Id TileKind)
      f p = case EM.lookup p ltile of
        Just t -> return t
        Nothing -> cdefTile
  converted1 <- PointArray.generateMA cxsize cysize f
  case mcdefTileWalkable of
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
            && Tile.isWalkable cotile (array PointArray.! p)
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
                           && not (Tile.isWalkable cotile c)
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

placeStairs :: Kind.COps -> TileMap -> CaveKind -> [Point]
            -> Rnd Point
placeStairs Kind.COps{cotile} cmap CaveKind{..} ps = do
  let dist cmin l _ = all (\pos -> chessDist l pos > cmin) ps
  findPosTry 1000 cmap
    (\p t -> Tile.isWalkable cotile t
             && not (Tile.hasFeature cotile TK.NoActor t)
             && dist 0 p t)  -- can't overwrite stairs with other stairs
    [ dist $ cminStairDist
    , dist $ cminStairDist `div` 2
    , dist $ cminStairDist `div` 4
    , const $ Tile.hasFeature cotile TK.OftenActor
    , dist $ cminStairDist `div` 8
    ]

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
      hasEscape p t = Tile.kindHasFeature (TK.Cause $ IK.Escape p) t
      ascendable  = Tile.kindHasFeature $ TK.Cause (IK.Ascend 1)
      descendable = Tile.kindHasFeature $ TK.Cause (IK.Ascend (-1))
      nightCond kt = (not (Tile.kindHasFeature TK.Clear kt)
                      || (if dnight then id else not)
                            (Tile.kindHasFeature TK.Dark kt))
      dcond kt = (cpassable
                  || not (Tile.kindHasFeature TK.Walkable kt))
                 && nightCond kt
      pickDefTile = fmap (fromMaybe $ assert `failure` cdefTile)
                    $ opick cdefTile dcond
      wcond kt = Tile.kindHasFeature TK.Walkable kt
                 && nightCond kt
      mpickWalkable =
        if cpassable
        then Just $ fmap (fromMaybe $ assert `failure` cdefTile)
                  $ opick cdefTile wcond
        else Nothing
  cmap <- convertTileMaps cops pickDefTile mpickWalkable cxsize cysize dmap
  -- We keep two-way stairs separately, in the last component.
  let makeStairs :: Bool -> Bool -> Bool
                 -> ( [(Point, Kind.Id TileKind)]
                    , [(Point, Kind.Id TileKind)]
                    , [(Point, Kind.Id TileKind)] )
                 -> Rnd ( [(Point, Kind.Id TileKind)]
                        , [(Point, Kind.Id TileKind)]
                        , [(Point, Kind.Id TileKind)] )
      makeStairs moveUp noAsc noDesc (up, down, upDown) =
        if (if moveUp then noAsc else noDesc) then
          return (up, down, upDown)
        else do
          let cond tk = (if moveUp then ascendable tk else descendable tk)
                        && (if noAsc then not (ascendable tk) else True)
                        && (if noDesc then not (descendable tk) else True)
              stairsCur = up ++ down ++ upDown
              posCur = nub $ sort $ map fst stairsCur
          spos <- placeStairs cops cmap kc posCur
          let legend = findLegend spos
          stairId <- fmap (fromMaybe $ assert `failure` legend)
                     $ opick legend cond
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
  assert (null stairsUp1) skip
  let nstairUpLeft = nstairUp - length stairsUpDown1
  (stairsUp2, stairsDown2, stairsUpDown2) <-
    foldM (\sts _ -> makeStairs True (ln == maxD) (ln == minD) sts)
          (stairsUp1, stairsDown1, stairsUpDown1)
          [1 .. nstairUpLeft]
  -- If only a single tile of up-and-down stairs, add one more stairs down.
  (stairsUp, stairsDown, stairsUpDown) <-
    if length (stairsUp2 ++ stairsDown2) == 0
    then (makeStairs False True (ln == minD)
             (stairsUp2, stairsDown2, stairsUpDown2))
    else return (stairsUp2, stairsDown2, stairsUpDown2)
  let stairsUpAndUpDown = stairsUp ++ stairsUpDown
  assert (length stairsUpAndUpDown == nstairUp) skip
  let stairsTotal = stairsUpAndUpDown ++ stairsDown
      posTotal = nub $ sort $ map fst stairsTotal
  epos <- placeStairs cops cmap kc posTotal
  escape <- case escapeFeature of
              Nothing -> return []
              Just True -> do
                let legend = findLegend epos
                upEscape <- fmap (fromMaybe $ assert `failure` legend)
                            $ opick legend $ hasEscape 1
                return [(epos, upEscape)]
              Just False -> do
                let legend = findLegend epos
                downEscape <- fmap (fromMaybe $ assert `failure` legend)
                              $ opick legend $ hasEscape (-1)
                return [(epos, downEscape)]
  let exits = stairsTotal ++ escape
      ltile = cmap PointArray.// exits
      -- We reverse the order in down stairs, to minimize long stair chains.
      lstair = ( map fst $ stairsUp ++ stairsUpDown
               , map fst $ stairsUpDown ++ stairsDown )
  -- traceShow (ln, nstairUp, (stairsUp, stairsDown, stairsUpDown)) skip
  litemNum <- castDice ldepth totalDepth citemNum
  lsecret <- randomR (1, maxBound)  -- 0 means unknown
  return $! levelFromCaveKind cops kc ldepth ltile lstair
                              cactorCoeff cactorFreq litemNum citemFreq
                              lsecret (isJust escapeFeature)

-- | Build rudimentary level from a cave kind.
levelFromCaveKind :: Kind.COps
                  -> CaveKind -> AbsDepth -> TileMap -> ([Point], [Point])
                  -> Int -> Freqs ItemKind -> Int -> Freqs ItemKind -> Int -> Bool
                  -> Level
levelFromCaveKind Kind.COps{cotile}
                  CaveKind{..}
                  ldepth ltile lstair lactorCoeff lactorFreq litemNum litemFreq
                  lsecret lescape =
  let lvl = Level
        { ldepth
        , lprio = EM.empty
        , lfloor = EM.empty
        , lembed = EM.empty  -- is populated inside $MonadServer$
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
      f n t | Tile.isExplorable cotile t = n + 1
            | otherwise = n
      lclear = PointArray.foldlA f 0 ltile
  in lvl {lclear}

findGenerator :: Kind.COps -> LevelId -> LevelId -> LevelId -> AbsDepth -> Int
              -> (GroupName CaveKind, Maybe Bool)
              -> Rnd Level
findGenerator cops ln minD maxD totalDepth nstairUp
              (genName, escapeFeature) = do
  let Kind.COps{cocave=Kind.Ops{opick}} = cops
  ci <- fmap (fromMaybe $ assert `failure` genName)
        $ opick genName (const True)
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
      gen (nstairUp, l) (n, caveTB) = do
        let ln = toEnum n
        lvl <- findGenerator cops ln minId maxId freshTotalDepth nstairUp caveTB
        -- nstairUp for the next level is nstairDown for the current level
        let nstairDown = length $ snd $ lstair lvl
        return $ (nstairDown, (ln, lvl) : l)
  (nstairUpLast, levels) <- foldM gen (0, []) $ reverse $ IM.assocs caves
  assert (nstairUpLast == 0) skip
  let freshDungeon = EM.fromList levels
  return $! FreshDungeon{..}
