{-# LANGUAGE OverloadedStrings #-}
-- | The main dungeon generation routine.
module Game.LambdaHack.Server.DungeonGen
  ( FreshDungeon(..), dungeonGen
  ) where

import Control.Arrow (second)
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified System.Random as R

import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.DungeonGen.Cave hiding (TileMapXY)
import Game.LambdaHack.Server.DungeonGen.Place
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

convertTileMaps :: Rnd (Kind.Id TileKind) -> Int -> Int -> TileMapXY
                -> Rnd TileMap
convertTileMaps cdefTile cxsize cysize ltile = do
  let bounds = (origin, toPoint cxsize $ PointXY (cxsize - 1, cysize - 1))
      assocs = map (\ (xy, t) -> (toPoint cxsize xy, t)) (M.assocs ltile)
  pickedTiles <- replicateM (cxsize * cysize) cdefTile
  return $ Kind.listArray bounds pickedTiles Kind.// assocs

mapToIMap :: X -> M.Map PointXY a -> EM.EnumMap Point a
mapToIMap cxsize m =
  EM.fromList $ map (\ (xy, a) -> (toPoint cxsize xy, a)) (M.assocs m)

placeStairs :: Kind.Ops TileKind -> TileMap -> CaveKind -> [Place]
            -> Rnd (Point, Kind.Id TileKind, Point, Kind.Id TileKind)
placeStairs cotile@Kind.Ops{opick} cmap CaveKind{..} dplaces = do
  su <- findPos cmap (const (Tile.hasFeature cotile F.Boring))
  sd <- findPosTry 1000 cmap
          [ \ l _ -> chessDist cxsize su l >= cminStairDist
          , \ l _ -> chessDist cxsize su l >= cminStairDist `div` 2
          , \ l t -> l /= su && Tile.hasFeature cotile F.Boring t
          ]
  let fitArea loc = inside cxsize loc . qarea
      findLegend loc =
        maybe clitLegendTile qlegend $ find (fitArea loc) dplaces
  upId   <- opick (findLegend su) $ Tile.kindHasFeature F.Ascendable
  downId <- opick (findLegend sd) $ Tile.kindHasFeature F.Descendable
  return (su, upId, sd, downId)

-- | Create a level from a cave, from a cave kind.
buildLevel :: Kind.COps -> Cave -> Int -> Int
           -> Rnd (Level, RollDice)
buildLevel Kind.COps{ cotile=cotile@Kind.Ops{opick}
                    , cocave=Kind.Ops{okind} }
           Cave{..} ldepth depth = do
  let kc@CaveKind{..} = okind dkind
  cmap <- convertTileMaps (opick cdefTile (const True)) cxsize cysize dmap
  (su, upId, sd, downId) <-
    placeStairs cotile cmap kc dplaces
  let stairs = (su, upId) : if ldepth == depth then [] else [(sd, downId)]
      ltile = cmap Kind.// stairs
      f !n !tk | Tile.isExplorable cotile tk = n + 1
               | otherwise = n
      lclear = Kind.foldlArray f 0 ltile
  -- TODO: split this into Level.defaultLevel
      level = Level
        { ldepth
        , lactor = EM.empty
        , litem = EM.empty
        , lfloor = EM.empty
        , ltile
        , lxsize = cxsize
        , lysize = cysize
        , lsmell = EM.empty
        , ldesc = cname
        , lstair = (su, sd)
        , lseen = 0
        , lclear
        , ltime = timeTurn
        , lsecret = mapToIMap cxsize dsecret
        }
  return (level, citemNum)

matchGenerator :: Kind.Ops CaveKind -> Maybe Text -> Rnd (Kind.Id CaveKind)
matchGenerator Kind.Ops{opick} mname =
  opick (fromMaybe "dng" mname) (const True)

findGenerator :: Kind.COps -> Config -> Int -> Int -> Rnd (Level, RollDice)
findGenerator cops Config{configCaves} k depth = do
  let ln = "LambdaCave_" <> showT k
      genName = lookup ln configCaves
  ci <- matchGenerator (Kind.cocave cops) genName
  cave <- buildCave cops k depth ci
  buildLevel cops cave k depth

-- | Find starting postions for all factions. Try to make them distant
-- from each other and from any stairs.
findEntryPoss :: Kind.COps -> Level -> Rnd [Point]
findEntryPoss Kind.COps{cotile} Level{ltile, lxsize, lstair} =
  let cminStairDist = chessDist lxsize (fst lstair) (snd lstair)
      dist l poss cmin =
        all (\pos -> chessDist lxsize l pos > cmin) poss
      tryFind poss = do
        pos <- findPosTry 20 ltile  -- 20 only, for unpredictability
                 [ \ l _ -> dist l poss $ 2 * cminStairDist
                 , \ l _ -> dist l poss cminStairDist
                 , \ l _ -> dist l poss $ cminStairDist `div` 2
                 , \ l _ -> dist l poss $ cminStairDist `div` 4
                 , const (Tile.hasFeature cotile F.Walkable)
                 ]
        fmap (pos :) $ tryFind (pos : poss)
      stairPoss = [fst lstair, snd lstair]
  in tryFind stairPoss

-- | Freshly generated and not yet populated dungeon.
data FreshDungeon = FreshDungeon
  { entryLevel   :: LevelId  -- ^ starting level
  , entryPoss    :: [Point]  -- ^ starting positions for non-spawning parties
  , freshDungeon :: Dungeon  -- ^ maps for all levels
  , freshDepth   :: Int      -- ^ dungeon depth (can be different than size)
  , itemCounts   :: [(LevelId, (Level, RollDice))]
  }

-- | Generate the dungeon for a new game.
dungeonGen :: Kind.COps -> Config -> Rnd FreshDungeon
dungeonGen cops config@Config{configDepth} =
  let gen :: R.StdGen -> Int -> (R.StdGen, (LevelId, (Level, RollDice)))
      gen g k =
        let (g1, g2) = R.split g
            res = St.evalState (findGenerator cops config k configDepth) g1
        in (g2, (toEnum k, res))
      con :: R.StdGen -> (FreshDungeon, R.StdGen)
      con g = assert (configDepth >= 1 `blame` configDepth) $
        let (gd, itemCounts) = mapAccumL gen g [1..configDepth]
            levels = map (second fst) itemCounts
            entryLevel = initialLevel
            (entryPoss, gp) =
              St.runState (findEntryPoss cops (snd (head levels))) gd
            freshDungeon = EM.fromList levels
            freshDepth = configDepth
       in (FreshDungeon{..}, gp)
  in St.state con
