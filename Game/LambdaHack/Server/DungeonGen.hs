-- | The main dungeon generation routine.
module Game.LambdaHack.Server.DungeonGen
  ( FreshDungeon(..), dungeonGen
  ) where

import Control.Arrow (first)
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import qualified System.Random as R

import qualified Game.LambdaHack.Common.Effect as Effect
import qualified Game.LambdaHack.Common.Feature as F
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Server.DungeonGen.Cave hiding (TileMapXY)
import Game.LambdaHack.Server.DungeonGen.Place
import Game.LambdaHack.Utils.Assert

convertTileMaps :: Rnd (Kind.Id TileKind) -> Int -> Int -> TileMapXY
                -> Rnd TileMap
convertTileMaps cdefTile cxsize cysize ltile = do
  let bounds = (origin, toPoint cxsize $ PointXY (cxsize - 1, cysize - 1))
      assocs = map (first (toPoint cxsize)) (EM.assocs ltile)
  pickedTiles <- replicateM (cxsize * cysize) cdefTile
  return $ Kind.listArray bounds pickedTiles Kind.// assocs

placeStairs :: Kind.Ops TileKind -> TileMap -> CaveKind
            -> Rnd (Point, Point, Point)
placeStairs cotile cmap CaveKind{..} = do
  su <- findPos cmap (const (Tile.hasFeature cotile F.CanActor))
  sd <- findPosTry 1000 cmap
          [ \ l _ -> chessDist cxsize su l >= cminStairDist
          , \ l _ -> chessDist cxsize su l >= cminStairDist `div` 2
          , \ l t -> l /= su && Tile.hasFeature cotile F.CanActor t
          ]
  sq <- findPos cmap (const (Tile.hasFeature cotile F.CanActor))
  return (sq, su, sd)

-- | Create a level from a cave, from a cave kind.
buildLevel :: Kind.COps -> Cave -> Int -> Int -> Int -> Maybe Bool -> Rnd Level
buildLevel cops@Kind.COps{ cotile=cotile@Kind.Ops{opick}
                         , cocave=Kind.Ops{okind} }
           Cave{..} ldepth minD maxD escapeFeature = do
  let kc@CaveKind{..} = okind dkind
      fitArea pos = inside cxsize pos . qarea
      findLegend pos =
        maybe clitLegendTile qlegend $ find (fitArea pos) dplaces
      hasEscapeAndSymbol sym t =
        Tile.kindHasFeature (F.Cause Effect.Escape) t
        && tsymbol t == sym
      ascendable tk =
        any (\f -> case f of F.Cause (Effect.Ascend _) -> True; _ -> False
            ) $ tfeature tk
      descendable tk =
        any (\f -> case f of F.Cause (Effect.Descend _) -> True; _ -> False
            ) $ tfeature tk
  cmap <- convertTileMaps (opick cdefTile (const True)) cxsize cysize dmap
  (sq, su, sd) <- placeStairs cotile cmap kc
  stairsUp <- if ldepth == minD then return []
              else do
                let cond tk | ldepth == maxD = ascendable tk
                                               && not (descendable tk)
                            | otherwise = ascendable tk
                upId <- opick (findLegend su) cond
                return [(su, upId)]
  stairsDown <- if ldepth == maxD then return []
                else do
                  let cond tk | ldepth == minD = descendable tk
                                                 && not (ascendable tk)
                              | otherwise = descendable tk
                  downId <- opick (findLegend sd) cond
                  return [(sd, downId)]
  escape <- case escapeFeature of
              Nothing -> return []
              Just True -> do
                upEscape <- opick (findLegend su) $ hasEscapeAndSymbol '<'
                return [(sq, upEscape)]
              Just False -> do
                downEscape <- opick (findLegend su) $ hasEscapeAndSymbol '>'
                return [(sq, downEscape)]
  let exits = stairsUp ++ stairsDown ++ escape
      ltile = cmap Kind.// exits
      lstair = ([su], [sd])  -- TODO: add more stairs, if required
  litemNum <- castDice citemNum
  lsecret <- random
  return $! levelFromCaveKind cops kc ldepth ltile lstair litemNum lsecret

levelFromCaveKind :: Kind.COps
                  -> CaveKind -> Int -> TileMap -> ([Point], [Point])
                  -> Int -> Int
                  -> Level
levelFromCaveKind Kind.COps{cotile}
                  CaveKind{..} ldepth ltile lstair litemNum lsecret =
  Level
    { ldepth
    , lprio = EM.empty
    , lfloor = EM.empty
    , ltile
    , lxsize = cxsize
    , lysize = cysize
    , lsmell = EM.empty
    , ldesc = cname
    , lstair
    , lseen = 0
    , lclear = let f !n !tk | Tile.isExplorable cotile tk = n + 1
                            | otherwise = n
               in Kind.foldlArray f 0 ltile
    , ltime = timeTurn
    , litemNum
    , lsecret
    , lhidden = chidden
    }

findGenerator :: Kind.COps -> Caves -> LevelId -> LevelId -> LevelId
              -> Rnd Level
findGenerator cops caves ldepth minD maxD = do
  let Kind.COps{cocave=Kind.Ops{opick}} = cops
      (genName, escapeFeature) =
        fromMaybe ("dng", Nothing) $ EM.lookup ldepth caves
  ci <- opick genName (const True)
  let maxDepth = if minD == maxD then 10 else fromEnum maxD
  cave <- buildCave cops (fromEnum ldepth) maxDepth ci
  buildLevel cops cave
             (fromEnum ldepth) (fromEnum minD) (fromEnum maxD)
             escapeFeature

-- | Freshly generated and not yet populated dungeon.
data FreshDungeon = FreshDungeon
  { freshDungeon :: !Dungeon  -- ^ maps for all levels
  , freshDepth   :: !Int      -- ^ dungeon depth (can be different than size)
  }

-- | Generate the dungeon for a new game.
dungeonGen :: Kind.COps -> Caves -> Rnd FreshDungeon
dungeonGen cops caves = do
  let (minD, maxD) =
        case (EM.minViewWithKey caves, EM.maxViewWithKey caves) of
          (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
          _ -> assert `failure` "no caves" `with` caves
  assert (minD <= maxD && fromEnum minD >= 1 `blame` "wrongly labeled caves"
                                             `with` caves) skip
  let gen :: R.StdGen -> LevelId -> (R.StdGen, (LevelId, Level))
      gen g ldepth =
        let (g1, g2) = R.split g
            findG = findGenerator cops caves ldepth minD maxD
            res = St.evalState findG g1
        in (g2, (ldepth, res))
      con :: R.StdGen -> (FreshDungeon, R.StdGen)
      con g = let (gd, levels) = mapAccumL gen g [minD..maxD]
                  freshDungeon = EM.fromList levels
                  freshDepth = if minD == maxD then 10 else fromEnum maxD
              in (FreshDungeon{..}, gd)
  St.state con
