{-# LANGUAGE OverloadedStrings #-}
-- | The main dungeon generation routine.
module Game.LambdaHack.Server.DungeonGen
  ( FreshDungeon(..), dungeonGen
  ) where

import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified System.Random as R

import qualified Game.LambdaHack.Common.Feature as F
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.DungeonGen.Cave hiding (TileMapXY)
import Game.LambdaHack.Server.DungeonGen.Place
import Game.LambdaHack.Utils.Assert

convertTileMaps :: Rnd (Kind.Id TileKind) -> Int -> Int -> TileMapXY
                -> Rnd TileMap
convertTileMaps cdefTile cxsize cysize ltile = do
  let bounds = (origin, toPoint cxsize $ PointXY (cxsize - 1, cysize - 1))
      assocs = map (\ (xy, t) -> (toPoint cxsize xy, t)) (EM.assocs ltile)
  pickedTiles <- replicateM (cxsize * cysize) cdefTile
  return $ Kind.listArray bounds pickedTiles Kind.// assocs

placeStairs :: Kind.Ops TileKind -> TileMap -> CaveKind -> [Place]
            -> Rnd (Point, Kind.Id TileKind, Point, Kind.Id TileKind)
placeStairs cotile@Kind.Ops{opick} cmap CaveKind{..} dplaces = do
  su <- findPos cmap (const (Tile.hasFeature cotile F.Boring))
  sd <- findPosTry 1000 cmap
          [ \ l _ -> chessDist cxsize su l >= cminStairDist
          , \ l _ -> chessDist cxsize su l >= cminStairDist `div` 2
          , \ l t -> l /= su && Tile.hasFeature cotile F.Boring t
          ]
  let fitArea pos = inside cxsize pos . qarea
      findLegend pos =
        maybe clitLegendTile qlegend $ find (fitArea pos) dplaces
  upId   <- opick (findLegend su) $ Tile.kindHasFeature F.Ascendable
  downId <- opick (findLegend sd) $ Tile.kindHasFeature F.Descendable
  return (su, upId, sd, downId)

-- | Create a level from a cave, from a cave kind.
buildLevel :: Kind.COps -> Cave -> Int -> Int -> Rnd Level
buildLevel Kind.COps{ cotile=cotile@Kind.Ops{opick}
                    , cocave=Kind.Ops{okind} }
           Cave{..} ldepth depth = do
  let kc@CaveKind{..} = okind dkind
  cmap <- convertTileMaps (opick cdefTile (const True)) cxsize cysize dmap
  (su, upId, sd, downId) <-
    placeStairs cotile cmap kc dplaces
  litemNum <- rollDice citemNum
  secret <- random
  let stairs = (su, upId) : if ldepth == depth then [] else [(sd, downId)]
      ltile = cmap Kind.// stairs
      f !n !tk | Tile.isExplorable cotile tk = n + 1
               | otherwise = n
      lclear = Kind.foldlArray f 0 ltile
  -- TODO: split this into Level.defaultLevel
      level = Level
        { ldepth
        , lprio = EM.empty
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
        , litemNum
        , lsecret = secret
        , lhidden = chidden
        }
  return level

matchGenerator :: Kind.Ops CaveKind -> Maybe Text -> Rnd (Kind.Id CaveKind)
matchGenerator Kind.Ops{opick} mname =
  opick (fromMaybe "dng" mname) (const True)

findGenerator :: Kind.COps -> Caves -> Int -> Int -> Rnd Level
findGenerator cops caves k depth = do
  let genName = EM.lookup (toEnum k) caves
  ci <- matchGenerator (Kind.cocave cops) genName
  cave <- buildCave cops k depth ci
  buildLevel cops cave k depth

-- | Freshly generated and not yet populated dungeon.
data FreshDungeon = FreshDungeon
  { freshDungeon :: !Dungeon  -- ^ maps for all levels
  , freshDepth   :: !Int      -- ^ dungeon depth (can be different than size)
  }

-- | Generate the dungeon for a new game.
dungeonGen :: Kind.COps -> Caves -> Rnd FreshDungeon
dungeonGen cops caves =
  let depth = case EM.maxViewWithKey caves of
        Just ((d, _), _) -> fromEnum d
        Nothing ->
          assert `failure` "empty caves configuration:" <+> showT caves
      gen :: R.StdGen -> Int -> (R.StdGen, (LevelId, Level))
      gen g k =
        let (g1, g2) = R.split g
            res = St.evalState (findGenerator cops caves k depth) g1
        in (g2, (toEnum k, res))
      con :: R.StdGen -> (FreshDungeon, R.StdGen)
      con g = assert (depth >= 1 `blame` depth) $
        let (gd, levels) = mapAccumL gen g [1..depth]
            freshDungeon = EM.fromList levels
            freshDepth = depth
        in (FreshDungeon{..}, gd)
  in St.state con
