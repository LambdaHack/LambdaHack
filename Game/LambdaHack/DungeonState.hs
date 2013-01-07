{-# LANGUAGE OverloadedStrings #-}
-- | Dungeon operations that require 'State', 'Kind.COps'
-- or 'Config.Config' type.
module Game.LambdaHack.DungeonState
  ( -- * Dungeon generation
    FreshDungeon(..), generate
    -- * Dungeon travel
  , whereTo
  ) where

import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.IntMap as IM
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified System.Random as R

import Game.LambdaHack.Cave hiding (TileMapXY)
import Game.LambdaHack.Config
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Effect as Effect
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Place
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.Random
import Game.LambdaHack.State
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

mapToIMap :: X -> M.Map PointXY a -> IM.IntMap a
mapToIMap cxsize m =
  IM.fromList $ map (\ (xy, a) -> (toPoint cxsize xy, a)) (M.assocs m)

rollItems :: Kind.COps -> FlavourMap -> DiscoRev -> Int -> Int
          -> CaveKind -> TileMap -> Point
          -> Rnd [(Point, Item)]
rollItems Kind.COps{cotile, coitem=coitem} flavour discoRev
          ln depth CaveKind{cxsize, citemNum, cminStairDist} ltile ppos = do
  nri <- rollDice citemNum
  replicateM nri $ do
    (item, ik) <- newItem coitem flavour discoRev ln depth
    l <- case ieffect ik of
           Effect.Wound dice | maxDice dice > 0  -- a weapon
                               && maxDice dice + maxDeep (ipower ik) > 3 ->
             -- Powerful weapons generated close to monsters, MUAHAHAHA.
             findPosTry 20 ltile  -- 20 only, for unpredictability
               [ \ l _ -> chessDist cxsize ppos l > cminStairDist
               , \ l _ -> chessDist cxsize ppos l > 2 * cminStairDist `div` 3
               , \ l _ -> chessDist cxsize ppos l > cminStairDist `div` 2
               , \ l _ -> chessDist cxsize ppos l > cminStairDist `div` 3
               , const (Tile.hasFeature cotile F.Boring)
               ]
           _ -> findPos ltile (const (Tile.hasFeature cotile F.Boring))
    return (l, item)

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
buildLevel :: Kind.COps -> FlavourMap -> DiscoRev -> Cave -> Int -> Int
           -> Rnd Level
buildLevel cops@Kind.COps{ cotile=cotile@Kind.Ops{opick}
                         , cocave=Kind.Ops{okind} }
           flavour discoRev Cave{..} ln depth = do
  let kc@CaveKind{..} = okind dkind
  cmap <- convertTileMaps (opick cdefTile (const True)) cxsize cysize dmap
  (su, upId, sd, downId) <-
    placeStairs cotile cmap kc dplaces
  let stairs = (su, upId) : if ln == depth then [] else [(sd, downId)]
      ltile = cmap Kind.// stairs
      f !n !tk | Tile.isExplorable cotile tk = n + 1
               | otherwise = n
      lclear = Kind.foldlArray f 0 ltile
  is <- rollItems cops flavour discoRev ln depth kc ltile su
  -- TODO: split this into Level.defaultLevel
  let itemMap = mapToIMap cxsize ditem `IM.union` IM.fromList is
      litem = IM.map (: []) itemMap
      level = Level
        { lactor = IM.empty
        , linv = IM.empty
        , litem
        , ltile
        , lxsize = cxsize
        , lysize = cysize
        , lsmell = IM.empty
        , ldesc = cname
        , lstair = (su, sd)
        , lseen = 0
        , lclear
        , ltime = timeTurn
        , lsecret = mapToIMap cxsize dsecret
        }
  return level

matchGenerator :: Kind.Ops CaveKind -> Maybe Text -> Rnd (Kind.Id CaveKind)
matchGenerator Kind.Ops{opick} mname =
  opick (fromMaybe "dng" mname) (const True)

findGenerator :: Kind.COps -> FlavourMap -> DiscoRev -> Config -> Int -> Int
              -> Rnd Level
findGenerator cops flavour discoRev Config{configCaves} k depth = do
  let ln = "LambdaCave_" <> showT k
      genName = lookup ln configCaves
  ci <- matchGenerator (Kind.cocave cops) genName
  cave <- buildCave cops k depth ci
  buildLevel cops flavour discoRev cave k depth

-- | Freshly generated and not yet populated dungeon.
data FreshDungeon = FreshDungeon
  { entryLevel   :: LevelId  -- ^ starting level for the party
  , entryLoc     :: Point    -- ^ starting position for the party
  , freshDungeon :: Dungeon  -- ^ level maps
  , freshDepth   :: Int      -- ^ dungeon depth (can be different than size)
  }

-- | Generate the dungeon for a new game.
generate :: Kind.COps -> FlavourMap -> DiscoRev -> Config -> Rnd FreshDungeon
generate cops flavour discoRev config@Config{configDepth}  =
  let gen :: R.StdGen -> Int -> (R.StdGen, (LevelId, Level))
      gen g k =
        let (g1, g2) = R.split g
            res = St.evalState (findGenerator cops flavour discoRev
                                              config k configDepth) g1
        in (g2, (levelDefault k, res))
      con :: R.StdGen -> (FreshDungeon, R.StdGen)
      con g = assert (configDepth >= 1 `blame` configDepth) $
        let (gd, levels) = mapAccumL gen g [1..configDepth]
            entryLevel = levelDefault 1
            entryLoc = fst (lstair (snd (head levels)))
            freshDungeon = M.fromList levels
            freshDepth = configDepth
        in (FreshDungeon{..}, gd)
  in St.state con

-- | Compute the level identifier and starting position on the level,
-- after a level change.
whereTo :: State    -- ^ game state
        -> LevelId  -- ^ start from this level
        -> Int      -- ^ jump this many levels
        -> Maybe (LevelId, Point)
                    -- ^ target level and the position of its receiving stairs
whereTo State{sdungeon} lid k = assert (k /= 0) $
  let n = levelNumber lid
      nln = n - k
      ln = levelDefault nln
  in case M.lookup ln sdungeon of
    Nothing     -> Nothing
    Just lvlTrg -> Just (ln, (if k < 0 then fst else snd) (lstair lvlTrg))
