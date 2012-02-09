-- | Dungeon operations that require 'State', 'Kind.COps'
-- or 'Config.Config' type.
module Game.LambdaHack.DungeonState
  ( -- * Dungeon generation
    FreshDungeon(..), generate
    -- * Dungeon travel
  , whereTo
  ) where

import qualified System.Random as R
import qualified Data.List as L
import qualified Control.Monad.State as MState
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Point
import Game.LambdaHack.Level
import qualified Game.LambdaHack.Dungeon as Dungeon
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.State
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Cave hiding (TileMapXY)
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Item
import Game.LambdaHack.PointXY
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Place
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Content.ItemKind

convertTileMaps :: Rnd (Kind.Id TileKind) -> Int -> Int -> TileMapXY
                -> Rnd TileMap
convertTileMaps cdefTile cxsize cysize lmap = do
  let bounds = (origin, toPoint cxsize $ PointXY (cxsize - 1, cysize - 1))
      assocs = map (\ (xy, t) -> (toPoint cxsize xy, t)) (M.assocs lmap)
  pickedTiles <- replicateM (cxsize * cysize) cdefTile
  return $ Kind.listArray bounds pickedTiles Kind.// assocs

unknownTileMap :: Kind.Id TileKind -> Int -> Int -> TileMap
unknownTileMap unknownId cxsize cysize =
  let bounds = (origin, toPoint cxsize $ PointXY (cxsize - 1, cysize - 1))
  in Kind.listArray bounds (repeat unknownId)

mapToIMap :: X -> M.Map PointXY a -> IM.IntMap a
mapToIMap cxsize m =
  IM.fromList $ map (\ (xy, a) -> (toPoint cxsize xy, a)) (M.assocs m)

rollItems :: Kind.COps -> Int -> Int -> CaveKind -> TileMap -> Point
          -> Rnd [(Point, Item)]
rollItems Kind.COps{cotile, coitem=coitem@Kind.Ops{okind}}
          lvl depth CaveKind{cxsize, citemNum, cminStairDist} lmap ploc = do
  nri <- rollDice citemNum
  replicateM nri $ do
    item <- newItem coitem lvl depth
    let ik = okind (jkind item)
    l <- case ieffect ik of
           Effect.Wound dice | maxDice dice > 0  -- a weapon
                               && maxDice dice + maxDeep (ipower ik) > 3 ->
             -- Powerful weapons generated close to monsters, MUAHAHAHA.
             findLocTry 20 lmap  -- 20 only, for unpredictability
               [ \ l _ -> chessDist cxsize ploc l > cminStairDist
               , \ l _ -> chessDist cxsize ploc l > 2 * cminStairDist `div` 3
               , \ l _ -> chessDist cxsize ploc l > cminStairDist `div` 2
               , \ l _ -> chessDist cxsize ploc l > cminStairDist `div` 3
               , const (Tile.hasFeature cotile F.Boring)
               ]
           _ -> findLoc lmap (const (Tile.hasFeature cotile F.Boring))
    return (l, item)

placeStairs :: Kind.Ops TileKind -> TileMap -> X -> Int -> [Place]
            -> Rnd (Point, Kind.Id TileKind, Point, Kind.Id TileKind)
placeStairs cotile@Kind.Ops{opick} cmap cxsize cminStairDist dplaces = do
  su <- findLoc cmap (const (Tile.hasFeature cotile F.Boring))
  sd <- findLocTry 1000 cmap
          [ \ l _ -> chessDist cxsize su l >= cminStairDist
          , \ l _ -> chessDist cxsize su l >= cminStairDist `div` 2
          , \ l t -> l /= su && Tile.hasFeature cotile F.Boring t
          ]
  let fitArea loc = inside cxsize loc . qarea
      findLegend loc = maybe "litLegend" qlegend $ L.find (fitArea loc) dplaces
  upId   <- opick (findLegend su) $ Tile.kindHasFeature F.Ascendable
  downId <- opick (findLegend sd) $ Tile.kindHasFeature F.Descendable
  return (su, upId, sd, downId)

-- | Create a level from a cave, from a cave kind.
buildLevel :: Kind.COps -> Cave -> Int -> Int -> Rnd Level
buildLevel cops@Kind.COps{ cotile=cotile@Kind.Ops{opick, ouniqGroup}
                         , cocave=Kind.Ops{okind} }
           Cave{..} lvl depth = do
  let cfg@CaveKind{..} = okind dkind
  cmap <- convertTileMaps (opick cdefTile (const True)) cxsize cysize dmap
  (su, upId, sd, downId) <-
    placeStairs cotile cmap cxsize cminStairDist dplaces
  let stairs = (su, upId) : if lvl == depth then [] else [(sd, downId)]
      lmap = cmap Kind.// stairs
  is <- rollItems cops lvl depth cfg lmap su
  -- TODO: split this into Level.defaultLevel
  let itemMap = mapToIMap cxsize ditem `IM.union` IM.fromList is
      litem = IM.map (\ i -> ([i], [])) itemMap
      unknownId = ouniqGroup "unknown space"
      level = Level
        { lheroes = IM.empty
        , lheroItem = IM.empty
        , lxsize = cxsize
        , lysize = cysize
        , lmonsters = IM.empty
        , lmonItem = IM.empty
        , lsmell = IM.empty
        , lsecret = mapToIMap cxsize dsecret
        , litem
        , lmap
        , lrmap = unknownTileMap unknownId cxsize cysize
        , ldesc = cname
        , lmeta = dmeta
        , lstairs = (su, sd)
        }
  return level

matchGenerator :: Kind.Ops CaveKind -> Maybe String -> Rnd (Kind.Id CaveKind)
matchGenerator Kind.Ops{opick} mname =
  opick (fromMaybe "dng" mname) (const True)

findGenerator :: Kind.COps -> Config.CP -> Int -> Int -> Rnd Level
findGenerator cops config k depth = do
  let ln = "LambdaCave_" ++ show k
      genName = Config.getOption config "dungeon" ln
  ci <- matchGenerator (Kind.cocave cops) genName
  cave <- buildCave cops k depth ci
  buildLevel cops cave k depth

-- | Freshly generated and not yet populated dungeon.
data FreshDungeon = FreshDungeon
  { entryLevel   :: Dungeon.LevelId  -- ^ starting level for the party
  , entryLoc     :: Point            -- ^ starting location for the party
  , freshDungeon :: Dungeon.Dungeon  -- ^ level maps
  }

-- | Generate the dungeon for a new game.
generate :: Kind.COps -> Config.CP -> Rnd FreshDungeon
generate cops config =
  let depth = Config.get config "dungeon" "depth"
      gen :: R.StdGen -> Int -> (R.StdGen, (Dungeon.LevelId, Level))
      gen g k =
        let (g1, g2) = R.split g
            res = MState.evalState (findGenerator cops config k depth) g1
        in (g2, (Dungeon.levelDefault k, res))
      con :: R.StdGen -> (FreshDungeon, R.StdGen)
      con g =
        let (gd, levels) = L.mapAccumL gen g [1..depth]
            entryLevel = Dungeon.levelDefault 1
            entryLoc = fst (lstairs (snd (head levels)))
            freshDungeon = Dungeon.fromList levels depth
        in (FreshDungeon{..}, gd)
  in MState.state con

-- | Compute the level identifier and starting location on the level,
-- after a level change.
whereTo :: State  -- ^ game state
        -> Int    -- ^ jump this many levels
        -> Maybe (Dungeon.LevelId, Point)
             -- ^ target level and the location of its receiving stairs
whereTo State{slid, sdungeon} k = assert (k /= 0) $
  let n = Dungeon.levelNumber slid
      nln = n - k
      ln = Dungeon.levelDefault nln
  in case Dungeon.lookup ln sdungeon of
    Nothing     -> Nothing
    Just lvlTrg -> Just (ln, (if k < 0 then fst else snd) (lstairs lvlTrg))
