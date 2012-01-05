module Game.LambdaHack.DungeonState where

import qualified System.Random as R
import qualified Data.List as L
import qualified Control.Monad.State as MState
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Loc
import Game.LambdaHack.Level
import qualified Game.LambdaHack.Dungeon as Dungeon
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.WorldLoc
import Game.LambdaHack.State
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Cave
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Item
import Game.LambdaHack.Geometry
import Game.LambdaHack.Content.TileKind

convertTileMaps :: Rnd (Kind.Id TileKind) -> Int -> Int -> TileMapXY
                -> Rnd TileMap
convertTileMaps cdefTile cxsize cysize lmap = do
  let bounds = (zeroLoc, toLoc cxsize (cxsize - 1, cysize - 1))
      assocs = map (\ (xy, t) -> (toLoc cxsize xy, t)) (M.assocs lmap)
  pickedTiles <- replicateM (cxsize * cysize) cdefTile
  return $ Kind.listArray bounds pickedTiles Kind.// assocs

unknownTileMap :: Kind.Id TileKind -> Int -> Int -> TileMap
unknownTileMap unknownId cxsize cysize =
  Kind.listArray (zeroLoc, toLoc cxsize (cxsize - 1, cysize - 1))
    (repeat unknownId)

mapToIMap :: X -> M.Map (X, Y) a -> IM.IntMap a
mapToIMap cxsize m =
  IM.fromList $ map (\ (xy, a) -> (toLoc cxsize xy, a)) (M.assocs m)

rollItems :: Kind.COps -> LevelId -> Int -> CaveKind -> TileMap -> Loc
          -> Rnd [(Loc, Item)]
rollItems Kind.COps{cotile, coitem=coitem@Kind.Ops{osymbol}}
          lvl depth CaveKind{cxsize, citemNum} lmap ploc = do
  nri <- rollDice citemNum
  replicateM nri $ do
    item <- newItem coitem lvl depth
    l <- case osymbol (jkind item) of
           ')' ->
             -- melee weapons generated close to monsters; MUAHAHAHA
             findLocTry 2000 lmap
               (const (Tile.hasFeature cotile F.Boring))
               (\ l _ -> distance cxsize ploc l > 30)
           _ -> findLoc lmap (const (Tile.hasFeature cotile F.Boring))
    return (l, item)

-- | Create a level from a cave, from a cave kind.
buildLevel :: Kind.COps -> Cave -> LevelId -> Int -> Rnd Level
buildLevel cops@Kind.COps{cotile=cotile@Kind.Ops{opick}, cocave=Kind.Ops{okind}}
           Cave{dkind, dsecret, ditem, dmap, dmeta} lvl depth = do
  let cfg@CaveKind{..} = okind dkind
  cmap <- convertTileMaps (opick cdefTile) cxsize cysize dmap
  -- Roll locations of the stairs.
  su <- findLoc cmap (const (Tile.hasFeature cotile F.Boring))
  sd <- findLocTry 2000 cmap
          (\ l t -> l /= su && Tile.hasFeature cotile F.Boring t)
          (\ l _ -> distance cxsize su l >= cminStairDist)
  upId   <- Tile.stairsUpId   cotile
  downId <- Tile.stairsDownId cotile
  let stairs = [(su, upId)] ++ if levelNumber lvl == depth
                               then []
                               else [(sd, downId)]
      lmap = cmap Kind.// stairs
  is <- rollItems cops lvl depth cfg lmap su
  let itemMap = mapToIMap cxsize ditem `IM.union` IM.fromList is
      litem = IM.map (\ i -> ([i], [])) itemMap
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
        , lrmap = unknownTileMap (Tile.unknownId cotile) cxsize cysize
        , ldesc = cdesc
        , lmeta = dmeta
        , lstairs = (su, sd)
        }
  return level

matchGenerator :: Kind.Ops CaveKind -> Maybe String -> Rnd (Kind.Id CaveKind)
matchGenerator Kind.Ops{opick} Nothing = opick (const True)
matchGenerator Kind.Ops{ofoldrWithKey, ofreq, opick} (Just name) =
  let p = (== name) . cname
      l = ofoldrWithKey (\ i k is -> if p k then i : is else is) []
  in case l of
    [] -> error $ "Unknown dungeon generator " ++ name
    i : _
      | sum (map ofreq l) == 0 ->
          -- The user insists on a dangerous level, so just pick the first.
          return i
      | otherwise -> opick p

findGenerator :: Kind.COps -> Config.CP -> Int -> Int -> Rnd Level
findGenerator cops config k depth = do
  let ln = "LambdaCave_" ++ show k
      genName = Config.getOption config "dungeon" ln
      lvl = LambdaCave k
  ci <- matchGenerator (Kind.cocave cops) genName
  cave <- buildCave cops lvl depth ci
  buildLevel cops cave lvl depth

-- | Generate the dungeon for a new game.
generate :: Kind.COps -> Config.CP -> Rnd (Loc, LevelId, Dungeon.Dungeon)
generate cops config =
  let depth = Config.get config "dungeon" "depth"
      gen :: R.StdGen -> Int -> (R.StdGen, (LevelId, Level))
      gen g k =
        let (g1, g2) = R.split g
            res = MState.evalState (findGenerator cops config k depth) g1
        in (g2, (LambdaCave k, res))
      con :: R.StdGen -> ((Loc, LevelId, Dungeon.Dungeon), R.StdGen)
      con g =
        let (gd, levels) = L.mapAccumL gen g [1..depth]
            ploc = fst (lstairs (snd (head levels)))
        in ((ploc, LambdaCave 1, Dungeon.fromList levels depth), gd)
  in MState.state con

-- | Computes the target world location of using stairs.
whereTo :: State -> Int -> Maybe WorldLoc
whereTo State{slid, sdungeon} k = assert (k /= 0) $
  let n = levelNumber slid
      nln = n - k
      ln = LambdaCave nln
  in case Dungeon.lookup ln sdungeon of
     Nothing     -> Nothing
     Just lvlTrg -> Just (ln, (if k < 0 then fst else snd) (lstairs lvlTrg))
