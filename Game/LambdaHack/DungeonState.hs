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
import Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Item
import Game.LambdaHack.Geometry
import Game.LambdaHack.Frequency

listArrayCfg :: Int -> Int -> TileMapXY -> TileMap
listArrayCfg cxsize cysize lmap =
  Kind.listArray (zeroLoc, toLoc cxsize (cxsize - 1, cysize - 1))
    (M.elems $ M.mapKeys (\ (x, y) -> (y, x)) lmap)

unknownTileMap :: Int -> Int ->  TileMap
unknownTileMap cxsize cysize =
  Kind.listArray (zeroLoc, toLoc cxsize (cxsize - 1, cysize - 1))
    (repeat Tile.unknownId)

mapToIMap :: X -> M.Map (X, Y) a -> IM.IntMap a
mapToIMap cxsize m =
  IM.fromList $ map (\ (xy, a) -> (toLoc cxsize xy, a)) (M.assocs m)

rollItems :: Kind.COps -> Int -> CaveKind -> TileMap -> Loc -> Rnd [(Loc, Item)]
rollItems cops@Kind.COps{coitem=Kind.Ops{ofindKind}} n CaveKind{cxsize, citemNum} lmap ploc =
  do
    nri <- rollDice citemNum
    replicateM nri $
      do
        item <- newItem cops n
        l <- case iname (ofindKind (jkind item)) of
               "sword" ->
                 -- swords generated close to monsters; MUAHAHAHA
                 findLocTry 2000 lmap
                   (const Tile.isBoring)
                   (\ l _ -> distance cxsize ploc l > 30)
               _ -> findLoc lmap
                      (const Tile.isBoring)
        return (l, item)

-- | Create a level from a cave, from a cave kind.
buildLevel :: Kind.COps -> Cave -> Int -> Int -> Rnd Level
buildLevel cops Cave{dkind, dsecret, ditem, dmap, dmeta} n depth = do
  let cfg@CaveKind{cxsize, cysize, minStairsDistance} = Kind.getKind dkind
      cmap = listArrayCfg  cxsize cysize dmap
  -- Roll locations of the stairs.
  su <- findLoc cmap (const Tile.isBoring)
  sd <- findLocTry 2000 cmap
          (\ l t -> l /= su && Tile.isBoring t)
          (\ l _ -> distance cxsize su l >= minStairsDistance)
  let stairs =
        [(su, Tile.stairsUpId)]
        ++ if n == depth then [] else [(sd, Tile.stairsDownId)]
      lmap = cmap Kind.// stairs
  is <- rollItems cops n cfg lmap su
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
        , lrmap = unknownTileMap cxsize cysize
        , lmeta = dmeta
        , lstairs = (su, sd)
        }
  return level

matchGenerator :: Kind.COps -> Maybe String -> Rnd (Kind.Id CaveKind)
matchGenerator Kind.COps{cocave=Kind.Ops{ofrequency}} Nothing = do
  (ci, _) <- frequency ofrequency
  return ci
matchGenerator Kind.COps{cocave=Kind.Ops{ofrequency, oname}} (Just name) =
  let freq@(Frequency l) =
        filterFreq ((== name) . oname . snd) ofrequency
  in case l of
    [] -> error $ "Unknown dungeon generator " ++ name
    _ | sum (map fst l) == 0 ->  -- HACK for dangerous levels
          return $ fst (snd (head l))
      | otherwise -> do
          (ci, _) <- frequency freq
          return ci

findGenerator :: Kind.COps -> Config.CP -> Int -> Int -> Rnd Level
findGenerator cops config n depth = do
  let ln = "LambdaCave_" ++ show n
      genName = Config.getOption config "dungeon" ln
  ci <- matchGenerator cops genName
  cave <- buildCave n ci
  buildLevel cops cave n depth

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
        in ((ploc, LambdaCave 1, Dungeon.fromList levels), gd)
  in MState.state con

whereTo :: State -> Loc -> Maybe WorldLoc
whereTo state@State{slid, sdungeon} loc =
  let lvl = slevel state
      tile = lvl `at` loc
      k | Tile.hasFeature F.Climbable tile = -1
        | Tile.hasFeature F.Descendable tile = 1
        | otherwise = assert `failure` tile
      n = levelNumber slid
      nln = n + k
      ln = LambdaCave nln
      lvlTrg = sdungeon Dungeon.! ln
  in if (nln < 1)
     then Nothing
     else Just (ln, (if k == 1 then fst else snd) (lstairs lvlTrg))
