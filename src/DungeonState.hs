module DungeonState where

import qualified System.Random as R
import qualified Data.List as L
import qualified Control.Monad.State as MState
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad

import Utils.Assert
import Loc
import Level
import qualified Dungeon
import Random
import qualified Config
import WorldLoc
import State
import qualified Feature as F
import qualified Tile
import Content.CaveKind
import Cave
import Content.ItemKind
import qualified Kind
import Item
import Geometry

listArrayCfg :: CaveKind -> TileMapXY -> TileMap
listArrayCfg CaveKind{cxsize, cysize} lmap =
  Kind.listArray (zeroLoc, toLoc cxsize (cxsize - 1, cysize - 1))
    (M.elems $ M.mapKeys (\ (x, y) -> (y, x)) lmap)

unknownTileMap :: CaveKind -> TileMap
unknownTileMap CaveKind{cxsize, cysize} =
  Kind.listArray (zeroLoc, toLoc cxsize (cxsize - 1, cysize - 1))
    (repeat Tile.unknownId)

mapToIMap :: X -> M.Map (X, Y) a -> IM.IntMap a
mapToIMap cxsize m =
  IM.fromList $ map (\ (xy, a) -> (toLoc cxsize xy, a)) (M.assocs m)

rollItems :: Int -> CaveKind -> TileMap -> Loc -> Rnd [(Loc, Item)]
rollItems n cfg@CaveKind{cxsize} lmap ploc =
  do
    nri <- nrItems cfg
    replicateM nri $
      do
        item <- newItem n
        l <- case iname (Kind.getKind (jkind item)) of
               "sword" ->
                 -- swords generated close to monsters; MUAHAHAHA
                 findLocTry 2000 lmap
                   (const Tile.isBoring)
                   (\ l _ -> distance cxsize ploc l > 30)
               _ -> findLoc lmap
                      (const Tile.isBoring)
        return (l, item)

-- | Create a level from a cave, from a cave kind.
buildLevel :: Rnd Cave -> Int -> Int -> Rnd Level
buildLevel cave n depth = do
  Cave{dkind, dsecret, ditem, dmap, dmeta} <- cave
  let cfg@CaveKind{cxsize, cysize} = Kind.getKind dkind
  let cmap = listArrayCfg cfg dmap
  -- Roll locations of the stairs.
  su <- findLoc cmap (const Tile.isBoring)
  sd <- findLocTry 2000 cmap
          (\ l t -> l /= su && Tile.isBoring t)
          (\ l _ -> distance cxsize su l >= minStairsDistance cfg)
  let stairs =
        [(su, Tile.stairsUpId)]
        ++ if n == depth then [] else [(sd, Tile.stairsDownId)]
      lmap = cmap Kind.// stairs
  is <- rollItems n cfg lmap su
  let itemMap = mapToIMap cxsize ditem `IM.union` IM.fromList is
      litem = IM.map (\ i -> ([i], [])) itemMap
      level = Level
        { lheroes = emptyParty
        , lxsize = cxsize
        , lysize = cysize
        , lmonsters = emptyParty
        , lsmell = IM.empty
        , lsecret = mapToIMap cxsize dsecret
        , litem
        , lmap
        , lrmap = unknownTileMap cfg
        , lmeta = dmeta
        , lstairs = (su, sd)
        }
  return level

matchGenerator :: Maybe String -> Kind.Id CaveKind -> Int -> Int
                  -> Rnd Level
matchGenerator Nothing ci n = buildLevel (caveRogue n ci) n -- the default
matchGenerator (Just "bigRoom") ci n = buildLevel (caveEmpty n ci) n
matchGenerator (Just "noiseRoom") ci n = buildLevel (caveNoise n ci) n
matchGenerator (Just "rogueRoom") ci n = buildLevel (caveRogue n ci) n
matchGenerator (Just s) _ci n =
  error $ "Unknown dungeon generator " ++ s ++ " for level " ++ show n ++ "."

findGenerator :: Config.CP -> Int -> Int -> Rnd Level
findGenerator config n depth =
  let ln = "LambdaCave_" ++ show n
      genName = Config.getOption config "dungeon" ln
      defaultCaveKindId = Kind.getId ((<= 100) . cxsize)
  in matchGenerator genName defaultCaveKindId n depth

-- | Generate the dungeon for a new game.
generate :: Config.CP -> Rnd (Loc, LevelId, Dungeon.Dungeon)
generate config =
  let depth = Config.get config "dungeon" "depth"
      gen :: R.StdGen -> Int -> (R.StdGen, (LevelId, Level))
      gen g k =
        let (g1, g2) = R.split g
            res = MState.evalState (findGenerator config k depth) g1
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
