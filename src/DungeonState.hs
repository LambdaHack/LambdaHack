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

listArrayCfg :: CaveKind -> TileMapXY -> TileMap
listArrayCfg CaveKind{cxsize, cysize} lmap =
  Kind.listArray (zeroLoc, toLoc cxsize (cxsize - 1, cysize - 1))
    (M.elems $ M.mapKeys (\ (x, y) -> (y, x)) lmap)

unknownTileMap :: CaveKind -> TileMap
unknownTileMap CaveKind{cxsize, cysize} =
  Kind.listArray (zeroLoc, toLoc cxsize (cxsize - 1, cysize - 1))
    (repeat Tile.unknownId)

rollItems :: CaveKind -> TileMap -> Loc
             -> Rnd [(Loc, ([Item], [Item]))]
rollItems cfg@CaveKind{cxsize} lmap ploc =
  do
    nri <- nrItems cfg
    replicateM nri $
      do
        item <- newItem (depth cfg)
        l <- case iname (Kind.getKind (jkind item)) of
               "sword" ->
                 -- swords generated close to monsters; MUAHAHAHA
                 findLocTry 2000 lmap
                   (const Tile.isBoring)
                   (\ l _ -> distance cxsize ploc l > 30)
               _ -> findLoc lmap
                      (const Tile.isBoring)
        return (l,([item], []))

-- | Create a level consisting of only one room. Optionally, insert some walls.
buildLevel :: (CaveKind -> Rnd (TileMapXY, SecretMap, String))
              -> CaveKind -> Bool
              -> Rnd Level
buildLevel buildCave cfg@CaveKind{cxsize, cysize} isLast =
  do
    (caveXY, secretMap, meta) <- buildCave cfg
    let cave = listArrayCfg cfg caveXY
    -- Roll locations of the stairs.
    su <- findLoc cave (const Tile.isBoring)
    sd <- findLocTry 2000 cave
            (\ l t -> l /= su && Tile.isBoring t)
            (\ l _ -> distance cxsize su l >= minStairsDistance cfg)
    let stairs =
          [(su, Tile.stairsUpId)]
          ++ if isLast then [] else [(sd, Tile.stairsDownId)]
        level = cave Kind.// stairs
    is <- rollItems cfg level su
    return $ Level emptyParty cxsize cysize emptyParty
                   IM.empty secretMap (IM.fromList is) level (unknownTileMap cfg)
                   meta (su, sd)

matchGenerator :: Int -> Maybe String -> CaveKind -> Bool
                  -> Rnd Level
matchGenerator _ Nothing = buildLevel caveRogue  -- the default
matchGenerator _ (Just "bigRoom")   = buildLevel caveEmpty
matchGenerator _ (Just "noiseRoom") = buildLevel caveNoise
matchGenerator _ (Just "rogueRoom") = buildLevel caveRogue
matchGenerator n (Just s) =
  error $ "Unknown dungeon generator " ++ s ++ " for level " ++ show n ++ "."

findGenerator :: Config.CP -> Int -> Int -> Rnd Level
findGenerator config n depth =
  let ln = "LambdaCave_" ++ show n
      genName = Config.getOption config "dungeon" ln
  in matchGenerator
       n genName (defaultCaveKind n) (n == depth)

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
