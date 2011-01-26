module Main where

import System.Directory
import Control.Monad
import Data.Map as M
import Data.Maybe

import Action
import State
import Geometry
import Level
import Dungeon
import Perception
import Display2
import Random
import qualified Save
import Turn
import Item
import qualified Config

main :: IO ()
main = startup start

-- | Either restore a saved game, or setup a new game.
start :: Session -> IO ()
start session =
    do
      -- check if we have a savegame
      config <- Config.config
      f <- Save.file config
      x <- doesFileExist f
      restored <- if x then do
                              displayBlankConfirm session "Restoring save game"
                              Save.restoreGame config
                       else return $ Right "Welcome to LambdaHack!"  -- new game
      case restored of
        Right msg  -> generate config session msg
        Left state -> handlerToIO session state "Welcome back to LambdaHack."
                        handle

-- | Generate the dungeon for a new game, and start the game loop.
generate :: Config.CP -> Session -> String -> IO ()
generate config session msg =
  let matchGenerator n Nothing =
        if n == 3 then bigroom else
          if n == 10 then noiseroom else  -- access to stairs may be blocked
            rogueroom
      matchGenerator n (Just "bigroom")   = bigroom
      matchGenerator n (Just "noiseroom") = noiseroom
      matchGenerator n (Just "rogueroom") = rogueroom
      matchGenerator n (Just s) =
        error $ "findGenerator: unknown: " ++ show n ++ ", " ++ s

      findGenerator n =
        let genName = Config.getOption config "dungeon" ("level" ++ show n)
            generator = matchGenerator n genName
        in  rndToIO $ generator (defaultLevelConfig n) (LambdaCave n)

      connect :: Maybe (Maybe DungeonLoc) ->
                 [(Maybe (Maybe DungeonLoc) -> Maybe (Maybe DungeonLoc) ->
                   Level, Loc, Loc)] ->
                 [Level]
      connect au [(x,_,_)] = [x au Nothing]
      connect au ((x,_,d):ys@((_,u,_):_)) =
        let (z:zs) = connect (Just (Just (lname x',d))) ys
            x'     = x au (Just (Just (lname z,u)))
        in  x' : z : zs
  in
   do
     let depth = fromMaybe 10 $ Config.getOption config "dungeon" "depth"
     levels <- mapM findGenerator [1..depth]
     let lvls = connect (Just Nothing) levels
         (lvl,dng) = (head lvls, dungeon (tail lvls))
         -- generate item associations
         assocs = M.fromList
                    [ (Potion PotionWater,   Clear),
                      (Potion PotionHealing, White) ]
         defState = defaultState ((\ (_,x,_) -> x) (head levels)) dng lvl
         state = defState { sassocs = assocs, sconfig = config }
     handlerToIO session state msg handle
