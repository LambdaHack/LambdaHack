module Main where

import System.Directory
import Control.Monad

import State
import Geometry
import Level
import Dungeon
import Perception
import Display2
import Random
import Save
import Turn

main :: IO ()
main = startup start

-- | Either restore a saved game, or setup a new game.
start :: Session -> IO ()
start session =
    do
      -- check if we have a savegame
      x <- doesFileExist savefile
      restored <- if x then do
                              displayBlankConfirm session "Restoring save game"
                              restoreGame
                       else return $ Right "Welcome to LambdaHack!"  -- new game
      case restored of
        Right msg        -> generate session msg
        Left (lvl,state) -> handle session lvl state (perception_ state lvl)
                                   "Welcome back to LambdaHack."

-- | Generate the dungeon for a new game, and start the game loop.
generate :: Session -> String -> IO ()
generate session msg =
  do
    -- generate dungeon with 10 levels
    levels <- rndToIO $
              mapM (\n -> (if n == 3 then bigroom else level) (defaultLevelConfig n) $
              LambdaCave n) [1..10]
    let connect :: Maybe (Maybe DungeonLoc) ->
                   [(Maybe (Maybe DungeonLoc) -> Maybe (Maybe DungeonLoc) -> Level, Loc, Loc)] ->
                   [Level]
        connect au [(x,_,_)] = [x au Nothing]
        connect au ((x,_,d):ys@((_,u,_):_)) =
                            let (z:zs) = connect (Just (Just (lname x',d))) ys
                                x'     = x au (Just (Just (lname z,u)))
                            in  x' : z : zs
    let lvls = connect (Just Nothing) levels
    let (lvl,dng) = (head lvls, dungeon (tail lvls))
    let state = defaultState ((\ (_,x,_) -> x) (head levels)) dng
    handle session lvl state (perception_ state lvl) msg

