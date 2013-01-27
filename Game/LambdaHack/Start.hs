{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | Spwaning client threads and starting up the server.
module Game.LambdaHack.Start
  ( speedupCOps, launchClients, waitForChildren
  ) where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Action
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import qualified Game.LambdaHack.Tile as Tile

-- | Compute and insert auxiliary optimized components into game content,
-- to be used in time-critical sections of the code. Also, evaluate content
-- to check consistency.
speedupCOps :: Kind.COps -> Kind.COps
speedupCOps !copsSlow@Kind.COps{cotile=tile} =
  let ospeedup = Tile.speedup tile
      cotile = tile {Kind.ospeedup}
  in copsSlow {Kind.cotile}

-- | Connect to clients by starting them in spawned threads that read
-- and write directly to the channels.
launchClients :: (FactionId -> ConnCli -> Bool -> IO ()) -> ConnDict -> IO ()
launchClients executorC chanAssocs =
  let forkClient (fid, (chanCli, chanAI)) = do
        let forkAI = case chanAI of
              -- TODO: for a screensaver, try True
              Just ch -> void $ forkChild $ executorC fid ch False
              Nothing -> return ()
        case chanCli of
          Just ch -> do
            void $ forkChild $ executorC fid ch True
            forkAI
          Nothing ->
            forkAI
  in mapM_ forkClient $ EM.toList chanAssocs

-- Swiped from http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.6.0.0/Control-Concurrent.html
children :: MVar [MVar ()]
{-# NOINLINE children #-}
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m : ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar : childs)
  forkIO (io `finally` putMVar mvar ())
-- 7.6  forkFinally io (\_ -> putMVar mvar ())
