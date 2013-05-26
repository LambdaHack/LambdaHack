{-# LANGUAGE OverloadedStrings #-}
-- | Sending atomic commands to clients and executing them on the server.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server.AtomicSemSer
  ( atomicSendSem
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Maybe

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.AtomicPos
import Game.LambdaHack.Common.AtomicSem
import Game.LambdaHack.Common.ClientCmd
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.State
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.State
import Game.LambdaHack.Utils.Assert

storeUndo :: MonadServer m => Atomic -> m ()
storeUndo atomic = do
  maybe skip (\a -> modifyServer $ \ser -> ser {sundo = a : sundo ser})
    $ undoAtomic atomic

atomicServerSem :: (MonadAction m, MonadServer m)
                => PosAtomic -> Atomic -> m ()
atomicServerSem posAtomic atomic =
  when (seenAtomicSer posAtomic) $ do
    storeUndo atomic
    case atomic of
      CmdAtomic cmd -> cmdAtomicSem cmd
      SfxAtomic _ -> return ()

-- | Send an atomic action to all clients that can see it.
atomicSendSem :: (MonadAction m, MonadServerConn m) => Atomic -> m ()
atomicSendSem atomic = do
  -- Gather data from the old state.
  sOld <- getState
  persOld <- getsServer sper
  (ps, resets, atomicBroken, psBroken) <-
    case atomic of
      CmdAtomic cmd -> do
        ps <- posCmdAtomic cmd
        resets <- resetsFovAtomic cmd
        atomicBroken <- breakCmdAtomic cmd
        psBroken <- mapM posCmdAtomic atomicBroken
        return (ps, resets, atomicBroken, psBroken)
      SfxAtomic sfx -> do
        ps <- posSfxAtomic sfx
        return (ps, Just [], [], [])
  let atomicPsBroken = zip atomicBroken psBroken
  -- TODO: assert also that the sum of psBroken is equal to ps
  -- TODO: with deep equality these assertions can be expensive. Optimize.
  assert (case ps of
            PosSight{} -> True
            PosFidAndSight{} -> True
            _ -> resets == Just []
                 && (null atomicBroken
                     || fmap CmdAtomic atomicBroken == [atomic])) skip
  -- Perform the action on the server.
  atomicServerSem ps atomic
  -- Send some actions to the clients, one faction at a time.
  knowEvents <- getsServer $ sknowEvents . sdebugSer
  let sendA fid cmd = do
        sendUpdateUI fid $ CmdAtomicUI cmd
        sendUpdateAI fid $ CmdAtomicAI cmd
      sendUpdate fid (CmdAtomic cmd) = sendA fid cmd
      sendUpdate fid (SfxAtomic sfx) = sendUpdateUI fid $ SfxAtomicUI sfx
      breakSend fid perNew = do
        let send2 (atomic2, ps2) =
              if seenAtomicCli knowEvents fid perNew ps2
                then sendUpdate fid $ CmdAtomic atomic2
                else when (loudCmdAtomic fid atomic2) $
                       sendUpdate fid
                       $ SfxAtomic $ BroadcastD "You hear some noises."
        mapM_ send2 atomicPsBroken
      anySend fid perOld perNew = do
        let startSeen = seenAtomicCli knowEvents fid perOld ps
            endSeen = seenAtomicCli knowEvents fid perNew ps
        if startSeen && endSeen
          then sendUpdate fid atomic
          else breakSend fid perNew
      posLevel fid lid = do
        let perOld = persOld EM.! fid EM.! lid
            resetsFid = maybe True (fid `elem`) resets
        if resetsFid then do
          resetFidPerception fid lid
          perNew <- getPerFid fid lid
          let inPer = diffPer perNew perOld
              inPA = perActor inPer
              outPer = diffPer perOld perNew
              outPA = perActor outPer
          if EM.null outPA && EM.null inPA
            then anySend fid perOld perOld
            else do
              sendA fid $ PerceptionA lid outPA inPA
              unless knowEvents $ do  -- inconsistencies would quickly manifest
                let remember = atomicRemember lid inPer sOld
                    seenNew = seenAtomicCli False fid perNew
                    seenOld = seenAtomicCli False fid perOld
                -- TODO: these assertions are probably expensive
                psRem <- mapM posCmdAtomic remember
                -- Verify that we remember only currently seen things.
                assert (allB seenNew psRem) skip
                -- Verify that we remember only new things.
                assert (allB (not . seenOld) psRem) skip
                mapM_ (sendA fid) remember
              anySend fid perOld perNew
        else anySend fid perOld perOld
      send fid = case ps of
        PosSight lid _ -> posLevel fid lid
        PosFidAndSight _ lid _ -> posLevel fid lid
        -- In the following cases, from the assertion above,
        -- @resets@ is false here and broken atomic has the same ps.
        PosSmell lid _ -> do
          let perOld = persOld EM.! fid EM.! lid
          anySend fid perOld perOld
        PosFid fid2 -> when (fid == fid2) $ sendUpdate fid atomic
        PosFidAndSer fid2 -> when (fid == fid2) $ sendUpdate fid atomic
        PosSer -> return ()
        PosAll -> sendUpdate fid atomic
        PosNone -> assert `failure` (atomic, fid)
  factionD <- getsState sfactionD
  mapM_ send $ EM.keys factionD

atomicRemember :: LevelId -> Perception -> State -> [CmdAtomic]
atomicRemember lid inPer s =
  -- No @LoseItemA@ is sent for items that became out of sight.
  -- The client will create these atomic actions based on @outPer@,
  -- if required. Any client that remembers out of sight items, OTOH,
  -- will create atomic actions that forget remembered items
  -- that are revealed not to be there any more (no @SpotItemA@ for them).
  -- Similarly no @LoseActorA@, @LoseTileA@ nor @LoseSmellA@.
  let inFov = ES.elems $ totalVisible inPer
      lvl = sdungeon s EM.! lid
      -- Actors.
      inPrio = mapMaybe (\p -> posToActor p lid s) inFov
      fActor aid = SpotActorA aid (getActorBody aid s) (getActorItem aid s)
      inActor = map fActor inPrio
      -- Items.
      pMaybe p = maybe Nothing (\x -> Just (p, x))
      inFloor = mapMaybe (\p -> pMaybe p $ EM.lookup p (lfloor lvl)) inFov
      fItem p (iid, k) = SpotItemA iid (getItemBody iid s) k (CFloor lid p)
      fBag (p, bag) = map (fItem p) $ EM.assocs bag
      inItem = concatMap fBag inFloor
      -- Tiles.
      cotile = Kind.cotile (scops s)
      inTileMap = map (\p -> (p, hideTile cotile p lvl)) inFov
      atomicTile = if null inTileMap then [] else [SpotTileA lid inTileMap]
      -- TODO: somehow also use this
      -- bonus = case strongestSearch itemAssocs of
      --          Just (k, _)  -> k + 1
      --          Nothing -> 1
      -- TODO: add 'search' that rescans FOV, perhaps with a bonus
      -- TODO: add 'explore' that tells a tile is a hidden door, etc.
      -- TODO: when explored tile is boring, display tips, monster scratches,
      -- old inscriptions and other flavour, as in UnAngband
      -- TODO: make floor paths from hidden tiles
      -- TODO: perhaps decrease secrecy as lseen, ltime or lsmell increases
      -- or a per-party counter increases
      -- TODO: give spawning factions a bonus to searching
      -- Smells.
      inSmellFov = ES.elems $ smellVisible inPer
      inSm = mapMaybe (\p -> pMaybe p $ EM.lookup p (lsmell lvl)) inSmellFov
      atomicSmell = if null inSm then [] else [SpotSmellA lid inSm]
  in inItem ++ inActor ++ atomicTile ++ atomicSmell
