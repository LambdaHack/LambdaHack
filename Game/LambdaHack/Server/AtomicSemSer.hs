-- | Sending atomic commands to clients and executing them on the server.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server.AtomicSemSer
  ( atomicSendSem
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Key (mapWithKeyM_)
import Data.Maybe

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.HandleCmdAtomicWrite
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Atomic.PosCmdAtomicRead
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

storeUndo :: MonadServer m => Atomic -> m ()
storeUndo _atomic =
  maybe skip (\a -> modifyServer $ \ser -> ser {sundo = a : sundo ser})
    $ Nothing   -- TODO: undoAtomic atomic

atomicServerSem :: (MonadWriteState m, MonadServer m)
                => PosAtomic -> Atomic -> m ()
atomicServerSem posAtomic atomic =
  when (seenAtomicSer posAtomic) $ do
    storeUndo atomic
    case atomic of
      CmdAtomic cmd -> cmdAtomicSem cmd
      SfxAtomic _ -> return ()

-- | Send an atomic action to all clients that can see it.
atomicSendSem :: (MonadWriteState m, MonadConnServer m) => Atomic -> m ()
atomicSendSem atomic = do
  -- Gather data from the old state.
  sOld <- getState
  factionD <- getsState sfactionD
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
  let sendUI fid cmdUI =
        when (playerUI $ gplayer $ factionD EM.! fid) $ sendUpdateUI fid cmdUI
      sendAI fid cmdAI = sendUpdateAI fid cmdAI
      sendA fid cmd = do
        sendUI fid $ RespCmdAtomicUI cmd
        sendAI fid $ RespCmdAtomicAI cmd
      sendUpdate fid (CmdAtomic cmd) = sendA fid cmd
      sendUpdate fid (SfxAtomic sfx) = sendUI fid $ RespSfxAtomicUI sfx
      breakSend fid perNew = do
        let send2 (atomic2, ps2) =
              if seenAtomicCli knowEvents fid perNew ps2
                then sendUpdate fid $ CmdAtomic atomic2
                else when (loudCmdAtomic fid atomic2) $
                       sendUpdate fid
                       $ SfxAtomic $ MsgAllD "You hear some noises."
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
              outPer = diffPer perOld perNew
          if nullPer outPer && nullPer inPer
            then anySend fid perOld perOld
            else do
              unless knowEvents $ do  -- inconsistencies would quickly manifest
                sendA fid $ PerceptionA lid outPer inPer
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
        PosNone -> assert `failure` "illegal sending" `twith` (atomic, fid)
  mapWithKeyM_ (\fid _ -> send fid) factionD

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
      inPrio = concatMap (\p -> posToActors p lid s) inFov
      fActor ((aid, b), ais) = SpotActorA aid b ais
      inActor = map fActor inPrio
      -- Items.
      pMaybe p = maybe Nothing (\x -> Just (p, x))
      inFloor = mapMaybe (\p -> pMaybe p $ EM.lookup p (lfloor lvl)) inFov
      fItem p (iid, k) = SpotItemA iid (getItemBody iid s) k (CFloor lid p)
      fBag (p, bag) = map (fItem p) $ EM.assocs bag
      inItem = concatMap fBag inFloor
      -- Tiles.
      cotile = Kind.cotile (scops s)
      inTileMap = map (\p -> (p, hideTile cotile lvl p)) inFov
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
      -- Smells.
      inSmellFov = ES.elems $ smellVisible inPer
      inSm = mapMaybe (\p -> pMaybe p $ EM.lookup p (lsmell lvl)) inSmellFov
      atomicSmell = if null inSm then [] else [SpotSmellA lid inSm]
  in inItem ++ inActor ++ atomicTile ++ atomicSmell
