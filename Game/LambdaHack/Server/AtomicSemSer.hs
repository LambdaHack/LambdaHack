{-# LANGUAGE FlexibleInstances, OverloadedStrings, RankNTypes #-}
-- | Sending atomic commands to clients and executing them on the server.
-- See https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture.
module Game.LambdaHack.Server.AtomicSemSer
  ( atomicSendSem
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.ActorState
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.AtomicPos
import Game.LambdaHack.AtomicSem
import Game.LambdaHack.ClientCmd
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Perception
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert

seenAtomicCli :: Bool -> FactionId -> Perception -> PosAtomic -> Bool
seenAtomicCli knowEvents fid per posAtomic =
  case posAtomic of
    PosLevel _ ps -> knowEvents || all (`ES.member` totalVisible per) ps
    PosSmell _ ps -> knowEvents || all (`ES.member` smellVisible per) ps
    PosOnly fid2 -> fid == fid2
    PosAndSer fid2 -> fid == fid2
    PosServer -> False
    PosAll -> True
    PosNone -> assert `failure` fid

seenAtomicSer :: PosAtomic -> Bool
seenAtomicSer posAtomic =
  case posAtomic of
    PosOnly _ -> False
    PosNone -> assert `failure` ("PosNone considered for the server" :: Text)
    _ -> True

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
  (ps, resets, atomicBroken, psBroken, psLoud) <-
    case atomic of
      CmdAtomic cmd -> do
        ps <- posCmdAtomic cmd
        resets <- resetsFovAtomic cmd
        atomicBroken <- breakCmdAtomic cmd
        psBroken <- mapM posCmdAtomic atomicBroken
        psLoud <- mapM loudCmdAtomic atomicBroken
        return (ps, resets, atomicBroken, psBroken, psLoud)
      SfxAtomic sfx -> do
        ps <- posSfxAtomic sfx
        return (ps, Just [], [], [], [])
  let atomicPsBroken = zip3 atomicBroken psBroken psLoud
  -- TODO: assert also that the sum of psBroken is equal to ps
  -- TODO: with deep equality these assertions can be expensive. Optimize.
  assert (case ps of
            PosLevel{} -> True
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
        let send2 (atomic2, ps2, loud2) =
              if seenAtomicCli knowEvents fid perNew ps2
                then sendUpdate fid $ CmdAtomic atomic2
                else when loud2 $
                       sendUpdate fid
                       $ SfxAtomic $ BroadcastD "You hear some noises."
        mapM_ send2 atomicPsBroken
      anySend fid perOld perNew = do
        let startSeen = seenAtomicCli knowEvents fid perOld ps
            endSeen = seenAtomicCli knowEvents fid perNew ps
        if startSeen && endSeen
          then sendUpdate fid atomic
          else breakSend fid perNew
      posLevel fid arena = do
        let perOld = persOld EM.! fid EM.! arena
            resetsFid = maybe True (fid `elem`) resets
        if resetsFid then do
          resetFidPerception fid arena
          perNew <- getPerFid fid arena
          let inPer = diffPer perNew perOld
              inPA = perActor inPer
              outPer = diffPer perOld perNew
              outPA = perActor outPer
          if EM.null outPA && EM.null inPA
            then anySend fid perOld perOld
            else do
              sendA fid $ PerceptionA arena outPA inPA
              unless knowEvents $  -- inconsistencies would quickly manifest
                mapM_ (sendA fid) $ atomicRemember arena inPer sOld
              anySend fid perOld perNew
        else anySend fid perOld perOld
      send fid = case ps of
        PosLevel arena _ -> posLevel fid arena
        -- In the following cases, from the assertion above,
        -- @resets@ is false here and broken atomic has the same ps.
        PosSmell arena _ -> do
          let perOld = persOld EM.! fid EM.! arena
          anySend fid perOld perOld
        PosOnly fid2 -> when (fid == fid2) $ sendUpdate fid atomic
        PosAndSer fid2 -> when (fid == fid2) $ sendUpdate fid atomic
        PosServer -> return ()
        PosAll -> sendUpdate fid atomic
        PosNone -> assert `failure` (atomic, fid)
  faction <- getsState sfaction
  mapM_ send $ EM.keys faction

atomicRemember :: LevelId -> Perception -> State -> [CmdAtomic]
atomicRemember lid inPer s =
  -- No @LoseItemA@ is sent for items that became out of sight.
  -- The client will create these atomic actions based on @outPer@,
  -- if required. Any client that remembers out of sight items, OTOH,
  -- will create atomic actions that forget remembered items
  -- that are revealed not to be there any more (no @SpotItemA@ for them).
  -- Similarly no @LoseActorA@, @LoseTileA@ nor  @LoseSmellA@.
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
      inTileMap = map (\p -> (p, ltile lvl Kind.! p)) inFov
      atomicTile = if null inTileMap then [] else [SpotTileA lid inTileMap]
      -- Smells.
      inSmellFov = ES.elems $ smellVisible inPer
      inSm = mapMaybe (\p -> pMaybe p $ EM.lookup p (lsmell lvl)) inSmellFov
      atomicSmell = if null inSm then [] else [SpotSmellA lid inSm]
  in inItem ++ inActor ++ atomicTile ++ atomicSmell
