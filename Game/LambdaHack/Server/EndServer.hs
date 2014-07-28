-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.EndServer
  ( endOrLoop, dieSer, dropEqpItems
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State
import Game.LambdaHack.Server.CommonServer
import Game.LambdaHack.Server.HandleEffectServer
import Game.LambdaHack.Server.ItemServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- | Continue or exit or restart the game.
endOrLoop :: (MonadAtomic m, MonadServer m)
          => m () -> m () -> m () -> m () -> m ()
endOrLoop loop restart gameExit gameSave = do
  factionD <- getsState sfactionD
  let inGame fact = case gquit fact of
        Nothing -> True
        Just Status{stOutcome=Camping} -> True
        _ -> False
      gameOver = not $ any inGame $ EM.elems factionD
  let getQuitter fact = case gquit fact of
        Just Status{stOutcome=Restart, stNewGame} -> stNewGame
        _ -> Nothing
      quitters = mapMaybe getQuitter $ EM.elems factionD
  let isCamper fact = case gquit fact of
        Just Status{stOutcome=Camping} -> True
        _ -> False
      campers = filter (isCamper . snd) $ EM.assocs factionD
  -- Wipe out the quit flag for the savegame files.
  mapM_ (\(fid, fact) ->
            execUpdAtomic
            $ UpdQuitFaction fid Nothing (gquit fact) Nothing) campers
  bkpSave <- getsServer sbkpSave
  when bkpSave $ do
    modifyServer $ \ser -> ser {sbkpSave = False}
    gameSave
  case (quitters, campers) of
    (sgameMode : _, _) -> do
      modifyServer $ \ser -> ser {sdebugNxt = (sdebugNxt ser) {sgameMode}}
      restart
    _ | gameOver -> restart
    ([], []) -> loop  -- continue current game
    ([], _ : _) -> gameExit  -- don't call @loop@, that is, quit the game loop

dieSer :: (MonadAtomic m, MonadServer m) => ActorId -> Actor -> Bool -> m ()
dieSer aid b hit = do
  -- TODO: clients don't see the death of their last standing actor;
  --       modify Draw.hs and Client.hs to handle that
  if bproj b then do
    dropEqpItems aid b hit
    b2 <- getsState $ getActorBody aid
    execUpdAtomic $ UpdDestroyActor aid b2 []
  else do
    discoKind <- getsServer sdiscoKind
    trunk <- getsState $ getItemBody $ btrunk b
    let ikind = discoKind EM.! jkindIx trunk
    execUpdAtomic $ UpdRecordKill aid ikind 1
    electLeader (bfid b) (blid b) aid
    equipAllItems aid b
    dropEqpItems aid b False
    b2 <- getsState $ getActorBody aid
    execUpdAtomic $ UpdDestroyActor aid b2 []
    deduceKilled b

equipAllItems :: (MonadAtomic m, MonadServer m)
              => ActorId -> Actor -> m ()
equipAllItems aid b = do
  fact <- getsState $ (EM.! bfid b) . sfactionD
  -- A faction that is defeated, leaderless or with temporarlity no member
  -- drops all items from the faction stash, too.
  when (isNothing $ gleader fact) $ moveStores aid CSha CEqp
  moveStores aid CInv CEqp

-- | Drop all actor's items. If the actor hits another actor and this
-- collision results in all item being dropped, all items are destroyed.
-- If the actor does not hit, but dies, only fragile items are destroyed
-- and only if the actor was a projectile (and so died by dropping
-- to the ground due to exceeded range or bumping off an obstacle).
dropEqpItems :: (MonadAtomic m, MonadServer m)
             => ActorId -> Actor -> Bool -> m ()
dropEqpItems aid b hit = mapActorCStore_ CEqp (dropEqpItem aid b hit) b
