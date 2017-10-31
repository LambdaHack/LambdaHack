-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.EndM
  ( endOrLoop, dieSer
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.CommonM
import Game.LambdaHack.Server.HandleEffectM
import Game.LambdaHack.Server.ItemM
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- | Continue or exit or restart the game.
endOrLoop :: MonadServerAtomic m
          => m () -> (Maybe (GroupName ModeKind) -> m ()) -> m () -> m ()
          -> m ()
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
      restartNeeded = gameOver || not (null quitters)
  let isCamper fact = case gquit fact of
        Just Status{stOutcome=Camping} -> True
        _ -> False
      campers = filter (isCamper . snd) $ EM.assocs factionD
  -- Wipe out the quit flag for the savegame files.
  mapM_ (\(fid, fact) ->
    execUpdAtomic $ UpdQuitFaction fid (gquit fact) Nothing) campers
  swriteSave <- getsServer swriteSave
  when (swriteSave && not restartNeeded) $ do
    modifyServer $ \ser -> ser {swriteSave = False}
    gameSave
  if | restartNeeded -> restart (listToMaybe quitters)
     | not $ null campers -> gameExit  -- and @loop@ is not called
     | otherwise -> loop  -- continue current game

dieSer :: MonadServerAtomic m => ActorId -> Actor -> m ()
dieSer aid b = do
  unless (bproj b) $ do
    discoKind <- getsState sdiscoKind
    trunk <- getsState $ getItemBody $ btrunk b
    let KindMean{kmKind} = discoKind EM.! jkindIx trunk
    execUpdAtomic $ UpdRecordKill aid kmKind 1
    -- At this point the actor's body exists and his items are not dropped.
    deduceKilled aid
    electLeader (bfid b) (blid b) aid
    fact <- getsState $ (EM.! bfid b) . sfactionD
    -- Prevent faction's stash from being lost in case they are not spawners.
    -- Projectiles can't drop stash, because they are blind and so the faction
    -- would not see the actor that drops the stash, leading to a crash.
    -- But this is OK; projectiles can't be leaders, so stash dropped earlier.
    when (isNothing $ _gleader fact) $ moveStores False aid CSha CInv
  -- If the actor was a projectile and no effect was triggered by hitting
  -- an enemy, the item still exists and @OnSmash@ effects will be triggered:
  dropAllItems aid b
  b2 <- getsState $ getActorBody aid
  execUpdAtomic $ UpdDestroyActor aid b2 []

-- | Drop all actor's items.
dropAllItems :: MonadServerAtomic m => ActorId -> Actor -> m ()
dropAllItems aid b = do
  mapActorCStore_ CInv (dropCStoreItem False CInv aid b maxBound) b
  mapActorCStore_ CEqp (dropCStoreItem False CEqp aid b maxBound) b
