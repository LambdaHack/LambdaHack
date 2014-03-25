-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.EndServer
  ( endOrLoop, dieSer
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.CommonServer
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
        Just Status{stOutcome=Restart, stInfo} -> Just stInfo
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
    dropAllItems aid b hit
    b2 <- getsState $ getActorBody aid
    execUpdAtomic $ UpdDestroyActor aid b2 []
  else do
    execUpdAtomic $ UpdRecordKill aid 1
    electLeader (bfid b) (blid b) aid
    dropAllItems aid b False
    b2 <- getsState $ getActorBody aid
    execUpdAtomic $ UpdDestroyActor aid b2 []
    deduceKilled b

-- | Drop all actor's items. If the actor hits another actor and this
-- collision results in all item being dropped, all items are destroyed.
-- If the actor does not hit, but dies, only fragile items are destroyed
-- and only if the actor was a projectile (and so died by dropping
-- to the ground due to exceeded range or bumping off an obstacle).
dropAllItems :: (MonadAtomic m, MonadServer m)
             => ActorId -> Actor -> Bool -> m ()
dropAllItems aid b hit = do
  Kind.COps{coitem, corule} <- getsState scops
  let RuleKind{rsharedInventory} = Kind.stdRuleset corule
  discoS <- getsServer sdisco
  let container = CActor aid CEqp
      loseInv = do
        let g iid k = execUpdAtomic
                      $ UpdMoveItem iid k aid CInv CEqp
        mapActorInv_ g b
  if not rsharedInventory then loseInv
  else do
    fact <- getsState $ (EM.! bfid b) . sfactionD
    case gleader fact of
      Nothing -> loseInv
      Just leader -> do
        let g iid k = do
              upds <- generalMoveItem iid k (CActor aid CInv)
                                            (CActor leader CInv)
              mapM_ execUpdAtomic upds
        mapActorInv_ g b
  let isDestroyed item = hit || bproj b && isFragile coitem discoS item
      f iid k = do
        item <- getsState $ getItemBody iid
        if isDestroyed item then
          case isExplosive coitem discoS item of
            Nothing -> execUpdAtomic $ UpdDestroyItem iid item k container
            Just cgroup -> do
              let ik = fromJust $ jkind discoS item
              execUpdAtomic $ UpdDiscover (blid b) (bpos b) iid ik
              execUpdAtomic $ UpdDestroyItem iid item k container
              explodeItem aid b cgroup
        else
          execUpdAtomic $ UpdMoveItem iid k aid CEqp CGround
  mapActorEqp_ f b

explodeItem :: (MonadAtomic m, MonadServer m)
            => ActorId -> Actor -> Text -> m ()
explodeItem aid b cgroup = do
  Kind.COps{coitem} <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  Level{ldepth} <- getLevel $ blid b
  depth <- getsState sdepth
  let itemFreq = toFreq "shrapnel group" [(1, cgroup)]
  (item, n1, _) <- rndToAction
                   $ newItem coitem flavour discoRev itemFreq ldepth depth
  let container = CActor aid CEqp
  iid <- registerItem item n1 container False
  let Point x y = bpos b
      projectN n = replicateM_ n $ do
        tpxy <- rndToAction $ do
          border <- randomR (1, 4)
          -- We pick a point at the border, not inside, to have a uniform
          -- distribution for the points the line goes through at each distance
          -- from the source. Otherwise, e.g., the points on cardinal
          -- and diagonal lines from the source would be more common.
          case border :: Int of
            1 -> fmap (Point (x - 10)) $ randomR (y - 10, y + 10)
            2 -> fmap (Point (x + 10)) $ randomR (y - 10, y + 10)
            3 -> fmap (flip Point (y - 10)) $ randomR (x - 10, x + 10)
            4 -> fmap (flip Point (y + 10)) $ randomR (x - 10, x + 10)
            _ -> assert `failure` border
        let eps = px tpxy + py tpxy
            req = ReqProject aid tpxy eps iid CEqp
        mfail <- projectFail aid tpxy eps iid CEqp True
        case mfail of
          Nothing -> return ()
          Just ProjectBlockTerrain -> return ()
          Just failMsg -> execFailure req failMsg
  projectN n1
  bag2 <- getsState $ beqp . getActorBody aid
  let mn2 = EM.lookup iid bag2
  maybe skip projectN mn2  -- assume all shrapnels bounce off obstacles once
  bag3 <- getsState $ beqp . getActorBody aid
  let mn3 = EM.lookup iid bag3
  maybe skip (\k -> execUpdAtomic $ UpdLoseItem iid item k container) mn3
