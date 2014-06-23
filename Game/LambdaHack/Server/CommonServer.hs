{-# LANGUAGE TupleSections #-}
-- | Server operations common to many modules.
module Game.LambdaHack.Server.CommonServer
  ( execFailure, resetFidPerception, resetLitInDungeon, getPerFid
  , revealItems, deduceQuits, deduceKilled, electLeader
  , projectFail, pickWeaponServer, sumBodyEqpServer
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.ItemServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

execFailure :: (MonadAtomic m, MonadServer m)
            => ActorId -> RequestTimed a -> ReqFailure -> m ()
execFailure aid req failureSer = do
  -- Clients should rarely do that (only in case of invisible actors)
  -- so we report it, send a --more-- meeesage (if not AI), but do not crash
  -- (server should work OK with stupid clients, too).
  body <- getsState $ getActorBody aid
  let fid = bfid body
      msg = showReqFailure failureSer
  debugPrint $ "execFailure:" <+> msg <> "\n"
               <> tshow body <> "\n" <> tshow req
  execSfxAtomic $ SfxMsgFid fid $ "Unexpected problem:" <+> msg <> "."
    -- TODO: --more--, but keep in history

-- | Update the cached perception for the selected level, for a faction.
-- The assumption is the level, and only the level, has changed since
-- the previous perception calculation.
resetFidPerception :: MonadServer m
                   => PersLit -> FactionId -> LevelId
                   -> m Perception
resetFidPerception persLit fid lid = do
  cops <- getsState scops
  sfovMode <- getsServer $ sfovMode . sdebugSer
  lvl <- getLevel lid
  let fovMode = fromMaybe Digital sfovMode
      per = fidLidPerception cops fovMode persLit fid lid lvl
      upd = EM.adjust (EM.adjust (const per) lid) fid
  modifyServer $ \ser2 -> ser2 {sper = upd (sper ser2)}
  return $! per

resetLitInDungeon :: MonadServer m => m PersLit
resetLitInDungeon = do
  sfovMode <- getsServer $ sfovMode . sdebugSer
  ser <- getServer
  let fovMode = fromMaybe Digital sfovMode
  getsState $ \s -> litInDungeon fovMode s ser

getPerFid :: MonadServer m => FactionId -> LevelId -> m Perception
getPerFid fid lid = do
  pers <- getsServer sper
  let fper = fromMaybe (assert `failure` "no perception for faction"
                               `twith` (lid, fid)) $ EM.lookup fid pers
      per = fromMaybe (assert `failure` "no perception for level"
                              `twith` (lid, fid)) $ EM.lookup lid fper
  return $! per

revealItems :: (MonadAtomic m, MonadServer m)
            => Maybe FactionId -> Maybe Actor -> m ()
revealItems mfid mbody = do
  itemToF <- itemToFullServer
  dungeon <- getsState sdungeon
  let discover b iid k =
        let itemFull = itemToF iid k
        in case itemDisco itemFull of
          Just ItemDisco{itemKindId} -> do
            seed <- getsServer $ (EM.! iid) . sitemSeedD
            execUpdAtomic $ UpdDiscover (blid b) (bpos b) iid itemKindId seed
          _ -> assert `failure` (mfid, mbody, iid, itemFull)
      f aid = do
        b <- getsState $ getActorBody aid
        let ourSide = maybe True (== bfid b) mfid
        when (ourSide && Just b /= mbody) $ mapActorItems_ (discover b) b
  mapDungeonActors_ f dungeon
  maybe skip (\b -> mapActorItems_ (discover b) b) mbody

quitF :: (MonadAtomic m, MonadServer m)
      => Maybe Actor -> Status -> FactionId -> m ()
quitF mbody status fid = do
  assert (maybe True ((fid ==) . bfid) mbody) skip
  fact <- getsState $ (EM.! fid) . sfactionD
  let oldSt = gquit fact
  case fmap stOutcome $ oldSt of
    Just Killed -> return ()    -- Do not overwrite in case
    Just Defeated -> return ()  -- many things happen in 1 turn.
    Just Conquer -> return ()
    Just Escape -> return ()
    _ -> do
      when (playerUI $ gplayer fact) $ do
        revealItems (Just fid) mbody
        registerScore status mbody fid
      execUpdAtomic $ UpdQuitFaction fid mbody oldSt $ Just status
      modifyServer $ \ser -> ser {squit = True}  -- end turn ASAP

-- Send any QuitFactionA actions that can be deduced from their current state.
deduceQuits :: (MonadAtomic m, MonadServer m) => Actor -> Status -> m ()
deduceQuits body status@Status{stOutcome}
  | stOutcome `elem` [Defeated, Camping, Restart, Conquer] =
    assert `failure` "no quitting to deduce" `twith` (status, body)
deduceQuits body status = do
  let fid = bfid body
      mapQuitF statusF fids = mapM_ (quitF Nothing statusF) $ delete fid fids
  quitF (Just body) status fid
  let inGameOutcome (_, fact) = case fmap stOutcome $ gquit fact of
        Just Killed -> False
        Just Defeated -> False
        Just Restart -> False  -- effectively, commits suicide
        _ -> True
      inGame (fid2, fact2) =
        if inGameOutcome (fid2, fact2)
        then anyActorsAlive fid2
        else return False
  factionD <- getsState sfactionD
  assocsInGame <- filterM inGame $ EM.assocs factionD
  let assocsInGameOutcome = filter inGameOutcome $ EM.assocs factionD
      keysInGame = map fst assocsInGameOutcome
      assocsKeepArena = filter (keepArenaFact . snd) assocsInGame
      assocsUI = filter (playerUI . gplayer . snd) assocsInGame
      worldPeace =
        all (\(fid1, _) -> all (\(_, fact2) -> not $ isAtWar fact2 fid1)
                           assocsInGame)
        assocsInGame
  case assocsKeepArena of
    _ | null assocsUI ->
      -- Only non-UI players left in the game and they all win.
      mapQuitF status{stOutcome=Conquer} keysInGame
    [] ->
      -- Only leaderless and spawners remain (the latter may sometimes
      -- have no leader, just as the former), so they win,
      -- or we could end up in a state with no active arena.
      mapQuitF status{stOutcome=Conquer} keysInGame
    _ | worldPeace ->
      -- Nobody is at war any more, so all win (e.g., horrors, but never mind).
      mapQuitF status{stOutcome=Conquer} keysInGame
    _ | stOutcome status == Escape -> do
      -- Otherwise, in a game with many warring teams alive,
      -- only complete Victory matters, until enough of them die.
      let (victors, losers) = partition (flip isAllied fid . snd)
                              assocsInGameOutcome
      mapQuitF status{stOutcome=Escape} $ map fst victors
      mapQuitF status{stOutcome=Defeated} $ map fst losers
    _ -> return ()

deduceKilled :: (MonadAtomic m, MonadServer m) => Actor -> m ()
deduceKilled body = do
  Kind.COps{corule} <- getsState scops
  let firstDeathEnds = rfirstDeathEnds $ Kind.stdRuleset corule
      fid = bfid body
  fact <- getsState $ (EM.! fid) . sfactionD
  unless (isSpawnFact fact) $ do  -- spawners never die off
    actorsAlive <- anyActorsAlive fid
    when (not actorsAlive || firstDeathEnds) $
      deduceQuits body $ Status Killed (fromEnum $ blid body) ""

anyActorsAlive :: MonadServer m => FactionId -> m Bool
anyActorsAlive fid = do
  fact <- getsState $ (EM.! fid) . sfactionD
  if playerLeader (gplayer fact)
    then return $! isJust $ gleader fact
    else do
      as <- getsState $ fidActorNotProjList fid
      return $! not $ null as

electLeader :: MonadAtomic m => FactionId -> LevelId -> ActorId -> m ()
electLeader fid lid aidDead = do
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  when (isNothing mleader || mleader == Just aidDead) $ do
    actorD <- getsState sactorD
    let ours (_, b) = bfid b == fid && not (bproj b)
        party = filter ours $ EM.assocs actorD
    onLevel <- getsState $ actorRegularAssocs (== fid) lid
    let mleaderNew = listToMaybe $ filter (/= aidDead)
                     $ map fst $ onLevel ++ party
    unless (mleader == mleaderNew) $
      execUpdAtomic $ UpdLeadFaction fid mleader mleaderNew

projectFail :: (MonadAtomic m, MonadServer m)
            => ActorId    -- ^ actor projecting the item (is on current lvl)
            -> Point      -- ^ target position of the projectile
            -> Int        -- ^ digital line parameter
            -> ItemId     -- ^ the item to be projected
            -> CStore     -- ^ whether the items comes from floor or inventory
            -> Bool       -- ^ whether the item is a shrapnel
            -> m (Maybe ReqFailure)
projectFail source tpxy eps iid cstore isShrapnel = do
  Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
      spos = bpos sb
  lvl@Level{lxsize, lysize} <- getLevel lid
  case bla lxsize lysize eps spos tpxy of
    Nothing -> return $ Just ProjectAimOnself
    Just [] -> assert `failure` "projecting from the edge of level"
                      `twith` (spos, tpxy)
    Just (pos : restUnlimited) -> do
      item <- getsState $ getItemBody iid
      let fragile = Effect.Fragile `elem` jfeature item
          rest = if fragile
                 then take (chessDist spos tpxy - 1) restUnlimited
                 else restUnlimited
          t = lvl `at` pos
      if not $ Tile.isWalkable cotile t
        then return $ Just ProjectBlockTerrain
        else do
          mab <- getsState $ posToActor pos lid
          actorBlind <- radiusBlind
                        <$> sumBodyEqpServer Effect.EqpSlotSightRadius source
          activeItems <- activeItemsServer source
          if not $ maybe True (bproj . snd . fst) mab
            then if isShrapnel then do
                   -- Hit the blocking actor.
                   projectBla source spos (pos : rest) iid cstore
                   return Nothing
                 else return $ Just ProjectBlockActor
            else if not (isShrapnel || calmEnough sb activeItems) then
                   return $ Just ProjectNotCalm
                 else if actorBlind && not (bproj sb) then
                   return $ Just ProjectBlind
                 else do
                   if isShrapnel && eps `mod` 2 == 0 then
                     -- Make the explosion a bit less regular.
                     projectBla source spos (pos:rest) iid cstore
                   else
                     projectBla source pos rest iid cstore
                   return Nothing

projectBla :: (MonadAtomic m, MonadServer m)
           => ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ starting point of the projectile
           -> [Point]    -- ^ rest of the trajectory of the projectile
           -> ItemId     -- ^ the item to be projected
           -> CStore     -- ^ whether the items comes from floor or inventory
           -> m ()
projectBla source pos rest iid cstore = do
  sb <- getsState $ getActorBody source
  item <- getsState $ getItemBody iid
  let lid = blid sb
      time = btime sb
  unless (bproj sb) $ execSfxAtomic $ SfxProject source iid
  addProjectile pos rest iid lid (bfid sb) time
  let c = CActor source cstore
  execUpdAtomic $ UpdLoseItem iid item 1 c

-- | Create a projectile actor containing the given missile.
--
-- Projectile has no body parts except for the trunk.
addProjectile :: (MonadAtomic m, MonadServer m)
              => Point -> [Point] -> ItemId -> LevelId -> FactionId -> Time
              -> m ()
addProjectile bpos rest iid blid bfid btime = do
  aid <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ aid}
  -- We bootstrap the actor by first creating the trunk of the actor's body
  -- contains the constant properties.
  let trunkFreq = toFreq "create trunk" [(1, "projectile")]
  (trunkId, _trunkFull@ItemFull{..})
    <- rollAndRegisterItem blid trunkFreq (CTrunk blid bpos) False
  item <- getsState $ getItemBody iid
  let ts@(trajectory, _) = itemTrajectory item (bpos : rest)
      trange = length trajectory
      adj | trange < 5 = "falling"
          | otherwise = "flying"
      -- Not much detail about a fast flying item.
      (object1, object2) = partItem $ itemNoDisco (item, 1)
      name = makePhrase [MU.AW $ MU.Text adj, object1, object2]
      b = actorTemplate trunkId (jsymbol itemBase) name "it"
                        (flavourToColor $ jflavour itemBase) 0 maxBound
                        bpos blid btime bfid True
      btra = b {btrajectory = Just ts}
      beqp = btra {beqp = EM.singleton iid 1}
      -- Insert the trunk as the actor's body part.
      btrunk = beqp {bbody = EM.singleton trunkId itemK}
  execUpdAtomic $ UpdCreateActor aid btrunk [(iid, item), (trunkId, itemBase)]

-- Server has to pick a random weapon or it could leak item discovery
-- information.
pickWeaponServer :: MonadServer m => ActorId -> m (Maybe (ItemId, CStore))
pickWeaponServer source = do
  sb <- getsState $ getActorBody source
  eqpAssocs <- fullAssocsServer source [CEqp]
  bodyAssocs <- fullAssocsServer source [CBody]
  -- For projectiles we need to accept even items without any effect,
  -- so that the projectile dissapears and NoEffect feedback is produced.
  let allAssocs = eqpAssocs ++ bodyAssocs
      strongest | bproj sb = map (1,) allAssocs
                | otherwise =
                    filter (not . unknownPrecious . snd . snd)
                    $ strongestSlotNoFilter Effect.EqpSlotWeapon allAssocs
  case strongest of
    [] -> return Nothing
    iis -> do
      let is = map snd iis
      -- TODO: pick the item according to the frequency of its kind.
      (iid, _) <- rndToAction $ oneOf is
      let cstore = if isJust (lookup iid bodyAssocs) then CBody else CEqp
      return $ Just (iid, cstore)

sumBodyEqpServer :: MonadServer m
                 => Effect.EqpSlot -> ActorId -> m Int
sumBodyEqpServer eqpSlot aid = do
  eqpAssocs <- fullAssocsServer aid [CEqp]
  bodyAssocs <- fullAssocsServer aid [CBody]
  return $! sumSlotNoFilter eqpSlot $ map snd $ eqpAssocs ++ bodyAssocs
