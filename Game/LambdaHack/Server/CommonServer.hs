-- | Server operations common to many modules.
module Game.LambdaHack.Server.CommonServer
  ( execFailure, resetFidPerception, getPerFid
  , revealItems, deduceQuits, deduceKilled, electLeader
  , registerItem, createItems, projectFail
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

execFailure :: (MonadAtomic m, MonadServer m)
            => ActorId -> ReqFailure -> m ()
execFailure aid failureSer = do
  -- Clients should rarely do that (only in case of invisible actors)
  -- so we report it, send a --more-- meeesage (if not AI), but do not crash
  -- (server should work OK with stupid clients, too).
  body <- getsState $ getActorBody aid
  let fid = bfid body
      msg = showReqFailure failureSer
  debugPrint
    $ "execFailure:" <+> tshow fid <+> ":" <+> msg <> "\n" <> tshow body
  execSfxAtomic $ SfxMsgFid fid $ "Unexpected problem:" <+> msg <> "."
    -- TODO: --more--, but keep in history

-- | Update the cached perception for the selected level, for a faction.
-- The assumption is the level, and only the level, has changed since
-- the previous perception calculation.
resetFidPerception :: MonadServer m => FactionId -> LevelId -> m Perception
resetFidPerception fid lid = do
  cops <- getsState scops
  lvl <- getLevel lid
  fovMode <- getsServer $ sfovMode . sdebugSer
  per <- getsState
         $ levelPerception cops (fromMaybe (Digital 12) fovMode) fid lid lvl
  let upd = EM.adjust (EM.adjust (const per) lid) fid
  modifyServer $ \ser -> ser {sper = upd (sper ser)}
  return $! per

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
  dungeon <- getsState sdungeon
  discoS <- getsServer sdisco
  let discover b iid _numPieces = do
        item <- getsState $ getItemBody iid
        let ik = fromJust $ jkind discoS item
        execUpdAtomic $ UpdDiscover (blid b) (bpos b) iid ik
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
  cops <- getsState scops
  let fid = bfid body
      mapQuitF statusF fids = mapM_ (quitF Nothing statusF) $ delete fid fids
  quitF (Just body) status fid
  let inGame fact = case fmap stOutcome $ gquit fact of
        Just Killed -> False
        Just Defeated -> False
        Just Restart -> False  -- effectively, commits suicide
        _ -> True
  factionD <- getsState sfactionD
  let assocsInGame = filter (inGame . snd) $ EM.assocs factionD
      keysInGame = map fst assocsInGame
      assocsNotHorror = filter (not . isHorrorFact cops . snd) assocsInGame
      assocsUI = filter (playerUI . gplayer . snd) assocsInGame
  case assocsNotHorror of
    _ | null assocsUI ->
      -- Only non-UI players left in the game and they all win.
      mapQuitF status{stOutcome=Conquer} keysInGame
    [] ->
      -- Only horrors remain, so they win.
      mapQuitF status{stOutcome=Conquer} keysInGame
    (_, fact1) : rest | all (not . isAtWar fact1 . fst) rest ->
      -- Nobody is at war any more, so all win.
      -- TODO: check not at war with each other.
      mapQuitF status{stOutcome=Conquer} keysInGame
    _ | stOutcome status == Escape -> do
      -- Otherwise, in a game with many warring teams alive,
      -- only complete Victory matters, until enough of them die.
      let (victors, losers) = partition (flip isAllied fid . snd) assocsInGame
      mapQuitF status{stOutcome=Escape} $ map fst victors
      mapQuitF status{stOutcome=Defeated} $ map fst losers
    _ -> return ()

deduceKilled :: (MonadAtomic m, MonadServer m) => Actor -> m ()
deduceKilled body = do
  cops@Kind.COps{corule} <- getsState scops
  let firstDeathEnds = rfirstDeathEnds $ Kind.stdRuleset corule
      fid = bfid body
  spawn <- getsState $ isSpawnFaction fid
  fact <- getsState $ (EM.! fid) . sfactionD
  let horror = isHorrorFact cops fact
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  when (not spawn && not horror
        && (isNothing mleader || firstDeathEnds)) $
    deduceQuits body $ Status Killed (fromEnum $ blid body) ""

electLeader :: MonadAtomic m => FactionId -> LevelId -> ActorId -> m ()
electLeader fid lid aidDead = do
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  when (isNothing mleader || mleader == Just aidDead) $ do
    actorD <- getsState sactorD
    let ours (_, b) = bfid b == fid && not (bproj b)
        party = filter ours $ EM.assocs actorD
    onLevel <- getsState $ actorNotProjAssocs (== fid) lid
    let mleaderNew = listToMaybe $ filter (/= aidDead)
                     $ map fst $ onLevel ++ party
    unless (mleader == mleaderNew) $
      execUpdAtomic $ UpdLeadFaction fid mleader mleaderNew

registerItem :: (MonadAtomic m, MonadServer m)
             => Item -> Int -> Container -> Bool -> m ItemId
registerItem item k container verbose = do
  itemRev <- getsServer sitemRev
  let cmd = if verbose then UpdCreateItem else UpdSpotItem
  case HM.lookup item itemRev of
    Just iid -> do
      -- TODO: try to avoid this case for createItems,
      -- to make items more interesting
      execUpdAtomic $ cmd iid item k container
      return iid
    Nothing -> do
      icounter <- getsServer sicounter
      modifyServer $ \ser ->
        ser { sicounter = succ icounter
            , sitemRev = HM.insert item icounter (sitemRev ser) }
      execUpdAtomic $ cmd icounter item k container
      return $! icounter

createItems :: (MonadAtomic m, MonadServer m)
            => Int -> Point -> LevelId -> m ()
createItems n pos lid = do
  Kind.COps{coitem} <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  Level{ldepth, litemFreq} <- getLevel lid
  depth <- getsState sdepth
  let container = CFloor lid pos
  replicateM_ n $ do
    (item, k, _) <- rndToAction
                    $ newItem coitem flavour discoRev litemFreq ldepth depth
    void $ registerItem item k container True

projectFail :: (MonadAtomic m, MonadServer m)
            => ActorId    -- ^ actor projecting the item (is on current lvl)
            -> Point      -- ^ target position of the projectile
            -> Int        -- ^ digital line parameter
            -> ItemId     -- ^ the item to be projected
            -> CStore     -- ^ whether the items comes from floor or inventory
            -> Bool       -- ^ whether the item is a shrapnel
            -> m (Maybe ReqFailure)
projectFail source tpxy eps iid cstore isShrapnel = do
  Kind.COps{coactor=Kind.Ops{okind}, cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
      spos = bpos sb
      kind = okind $ bkind sb
  lvl@Level{lxsize, lysize} <- getLevel lid
  case bla lxsize lysize eps spos tpxy of
    Nothing -> return $ Just ProjectAimOnself
    Just [] -> assert `failure` "projecting from the edge of level"
                      `twith` (spos, tpxy)
    Just (pos : rest) -> do
      let t = lvl `at` pos
      if not $ Tile.isClear cotile t
        then return $ Just ProjectBlockTerrain
        else do
          mab <- getsState $ posToActor pos lid
          if not $ maybe True (bproj . snd . fst) mab
            then if isShrapnel then do
                   -- Hit the blocking actor.
                   projectBla source spos (pos : rest) iid cstore
                   return Nothing
                 else return $ Just ProjectBlockActor
            else if not (isShrapnel || calmEnough sb kind) then
                   return $ Just ProjectNotCalm
                 else if not (asight kind || bproj sb) then
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
  let lid = blid sb
      time = btime sb
  unless (bproj sb) $ execSfxAtomic $ SfxProject source iid
  projId <- addProjectile pos rest iid lid (bfid sb) time
  cs <- actorConts iid 1 source cstore
  mapM_ (\(_, c) -> execUpdAtomic $ UpdMoveItem iid 1 c (CActor projId CEqp)) cs

-- | Create a projectile actor containing the given missile.
addProjectile :: (MonadAtomic m, MonadServer m)
              => Point -> [Point] -> ItemId -> LevelId -> FactionId -> Time
              -> m ActorId
addProjectile bpos rest iid blid bfid btime = do
  Kind.COps{ coactor=coactor@Kind.Ops{okind}
           , coitem=coitem@Kind.Ops{okind=iokind} } <- getsState scops
  disco <- getsServer sdisco
  item <- getsState $ getItemBody iid
  let lingerPercent = isLingering coitem disco item
      ik = iokind (fromJust $ jkind disco item)
      speed = speedFromWeight (jweight item) (itoThrow ik)
      range = rangeFromSpeed speed
      adj | range < 5 = "falling"
          | otherwise = "flying"
      -- Not much detail about a fast flying item.
      (object1, object2) = partItem coitem EM.empty item
      name = makePhrase [MU.AW $ MU.Text adj, object1, object2]
      trajectoryLength = lingerPercent * range `div` 100
      dirTrajectory = take trajectoryLength $ pathToTrajectory (bpos : rest)
      kind = okind $ projectileKindId coactor
      m = actorTemplate (projectileKindId coactor) (asymbol kind) name
                        (acolor kind) speed 0 maxBound (Just dirTrajectory)
                        bpos blid btime bfid True
  acounter <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ acounter}
  execUpdAtomic $ UpdCreateActor acounter m [(iid, item)]
  return $! acounter
