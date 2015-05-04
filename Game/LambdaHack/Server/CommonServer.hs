{-# LANGUAGE TupleSections #-}
-- | Server operations common to many modules.
module Game.LambdaHack.Server.CommonServer
  ( execFailure, resetFidPerception, resetLitInDungeon, getPerFid
  , revealItems, moveStores, deduceQuits, deduceKilled, electLeader
  , addActor, addActorIid, projectFail
  , pickWeaponServer, sumOrganEqpServer, actorSkillsServer
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import qualified Text.Show.Pretty as Show.Pretty

import Game.LambdaHack.Atomic
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
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
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
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
      impossible = impossibleReqFailure failureSer
      debugShow :: Show a => a -> Text
      debugShow = T.pack . Show.Pretty.ppShow
      possiblyAlarm = if impossible
                      then debugPossiblyPrintAndExit
                      else debugPossiblyPrint
  possiblyAlarm $
    "execFailure:" <+> msg <> "\n"
    <> debugShow body <> "\n" <> debugShow req
  execSfxAtomic $ SfxMsgFid fid $ "Unexpected problem:" <+> msg <> "."
    -- TODO: --more--, but keep in history

-- | Update the cached perception for the selected level, for a faction.
-- The assumption is the level, and only the level, has changed since
-- the previous perception calculation.
resetFidPerception :: MonadServer m
                   => PersLit -> FactionId -> LevelId
                   -> m Perception
resetFidPerception persLit fid lid = do
  sfovMode <- getsServer $ sfovMode . sdebugSer
  lvl <- getLevel lid
  let fovMode = fromMaybe Digital sfovMode
      per = fidLidPerception fovMode persLit fid lid lvl
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
  let failFact = assert `failure` "no perception for faction" `twith` (lid, fid)
      fper = EM.findWithDefault failFact fid pers
      failLvl = assert `failure` "no perception for level" `twith` (lid, fid)
      per = EM.findWithDefault failLvl lid fper
  return $! per

revealItems :: (MonadAtomic m, MonadServer m)
            => Maybe FactionId -> Maybe (ActorId, Actor) -> m ()
revealItems mfid mbody = do
  let !_A = assert (maybe True (not . bproj . snd) mbody) ()
  itemToF <- itemToFullServer
  dungeon <- getsState sdungeon
  let discover aid store iid k =
        let itemFull = itemToF iid k
            c = CActor aid store
        in case itemDisco itemFull of
          Just ItemDisco{itemKindId} -> do
            seed <- getsServer $ (EM.! iid) . sitemSeedD
            Level{ldepth} <- getLevel $ jlid $ itemBase itemFull
            execUpdAtomic $ UpdDiscover c iid itemKindId seed ldepth
          _ -> assert `failure` (mfid, mbody, c, iid, itemFull)
      f aid = do
        b <- getsState $ getActorBody aid
        let ourSide = maybe True (== bfid b) mfid
        -- Don't ID projectiles, because client may not see them.
        when (not (bproj b) && ourSide) $
          -- CSha is IDed for each actor of each faction, which is OK,
          -- even though it may introduce a slight lag.
          -- AI clients being sent this is a bigger waste anyway.
          join $ getsState $ mapActorItems_ (discover aid) b
  mapDungeonActors_ f dungeon
  maybe (return ())
        (\(aid, b) -> join $ getsState $ mapActorItems_ (discover aid) b)
        mbody

moveStores :: (MonadAtomic m, MonadServer m)
           => ActorId -> CStore -> CStore -> m ()
moveStores aid fromStore toStore = do
  b <- getsState $ getActorBody aid
  let g iid (k, _) = execUpdAtomic $ UpdMoveItem iid k aid fromStore toStore
  mapActorCStore_ fromStore g b

quitF :: (MonadAtomic m, MonadServer m)
      => Maybe (ActorId, Actor) -> Status -> FactionId -> m ()
quitF mbody status fid = do
  let !_A = assert (maybe True ((fid ==) . bfid . snd) mbody) ()
  fact <- getsState $ (EM.! fid) . sfactionD
  let oldSt = gquit fact
  case stOutcome <$> oldSt of
    Just Killed -> return ()    -- Do not overwrite in case
    Just Defeated -> return ()  -- many things happen in 1 turn.
    Just Conquer -> return ()
    Just Escape -> return ()
    _ -> do
      when (fhasUI $ gplayer fact) $ do
        keepAutomated <- getsServer $ skeepAutomated . sdebugSer
        when (isAIFact fact
              && fleaderMode (gplayer fact) /= LeaderNull
              && not keepAutomated) $
          execUpdAtomic $ UpdAutoFaction fid False
        revealItems (Just fid) mbody
        registerScore status (snd <$> mbody) fid
      execUpdAtomic $ UpdQuitFaction fid (snd <$> mbody) oldSt $ Just status  -- TODO: send only aid to UpdQuitFaction and elsewhere --- aid is alive
      modifyServer $ \ser -> ser {squit = True}  -- end turn ASAP

-- Send any QuitFactionA actions that can be deduced from their current state.
deduceQuits :: (MonadAtomic m, MonadServer m)
            => FactionId -> Maybe (ActorId, Actor) -> Status -> m ()
deduceQuits fid mbody status@Status{stOutcome}
  | stOutcome `elem` [Defeated, Camping, Restart, Conquer] =
    assert `failure` "no quitting to deduce" `twith` (fid, mbody, status)
deduceQuits fid mbody status = do
  let mapQuitF statusF fids = mapM_ (quitF Nothing statusF) $ delete fid fids
  quitF mbody status fid
  let inGameOutcome (_, fact) = case stOutcome <$> gquit fact of
        Just Killed -> False
        Just Defeated -> False
        Just Restart -> False  -- effectively, commits suicide
        _ -> True
  factionD <- getsState sfactionD
  let assocsInGame = filter inGameOutcome $ EM.assocs factionD
      keysInGame = map fst assocsInGame
      assocsKeepArena = filter (keepArenaFact . snd) assocsInGame
      assocsUI = filter (fhasUI . gplayer . snd) assocsInGame
      nonHorrorAIG = filter (not . isHorrorFact . snd) assocsInGame
      worldPeace =
        all (\(fid1, _) -> all (\(_, fact2) -> not $ isAtWar fact2 fid1)
                           nonHorrorAIG)
        nonHorrorAIG
  case assocsKeepArena of
    _ | null assocsUI ->
      -- Only non-UI players left in the game and they all win.
      mapQuitF status{stOutcome=Conquer} keysInGame
    [] ->
      -- Only leaderless and spawners remain (the latter may sometimes
      -- have no leader, just as the former), so they win,
      -- or we could get stuck in a state with no active arena and so no spawns.
      mapQuitF status{stOutcome=Conquer} keysInGame
    _ | worldPeace ->
      -- Nobody is at war any more, so all win (e.g., horrors, but never mind).
      mapQuitF status{stOutcome=Conquer} keysInGame
    _ | stOutcome status == Escape -> do
      -- Otherwise, in a game with many warring teams alive,
      -- only complete Victory matters, until enough of them die.
      let (victors, losers) =
            partition (flip isAllied fid . snd) assocsInGame
      mapQuitF status{stOutcome=Escape} $ map fst victors
      mapQuitF status{stOutcome=Defeated} $ map fst losers
    _ -> return ()

-- | Tell whether a faction that we know is still in game, keeps arena.
-- Keeping arena means, if the faction is still in game,
-- it always has a leader in the dungeon somewhere.
-- So, leaderless factions and spawner factions do not keep an arena,
-- even though the latter usually has a leader for most of the game.
keepArenaFact :: Faction -> Bool
keepArenaFact fact = fleaderMode (gplayer fact) /= LeaderNull
                     && fneverEmpty (gplayer fact)

-- We assume the actor in the second argumet is dead or dominated
-- by this point. Even if the actor is to be dominated,
-- @bfid@ of the actor body is still the old faction.
deduceKilled :: (MonadAtomic m, MonadServer m)
             => ActorId -> Actor -> m ()
deduceKilled aid body = do
  Kind.COps{corule} <- getsState scops
  let firstDeathEnds = rfirstDeathEnds $ Kind.stdRuleset corule
      fid = bfid body
  fact <- getsState $ (EM.! fid) . sfactionD
  when (fneverEmpty $ gplayer fact) $ do
    actorsAlive <- anyActorsAlive fid (Just aid)
    when (not actorsAlive || firstDeathEnds) $
      deduceQuits fid (Just (aid, body))
      $ Status Killed (fromEnum $ blid body) Nothing

anyActorsAlive :: MonadServer m => FactionId -> Maybe ActorId -> m Bool
anyActorsAlive fid maid = do
  fact <- getsState $ (EM.! fid) . sfactionD
  if fleaderMode (gplayer fact) /= LeaderNull
    then return $! isJust $ gleader fact
    else do
      as <- getsState $ fidActorNotProjAssocs fid
      return $! not $ null $ maybe as (\aid -> filter ((/= aid) . fst) as) maid

electLeader :: MonadAtomic m => FactionId -> LevelId -> ActorId -> m ()
electLeader fid lid aidDead = do
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  when (isNothing mleader || fmap fst mleader == Just aidDead) $ do
    actorD <- getsState sactorD
    let ours (_, b) = bfid b == fid && not (bproj b)
        party = filter ours $ EM.assocs actorD
    onLevel <- getsState $ actorRegularAssocs (== fid) lid
    let mleaderNew = case filter (/= aidDead) $ map fst $ onLevel ++ party of
          [] -> Nothing
          aid : _ -> Just (aid, Nothing)
    unless (mleader == mleaderNew) $
      execUpdAtomic $ UpdLeadFaction fid mleader mleaderNew

projectFail :: (MonadAtomic m, MonadServer m)
            => ActorId    -- ^ actor projecting the item (is on current lvl)
            -> Point      -- ^ target position of the projectile
            -> Int        -- ^ digital line parameter
            -> ItemId     -- ^ the item to be projected
            -> CStore     -- ^ whether the items comes from floor or inventory
            -> Bool       -- ^ whether the item is a blast
            -> m (Maybe ReqFailure)
projectFail source tpxy eps iid cstore isBlast = do
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
      bag <- getsState $ getActorBag source cstore
      case EM.lookup iid bag of
        Nothing ->  return $ Just ProjectOutOfReach
        Just kit -> do
          itemToF <- itemToFullServer
          activeItems <- activeItemsServer source
          actorSk <- actorSkillsServer source
          let skill = EM.findWithDefault 0 Ability.AbProject actorSk
              itemFull@ItemFull{itemBase} = itemToF iid kit
              forced = isBlast || bproj sb
              legal = permittedProject " " forced skill itemFull sb activeItems
          case legal of
            Left reqFail ->  return $ Just reqFail
            Right _ -> do
              let fragile = IK.Fragile `elem` jfeature itemBase
                  rest = if fragile
                         then take (chessDist spos tpxy - 1) restUnlimited
                         else restUnlimited
                  t = lvl `at` pos
              if not $ Tile.isWalkable cotile t
                then return $ Just ProjectBlockTerrain
                else do
                  lab <- getsState $ posToActors pos lid
                  if not $ all (bproj . snd) lab
                    then if isBlast && bproj sb then do
                           -- Hit the blocking actor.
                           projectBla source spos (pos:rest) iid cstore isBlast
                           return Nothing
                         else return $ Just ProjectBlockActor
                    else do
                      if isBlast && bproj sb && eps `mod` 2 == 0 then
                        -- Make the explosion a bit less regular.
                        projectBla source spos (pos:rest) iid cstore isBlast
                      else
                        projectBla source pos rest iid cstore isBlast
                      return Nothing


projectBla :: (MonadAtomic m, MonadServer m)
           => ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ starting point of the projectile
           -> [Point]    -- ^ rest of the trajectory of the projectile
           -> ItemId     -- ^ the item to be projected
           -> CStore     -- ^ whether the items comes from floor or inventory
           -> Bool       -- ^ whether the item is a blast
           -> m ()
projectBla source pos rest iid cstore isBlast = do
  sb <- getsState $ getActorBody source
  item <- getsState $ getItemBody iid
  let lid = blid sb
  localTime <- getsState $ getLocalTime lid
  unless isBlast $ execSfxAtomic $ SfxProject source iid cstore
  bag <- getsState $ getActorBag source cstore
  case iid `EM.lookup` bag of
    Nothing -> assert `failure` (source, pos, rest, iid, cstore)
    Just kit@(_, it) -> do
      addProjectile pos rest iid kit lid (bfid sb) localTime isBlast
      let c = CActor source cstore
      execUpdAtomic $ UpdLoseItem iid item (1, take 1 it) c

-- | Create a projectile actor containing the given missile.
--
-- Projectile has no organs except for the trunk.
addProjectile :: (MonadAtomic m, MonadServer m)
              => Point -> [Point] -> ItemId -> ItemQuant -> LevelId
              -> FactionId -> Time -> Bool
              -> m ()
addProjectile bpos rest iid (_, it) blid bfid btime isBlast = do
  localTime <- getsState $ getLocalTime blid
  itemToF <- itemToFullServer
  let itemFull@ItemFull{itemBase} = itemToF iid (1, take 1 it)
      (trajectory, (speed, trange)) = itemTrajectory itemBase (bpos : rest)
      adj | trange < 5 = "falling"
          | otherwise = "flying"
      -- Not much detail about a fast flying item.
      (_, object1, object2) = partItem CInv localTime
                                       (itemNoDisco (itemBase, 1))
      bname = makePhrase [MU.AW $ MU.Text adj, object1, object2]
      tweakBody b = b { bsymbol = if isBlast then bsymbol b else '*'
                      , bcolor = if isBlast then bcolor b else Color.BrWhite
                      , bname
                      , bhp = 1
                      , bproj = True
                      , btrajectory = Just (trajectory, speed)
                      , beqp = EM.singleton iid (1, take 1 it)
                      , borgan = EM.empty}
      bpronoun = "it"
  void $ addActorIid iid itemFull
                     True bfid bpos blid tweakBody bpronoun btime

addActor :: (MonadAtomic m, MonadServer m)
         => GroupName ItemKind -> FactionId -> Point -> LevelId
         -> (Actor -> Actor) -> Text -> Time
         -> m (Maybe ActorId)
addActor actorGroup bfid pos lid tweakBody bpronoun time = do
  -- We bootstrap the actor by first creating the trunk of the actor's body
  -- contains the constant properties.
  let trunkFreq = [(actorGroup, 1)]
  m2 <- rollAndRegisterItem lid trunkFreq (CTrunk bfid lid pos) False Nothing
  case m2 of
    Nothing -> return Nothing
    Just (trunkId, (trunkFull, _)) ->
      addActorIid trunkId trunkFull False bfid pos lid tweakBody bpronoun time

addActorIid :: (MonadAtomic m, MonadServer m)
            => ItemId -> ItemFull -> Bool -> FactionId -> Point -> LevelId
            -> (Actor -> Actor) -> Text -> Time
            -> m (Maybe ActorId)
addActorIid trunkId trunkFull@ItemFull{..} bproj
            bfid pos lid tweakBody bpronoun time = do
  let trunkKind = case itemDisco of
        Just ItemDisco{itemKind} -> itemKind
        Nothing -> assert `failure` trunkFull
  -- Initial HP and Calm is based only on trunk and ignores organs.
  let hp = xM (max 2 $ sumSlotNoFilter IK.EqpSlotAddMaxHP [trunkFull])
           `div` 2
      calm = xM $ max 1
             $ sumSlotNoFilter IK.EqpSlotAddMaxCalm [trunkFull]
  -- Create actor.
  factionD <- getsState sfactionD
  let factMine = factionD EM.! bfid
  DebugModeSer{scurDiff} <- getsServer sdebugSer
  nU <- nUI
  -- If difficulty is below standard, HP is added to the UI factions,
  -- otherwise HP is added to their enemies.
  -- If no UI factions, their role is taken by the escapees (for testing).
  let diffBonusCoeff = difficultyCoeff scurDiff
      hasUIorEscapes Faction{gplayer} =
        fhasUI gplayer || nU == 0 && fcanEscape gplayer
      boostFact = not bproj
                  && if diffBonusCoeff > 0
                     then hasUIorEscapes factMine
                          || any hasUIorEscapes
                                 (filter (`isAllied` bfid) $ EM.elems factionD)
                     else any hasUIorEscapes
                              (filter (`isAtWar` bfid) $ EM.elems factionD)
      diffHP | boostFact = hp * 2 ^ abs diffBonusCoeff
             | otherwise = hp
      bonusHP = fromIntegral $ (diffHP - hp) `divUp` oneM
      healthOrgans = [(Just bonusHP, ("bonus HP", COrgan)) | bonusHP /= 0]
      bsymbol = jsymbol itemBase
      bname = IK.iname trunkKind
      bcolor = flavourToColor $ jflavour itemBase
      b = actorTemplate trunkId bsymbol bname bpronoun bcolor diffHP calm
                        pos lid time bfid
      -- Insert the trunk as the actor's organ.
      withTrunk = b {borgan = EM.singleton trunkId (itemK, itemTimer)}
  aid <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ aid}
  execUpdAtomic $ UpdCreateActor aid (tweakBody withTrunk) [(trunkId, itemBase)]
  -- Create, register and insert all initial actor items, including
  -- the bonus health organs from difficulty setting.
  forM_ (healthOrgans ++ map (Nothing,) (IK.ikit trunkKind))
        $ \(mk, (ikText, cstore)) -> do
    let container = CActor aid cstore
        itemFreq = [(ikText, 1)]
    mIidEtc <- rollAndRegisterItem lid itemFreq container False mk
    case mIidEtc of
      Nothing -> assert `failure` (lid, itemFreq, container, mk)
      Just (_, (ItemFull{itemDisco=
                  Just ItemDisco{itemAE=
                  Just ItemAspectEffect{jeffects=_:_}}}, _)) ->
        return ()  -- discover by use
      Just (iid, (ItemFull{itemBase=itemBase2}, _)) -> do
        seed <- getsServer $ (EM.! iid) . sitemSeedD
        Level{ldepth} <- getLevel $ jlid itemBase2
        execUpdAtomic $ UpdDiscoverSeed container iid seed ldepth
  return $ Just aid

-- Server has to pick a random weapon or it could leak item discovery
-- information. In case of non-projectiles, it only picks items
-- with some effects, though, so it leaks properties of completely
-- unidentified items.
pickWeaponServer :: MonadServer m => ActorId -> m (Maybe (ItemId, CStore))
pickWeaponServer source = do
  eqpAssocs <- fullAssocsServer source [CEqp]
  bodyAssocs <- fullAssocsServer source [COrgan]
  actorSk <- actorSkillsServer source
  sb <- getsState $ getActorBody source
  localTime <- getsState $ getLocalTime (blid sb)
  -- For projectiles we need to accept even items without any effect,
  -- so that the projectile dissapears and "No effect" feedback is produced.
  let allAssocs = eqpAssocs ++ bodyAssocs
      calm10 = calmEnough10 sb $ map snd allAssocs
      forced = bproj sb
      permitted = permittedPrecious calm10 forced
      legalPrecious = either (const False) (const True) . permitted
      preferredPrecious = either (const False) id . permitted
      strongest = strongestMelee True localTime allAssocs
      strongestLegal = filter (legalPrecious . snd . snd) strongest
      strongestPreferred = filter (preferredPrecious . snd . snd) strongestLegal
      best = case strongestPreferred of
        _ | bproj sb -> map (1,) eqpAssocs
        _ | EM.findWithDefault 0 Ability.AbMelee actorSk <= 0 -> []
        _:_ -> strongestPreferred
        [] -> strongestLegal
  case best of
    [] -> return Nothing
    iis@((maxS, _) : _) -> do
      let maxIis = map snd $ takeWhile ((== maxS) . fst) iis
      (iid, _) <- rndToAction $ oneOf maxIis
      let cstore = if isJust (lookup iid bodyAssocs) then COrgan else CEqp
      return $ Just (iid, cstore)

sumOrganEqpServer :: MonadServer m
                 => IK.EqpSlot -> ActorId -> m Int
sumOrganEqpServer eqpSlot aid = do
  activeAssocs <- activeItemsServer aid
  return $! sumSlotNoFilter eqpSlot activeAssocs

actorSkillsServer :: MonadServer m => ActorId -> m Ability.Skills
actorSkillsServer aid  = do
  activeItems <- activeItemsServer aid
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let mleader = fst <$> gleader fact
  getsState $ actorSkills mleader aid activeItems
