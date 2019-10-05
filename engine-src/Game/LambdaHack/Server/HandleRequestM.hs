-- | Semantics of requests
-- .
-- A couple of them do not take time, the rest does.
-- Note that since the results are atomic commands, which are executed
-- only later (on the server and some of the clients), all condition
-- are checkd by the semantic functions in the context of the state
-- before the server command. Even if one or more atomic actions
-- are already issued by the point an expression is evaluated, they do not
-- influence the outcome of the evaluation.
module Game.LambdaHack.Server.HandleRequestM
  ( handleRequestAI, handleRequestUI, handleRequestTimed, switchLeader
  , reqMoveGeneric, reqDisplaceGeneric, reqAlterFail
  , reqGameDropAndExit, reqGameSaveAndExit
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , execFailure, checkWaiting, processWatchfulness, managePerRequest
  , handleRequestTimedCases, affectSmell, affectStash
  , reqMove, reqMelee, reqMeleeChecked, reqDisplace, reqAlter
  , reqWait, reqWait10, reqYell, reqMoveItems, reqMoveItem, reqProject, reqApply
  , reqGameRestart, reqGameSave, reqDoctrine, reqAutomate
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import qualified Text.Show.Pretty as Show.Pretty

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Client (ReqAI (..), ReqUI (..),
                                         RequestTimed (..))
import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Analytics
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Server.CommonM
import           Game.LambdaHack.Server.HandleEffectM
import           Game.LambdaHack.Server.ItemM
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.PeriodicM
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

execFailure :: MonadServerAtomic m
            => ActorId -> RequestTimed -> ReqFailure -> m ()
execFailure aid req failureSer = do
  -- Clients should rarely do that (only in case of invisible actors)
  -- so we report it to the client, but do not crash
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
    "Server: execFailure:" <+> msg <> "\n"
    <> debugShow body <> "\n" <> debugShow req <> "\n" <> debugShow failureSer
  execSfxAtomic $ SfxMsgFid fid $ SfxUnexpected failureSer

-- | The semantics of server commands.
-- AI always takes time and so doesn't loop.
handleRequestAI :: MonadServerAtomic m
                => ReqAI
                -> m (Maybe RequestTimed)
handleRequestAI cmd = case cmd of
  ReqAITimed cmdT -> return $ Just cmdT
  ReqAINop -> return Nothing

-- | The semantics of server commands. Only the first two cases affect time.
handleRequestUI :: MonadServerAtomic m
                => FactionId -> ActorId -> ReqUI
                -> m (Maybe RequestTimed)
handleRequestUI fid aid cmd = case cmd of
  ReqUITimed cmdT -> return $ Just cmdT
  ReqUIGameRestart t d -> reqGameRestart aid t d >> return Nothing
  ReqUIGameDropAndExit -> reqGameDropAndExit aid >> return Nothing
  ReqUIGameSaveAndExit -> reqGameSaveAndExit aid >> return Nothing
  ReqUIGameSave -> reqGameSave >> return Nothing
  ReqUIDoctrine toT -> reqDoctrine fid toT >> return Nothing
  ReqUIAutomate -> reqAutomate fid >> return Nothing
  ReqUINop -> return Nothing

checkWaiting :: RequestTimed -> Maybe Bool
checkWaiting cmd = case cmd of
  ReqWait -> Just True  -- true wait, with bracing, no overhead, etc.
  ReqWait10 -> Just False  -- false wait, only one clip at a time
  _ -> Nothing

-- | This is a shorthand. Instead of setting @bwatch@ in @ReqWait@
-- and unsetting in all other requests, we call this once after
-- executing a request.
-- In game state, we collect the number of server requests pertaining
-- to the actor (the number of actor's "moves"), through which
-- the actor was waiting.
processWatchfulness :: MonadServerAtomic m => Maybe Bool -> ActorId -> m ()
processWatchfulness mwait aid = do
  b <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  let uneasy = deltasSerious (bcalmDelta b) || not (calmEnough b actorMaxSk)
  case bwatch b of
    WSleep ->
      if mwait /= Just False  -- lurk can't wake up regardless; too short
         && (not (isJust mwait)  -- not a wait
             || uneasy  -- spooked
             || not (deltaBenign $ bhpDelta b))  -- any HP lost
      then execUpdAtomic $ UpdWaitActor aid WSleep WWake
      else execUpdAtomic $ UpdRefillHP aid 10000
             -- no @xM@, so slow, but each turn HP gauge green;
             -- this is 1HP per 100 turns, so it's 10 slower than a necklace
             -- that gives 1HP per 10 turns;
             -- so if an actor sleeps for the duration of a 1000 turns,
             -- which may be the time it takes to fully explore a level,
             -- 10HP would be gained, so weak actors would wake up
    WWake -> unless (mwait == Just False) $  -- lurk can't wake up; too fast
      removeSleepSingle aid
    WWait 0 -> case mwait of  -- actor couldn't brace last time
      Just True -> return ()  -- if he still waits, keep him stuck unbraced
      _ -> execUpdAtomic $ UpdWaitActor aid (WWait 0) WWatch
    WWait n -> case mwait of
      Just True ->  -- only proper wait prevents switching to watchfulness
        if n >= 500 then  -- enough dozing to fall asleep
          if not uneasy  -- won't wake up at once
             && canSleep actorMaxSk  -- enough skills
          then do
            nAll <- removeConditionSingle "braced" aid
            let !_A = assert (nAll == 0) ()
            addSleep aid
          else
            -- Start dozing from scratch to prevent hopeless skill checks.
            execUpdAtomic $ UpdWaitActor aid (WWait n) (WWait 1)
        else
          -- Doze some more before checking sleep eligibility.
          execUpdAtomic $ UpdWaitActor aid (WWait n) (WWait $ n + 1)
      _ -> do
        nAll <- removeConditionSingle "braced" aid
        let !_A = assert (nAll == 0) ()
        execUpdAtomic $ UpdWaitActor aid (WWait n) WWatch
    WWatch ->
      when (mwait == Just True) $  -- only long wait switches to wait state
        if Ability.getSk Ability.SkWait actorMaxSk >= 2 then do
          addCondition False "braced" aid
          execUpdAtomic $ UpdWaitActor aid WWatch (WWait 1)
        else
          execUpdAtomic $ UpdWaitActor aid WWatch (WWait 0)

handleRequestTimed :: MonadServerAtomic m
                   => FactionId -> ActorId -> RequestTimed -> m Bool
handleRequestTimed fid aid cmd = do
  let mwait = checkWaiting cmd
  b <- getsState $ getActorBody aid
  -- Note that only the ordinary 1-turn wait eliminates overhead.
  -- The more fine-graned waits don't make actors braced and induce
  -- overhead, so that they have some drawbacks in addition to the
  -- benefit of seeing approaching danger up to almost a turn faster.
  -- It may be too late to block then, but not too late to sidestep or attack.
  unless (mwait == Just True) $ overheadActorTime fid (blid b)
  advanceTime aid (if mwait == Just False then 10 else 100) True
  handleRequestTimedCases aid cmd
  managePerRequest aid
  -- Note that due to the order, actor was still braced or sleeping
  -- throughout request processing, etc. So, if he hits himself kinetically,
  -- his armor from bracing previous turn is still in effect.
  processWatchfulness mwait aid
  return $! isNothing mwait  -- for speed, we report if @cmd@ harmless

-- | Clear deltas for Calm and HP for proper UI display and AI hints.
managePerRequest :: MonadServerAtomic m => ActorId -> m ()
managePerRequest aid = do
  b <- getsState $ getActorBody aid
  let clearMark = 0
  unless (bcalmDelta b == ResDelta (0, 0) (0, 0)) $
    -- Clear delta for the next actor move.
    execUpdAtomic $ UpdRefillCalm aid clearMark
  unless (bhpDelta b == ResDelta (0, 0) (0, 0)) $
    -- Clear delta for the next actor move.
    execUpdAtomic $ UpdRefillHP aid clearMark

handleRequestTimedCases :: MonadServerAtomic m
                        => ActorId -> RequestTimed -> m ()
handleRequestTimedCases aid cmd = case cmd of
  ReqMove target -> reqMove aid target
  ReqMelee target iid cstore -> reqMelee aid target iid cstore
  ReqDisplace target -> reqDisplace aid target
  ReqAlter tpos -> reqAlter aid tpos
  ReqWait -> reqWait aid
  ReqWait10 -> reqWait10 aid
  ReqYell -> reqYell aid
  ReqMoveItems l -> reqMoveItems aid l
  ReqProject p eps iid cstore -> reqProject aid p eps iid cstore
  ReqApply iid cstore -> reqApply aid iid cstore

switchLeader :: MonadServerAtomic m => FactionId -> ActorId -> m ()
{-# INLINE switchLeader #-}
switchLeader fid aidNew = do
  fact <- getsState $ (EM.! fid) . sfactionD
  bPre <- getsState $ getActorBody aidNew
  let mleader = gleader fact
      !_A1 = assert (Just aidNew /= mleader
                     && not (bproj bPre)
                     `blame` (aidNew, bPre, fid, fact)) ()
      !_A2 = assert (bfid bPre == fid
                     `blame` "client tries to move other faction actors"
                     `swith` (aidNew, bPre, fid, fact)) ()
  let (autoDun, _) = autoDungeonLevel fact
  arena <- case mleader of
    Nothing -> return $! blid bPre
    Just leader -> do
      b <- getsState $ getActorBody leader
      return $! blid b
  if | blid bPre /= arena && autoDun ->
       execFailure aidNew ReqWait{-hack-} NoChangeDunLeader
     | otherwise -> do
       execUpdAtomic $ UpdLeadFaction fid mleader (Just aidNew)
     -- We exchange times of the old and new leader.
     -- This permits an abuse, because a slow tank can be moved fast
     -- by alternating between it and many fast actors (until all of them
     -- get slowed down by this and none remain). But at least the sum
     -- of all times of a faction is conserved. And we avoid double moves
     -- against the UI player caused by his leader changes. There may still
     -- happen double moves caused by AI leader changes, but that's rare.
     -- The flip side is the possibility of multi-moves of the UI player
     -- as in the case of the tank.
     -- Warning: when the action is performed on the server,
     -- the time of the actor is different than when client prepared that
     -- action, so any client checks involving time should discount this.
       case mleader of
         Just aidOld | aidOld /= aidNew -> swapTime aidOld aidNew
         _ -> return ()

-- * ReqMove

-- | Add a smell trace for the actor to the level. If smell already there
-- and the actor can smell, remove smell. Projectiles are ignored.
-- As long as an actor can smell, he doesn't leave any smell ever.
-- Smell trace is never left in water tiles.
affectSmell :: MonadServerAtomic m => ActorId -> m ()
affectSmell aid = do
  COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let aquatic = Tile.isAquatic coTileSpeedup $ lvl `at` bpos b
  unless (bproj b || aquatic) $ do
    actorMaxSk <- getsState $ getActorMaxSkills aid
    let smellRadius = Ability.getSk Ability.SkSmell actorMaxSk
        hasOdor = Ability.getSk Ability.SkOdor actorMaxSk > 0
    when (hasOdor || smellRadius > 0) $ do
      localTime <- getsState $ getLocalTime $ blid b
      let oldS = fromMaybe timeZero $ EM.lookup (bpos b) . lsmell $ lvl
          newTime = timeShift localTime smellTimeout
          newS = if smellRadius > 0
                 then timeZero
                 else newTime
      when (oldS /= newS) $
        execUpdAtomic $ UpdAlterSmell (blid b) (bpos b) oldS newS

affectStash :: MonadServerAtomic m => ActorId -> Point -> m ()
affectStash aid tpos = do
  b <- getsState $ getActorBody aid
  actorSk <- currentSkillsServer aid
  let abInSkill sk = isJust (btrajectory b)
                     || Ability.getSk sk actorSk > 0
  when (abInSkill Ability.SkMoveItem) $ do
    let locateStash (fid, fact) = case gstash fact of
          Just (lidS, posS)
            | lidS == blid b && posS == tpos && fid /= bfid b ->
              execUpdAtomic $ UpdLoseStashFaction True fid lidS posS
          _ -> return ()
    factionD <- getsState sfactionD
    mapM_ locateStash $ EM.assocs factionD

-- | Actor moves or attacks.
-- Note that client may not be able to see an invisible monster
-- so it's the server that determines if melee took place, etc.
-- Also, only the server is authorized to check if a move is legal
-- and it needs full context for that, e.g., the initial actor position
-- to check if melee attack does not try to reach to a distant tile.
reqMove :: MonadServerAtomic m => ActorId -> Vector -> m ()
reqMove = reqMoveGeneric True True

reqMoveGeneric :: MonadServerAtomic m
               => Bool -> Bool -> ActorId -> Vector -> m ()
reqMoveGeneric voluntary mayAttack source dir = do
  COps{coTileSpeedup} <- getsState scops
  actorSk <- currentSkillsServer source
  sb <- getsState $ getActorBody source
  let abInSkill sk = isJust (btrajectory sb)
                     || Ability.getSk sk actorSk > 0
      lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb
      tpos = spos `shift` dir
  -- This predicate is symmetric wrt source and target, though the effect
  -- of collision may not be (the source projectiles applies its effect
  -- on the target particles, but loses 1 HP due to the collision).
  -- The condition implies that it's impossible to shoot down a bullet
  -- with a bullet, but a bullet can shoot down a burstable target,
  -- as well as be swept away by it, and two burstable projectiles
  -- burst when meeting mid-air. Projectiles that are not bursting
  -- nor damaging never collide with any projectile.
  collides <- getsState $ \s tb ->
    let sitemKind = getIidKindServer (btrunk sb) s
        titemKind = getIidKindServer (btrunk tb) s
        sar = sdiscoAspect s EM.! btrunk sb
        tar = sdiscoAspect s EM.! btrunk tb
        -- Such projectiles are prone to bursting or are themselves
        -- particles of an explosion shockwave.
        bursting arItem =
          IA.checkFlag Ability.Fragile arItem
          && IA.checkFlag Ability.Lobable arItem
        sbursting = bursting sar
        tbursting = bursting tar
        -- Such projectiles, even if not bursting themselves, can cause
        -- another projectile to burst.
        damaging itemKind = IK.idamage itemKind /= 0
        sdamaging = damaging sitemKind
        tdamaging = damaging titemKind
        -- Avoid explosion extinguishing itself via its own particles colliding.
        sameBlast = IA.checkFlag Ability.Blast sar
                    && getIidKindIdServer (btrunk sb) s
                       == getIidKindIdServer (btrunk tb) s
    in not sameBlast
       && (sbursting && (tdamaging || tbursting)
           || (tbursting && (sdamaging || sbursting)))
  -- We start by checking actors at the target position.
  tgt <- getsState $ posToAidAssocs tpos lid
  case tgt of
    (target, tb) : _ | mayAttack && (not (bproj sb)
                                     || not (bproj tb)
                                     || collides tb) -> do
      -- A projectile is too small and insubstantial to hit another projectile,
      -- unless it's large enough or tends to explode (fragile and lobable).
      -- The actor in the way is visible or not; server sees him always.
      -- Below the only weapon (the only item) of projectiles is picked.
      mweapon <- pickWeaponServer source
      case mweapon of
        Just (wp, cstore) | abInSkill Ability.SkMelee ->
          reqMeleeChecked voluntary source target wp cstore
        _ -> return ()  -- waiting, even if no @SkWait@ skill
      -- Movement of projectiles only happens after melee and a check
      -- if they survive, so that if they don't, they explode in front
      -- of enemy, not under him, so that already first explosion blasts
      -- reach him, not only potential secondary explosions.
      when (bproj sb) $ do
        b2 <- getsState $ getActorBody source
        unless (actorDying b2) $ reqMoveGeneric voluntary False source dir
    _ ->
      -- Either the position is empty, or all involved actors are proj.
      -- Movement requires full access and skill.
      if Tile.isWalkable coTileSpeedup $ lvl `at` tpos then
        if abInSkill Ability.SkMove then do
          execUpdAtomic $ UpdMoveActor source spos tpos
          affectSmell source
          affectStash source tpos
          void $ reqAlterFail False voluntary source tpos
            -- possibly alter or activate
       else execFailure source (ReqMove dir) MoveUnskilled
      else
        -- Client foolishly tries to move into unwalkable tile.
        execFailure source (ReqMove dir) MoveNothing

-- * ReqMelee

-- | Resolves the result of an actor moving into another.
-- Actors on unwalkable positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall can be attacked from
-- an adjacent position. This function is analogous to projectGroupItem,
-- but for melee and not using up the weapon.
-- No problem if there are many projectiles at the spot. We just
-- attack the one specified.
reqMelee :: MonadServerAtomic m
         => ActorId -> ActorId -> ItemId -> CStore -> m ()
reqMelee source target iid cstore = do
  actorSk <- currentSkillsServer source
  if Ability.getSk Ability.SkMelee actorSk > 0 then
    reqMeleeChecked True source target iid cstore
  else execFailure source (ReqMelee target iid cstore) MeleeUnskilled

reqMeleeChecked :: forall m. MonadServerAtomic m
                => Bool -> ActorId -> ActorId -> ItemId -> CStore -> m ()
reqMeleeChecked voluntary source target iid cstore = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let req = ReqMelee target iid cstore
  if source == target then execFailure source req MeleeSelf
  else if not (checkAdjacent sb tb) then execFailure source req MeleeDistant
  else do
    -- If @voluntary@ is set, blame is exact, otherwise, an approximation.
    killer <- if | voluntary -> assert (not (bproj sb)) $ return source
                 | bproj sb -> getsServer $ EM.findWithDefault source source
                               . strajPushedBy
                 | otherwise -> return source
    discoAspect <- getsState sdiscoAspect
    let arTrunk = discoAspect EM.! btrunk tb
        arWeapon = discoAspect EM.! iid
        sfid = bfid sb
        tfid = bfid tb
        -- Let the missile drop down, but don't remove its trajectory
        -- so that it doesn't pretend to have hit a wall.
        haltTrajectory :: KillHow -> ActorId -> Actor -> m ()
        haltTrajectory killHow aid b = case btrajectory b of
          btra@(Just (l, speed)) | not $ null l -> do
            execUpdAtomic $ UpdTrajectory aid btra $ Just ([], speed)
            let arTrunkAid = discoAspect EM.! btrunk b
            when (bproj b && not (IA.checkFlag Ability.Blast arTrunkAid)) $
              addKillToAnalytics killer killHow (bfid b) (btrunk b)
          _ -> return ()
    -- Only catch if braced. Never steal trunk from an already caught
    -- projectile or one with many items inside.
    if bproj tb
       && EM.size (beqp tb) == 1
       && not (IA.checkFlag Ability.Blast arTrunk)
       && actorWaits sb  -- still valid while request being processed
    then do
      -- Catching the projectile, that is, stealing the item from its eqp.
      -- No effect from our weapon (organ) is applied to the projectile
      -- and the weapon (organ) is never destroyed, even if not durable.
      -- Pushed actor doesn't stop flight by catching the projectile
      -- nor does he lose 1HP.
      -- This is not overpowered, because usually at least one partial wait
      -- is needed to sync (if not, attacker should switch missiles)
      -- and so only every other missile can be caught. Normal sidestepping
      -- or sync and displace, if in a corridor, is as effective
      -- and blocking can be even more so, depending on powers of the missile.
      -- Missiles are really easy to defend against, but sight (and so, Calm)
      -- is the key, as well as light, ambush around a corner, etc.
      execSfxAtomic $ SfxSteal source target iid
      case EM.assocs $ beqp tb of
        [(iid2, (k, _))] -> do
          upds <- generalMoveItem True iid2 k (CActor target CEqp)
                                              (CActor source CStash)
          mapM_ execUpdAtomic upds
          itemFull <- getsState $ itemToFull iid2
          discoverIfMinorEffects (CActor source CStash)
                                 iid2 (itemKindId itemFull)
        err -> error $ "" `showFailure` err
      haltTrajectory KillCatch target tb
    else do
      if bproj sb && bproj tb then do
        -- Special case for collision of projectiles, because they just
        -- symmetrically ram into each other, so picking one to hit another,
        -- based on random timing, would be wrong.
        -- Instead of suffering melee attack, let the target projectile
        -- get smashed and burst (if fragile and if not piercing).
        -- The source projectile terminates flight (unless pierces) later on.
        when (bhp tb > oneM) $
          execUpdAtomic $ UpdRefillHP target minusM
        when (bhp tb <= oneM) $ do
          -- If projectile has too low HP to pierce, terminate its flight.
          let killHow | IA.checkFlag Ability.Blast arWeapon = KillKineticBlast
                      | otherwise = KillKineticRanged
          haltTrajectory killHow target tb
        -- Avoid spam when two explosions collide.
        unless (IA.checkFlag Ability.Blast arWeapon
                && IA.checkFlag Ability.Blast arTrunk) $
          execSfxAtomic $ SfxStrike source target iid
      else do
        -- Normal hit, with effects. Msgs inside @SfxStrike@ describe
        -- the source part of the strike.
        execSfxAtomic $ SfxStrike source target iid
        let c = CActor source cstore
            mayDestroy = not (bproj sb) || bhp sb <= oneM
              -- piercing projectiles may not have their weapon destroyed
        -- Msgs inside @itemEffect@ describe the target part of the strike.
        -- If any effects and aspects, this is also where they are identified.
        -- Here also the kinetic damage is applied, before any effects are.
        --
        -- Note: that "hornet swarm detect items" via a scrolls is intentional,
        -- even though unrealistic and funny. Otherwise actors could protect
        -- themselves from some projectiles by lowering their apply stat.
        -- Also, the animal faction won't have too much benefit from that info,
        -- so the problem is not balance, but the goofy message.
        void $ kineticEffectAndDestroy False voluntary killer
                                       source target iid c mayDestroy
      sb2 <- getsState $ getActorBody source
      case btrajectory sb2 of
        Just{} -> do
          -- Deduct a hitpoint for a pierce of a projectile
          -- or due to a hurled actor colliding with another.
          -- Don't deduct if no pierce, to prevent spam.
          -- Never kill in this way.
          when (bhp sb2 > oneM) $ do
            execUpdAtomic $ UpdRefillHP source minusM
            unless (bproj sb2) $ do
              execSfxAtomic $
                SfxMsgFid (bfid sb2) $ SfxCollideActor (blid tb) source target
              unless (bproj tb) $
                execSfxAtomic $
                  SfxMsgFid (bfid tb) $ SfxCollideActor (blid tb) source target
          when (not (bproj sb2) || bhp sb2 <= oneM) $
            -- Non-projectiles can't pierce, so terminate their flight.
            -- If projectile has too low HP to pierce, ditto.
            haltTrajectory KillActorLaunch source sb2
        _ -> return ()
      -- The only way to start a war is to slap an enemy voluntarily..
      -- Being hit by and hitting projectiles, as well as via pushing,
      -- count as unintentional friendly fire.
      sfact <- getsState $ (EM.! sfid) . sfactionD
      let friendlyFire = bproj sb2 || bproj tb || not voluntary
          fromDipl = EM.findWithDefault Unknown tfid (gdipl sfact)
      unless (friendlyFire
              || isFoe sfid sfact tfid  -- already at war
              || isFriend sfid sfact tfid) $  -- allies never at war
        execUpdAtomic $ UpdDiplFaction sfid tfid fromDipl War

-- * ReqDisplace

-- | Actor tries to swap positions with another.
reqDisplace :: MonadServerAtomic m => ActorId -> ActorId -> m ()
reqDisplace = reqDisplaceGeneric True

reqDisplaceGeneric :: MonadServerAtomic m => Bool -> ActorId -> ActorId -> m ()
reqDisplaceGeneric voluntary source target = do
  COps{coTileSpeedup} <- getsState scops
  actorSk <- currentSkillsServer source
  sb <- getsState $ getActorBody source
  let abInSkill sk = isJust (btrajectory sb)
                     || Ability.getSk sk actorSk > 0
  tb <- getsState $ getActorBody target
  tfact <- getsState $ (EM.! bfid tb) . sfactionD
  let spos = bpos sb
      tpos = bpos tb
      atWar = isFoe (bfid tb) tfact (bfid sb)
      req = ReqDisplace target
  actorMaxSk <- getsState $ getActorMaxSkills target
  dEnemy <- getsState $ dispEnemy source target actorMaxSk
  if | not (abInSkill Ability.SkDisplace) ->
         execFailure source req DisplaceUnskilled
     | not (checkAdjacent sb tb) -> execFailure source req DisplaceDistant
     | atWar && not dEnemy -> do  -- if not at war, can displace always
       -- We don't fail with DisplaceImmobile and DisplaceSupported.
       -- because it's quite common they can't be determined by the attacker,
       -- and so the failure would be too alarming to the player.
       -- If the character melees instead, the player can tell displace failed.
       -- As for the other failures, they are impossible and we don't
       -- verify here that they don't occur, for simplicity.
       mweapon <- pickWeaponServer source
       case mweapon of
         Just (wp, cstore) | abInSkill Ability.SkMelee ->
           reqMeleeChecked voluntary source target wp cstore
         _ -> return ()  -- waiting, even if no @SkWait@ skill
     | otherwise -> do
       let lid = blid sb
       lvl <- getLevel lid
       -- Displacing requires full access.
       if Tile.isWalkable coTileSpeedup $ lvl `at` tpos then
         case posToAidsLvl tpos lvl of
           [] -> error $ "" `showFailure` (source, sb, target, tb)
           [_] -> do
             execUpdAtomic $ UpdDisplaceActor source target
             -- We leave or wipe out smell, for consistency, but it's not
             -- absolute consistency, e.g., blinking doesn't touch smell,
             -- so sometimes smellers will backtrack once to wipe smell. OK.
             affectSmell source
             affectSmell target
             affectStash source tpos
             affectStash target spos
             void $ reqAlterFail False voluntary source tpos
               -- possibly alter or activate
             void $ reqAlterFail False voluntary target spos
           _ -> execFailure source req DisplaceMultiple
       else
         -- Client foolishly tries to displace an actor without access.
         execFailure source req DisplaceAccess

-- * ReqAlter

-- | Search and/or alter the tile.
reqAlter :: MonadServerAtomic m => ActorId -> Point -> m ()
reqAlter source tpos = do
  mfail <- reqAlterFail False True source tpos
  let req = ReqAlter tpos
  maybe (return ()) (execFailure source req) mfail

reqAlterFail :: MonadServerAtomic m
             => Bool -> Bool -> ActorId -> Point -> m (Maybe ReqFailure)
reqAlterFail onCombineOnly voluntary source tpos = do
  cops@COps{cotile, coTileSpeedup} <- getsState scops
  sb <- getsState $ getActorBody source
  actorMaxSk <- getsState $ getActorMaxSkills source
  factionD <- getsState sfactionD
  let calmE = calmEnough sb actorMaxSk
      lid = blid sb
  sClient <- getsServer $ (EM.! bfid sb) . sclientStates
  itemToF <- getsState $ flip itemToFull
  actorSk <- currentSkillsServer source
  localTime <- getsState $ getLocalTime lid
  let alterSkill = Ability.getSk Ability.SkAlter actorSk
  embeds <- getsState $ getEmbedBag lid tpos
  lvl <- getLevel lid
  getKind <- getsState $ flip getIidKindServer
  let serverTile = lvl `at` tpos
      lvlClient = (EM.! lid) . sdungeon $ sClient
      clientTile = lvlClient `at` tpos
      hiddenTile = Tile.hideAs cotile serverTile
      revealEmbeds = unless (EM.null embeds) $
        execUpdAtomic $ UpdSpotItemBag (CEmbed lid tpos) embeds
      tryApplyEmbeds = do
        urs <- mapM tryApplyEmbed
                    (sortEmbeds cops getKind serverTile embeds)
        return $! case urs of
          [] -> UseDud  -- there was no effects
          _ -> maximum urs
      tryApplyEmbed (iid, kit) = do
        let itemFull = itemToF iid
            -- Let even completely apply-unskilled actors trigger basic embeds.
            -- See the note about no skill check when melee triggers effects.
            legal = permittedApply localTime maxBound calmE itemFull kit
            (object1, object2) = partItemShortest (bfid sb) factionD localTime
                                                  itemFull (1, [])
            name = makePhrase [object1, object2]
        case legal of
          Left ApplyNoEffects -> return UseDud  -- pure flavour embed
          Left reqFail -> do
            -- The failure is fully expected, because client may choose
            -- to trigger some embeds, knowing that others won't fire.
            execSfxAtomic $ SfxMsgFid (bfid sb) $
              SfxExpected ("embedded" <+> name) reqFail
            return UseDud
          _ -> itemEffectEmbedded onCombineOnly voluntary source lid tpos iid
      underFeet = tpos == bpos sb  -- if enter and alter, be more permissive
  if chessDist tpos (bpos sb) > 1
  then return $ Just AlterDistant
  else if Just clientTile == hiddenTile then  -- searches
    -- Only actors with SkAlter > 1 can search for hidden doors, etc.
    if not underFeet && alterSkill <= 1
    then return $ Just AlterUnskilled  -- don't leak about searching
    else do
      -- Blocking by items nor actors does not prevent searching.
      -- Searching broadcasted, in case actors from other factions are present
      -- so that they can learn the tile and learn our action.
      -- If they already know the tile, they will just consider our action
      -- a waste of time and ignore the command.
      execUpdAtomic $ UpdSearchTile source tpos serverTile
      -- Searching also reveals the embedded items of the tile.
      -- If the items are already seen by the client
      -- (e.g., due to item detection, despite tile being still hidden),
      -- the command is ignored on the client.
      revealEmbeds
      -- If the entries are already seen by the client
      -- the command is ignored on the client.
      case EM.lookup tpos $ lentry lvl of
        Nothing -> return ()
        Just entry -> execUpdAtomic $ UpdSpotEntry lid [(tpos, entry)]
      -- Seaching triggers the embeds as well, after they are revealed.
      -- The rationale is that the items were all the time present
      -- (just invisible to the client), so they need to be triggered.
      -- The exception is changable tiles, because they are not so easy
      -- to trigger; they need subsequent altering.
      unless (Tile.isDoor coTileSpeedup serverTile
              || Tile.isChangable coTileSpeedup serverTile
              || EM.null embeds) $ do
        -- Can't send @SfxTrigger@ afterwards, because actor may be moved
        -- by the embeds to another level, where @tpos@ is meaningless.
        execSfxAtomic $ SfxTrigger source tpos
        void $ tryApplyEmbeds
      return Nothing  -- success
  else if clientTile == serverTile then  -- alters
    if not underFeet && alterSkill < Tile.alterMinSkill coTileSpeedup serverTile
    then return $ Just AlterUnskilled  -- don't leak about altering
    else do
      let changeTo tgroup = do
            lvl2 <- getLevel lid
            -- No @SfxAlter@, because the effect is obvious (e.g., opened door).
            let nightCond kt = not (Tile.kindHasFeature TK.Walkable kt
                                    && Tile.kindHasFeature TK.Clear kt)
                               || (if lnight lvl2 then id else not)
                                    (Tile.kindHasFeature TK.Dark kt)
            -- Sometimes the tile is determined precisely by the ambient light
            -- of the source tiles. If not, default to cave day/night condition.
            mtoTile <- rndToAction $ opick cotile tgroup nightCond
            toTile <- maybe (rndToAction
                             $ fromMaybe (error $ "" `showFailure` tgroup)
                               <$> opick cotile tgroup (const True))
                            return
                            mtoTile
            unless (toTile == serverTile) $ do  -- don't regenerate same tile
              -- At most one of these two will be accepted on any given client.
              execUpdAtomic $ UpdAlterTile lid tpos serverTile toTile
              -- This case happens when a client does not see a searching
              -- action by another faction, but sees the subsequent altering.
              case hiddenTile of
                Just tHidden ->
                  execUpdAtomic $ UpdAlterTile lid tpos tHidden toTile
                Nothing -> return ()
              case (Tile.isExplorable coTileSpeedup serverTile,
                    Tile.isExplorable coTileSpeedup toTile) of
                (False, True) -> execUpdAtomic $ UpdAlterExplorable lid 1
                (True, False) -> execUpdAtomic $ UpdAlterExplorable lid (-1)
                _ -> return ()
              -- At the end we replace old embeds (even if partially used up)
              -- with new ones.
              -- If the source tile was hidden, the items could not be visible
              -- on a client, in which case the command would be ignored
              -- on the client, without causing any problems. Otherwise,
              -- if the position is in view, client has accurate info.
              case EM.lookup tpos (lembed lvl2) of
                Just bag -> execUpdAtomic $ UpdLoseItemBag (CEmbed lid tpos) bag
                Nothing -> return ()
              -- Altering always reveals the outcome tile, so it's not hidden
              -- and so its embedded items are always visible.
              embedItem lid tpos toTile
          durableFirst = sortOn $ not . IA.checkFlag Ability.Durable
                                  . aspectRecordFull . fst . snd
          tryChangeWith store (grps, tgroup) = do
            kitAss <- getsState $ durableFirst . kitAssocs source [store]
            case foldl' subtractGrpfromBag (Just (kitAss, EM.empty, [])) grps of
              Nothing -> return False
              Just (_, bagToLose, iidsToApply) -> do
                -- We don't invoke @OnSmash@ effects, so we avoid the risk
                -- of the first removed item displacing the actor, destroying
                -- or scattering some pending items ahead of time, etc.
                -- The embed should provide any requisite fireworks instead.
                unless (EM.null bagToLose) $
                  execUpdAtomic $ UpdLoseItemBag (CActor source store) bagToLose
                -- But afterwards we do apply normal effects of durable items,
                -- even if the actor or other items displaced in the process.
                let applyItemIfPresent iid = do
                      bag <- getsState $ getContainerBag (CActor source store)
                      when (iid `EM.member` bag) $
                        applyItem source iid store
                mapM_ applyItemIfPresent iidsToApply
                changeTo tgroup
                return True
          subtractGrpfromBag
            :: Maybe ([(ItemId, ItemFullKit)], ItemBag, [ItemId])
            -> GroupName IK.ItemKind
            -> Maybe ([(ItemId, ItemFullKit)], ItemBag, [ItemId])
          subtractGrpfromBag Nothing _ = Nothing
          subtractGrpfromBag (Just (kitAss, bagToLose, iidsToApply)) grp =
            let grpInItemFull ItemFull{itemKind} =
                  fromMaybe 0 (lookup grp $ IK.ifreq itemKind) > 0
                grpInItemKit (_, (itemFull, _)) = grpInItemFull itemFull
            in case break grpInItemKit kitAss of
              (_, []) -> Nothing
              (prefix, (iid, (itemFull, (k, it))) : rest) -> Just $
                let remainingAss = case compare k 1 of
                      LT -> error "subtractGrpfromBag: no copies in bag"
                      EQ -> []
                      GT -> [(iid, (itemFull, (k - 1, drop 1 it)))]
                    remainingAssocs = prefix ++ remainingAss ++ rest
                    arItem = aspectRecordFull itemFull
                    durable = IA.checkFlag Ability.Durable arItem
                    removedBag = EM.singleton iid (1, take 1 it)
                in if durable
                   then ( remainingAssocs
                        , bagToLose
                        , iid : iidsToApply )
                   else ( remainingAssocs
                        , EM.unionWith mergeItemQuant removedBag bagToLose
                        , iidsToApply )
          feats = TK.tfeature $ okind cotile serverTile
          toAlter feat =
            case feat of
              TK.OpenTo tgroup -> Just tgroup
              TK.CloseTo tgroup -> Just tgroup
              TK.ChangeTo tgroup -> Just tgroup
              _ -> Nothing
          toWalkable feat =  -- assuming the tile originally walkable
            case feat of
              TK.ChangeTo tgroup -> Just tgroup
              _ -> Nothing
          groupsToAlterTo | underFeet = mapMaybe toWalkable feats
                                          -- don't autoclose doors under actor
                          | otherwise = mapMaybe toAlter feats
          toAlterWith feat =
            case feat of
              TK.OpenWith grps tgroup -> Just (grps, tgroup)
              TK.CloseWith grps tgroup -> Just (grps, tgroup)
              TK.ChangeWith grps tgroup -> Just (grps, tgroup)
              _ -> Nothing
          toWalkableWith feat =  -- assuming the tile originally walkable
            case feat of
              TK.ChangeWith grps tgroup -> Just (grps, tgroup)
              _ -> Nothing
          groupstoAlterWith | underFeet = mapMaybe toWalkableWith feats
                                            -- don't autoclose doors under actor
                            | otherwise = mapMaybe toAlterWith feats
      if null groupsToAlterTo && null groupstoAlterWith && EM.null embeds then
        return $ Just AlterNothing  -- no altering possible; silly client
      else
        if underFeet || EM.notMember tpos (lfloor lvl) then
          if underFeet || not (occupiedBigLvl tpos lvl)
                          && not (occupiedProjLvl tpos lvl) then do
            -- If the only thing that happens is the change of the tile,
            -- don't display a message, because the change
            -- is visible on the map (unless it changes into itself)
            -- and there's nothing more to speak about.
            triggered <- if EM.null embeds then return UseDud else do
              -- Can't send @SfxTrigger@ afterwards, because actor may be moved
              -- by the embeds to another level, where @tpos@ is meaningless.
              -- However, don't spam with projectiles on ice.
              unless (bproj sb || underFeet) $
                execSfxAtomic $ SfxTrigger source tpos
              -- The embeds of the initial tile are activated before the tile
              -- is altered. This prevents, e.g., trying to activate items
              -- where none are present any more, or very different to what
              -- the client expected. Surprise only comes through searching
              -- as implemented above.
              -- The items are first revealed for the sake of clients that
              -- may see the tile as hidden. Note that the tile is not revealed
              -- (unless it's altered later on, in which case the new one is).
              revealEmbeds
              tryApplyEmbeds
            case groupsToAlterTo of
              _ | not (EM.null embeds) && triggered /= UseUp
                  || underFeet && not onCombineOnly -> return ()
              [] -> do
                let tryChangeStore store =
                      foldM (\changed groupToAlterWith ->
                               if changed
                               then return True
                               else tryChangeWith store groupToAlterWith)
                            False
                            groupstoAlterWith
                alteredGround <- tryChangeStore CGround
                altered <- if alteredGround
                           then return True
                           else tryChangeStore CEqp
                unless (altered || null groupstoAlterWith) $
                  execSfxAtomic $ SfxMsgFid (bfid sb)
                                $ SfxNoItemsForTile $ map fst groupstoAlterWith
              [groupToAlterTo] -> changeTo groupToAlterTo
              l -> error $ "tile changeable in many ways" `showFailure` l
            return Nothing  -- success
          else return $ Just AlterBlockActor
        else return $ Just AlterBlockItem
  else  -- client is misguided re tile at that position, so bail out
    return $ Just AlterNothing

-- * ReqWait

-- | Do nothing. Wait skill 1 required. Bracing requires 2, sleep 3, lurking 4.
--
-- Something is sometimes done in 'processWatchfulness'.
reqWait :: MonadServerAtomic m => ActorId -> m ()
{-# INLINE reqWait #-}
reqWait source = do
  actorSk <- currentSkillsServer source
  unless (Ability.getSk Ability.SkWait actorSk > 0) $
    execFailure source ReqWait WaitUnskilled

-- * ReqWait10

-- | Do nothing.
--
-- Something is sometimes done in 'processWatchfulness'.
reqWait10 :: MonadServerAtomic m => ActorId -> m ()
{-# INLINE reqWait10 #-}
reqWait10 source = do
  actorSk <- currentSkillsServer source
  unless (Ability.getSk Ability.SkWait actorSk >= 4) $
    execFailure source ReqWait10 WaitUnskilled

-- * ReqYell

-- | Yell/yawn/stretch/taunt.
-- Wakes up (gradually) from sleep. Causes noise heard by enemies on the level
-- even if out of their hearing range.
--
-- Governed by the waiting skill (because everyone is supposed to have it).
-- unlike @ReqWait@, induces overhead.
--
-- This is similar to the effect @Yell@, but always voluntary.
reqYell :: MonadServerAtomic m => ActorId -> m ()
reqYell source = do
  actorSk <- currentSkillsServer source
  if | Ability.getSk Ability.SkWait actorSk > 0 ->
       -- Last yawn before waking up is displayed as a yell, but that's fine.
       -- To fix that, we'd need to move the @SfxTaunt@
       -- to @processWatchfulness@.
       execSfxAtomic $ SfxTaunt True source
     | Ability.getSk Ability.SkMove actorSk <= 0
       || Ability.getSk Ability.SkDisplace actorSk <= 0
       || Ability.getSk Ability.SkMelee actorSk <= 0 ->
       -- Potentially, only waiting is possible, so given that it's drained,
       -- don't let the actor be stuck nor alarm about server failure.
       execSfxAtomic $ SfxTaunt False source
     | otherwise ->
       -- In most situation one of the 3 actions above
       -- can be performed and waiting skill is not needed for that,
       -- so given the 3 skills are available, waste turn
       -- but don't alarm, because it does happen sometimes in crowds.
       --   execFailure source ReqYell YellUnskilled
       return ()

-- * ReqMoveItems

reqMoveItems :: MonadServerAtomic m
             => ActorId -> [(ItemId, Int, CStore, CStore)] -> m ()
reqMoveItems source l = do
  actorSk <- currentSkillsServer source
  if Ability.getSk Ability.SkMoveItem actorSk > 0 then do
    b <- getsState $ getActorBody source
    actorMaxSk <- getsState $ getActorMaxSkills source
    -- Server accepts item movement based on calm at the start, not end
    -- or in the middle, to avoid interrupted or partially ignored commands.
    let calmE = calmEnough b actorMaxSk
    mapM_ (reqMoveItem source calmE) l
  else execFailure source (ReqMoveItems l) MoveItemUnskilled

reqMoveItem :: MonadServerAtomic m
            => ActorId -> Bool -> (ItemId, Int, CStore, CStore) -> m ()
reqMoveItem aid calmE (iid, k, fromCStore, toCStore) = do
  b <- getsState $ getActorBody aid
  let fromC = CActor aid fromCStore
      req = ReqMoveItems [(iid, k, fromCStore, toCStore)]
  toC <- case toCStore of
    CGround -> pickDroppable False aid b  -- drop over fog, etc.
    _ -> return $! CActor aid toCStore
  bagBefore <- getsState $ getContainerBag toC
  if
   | k < 1 || fromCStore == toCStore -> execFailure aid req ItemNothing
   | toCStore == CEqp && not calmE ->
     execFailure aid req ItemNotCalm
   | toCStore == CEqp && eqpOverfull b k ->
     execFailure aid req EqpOverfull
   | otherwise -> do
    upds <- generalMoveItem True iid k fromC toC
    mapM_ execUpdAtomic upds
    itemFull <- getsState $ itemToFull iid
    -- Let any item manipulation attempt to identify, in case the item
    -- got into stash, e.g., by being thrown at the stash location,
    -- and gets identified only when equipped or dropped and picked up again.
    discoverIfMinorEffects toC iid (itemKindId itemFull)
    -- The first recharging period after equipping is random,
    -- between 1 and 2 standard timeouts of the item.
    -- Timeouts for items in shared stash are not consistent wrt the actor's
    -- local time, because actors from many levels put items there
    -- all the time (and don't rebase it to the clock of the stash's level).
    -- If wrong local time in shared stash causes an item to recharge
    -- for a very long time wrt actor on some level,
    -- the player can reset it by dropping the item and picking up again
    -- (as a flip side, a charging item in stash may sometimes
    -- be used at once on another level, with different local time, but only
    -- once, because after first use, the timeout is set to local time).
    -- This is not terribly consistent, but not recharging in stash is
    -- not better, because either we block activation of any items with timeout,
    -- or encourage moving items out of stash, recharging and moving in.
    -- Which is not fun at all, but one more thing to remember doing regularly.
    when (toCStore `elem` [CEqp, COrgan]
          && fromCStore `notElem` [CEqp, COrgan]
          || fromCStore == CStash) $ do
      let beforeIt = case iid `EM.lookup` bagBefore of
            Nothing -> []  -- no such items before move
            Just (_, it2) -> it2
      randomResetTimeout k iid itemFull beforeIt toC
    when (toCStore == CGround) $
      void $ reqAlterFail True True aid (bpos b)
        -- dropping an item engages the item embedded in the ground;
        -- e.g., ignites grass, if the item is torch;
        -- note that grass is not ignited by torch spawned on the ground,
        -- but the next time any item is dropped there, it is ignited,
        -- which is fine --- the torch must have been wrongly positioned

-- * ReqProject

reqProject :: MonadServerAtomic m
           => ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ target position of the projectile
           -> Int        -- ^ digital line parameter
           -> ItemId     -- ^ the item to be projected
           -> CStore     -- ^ which store the items comes from
           -> m ()
reqProject source tpxy eps iid cstore = do
  let req = ReqProject tpxy eps iid cstore
  b <- getsState $ getActorBody source
  actorMaxSk <- getsState $ getActorMaxSkills source
  let calmE = calmEnough b actorMaxSk
  if cstore == CEqp && not calmE then execFailure source req ItemNotCalm
  else do
    mfail <- projectFail source source tpxy eps False iid cstore False
    maybe (return ()) (execFailure source req) mfail

-- * ReqApply

reqApply :: MonadServerAtomic m
         => ActorId  -- ^ actor applying the item (is on current level)
         -> ItemId   -- ^ the item to be applied
         -> CStore   -- ^ the location of the item
         -> m ()
reqApply aid iid cstore = do
  let req = ReqApply iid cstore
  b <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  let calmE = calmEnough b actorMaxSk
  if cstore == CEqp && not calmE then execFailure aid req ItemNotCalm
  else do
    bag <- getsState $ getBodyStoreBag b cstore
    case EM.lookup iid bag of
      Nothing -> execFailure aid req ApplyOutOfReach
      Just kit -> do
        itemFull <- getsState $ itemToFull iid
        actorSk <- currentSkillsServer aid
        localTime <- getsState $ getLocalTime (blid b)
        let skill = Ability.getSk Ability.SkApply actorSk
            legal = permittedApply localTime skill calmE itemFull kit
        case legal of
          Left reqFail -> execFailure aid req reqFail
          Right _ -> applyItem aid iid cstore

-- * ReqGameRestart

reqGameRestart :: MonadServerAtomic m
               => ActorId -> GroupName ModeKind -> Challenge
               -> m ()
reqGameRestart aid groupName scurChalSer = do
  -- This call to `revealItems` is really needed, because the other
  -- happens only at natural game conclusion, not at forced quitting.
  isNoConfirms <- isNoConfirmsGame
  factionD <- getsState sfactionD
  let fidsUI = map fst $ filter (\(_, fact) -> fhasUI (gplayer fact))
                                (EM.assocs factionD)
  unless isNoConfirms $
    mapM_ (\fid -> revealItems fid) fidsUI
  -- Announcing end of game, we send lore, because game is over.
  b <- getsState $ getActorBody aid
  oldSt <- getsState $ gquit . (EM.! bfid b) . sfactionD
  factionAn <- getsServer sfactionAn
  generationAn <- getsServer sgenerationAn
  execUpdAtomic $ UpdQuitFaction
                    (bfid b)
                    oldSt
                    (Just $ Status Restart (fromEnum $ blid b) (Just groupName))
                    (Just (factionAn, generationAn))
  -- We don't save game and don't wait for clips end. ASAP.
  modifyServer $ \ser -> ser { sbreakASAP = True
                             , soptionsNxt = (soptionsNxt ser) {scurChalSer} }

-- * ReqGameDropAndExit

-- After we break out of the game loop, we will notice from @Camping@
-- we shouldn exit the game.
reqGameDropAndExit :: MonadServerAtomic m => ActorId -> m ()
reqGameDropAndExit aid = do
  verifyAssertExplored
  b <- getsState $ getActorBody aid
  oldSt <- getsState $ gquit . (EM.! bfid b) . sfactionD
  execUpdAtomic $ UpdQuitFaction
                    (bfid b)
                    oldSt
                    (Just $ Status Camping (fromEnum $ blid b) Nothing)
                    Nothing
  modifyServer $ \ser -> ser { sbreakASAP = True
                             , sbreakLoop = True }

verifyAssertExplored :: MonadServer m => m ()
verifyAssertExplored = do
  assertExplored <- getsServer $ sassertExplored . soptions
  case assertExplored of
    Nothing -> return ()
    Just lvlN -> do
      -- Exploration (by any party) verfied via spawning; beware of levels
      -- with disabled spawning.
      snumSpawned <- getsServer snumSpawned
      let !_A = assert (toEnum lvlN `EM.member` snumSpawned
                        || toEnum (- lvlN) `EM.member` snumSpawned) ()
      return ()

-- * ReqGameSaveAndExit

-- After we break out of the game loop, we will notice from @Camping@
-- we shouldn exit the game.
reqGameSaveAndExit :: MonadServerAtomic m => ActorId -> m ()
reqGameSaveAndExit aid = do
  verifyAssertExplored
  b <- getsState $ getActorBody aid
  oldSt <- getsState $ gquit . (EM.! bfid b) . sfactionD
  execUpdAtomic $ UpdQuitFaction
                    (bfid b)
                    oldSt
                    (Just $ Status Camping (fromEnum $ blid b) Nothing)
                    Nothing
  modifyServer $ \ser -> ser { sbreakASAP = True
                             , swriteSave = True }

-- * ReqGameSave

-- After we break out of the game loop, we will notice we shouldn't quit
-- the game and we will enter the game loop again.
reqGameSave :: MonadServer m => m ()
reqGameSave =
  modifyServer $ \ser -> ser { sbreakASAP = True
                             , swriteSave = True }

-- * ReqDoctrine

reqDoctrine :: MonadServerAtomic m => FactionId -> Ability.Doctrine -> m ()
reqDoctrine fid toT = do
  fromT <- getsState $ fdoctrine . gplayer . (EM.! fid) . sfactionD
  execUpdAtomic $ UpdDoctrineFaction fid toT fromT

-- * ReqAutomate

reqAutomate :: MonadServerAtomic m => FactionId -> m ()
reqAutomate fid = execUpdAtomic $ UpdAutoFaction fid True
