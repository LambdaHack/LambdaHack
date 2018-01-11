{-# LANGUAGE TupleSections #-}
-- | Handle effects. They are most often caused by requests sent by clients
-- but sometimes also caused by projectiles or periodically activated items.
module Game.LambdaHack.Server.HandleEffectM
  ( applyItem, meleeEffectAndDestroy, effectAndDestroy, itemEffectEmbedded
  , dropCStoreItem, dominateFidSfx, pickDroppable, refillHP, cutCalm
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , applyMeleeDamage, imperishableKit, itemEffectDisco, effectSem
  , effectBurn, effectExplode, effectRefillHP, effectRefillCalm
  , effectDominate, dominateFid, effectImpress, effectSummon
  , effectAscend, findStairExit, switchLevels1, switchLevels2, effectEscape
  , effectParalyze, effectInsertMove, effectTeleport, effectCreateItem
  , effectDropItem, allGroupItems, effectPolyItem, effectIdentify, identifyIid
  , effectDetect, effectDetectX, effectDetectActor, effectDetectItem
  , effectDetectExit, effectDetectHidden
  , effectSendFlying, sendFlyingVector, effectDropBestWeapon
  , effectActivateInv, effectTransformContainer, effectApplyPerfume, effectOneOf
  , effectRecharging, effectTemporary, effectComposite
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Bits (xor)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Key (mapWithKeyM_)

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Client
import qualified Game.LambdaHack.Common.Ability as Ability
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK
import           Game.LambdaHack.Server.CommonM
import           Game.LambdaHack.Server.ItemM
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.PeriodicM
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

-- * Semantics of effects

data UseResult = UseDud | UseId | UseUp
 deriving (Eq, Ord)

applyItem :: MonadServerAtomic m => ActorId -> ItemId -> CStore -> m ()
applyItem aid iid cstore = do
  execSfxAtomic $ SfxApply aid iid cstore
  let c = CActor aid cstore
  meleeEffectAndDestroy aid aid iid c

applyMeleeDamage :: MonadServerAtomic m
                 => ActorId -> ActorId -> ItemId -> m Bool
applyMeleeDamage source target iid = do
  itemBase <- getsState $ getItemBody iid
  if jdamage itemBase == 0 then return False else do  -- speedup
    sb <- getsState $ getActorBody source
    tb <- getsState $ getActorBody target
    ar <- getsState $ getActorAspect target
    hurtMult <- getsState $ armorHurtBonus source target
    totalDepth <- getsState stotalDepth
    Level{ldepth} <- getLevel (blid tb)
    dmg <- rndToAction $ castDice ldepth totalDepth $ jdamage itemBase
    let hpMax = aMaxHP ar
        rawDeltaHP = fromIntegral hurtMult * xM dmg `divUp` 100
        speedDeltaHP = case btrajectory sb of
          Just (_, speed) -> - modifyDamageBySpeed rawDeltaHP speed
          Nothing -> - rawDeltaHP
        -- Any amount of damage is serious, because next turn a stronger
        -- projectile or melee weapon may be used and also it accumulates.
        serious = speedDeltaHP < 0 && source /= target && not (bproj tb)
        deltaHP | serious = -- if HP overfull, at least cut back to max HP
                            min speedDeltaHP (xM hpMax - bhp tb)
                | otherwise = speedDeltaHP
    if deltaHP < 0 then do  -- damage the target, never heal
      refillHP serious target tb deltaHP
      return True
    else return False

refillHP :: MonadServerAtomic m => Bool -> ActorId -> Actor -> Int64 -> m ()
refillHP serious target tbOld deltaHP = do
  execUpdAtomic $ UpdRefillHP target deltaHP
  when serious $ cutCalm target
  -- If leader just lost all HP, change the leader to let players rescue him,
  -- especially if he's slowed by the attackers.
  tb <- getsState $ getActorBody target
  when (bhp tb <= 0 && bhp tbOld > 0) $ do
    mleader <- getsState $ gleader . (EM.! bfid tb) . sfactionD
    when (Just target == mleader) $ do
      actorD <- getsState sactorD
      let ours (_, b) = bfid b == bfid tb && not (bproj b) && bhp b > 0
          -- Only consider actors positive HP.
          positive = filter ours $ EM.assocs actorD
      onLevel <- getsState $ fidActorRegularIds (bfid tb) (blid tb)
      case onLevel ++ map fst positive of
        [] -> return ()
        aid : _ -> execUpdAtomic $ UpdLeadFaction (bfid tb) mleader $ Just aid

-- Here melee damage is applied. This is necessary so that the same
-- AI benefit calculation may be used for flinging and for applying items.
meleeEffectAndDestroy :: MonadServerAtomic m
                      => ActorId -> ActorId -> ItemId -> Container -> m ()
meleeEffectAndDestroy source target iid c = do
  meleePerformed <- applyMeleeDamage source target iid
  bag <- getsState $ getContainerBag c
  case iid `EM.lookup` bag of
    Nothing -> error $ "" `showFailure` (source, target, iid, c)
    Just kit -> do
      itemToF <- getsState itemToFull
      let itemFull = itemToF iid kit
      case itemDisco itemFull of
        Just ItemDisco {itemKind=IK.ItemKind{IK.ieffects}} ->
          effectAndDestroy meleePerformed source target iid c False ieffects
                           itemFull
        _ -> error $ "" `showFailure` (source, target, iid, c)

effectAndDestroy :: MonadServerAtomic m
                 => Bool -> ActorId -> ActorId -> ItemId -> Container -> Bool
                 -> [IK.Effect] -> ItemFull
                 -> m ()
effectAndDestroy meleePerformed _ _ iid container periodic []
                 itemFull@ItemFull{..} =
  -- No identification occurs if effects are null. This case is also a speedup.
  if meleePerformed then do  -- melee may cause item destruction
    let (imperishable, kit) = imperishableKit True periodic itemTimer itemFull
    unless imperishable $
      execUpdAtomic $ UpdLoseItem False iid itemBase kit container
  else return ()
effectAndDestroy meleePerformed source target iid container periodic effs
                 itemFull@ItemFull{..} = do
  let timeout = case itemDisco of
        Just ItemDisco{itemAspect=Just ar} -> aTimeout ar
        _ -> error $ "" `showFailure` itemDisco
      permanent = let tmpEffect :: IK.Effect -> Bool
                      tmpEffect IK.Temporary{} = True
                      tmpEffect (IK.Recharging IK.Temporary{}) = True
                      tmpEffect (IK.OnSmash IK.Temporary{}) = True
                      tmpEffect _ = False
                  in not $ any tmpEffect effs
  lid <- getsState $ lidFromC container
  localTime <- getsState $ getLocalTime lid
  let it1 = let timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
                charging startT = timeShift startT timeoutTurns > localTime
            in filter charging itemTimer
      len = length it1
      recharged = len < itemK
      it2 = if timeout /= 0 && recharged
            then if periodic && not permanent  -- copies are spares only
                 then replicate (itemK - length it1) localTime ++ it1
                 else localTime : it1  -- copies all fire, in turn
            else itemTimer
      !_A = assert (len <= itemK `blame` (source, target, iid, container)) ()
  -- We use up the charge even if eventualy every effect fizzles. Tough luck.
  -- At least we don't destroy the item in such case. Also, we ID it regardless.
  unless (itemTimer == it2) $
    execUpdAtomic $ UpdTimeItem iid container itemTimer it2
  -- If the activation is not periodic, trigger at least the effects
  -- that are not recharging and so don't depend on @recharged@.
  -- Also, if the item was meleed with, let it get destroyed, if perishable,
  -- and let it get identified, even if no effect was eventually triggered.
  -- Otherwise don't even id the item --- no risk of destruction, no id.
  when (not periodic || recharged || meleePerformed) $ do
    -- We have to destroy the item before the effect affects the item
    -- or the actor holding it or standing on it (later on we could
    -- lose track of the item and wouldn't be able to destroy it) .
    -- This is OK, because we don't remove the item type from various
    -- item dictionaries, just an individual copy from the container,
    -- so, e.g., the item can be identified after it's removed.
    let (imperishable, kit) = imperishableKit permanent periodic it2 itemFull
    unless imperishable $
      execUpdAtomic $ UpdLoseItem False iid itemBase kit container
    -- At this point, the item is potentially no longer in container @c@,
    -- so we don't pass @c@ along.
    triggeredEffect <-
      itemEffectDisco source target iid container recharged periodic effs
    let triggered = if meleePerformed then UseUp else triggeredEffect
    sb <- getsState $ getActorBody source
    -- Announce no effect, which is rare and wastes time, so noteworthy.
    unless (triggered == UseUp  -- effects triggered; feedback comes from them
            || periodic  -- don't spam via fizzled periodic effects
            || bproj sb  -- don't spam, projectiles can be very numerous
           ) $
      execSfxAtomic $ SfxMsgFid (bfid sb) $
        if any IK.forApplyEffect effs
        then SfxFizzles  -- something didn't work, despite promising effects
        else SfxNothingHappens  -- fully expected
    -- If none of item's effects was performed, we try to recreate the item.
    -- Regardless, we don't rewind the time, because some info is gained
    -- (that the item does not exhibit any effects in the given context).
    unless (triggered == UseUp || imperishable) $
      execUpdAtomic $ UpdSpotItem False iid itemBase kit container

imperishableKit :: Bool -> Bool -> ItemTimer -> ItemFull
                -> (Bool, ItemQuant)
imperishableKit permanent periodic it2 ItemFull{..} =
  let fragile = IK.Fragile `elem` jfeature itemBase
      durable = IK.Durable `elem` jfeature itemBase
      imperishable = durable && not fragile || periodic && permanent
      kit = if permanent || periodic then (1, take 1 it2) else (itemK, it2)
  in (imperishable, kit)

-- The item is triggered exactly once. If there are more copies,
-- they are left to be triggered next time.
itemEffectEmbedded :: MonadServerAtomic m
                   => ActorId -> LevelId -> Point -> ItemId -> m ()
itemEffectEmbedded aid lid tpos iid = do
  -- First embedded item may move actor to another level, so @lid@
  -- may be unqual to @blid sb@.
  let c = CEmbed lid tpos
  execSfxAtomic $ SfxTrigger aid tpos
  meleeEffectAndDestroy aid aid iid c

-- | The source actor affects the target actor, with a given item.
-- If any of the effects fires up, the item gets identified.
--
-- Note that if we activate a durable item, e.g., armor, from the ground,
-- it will get identified, which is perfectly fine, until we want to add
-- sticky armor that can't be easily taken off (and, e.g., has some maluses).
itemEffectDisco :: MonadServerAtomic m
                => ActorId -> ActorId -> ItemId -> Container -> Bool -> Bool
                -> [IK.Effect]
                -> m UseResult
itemEffectDisco source target iid c recharged periodic effs = do
  urs <- mapM (effectSem source target iid c recharged periodic) effs
  let ur = case urs of
        [] -> UseDud
        _ -> maximum urs
  when (ur >= UseId) $ do  -- note: @UseId@ suffices, not only @UseUp@
    discoKind <- getsState sdiscoKind
    item <- getsState $ getItemBody iid
    case EM.lookup (jkindIx item) discoKind of
      Just KindMean{kmKind} -> do
        seed <- getsServer $ (EM.! iid) . sitemSeedD
        execUpdAtomic $ UpdDiscover c iid kmKind seed
      _ -> error $ "" `showFailure` (source, target, iid, item)
  return ur

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The item may or may not still be in the container.
-- The boolean result indicates if the effect actually fired up,
-- as opposed to fizzled.
effectSem :: MonadServerAtomic m
          => ActorId -> ActorId -> ItemId -> Container -> Bool -> Bool
          -> IK.Effect
          -> m UseResult
effectSem source target iid c recharged periodic effect = do
  let recursiveCall = effectSem source target iid c recharged periodic
  sb <- getsState $ getActorBody source
  pos <- getsState $ posFromC c
  -- @execSfx@ usually comes last in effect semantics, but not always
  -- and we are likely to introduce more variety.
  let execSfx = execSfxAtomic $ SfxEffect (bfid sb) target effect 0
  case effect of
    IK.ELabel _ -> return UseDud
    IK.EqpSlot _ -> return UseDud
    IK.Burn nDm -> effectBurn nDm source target
    IK.Explode t -> effectExplode execSfx t target
    IK.RefillHP p -> effectRefillHP p source target
    IK.RefillCalm p -> effectRefillCalm execSfx p source target
    IK.Dominate -> effectDominate recursiveCall source target
    IK.Impress -> effectImpress recursiveCall execSfx source target
    IK.Summon grp nDm -> effectSummon grp nDm iid source target periodic
    IK.Ascend p -> effectAscend recursiveCall execSfx p source target pos
    IK.Escape{} -> effectEscape source target
    IK.Paralyze nDm -> effectParalyze execSfx nDm target
    IK.InsertMove nDm -> effectInsertMove execSfx nDm target
    IK.Teleport nDm -> effectTeleport execSfx nDm source target
    IK.CreateItem store grp tim ->
      effectCreateItem (Just $ bfid sb) Nothing target store grp tim
    IK.DropItem n k store grp -> effectDropItem execSfx n k store grp target
    IK.PolyItem -> effectPolyItem execSfx source target
    IK.Identify -> effectIdentify execSfx iid source target
    IK.Detect radius -> effectDetect execSfx radius target
    IK.DetectActor radius -> effectDetectActor execSfx radius target
    IK.DetectItem radius -> effectDetectItem execSfx radius target
    IK.DetectExit radius -> effectDetectExit execSfx radius target
    IK.DetectHidden radius -> effectDetectHidden execSfx radius target pos
    IK.SendFlying tmod ->
      effectSendFlying execSfx tmod source target Nothing
    IK.PushActor tmod ->
      effectSendFlying execSfx tmod source target (Just True)
    IK.PullActor tmod ->
      effectSendFlying execSfx tmod source target (Just False)
    IK.DropBestWeapon -> effectDropBestWeapon execSfx target
    IK.ActivateInv symbol -> effectActivateInv execSfx target symbol
    IK.ApplyPerfume -> effectApplyPerfume execSfx target
    IK.OneOf l -> effectOneOf recursiveCall l
    IK.OnSmash _ -> return UseDud  -- ignored under normal circumstances
    IK.Recharging e -> effectRecharging recursiveCall e recharged
    IK.Temporary _ -> effectTemporary execSfx source iid c
    IK.Unique -> return UseDud
    IK.Periodic -> return UseDud
    IK.Composite l -> effectComposite recursiveCall l

-- * Individual semantic functions for effects

-- ** Burn

-- Damage from fire. Not affected by armor.
effectBurn :: MonadServerAtomic m
           => Dice.Dice -> ActorId -> ActorId -> m UseResult
effectBurn nDm source target = do
  tb <- getsState $ getActorBody target
  ar <- getsState $ getActorAspect target
  let hpMax = aMaxHP ar
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  n <- rndToAction $ castDice ldepth totalDepth nDm
  let rawDeltaHP = - xM n
      -- We ignore minor burns.
      serious = n > 1 && source /= target && not (bproj tb)
      deltaHP | serious = -- if HP overfull, at least cut back to max HP
                          min rawDeltaHP (xM hpMax - bhp tb)
              | otherwise = rawDeltaHP
  if deltaHP == 0
  then return UseDud
  else do
    sb <- getsState $ getActorBody source
    -- Display the effect more accurately.
    let reportedEffect = IK.Burn $ Dice.intToDice n
    execSfxAtomic $ SfxEffect (bfid sb) target reportedEffect deltaHP
    -- Damage the target.
    refillHP serious target tb deltaHP
    return UseUp

-- ** Explode

effectExplode :: MonadServerAtomic m
              => m () -> GroupName ItemKind -> ActorId -> m UseResult
effectExplode execSfx cgroup target = do
  execSfx
  tb <- getsState $ getActorBody target
  let itemFreq = [(cgroup, 1)]
      -- Explosion particles are placed among organs of the victim:
      container = CActor target COrgan
  m2 <- rollAndRegisterItem (blid tb) itemFreq container False Nothing
  let (iid, (ItemFull{itemBase, itemK}, _)) =
        fromMaybe (error $ "" `showFailure` cgroup) m2
      Point x y = bpos tb
      semirandom = fromEnum (jkindIx itemBase)
      projectN k100 (n, _) = do
        -- We pick a point at the border, not inside, to have a uniform
        -- distribution for the points the line goes through at each distance
        -- from the source. Otherwise, e.g., the points on cardinal
        -- and diagonal lines from the source would be more common.
        let veryrandom = (k100 `xor` (semirandom + n)) `mod` 5
            fuzz = 5 + veryrandom
            k | itemK >= 8 && n < 4 = 0  -- speed up if only a handful remains
              | n < 16 && n >= 12 = 12
              | n < 12 && n >= 8 = 8
              | n < 8 && n >= 4 = 4
              | otherwise = min n 16  -- fire in groups of 16 including old duds
            psAll =
              [ Point (x - 12) (y + 12)
              , Point (x + 12) (y + 12)
              , Point (x - 12) (y - 12)
              , Point (x + 12) (y - 12)
              , Point (x - 12) y
              , Point (x + 12) y
              , Point x (y + 12)
              , Point x (y - 12)
              , Point (x - 12) $ y + fuzz
              , Point (x + 12) $ y + fuzz
              , Point (x - 12) $ y - fuzz
              , Point (x + 12) $ y - fuzz
              , flip Point (y - 12) $ x + fuzz
              , flip Point (y + 12) $ x + fuzz
              , flip Point (y - 12) $ x - fuzz
              , flip Point (y + 12) $ x - fuzz
              ]
            ps = take k $ cycle $ drop ((k100 + itemK + n) `mod` 16) psAll
        forM_ ps $ \tpxy -> do
          let req = ReqProject tpxy veryrandom iid COrgan
          mfail <- projectFail target tpxy veryrandom iid COrgan True
          case mfail of
            Nothing -> return ()
            Just ProjectBlockTerrain -> return ()
            Just ProjectBlockActor | not $ bproj tb -> return ()
            Just failMsg -> execFailure target req failMsg
      tryFlying 0 = return ()
      tryFlying k100 = do
        -- Explosion particles are placed among organs of the victim:
        bag2 <- getsState $ borgan . getActorBody target
        let mn2 = EM.lookup iid bag2
        case mn2 of
          Nothing -> return ()
          Just n2 -> do
            projectN k100 n2
            tryFlying $ k100 - 1
  -- Particles that fail to take off, bounce off obstacles up to 100 times
  -- in total, trying to fly in different directions.
  tryFlying 100
  bag3 <- getsState $ borgan . getActorBody target
  let mn3 = EM.lookup iid bag3
  -- Give up and destroy the remaining particles, if any.
  maybe (return ()) (\kit -> execUpdAtomic
                             $ UpdLoseItem False iid itemBase kit container) mn3
  return UseUp  -- we neglect verifying that at least one projectile got off

-- ** RefillHP

-- Unaffected by armor.
effectRefillHP :: MonadServerAtomic m
               => Int -> ActorId -> ActorId -> m UseResult
effectRefillHP power source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  ar <- getsState $ getActorAspect target
  let hpMax = aMaxHP ar
      -- We ignore light poison and similar -1HP per turn annoyances.
      serious = power < -1 && source /= target && not (bproj tb)
      deltaHP | serious = -- if overfull, at least cut back to max
                          min (xM power) (xM hpMax - bhp tb)
              | otherwise = min (xM power) (max 0 $ xM 999 - bhp tb)
                                                         -- UI limitation
  curChalSer <- getsServer $ scurChalSer . soptions
  fact <- getsState $ (EM.! bfid tb) . sfactionD
  if | cfish curChalSer && power > 0
       && fhasUI (gplayer fact) && bfid sb /= bfid tb -> do
       execSfxAtomic $ SfxMsgFid (bfid tb) SfxColdFish
       return UseId
     | deltaHP == 0 -> return UseDud
     | otherwise -> do
       execSfxAtomic $ SfxEffect (bfid sb) target (IK.RefillHP power) deltaHP
       refillHP serious target tb deltaHP
       return UseUp

cutCalm :: MonadServerAtomic m => ActorId -> m ()
cutCalm target = do
  tb <- getsState $ getActorBody target
  ar <- getsState $ getActorAspect target
  let upperBound = if hpTooLow tb ar
                   then 0  -- to trigger domination, etc.
                   else xM $ aMaxCalm ar
      deltaCalm = min minusM1 (upperBound - bcalm tb)
  -- HP loss decreases Calm by at least @minusM1@ to avoid "hears something",
  -- which is emitted when decreasing Calm by @minusM@.
  udpateCalm target deltaCalm

-- ** RefillCalm

effectRefillCalm :: MonadServerAtomic m
                 => m () -> Int -> ActorId -> ActorId -> m UseResult
effectRefillCalm execSfx power source target = do
  tb <- getsState $ getActorBody target
  ar <- getsState $ getActorAspect target
  let calmMax = aMaxCalm ar
      serious = not (bproj tb) && source /= target && power > 1
      deltaCalm | power < 0 && serious =  -- if overfull, at least cut to max
                    min (xM power) (xM calmMax - bcalm tb)
                | otherwise = min (xM power) (max 0 $ xM 999 - bcalm tb)
                                                           -- UI limitation
  if deltaCalm == 0
  then return UseDud
  else do
    execSfx
    udpateCalm target deltaCalm
    return UseUp

-- ** Dominate

effectDominate :: MonadServerAtomic m
               => (IK.Effect -> m UseResult) -> ActorId -> ActorId
               -> m UseResult
effectDominate recursiveCall source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if | bproj tb -> return UseDud
     | bfid tb == bfid sb ->
       -- Dominate is rather on projectiles than on items, so alternate effect
       -- is useful to avoid boredom if domination can't happen.
       recursiveCall IK.Impress
     | otherwise -> do
       b <- dominateFidSfx (bfid sb) target
       return $! if b then UseUp else UseDud

dominateFidSfx :: MonadServerAtomic m => FactionId -> ActorId -> m Bool
dominateFidSfx fid target = do
  tb <- getsState $ getActorBody target
  -- Actors that don't move freely can't be dominated, for otherwise,
  -- when they are the last survivors, they could get stuck
  -- and the game wouldn't end.
  ar <- getsState $ getActorAspect target
  let actorMaxSk = aSkills ar
      -- Check that the actor can move, also between levels and through doors.
      -- Otherwise, it's too awkward for human player to control, e.g.,
      -- being stuck in a room with revolving doors closing after one turn
      -- and the player needing to micromanage opening such doors with
      -- another actor all the time. Completely immovable actors
      -- e.g., an impregnable surveillance camera in a crowded corridor,
      -- are less of a problem due to micromanagment, but more due to
      -- the constant disturbing of other actor's running, etc..
      canMove = EM.findWithDefault 0 Ability.AbMove actorMaxSk > 0
                && EM.findWithDefault 0 Ability.AbAlter actorMaxSk
                   >= fromEnum TK.talterForStairs
  if canMove && not (bproj tb) && bhp tb > 0 then do
    let execSfx = execSfxAtomic $ SfxEffect fid target IK.Dominate 0
    execSfx  -- if actor ours, possibly the last occasion to see him
    gameOver <- dominateFid fid target
    unless gameOver  -- avoid spam
      execSfx  -- see the actor as theirs, unless position not visible
    return True
  else
    return False

dominateFid :: MonadServerAtomic m => FactionId -> ActorId -> m Bool
dominateFid fid target = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  tb0 <- getsState $ getActorBody target
  -- At this point the actor's body exists and his items are not dropped.
  deduceKilled target
  electLeader (bfid tb0) (blid tb0) target
  fact <- getsState $ (EM.! bfid tb0) . sfactionD
  -- Prevent the faction's stash from being lost in case they are not spawners.
  when (isNothing $ gleader fact) $ moveStores False target CSha CInv
  tb <- getsState $ getActorBody target
  ais <- getsState $ getCarriedAssocsAndTrunk tb
  ar <- getsState $ getActorAspect target
  getItem <- getsState $ flip getItemBody
  discoKind <- getsState sdiscoKind
  let isImpression iid = case EM.lookup (jkindIx $ getItem iid) discoKind of
        Just KindMean{kmKind} ->
          maybe False (> 0) $ lookup "impressed" $ IK.ifreq (okind kmKind)
        Nothing -> error $ "" `showFailure` iid
      dropAllImpressions = EM.filterWithKey (\iid _ -> not $ isImpression iid)
      borganNoImpression = dropAllImpressions $ borgan tb
  btime <-
    getsServer $ (EM.! target) . (EM.! blid tb) . (EM.! bfid tb) . sactorTime
  execUpdAtomic $ UpdLoseActor target tb ais
  let bNew = tb { bfid = fid
                , bcalm = max (xM 10) $ xM (aMaxCalm ar) `div` 2
                , bhp = min (xM $ aMaxHP ar) $ bhp tb + xM 10
                , borgan = borganNoImpression}
  aisNew <- getsState $ getCarriedAssocsAndTrunk bNew
  execUpdAtomic $ UpdSpotActor target bNew aisNew
  modifyServer $ \ser ->
    ser {sactorTime = updateActorTime fid (blid tb) target btime
                      $ sactorTime ser}
  factionD <- getsState sfactionD
  let inGame fact2 = case gquit fact2 of
        Nothing -> True
        Just Status{stOutcome=Camping} -> True
        _ -> False
      gameOver = not $ any inGame $ EM.elems factionD
  if gameOver
  then return True  -- avoid spam identifying item at this point
  else do
    -- Add some nostalgia for the old faction.
    void $ effectCreateItem (Just $ bfid tb) (Just 10) target COrgan
                            "impressed" IK.timerNone
    itemToF <- getsState itemToFull
    let discoverIf (iid, cstore) = do
          let itemFull = itemToF iid (1, [])
              c = CActor target cstore
          discoverIfNoEffects c iid itemFull
        aic = (btrunk tb, if bproj tb then CEqp else COrgan)
              : filter ((/= btrunk tb) . fst) (getCarriedIidCStore tb)
    mapM_ discoverIf aic
    -- Focus on the dominated actor, by making him a leader.
    supplantLeader fid target
    return False

-- ** Impress

effectImpress :: MonadServerAtomic m
              => (IK.Effect -> m UseResult) -> m () -> ActorId -> ActorId
              -> m UseResult
effectImpress recursiveCall execSfx source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if | bproj tb || bhp tb <= 0 -> return UseDud  -- avoid spam just before death
     | bfid tb == bfid sb -> do
       -- Unimpress wrt others, but only once.
       ur <- recursiveCall $ IK.DropItem 1 1 COrgan "impressed"
       when (ur == UseUp) execSfx
       return ur
     | otherwise -> do
       execSfx
       effectCreateItem (Just $ bfid sb) (Just 1) target COrgan
                        "impressed" IK.timerNone

-- ** Summon

-- Note that the Calm expended doesn't depend on the number of actors summoned.
effectSummon :: MonadServerAtomic m
             => GroupName ItemKind -> Dice.Dice -> ItemId
             -> ActorId -> ActorId -> Bool
             -> m UseResult
effectSummon grp nDm iid source target periodic = do
  -- Obvious effect, nothing announced.
  Kind.COps{coTileSpeedup} <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  actorAspect <- getsState sactorAspect
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  power <- rndToAction $ castDice ldepth totalDepth nDm
  item <- getsState $ getItemBody iid
  -- We put @source@ instead of @target@ and @power@ instead of dice
  -- to make the message more accurate.
  let effect = IK.Summon grp $ Dice.intToDice power
      execSfx = execSfxAtomic $ SfxEffect (bfid sb) source effect 0
      sar = actorAspect EM.! source
      tar = actorAspect EM.! target
      durable = IK.Durable `elem` jfeature item
      deltaCalm = - xM 30
  -- Verify Calm only at periodic activations or if the item is durable.
  -- Otherwise summon uses up the item, which prevents summoning getting
  -- out of hand. I don't verify Calm otherwise, to prevent an exploit
  -- via draining one's calm on purpose when an item with good activation
  -- has a nasty summoning side-effect (the exploit still works on durables).
  if | bproj sb -> return UseDud  -- basically a misfire
     | power <= 0 -> return UseDud  -- e.g., @1 `dL` x@ at depth 1
     | (periodic || durable)
       && (bcalm sb < - deltaCalm || not (calmEnough sb sar)) -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxSummonLackCalm source
       return UseId
     | otherwise -> do
       execSfx
       unless (bproj sb) $ udpateCalm source deltaCalm
       let validTile t = not $ Tile.isNoActor coTileSpeedup t
       ps <- getsState $ nearbyFreePoints validTile (bpos tb) (blid tb)
       localTime <- getsState $ getLocalTime (blid tb)
       -- Make sure summoned actors start acting after the victim.
       let actorTurn = ticksPerMeter $ bspeed tb tar
           targetTime = timeShift localTime actorTurn
           afterTime = timeShift targetTime $ Delta timeClip
       bs <- forM (take power ps) $ \p -> do
         -- Mark as summoned to prevent immediate chain summoning.
         maid <- addAnyActor True [(grp, 1)] (blid tb) afterTime (Just p)
         case maid of
           Nothing -> return False  -- not enough space in dungeon?
           Just aid -> do
             b <- getsState $ getActorBody aid
             mleader <- getsState $ gleader . (EM.! bfid b) . sfactionD
             when (isNothing mleader) $ supplantLeader (bfid b) aid
             return True
       return $! if or bs then UseUp else UseId

-- ** Ascend

-- Note that projectiles can be teleported, too, for extra fun.
effectAscend :: MonadServerAtomic m
             => (IK.Effect -> m UseResult)
             -> m () -> Bool -> ActorId -> ActorId -> Point
             -> m UseResult
effectAscend recursiveCall execSfx up source target pos = do
  b1 <- getsState $ getActorBody target
  let lid1 = blid b1
  (lid2, pos2) <- getsState $ whereTo lid1 pos (Just up) . sdungeon
  sb <- getsState $ getActorBody source
  if | braced b1 -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxBracedImmune target
       return UseId
     | lid2 == lid1 && pos2 == pos -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxLevelNoMore
       -- We keep it useful even in shallow dungeons.
       recursiveCall $ IK.Teleport 30  -- powerful teleport
     | otherwise -> do
       execSfx
       btime_bOld <- getsServer $ (EM.! target) . (EM.! lid1)
                       . (EM.! bfid b1) . sactorTime
       pos3 <- findStairExit (bfid sb) up lid2 pos2
       let switch1 = void $ switchLevels1 (target, b1)
           switch2 = do
             -- Make the initiator of the stair move the leader,
             -- to let him clear the stairs for others to follow.
             let mlead = Just target
             -- Move the actor to where the inhabitants were, if any.
             switchLevels2 lid2 pos3 (target, b1) btime_bOld mlead
       -- The actor will be added to the new level,
       -- but there can be other actors at his new position.
       inhabitants <- getsState $ posToAssocs pos3 lid2
       case inhabitants of
         [] -> do
           switch1
           switch2
         (_, b2) : _ -> do
           -- Alert about the switch.
           -- Only tell one player, even if many actors, because then
           -- they are projectiles, so not too important.
           execSfxAtomic $ SfxMsgFid (bfid b2) SfxLevelPushed
           -- Move the actor out of the way.
           switch1
           -- Move the inhabitants out of the way and to where the actor was.
           let moveInh inh = do
                 -- Preserve the old leader, since the actor is pushed,
                 -- so possibly has nothing worhwhile to do on the new level
                 -- (and could try to switch back, if made a leader,
                 -- leading to a loop).
                 btime_inh <-
                   getsServer $ (EM.! fst inh) . (EM.! lid2)
                                . (EM.! bfid (snd inh)) . sactorTime
                 inhMLead <- switchLevels1 inh
                 switchLevels2 lid1 (bpos b1) inh btime_inh inhMLead
           mapM_ moveInh inhabitants
           -- Move the actor to his destination.
           switch2
       return UseUp

findStairExit :: MonadStateRead m
              => FactionId -> Bool -> LevelId -> Point -> m Point
findStairExit side moveUp lid pos = do
  Kind.COps{coTileSpeedup} <- getsState scops
  fact <- getsState $ (EM.! side) . sfactionD
  lvl <- getLevel lid
  let defLanding = uncurry Vector $ if moveUp then (1, 0) else (-1, 0)
      center = uncurry Vector $ if moveUp then (-1, 0) else (1, 0)
      (mvs2, mvs1) = break (== defLanding) moves
      mvs = center : filter (/= center) (mvs1 ++ mvs2)
      ps = filter (Tile.isWalkable coTileSpeedup . (lvl `at`))
           $ map (shift pos) mvs
      posOcc :: State -> Int -> Point -> Bool
      posOcc s k p = case posToAssocs p lid s of
        [] -> k == 0
        (_, b) : _ | bproj b -> k == 3
        (_, b) : _ | isAtWar fact (bfid b) -> k == 1  -- non-proj foe
        _ -> k == 2  -- moving a non-projectile friend
  unocc <- getsState posOcc
  case concatMap (\k -> filter (unocc k) ps) [0..3] of
    [] -> error $ "" `showFailure` ps
    posRes : _ -> return posRes

switchLevels1 :: MonadServerAtomic m => (ActorId, Actor) -> m (Maybe ActorId)
switchLevels1 (aid, bOld) = do
  let side = bfid bOld
  mleader <- getsState $ gleader . (EM.! side) . sfactionD
  -- Prevent leader pointing to a non-existing actor.
  mlead <-
    if not (bproj bOld) && isJust mleader then do
      execUpdAtomic $ UpdLeadFaction side mleader Nothing
      return mleader
        -- outside of a client we don't know the real tgt of aid, hence fst
    else return Nothing
  -- Remove the actor from the old level.
  -- Onlookers see somebody disappear suddenly.
  -- @UpdDestroyActor@ is too loud, so use @UpdLoseActor@ instead.
  ais <- getsState $ getCarriedAssocsAndTrunk bOld
  execUpdAtomic $ UpdLoseActor aid bOld ais
  return mlead

switchLevels2 ::MonadServerAtomic m
              => LevelId -> Point -> (ActorId, Actor) -> Time -> Maybe ActorId
              -> m ()
switchLevels2 lidNew posNew (aid, bOld) btime_bOld mlead = do
  let lidOld = blid bOld
      side = bfid bOld
  let !_A = assert (lidNew /= lidOld `blame` "stairs looped" `swith` lidNew) ()
  -- Sync actor's items' timeouts with the new local time of the level.
  -- We need to sync organs and equipment due to periodic activations,
  -- but also inventory pack (as well as some organs and equipment),
  -- due to timeouts after use, e.g., for some weapons (they recharge also
  -- in the pack; however, this doesn't encourage micromanagement for periodic
  -- items, because the timeout is randomised upon move to equipment).
  --
  -- We don't rebase timeouts for items in stash, because they are
  -- used by many actors on levels with different local times,
  -- so there is no single rebase that would match all.
  -- This is not a big problem: after a single use by an actor the timeout is
  -- set to his current local time, so further uses by that actor have
  -- not anomalously short or long recharge times. If the recharge time
  -- is very long, the player has an option of moving the item from stash
  -- to pack and back, to reset the timeout. An abuse is possible when recently
  -- used item is put from inventory to stash and at once used on another level
  -- taking advantage of local time difference, but this only works once
  -- and using the item back again at the original level makes the recharge
  -- time longer, in turn.
  timeOld <- getsState $ getLocalTime lidOld
  timeLastActive <- getsState $ getLocalTime lidNew
  let delta = timeLastActive `timeDeltaToFrom` timeOld
      shiftByDelta = (`timeShift` delta)
      computeNewTimeout :: ItemQuant -> ItemQuant
      computeNewTimeout (k, it) = (k, map shiftByDelta it)
      rebaseTimeout :: ItemBag -> ItemBag
      rebaseTimeout = EM.map computeNewTimeout
      bNew = bOld { blid = lidNew
                  , bpos = posNew
                  , boldpos = Just posNew  -- new level, new direction
                  , borgan = rebaseTimeout $ borgan bOld
                  , beqp = rebaseTimeout $ beqp bOld
                  , binv = rebaseTimeout $ binv bOld }
  -- Materialize the actor at the new location.
  -- Onlookers see somebody appear suddenly. The actor himself
  -- sees new surroundings and has to reset his perception.
  ais <- getsState $ getCarriedAssocsAndTrunk bOld
  execUpdAtomic $ UpdCreateActor aid bNew ais
  -- Sync the actor time with the level time.
  -- This time shift may cause a double move of a foe of the same speed,
  -- but this is OK --- the foe didn't have a chance to move
  -- before, because the arena went inactive, so he moves now one more time.
  let btime = shiftByDelta btime_bOld
  modifyServer $ \ser ->
    ser {sactorTime = updateActorTime (bfid bNew) lidNew aid btime
                      $ sactorTime ser}
  case mlead of
    Nothing -> return ()
    Just leader -> supplantLeader side leader

-- ** Escape

-- | The faction leaves the dungeon.
effectEscape :: MonadServerAtomic m => ActorId -> ActorId -> m UseResult
effectEscape source target = do
  -- Obvious effect, nothing announced.
  sb <- getsState $ getActorBody source
  b <- getsState $ getActorBody target
  let fid = bfid b
  fact <- getsState $ (EM.! fid) . sfactionD
  if | bproj b ->
       return UseDud  -- basically a misfire
     | not (fcanEscape $ gplayer fact) -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxEscapeImpossible
       return UseId
     | otherwise -> do
       deduceQuits (bfid b) $ Status Escape (fromEnum $ blid b) Nothing
       return UseUp

-- ** Paralyze

-- | Advance target actor time by this many time clips. Not by actor moves,
-- to hurt fast actors more.
effectParalyze :: MonadServerAtomic m
               => m () -> Dice.Dice -> ActorId -> m UseResult
effectParalyze execSfx nDm target = do
  tb <- getsState $ getActorBody target
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  p <- rndToAction $ castDice ldepth totalDepth nDm
  let t = timeDeltaScale (Delta timeClip) p
  if p <= 0 || bproj tb || bhp tb <= 0
  then return UseDud
  else do
    execSfx
    modifyServer $ \ser ->
      ser {sactorTime = ageActor (bfid tb) (blid tb) target t $ sactorTime ser}
    return UseUp

-- ** InsertMove

-- | Give target actor the given number of extra moves. Don't give
-- an absolute amount of time units, to benefit slow actors more.
effectInsertMove :: MonadServerAtomic m
                 => m () -> Dice.Dice -> ActorId -> m UseResult
effectInsertMove execSfx nDm target = do
  tb <- getsState $ getActorBody target
  ar <- getsState $ getActorAspect target
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  p <- rndToAction $ castDice ldepth totalDepth nDm
  let actorTurn = ticksPerMeter $ bspeed tb ar
      t = timeDeltaScale actorTurn (-p)
  if p <= 0
  then return UseDud
  else do
    execSfx
    modifyServer $ \ser ->
      ser {sactorTime = ageActor (bfid tb) (blid tb) target t $ sactorTime ser}
    return UseUp

-- ** Teleport

-- | Teleport the target actor.
-- Note that projectiles can be teleported, too, for extra fun.
effectTeleport :: MonadServerAtomic m
               => m () -> Dice.Dice -> ActorId -> ActorId -> m UseResult
effectTeleport execSfx nDm source target = do
  Kind.COps{coTileSpeedup} <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  totalDepth <- getsState stotalDepth
  lvl@Level{ldepth, ltile} <- getLevel (blid tb)
  range <- rndToAction $ castDice ldepth totalDepth nDm
  let spos = bpos tb
      dMinMax delta pos =
        let d = chessDist spos pos
        in d >= range - delta && d <= range + delta
      dist delta pos _ = dMinMax delta pos
  tpos <- rndToAction $ findPosTry 200 ltile
    (\p t -> Tile.isWalkable coTileSpeedup t
             && (not (dMinMax 9 p)  -- don't loop, very rare
                 || not (Tile.isNoActor coTileSpeedup t)
                    && null (posToAidsLvl p lvl)))
    [ dist 1
    , dist $ 1 + range `div` 9
    , dist $ 1 + range `div` 7
    , dist $ 1 + range `div` 5
    , dist 5
    , dist 7
    , dist 9
    ]
  if | braced tb -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxBracedImmune target
       return UseId
     | not (dMinMax 9 tpos) -> do  -- very rare
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxTransImpossible
       return UseId
     | otherwise -> do
       execSfx
       execUpdAtomic $ UpdMoveActor target spos tpos
       return UseUp

-- ** CreateItem

effectCreateItem :: MonadServerAtomic m
                 => Maybe FactionId -> Maybe Int -> ActorId -> CStore
                 -> GroupName ItemKind -> IK.TimerDice
                 -> m UseResult
effectCreateItem jfidRaw mcount target store grp tim = do
  tb <- getsState $ getActorBody target
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  let fscale unit nDm = do
        k <- rndToAction $ castDice ldepth totalDepth nDm
        let !_A = assert (k >= 0) ()
        return $! timeDeltaScale unit k
      fgame = fscale (Delta timeTurn)
      factor nDm = do
        ar <- getsState $ getActorAspect target
        let actorTurn = ticksPerMeter $ bspeed tb ar
        fscale actorTurn nDm
  delta <- IK.foldTimer (return $ Delta timeZero) fgame factor tim
  let c = CActor target store
  bagBefore <- getsState $ getBodyStoreBag tb store
  let litemFreq = [(grp, 1)]
  -- Power depth of new items unaffected by number of spawned actors.
  m5 <- rollItem 0 (blid tb) litemFreq
  let (itemKnownRaw, itemFullRaw, _, seed, _) =
        fromMaybe (error $ "" `showFailure` (blid tb, litemFreq, c)) m5
      -- Avoid too many different item identifiers (one for each faction)
      -- for blasts or common item generating tiles. Temporary organs are
      -- allowed to be duplicated, because they provide really useful info
      -- (perpetrator). However, if timer is none, they are not duplicated
      -- to make sure that, e.g., poisons stack with each other regardless
      -- of perpetrator and we don't get "no longer poisoned" message
      -- while still poisoned due to another faction. With timed aspects,
      -- e.g., slowness, the message is also misleading, but it's interesting
      -- that I'm twice slower due to aspects from two factions and as deadly
      -- as being poisoned at twice the rate from two factions.
      jfid = if store == COrgan && not (IK.isTimerNone tim)
             then jfidRaw
             else Nothing
      (itemKnown, itemFullFid) =
        let (kindIx, ar, damage, _) = itemKnownRaw
        in ( (kindIx, ar, damage, jfid)
           , itemFullRaw {itemBase = (itemBase itemFullRaw) {jfid}} )
      itemFull = case mcount of
        Just itemK -> itemFullFid {itemK}
        Nothing -> itemFullFid
  itemRev <- getsServer sitemRev
  let mquant = case HM.lookup itemKnown itemRev of
        Nothing -> Nothing
        Just iid -> (iid,) <$> iid `EM.lookup` bagBefore
  case mquant of
    Just (iid, (_, afterIt@(timer : rest))) | not $ IK.isTimerNone tim -> do
      -- Already has such items and timer change requested, so only increase
      -- the timer of the first item by the delta, but don't create items.
      let newIt = timer `timeShift` delta : rest
      if afterIt /= newIt then do
        execUpdAtomic $ UpdTimeItem iid c afterIt newIt
        -- It's hard for the client to tell this timer change from charge use,
        -- timer reset on pickup, etc., so we create the msg manually.
        execSfxAtomic $ SfxMsgFid (bfid tb) $ SfxTimerExtended target iid store
        return UseUp
      else return UseDud  -- probably incorrect content, but let it be
    _ -> do
      -- No such items or some items, but void delta, so create items.
      -- If it's, e.g., a periodic poison, the new items will stack with any
      -- already existing items.
      iid <- registerItem itemFull itemKnown seed c True
      -- If created not on the ground, ID it, because it's not IDed on pickup.
      when (store /= CGround) $ discoverIfNoEffects c iid itemFull
      -- Now, if timer change requested, change the timer, but in the new items,
      -- possibly increased in number wrt old items.
      when (not $ IK.isTimerNone tim) $ do
        tb2 <- getsState $ getActorBody target
        bagAfter <- getsState $ getBodyStoreBag tb2 store
        localTime <- getsState $ getLocalTime (blid tb)
        let newTimer = localTime `timeShift` delta
            (afterK, afterIt) =
              fromMaybe (error $ "" `showFailure` (iid, bagAfter, c))
                        (iid `EM.lookup` bagAfter)
            newIt = replicate afterK newTimer
        when (afterIt /= newIt) $
          execUpdAtomic $ UpdTimeItem iid c afterIt newIt
      return UseUp

-- ** DropItem

-- | Make the target actor drop all items in a store from the given group
-- (not just a random single item, or cluttering equipment with rubbish
-- would be beneficial).
effectDropItem :: MonadServerAtomic m
               => m () -> Int -> Int -> CStore -> GroupName ItemKind -> ActorId
               -> m UseResult
effectDropItem execSfx ngroup kcopy store grp target = do
  b <- getsState $ getActorBody target
  is <- allGroupItems store grp target
  if null is
  then return UseDud
  else do
    unless (store == COrgan) execSfx
    mapM_ (uncurry (dropCStoreItem True store target b kcopy)) $ take ngroup is
    return UseUp

allGroupItems :: MonadServerAtomic m
              => CStore -> GroupName ItemKind -> ActorId
              -> m [(ItemId, ItemQuant)]
allGroupItems store grp target = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  discoKind <- getsState sdiscoKind
  b <- getsState $ getActorBody target
  let hasGroup (iid, _) = do
        item <- getsState $ getItemBody iid
        case EM.lookup (jkindIx item) discoKind of
          Just KindMean{kmKind} ->
            return $! maybe False (> 0) $ lookup grp $ IK.ifreq (okind kmKind)
          Nothing ->
            error $ "" `showFailure` (target, grp, iid, item)
  assocsCStore <- getsState $ EM.assocs . getBodyStoreBag b store
  filterM hasGroup assocsCStore

-- | Drop a single actor's item. Note that if there are multiple copies,
-- at most one explodes to avoid excessive carnage and UI clutter
-- (let's say, the multiple explosions interfere with each other or perhaps
-- larger quantities of explosives tend to be packaged more safely).
dropCStoreItem :: MonadServerAtomic m
               => Bool -> CStore -> ActorId -> Actor -> Int
               -> ItemId -> ItemQuant
               -> m ()
dropCStoreItem verbose store aid b kMax iid kit@(k, _) = do
  item <- getsState $ getItemBody iid
  let c = CActor aid store
      fragile = IK.Fragile `elem` jfeature item
      durable = IK.Durable `elem` jfeature item
      isDestroyed = bproj b && (bhp b <= 0 && not durable || fragile)
                    || fragile && durable  -- hack for tmp organs
  if isDestroyed then do
    itemToF <- getsState itemToFull
    let itemFull = itemToF iid kit
        effs = strengthOnSmash itemFull
    -- Activate even if effects null, to destroy the item.
    effectAndDestroy False aid aid iid c False effs itemFull
  else do
    cDrop <- pickDroppable aid b
    mvCmd <- generalMoveItem verbose iid (min kMax k) (CActor aid store) cDrop
    mapM_ execUpdAtomic mvCmd

pickDroppable :: MonadStateRead m => ActorId -> Actor -> m Container
pickDroppable aid b = do
  Kind.COps{coTileSpeedup} <- getsState scops
  lvl <- getLevel (blid b)
  let validTile t = not $ Tile.isNoItem coTileSpeedup t
  if validTile $ lvl `at` bpos b
  then return $! CActor aid CGround
  else do
    ps <- getsState $ nearbyFreePoints validTile (bpos b) (blid b)
    return $! case ps of
      [] -> CActor aid CGround  -- fallback; still correct, though not ideal
      pos : _ -> CFloor (blid b) pos

-- ** PolyItem

effectPolyItem :: MonadServerAtomic m
               => m () -> ActorId -> ActorId -> m UseResult
effectPolyItem execSfx source target = do
  sb <- getsState $ getActorBody source
  let cstore = CGround
  allAssocs <- getsState $ fullAssocs target [cstore]
  case allAssocs of
    [] -> do
      execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxPurposeNothing cstore
      return UseId
    (iid, itemFull@ItemFull{..}) : _ -> case itemDisco of
      Just ItemDisco{itemKind, itemKindId} -> do
        let maxCount = Dice.maxDice $ IK.icount itemKind
        if | itemK < maxCount -> do
             execSfxAtomic $ SfxMsgFid (bfid sb)
                           $ SfxPurposeTooFew maxCount itemK
             return UseId
           | IK.Unique `elem` IK.ieffects itemKind -> do
             execSfxAtomic $ SfxMsgFid (bfid sb) SfxPurposeUnique
             return UseId
           | otherwise -> do
             -- Only the required number of items is used up, not all of them.
             let c = CActor target cstore
                 kit = (maxCount, take maxCount itemTimer)
             execSfx
             identifyIid iid c itemKindId
             execUpdAtomic $ UpdDestroyItem iid itemBase kit c
             effectCreateItem (Just $ bfid sb) Nothing
                              target cstore "useful" IK.timerNone
      _ -> error $ "" `showFailure` (target, iid, itemFull)

-- ** Identify

effectIdentify :: MonadServerAtomic m
               => m () -> ItemId -> ActorId -> ActorId -> m UseResult
effectIdentify execSfx iidId source target = do
  sb <- getsState $ getActorBody source
  s <- getsServer $ (EM.! bfid sb) . sclientStates
  let tryFull store as = case as of
        [] -> return False
        (iid, _) : rest | iid == iidId -> tryFull store rest  -- don't id itself
        (iid, ItemFull{itemBase, itemDisco=Just ItemDisco{..}}) : rest ->
          if iid `EM.member` sdiscoAspect s  -- already identified
             || itemConst  -- doesn't need further identification
                && jkindIx itemBase `EM.member` sdiscoKind s
             || store == CGround
                && (not $ any IK.forIdEffect $ IK.ieffects itemKind)
                  -- will be identified as soon as picked up, so don't bother;
                  -- darts included, which prevents wasting the scroll
             || maybe False (> 0) (lookup "gem" $ IK.ifreq itemKind)  -- a hack
          then tryFull store rest
          else do
            let c = CActor target store
            execSfx
            identifyIid iid c itemKindId
            return True
        _ -> error $ "" `showFailure` (store, as)
      tryStore stores = case stores of
        [] -> do
          execSfxAtomic $ SfxMsgFid (bfid sb) SfxIdentifyNothing
          return UseId  -- the message tells it's ID effect
        store : rest -> do
          allAssocs <- getsState $ fullAssocs target [store]
          go <- tryFull store allAssocs
          if go then return UseUp else tryStore rest
  tryStore [CGround, CEqp, CInv, CSha]

identifyIid :: MonadServerAtomic m
            => ItemId -> Container -> Kind.Id ItemKind -> m ()
identifyIid iid c itemKindId = do
  seed <- getsServer $ (EM.! iid) . sitemSeedD
  execUpdAtomic $ UpdDiscover c iid itemKindId seed

-- ** Detect

effectDetect :: MonadServerAtomic m
             => m () -> Int -> ActorId -> m UseResult
effectDetect = effectDetectX (const True) (const $ return False)

effectDetectX :: MonadServerAtomic m
              => (Point -> Bool) -> ([Point] -> m Bool)
              -> m () -> Int -> ActorId -> m UseResult
effectDetectX predicate action execSfx radius target = do
  b <- getsState $ getActorBody target
  Level{lxsize, lysize} <- getLevel $ blid b
  sperFidOld <- getsServer sperFid
  let perOld = sperFidOld EM.! bfid b EM.! blid b
      Point x0 y0 = bpos b
      perList = filter predicate
        [ Point x y
        | y <- [max 0 (y0 - radius) .. min (lysize - 1) (y0 + radius)]
        , x <- [max 0 (x0 - radius) .. min (lxsize - 1) (x0 + radius)]
        ]
      extraPer = emptyPer {psight = PerVisible $ ES.fromDistinctAscList perList}
      inPer = diffPer extraPer perOld
  unless (nullPer inPer) $ do
    -- Perception is modified on the server and sent to the client
    -- together with all the revealed info.
    let perNew = addPer inPer perOld
        fper = EM.adjust (EM.insert (blid b) perNew) (bfid b)
    modifyServer $ \ser -> ser {sperFid = fper $ sperFid ser}
    execSendPer (bfid b) (blid b) emptyPer inPer perNew
  pointsModified <- action perList
  if not (nullPer inPer) || pointsModified then do
    execSfx
    -- Perception is reverted. This is necessary to ensure save and restore
    -- doesn't change game state.
    unless (nullPer inPer) $ do
      modifyServer $ \ser -> ser {sperFid = sperFidOld}
      execSendPer (bfid b) (blid b) inPer emptyPer perOld
  else
    execSfxAtomic $ SfxMsgFid (bfid b) SfxVoidDetection
  return UseUp  -- even if nothing spotted, in itself it's still useful data

-- ** DetectActor

effectDetectActor :: MonadServerAtomic m
                  => m () -> Int -> ActorId -> m UseResult
effectDetectActor execSfx radius target = do
  b <- getsState $ getActorBody target
  Level{lactor} <- getLevel $ blid b
  effectDetectX (`EM.member` lactor) (const $ return False)
                execSfx radius target

-- ** DetectItem

effectDetectItem :: MonadServerAtomic m => m () -> Int -> ActorId -> m UseResult
effectDetectItem execSfx radius target = do
  b <- getsState $ getActorBody target
  Level{lfloor} <- getLevel $ blid b
  effectDetectX (`EM.member` lfloor) (const $ return False)
                execSfx radius target

-- ** DetectExit

effectDetectExit :: MonadServerAtomic m => m () -> Int -> ActorId -> m UseResult
effectDetectExit execSfx radius target = do
  b <- getsState $ getActorBody target
  Level{lstair=(ls1, ls2), lescape} <- getLevel $ blid b
  effectDetectX (`elem` ls1 ++ ls2 ++ lescape) (const $ return False)
                execSfx radius target

-- ** DetectHidden

effectDetectHidden :: MonadServerAtomic m
                   => m () -> Int -> ActorId -> Point -> m UseResult
effectDetectHidden execSfx radius target pos = do
  Kind.COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody target
  lvl <- getLevel $ blid b
  let predicate p = Tile.isHideAs coTileSpeedup $ lvl `at` p
      revealEmbed p = do
        embeds <- getsState $ getEmbedBag (blid b) p
        unless (null embeds) $ do
          s <- getState
          let ais = map (\iid -> (iid, getItemBody iid s)) (EM.keys embeds)
          execUpdAtomic $ UpdSpotItemBag (CEmbed (blid b) p) embeds ais
      action l = do
        let f p = when (p /= pos) $ do
              let t = lvl `at` p
              execUpdAtomic $ UpdSearchTile target p t
              -- This is safe searching; embedded items are not triggered,
              -- but they are revealed.
              revealEmbed p
        mapM_ f l
        return $! not $ null l
  effectDetectX predicate action execSfx radius target

-- ** SendFlying

-- | Send the target actor flying like a projectile. The arguments correspond
-- to @ToThrow@ and @Linger@ properties of items. If the actors are adjacent,
-- the vector is directed outwards, if no, inwards, if it's the same actor,
-- boldpos is used, if it can't, a random outward vector of length 10
-- is picked.
effectSendFlying :: MonadServerAtomic m
                 => m () -> IK.ThrowMod -> ActorId -> ActorId -> Maybe Bool
                 -> m UseResult
effectSendFlying execSfx IK.ThrowMod{..} source target modePush = do
  v <- sendFlyingVector source target modePush
  Kind.COps{coTileSpeedup} <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  lvl@Level{lxsize, lysize} <- getLevel (blid tb)
  let eps = 0
      fpos = bpos tb `shift` v
  if braced tb then do
    execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxBracedImmune target
    return UseId  -- the message reveals what's going on
  else case bla lxsize lysize eps (bpos tb) fpos of
    Nothing -> error $ "" `showFailure` (fpos, tb)
    Just [] -> error $ "projecting from the edge of level"
                       `showFailure` (fpos, tb)
    Just (pos : rest) -> do
      let t = lvl `at` pos
      if not $ Tile.isWalkable coTileSpeedup t
      then return UseDud  -- supported by a wall, not noticeable
      else do
        weightAssocs <- getsState $ fullAssocs target [CInv, CEqp, COrgan]
        let weight = sum $ map (jweight . itemBase . snd) weightAssocs
            path = bpos tb : pos : rest
            (trajectory, (speed, range)) =
              computeTrajectory weight throwVelocity throwLinger path
            ts = Just (trajectory, speed)
        if null trajectory || btrajectory tb == ts
           || throwVelocity <= 0 || throwLinger <= 0
        then return UseId  -- e.g., actor is too heavy; but a jerk is noticeable
        else do
          execSfx
          execUpdAtomic $ UpdTrajectory target (btrajectory tb) ts
          -- Give the actor back all the time spent flying (speed * range)
          -- and also let the push start ASAP. So, he will not lose
          -- any turn of movement (but he may need to retrace the push).
          let delta = timeDeltaScale (ticksPerMeter speed) (-range)
          modifyServer $ \ser ->
            ser {sactorTime = ageActor (bfid tb) (blid tb) target delta
                              $ sactorTime ser}
          return UseUp

sendFlyingVector :: MonadServerAtomic m
                 => ActorId -> ActorId -> Maybe Bool -> m Vector
sendFlyingVector source target modePush = do
  sb <- getsState $ getActorBody source
  let boldpos_sb = fromMaybe originPoint (boldpos sb)
  if source == target then
    if boldpos_sb == bpos sb then rndToAction $ do
      z <- randomR (-10, 10)
      oneOf [Vector 10 z, Vector (-10) z, Vector z 10, Vector z (-10)]
    else
      return $! vectorToFrom (bpos sb) boldpos_sb
  else do
    tb <- getsState $ getActorBody target
    let (sp, tp) = if adjacent (bpos sb) (bpos tb)
                   then let pos = if chessDist boldpos_sb (bpos tb)
                                     > chessDist (bpos sb) (bpos tb)
                                  then boldpos_sb  -- avoid cardinal dir
                                  else bpos sb
                        in (pos, bpos tb)
                   else (bpos sb, bpos tb)
        pushV = vectorToFrom tp sp
        pullV = vectorToFrom sp tp
    return $! case modePush of
                Just True -> pushV
                Just False -> pullV
                Nothing | adjacent (bpos sb) (bpos tb) -> pushV
                Nothing -> pullV

-- ** DropBestWeapon

-- | Make the target actor drop his best weapon (stack).
effectDropBestWeapon :: MonadServerAtomic m => m () -> ActorId -> m UseResult
effectDropBestWeapon execSfx target = do
  tb <- getsState $ getActorBody target
  localTime <- getsState $ getLocalTime (blid tb)
  allAssocsRaw <- getsState $ fullAssocs target [CEqp]
  let allAssocs = filter (isMelee . itemBase . snd) allAssocsRaw
  case strongestMelee Nothing localTime allAssocs of
    (_, (iid, _)) : _ -> do
      execSfx
      let kit = beqp tb EM.! iid
      dropCStoreItem True CEqp target tb 1 iid kit  -- not the whole stack
      return UseUp
    [] ->
      return UseDud

-- ** ActivateInv

-- | Activate all items with the given symbol
-- in the target actor's equipment (there's no variant that activates
-- a random one, to avoid the incentive for carrying garbage).
-- Only one item of each stack is activated (and possibly consumed).
effectActivateInv :: MonadServerAtomic m
                  => m () -> ActorId -> Char -> m UseResult
effectActivateInv execSfx target symbol = do
  let c = CActor target CInv
  effectTransformContainer execSfx symbol c $ \iid _ ->
    meleeEffectAndDestroy target target iid c

effectTransformContainer :: forall m. MonadServerAtomic m
                         => m () -> Char -> Container
                         -> (ItemId -> ItemQuant -> m ())
                         -> m UseResult
effectTransformContainer execSfx symbol c m = do
  let hasSymbol (iid, _) = do
        item <- getsState $ getItemBody iid
        return $! jsymbol item == symbol
  assocsCStore <- getsState $ EM.assocs . getContainerBag c
  is <- if symbol == ' '
        then return assocsCStore
        else filterM hasSymbol assocsCStore
  if null is
  then return UseDud
  else do
    execSfx
    mapM_ (uncurry m) is
    -- Even if no item produced any visible effect, rummaging through
    -- the inventory uses up the effect and produced discernible vibrations.
    return UseUp

-- ** ApplyPerfume

effectApplyPerfume :: MonadServerAtomic m => m () -> ActorId -> m UseResult
effectApplyPerfume execSfx target = do
  execSfx
  tb <- getsState $ getActorBody target
  Level{lsmell} <- getLevel $ blid tb
  let f p fromSm =
        execUpdAtomic $ UpdAlterSmell (blid tb) p fromSm timeZero
  mapWithKeyM_ f lsmell
  return UseUp  -- even if no smell before, the perfume is noticeable

-- ** OneOf

effectOneOf :: MonadServerAtomic m
            => (IK.Effect -> m UseResult) -> [IK.Effect] -> m UseResult
effectOneOf recursiveCall l = do
  let call1 = do
        ef <- rndToAction $ oneOf l
        recursiveCall ef
      call99 = replicate 99 call1
      f call result = do
        ur <- call
        -- We avoid 99 calls to a fizzling effect that only prints
        -- a failure message and IDs the item.
        if ur == UseDud then result else return ur
  foldr f (return UseDud) call99
  -- no @execSfx@, because individual effects sent them

-- ** Recharging

effectRecharging :: MonadServerAtomic m
                 => (IK.Effect -> m UseResult) -> IK.Effect -> Bool
                 -> m UseResult
effectRecharging recursiveCall e recharged =
  if recharged
  then recursiveCall e
  else return UseDud

-- ** Temporary

effectTemporary :: MonadServerAtomic m
                => m () -> ActorId -> ItemId -> Container -> m UseResult
effectTemporary execSfx source iid c = do
  case c of
    CActor _ COrgan -> do
      b <- getsState $ getActorBody source
      case iid `EM.lookup` borgan b of
        Just _ -> return ()  -- still some copies left of a multi-copy tmp organ
        Nothing -> execSfx  -- last copy just destroyed
    _ -> do
      execSfx
  return UseUp  -- temporary, so usually used up just by sitting there

-- ** Composite

effectComposite :: forall m. MonadServerAtomic m
                => (IK.Effect -> m UseResult) -> [IK.Effect] -> m UseResult
effectComposite recursiveCall l = do
  let f :: IK.Effect -> m UseResult -> m UseResult
      f eff result = do
        ur <- recursiveCall eff
        when (ur == UseUp) $ void $ result  -- UseResult comes from the first
        return ur
  foldr f (return UseDud) l  -- @True@ if any effect triggered
  -- no @execSfx@, because individual effects sent them
