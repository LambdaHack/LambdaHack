{-# LANGUAGE TupleSections #-}
-- | Handle effects (most often caused by requests sent by clients).
module Game.LambdaHack.Server.HandleEffectM
  ( applyItem, itemEffectAndDestroy, effectAndDestroy, itemEffectEmbedded
  , dropCStoreItem, dominateFidSfx, pickDroppable, cutCalm
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Bits (xor)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import Data.Key (mapWithKeyM_)

import Game.LambdaHack.Atomic
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK
import Game.LambdaHack.Server.CommonM
import Game.LambdaHack.Server.ItemM
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicM
import Game.LambdaHack.Server.State

-- + Semantics of effects

applyItem :: (MonadAtomic m, MonadServer m)
          => ActorId -> ItemId -> CStore -> m ()
applyItem aid iid cstore = do
  execSfxAtomic $ SfxApply aid iid cstore
  let c = CActor aid cstore
  itemEffectAndDestroy aid aid iid c

itemEffectAndDestroy :: (MonadAtomic m, MonadServer m)
                     => ActorId -> ActorId -> ItemId -> Container
                     -> m ()
itemEffectAndDestroy source target iid c = do
  bag <- getsState $ getContainerBag c
  case iid `EM.lookup` bag of
    Nothing -> assert `failure` (source, target, iid, c)
    Just kit -> do
      itemToF <- itemToFullServer
      let itemFull = itemToF iid kit
      case itemDisco itemFull of
        Just ItemDisco {itemKind=IK.ItemKind{IK.ieffects}} ->
          effectAndDestroy source target iid c False ieffects itemFull
        _ -> assert `failure` (source, target, iid, c)

effectAndDestroy :: (MonadAtomic m, MonadServer m)
                 => ActorId -> ActorId -> ItemId -> Container -> Bool
                 -> [IK.Effect] -> ItemFull
                 -> m ()
effectAndDestroy _ _ iid container periodic [] itemFull@ItemFull{..} =
  -- This case is a speedup only.
  if not periodic && jdamage itemBase /= 0 then do
    -- No effect triggered, but physical damage dealt.
    let (imperishable, kit) = imperishableKit [] periodic itemTimer itemFull
    unless imperishable $
      execUpdAtomic $ UpdLoseItem False iid itemBase kit container
  else return ()
effectAndDestroy source target iid container periodic effs
                 itemFull@ItemFull{..} = do
  let timeout = case itemDisco of
        Just ItemDisco{itemAspect=Just ar} -> aTimeout ar
        _ -> assert `failure` itemDisco
  lid <- getsState $ lidFromC container
  localTime <- getsState $ getLocalTime lid
  let it1 = let timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
                charging startT = timeShift startT timeoutTurns > localTime
            in filter charging itemTimer
      len = length it1
      recharged = len < itemK
      it2 = if timeout /= 0 && recharged then localTime : it1 else itemTimer
      !_A = assert (len <= itemK `blame` (source, target, iid, container)) ()
  -- We use up the charge even if eventualy every effect fizzles. Tough luck.
  -- At least we don't destroy the item in such case. Also, we ID it regardless.
  unless (itemTimer == it2) $
    execUpdAtomic $ UpdTimeItem iid container itemTimer it2
  -- If the activation is not periodic, trigger at least the effects
  -- that are not recharging and so don't depend on @recharged@.
  when (not periodic || recharged) $ do
    -- We have to destroy the item before the effect affects the item
    -- or the actor holding it or standing on it (later on we could
    -- lose track of the item and wouldn't be able to destroy it) .
    -- This is OK, because we don't remove the item type from various
    -- item dictionaries, just an individual copy from the container,
    -- so, e.g., the item can be identified after it's removed.
    let (imperishable, kit) = imperishableKit effs periodic it2 itemFull
    unless imperishable $
      execUpdAtomic $ UpdLoseItem False iid itemBase kit container
    -- At this point, the item is potentially no longer in container @c@,
    -- so we don't pass @c@ along.
    triggeredEffect <-
      itemEffectDisco source target iid container recharged periodic effs
    let triggered = triggeredEffect || jdamage itemBase /= 0
    -- If none of item's effects was performed, we try to recreate the item.
    -- Regardless, we don't rewind the time, because some info is gained
    -- (that the item does not exhibit any effects in the given context).
    unless (triggered || imperishable) $
      execUpdAtomic $ UpdSpotItem False iid itemBase kit container

imperishableKit :: [IK.Effect] -> Bool -> ItemTimer -> ItemFull
                -> (Bool, ItemQuant)
imperishableKit effs periodic it2 ItemFull{..} =
  let permanent = let tmpEffect :: IK.Effect -> Bool
                      tmpEffect IK.Temporary{} = True
                      tmpEffect (IK.Recharging IK.Temporary{}) = True
                      tmpEffect (IK.OnSmash IK.Temporary{}) = True
                      tmpEffect _ = False
                  in not $ any tmpEffect effs
      fragile = IK.Fragile `elem` jfeature itemBase
      durable = IK.Durable `elem` jfeature itemBase
      imperishable = durable && not fragile || periodic && permanent
      kit = if permanent || periodic then (1, take 1 it2) else (itemK, it2)
  in (imperishable, kit)

-- One item of each @iid@ is triggered at once. If there are more copies,
-- they are left to be triggered next time.
itemEffectEmbedded :: (MonadAtomic m, MonadServer m)
                   => ActorId -> Point -> ItemBag -> m ()
itemEffectEmbedded aid tpos bag = do
  sb <- getsState $ getActorBody aid
  let c = CEmbed (blid sb) tpos
      f iid = do
        -- No block against tile, hence unconditional.
        execSfxAtomic $ SfxTrigger aid tpos
        itemEffectAndDestroy aid aid iid c
  mapM_ f $ EM.keys bag

-- | The source actor affects the target actor, with a given item.
-- If any of the effects fires up, the item gets identified. This function
-- is mutually recursive with @effect@ and so it's a part of @Effect@
-- semantics.
--
-- Note that if we activate a durable item, e.g., armor, from the ground,
-- it will get identified, which is perfectly fine, until we wanto to add
-- sticky armor that can't be easily taken off (and, e.g., has some maluses).
itemEffectDisco :: (MonadAtomic m, MonadServer m)
                => ActorId -> ActorId -> ItemId -> Container -> Bool -> Bool
                -> [IK.Effect]
                -> m Bool
itemEffectDisco source target iid c recharged periodic effs = do
  discoKind <- getsServer sdiscoKind
  item <- getsState $ getItemBody iid
  case EM.lookup (jkindIx item) discoKind of
    Just KindMean{kmKind} -> do
      seed <- getsServer $ (EM.! iid) . sitemSeedD
      execUpdAtomic $ UpdDiscover c iid kmKind seed
      itemEffect source target iid c recharged periodic effs
    _ -> assert `failure` (source, target, iid, item)

itemEffect :: (MonadAtomic m, MonadServer m)
           => ActorId -> ActorId -> ItemId -> Container -> Bool -> Bool
           -> [IK.Effect]
           -> m Bool
itemEffect source target iid c recharged periodic effects = do
  trs <- mapM (effectSem source target iid c recharged periodic) effects
  let triggered = or trs
  sb <- getsState $ getActorBody source
  -- Announce no effect, which is rare and wastes time, so noteworthy.
  unless (triggered  -- some effect triggered, so feedback comes from them
          || periodic  -- don't spam from fizzled periodic effects
          || bproj sb  -- don't spam, projectiles can be very numerous
          || null (filter IK.properEffect effects)) $
    execSfxAtomic $ SfxMsgFid (bfid sb) SfxFizzles
  return triggered

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The item may or may not still be in the container.
-- The boolean result indicates if the effect actually fired up,
-- as opposed to fizzled.
effectSem :: (MonadAtomic m, MonadServer m)
          => ActorId -> ActorId -> ItemId -> Container -> Bool -> Bool
          -> IK.Effect
          -> m Bool
effectSem source target iid c recharged periodic effect = do
  let recursiveCall = effectSem source target iid c recharged periodic
  sb <- getsState $ getActorBody source
  pos <- getsState $ posFromC c
  -- @execSfx@ usually comes last in effect semantics, but not always
  -- and we are likely to introduce more variety.
  let execSfx = execSfxAtomic $ SfxEffect (bfid sb) target effect 0
  case effect of
    IK.ELabel _ -> return False
    IK.EqpSlot _ -> return False
    IK.Burn nDm -> effectBurn nDm source target
    IK.Explode t -> effectExplode execSfx t target
    IK.RefillHP p -> effectRefillHP False p source target
    IK.OverfillHP p -> effectRefillHP True p source target
    IK.RefillCalm p -> effectRefillCalm False execSfx p source target
    IK.OverfillCalm p -> effectRefillCalm True execSfx p source target
    IK.Dominate -> effectDominate recursiveCall source target
    IK.Impress -> effectImpress recursiveCall execSfx source target
    IK.Summon grp p -> effectSummon execSfx grp p source target periodic
    IK.Ascend p -> effectAscend recursiveCall execSfx p source target pos
    IK.Escape{} -> effectEscape source target
    IK.Paralyze p -> effectParalyze execSfx p target
    IK.InsertMove p -> effectInsertMove execSfx p target
    IK.Teleport p -> effectTeleport execSfx p source target
    IK.CreateItem store grp tim -> effectCreateItem Nothing target store grp tim
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
    IK.OnSmash _ -> return False  -- ignored under normal circumstances
    IK.Recharging e -> effectRecharging recursiveCall e recharged
    IK.Temporary _ -> effectTemporary execSfx source iid c
    IK.Unique -> return False
    IK.Periodic -> return False

-- + Individual semantic functions for effects

-- ** Burn

-- Damage from fire. Not affected by armor.
effectBurn :: (MonadAtomic m, MonadServer m)
           => Dice.Dice -> ActorId -> ActorId
           -> m Bool
effectBurn nDm source target = do
  tb <- getsState $ getActorBody target
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
      hpMax = aMaxHP ar
  n <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  let rawDeltaHP = - (fromIntegral $ xM n)
      -- We ignore minor burns.
      serious = not (bproj tb) && source /= target && n > 1
      deltaHP | serious = -- if HP overfull, at least cut back to max HP
                          min rawDeltaHP (xM hpMax - bhp tb)
              | otherwise = rawDeltaHP
  if deltaHP == 0
  then return False
  else do
    sb <- getsState $ getActorBody source
    -- Display the effect.
    let reportedEffect = IK.Burn $ Dice.intToDice n
    execSfxAtomic $ SfxEffect (bfid sb) target reportedEffect deltaHP
    -- Damage the target.
    execUpdAtomic $ UpdRefillHP target deltaHP
    when serious $ cutCalm target
    return True

-- ** Explode

effectExplode :: (MonadAtomic m, MonadServer m)
              => m () -> GroupName ItemKind -> ActorId -> m Bool
effectExplode execSfx cgroup target = do
  execSfx
  tb <- getsState $ getActorBody target
  let itemFreq = [(cgroup, 1)]
      container = CActor target COrgan
  m2 <- rollAndRegisterItem (blid tb) itemFreq container False Nothing
  let (iid, (ItemFull{itemBase, itemK}, _)) =
        fromMaybe (assert `failure` cgroup) m2
      Point x y = bpos tb
      semirandom = fromEnum (jkindIx itemBase)
      projectN k100 (n, _) = do
        -- We pick a point at the border, not inside, to have a uniform
        -- distribution for the points the line goes through at each distance
        -- from the source. Otherwise, e.g., the points on cardinal
        -- and diagonal lines from the source would be more common.
        let veryrandom = k100 `xor` (semirandom + n)
            fuzz = 5 + veryrandom `mod` 5
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
            ps = take k psAll
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
  return True  -- we neglect verifying that at least one projectile got off

-- ** RefillHP

-- Unaffected by armor.
effectRefillHP :: (MonadAtomic m, MonadServer m)
               => Bool -> Int -> ActorId -> ActorId -> m Bool
effectRefillHP overfill power source target = do
  tb <- getsState $ getActorBody target
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
      hpMax = aMaxHP ar
      overMax | overfill = xM hpMax * 10  -- arbitrary limit to scumming
              | otherwise = xM hpMax
      -- We ignore light poison and similar -1HP per turn annoyances.
      serious = not (bproj tb) && source /= target && abs power > 1
      deltaHP | power > 0 = min (xM power) (max 0 $ overMax - bhp tb)
              | serious = -- if overfull, at least cut back to max
                          min (xM power) (xM hpMax - bhp tb)
              | otherwise = xM power
  if deltaHP == 0
  then return False
  else do
    sb <- getsState $ getActorBody source
    let reportedEffect | overfill = IK.OverfillHP power
                       | otherwise = IK.RefillHP power
    execSfxAtomic $ SfxEffect (bfid sb) target reportedEffect deltaHP
    execUpdAtomic $ UpdRefillHP target deltaHP
    when (deltaHP < 0 && serious) $ cutCalm target
    return True

cutCalm :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
cutCalm target = do
  tb <- getsState $ getActorBody target
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
      upperBound = if hpTooLow tb ar
                   then 0  -- to trigger domination, etc.
                   else xM $ aMaxCalm ar
      deltaCalm = min minusM1 (upperBound - bcalm tb)
  -- HP loss decreases Calm by at least @minusM1@ to avoid "hears something",
  -- which is emitted when decreasing Calm by @minusM@.
  udpateCalm target deltaCalm

-- ** RefillCalm

effectRefillCalm ::  (MonadAtomic m, MonadServer m)
                 => Bool -> m () -> Int -> ActorId -> ActorId -> m Bool
effectRefillCalm overfill execSfx power source target = do
  tb <- getsState $ getActorBody target
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
      calmMax = aMaxCalm ar
      overMax | overfill = xM calmMax * 10  -- arbitrary limit to scumming
              | otherwise = xM calmMax
      serious = not (bproj tb) && source /= target && power > 1
      deltaCalm | power > 0 = min (xM power) (max 0 $ overMax - bcalm tb)
                | serious = -- if overfull, at least cut back to max
                            min (xM power) (xM calmMax - bcalm tb)
                | otherwise = xM power
  if deltaCalm == 0
    then return False
    else do
      execSfx
      udpateCalm target deltaCalm
      return True

-- ** Dominate

effectDominate :: (MonadAtomic m, MonadServer m)
               => (IK.Effect -> m Bool)
               -> ActorId -> ActorId
               -> m Bool
effectDominate recursiveCall source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if | bproj tb -> return False
     | bfid tb == bfid sb ->
       -- Dominate is rather on projectiles than on items, so alternate effect
       -- is useful to avoid boredom if domination can't happen.
       recursiveCall IK.Impress
     | otherwise -> dominateFidSfx (bfid sb) target

dominateFidSfx :: (MonadAtomic m, MonadServer m)
               => FactionId -> ActorId -> m Bool
dominateFidSfx fid target = do
  tb <- getsState $ getActorBody target
  -- Actors that don't move freely can't be dominated, for otherwise,
  -- when they are the last survivors, they could get stuck
  -- and the game wouldn't end.
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
      actorMaxSk = aSkills ar
      -- Check that the actor can move, also between levels and through doors.
      -- Otherwise, it's too awkward for human player to control.
      canMove = EM.findWithDefault 0 Ability.AbMove actorMaxSk > 0
                && EM.findWithDefault 0 Ability.AbAlter actorMaxSk
                   >= fromEnum TK.talterForStairs
  if canMove && not (bproj tb) then do
    let execSfx = execSfxAtomic $ SfxEffect fid target IK.Dominate 0
    execSfx  -- if actor ours, possibly the last occasion to see him
    dominateFid fid target
    execSfx  -- see the actor as theirs, unless position not visible
    return True
  else
    return False

dominateFid :: (MonadAtomic m, MonadServer m) => FactionId -> ActorId -> m ()
dominateFid fid target = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  tb0 <- getsState $ getActorBody target
  deduceKilled target  -- the actor body exists and his items are not dropped
  electLeader (bfid tb0) (blid tb0) target
  fact <- getsState $ (EM.! bfid tb0) . sfactionD
  -- Prevent the faction's stash from being lost in case they are not spawners.
  when (isNothing $ _gleader fact) $ moveStores False target CSha CInv
  tb <- getsState $ getActorBody target
  ais <- getsState $ getCarriedAssocs tb
  actorAspect <- getsServer sactorAspect
  getItem <- getsState $ flip getItemBody
  discoKind <- getsServer sdiscoKind
  let ar = actorAspect EM.! target
      isImpression iid = case EM.lookup (jkindIx $ getItem iid) discoKind of
        Just KindMean{kmKind} ->
          maybe False (> 0) $ lookup "impressed" $ IK.ifreq (okind kmKind)
        Nothing -> assert `failure` iid
      dropAllImpressions = EM.filterWithKey (\iid _ -> not $ isImpression iid)
      borganNoImpression = dropAllImpressions $ borgan tb
  btime <-
    getsServer $ (EM.! target) . (EM.! blid tb) . (EM.! bfid tb) . sactorTime
  execUpdAtomic $ UpdLoseActor target tb ais
  let bNew = tb { bfid = fid
                , bcalm = max (xM 10) $ xM (aMaxCalm ar) `div` 2
                , bhp = min (xM $ aMaxHP ar) $ bhp tb + xM 10
                , borgan = borganNoImpression}
  aisNew <- getsState $ getCarriedAssocs bNew
  execUpdAtomic $ UpdSpotActor target bNew aisNew
  -- Add some nostalgia for the old faction.
  void $ effectCreateItem (Just (bfid tb, 10)) target COrgan
                          "impressed" IK.TimerNone
  modifyServer $ \ser ->
    ser {sactorTime = updateActorTime fid (blid tb) target btime
                      $ sactorTime ser}
  let discoverSeed (iid, cstore) = do
        seed <- getsServer $ (EM.! iid) . sitemSeedD
        let c = CActor target cstore
        execUpdAtomic $ UpdDiscoverSeed c iid seed
      aic = getCarriedIidCStore tb
  mapM_ discoverSeed aic
  -- Focus on the dominated actor, by making him a leader.
  supplantLeader fid target

-- ** Impress

effectImpress :: (MonadAtomic m, MonadServer m)
              => (IK.Effect -> m Bool) -> m () -> ActorId -> ActorId -> m Bool
effectImpress recursiveCall execSfx source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if | bproj tb -> return False
     | bfid tb == bfid sb -> do
       -- Unimpress wrt others, but only once.
       res <- recursiveCall $ IK.DropItem 1 1 COrgan "impressed"
       when res execSfx
       return res
     | otherwise -> do
       execSfx
       effectCreateItem (Just (bfid sb, 1)) target COrgan
                        "impressed" IK.TimerNone

-- ** Summon

-- Note that the Calm expended doesn't depend on the number of actors summoned.
effectSummon :: (MonadAtomic m, MonadServer m)
             => m () -> GroupName ItemKind -> Dice.Dice -> ActorId -> ActorId
             -> Bool
             -> m Bool
effectSummon execSfx grp nDm source target periodic = do
  -- Obvious effect, nothing announced.
  Kind.COps{coTileSpeedup} <- getsState scops
  power <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! source
  -- Verify Calm only at periodic activations. Otherwise summon uses up
  -- the item, which prevent summoning getting out of control
  -- (unless the item is durable, but normally it shouldn't be).
  -- I don't verify Calm otherwise, to prevent an exploit via draining
  -- one's calm on purpose when an item with good activation has a nasty
  -- summoning side-effect.
  if periodic && not (bproj sb) && not (calmEnough sb ar) then do
    unless (bproj sb) $ do
      execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxSummonLackCalm source
    return False
  else do
    execSfx
    let deltaCalm = - xM 30
    unless (bproj sb) $ udpateCalm source deltaCalm
    let validTile t = not $ Tile.isNoActor coTileSpeedup t
    ps <- getsState $ nearbyFreePoints validTile (bpos tb) (blid tb)
    localTime <- getsState $ getLocalTime (blid tb)
    -- Make sure summoned actors start acting after the victim.
    let targetTime = timeShift localTime $ ticksPerMeter $ bspeed tb ar
        afterTime = timeShift targetTime $ Delta timeClip
    bs <- forM (take power ps) $ \p -> do
      maid <- addAnyActor [(grp, 1)] (blid tb) afterTime (Just p)
      case maid of
        Nothing -> return False  -- not enough space in dungeon?
        Just aid -> do
          b <- getsState $ getActorBody aid
          mleader <- getsState $ _gleader . (EM.! bfid b) . sfactionD
          when (isNothing mleader) $ supplantLeader (bfid b) aid
          return True
    return $! or bs

-- ** Ascend

-- Note that projectiles can be teleported, too, for extra fun.
effectAscend :: (MonadAtomic m, MonadServer m)
             => (IK.Effect -> m Bool)
             -> m () -> Bool -> ActorId -> ActorId -> Point
             -> m Bool
effectAscend recursiveCall execSfx up source target pos = do
  b1 <- getsState $ getActorBody target
  let lid1 = blid b1
  (lid2, pos2) <- getsState $ whereTo lid1 pos (Just up) . sdungeon
  sb <- getsState $ getActorBody source
  if | braced b1 -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxBracedImmune target
       return False
     | lid2 == lid1 && pos2 == pos -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxLevelNoMore
       -- We keep it useful even in shallow dungeons.
       recursiveCall $ IK.Teleport 30  -- powerful teleport
     | otherwise -> do
       execSfx
       btime_bOld <- getsServer $ (EM.! target) . (EM.! lid1)
                       . (EM.! bfid b1) . sactorTime
       pos3 <- findStairExit up lid2 pos2
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
       return True

findStairExit :: MonadStateRead m => Bool -> LevelId -> Point -> m Point
findStairExit moveUp lid pos = do
  Kind.COps{coTileSpeedup} <- getsState scops
  lvl <- getLevel lid
  let defLanding = uncurry Vector $ if moveUp then (-1, 0) else (1, 0)
      (mvs2, mvs1) = break (== defLanding) moves
      mvs = mvs1 ++ mvs2
  unocc <- getsState $ \s p -> Tile.isWalkable coTileSpeedup (lvl `at` p)
                               && null (posToAssocs p lid s)
  case find unocc $ map (shift pos) mvs of
    Nothing -> return $! shift pos defLanding
    Just posRes -> return posRes

switchLevels1 :: MonadAtomic m => (ActorId, Actor) -> m (Maybe ActorId)
switchLevels1 (aid, bOld) = do
  let side = bfid bOld
  mleader <- getsState $ _gleader . (EM.! side) . sfactionD
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
  ais <- getsState $ getCarriedAssocs bOld
  execUpdAtomic $ UpdLoseActor aid bOld ais
  return mlead

switchLevels2 ::(MonadAtomic m, MonadServer m)
              => LevelId -> Point -> (ActorId, Actor) -> Time -> Maybe ActorId
              -> m ()
switchLevels2 lidNew posNew (aid, bOld) btime_bOld mlead = do
  let lidOld = blid bOld
      side = bfid bOld
  let !_A = assert (lidNew /= lidOld `blame` "stairs looped" `twith` lidNew) ()
  -- Sync the actor time with the level time.
  timeOld <- getsState $ getLocalTime lidOld
  timeLastActive <- getsState $ getLocalTime lidNew
  -- This time calculation may cause a double move of a foe of the same
  -- speed, but this is OK --- the foe didn't have a chance to move
  -- before, because the arena went inactive, so he moves now one more time.
  let delta = timeLastActive `timeDeltaToFrom` timeOld
      shiftByDelta = (`timeShift` delta)
      computeNewTimeout :: ItemQuant -> ItemQuant
      computeNewTimeout (k, it) = (k, map shiftByDelta it)
      setTimeout :: ItemBag -> ItemBag
      setTimeout = EM.map computeNewTimeout
      bNew = bOld { blid = lidNew
                  , bpos = posNew
                  , boldpos = Just posNew  -- new level, new direction
                  , borgan = setTimeout $ borgan bOld
                  , beqp = setTimeout $ beqp bOld }
  -- Materialize the actor at the new location.
  -- Onlookers see somebody appear suddenly. The actor himself
  -- sees new surroundings and has to reset his perception.
  ais <- getsState $ getCarriedAssocs bOld
  execUpdAtomic $ UpdCreateActor aid bNew ais
  let btime = shiftByDelta $ btime_bOld
  modifyServer $ \ser ->
    ser {sactorTime = updateActorTime (bfid bNew) lidNew aid btime $ sactorTime ser}
  case mlead of
    Nothing -> return ()
    Just leader -> supplantLeader side leader

-- ** Escape

-- | The faction leaves the dungeon.
effectEscape :: (MonadAtomic m, MonadServer m) => ActorId -> ActorId -> m Bool
effectEscape source target = do
  -- Obvious effect, nothing announced.
  sb <- getsState $ getActorBody source
  b <- getsState $ getActorBody target
  let fid = bfid b
  fact <- getsState $ (EM.! fid) . sfactionD
  if | bproj b ->
       return False
     | not (fcanEscape $ gplayer fact) -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxEscapeImpossible
       return False
     | otherwise -> do
       deduceQuits (bfid b) $ Status Escape (fromEnum $ blid b) Nothing
       return True

-- ** Paralyze

-- | Advance target actor time by this many time clips. Not by actor moves,
-- to hurt fast actors more.
effectParalyze :: (MonadAtomic m, MonadServer m)
               => m () -> Dice.Dice -> ActorId -> m Bool
effectParalyze execSfx nDm target = do
  p <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  b <- getsState $ getActorBody target
  if bproj b || bhp b <= 0
    then return False
    else do
      execSfx
      let t = timeDeltaScale (Delta timeClip) p
      modifyServer $ \ser ->
        ser {sactorTime = ageActor (bfid b) (blid b) target t $ sactorTime ser}
      return True

-- ** InsertMove

-- | Give target actor the given number of extra moves. Don't give
-- an absolute amount of time units, to benefit slow actors more.
effectInsertMove :: (MonadAtomic m, MonadServer m)
                 => m () -> Dice.Dice -> ActorId -> m Bool
effectInsertMove execSfx nDm target = do
  execSfx
  p <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  b <- getsState $ getActorBody target
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
  let tpm = ticksPerMeter $ bspeed b ar
      t = timeDeltaScale tpm (-p)
  modifyServer $ \ser ->
    ser {sactorTime = ageActor (bfid b) (blid b) target t $ sactorTime ser}
  return True

-- ** Teleport

-- | Teleport the target actor.
-- Note that projectiles can be teleported, too, for extra fun.
effectTeleport :: (MonadAtomic m, MonadServer m)
               => m () -> Dice.Dice -> ActorId -> ActorId -> m Bool
effectTeleport execSfx nDm source target = do
  Kind.COps{coTileSpeedup} <- getsState scops
  range <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  sb <- getsState $ getActorBody source
  b <- getsState $ getActorBody target
  Level{ltile} <- getLevel (blid b)
  let spos = bpos b
      dMinMax delta pos =
        let d = chessDist spos pos
        in d >= range - delta && d <= range + delta
      dist delta pos _ = dMinMax delta pos
  lvl <- getLevel (blid b)
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
  if | braced b -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxBracedImmune target
       return False
     | not (dMinMax 9 tpos) -> do  -- very rare
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxTransImpossible
       return False
     | otherwise -> do
       execSfx
       execUpdAtomic $ UpdMoveActor target spos tpos
       return True

-- ** CreateItem

effectCreateItem :: (MonadAtomic m, MonadServer m)
                 => Maybe (FactionId, Int) -> ActorId -> CStore
                 -> GroupName ItemKind -> IK.TimerDice
                 -> m Bool
effectCreateItem mfidSource target store grp tim = do
  tb <- getsState $ getActorBody target
  delta <- case tim of
    IK.TimerNone -> return $ Delta timeZero
    IK.TimerGameTurn nDm -> do
      k <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
      let !_A = assert (k >= 0) ()
      return $! timeDeltaScale (Delta timeTurn) k
    IK.TimerActorTurn nDm -> do
      k <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
      let !_A = assert (k >= 0) ()
      actorAspect <- getsServer sactorAspect
      let ar = actorAspect EM.! target
          actorTurn = ticksPerMeter $ bspeed tb ar
      return $! timeDeltaScale actorTurn k
  let c = CActor target store
  bagBefore <- getsState $ getBodyStoreBag tb store
  let litemFreq = [(grp, 1)]
  -- Power depth of new items unaffected by number of spawned actors.
  m5 <- rollItem 0 (blid tb) litemFreq
  let (itemKnownRaw, itemFullRaw, _, seed, _) =
        fromMaybe (assert `failure` (blid tb, litemFreq, c)) m5
      (itemKnown, itemFull) = case mfidSource of
        Just (fidSource, k) ->
          let (kindIx, ar, damage, _) = itemKnownRaw
              jfid = Just fidSource
          in ( (kindIx, ar, damage, jfid)
             , itemFullRaw { itemBase = (itemBase itemFullRaw) {jfid}
                           , itemK = k })
        Nothing -> (itemKnownRaw, itemFullRaw)
  itemRev <- getsServer sitemRev
  let mquant = case HM.lookup itemKnown itemRev of
        Nothing -> Nothing
        Just iid -> (iid,) <$> iid `EM.lookup` bagBefore
  case mquant of
    Just (iid, (1, afterIt@(timer : rest))) | tim /= IK.TimerNone -> do
      -- Already has such an item, so only increase the timer by the amount.
      let newIt = timer `timeShift` delta : rest
      when (afterIt /= newIt) $
        execUpdAtomic $ UpdTimeItem iid c afterIt newIt
    _ -> do
      -- Multiple such items, so it's a periodic poison, etc., so just stack,
      -- or no such items at all, so create some.
      iid <- registerItem itemFull itemKnown seed c True
      unless (tim == IK.TimerNone) $ do
        tb2 <- getsState $ getActorBody target
        bagAfter <- getsState $ getBodyStoreBag tb2 store
        localTime <- getsState $ getLocalTime (blid tb)
        let newTimer = localTime `timeShift` delta
            (afterK, afterIt) =
              fromMaybe (assert `failure` (iid, bagAfter, c))
                        (iid `EM.lookup` bagAfter)
            newIt = replicate afterK newTimer
        when (afterIt /= newIt) $
          execUpdAtomic $ UpdTimeItem iid c afterIt newIt
  return True

-- ** DropItem

-- | Make the target actor drop all items in a store from the given group
-- (not just a random single item, or cluttering equipment with rubbish
-- would be beneficial).
effectDropItem :: (MonadAtomic m, MonadServer m)
               => m () -> Int ->Int ->  CStore -> GroupName ItemKind -> ActorId
               -> m Bool
effectDropItem execSfx n k store grp target = do
  b <- getsState $ getActorBody target
  is <- allGroupItems store grp target
  if null is then return False
  else do
    unless (store == COrgan) execSfx
    mapM_ (uncurry (dropCStoreItem True store target b k)) $ take n is
    return True

allGroupItems :: (MonadAtomic m, MonadServer m)
              => CStore -> GroupName ItemKind -> ActorId
              -> m [(ItemId, ItemQuant)]
allGroupItems store grp target = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  discoKind <- getsServer sdiscoKind
  b <- getsState $ getActorBody target
  let hasGroup (iid, _) = do
        item <- getsState $ getItemBody iid
        case EM.lookup (jkindIx item) discoKind of
          Just KindMean{kmKind} ->
            return $! maybe False (> 0) $ lookup grp $ IK.ifreq (okind kmKind)
          Nothing ->
            assert `failure` (target, grp, iid, item)
  assocsCStore <- getsState $ EM.assocs . getBodyStoreBag b store
  filterM hasGroup assocsCStore

-- | Drop a single actor's item. Note that if there are multiple copies,
-- at most one explodes to avoid excessive carnage and UI clutter
-- (let's say, the multiple explosions interfere with each other or perhaps
-- larger quantities of explosives tend to be packaged more safely).
dropCStoreItem :: (MonadAtomic m, MonadServer m)
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
    itemToF <- itemToFullServer
    let itemFull = itemToF iid kit
        effs = strengthOnSmash itemFull
    effectAndDestroy aid aid iid c False effs itemFull
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

effectPolyItem :: (MonadAtomic m, MonadServer m)
               => m () -> ActorId -> ActorId -> m Bool
effectPolyItem execSfx source target = do
  sb <- getsState $ getActorBody source
  let cstore = CGround
  allAssocs <- fullAssocsServer target [cstore]
  case allAssocs of
    [] -> do
      execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxPurposeNothing cstore
      return False
    (iid, itemFull@ItemFull{..}) : _ -> case itemDisco of
      Just ItemDisco{itemKind, itemKindId} -> do
        let maxCount = Dice.maxDice $ IK.icount itemKind
        if | itemK < maxCount -> do
             execSfxAtomic $ SfxMsgFid (bfid sb)
                           $ SfxPurposeTooFew maxCount itemK
             return False
           | IK.Unique `elem` IK.ieffects itemKind -> do
             execSfxAtomic $ SfxMsgFid (bfid sb) SfxPurposeUnique
             return False
           | otherwise -> do
             let c = CActor target cstore
                 kit = (maxCount, take maxCount itemTimer)
             execSfx
             identifyIid iid c itemKindId
             execUpdAtomic $ UpdDestroyItem iid itemBase kit c
             effectCreateItem Nothing target cstore "useful" IK.TimerNone
      _ -> assert `failure` (target, iid, itemFull)

-- ** Identify

effectIdentify :: (MonadAtomic m, MonadServer m)
               => m () -> ItemId -> ActorId -> ActorId -> m Bool
effectIdentify execSfx iidId source target = do
  sb <- getsState $ getActorBody source
  let tryFull store as = case as of
        [] -> do
          execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxIdentifyNothing store
          return False
        (iid, _) : rest | iid == iidId -> tryFull store rest  -- don't id itself
        (iid, ItemFull{itemDisco=Just ItemDisco{..}}) : rest -> do
          -- We avoid identifying trivial items, but they may also be right
          -- in the middle of bonus ranges, to if no other option, id them;
          -- client will ignore them if really trivial.
          let ided = IK.Identified `elem` IK.ifeature itemKind
              statsObvious = Just itemAspectMean == itemAspect
          if ided && statsObvious && not (null rest)
            then tryFull store rest
            else do
              let c = CActor target store
              execSfx
              identifyIid iid c itemKindId
              return True
        _ -> assert `failure` (store, as)
      tryStore stores = case stores of
        [] -> return False
        store : rest -> do
          allAssocs <- fullAssocsServer target [store]
          go <- tryFull store allAssocs
          if go then return True else tryStore rest
  tryStore [CGround]

identifyIid :: (MonadAtomic m, MonadServer m)
            => ItemId -> Container -> Kind.Id ItemKind -> m ()
identifyIid iid c itemKindId = do
  seed <- getsServer $ (EM.! iid) . sitemSeedD
  execUpdAtomic $ UpdDiscover c iid itemKindId seed

-- ** Detect

effectDetect :: (MonadAtomic m, MonadServer m)
             => m () -> Int -> ActorId -> m Bool
effectDetect execSfx radius target =
  effectDetectX (const True) (const $ return False) execSfx radius target

effectDetectX :: (MonadAtomic m, MonadServer m)
              => (Point -> Bool) -> ([Point] -> m Bool)
              -> m () -> Int -> ActorId -> m Bool
effectDetectX predicate action execSfx radius target = do
  b <- getsState $ getActorBody target
  Level{lxsize, lysize} <- getLevel $ blid b
  sperFidOld <- getsServer sperFid
  let perOld = sperFidOld EM.! bfid b EM.! blid b
      Point x0 y0 = bpos b
      perList = filter predicate $
        [ Point x y
        | y <- [max 0 (y0 - radius) .. min (lysize - 1) (y0 + radius)]
        , x <- [max 0 (x0 - radius) .. min (lxsize - 1) (x0 + radius)]
        ]
      extraPer = emptyPer {psight = PerVisible $ ES.fromDistinctAscList perList}
      inPer = diffPer extraPer perOld
  perModified <- if nullPer inPer then return False else do
    -- Perception is modified on the server and sent to the client
    -- together with all the revealed info.
    let perNew = addPer inPer perOld
        fper = EM.adjust (EM.insert (blid b) perNew) (bfid b)
    modifyServer $ \ser -> ser {sperFid = fper $ sperFid ser}
    execSendPer (bfid b) (blid b) emptyPer inPer perNew
    return True
  pointsModified <- action perList
  if perModified || pointsModified then do
    execSfx
    -- Perception is reverted. This is necessary to ensure save and restore
    -- doesn't change game state.
    when perModified $ do
      modifyServer $ \ser -> ser {sperFid = sperFidOld}
      execSendPer (bfid b) (blid b) inPer emptyPer perOld
  else
    execSfxAtomic $ SfxMsgFid (bfid b) SfxVoidDetection
  return True  -- even if nothing spotted, in itself it's still useful data

-- ** DetectActor

effectDetectActor :: (MonadAtomic m, MonadServer m)
                  => m () -> Int -> ActorId -> m Bool
effectDetectActor execSfx radius target = do
  b <- getsState $ getActorBody target
  Level{lactor} <- getLevel $ blid b
  effectDetectX (`EM.member` lactor) (const $ return False)
                execSfx radius target

-- ** DetectItem

effectDetectItem :: (MonadAtomic m, MonadServer m)
                 => m () -> Int -> ActorId -> m Bool
effectDetectItem execSfx radius target = do
  b <- getsState $ getActorBody target
  Level{lfloor} <- getLevel $ blid b
  effectDetectX (`EM.member` lfloor) (const $ return False)
                execSfx radius target

-- ** DetectExit

effectDetectExit :: (MonadAtomic m, MonadServer m)
                 => m () -> Int -> ActorId -> m Bool
effectDetectExit execSfx radius target = do
  b <- getsState $ getActorBody target
  Level{lstair=(ls1, ls2), lescape} <- getLevel $ blid b
  effectDetectX (`elem` ls1 ++ ls2 ++ lescape) (const $ return False)
                execSfx radius target

-- ** DetectHidden

effectDetectHidden :: (MonadAtomic m, MonadServer m)
                   => m () -> Int -> ActorId -> Point -> m Bool
effectDetectHidden execSfx radius target pos = do
  Kind.COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody target
  lvl <- getLevel $ blid b
  let predicate p = Tile.isHideAs coTileSpeedup $ lvl `at` p
      action l = do
        let f p = when (p /= pos)
                  $ execUpdAtomic $ UpdSearchTile target p $ lvl `at` p
        mapM_ f l
        return $! not $ null l
  effectDetectX predicate action execSfx radius target

-- ** SendFlying

-- | Shend the target actor flying like a projectile. The arguments correspond
-- to @ToThrow@ and @Linger@ properties of items. If the actors are adjacent,
-- the vector is directed outwards, if no, inwards, if it's the same actor,
-- boldpos is used, if it can't, a random outward vector of length 10
-- is picked.
effectSendFlying :: (MonadAtomic m, MonadServer m)
                 => m () -> IK.ThrowMod
                 -> ActorId -> ActorId -> Maybe Bool
                 -> m Bool
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
    return False
  else case bla lxsize lysize eps (bpos tb) fpos of
    Nothing -> assert `failure` (fpos, tb)
    Just [] -> assert `failure` "projecting from the edge of level"
                      `twith` (fpos, tb)
    Just (pos : rest) -> do
      let t = lvl `at` pos
      if not $ Tile.isWalkable coTileSpeedup t
        then return False  -- supported by a wall
        else do
          weightAssocs <- fullAssocsServer target [CInv, CEqp, COrgan]
          let weight = sum $ map (jweight . itemBase . snd) weightAssocs
              path = bpos tb : pos : rest
              (trajectory, (speed, _)) =
                computeTrajectory weight throwVelocity throwLinger path
              ts = Just (trajectory, speed)
          if null trajectory || btrajectory tb == ts
             || throwVelocity <= 0 || throwLinger <= 0
            then return False  -- e.g., actor is too heavy; OK
            else do
              execSfx
              execUpdAtomic $ UpdTrajectory target (btrajectory tb) ts
              -- Give the actor one extra turn and also let the push start ASAP.
              -- So, if the push lasts one (his) turn, he will not lose
              -- any turn of movement (but he may need to retrace the push).
              actorAspect <- getsServer sactorAspect
              let ar = actorAspect EM.! target
                  tpm = ticksPerMeter $ bspeed tb ar
                  delta = timeDeltaScale tpm (-1)
              modifyServer $ \ser ->
                ser {sactorTime = ageActor (bfid tb) (blid tb) target delta
                                  $ sactorTime ser}
              return True

sendFlyingVector :: (MonadAtomic m, MonadServer m)
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
effectDropBestWeapon :: (MonadAtomic m, MonadServer m)
                     => m () -> ActorId -> m Bool
effectDropBestWeapon execSfx target = do
  tb <- getsState $ getActorBody target
  allAssocs <- fullAssocsServer target [CEqp]
  localTime <- getsState $ getLocalTime (blid tb)
  case strongestMelee False localTime allAssocs of
    (_, (iid, _)) : _ -> do
      execSfx
      let kit = beqp tb EM.! iid
      dropCStoreItem True CEqp target tb 1 iid kit  -- not the whole stack
      return True
    [] ->
      return False

-- ** ActivateInv

-- | Activate all items with the given symbol
-- in the target actor's equipment (there's no variant that activates
-- a random one, to avoid the incentive for carrying garbage).
-- Only one item of each stack is activated (and possibly consumed).
effectActivateInv :: (MonadAtomic m, MonadServer m)
                  => m () -> ActorId -> Char -> m Bool
effectActivateInv execSfx target symbol =
  effectTransformEqp execSfx target symbol CInv $ \iid _ ->
    applyItem target iid CInv

effectTransformEqp :: forall m. MonadAtomic m
                   => m () -> ActorId -> Char -> CStore
                   -> (ItemId -> ItemQuant -> m ())
                   -> m Bool
effectTransformEqp execSfx target symbol cstore m = do
  b <- getsState $ getActorBody target
  let hasSymbol (iid, _) = do
        item <- getsState $ getItemBody iid
        return $! jsymbol item == symbol
  assocsCStore <- getsState $ EM.assocs . getBodyStoreBag b cstore
  is <- if symbol == ' '
        then return assocsCStore
        else filterM hasSymbol assocsCStore
  if null is
    then return False
    else do
      execSfx
      mapM_ (uncurry m) is
      return True

-- ** ApplyPerfume

effectApplyPerfume :: MonadAtomic m
                   => m () -> ActorId -> m Bool
effectApplyPerfume execSfx target = do
  execSfx
  tb <- getsState $ getActorBody target
  Level{lsmell} <- getLevel $ blid tb
  let f p fromSm =
        execUpdAtomic $ UpdAlterSmell (blid tb) p fromSm timeZero
  mapWithKeyM_ f lsmell
  return True

-- ** OneOf

effectOneOf :: (MonadAtomic m, MonadServer m)
            => (IK.Effect -> m Bool)
            -> [IK.Effect]
            -> m Bool
effectOneOf recursiveCall l = do
  let call1 = do
        ef <- rndToAction $ oneOf l
        recursiveCall ef
      call99 = replicate 99 call1
      f callNext result = do
        b <- result
        if b then return True else callNext
  foldr f (return False) call99

-- ** Recharging

effectRecharging :: MonadAtomic m
                 => (IK.Effect -> m Bool)
                 -> IK.Effect -> Bool
                 -> m Bool
effectRecharging recursiveCall e recharged =
  if recharged
  then recursiveCall e
  else return False

-- ** Temporary

effectTemporary :: MonadAtomic m
                => m () -> ActorId -> ItemId -> Container -> m Bool
effectTemporary execSfx source iid c = do
  case c of
    CActor _ COrgan -> do
      b <- getsState $ getActorBody source
      case iid `EM.lookup` borgan b of
        Just _ -> return ()  -- still some copies left of a multi-copy tmp organ
        Nothing -> execSfx  -- last copy just destroyed
      return True
    _ -> do
      execSfx
      return False  -- just a message
