{-# LANGUAGE TupleSections #-}
-- | Handle effects (most often caused by requests sent by clients).
module Game.LambdaHack.Server.HandleEffectServer
  ( applyItem, itemEffectAndDestroy, effectAndDestroy, itemEffectCause
  , dropCStoreItem, armorHurtBonus
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Bits (xor)
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.Key (mapWithKeyM_)
import Data.List
import Data.Maybe
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
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
import Game.LambdaHack.Server.CommonServer
import Game.LambdaHack.Server.ItemServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicServer
import Game.LambdaHack.Server.StartServer
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
  discoEffect <- getsServer sdiscoEffect
  case EM.lookup iid discoEffect of
    Just ItemAspectEffect{jeffects, jaspects} -> do
      bag <- getsState $ getCBag c
      case iid `EM.lookup` bag of
        Nothing -> assert `failure` (source, target, iid, c)
        Just kit ->
          effectAndDestroy source target iid c False jeffects jaspects kit
    _ -> assert `failure` (source, target, iid, c)

effectAndDestroy :: (MonadAtomic m, MonadServer m)
                 => ActorId -> ActorId -> ItemId -> Container -> Bool
                 -> [IK.Effect] -> [IK.Aspect Int] -> ItemQuant
                 -> m ()
effectAndDestroy source target iid c periodic effs aspects kitK@(k, it) = do
  let mtimeout = let timeoutAspect :: IK.Aspect a -> Bool
                     timeoutAspect IK.Timeout{} = True
                     timeoutAspect _ = False
                 in find timeoutAspect aspects
  lid <- getsState $ lidFromC c
  localTime <- getsState $ getLocalTime lid
  let it1 = case mtimeout of
        Just (IK.Timeout timeout) ->
          let timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
              charging startT = timeShift startT timeoutTurns > localTime
          in filter charging it
        _ -> []
      len = length it1
      recharged = len < k
  let !_A = assert (len <= k `blame` (kitK, source, target, iid, c)) ()
  -- If there is no Timeout, but there are Recharging,
  -- then such effects are disabled whenever the item is affected
  -- by a Discharge attack (TODO).
  it2 <- case mtimeout of
    Just (IK.Timeout _) | recharged ->
      return $ localTime : it1
    _ ->
      -- TODO: if has timeout and not recharged, report failure
      return it1
  -- We use up the charge even if eventualy every effect fizzles. Tough luck.
  -- At least we don't destroy the item in such case. Also, we ID it regardless.
  it3 <- if it /= it2 && mtimeout /= Just (IK.Timeout 0) then do
           execUpdAtomic $ UpdTimeItem iid c it it2
           return it2
         else return it
  -- If the activation is not periodic, trigger at least the effects
  -- that are not recharging and so don't depend on @recharged@.
  when (not periodic || recharged) $ do
    -- We have to destroy the item before the effect affects the item
    -- or the actor holding it or standing on it (later on we could
    -- lose track of the item and wouldn't be able to destroy it) .
    -- This is OK, because we don't remove the item type from various
    -- item dictionaries, just an individual copy from the container,
    -- so, e.g., the item can be identified after it's removed.
    let mtmp = let tmpEffect :: IK.Effect -> Bool
                   tmpEffect IK.Temporary{} = True
                   tmpEffect (IK.Recharging IK.Temporary{}) = True
                   tmpEffect (IK.OnSmash IK.Temporary{}) = True
                   tmpEffect _ = False
               in find tmpEffect effs
    item <- getsState $ getItemBody iid
    let durable = IK.Durable `elem` jfeature item
        imperishable = durable || periodic && isNothing mtmp
        kit = if isNothing mtmp || periodic then (1, take 1 it3) else (k, it3)
    unless imperishable $
      execUpdAtomic $ UpdLoseItem iid item kit c
    -- At this point, the item is potentially no longer in container @c@,
    -- so we don't pass @c@ along.
    triggered <- itemEffectDisco source target iid c recharged periodic effs
    -- If none of item's effects was performed, we try to recreate the item.
    -- Regardless, we don't rewind the time, because some info is gained
    -- (that the item does not exhibit any effects in the given context).
    unless (triggered || imperishable) $
      execUpdAtomic $ UpdSpotItem iid item kit c

itemEffectCause :: (MonadAtomic m, MonadServer m)
                => ActorId -> Point -> IK.Effect
                -> m Bool
itemEffectCause aid tpos ef = do
  sb <- getsState $ getActorBody aid
  let c = CEmbed (blid sb) tpos
  bag <- getsState $ getCBag c
  case EM.assocs bag of
    [(iid, kit)] -> do
      -- No block against tile, hence unconditional.
      discoEffect <- getsServer sdiscoEffect
      let aspects = case EM.lookup iid discoEffect of
            Just ItemAspectEffect{jaspects} -> jaspects
            _ -> assert `failure` (aid, tpos, ef, iid)
      execSfxAtomic $ SfxTrigger aid tpos $ TK.Cause ef
      effectAndDestroy aid aid iid c False [ef] aspects kit
      return True
    ab -> assert `failure` (aid, tpos, ab)

-- | The source actor affects the target actor, with a given item.
-- If any of the effects fires up, the item gets identified. This function
-- is mutually recursive with @effect@ and so it's a part of @Effect@
-- semantics.
itemEffectDisco :: (MonadAtomic m, MonadServer m)
                => ActorId -> ActorId -> ItemId -> Container -> Bool -> Bool
                -> [IK.Effect]
                -> m Bool
itemEffectDisco source target iid c recharged periodic effs = do
  discoKind <- getsServer sdiscoKind
  item <- getsState $ getItemBody iid
  case EM.lookup (jkindIx item) discoKind of
    Just itemKindId -> do
      seed <- getsServer $ (EM.! iid) . sitemSeedD
      execUpdAtomic $ UpdDiscover c iid itemKindId seed
      itemEffect source target iid recharged periodic effs
    _ -> assert `failure` (source, target, iid, item)

itemEffect :: (MonadAtomic m, MonadServer m)
           => ActorId -> ActorId -> ItemId -> Bool -> Bool
           -> [IK.Effect]
           -> m Bool
itemEffect source target iid recharged periodic effects = do
  trs <- mapM (effectSem source target iid recharged) effects
  let triggered = or trs
  sb <- getsState $ getActorBody source
  -- Announce no effect, which is rare and wastes time, so noteworthy.
  unless (triggered  -- some effect triggered, so feedback comes from them
          || null effects  -- no effects present, no feedback needed
          || periodic  -- don't spam from fizzled periodic effects
          || bproj sb) $  -- don't spam, projectiles can be very numerous
    execSfxAtomic $ SfxEffect (bfid sb) target $ IK.NoEffect ""
  return triggered

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The item may or may not still be in the container.
-- The boolean result indicates if the effect actually fired up,
-- as opposed to fizzled.
effectSem :: (MonadAtomic m, MonadServer m)
          => ActorId -> ActorId -> ItemId -> Bool -> IK.Effect
          -> m Bool
effectSem source target iid recharged effect = do
  let recursiveCall = effectSem source target iid recharged
  sb <- getsState $ getActorBody source
  -- @execSfx@ usually comes last in effect semantics, but not always
  -- and we are likely to introduce more variety.
  let execSfx = execSfxAtomic $ SfxEffect (bfid sb) target effect
  case effect of
    IK.NoEffect _ -> return False
    IK.Hurt nDm -> effectHurt nDm source target IK.RefillHP
    IK.Burn nDm -> effectBurn nDm source target
    IK.Explode t -> effectExplode execSfx t target
    IK.RefillHP p -> effectRefillHP False execSfx p source target
    IK.OverfillHP p -> effectRefillHP True execSfx p source target
    IK.RefillCalm p -> effectRefillCalm False execSfx p source target
    IK.OverfillCalm p -> effectRefillCalm True execSfx p source target
    IK.Dominate -> effectDominate recursiveCall source target
    IK.Impress -> effectImpress execSfx source target
    IK.CallFriend p -> effectCallFriend p source target
    IK.Summon freqs p -> effectSummon freqs p source target
    IK.Ascend p -> effectAscend recursiveCall execSfx p source target
    IK.Escape{} -> effectEscape source target
    IK.Paralyze p -> effectParalyze execSfx p target
    IK.InsertMove p -> effectInsertMove execSfx p target
    IK.Teleport p -> effectTeleport execSfx p source target
    IK.CreateItem store grp tim -> effectCreateItem target store grp tim
    IK.DropItem store grp hit -> effectDropItem execSfx store grp hit target
    IK.PolyItem -> effectPolyItem execSfx source target
    IK.Identify -> effectIdentify execSfx iid source target
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
    IK.Temporary _ -> effectTemporary execSfx source iid

-- + Individual semantic functions for effects

-- ** Hurt

-- Modified by armor. Can, exceptionally, add HP.
effectHurt :: (MonadAtomic m, MonadServer m)
           => Dice.Dice -> ActorId -> ActorId -> (Int -> IK.Effect)
           -> m Bool
effectHurt nDm source target verboseEffectConstructor = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  hpMax <- sumOrganEqpServer IK.EqpSlotAddMaxHP target
  n <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  hurtBonus <- armorHurtBonus source target
  let mult = 100 + hurtBonus
      rawDeltaHP = - (max oneM  -- at least 1 HP taken
                          (fromIntegral mult * xM n `divUp` 100))
      serious = source /= target && not (bproj tb)
      deltaHP | serious = -- if HP overfull, at least cut back to max HP
                          min rawDeltaHP (xM hpMax - bhp tb)
              | otherwise = rawDeltaHP
      deltaDiv = fromIntegral $ deltaHP `divUp` oneM
  -- Damage the target.
  execSfxAtomic $ SfxEffect (bfid sb) target $
    if source == target
    then verboseEffectConstructor deltaDiv
           -- no SfxStrike, so treat as any heal/wound
    else IK.Hurt (Dice.intToDice deltaDiv)
           -- SfxStrike already sent, avoid spam
  execUpdAtomic $ UpdRefillHP target deltaHP
  when serious $ halveCalm target
  return True

armorHurtBonus :: (MonadAtomic m, MonadServer m)
               => ActorId -> ActorId
               -> m Int
armorHurtBonus source target = do
  sactiveItems <- activeItemsServer source
  tactiveItems <- activeItemsServer target
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let itemBonus =
        if bproj sb
        then sumSlotNoFilter IK.EqpSlotAddHurtRanged sactiveItems
             - sumSlotNoFilter IK.EqpSlotAddArmorRanged tactiveItems
        else sumSlotNoFilter IK.EqpSlotAddHurtMelee sactiveItems
             - sumSlotNoFilter IK.EqpSlotAddArmorMelee tactiveItems
      block = braced tb
  return $! itemBonus - if block then 50 else 0

halveCalm :: (MonadAtomic m, MonadServer m)
          => ActorId -> m ()
halveCalm target = do
  tb <- getsState $ getActorBody target
  activeItems <- activeItemsServer target
  let calmMax = sumSlotNoFilter IK.EqpSlotAddMaxCalm activeItems
      upperBound = if hpTooLow tb activeItems
                   then 0  -- to trigger domination, etc.
                   else max (xM calmMax) (bcalm tb) `div` 2
      deltaCalm = min minusTwoM (upperBound - bcalm tb)
  -- HP loss decreases Calm by at least minusTwoM, to overcome Calm regen,
  -- when far from shooting foe and to avoid "hears something",
  -- which is emitted for decrease @minusM@.
  execUpdAtomic $ UpdRefillCalm target deltaCalm

-- ** Burn

-- Damage from both impact and fire. Modified by armor.
effectBurn :: (MonadAtomic m, MonadServer m)
           => Dice.Dice -> ActorId -> ActorId
           -> m Bool
effectBurn nDm source target =
  effectHurt nDm source target (\p -> IK.Burn $ Dice.intToDice p)

-- ** Explode

effectExplode :: (MonadAtomic m, MonadServer m)
              => m () -> GroupName ItemKind -> ActorId -> m Bool
effectExplode execSfx cgroup target = do
  tb <- getsState $ getActorBody target
  let itemFreq = [(cgroup, 1)]
      container = CActor target CEqp
  m2 <- rollAndRegisterItem (blid tb) itemFreq container False Nothing
  let (iid, (ItemFull{..}, _)) = fromMaybe (assert `failure` cgroup) m2
      Point x y = bpos tb
      projectN k100 (n, _) = do
        -- We pick a point at the border, not inside, to have a uniform
        -- distribution for the points the line goes through at each distance
        -- from the source. Otherwise, e.g., the points on cardinal
        -- and diagonal lines from the source would be more common.
        let fuzz = 2 + (k100 `xor` (itemK * n)) `mod` 9
            k | itemK >= 8 && n < 8 = 0
              | n < 8 && n >= 4 = 4
              | otherwise = n
            psAll =
              [ Point (x - 12) $ y + fuzz
              , Point (x + 12) $ y - fuzz
              , Point (x - 12) $ y - fuzz
              , Point (x + 12) $ y + fuzz
              , flip Point (y - 12) $ x + fuzz
              , flip Point (y + 12) $ x - fuzz
              , flip Point (y - 12) $ x - fuzz
              , flip Point (y + 12) $ x + fuzz
              ]
            -- Keep full symmetry, but only if enough projectiles. Fall back
            -- to random, on average, symmetry.
            ps = take k $
              if k >= 4 then psAll
              else drop ((n + x + y + fromEnum iid * 7) `mod` 16)
                   $ cycle $ psAll ++ reverse psAll
        forM_ ps $ \tpxy -> do
          let req = ReqProject tpxy k100 iid CEqp
          mfail <- projectFail target tpxy k100 iid CEqp True
          case mfail of
            Nothing -> return ()
            Just ProjectBlockTerrain -> return ()
            Just ProjectBlockActor | not $ bproj tb -> return ()
            Just failMsg -> execFailure target req failMsg
  -- All blasts bounce off obstacles many times before they destruct.
  forM_ [101..201] $ \k100 -> do
    bag2 <- getsState $ beqp . getActorBody target
    let mn2 = EM.lookup iid bag2
    maybe (return ()) (projectN k100) mn2
  bag3 <- getsState $ beqp . getActorBody target
  let mn3 = EM.lookup iid bag3
  maybe (return ()) (\kit -> execUpdAtomic
                             $ UpdLoseItem iid itemBase kit container) mn3
  execSfx
  return True  -- we neglect verifying that at least one projectile got off

-- ** RefillHP

-- Unaffected by armor.
effectRefillHP :: (MonadAtomic m, MonadServer m)
               => Bool -> m () -> Int -> ActorId -> ActorId -> m Bool
effectRefillHP overfill execSfx power source target = do
  tb <- getsState $ getActorBody target
  hpMax <- sumOrganEqpServer IK.EqpSlotAddMaxHP target
  let overMax | overfill = xM hpMax * 10  -- arbitrary limit to scumming
              | otherwise = xM hpMax
      serious = not (bproj tb) && source /= target && power > 1
      deltaHP | power > 0 = min (xM power) (max 0 $ overMax - bhp tb)
              | serious = -- if overfull, at least cut back to max
                          min (xM power) (xM hpMax - bhp tb)
              | otherwise = xM power
  if deltaHP == 0
    then return False
    else do
      execSfx
      execUpdAtomic $ UpdRefillHP target deltaHP
      when (deltaHP < 0 && serious) $ halveCalm target
      return True

-- ** RefillCalm

effectRefillCalm ::  (MonadAtomic m, MonadServer m)
                 => Bool -> m () -> Int -> ActorId -> ActorId -> m Bool
effectRefillCalm overfill execSfx power source target = do
  tb <- getsState $ getActorBody target
  calmMax <- sumOrganEqpServer IK.EqpSlotAddMaxCalm target
  let overMax | overfill = xM calmMax * 10  -- arbitrary limit to scumming
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
      execUpdAtomic $ UpdRefillCalm target deltaCalm
      return True

-- ** Dominate

effectDominate :: (MonadAtomic m, MonadServer m)
               => (IK.Effect -> m Bool)
               -> ActorId -> ActorId
               -> m Bool
effectDominate recursiveCall source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if bfid tb == bfid sb then
    -- Dominate is rather on projectiles than on items, so alternate effect
    -- is useful to avoid boredom if domination can't happen.
    recursiveCall IK.Impress
  else
    dominateFidSfx (bfid sb) target

-- ** Impress

effectImpress :: (MonadAtomic m, MonadServer m)
              => m () -> ActorId -> ActorId -> m Bool
effectImpress execSfx source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if bfidImpressed tb == bfid sb || bproj tb then
    return False
  else do
    execSfx
    execUpdAtomic $ UpdFidImpressedActor target (bfidImpressed tb) (bfid sb)
    return True

-- ** CallFriend

-- Note that the Calm expended doesn't depend on the number of actors called.
effectCallFriend :: (MonadAtomic m, MonadServer m)
                   => Dice.Dice -> ActorId -> ActorId
                   -> m Bool
effectCallFriend nDm source target = do
  -- Obvious effect, nothing announced.
  Kind.COps{cotile} <- getsState scops
  power <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  activeItems <- activeItemsServer target
  if not $ hpEnough10 tb activeItems then do
    unless (bproj tb) $ do
      let subject = partActor tb
          verb = "lack enough HP to call aid"
          msg = makeSentence [MU.SubjectVerbSg subject verb]
      execSfxAtomic $ SfxMsgFid (bfid sb) msg
    return False
  else do
    let deltaHP = - xM 10
    execUpdAtomic $ UpdRefillHP target deltaHP
    let validTile t = not $ Tile.hasFeature cotile TK.NoActor t
    ps <- getsState $ nearbyFreePoints validTile (bpos tb) (blid tb)
    time <- getsState $ getLocalTime (blid tb)
    -- We call target's friends so that AI monsters that test by throwing
    -- don't waste artifacts very valuable for heroes. Heroes should rather
    -- not test scrolls by throwing.
    recruitActors (take power ps) (blid tb) time (bfid tb)

-- ** Summon

-- Note that the Calm expended doesn't depend on the number of actors summoned.
effectSummon :: (MonadAtomic m, MonadServer m)
             => Freqs ItemKind -> Dice.Dice -> ActorId -> ActorId
             -> m Bool
effectSummon actorFreq nDm source target = do
  -- Obvious effect, nothing announced.
  Kind.COps{cotile} <- getsState scops
  power <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  activeItems <- activeItemsServer target
  if not $ calmEnough10 tb activeItems then do
    unless (bproj tb) $ do
      let subject = partActor tb
          verb = "lack enough Calm to summon"
          msg = makeSentence [MU.SubjectVerbSg subject verb]
      execSfxAtomic $ SfxMsgFid (bfid sb) msg
    return False
  else do
    let deltaCalm = - xM 10
    unless (bproj tb) $ execUpdAtomic $ UpdRefillCalm target deltaCalm
    let validTile t = not $ Tile.hasFeature cotile TK.NoActor t
    ps <- getsState $ nearbyFreePoints validTile (bpos tb) (blid tb)
    localTime <- getsState $ getLocalTime (blid tb)
    -- Make sure summoned actors start acting after the summoner.
    let targetTime = timeShift localTime $ ticksPerMeter $ bspeed tb activeItems
        afterTime = timeShift targetTime $ Delta timeClip
    bs <- forM (take power ps) $ \p -> do
      maid <- addAnyActor actorFreq (blid tb) afterTime (Just p)
      case maid of
        Nothing -> return False  -- actorFreq is null; content writers...
        Just aid -> do
          b <- getsState $ getActorBody aid
          mleader <- getsState $ gleader . (EM.! bfid b) . sfactionD
          when (isNothing mleader) $
            execUpdAtomic
            $ UpdLeadFaction (bfid b) Nothing (Just (aid, Nothing))
          return True
    return $! or bs

-- ** Ascend

-- Note that projectiles can be teleported, too, for extra fun.
effectAscend :: (MonadAtomic m, MonadServer m)
             => (IK.Effect -> m Bool)
             -> m () -> Int -> ActorId -> ActorId
             -> m Bool
effectAscend recursiveCall execSfx k source target = do
  b1 <- getsState $ getActorBody target
  ais1 <- getsState $ getCarriedAssocs b1
  let lid1 = blid b1
      pos1 = bpos b1
  (lid2, pos2) <- getsState $ whereTo lid1 pos1 k . sdungeon
  sb <- getsState $ getActorBody source
  if braced b1 then do
    execSfxAtomic $ SfxMsgFid (bfid sb)
                              "Braced actors are immune to translocation."
    return False
  else if lid2 == lid1 && pos2 == pos1 then do
    execSfxAtomic $ SfxMsgFid (bfid sb) "No more levels in this direction."
    -- We keep it useful even in shallow dungeons.
    recursiveCall $ IK.Teleport 30  -- powerful teleport
  else do
    let switch1 = void $ switchLevels1 ((target, b1), ais1)
        switch2 = do
          -- Make the initiator of the stair move the leader,
          -- to let him clear the stairs for others to follow.
          let mlead = Just target
          -- Move the actor to where the inhabitants were, if any.
          switchLevels2 lid2 pos2 ((target, b1), ais1) mlead
          -- Verify only one non-projectile actor on every tile.
          !_ <- getsState $ posToActors pos1 lid1  -- assertion is inside
          !_ <- getsState $ posToActors pos2 lid2  -- assertion is inside
          return ()
    -- The actor will be added to the new level, but there can be other actors
    -- at his new position.
    inhabitants <- getsState $ posToActors pos2 lid2
    case inhabitants of
      [] -> do
        switch1
        switch2
      ((_, b2), _) : _ -> do
        -- Alert about the switch.
        let subjects = map (partActor . snd . fst) inhabitants
            subject = MU.WWandW subjects
            verb = "be pushed to another level"
            msg2 = makeSentence [MU.SubjectVerbSg subject verb]
        -- Only tell one player, even if many actors, because then
        -- they are projectiles, so not too important.
        execSfxAtomic $ SfxMsgFid (bfid b2) msg2
        -- Move the actor out of the way.
        switch1
        -- Move the inhabitant out of the way and to where the actor was.
        let moveInh inh = do
              -- Preserve old the leader, since the actor is pushed, so possibly
              -- has nothing worhwhile to do on the new level (and could try
              -- to switch back, if made a leader, leading to a loop).
              inhMLead <- switchLevels1 inh
              switchLevels2 lid1 pos1 inh inhMLead
        mapM_ moveInh inhabitants
        -- Move the actor to his destination.
        switch2
    execSfx
    return True

switchLevels1 :: MonadAtomic m
              => ((ActorId, Actor), [(ItemId, Item)])
              -> m (Maybe ActorId)
switchLevels1 ((aid, bOld), ais) = do
  let side = bfid bOld
  mleader <- getsState $ gleader . (EM.! side) . sfactionD
  -- Prevent leader pointing to a non-existing actor.
  mlead <-
    if not (bproj bOld) && isJust mleader then do
      execUpdAtomic $ UpdLeadFaction side mleader Nothing
      return $ fst <$> mleader
        -- outside of a client we don't know the real tgt of aid, hence fst
    else return Nothing
  -- Remove the actor from the old level.
  -- Onlookers see somebody disappear suddenly.
  -- @UpdDestroyActor@ is too loud, so use @UpdLoseActor@ instead.
  execUpdAtomic $ UpdLoseActor aid bOld ais
  return mlead

switchLevels2 ::(MonadAtomic m, MonadServer m)
              => LevelId -> Point
              -> ((ActorId, Actor), [(ItemId, Item)]) -> Maybe ActorId
              -> m ()
switchLevels2 lidNew posNew ((aid, bOld), ais) mlead = do
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
                  , btime = shiftByDelta $ btime bOld
                  , bpos = posNew
                  , boldpos = posNew  -- new level, new direction
                  , boldlid = lidOld  -- record old level
                  , borgan = setTimeout $ borgan bOld
                  , beqp = setTimeout $ beqp bOld }
  -- Materialize the actor at the new location.
  -- Onlookers see somebody appear suddenly. The actor himself
  -- sees new surroundings and has to reset his perception.
  execUpdAtomic $ UpdCreateActor aid bNew ais
  case mlead of
    Nothing -> return ()
    Just leader ->
      execUpdAtomic $ UpdLeadFaction side Nothing (Just (leader, Nothing))

-- ** Escape

-- | The faction leaves the dungeon.
effectEscape :: (MonadAtomic m, MonadServer m) => ActorId -> ActorId -> m Bool
effectEscape source target = do
  -- Obvious effect, nothing announced.
  sb <- getsState $ getActorBody source
  b <- getsState $ getActorBody target
  let fid = bfid b
  fact <- getsState $ (EM.! fid) . sfactionD
  if bproj b then
    return False
  else if not (fcanEscape $ gplayer fact) then do
    execSfxAtomic $ SfxMsgFid (bfid sb)
                              "This faction doesn't want to escape outside."
    return False
  else do
    deduceQuits fid Nothing $ Status Escape (fromEnum $ blid b) Nothing
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
      let t = timeDeltaScale (Delta timeClip) p
      execUpdAtomic $ UpdAgeActor target t
      execSfx
      return True

-- ** InsertMove

-- | Give target actor the given number of extra moves. Don't give
-- an absolute amount of time units, to benefit slow actors more.
effectInsertMove :: (MonadAtomic m, MonadServer m)
                 => m () -> Dice.Dice -> ActorId -> m Bool
effectInsertMove execSfx nDm target = do
  p <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  b <- getsState $ getActorBody target
  activeItems <- activeItemsServer target
  let tpm = ticksPerMeter $ bspeed b activeItems
      t = timeDeltaScale tpm (-p)
  execUpdAtomic $ UpdAgeActor target t
  execSfx
  return True

-- ** Teleport

-- | Teleport the target actor.
-- Note that projectiles can be teleported, too, for extra fun.
effectTeleport :: (MonadAtomic m, MonadServer m)
               => m () -> Dice.Dice -> ActorId -> ActorId -> m Bool
effectTeleport execSfx nDm source target = do
  Kind.COps{cotile} <- getsState scops
  range <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  sb <- getsState $ getActorBody source
  b <- getsState $ getActorBody target
  Level{ltile} <- getLevel (blid b)
  as <- getsState $ actorList (const True) (blid b)
  let spos = bpos b
      dMinMax delta pos =
        let d = chessDist spos pos
        in d >= range - delta && d <= range + delta
      dist delta pos _ = dMinMax delta pos
  tpos <- rndToAction $ findPosTry 200 ltile
    (\p t -> Tile.isWalkable cotile t
             && (not (dMinMax 9 p)  -- don't loop, very rare
                 || not (Tile.hasFeature cotile TK.NoActor t)
                    && unoccupied as p))
    [ dist 1
    , dist $ 1 + range `div` 9
    , dist $ 1 + range `div` 7
    , dist $ 1 + range `div` 5
    , dist 5
    , dist 7
    ]
  if braced b then do
    execSfxAtomic $ SfxMsgFid (bfid sb)
                              "Braced actors are immune to translocation."
    return False
  else if not (dMinMax 9 tpos) then do  -- very rare
    execSfxAtomic $ SfxMsgFid (bfid sb) "Translocation not possible."
    return False
  else do
    execUpdAtomic $ UpdMoveActor target spos tpos
    execSfx
    return True

-- ** CreateItem

-- TODO: if the items is created not on the ground, perhaps it should
-- be IDed, so that there are no rings with unkown max Calm bonus
-- leading to attempts to do illegal actions (which the server then catches).
-- This is in analogy to picking item from the ground, whereas it's IDed.
effectCreateItem :: (MonadAtomic m, MonadServer m)
                  => ActorId -> CStore -> GroupName ItemKind -> IK.TimerDice
                  -> m Bool
effectCreateItem target store grp tim = do
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
      activeItems <- activeItemsServer target
      let actorTurn = ticksPerMeter $ bspeed tb activeItems
      return $! timeDeltaScale actorTurn k
  let c = CActor target store
  bagBefore <- getsState $ getCBag c
  let litemFreq = [(grp, 1)]
  m5 <- rollItem (blid tb) litemFreq
  let (itemKnown, itemFull, _, seed, _) =
        fromMaybe (assert `failure` (blid tb, litemFreq, c)) m5
  itemRev <- getsServer sitemRev
  let mquant = case HM.lookup itemKnown itemRev of
        Nothing -> Nothing
        Just iid -> (iid,) <$> iid `EM.lookup` bagBefore
  case mquant of
    Just (iid, (1, afterIt@(timer : rest))) | tim /= IK.TimerNone -> do
      -- Already has such an item, so only increase the timer by half delta.
      let newIt = let halfTurns = delta `timeDeltaDiv` 2
                      newTimer = timer `timeShift` halfTurns
                  in newTimer : rest
      when (afterIt /= newIt) $
        execUpdAtomic $ UpdTimeItem iid c afterIt newIt  -- TODO: announce
    _ -> do
      -- Multiple such items, so it's a periodic poison, etc., so just stack,
      -- or no such items at all, so create some.
      iid <- registerItem itemFull itemKnown seed (itemK itemFull) c True
      unless (tim == IK.TimerNone) $ do
        bagAfter <- getsState $ getCBag c
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

-- | Make the target actor drop all items in his equiment with the given symbol
-- (not just a random single item, or cluttering equipment with rubbish
-- would be beneficial).
effectDropItem :: (MonadAtomic m, MonadServer m)
               => m () -> CStore -> GroupName ItemKind -> Bool -> ActorId
               -> m Bool
effectDropItem execSfx store grp hit target = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  discoKind <- getsServer sdiscoKind
  b <- getsState $ getActorBody target
  let hasGroup (iid, _) = do
        item <- getsState $ getItemBody iid
        case EM.lookup (jkindIx item) discoKind of
          Just kindId ->
            return $! maybe False (> 0) $ lookup grp $ IK.ifreq (okind kindId)
          Nothing ->
            assert `failure` (target, grp, iid, item)
  assocsCStore <- getsState $ EM.assocs . getActorBag target store
  is <- filterM hasGroup assocsCStore
  if null is
    then return False
    else do
      mapM_ (uncurry (dropCStoreItem store target b hit)) is
      unless (store == COrgan) execSfx
      return True

-- | Drop a single actor's item. Note that if there are multiple copies,
-- at most one explodes to avoid excessive carnage and UI clutter
-- (let's say, the multiple explosions interfere with each other or perhaps
-- larger quantities of explosives tend to be packaged more safely).
dropCStoreItem :: (MonadAtomic m, MonadServer m)
               => CStore -> ActorId -> Actor -> Bool -> ItemId -> ItemQuant
               -> m ()
dropCStoreItem store aid b hit iid kit@(k, _) = do
  item <- getsState $ getItemBody iid
  let c = CActor aid store
      fragile = IK.Fragile `elem` jfeature item
      durable = IK.Durable `elem` jfeature item
      isDestroyed = hit && not durable || bproj b && fragile
  if isDestroyed then do
    discoEffect <- getsServer sdiscoEffect
    let aspects = case EM.lookup iid discoEffect of
          Just ItemAspectEffect{jaspects} -> jaspects
          _ -> assert `failure` (aid, iid)
    itemToF <- itemToFullServer
    let itemFull = itemToF iid kit
        effs = strengthOnSmash itemFull
    effectAndDestroy aid aid iid c False effs aspects kit
  else do
    mvCmd <- generalMoveItem iid k (CActor aid store)
                                   (CActor aid CGround)
    mapM_ execUpdAtomic mvCmd

-- ** PolyItem

-- TODO: ask player for an item
effectPolyItem :: (MonadAtomic m, MonadServer m)
               => m () -> ActorId -> ActorId -> m Bool
effectPolyItem execSfx source target = do
  sb <- getsState $ getActorBody source
  let cstore = CGround
  allAssocs <- fullAssocsServer target [cstore]
  case allAssocs of
    [] -> do
      execSfxAtomic $ SfxMsgFid (bfid sb) $
        "The purpose of repurpose cannot be availed without an item"
        <+> ppCStoreIn cstore <> "."
      return False
    (iid, itemFull@ItemFull{..}) : _ -> case itemDisco of
      Just ItemDisco{..} -> do
        discoEffect <- getsServer sdiscoEffect
        let maxCount = Dice.maxDice $ IK.icount itemKind
            aspects = jaspects $ discoEffect EM.! iid
        if itemK < maxCount then do
          execSfxAtomic $ SfxMsgFid (bfid sb) $
            "The purpose of repurpose is served by" <+> tshow maxCount
            <+> "pieces of this item, not by" <+> tshow itemK <> "."
          return False
        else if IK.Unique `elem` aspects then do
          execSfxAtomic $ SfxMsgFid (bfid sb) $
            "Unique items can't be repurposed."
          return False
        else do
          let c = CActor target cstore
              kit = (maxCount, take maxCount itemTimer)
          identifyIid execSfx iid c itemKindId
          execUpdAtomic $ UpdDestroyItem iid itemBase kit c
          execSfx
          effectCreateItem target cstore "useful" IK.TimerNone
      _ -> assert `failure` (target, iid, itemFull)

-- ** Identify

-- TODO: ask player for an item, because server doesn't know which
-- is already identified, it only knows which cannot ever be.
-- Perhaps refill Calm only when id successfull and scroll consumed,
-- id the scroll anyway. Explain the Calm gain: "your most pressing
-- existential concerns are answered scientifitically".
effectIdentify :: (MonadAtomic m, MonadServer m)
               => m () -> ItemId -> ActorId -> ActorId -> m Bool
effectIdentify execSfx iidId source target = do
  sb <- getsState $ getActorBody source
  let tryFull store as = case as of
        -- TODO: identify the scroll, but don't use up.
        [] -> do
          let (tIn, t) = ppCStore store
              msg = "Nothing to identify" <+> tIn <+> t <> "."
          execSfxAtomic $ SfxMsgFid (bfid sb) msg
          return False
        (iid, _) : rest | iid == iidId -> tryFull store rest  -- don't id itself
        (iid, itemFull@ItemFull{itemDisco=Just ItemDisco{..}}) : rest -> do
          -- TODO: use this (but faster, via traversing effects with 999?)
          -- also to prevent sending any other UpdDiscover.
          let ided = IK.Identified `elem` IK.ifeature itemKind
              itemSecret = itemNoAE itemFull
              statsObvious = textAllAE 7 store itemFull
                             == textAllAE 7 store itemSecret
          if ided && statsObvious
            then tryFull store rest
            else do
              let c = CActor target store
              identifyIid execSfx iid c itemKindId
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
            => m () -> ItemId -> Container -> Kind.Id ItemKind
            -> m ()
identifyIid execSfx iid c itemKindId = do
  execSfx
  seed <- getsServer $ (EM.! iid) . sitemSeedD
  execUpdAtomic $ UpdDiscover c iid itemKindId seed

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
  Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  lvl@Level{lxsize, lysize} <- getLevel (blid tb)
  let eps = 0
      fpos = bpos tb `shift` v
  if braced tb then do
    execSfxAtomic $ SfxMsgFid (bfid sb)
                              "Braced actors are immune to translocation."
    return False
  else case bla lxsize lysize eps (bpos tb) fpos of
    Nothing -> assert `failure` (fpos, tb)
    Just [] -> assert `failure` "projecting from the edge of level"
                      `twith` (fpos, tb)
    Just (pos : rest) -> do
      let t = lvl `at` pos
      if not $ Tile.isWalkable cotile t
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
              execUpdAtomic $ UpdTrajectory target (btrajectory tb) ts
              -- Give the actor one extra turn and also let the push start ASAP.
              -- So, if the push lasts one (his) turn, he will not lose
              -- any turn of movement (but he may need to retrace the push).
              activeItems <- activeItemsServer target
              let tpm = ticksPerMeter $ bspeed tb activeItems
                  delta = timeDeltaScale tpm (-1)
              execUpdAtomic $ UpdAgeActor target delta
              execSfx
              return True

sendFlyingVector :: (MonadAtomic m, MonadServer m)
                 => ActorId -> ActorId -> Maybe Bool -> m Vector
sendFlyingVector source target modePush = do
  sb <- getsState $ getActorBody source
  if source == target then
    if boldpos sb == bpos sb then rndToAction $ do
      z <- randomR (-10, 10)
      oneOf [Vector 10 z, Vector (-10) z, Vector z 10, Vector z (-10)]
    else
      return $! vectorToFrom (bpos sb) (boldpos sb)
  else do
    tb <- getsState $ getActorBody target
    let (sp, tp) = if adjacent (bpos sb) (bpos tb)
                   then let pos = if chessDist (boldpos sb) (bpos tb)
                                     > chessDist (bpos sb) (bpos tb)
                                  then boldpos sb  -- avoid cardinal dir
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
      let kit = beqp tb EM.! iid
      dropCStoreItem CEqp target tb False iid kit
      execSfx
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

effectTransformEqp :: forall m. (MonadAtomic m, MonadServer m)
                   => m () -> ActorId -> Char -> CStore
                   -> (ItemId -> ItemQuant -> m ())
                   -> m Bool
effectTransformEqp execSfx target symbol cstore m = do
  let hasSymbol (iid, _) = do
        item <- getsState $ getItemBody iid
        return $! jsymbol item == symbol
  assocsCStore <- getsState $ EM.assocs . getActorBag target cstore
  is <- if symbol == ' '
        then return assocsCStore
        else filterM hasSymbol assocsCStore
  if null is
    then return False
    else do
      mapM_ (uncurry m) is
      execSfx
      return True

-- ** ApplyPerfume

effectApplyPerfume :: (MonadAtomic m, MonadServer m)
                   => m () -> ActorId -> m Bool
effectApplyPerfume execSfx target = do
  tb <- getsState $ getActorBody target
  Level{lsmell} <- getLevel $ blid tb
  let f p fromSm =
        execUpdAtomic $ UpdAlterSmell (blid tb) p (Just fromSm) Nothing
  mapWithKeyM_ f lsmell
  execSfx
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

effectRecharging :: (MonadAtomic m, MonadServer m)
                 => (IK.Effect -> m Bool)
                 -> IK.Effect -> Bool
                 -> m Bool
effectRecharging recursiveCall e recharged =
  if recharged
  then recursiveCall e
  else return False

-- ** Temporary

effectTemporary :: (MonadAtomic m, MonadServer m)
                => m () -> ActorId -> ItemId
                -> m Bool
effectTemporary execSfx source iid = do
  bag <- getsState $ getCBag $ CActor source COrgan
  case iid `EM.lookup` bag of
    Just _ -> return ()  -- still some copies left of a multi-copy tmp item
    Nothing -> execSfx  -- last copy just destroyed
  return True
