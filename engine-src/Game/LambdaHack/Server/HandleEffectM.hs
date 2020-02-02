{-# LANGUAGE TupleSections #-}
-- | Handle effects. They are most often caused by requests sent by clients
-- but sometimes also caused by projectiles or periodically activated items.
module Game.LambdaHack.Server.HandleEffectM
  ( UseResult(..), EffApplyFlags(..)
  , applyItem, kineticEffectAndDestroy, effectAndDestroyAndAddKill
  , itemEffectEmbedded, highestImpression, dominateFidSfx
  , dropAllItems, pickDroppable, consumeItems
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , applyKineticDamage, refillHP, cutCalm, effectAndDestroy, imperishableKit
  , itemEffectDisco, effectSem
  , effectBurn, effectExplode, effectRefillHP, effectRefillCalm
  , effectDominate, dominateFid, effectImpress, effectPutToSleep, effectYell
  , effectSummon, effectAscend, findStairExit, switchLevels1, switchLevels2
  , effectEscape, effectParalyze, paralyze, effectParalyzeInWater
  , effectInsertMove, effectTeleport, effectCreateItem
  , effectDestroyItem, dropCStoreItem, effectDropItem
  , effectConsumeItems
  , effectDischarge, effectPolyItem, effectRerollItem, effectDupItem
  , effectIdentify, identifyIid, effectDetect, effectDetectX, effectSendFlying
  , sendFlyingVector, effectDropBestWeapon, effectApplyPerfume, effectOneOf
  , effectVerbNoLonger, effectVerbMsg, effectAndEffect, effectOrEffect
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Bits (xor)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Key (mapWithKeyM_)
import qualified Data.Ord as Ord
import qualified Data.Text as T

import           Game.LambdaHack.Atomic
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
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Server.CommonM
import           Game.LambdaHack.Server.ItemM
import           Game.LambdaHack.Server.ItemRev
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.PeriodicM
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

-- * Semantics of effects

data UseResult = UseDud | UseId | UseUp
 deriving (Eq, Ord)

data EffApplyFlags = EffApplyFlags
  { effOnCombineOnly    :: Bool
  , effOnSmashOnly      :: Bool
  , effVoluntary        :: Bool
  , effIgnoreCharging   :: Bool
  , effUseAllCopies     :: Bool
  , effKineticPerformed :: Bool
  , effPeriodic         :: Bool
  , effMayDestroy       :: Bool
  }

applyItem :: MonadServerAtomic m => ActorId -> ItemId -> CStore -> m ()
applyItem aid iid cstore = do
  execSfxAtomic $ SfxApply aid iid
  let c = CActor aid cstore
  -- Treated as if the actor hit himself with the item as a weapon,
  -- incurring both the kinetic damage and effect, hence the same call
  -- as in @reqMelee@.
  let effApplyFlags = EffApplyFlags
        { effOnCombineOnly    = False
        , effOnSmashOnly      = False
        , effVoluntary        = True
        , effIgnoreCharging   = False
        , effUseAllCopies     = False
        , effKineticPerformed = False
        , effPeriodic         = False
        , effMayDestroy       = True
        }
  void $ kineticEffectAndDestroy effApplyFlags aid aid aid iid c

applyKineticDamage :: MonadServerAtomic m
                   => ActorId -> ActorId -> ItemId -> m Bool
applyKineticDamage source target iid = do
  itemKind <- getsState $ getIidKindServer iid
  if IK.idamage itemKind == 0 then return False else do  -- speedup
    sb <- getsState $ getActorBody source
    hurtMult <- getsState $ armorHurtBonus source target
    totalDepth <- getsState stotalDepth
    Level{ldepth} <- getLevel (blid sb)
    dmg <- rndToAction $ castDice ldepth totalDepth $ IK.idamage itemKind
    let rawDeltaHP = fromIntegral hurtMult * xM dmg `divUp` 100
        speedDeltaHP = case btrajectory sb of
          Just (_, speed) | bproj sb -> - modifyDamageBySpeed rawDeltaHP speed
          _ -> - rawDeltaHP
    if speedDeltaHP < 0 then do  -- damage the target, never heal
      refillHP source target speedDeltaHP
      return True
    else return False

refillHP :: MonadServerAtomic m => ActorId -> ActorId -> Int64 -> m ()
refillHP source target speedDeltaHP = assert (speedDeltaHP /= 0) $ do
  tbOld <- getsState $ getActorBody target
  actorMaxSk <- getsState $ getActorMaxSkills target
  -- We don't ignore even tiny HP drains, because they can be very weak
  -- enemy projectiles and so will recur and in total can be deadly
  -- and also AI should rather be stupidly aggressive than stupidly lethargic.
  let serious = source /= target && not (bproj tbOld)
      hpMax = Ability.getSk Ability.SkMaxHP actorMaxSk
      deltaHP0 | serious && speedDeltaHP < minusM =
                 -- If overfull, at least cut back to max, unless minor drain.
                 min speedDeltaHP (xM hpMax - bhp tbOld)
               | otherwise = speedDeltaHP
      deltaHP = if | deltaHP0 > 0 && bhp tbOld > xM 999 ->  -- UI limit
                     tenthM  -- avoid nop, to avoid loops
                   | deltaHP0 < 0 && bhp tbOld < - xM 999 ->
                     -tenthM
                   | otherwise -> deltaHP0
  execUpdAtomic $ UpdRefillHP target deltaHP
  when serious $ cutCalm target
  tb <- getsState $ getActorBody target
  fact <- getsState $ (EM.! bfid tb) . sfactionD
  unless (bproj tb || fleaderMode (gplayer fact) == LeaderNull) $
    -- If leader just lost all HP, change the leader early (not when destroying
    -- the actor), to let players rescue him, especially if he's slowed
    -- by the attackers.
    when (bhp tb <= 0 && bhp tbOld > 0) $ do
      -- If all other party members dying, leadership will switch
      -- to one of them, which seems questionable, but it's rare
      -- and the disruption servers to underline the dire circumstance.
      electLeader (bfid tb) (blid tb) target
      mleader <- getsState $ gleader . (EM.! bfid tb) . sfactionD
      -- If really nobody else in the party, make him the leader back again
      -- on the oft chance that he gets revived by a projectile, etc.
      when (isNothing mleader) $
        execUpdAtomic $ UpdLeadFaction (bfid tb) Nothing $ Just target

cutCalm :: MonadServerAtomic m => ActorId -> m ()
cutCalm target = do
  tb <- getsState $ getActorBody target
  actorMaxSk <- getsState $ getActorMaxSkills target
  let upperBound = if hpTooLow tb actorMaxSk
                   then 2  -- to trigger domination on next attack, etc.
                   else xM $ Ability.getSk Ability.SkMaxCalm actorMaxSk
      deltaCalm = min minusM2 (upperBound - bcalm tb)
  -- HP loss decreases Calm by at least @minusM2@ to avoid "hears something",
  -- which is emitted when decreasing Calm by @minusM1@.
  updateCalm target deltaCalm

-- Here kinetic damage is applied. This is necessary so that the same
-- AI benefit calculation may be used for flinging and for applying items.
kineticEffectAndDestroy :: MonadServerAtomic m
                        => EffApplyFlags
                        -> ActorId -> ActorId -> ActorId -> ItemId -> Container
                        -> m UseResult
kineticEffectAndDestroy effApplyFlags0@EffApplyFlags{..}
                        killer source target iid c = do
  bag <- getsState $ getContainerBag c
  case iid `EM.lookup` bag of
    Nothing -> error $ "" `showFailure` (source, target, iid, c)
    Just kit -> do
      itemFull <- getsState $ itemToFull iid
      tbOld <- getsState $ getActorBody target
      localTime <- getsState $ getLocalTime (blid tbOld)
      let recharged = hasCharge localTime itemFull kit
      -- If neither kinetic hit nor any effect is activated, there's no chance
      -- the items can be destroyed or even timeout changes, so we abort early.
      if not recharged then return UseDud else do
        effKineticPerformed2 <- applyKineticDamage source target iid
        tb <- getsState $ getActorBody target
        -- Sometimes victim heals just after we registered it as killed,
        -- but that's OK, an actor killed two times is similar enough
        -- to two killed.
        when (effKineticPerformed2  -- speedup
              && bhp tb <= 0 && bhp tbOld > 0) $ do
          sb <- getsState $ getActorBody source
          arWeapon <- getsState $ (EM.! iid) . sdiscoAspect
          let killHow | not (bproj sb) =
                        if effVoluntary
                        then KillKineticMelee
                        else KillKineticPush
                      | IA.checkFlag Ability.Blast arWeapon = KillKineticBlast
                      | otherwise = KillKineticRanged
          addKillToAnalytics killer killHow (bfid tbOld) (btrunk tbOld)
        let effApplyFlags = effApplyFlags0
              { effUseAllCopies     = fst kit <= 1
              , effKineticPerformed = effKineticPerformed2
              }
        effectAndDestroyAndAddKill effApplyFlags
                                   killer source target iid c itemFull

effectAndDestroyAndAddKill :: MonadServerAtomic m
                           => EffApplyFlags
                           -> ActorId -> ActorId -> ActorId -> ItemId
                           -> Container -> ItemFull
                           -> m UseResult
effectAndDestroyAndAddKill effApplyFlags0@EffApplyFlags{..}
                           killer source target iid c itemFull = do
  tbOld <- getsState $ getActorBody target
  triggered <- effectAndDestroy effApplyFlags0 source target iid c itemFull
  tb <- getsState $ getActorBody target
  -- Sometimes victim heals just after we registered it as killed,
  -- but that's OK, an actor killed two times is similar enough to two killed.
  when (bhp tb <= 0 && bhp tbOld > 0) $ do
    sb <- getsState $ getActorBody source
    arWeapon <- getsState $ (EM.! iid) . sdiscoAspect
    let killHow | not (bproj sb) =
                  if effVoluntary then KillOtherMelee else KillOtherPush
                | IA.checkFlag Ability.Blast arWeapon = KillOtherBlast
                | otherwise = KillOtherRanged
    addKillToAnalytics killer killHow (bfid tbOld) (btrunk tbOld)
  return triggered

effectAndDestroy :: MonadServerAtomic m
                 => EffApplyFlags
                 -> ActorId -> ActorId -> ItemId -> Container -> ItemFull
                 -> m UseResult
effectAndDestroy effApplyFlags0@EffApplyFlags{..} source target iid container
                 itemFull@ItemFull{itemDisco, itemKindId, itemKind} = do
  bag <- getsState $ getContainerBag container
  let (itemK, itemTimer) = bag EM.! iid
      effs | effOnSmashOnly = IK.strengthOnSmash itemKind
           | effOnCombineOnly = IK.strengthOnCombine itemKind
           | otherwise = IK.ieffects itemKind
      arItem = case itemDisco of
        ItemDiscoFull itemAspect -> itemAspect
        _ -> error "effectAndDestroy: server ignorant about an item"
      timeout = IA.aTimeout arItem
  lid <- getsState $ lidFromC container
  localTime <- getsState $ getLocalTime lid
  let it1 = let timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
                charging startT = timeShift startT timeoutTurns > localTime
            in filter charging itemTimer
      len = length it1
      recharged = len < itemK || effOnSmashOnly || effIgnoreCharging
  -- If the item has no charges and the special cases don't apply
  -- we speed up by shortcutting early, because we don't need to activate
  -- effects and we know kinetic hit was not performed (no charges to do so
  -- and in case of @OnSmash@ and @effIgnoreCharging@,
  -- only effects are triggered).
  if not recharged then return UseDud else do
    let it2 = if timeout /= 0 && recharged
              then if effPeriodic && IA.checkFlag Ability.Fragile arItem
                   then replicate (itemK - length it1) localTime ++ it1
                           -- copies are spares only; one fires, all discharge
                   else take (itemK - length it1) [localTime] ++ it1
                           -- copies all fire, turn by turn; <= 1 discharges
              else itemTimer
        kit2 = (1, take 1 it2)
        !_A = assert (len <= itemK `blame` (source, target, iid, container)) ()
    -- We use up the charge even if eventualy every effect fizzles. Tough luck.
    -- At least we don't destroy the item in such case.
    -- Also, we ID it regardless.
    unless (itemTimer == it2) $
      execUpdAtomic $ UpdTimeItem iid container itemTimer it2
    -- We have to destroy the item before the effect affects the item
    -- or affects the actor holding it or standing on it (later on we could
    -- lose track of the item and wouldn't be able to destroy it) .
    -- This is OK, because we don't remove the item type from various
    -- item dictionaries, just an individual copy from the container,
    -- so, e.g., the item can be identified after it's removed.
    let imperishable = not effMayDestroy || imperishableKit effPeriodic itemFull
    unless imperishable $
      execUpdAtomic $ UpdLoseItem False iid kit2 container
    -- At this point, the item is potentially no longer in container
    -- @container@, therefore beware of assuming so in the code below.
    -- If the item activation is not periodic, but the item itself is,
    -- only the first effect gets activated (and the item may be destroyed,
    -- unlike with periodic activations).
    let effsManual = if not effPeriodic
                        && IA.checkFlag Ability.Periodic arItem
                        && not (IA.checkFlag Ability.Condition arItem)
                     then take 1 effs  -- may be empty
                     else effs
    triggeredEffect <- itemEffectDisco effApplyFlags0 source target iid
                                       itemKindId itemKind container effsManual
    sb <- getsState $ getActorBody source
    let triggered = if effKineticPerformed then UseUp else triggeredEffect
        isEmbed = case container of
          CEmbed{} -> True
          _ -> False
    -- Announce no effect, which is rare and wastes time, so noteworthy.
    unless (triggered == UseUp  -- effects triggered; feedback comes from them
            || effOnSmashOnly
            || effPeriodic  -- periodic effects repeat and so spam
            || bproj sb  -- projectiles can be very numerous
            || isEmbed   -- embeds may be just flavour
            ) $
      execSfxAtomic $ SfxMsgFid (bfid sb) $
        if any IK.forApplyEffect effsManual
        then SfxFizzles  -- something didn't work, despite promising effects
        else SfxNothingHappens  -- fully expected
    -- If none of item's effects nor a kinetic hit were performed,
    -- we recreate the item (assuming we deleted the item above).
    -- Regardless, we don't rewind the time, because some info is gained
    -- (that the item does not exhibit any effects in the given context).
    unless (imperishable || triggered == UseUp) $
      execUpdAtomic $ UpdSpotItem False iid kit2 container
    return triggered

imperishableKit :: Bool -> ItemFull -> Bool
imperishableKit periodic itemFull =
  let arItem = aspectRecordFull itemFull
  in IA.checkFlag Ability.Durable arItem
     || periodic && not (IA.checkFlag Ability.Fragile arItem)

-- The item is triggered exactly once. If there are more copies,
-- they are left to be triggered next time.
-- If the embed no longer exists at the given position, effect fizzles.
itemEffectEmbedded :: MonadServerAtomic m
                   => Bool -> Bool -> ActorId -> LevelId -> Point -> ItemId
                   -> m UseResult
itemEffectEmbedded effOnCombineOnly effVoluntary aid lid tpos iid = do
  embeds2 <- getsState $ getEmbedBag lid tpos
    -- might have changed due to other embedded items invocations
  if iid `EM.notMember` embeds2
  then return UseDud
  else do
    -- First embedded item may move actor to another level, so @lid@
    -- may be unequal to @blid sb@.
    let c = CEmbed lid tpos
    -- Treated as if the actor hit himself with the embedded item as a weapon,
    -- incurring both the kinetic damage and effect, hence the same call
    -- as in @reqMelee@. Information whether this happened due to being pushed
    -- is preserved, but how did the pushing is lost, so we blame the victim.
    let effApplyFlags = EffApplyFlags
          { effOnCombineOnly
          , effOnSmashOnly      = False
          , effVoluntary
          , effIgnoreCharging   = False
          , effUseAllCopies     = False
          , effKineticPerformed = False
          , effPeriodic         = False
          , effMayDestroy       = True
          }
    kineticEffectAndDestroy effApplyFlags aid aid aid iid c

-- | The source actor affects the target actor, with a given item.
-- If any of the effects fires up, the item gets identified.
-- Even using raw damage (beating the enemy with the magic wand,
-- for example) identifies the item. This means a costly @UpdDiscover@
-- is processed for each random timeout weapon hit and for most projectiles,
-- but at least not for most explosion particles nor plain organs.
-- And if not needed, the @UpdDiscover@ are eventually not sent to clients.
-- So, enemy missiles that hit us are no longer mysterious until picked up,
-- which is for the better, because the client knows their charging status
-- and so can generate accurate messages in the case when not recharged.
-- This also means that thrown consumables in flasks sturdy enough to cause
-- damage are always identified at hit, even if no effect activated.
-- So throwing them at foes is a better identification method than applying.
--
-- Note that if we activate a durable non-passive item, e.g., a spiked shield,
-- from the ground, it will get identified, which is perfectly fine,
-- until we want to add sticky armor that can't be easily taken off
-- (and, e.g., has some maluses).
itemEffectDisco :: MonadServerAtomic m
                => EffApplyFlags
                -> ActorId -> ActorId -> ItemId
                -> ContentId ItemKind -> ItemKind -> Container -> [IK.Effect]
                -> m UseResult
itemEffectDisco effApplyFlags0@EffApplyFlags{..}
                source target iid itemKindId itemKind c effs = do
  urs <- mapM (effectSem effApplyFlags0 source target iid c) effs
  let ur = case urs of
        [] -> UseDud  -- there was no effects
        _ -> maximum urs
  -- Note: @UseId@ suffices for identification, @UseUp@ is not necessary.
  when (ur >= UseId || effKineticPerformed) $
    identifyIid iid c itemKindId itemKind
  return ur

-- | Source actor affects target actor, with a given effect and it strength.
-- Both actors are on the current level and can be the same actor.
-- The item may or may not still be in the container.
effectSem :: MonadServerAtomic m
          => EffApplyFlags
          -> ActorId -> ActorId -> ItemId -> Container -> IK.Effect
          -> m UseResult
effectSem effApplyFlags0@EffApplyFlags{..}
          source target iid c effect = do
  let recursiveCall = effectSem effApplyFlags0 source target iid c
  sb <- getsState $ getActorBody source
  pos <- getsState $ posFromC c
  -- @execSfx@ usually comes last in effect semantics, but not always
  -- and we are likely to introduce more variety.
  let execSfx = execSfxAtomic $ SfxEffect (bfid sb) target effect 0
      execSfxSource = execSfxAtomic $ SfxEffect (bfid sb) source effect 0
  case effect of
    IK.Burn nDm -> effectBurn nDm source target
    IK.Explode t -> effectExplode execSfx t source target
    IK.RefillHP p -> effectRefillHP p source target
    IK.RefillCalm p -> effectRefillCalm execSfx p source target
    IK.Dominate -> effectDominate source target
    IK.Impress -> effectImpress recursiveCall execSfx source target
    IK.PutToSleep -> effectPutToSleep execSfx target
    IK.Yell -> effectYell execSfx target
    IK.Summon grp nDm -> effectSummon grp nDm iid source target effPeriodic
    IK.Ascend p -> effectAscend recursiveCall execSfx p source target pos
    IK.Escape{} -> effectEscape execSfx source target
    IK.Paralyze nDm -> effectParalyze execSfx nDm source target
    IK.ParalyzeInWater nDm -> effectParalyzeInWater execSfx nDm source target
    IK.InsertMove nDm -> effectInsertMove execSfx nDm source target
    IK.Teleport nDm -> effectTeleport execSfx nDm source target
    IK.CreateItem mcount store grp tim ->
      effectCreateItem (Just $ bfid sb) mcount source target (Just iid)
                       store grp tim
    IK.DestroyItem n k store grp ->
      effectDestroyItem execSfx iid n k store target grp
    IK.ConsumeItems tools raw -> effectConsumeItems execSfx iid target tools raw
    IK.DropItem n k store grp -> effectDropItem execSfx iid n k store grp target
    IK.Discharge nDm -> effectDischarge execSfx iid nDm target
    IK.PolyItem -> effectPolyItem execSfx iid target
    IK.RerollItem -> effectRerollItem execSfx iid target
    IK.DupItem -> effectDupItem execSfx iid target
    IK.Identify -> effectIdentify execSfx iid target
    IK.Detect d radius -> effectDetect execSfx d radius target pos
    IK.SendFlying tmod ->
      effectSendFlying execSfx tmod source target c Nothing
    IK.PushActor tmod ->
      effectSendFlying execSfx tmod source target c (Just True)
    IK.PullActor tmod ->
      effectSendFlying execSfx tmod source target c (Just False)
    IK.DropBestWeapon -> effectDropBestWeapon execSfx iid target
    IK.ApplyPerfume -> effectApplyPerfume execSfx target
    IK.OneOf l -> effectOneOf recursiveCall l
    IK.OnSmash _ -> return UseDud  -- ignored under normal circumstances
    IK.OnCombine _ -> return UseDud  -- ignored under normal circumstances
    IK.VerbNoLonger _ -> effectVerbNoLonger effUseAllCopies execSfxSource source
    IK.VerbMsg _ -> effectVerbMsg execSfxSource source
    IK.AndEffect eff1 eff2 -> effectAndEffect recursiveCall eff1 eff2
    IK.OrEffect eff1 eff2 -> effectOrEffect recursiveCall eff1 eff2

-- * Individual semantic functions for effects

-- ** Burn

-- Damage from fire. Not affected by armor.
effectBurn :: MonadServerAtomic m
           => Dice.Dice -> ActorId -> ActorId -> m UseResult
effectBurn nDm source target = do
  tb <- getsState $ getActorBody target
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  n0 <- rndToAction $ castDice ldepth totalDepth nDm
  let n = max 1 n0  -- avoid 0 and negative burn
      deltaHP = - xM n
  sb <- getsState $ getActorBody source
  -- Display the effect more accurately.
  let reportedEffect = IK.Burn $ Dice.intToDice n
  execSfxAtomic $ SfxEffect (bfid sb) target reportedEffect deltaHP
  refillHP source target deltaHP
  return UseUp

-- ** Explode

effectExplode :: MonadServerAtomic m
              => m () -> GroupName ItemKind -> ActorId -> ActorId -> m UseResult
effectExplode execSfx cgroup source target = do
  execSfx
  tb <- getsState $ getActorBody target
  let itemFreq = [(cgroup, 1)]
      -- Explosion particles are placed among organs of the victim:
      container = CActor target COrgan
  m2 <- rollAndRegisterItem False (blid tb) itemFreq container Nothing
  let (iid, (ItemFull{itemKind}, (itemK, _))) =
        fromMaybe (error $ "" `showFailure` cgroup) m2
      Point x y = bpos tb
      semirandom = T.length (IK.idesc itemKind)
      projectN k100 n = do
        -- We pick a point at the border, not inside, to have a uniform
        -- distribution for the points the line goes through at each distance
        -- from the source. Otherwise, e.g., the points on cardinal
        -- and diagonal lines from the source would be more common.
        let veryrandom = (k100 `xor` (semirandom + n)) `mod` 5
            fuzz = 5 + veryrandom
            k | n < 16 && n >= 12 = 12
              | n < 12 && n >= 8 = 8
              | n < 8 && n >= 4 = 4
              | otherwise = min n 16  -- fire in groups of 16 including old duds
            psDir4 =
              [ Point (x - 12) (y + 12)
              , Point (x + 12) (y + 12)
              , Point (x - 12) (y - 12)
              , Point (x + 12) (y - 12) ]
            psDir8 =
              [ Point (x - 12) y
              , Point (x + 12) y
              , Point x (y + 12)
              , Point x (y - 12) ]
            psFuzz =
              [ Point (x - 12) $ y + fuzz
              , Point (x + 12) $ y + fuzz
              , Point (x - 12) $ y - fuzz
              , Point (x + 12) $ y - fuzz
              , flip Point (y - 12) $ x + fuzz
              , flip Point (y + 12) $ x + fuzz
              , flip Point (y - 12) $ x - fuzz
              , flip Point (y + 12) $ x - fuzz ]
            randomReverse = if veryrandom `mod` 2 == 0 then id else reverse
            ps = take k $ concat $
              randomReverse
                [ zip (repeat True)  -- diagonal particles don't reach that far
                  $ take 4 (drop ((k100 + itemK + fuzz) `mod` 4) $ cycle psDir4)
                , zip (repeat False)  -- only some cardinal reach far
                  $ take 4 (drop ((k100 + n) `mod` 4) $ cycle psDir8) ]
              ++ [zip (repeat True)
                  $ take 8 (drop ((k100 + fuzz) `mod` 8) $ cycle psFuzz)]
        forM_ ps $ \(centerRaw, tpxy) -> do
          let center = centerRaw && itemK >= 8  -- if few, keep them regular
          mfail <- projectFail source target tpxy veryrandom center
                               iid COrgan True
          case mfail of
            Nothing -> return ()
            Just ProjectBlockTerrain -> return ()
            Just ProjectBlockActor | not $ bproj tb -> return ()
            Just failMsg ->
              execSfxAtomic $ SfxMsgFid (bfid tb) $ SfxUnexpected failMsg
      tryFlying 0 = return ()
      tryFlying k100 = do
        -- Explosion particles were placed among organs of the victim:
        bag2 <- getsState $ borgan . getActorBody target
        -- We stop bouncing old particles when less than half remains,
        -- to prevent hoarding explosives to use only in cramped spaces.
        case EM.lookup iid bag2 of
          Just (n2, _) | n2 >= itemK `div` 2 -> do
            projectN k100 n2
            tryFlying $ k100 - 1
          _ -> return ()
  -- Some of the particles that fail to take off, bounce off obstacles
  -- up to 100 times in total, trying to fly in different directions.
  tryFlying 100
  bag3 <- getsState $ borgan . getActorBody target
  let mn3 = EM.lookup iid bag3
  -- Give up and destroy the remaining particles, if any.
  maybe (return ()) (\kit -> execUpdAtomic
                             $ UpdLoseItem False iid kit container) mn3
  return UseUp  -- we neglect verifying that at least one projectile got off

-- ** RefillHP

-- Unaffected by armor.
effectRefillHP :: MonadServerAtomic m
               => Int -> ActorId -> ActorId -> m UseResult
effectRefillHP power0 source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  curChalSer <- getsServer $ scurChalSer . soptions
  fact <- getsState $ (EM.! bfid tb) . sfactionD
  let power = if power0 <= -1 then power0 else max 1 power0  -- avoid 0
      deltaHP = xM power
  if | cfish curChalSer && deltaHP > 0
       && fhasUI (gplayer fact) && bfid sb /= bfid tb -> do
       execSfxAtomic $ SfxMsgFid (bfid tb) SfxColdFish
       return UseId
     | otherwise -> do
       let reportedEffect = IK.RefillHP power
       execSfxAtomic $ SfxEffect (bfid sb) target reportedEffect deltaHP
       refillHP source target deltaHP
       return UseUp

-- ** RefillCalm

effectRefillCalm :: MonadServerAtomic m
                 => m () -> Int -> ActorId -> ActorId -> m UseResult
effectRefillCalm execSfx power0 source target = do
  tb <- getsState $ getActorBody target
  actorMaxSk <- getsState $ getActorMaxSkills target
  let power = if power0 <= -1 then power0 else max 1 power0  -- avoid 0
      rawDeltaCalm = xM power
      calmMax = Ability.getSk Ability.SkMaxCalm actorMaxSk
      serious = rawDeltaCalm <= minusM2 && source /= target && not (bproj tb)
      deltaCalm0 | serious =  -- if overfull, at least cut back to max
                     min rawDeltaCalm (xM calmMax - bcalm tb)
                 | otherwise = rawDeltaCalm
      deltaCalm = if | deltaCalm0 > 0 && bcalm tb > xM 999 ->  -- UI limit
                       tenthM  -- avoid nop, to avoid loops
                     | deltaCalm0 < 0 && bcalm tb < - xM 999 ->
                       -tenthM
                     | otherwise -> deltaCalm0
  execSfx
  updateCalm target deltaCalm
  return UseUp

-- ** Dominate

-- The is another way to trigger domination (the normal way is by zeroed Calm).
-- Calm is here irrelevant. The other conditions are the same.
effectDominate :: MonadServerAtomic m => ActorId -> ActorId -> m UseResult
effectDominate source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if | bproj tb -> return UseDud
     | bfid tb == bfid sb -> return UseDud  -- accidental hit; ignore
     | otherwise -> do
       fact <- getsState $ (EM.! bfid tb) . sfactionD
       hiImpression <- highestImpression tb
       let permitted = case hiImpression of
             Nothing -> False  -- no impression, no domination
             Just (hiImpressionFid, hiImpressionK) ->
                hiImpressionFid == bfid sb
                  -- highest impression needs to be by us
                && (fleaderMode (gplayer fact) /= LeaderNull
                    || hiImpressionK >= 10)
                     -- to tame/hack animal/robot, impress them a lot first
       if permitted then do
         b <- dominateFidSfx source target (bfid sb)
         return $! if b then UseUp else UseDud
       else do
         execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxUnimpressed target
         when (source /= target) $
           execSfxAtomic $ SfxMsgFid (bfid tb) $ SfxUnimpressed target
         return UseDud

highestImpression :: MonadServerAtomic m
                  => Actor -> m (Maybe (FactionId, Int))
highestImpression tb = do
  getKind <- getsState $ flip getIidKindServer
  getItem <- getsState $ flip getItemBody
  let isImpression iid =
        maybe False (> 0) $ lookup IK.S_IMPRESSED $ IK.ifreq $ getKind iid
      impressions = EM.filterWithKey (\iid _ -> isImpression iid) $ borgan tb
      f (_, (k, _)) = k
      maxImpression = maximumBy (Ord.comparing f) $ EM.assocs impressions
  if EM.null impressions
  then return Nothing
  else case jfid $ getItem $ fst maxImpression of
    Nothing -> return Nothing
    Just fid -> assert (fid /= bfid tb)
                $ return $ Just (fid, fst $ snd maxImpression)

dominateFidSfx :: MonadServerAtomic m
               => ActorId ->  ActorId -> FactionId -> m Bool
dominateFidSfx source target fid = do
  tb <- getsState $ getActorBody target
  let !_A = assert (not $ bproj tb) ()
  -- Actors that don't move freely can't be dominated, for otherwise,
  -- when they are the last survivors, they could get stuck and the game
  -- wouldn't end. Also, they are a hassle to guide through the dungeon.
  canTra <- getsState $ canTraverse target
  -- Being pushed protects from domination, for simplicity.
  -- A possible interesting exploit, but much help from content would be needed
  -- to make it practical.
  if isNothing (btrajectory tb) && canTra && bhp tb > 0 then do
    let execSfx = execSfxAtomic $ SfxEffect fid target IK.Dominate 0
    execSfx  -- if actor ours, possibly the last occasion to see him
    dominateFid fid source target
    -- If domination resulted in game over, the message won't be seen
    -- before the end game screens, but at least it will be seen afterwards
    -- and browsable in history while inside subsequent game, revealing
    -- the cause of the previous game over. Better than no message at all.
    execSfx  -- see the actor as theirs, unless position not visible
    return True
  else
    return False

dominateFid :: MonadServerAtomic m => FactionId -> ActorId -> ActorId -> m ()
dominateFid fid source target = do
  tb0 <- getsState $ getActorBody target
  -- Game over deduced very early, so no further animation nor message
  -- will appear before game end screens. This is good in that our last actor
  -- that yielded will still be on screen when end game messages roll.
  -- This is bad in that last enemy actor that got dominated by us
  -- may not be on screen and we have no clue how we won until
  -- we see history in the next game. Even worse if our ally dominated
  -- the enemy actor. Then we may never learn. Oh well, that's realism.
  deduceKilled target
  electLeader (bfid tb0) (blid tb0) target
  -- Drop all items so that domiation is not too nasty, especially
  -- if the dominated hero runs off or teleports away with gold
  -- or starts hitting with the most potent artifact weapon in the game.
  -- Drop items while still of the original faction
  -- to mark them on the map for other party members to collect.
  dropAllItems target tb0
  tb <- getsState $ getActorBody target
  actorMaxSk <- getsState $ getActorMaxSkills target
  getKind <- getsState $ flip getIidKindServer
  let isImpression iid =
        maybe False (> 0) $ lookup IK.S_IMPRESSED $ IK.ifreq $ getKind iid
      dropAllImpressions = EM.filterWithKey (\iid _ -> not $ isImpression iid)
      borganNoImpression = dropAllImpressions $ borgan tb
  -- Actor is not pushed nor projectile, so @sactorTime@ suffices.
  btime <-
    getsServer $ (EM.! target) . (EM.! blid tb) . (EM.! bfid tb) . sactorTime
  execUpdAtomic $ UpdLoseActor target tb
  let maxCalm = Ability.getSk Ability.SkMaxCalm actorMaxSk
      maxHp = Ability.getSk Ability.SkMaxHP actorMaxSk
      bNew = tb { bfid = fid
                , bcalm = max (xM 10) $ xM maxCalm `div` 2
                , bhp = min (xM maxHp) $ bhp tb + xM 10
                , borgan = borganNoImpression}
  modifyServer $ \ser ->
    ser {sactorTime = updateActorTime fid (blid tb) target btime
                      $ sactorTime ser}
  execUpdAtomic $ UpdSpotActor target bNew
  -- Focus on the dominated actor, by making him a leader.
  setFreshLeader fid target
  factionD <- getsState sfactionD
  let inGame fact2 = case gquit fact2 of
        Nothing -> True
        Just Status{stOutcome=Camping} -> True
        _ -> False
      gameOver = not $ any inGame $ EM.elems factionD
  -- Avoid the spam of identifying items, if game over.
  unless gameOver $ do
    -- Add some nostalgia for the old faction.
    void $ effectCreateItem (Just $ bfid tb) (Just 10) source target Nothing
                            COrgan IK.S_IMPRESSED IK.timerNone
    -- Identify organs that won't get identified by use.
    getKindId <- getsState $ flip getIidKindIdServer
    let discoverIf (iid, cstore) = do
          let itemKindId = getKindId iid
              c = CActor target cstore
          assert (cstore /= CGround) $
            discoverIfMinorEffects c iid itemKindId
        aic = (btrunk tb, COrgan)
              : filter ((/= btrunk tb) . fst) (getCarriedIidCStore tb)
    mapM_ discoverIf aic

-- | Drop all actor's items.
dropAllItems :: MonadServerAtomic m => ActorId -> Actor -> m ()
dropAllItems aid b =
  mapActorCStore_ CEqp (dropCStoreItem False False CEqp aid b maxBound) b

-- ** Impress

effectImpress :: MonadServerAtomic m
              => (IK.Effect -> m UseResult) -> m () -> ActorId -> ActorId
              -> m UseResult
effectImpress recursiveCall execSfx source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if | bproj tb -> return UseDud
     | bfid tb == bfid sb ->
       -- Unimpress wrt others, but only once. The recursive Sfx suffices.
       recursiveCall $ IK.DropItem 1 1 COrgan IK.S_IMPRESSED
     | otherwise -> do
       -- Actors that don't move freely and so are stupid, can't be impressed.
       canTra <- getsState $ canTraverse target
       if canTra then do
         unless (bhp tb <= 0)
           execSfx  -- avoid spam just before death
         effectCreateItem (Just $ bfid sb) (Just 1) source target Nothing COrgan
                          IK.S_IMPRESSED IK.timerNone
       else return UseDud  -- no message, because common and not crucial

-- ** PutToSleep

effectPutToSleep :: MonadServerAtomic m => m () -> ActorId -> m UseResult
effectPutToSleep execSfx target = do
  tb <- getsState $ getActorBody target
  if | bproj tb -> return UseDud
     | bwatch tb `elem` [WSleep, WWake] -> return UseId  -- can't increase sleep
     | otherwise -> do
       actorMaxSk <- getsState $ getActorMaxSkills target
       let maxCalm = xM $ Ability.getSk Ability.SkMaxCalm actorMaxSk
           deltaCalm = maxCalm - bcalm tb
       when (deltaCalm > 0) $
         updateCalm target deltaCalm  -- max Calm, but asleep vulnerability
       execSfx
       case bwatch tb of
         WWait n | n > 0 -> do
           nAll <- removeConditionSingle IK.S_BRACED target
           let !_A = assert (nAll == 0) ()
           return ()
         _ -> return ()
       -- Forced sleep. No check if the actor can sleep naturally.
       addSleep target
       return UseUp

-- ** Yell

-- This is similar to 'reqYell', but also mentions that the actor is startled,
-- because, presumably, he yells involuntarily. It doesn't wake him up
-- via Calm instantly, just like yelling in a dream not always does.
effectYell :: MonadServerAtomic m => m () -> ActorId -> m UseResult
effectYell execSfx target = do
  tb <- getsState $ getActorBody target
  if bproj tb || bhp tb <= 0 then  -- avoid yelling projectiles or corpses
    return UseDud  -- the yell never manifested
  else do
    execSfx
    execSfxAtomic $ SfxTaunt False target
    when (deltaBenign $ bcalmDelta tb) $
      execUpdAtomic $ UpdRefillCalm target minusM
    return UseUp

-- ** Summon

-- Note that the Calm expended doesn't depend on the number of actors summoned.
effectSummon :: MonadServerAtomic m
             => GroupName ItemKind -> Dice.Dice -> ItemId
             -> ActorId -> ActorId -> Bool
             -> m UseResult
effectSummon grp nDm iid source target periodic = do
  -- Obvious effect, nothing announced.
  cops@COps{coTileSpeedup} <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  sMaxSk <- getsState $ getActorMaxSkills source
  tMaxSk <- getsState $ getActorMaxSkills target
  totalDepth <- getsState stotalDepth
  lvl@Level{ldepth, lbig} <- getLevel (blid tb)
  nFriends <- getsState $ length . friendRegularAssocs (bfid sb) (blid sb)
  discoAspect <- getsState sdiscoAspect
  power0 <- rndToAction $ castDice ldepth totalDepth nDm
  let arItem = discoAspect EM.! iid
      power = max power0 1  -- KISS, always at least one summon
      -- We put @source@ instead of @target@ and @power@ instead of dice
      -- to make the message more accurate.
      effect = IK.Summon grp $ Dice.intToDice power
      durable = IA.checkFlag Ability.Durable arItem
      warnBothActors warning =
       unless (bproj sb) $ do
         execSfxAtomic $ SfxMsgFid (bfid sb) warning
         when (source /= target) $
           execSfxAtomic $ SfxMsgFid (bfid tb) warning
      deltaCalm = - xM 30
  -- Verify Calm only at periodic activations or if the item is durable.
  -- Otherwise summon uses up the item, which prevents summoning getting
  -- out of hand. I don't verify Calm otherwise, to prevent an exploit
  -- via draining one's calm on purpose when an item with good activation
  -- has a nasty summoning side-effect (the exploit still works on durables).
  if | (periodic || durable) && not (bproj sb)
       && (bcalm sb < - deltaCalm || not (calmEnough sb sMaxSk)) -> do
       warnBothActors $ SfxSummonLackCalm source
       return UseId
     | nFriends >= 20 -> do
       -- We assume the actor tries to summon his teammates or allies.
       -- As he repeats such summoning, he is going to bump into this limit.
       -- If he summons others, see the next condition.
       warnBothActors $ SfxSummonTooManyOwn source
       return UseId
     | EM.size lbig >= 200 -> do  -- lower than the 300 limit for spawning
       -- Even if the actor summons foes, he is prevented from exploiting it
       -- too many times and stopping natural monster spawning on the level
       -- (e.g., by filling the level with harmless foes).
       warnBothActors $ SfxSummonTooManyAll source
       return UseId
     | otherwise -> do
       unless (bproj sb) $ updateCalm source deltaCalm
       let validTile t = not $ Tile.isNoActor coTileSpeedup t
           ps = nearbyFreePoints cops lvl validTile (bpos tb)
       localTime <- getsState $ getLocalTime (blid tb)
       -- Make sure summoned actors start acting after the victim.
       let actorTurn = ticksPerMeter $ gearSpeed tMaxSk
           targetTime = timeShift localTime actorTurn
           afterTime = timeShift targetTime $ Delta timeClip
       when (length (take power ps) < power) $
          debugPossiblyPrint
            "Server: effectSummon: failed to find enough free positions"
       bs <- forM (take power ps) $ \p -> do
         -- Mark as summoned to prevent immediate chain summoning.
         -- Summon from current depth, not deeper due to many spawns already.
         maid <- addAnyActor True 0 [(grp, 1)] (blid tb) afterTime (Just p)
         case maid of
           Nothing -> return False  -- suspect content; server debug elsewhere
           Just aid -> do
             b <- getsState $ getActorBody aid
             mleader <- getsState $ gleader . (EM.! bfid b) . sfactionD
             when (isNothing mleader) $ setFreshLeader (bfid b) aid
             return True
       if or bs then do
         execSfxAtomic $ SfxEffect (bfid sb) source effect 0
         return UseUp
       else do
         -- We don't display detailed warnings when @addAnyActor@ fails,
         -- e.g., because the actor groups can't be generated on a given level.
         -- However, we at least don't claim any summoning happened
         -- and we offer a general summoning failure messages.
         warnBothActors $ SfxSummonFailure source
         return UseId

-- ** Ascend

-- Note that projectiles can be teleported, too, for extra fun.
effectAscend :: MonadServerAtomic m
             => (IK.Effect -> m UseResult)
             -> m () -> Bool -> ActorId -> ActorId -> Point
             -> m UseResult
effectAscend recursiveCall execSfx up source target pos = do
  b1 <- getsState $ getActorBody target
  let lid1 = blid b1
  destinations <- getsState $ whereTo lid1 pos up . sdungeon
  sb <- getsState $ getActorBody source
  if | actorWaits b1 && source /= target -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxBracedImmune target
       when (source /= target) $
         execSfxAtomic $ SfxMsgFid (bfid b1) $ SfxBracedImmune target
       return UseId
     | null destinations -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxLevelNoMore
       when (source /= target) $
         execSfxAtomic $ SfxMsgFid (bfid b1) SfxLevelNoMore
       -- We keep it useful even in shallow dungeons.
       recursiveCall $ IK.Teleport 30  -- powerful teleport
     | otherwise -> do
       (lid2, pos2) <- rndToAction $ oneOf destinations
       execSfx
       mbtime_bOld <-
         getsServer $ lookupActorTime (bfid b1) lid1 target . sactorTime
       mbtimeTraj_bOld <-
         getsServer $ lookupActorTime (bfid b1) lid1 target . strajTime
       pos3 <- findStairExit (bfid sb) up lid2 pos2
       let switch1 = void $ switchLevels1 (target, b1)
           switch2 = do
             -- Make the initiator of the stair move the leader,
             -- to let him clear the stairs for others to follow.
             let mlead = if bproj b1 then Nothing else Just target
             -- Move the actor to where the inhabitants were, if any.
             switchLevels2 lid2 pos3 (target, b1)
                           mbtime_bOld mbtimeTraj_bOld mlead
       -- The actor will be added to the new level,
       -- but there can be other actors at his new position.
       inhabitants <- getsState $ posToAidAssocs pos3 lid2
       case inhabitants of
         (_, b2) : _ | not $ bproj b1 -> do
           -- Alert about the switch.
           execSfxAtomic $ SfxMsgFid (bfid sb) SfxLevelPushed
           -- Only tell one pushed player, even if many actors, because then
           -- they are projectiles, so not too important.
           when (source /= target) $
             execSfxAtomic $ SfxMsgFid (bfid b2) SfxLevelPushed
           -- Move the actor out of the way.
           switch1
           -- Move the inhabitants out of the way and to where the actor was.
           let moveInh inh = do
                 -- Preserve the old leader, since the actor is pushed,
                 -- so possibly has nothing worhwhile to do on the new level
                 -- (and could try to switch back, if made a leader,
                 -- leading to a loop).
                 mbtime_inh <-
                   getsServer $ lookupActorTime (bfid (snd inh)) lid2 (fst inh)
                                . sactorTime
                 mbtimeTraj_inh <-
                   getsServer $ lookupActorTime (bfid (snd inh)) lid2 (fst inh)
                                . strajTime
                 inhMLead <- switchLevels1 inh
                 switchLevels2 lid1 (bpos b1) inh
                               mbtime_inh mbtimeTraj_inh inhMLead
           mapM_ moveInh inhabitants
           -- Move the actor to his destination.
           switch2
         _ -> do  -- no inhabitants or the stair-taker a projectile
           switch1
           switch2
       return UseUp

findStairExit :: MonadStateRead m
              => FactionId -> Bool -> LevelId -> Point -> m Point
findStairExit side moveUp lid pos = do
  COps{coTileSpeedup} <- getsState scops
  fact <- getsState $ (EM.! side) . sfactionD
  lvl <- getLevel lid
  let defLanding = uncurry Vector $ if moveUp then (1, 0) else (-1, 0)
      center = uncurry Vector $ if moveUp then (-1, 0) else (1, 0)
      (mvs2, mvs1) = break (== defLanding) moves
      mvs = center : filter (/= center) (mvs1 ++ mvs2)
      ps = filter (Tile.isWalkable coTileSpeedup . (lvl `at`))
           $ map (shift pos) mvs
      posOcc :: State -> Int -> Point -> Bool
      posOcc s k p = case posToAidAssocs p lid s of
        [] -> k == 0
        (_, b) : _ | bproj b -> k == 3
        (_, b) : _ | isFoe side fact (bfid b) -> k == 1  -- non-proj foe
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
  execUpdAtomic $ UpdLoseActor aid bOld
  return mlead

switchLevels2 ::MonadServerAtomic m
              => LevelId -> Point -> (ActorId, Actor)
              -> Maybe Time -> Maybe Time -> Maybe ActorId
              -> m ()
switchLevels2 lidNew posNew (aid, bOld) mbtime_bOld mbtimeTraj_bOld mlead = do
  let lidOld = blid bOld
      side = bfid bOld
  let !_A = assert (lidNew /= lidOld `blame` "stairs looped" `swith` lidNew) ()
  -- Sync actor's items' timeouts with the new local time of the level.
  -- We need to sync organs and equipment due to periodic activations,
  -- but also due to timeouts after use, e.g., for some weapons
  -- (they recharge also in the stash sometimes;
  -- however, this doesn't encourage micromanagement for periodic
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
  -- used item is put from equipment to stash and at once used on another level
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
                  , beqp = rebaseTimeout $ beqp bOld }
  -- Sync the actor time with the level time.
  -- This time shift may cause a double move of a foe of the same speed,
  -- but this is OK --- the foe didn't have a chance to move
  -- before, because the arena went inactive, so he moves now one more time.
  maybe (return ())
        (\btime_bOld ->
    modifyServer $ \ser ->
      ser {sactorTime = updateActorTime (bfid bNew) lidNew aid
                                        (shiftByDelta btime_bOld)
                        $ sactorTime ser})
        mbtime_bOld
  maybe (return ())
        (\btime_bOld ->
    modifyServer $ \ser ->
      ser {strajTime = updateActorTime (bfid bNew) lidNew aid
                                       (shiftByDelta btime_bOld)
                       $ strajTime ser})
        mbtimeTraj_bOld
  -- Materialize the actor at the new location.
  -- Onlookers see somebody appear suddenly. The actor himself
  -- sees new surroundings and has to reset his perception.
  execUpdAtomic $ UpdSpotActor aid bNew
  case mlead of
    Nothing -> return ()
    Just leader ->
      -- The leader is fresh in the sense that he's on a new level
      -- and so doesn't have up to date Perception.
      setFreshLeader side leader

-- ** Escape

-- | The faction leaves the dungeon.
effectEscape :: MonadServerAtomic m => m () -> ActorId -> ActorId -> m UseResult
effectEscape execSfx source target = do
  -- Obvious effect, nothing announced.
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let fid = bfid tb
  fact <- getsState $ (EM.! fid) . sfactionD
  if | bproj tb ->
       return UseDud  -- basically a misfire
     | not (fcanEscape $ gplayer fact) -> do
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxEscapeImpossible
       when (source /= target) $
         execSfxAtomic $ SfxMsgFid (bfid tb) SfxEscapeImpossible
       return UseId
     | otherwise -> do
       execSfx
       deduceQuits (bfid tb) $ Status Escape (fromEnum $ blid tb) Nothing
       return UseUp

-- ** Paralyze

-- | Advance target actor time by this many time clips. Not by actor moves,
-- to hurt fast actors more.
effectParalyze :: MonadServerAtomic m
               => m () -> Dice.Dice -> ActorId -> ActorId -> m UseResult
effectParalyze execSfx nDm source target = do
  tb <- getsState $ getActorBody target
  if bproj tb then return UseDud else  -- shortcut for speed
    paralyze execSfx nDm source target

paralyze :: MonadServerAtomic m
         => m () -> Dice.Dice -> ActorId -> ActorId -> m UseResult
paralyze execSfx nDm source target = do
  tb <- getsState $ getActorBody target
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  power0 <- rndToAction $ castDice ldepth totalDepth nDm
  let power = max power0 1  -- KISS, avoid special case
  actorStasis <- getsServer sactorStasis
  if | ES.member target actorStasis -> do
       sb <- getsState $ getActorBody source
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxStasisProtects
       when (source /= target) $
         execSfxAtomic $ SfxMsgFid (bfid tb) SfxStasisProtects
       return UseId
     | otherwise -> do
       execSfx
       let t = timeDeltaScale (Delta timeClip) power
       -- Only the normal time, not the trajectory time, is affected.
       modifyServer $ \ser ->
         ser { sactorTime = ageActor (bfid tb) (blid tb) target t
                            $ sactorTime ser
             , sactorStasis = ES.insert target (sactorStasis ser) }
                 -- actor's time warped, so he is in stasis,
                 -- immune to further warps
       return UseUp

-- ** ParalyzeInWater

-- | Advance target actor time by this many time clips. Not by actor moves,
-- to hurt fast actors more. Due to water, so resistable.
effectParalyzeInWater :: MonadServerAtomic m
                      => m () -> Dice.Dice -> ActorId -> ActorId -> m UseResult
effectParalyzeInWater execSfx nDm source target = do
  tb <- getsState $ getActorBody target
  if bproj tb then return UseDud else do  -- shortcut for speed
    actorMaxSk <- getsState $ getActorMaxSkills target
    let swimmingOrFlying = max (Ability.getSk Ability.SkSwimming actorMaxSk)
                               (Ability.getSk Ability.SkFlying actorMaxSk)
    if Dice.supDice nDm > swimmingOrFlying
    then paralyze execSfx nDm source target  -- no help at all
    else  -- fully resisted
      -- Don't spam:
      -- sb <- getsState $ getActorBody source
      -- execSfxAtomic $ SfxMsgFid (bfid sb) SfxWaterParalysisResisted
      return UseId

-- ** InsertMove

-- | Give target actor the given number of tenths of extra move. Don't give
-- an absolute amount of time units, to benefit slow actors more.
effectInsertMove :: MonadServerAtomic m
                 => m () -> Dice.Dice -> ActorId -> ActorId -> m UseResult
effectInsertMove execSfx nDm source target = do
  tb <- getsState $ getActorBody target
  actorMaxSk <- getsState $ getActorMaxSkills target
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  actorStasis <- getsServer sactorStasis
  power0 <- rndToAction $ castDice ldepth totalDepth nDm
  let power = max power0 1  -- KISS, avoid special case
      actorTurn = ticksPerMeter $ gearSpeed actorMaxSk
      t = timeDeltaScale (timeDeltaPercent actorTurn 10) (-power)
  if | bproj tb -> return UseDud  -- shortcut for speed
     | ES.member target actorStasis -> do
       sb <- getsState $ getActorBody source
       execSfxAtomic $ SfxMsgFid (bfid sb) SfxStasisProtects
       when (source /= target) $
         execSfxAtomic $ SfxMsgFid (bfid tb) SfxStasisProtects
       return UseId
     | otherwise -> do
       execSfx
       -- Only the normal time, not the trajectory time, is affected.
       modifyServer $ \ser ->
         ser { sactorTime = ageActor (bfid tb) (blid tb) target t
                            $ sactorTime ser
             , sactorStasis = ES.insert target (sactorStasis ser) }
                 -- actor's time warped, so he is in stasis,
                 -- immune to further warps
       return UseUp

-- ** Teleport

-- | Teleport the target actor.
-- Note that projectiles can be teleported, too, for extra fun.
effectTeleport :: MonadServerAtomic m
               => m () -> Dice.Dice -> ActorId -> ActorId -> m UseResult
effectTeleport execSfx nDm source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if actorWaits tb && source /= target
       -- immune only against not own effects, to enable teleport as beneficial
       -- necklace drawback; also consistent with sleep not protecting
  then do
    execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxBracedImmune target
    when (source /= target) $
      execSfxAtomic $ SfxMsgFid (bfid tb) $ SfxBracedImmune target
    return UseId
  else do
    COps{coTileSpeedup} <- getsState scops
    totalDepth <- getsState stotalDepth
    lvl@Level{ldepth} <- getLevel (blid tb)
    range <- rndToAction $ castDice ldepth totalDepth nDm
    let spos = bpos tb
        dMinMax !delta !pos =
          let d = chessDist spos pos
          in d >= range - delta && d <= range + delta
        dist !delta !pos _ = dMinMax delta pos
    mtpos <- rndToAction $ findPosTry 200 lvl
      (\p !t -> Tile.isWalkable coTileSpeedup t
                && not (Tile.isNoActor coTileSpeedup t)
                && not (occupiedBigLvl p lvl)
                && not (occupiedProjLvl p lvl))
      [ dist 1
      , dist $ 1 + range `div` 9
      , dist $ 1 + range `div` 7
      , dist $ 1 + range `div` 5
      , dist 5
      , dist 7
      , dist 9
      ]
    case mtpos of
      Nothing -> do  -- really very rare, so debug
        debugPossiblyPrint
          "Server: effectTeleport: failed to find any free position"
        execSfxAtomic $ SfxMsgFid (bfid sb) SfxTransImpossible
        when (source /= target) $
          execSfxAtomic $ SfxMsgFid (bfid tb) SfxTransImpossible
        return UseId
      Just tpos -> do
        execSfx
        execUpdAtomic $ UpdMoveActor target spos tpos
        return UseUp

-- ** CreateItem

effectCreateItem :: MonadServerAtomic m
                 => Maybe FactionId -> Maybe Int -> ActorId -> ActorId
                 -> Maybe ItemId -> CStore -> GroupName ItemKind -> IK.TimerDice
                 -> m UseResult
effectCreateItem jfidRaw mcount source target miidOriginal store grp tim = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  let fscale unit nDm = do
        k0 <- rndToAction $ castDice ldepth totalDepth nDm
        let k = max 1 k0  -- KISS, don't freak out if dice permit 0
        return $! timeDeltaScale unit k
      fgame = fscale (Delta timeTurn)
      factor nDm = do
        actorMaxSk <- getsState $ getActorMaxSkills target
        -- A tiny bit added to make sure length 1 effect doesn't end before
        -- the end of first turn, which would make, e.g., speed, useless.
        let actorTurn =
              timeDeltaPercent (ticksPerMeter $ gearSpeed actorMaxSk) 101
        fscale actorTurn nDm
  delta <- IK.foldTimer (return $ Delta timeZero) fgame factor tim
  let c = CActor target store
  bagBefore <- getsState $ getBodyStoreBag tb store
  -- Power depth of new items unaffected by number of spawned actors.
  freq <- prepareItemKind 0 (blid tb) [(grp, 1)]
  m2 <- rollItemAspect freq (blid tb)
  case m2 of
    Nothing -> return UseDud  -- e.g., unique already generated
    Just (itemKnownRaw, (itemFullRaw, kitRaw)) -> do
      -- Avoid too many different item identifiers (one for each faction)
      -- for blasts or common item generating tiles. Conditions are
      -- allowed to be duplicated, because they provide really useful info
      -- (perpetrator). However, if timer is none, they are not duplicated
      -- to make sure that, e.g., poisons stack with each other regardless
      -- of perpetrator and we don't get "no longer poisoned" message
      -- while still poisoned due to another faction. With timed aspects,
      -- e.g., slowness, the message is less misleading, and it's interesting
      -- that I'm twice slower due to aspects from two factions and not
      -- as deadly as being poisoned at twice the rate from two factions.
      let jfid = if store == COrgan && not (IK.isTimerNone tim)
                    || grp == IK.S_IMPRESSED
                 then jfidRaw
                 else Nothing
          (itemKnown, itemFull) =
            let ItemKnown kindIx ar _ = itemKnownRaw
            in ( ItemKnown kindIx ar jfid
               , itemFullRaw {itemBase = (itemBase itemFullRaw) {jfid}} )
          kitNew = case mcount of
            Just itemK -> (itemK, [])
            Nothing -> kitRaw
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
            -- It's hard for the client to tell this timer change from charge
            -- use, timer reset on pickup, etc., so we create the msg manually.
            -- Sending to both involved factions lets the player notice
            -- both the extensions he caused and suffered. Other faction causing
            -- that on themselves or on others won't be noticed. TMI.
            execSfxAtomic $ SfxMsgFid (bfid sb)
                          $ SfxTimerExtended target iid store delta
            when (bfid sb /= bfid tb) $
              execSfxAtomic $ SfxMsgFid (bfid tb)
                            $ SfxTimerExtended target iid store delta
            return UseUp
          else return UseDud  -- probably incorrect content, but let it be
        _ -> do
          case miidOriginal of
            Just iidOriginal | store /= COrgan ->
              execSfxAtomic $ SfxMsgFid (bfid tb)
                            $ SfxItemYield iidOriginal (blid tb)
            _ -> return ()
          -- No such items or some items, but void delta, so create items.
          -- If it's, e.g., a periodic poison, the new items will stack with any
          -- already existing items.
          iid <- registerItem True (itemFull, kitNew) itemKnown c
          -- If created not on the ground, ID it, because it won't be on pickup.
          -- If ground and stash coincide, unindentified item enters stash,
          -- so will be identified when equipped, used or dropped
          -- and picked again.
          discoAspect <- getsState sdiscoAspect
          let arItem = discoAspect EM.! iid
          if isJust mcount  -- not a random effect, so probably crafting
             && not (IA.isHumanTrinket (itemKind itemFull))
          then execUpdAtomic $ UpdDiscover c iid (itemKindId itemFull) arItem
          else when (store /= CGround) $
            discoverIfMinorEffects c iid (itemKindId itemFull)
          -- Now, if timer change requested, change the timer,
          -- but in the new items, possibly increased in number wrt old items.
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

-- ** DestroyItem

-- | Make the target actor destroy items in a store from the given group.
-- The item that caused the effect itself is immune (any copies).
-- Durable items are not immune, unlike in @ConsumeItems@.
effectDestroyItem :: MonadServerAtomic m
                  => m () -> ItemId -> Int -> Int -> CStore -> ActorId
                  -> GroupName ItemKind
                  -> m UseResult
effectDestroyItem execSfx iidOriginal ngroup kcopy store target grp = do
  tb <- getsState $ getActorBody target
  isRaw <- allGroupItems store grp target
  let is = filter ((/= iidOriginal) . fst) isRaw
  if | null is -> return UseDud
     | otherwise -> do
       execSfx
       urs <- mapM (uncurry (dropCStoreItem True True store target tb kcopy))
                   (take ngroup is)
       return $! case urs of
         [] -> UseDud  -- there was no effects
         _ -> maximum urs

-- | Drop a single actor's item (though possibly multiple copies).
-- Note that if there are multiple copies, at most one explodes
-- to avoid excessive carnage and UI clutter (let's say,
-- the multiple explosions interfere with each other or perhaps
-- larger quantities of explosives tend to be packaged more safely).
-- Note also that @OnSmash@ effects are activated even if item discharged.
dropCStoreItem :: MonadServerAtomic m
               => Bool -> Bool -> CStore -> ActorId -> Actor -> Int
               -> ItemId -> ItemQuant
               -> m UseResult
dropCStoreItem verbose destroy store aid b kMax iid (k, _) = do
  itemFull <- getsState $ itemToFull iid
  let arItem = aspectRecordFull itemFull
      c = CActor aid store
      fragile = IA.checkFlag Ability.Fragile arItem
      durable = IA.checkFlag Ability.Durable arItem
      isDestroyed = destroy
                    || bproj b && (bhp b <= 0 && not durable || fragile)
                    || IA.checkFlag Ability.Condition arItem
  if isDestroyed then do
    let effApplyFlags = EffApplyFlags
          { effOnCombineOnly    = False
              -- the embed could be combined here but not @iid@
          , effOnSmashOnly      = True
          , effVoluntary        = True
              -- we don't know if it's effVoluntary, so we conservatively assume
              -- it is and we blame @aid@
          , effIgnoreCharging   = False
          , effUseAllCopies     = kMax >= k
          , effKineticPerformed = False
          , effPeriodic         = False
          , effMayDestroy       = True
          }
    void $ effectAndDestroyAndAddKill effApplyFlags aid aid aid iid c itemFull
    -- One copy was destroyed (or none if the item was discharged),
    -- so let's mop up.
    bag <- getsState $ getContainerBag c
    maybe (return UseUp)
          (\(k1, it) ->
             let destroyedSoFar = k - k1
                 k2 = min (kMax - destroyedSoFar) k1
                 kit2 = (k2, take k2 it)
             in if k2 <= 0 then return UseDud else do
               execUpdAtomic $ UpdLoseItem False iid kit2 c
               return UseUp)
          (EM.lookup iid bag)
  else do
    cDrop <- pickDroppable False aid b  -- drop over fog, etc.
    mvCmd <- generalMoveItem verbose iid (min kMax k) (CActor aid store) cDrop
    mapM_ execUpdAtomic mvCmd
    return UseUp

pickDroppable :: MonadStateRead m => Bool -> ActorId -> Actor -> m Container
pickDroppable respectNoItem aid b = do
  cops@COps{coTileSpeedup} <- getsState scops
  lvl <- getLevel (blid b)
  let validTile t = not (respectNoItem && Tile.isNoItem coTileSpeedup t)
  if validTile $ lvl `at` bpos b
  then return $! CActor aid CGround
  else do
    let ps = nearbyFreePoints cops lvl validTile (bpos b)
    return $! case filter (adjacent $ bpos b) $ take 8 ps of
      [] -> CActor aid CGround  -- fallback; still correct, though not ideal
      pos : _ -> CFloor (blid b) pos

-- ** ConsumeItems

-- | Make the target actor destroy the given items, if all present,
-- or none at all, if any is missing. To be used in crafting.
-- The item that caused the effect itself is not considered (any copies).
effectConsumeItems :: MonadServerAtomic m
                   => m () -> ItemId -> ActorId
                   -> [(Int, GroupName ItemKind)]
                   -> [(Int, GroupName ItemKind)]
                   -> m UseResult
effectConsumeItems execSfx iidOriginal target tools0 raw0 = do
  kitAssG <- getsState $ kitAssocs target [CGround]
  kitAssE <- getsState $ kitAssocs target [CEqp]
  let kitAss = listToolsToConsume kitAssG kitAssE
      is = filter ((/= iidOriginal) . fst . snd) kitAss
      grps0 = map (\(x, y) -> (False, x, y)) tools0  -- apply if durable
              ++ map (\(x, y) -> (True, x, y)) raw0  -- destroy always
      (bagsToLose3, iidsToApply3, grps3) =
        foldl' subtractIidfromGrps (EM.empty, [], grps0) is
  if null grps3 then do
    execSfx
    consumeItems target bagsToLose3 iidsToApply3
    return UseUp
  else return UseDud

consumeItems :: MonadServerAtomic m
             => ActorId -> EM.EnumMap CStore ItemBag
             -> [(CStore, (ItemId, ItemFull))]
             -> m ()
consumeItems target bagsToLose iidsToApply = do
  COps{coitem} <- getsState scops
  let identifyStoreBag store bag =
        mapM_ (identifyStoreIid store) $ EM.keys bag
      identifyStoreIid store iid = do
        discoAspect2 <- getsState sdiscoAspect
          -- might have changed due to embedded items invocations
        itemKindId <- getsState $ getIidKindIdServer iid
        let arItem = discoAspect2 EM.! iid
            c = CActor target store
            itemKind = okind coitem itemKindId
        unless (IA.isHumanTrinket itemKind) $  -- a hack
          execUpdAtomic $ UpdDiscover c iid itemKindId arItem
  -- We don't invoke @OnSmash@ effects, so we avoid the risk
  -- of the first removed item displacing the actor, destroying
  -- or scattering some pending items ahead of time, etc.
  -- The embed should provide any requisite fireworks instead.
  forM_ (EM.assocs bagsToLose) $ \(store, bagToLose) ->
    unless (EM.null bagToLose) $ do
      identifyStoreBag store bagToLose
      -- Not @UpdLoseItemBag@, to be verbose.
      -- The bag is small, anyway.
      let c = CActor target store
      itemD <- getsState sitemD
      mapWithKeyM_ (\iid kit ->
        let itemBase = itemD EM.! iid
        in execUpdAtomic $ UpdDestroyItem True iid itemBase kit c) bagToLose
  -- But afterwards we do apply normal effects of durable items,
  -- even if the actor or other items displaced in the process.
  -- This makes applying double-purpose tool-weapons costly,
  -- which is also why durable tools are considered last.
  let applyItemIfPresent (store, (iid, itemFull)) = do
        let c = CActor target store
        bag <- getsState $ getContainerBag c
        when (iid `EM.member` bag) $ do
          execSfxAtomic $ SfxApply target iid
          -- Treated as if the actor only activated the item on himself,
          -- without kinetic damage, to avoid the exploit of wearing armor
          -- when using tools or transforming terrain.
          -- Also, timeouts of the item ignored to prevent exploit
          -- by discharging the item before using it.
          let effApplyFlags = EffApplyFlags
                { effOnCombineOnly    = False
                , effOnSmashOnly      = False
                , effVoluntary        = True
                , effIgnoreCharging   = True
                , effUseAllCopies     = False
                , effKineticPerformed = False
                , effPeriodic         = False
                , effMayDestroy       = False
                }
          void $ effectAndDestroyAndAddKill effApplyFlags
                                            target target target iid c itemFull
  mapM_ applyItemIfPresent iidsToApply

-- ** DropItem

-- | Make the target actor drop items in a store from the given group.
-- The item that caused the effect itself is immune (any copies).
effectDropItem :: MonadServerAtomic m
               => m () -> ItemId -> Int -> Int -> CStore
               -> GroupName ItemKind -> ActorId
               -> m UseResult
effectDropItem execSfx iidOriginal ngroup kcopy store grp target = do
  tb <- getsState $ getActorBody target
  fact <- getsState $ (EM.! bfid tb) . sfactionD
  isRaw <- allGroupItems store grp target
  curChalSer <- getsServer $ scurChalSer . soptions
  factionD <- getsState sfactionD
  let is = filter ((/= iidOriginal) . fst) isRaw
  if | bproj tb || null is -> return UseDud
     | ngroup == maxBound && kcopy == maxBound
       && store `elem` [CStash, CEqp]
       && fhasGender (gplayer fact)  -- hero in Allure's decontamination chamber
       && (cdiff curChalSer == 1     -- at lowest difficulty for its faction
           && any (fhasUI . gplayer . snd)
                  (filter (\(fi, fa) -> isFriend fi fa (bfid tb))
                          (EM.assocs factionD))
           || cdiff curChalSer == difficultyBound
              && any (fhasUI . gplayer  . snd)
                     (filter (\(fi, fa) -> isFoe fi fa (bfid tb))
                             (EM.assocs factionD))) ->
{-
A hardwired hack, because AI heroes don't cope with Allure's decontamination
chamber; beginners may struggle too, so this is trigered by difficulty.
- AI heroes don't switch leader to the hero past laboratory to equip
weapons from stash between the in-lab hero picks up the loot pile
and himself enters the decontamination chamber
- all consumables always end up in a pack and the whole pack
is always left behind, because consumables are not shared among
actors via shared stash (yet); we could pack consumables to stash
by default, but it's too confusing and risky for beginner players
and doesn't work for heroes that have not enough Calm ATM and AI
would still need to learn to spread consumables from stash to packs afterwards
- the items of the last actor would be lost anyway, unless AI
is taught the foolproof solution of this puzzle, which is yet a bit more
specific than the two general abilities described as desirable above
-}
       return UseUp
     | otherwise -> do
       unless (store == COrgan) execSfx
       urs <- mapM (uncurry (dropCStoreItem True False store target tb kcopy))
                   (take ngroup is)
       return $! case urs of
         [] -> UseDud  -- there was no effects
         _ -> maximum urs

-- ** Discharge

effectDischarge :: MonadServerAtomic m
                => m () -> ItemId -> Dice.Dice -> ActorId -> m UseResult
effectDischarge execSfx iidOriginal nDm target = do
  discoAspect <- getsState sdiscoAspect
  tb <- getsState $ getActorBody target
  localTime <- getsState $ getLocalTime (blid tb)
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel (blid tb)
  power0 <- rndToAction $ castDice ldepth totalDepth nDm
  let power = max 0 power0
      t = timeShift localTime $ timeDeltaScale (Delta timeClip) power
      c = CActor target CEqp
      eqpAss = EM.assocs $ beqp tb
      resetTimeout (iid, (k, itemTimer)) = do
        let arItem = discoAspect EM.! iid
            timeout = IA.aTimeout arItem
            timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
            charging startT = timeShift startT timeoutTurns > localTime
            it1 = filter charging itemTimer
            it2 = filter charging
                  $ replicate k $ timeShift t (timeDeltaReverse timeoutTurns)
        if iid == iidOriginal || it1 == it2 then return UseDud else do
          execUpdAtomic $ UpdTimeItem iid c itemTimer it2
          return UseUp
  urs <- mapM resetTimeout eqpAss
  let ur = case urs of
        [] -> UseDud  -- there was no effects
        _ -> maximum urs
  case ur of
    UseDud -> return UseDud
    _ -> do
      execSfx
      return UseUp

-- ** PolyItem

-- Can't apply to the item itself (any copies).
effectPolyItem :: MonadServerAtomic m
               => m () -> ItemId -> ActorId -> m UseResult
effectPolyItem execSfx iidOriginal target = do
  tb <- getsState $ getActorBody target
  let cstore = CGround
  kitAss <- getsState $ kitAssocs target [cstore]
  case filter ((/= iidOriginal) . fst) kitAss of
    [] -> do
      execSfxAtomic $ SfxMsgFid (bfid tb) SfxPurposeNothing
      -- Do not spam the source actor player about the failures.
      return UseId
    (iid, ( itemFull@ItemFull{itemBase, itemKindId, itemKind}
          , (itemK, itemTimer) )) : _ -> do
      let arItem = aspectRecordFull itemFull
          maxCount = Dice.supDice $ IK.icount itemKind
      if | IA.checkFlag Ability.Unique arItem -> do
           execSfxAtomic $ SfxMsgFid (bfid tb) SfxPurposeUnique
           return UseId
         | maybe True (<= 0) $ lookup IK.COMMON_ITEM $ IK.ifreq itemKind -> do
           execSfxAtomic $ SfxMsgFid (bfid tb) SfxPurposeNotCommon
           return UseId
         | itemK < maxCount -> do
           execSfxAtomic $ SfxMsgFid (bfid tb)
                         $ SfxPurposeTooFew maxCount itemK
           return UseId
         | otherwise -> do
           -- Only the required number of items is used up, not all of them.
           let c = CActor target cstore
               kit = (maxCount, take maxCount itemTimer)
           execSfx
           identifyIid iid c itemKindId itemKind
           execUpdAtomic $ UpdDestroyItem False iid itemBase kit c
           effectCreateItem (Just $ bfid tb) Nothing
                            target target Nothing cstore
                            IK.COMMON_ITEM IK.timerNone

-- ** RerollItem

-- Can't apply to the item itself (any copies).
effectRerollItem :: forall m . MonadServerAtomic m
                 => m () -> ItemId -> ActorId -> m UseResult
effectRerollItem execSfx iidOriginal target = do
  COps{coItemSpeedup} <- getsState scops
  tb <- getsState $ getActorBody target
  let cstore = CGround  -- if ever changed, call @discoverIfMinorEffects@
  kitAss <- getsState $ kitAssocs target [cstore]
  case filter ((/= iidOriginal) . fst) kitAss of
    [] -> do
      execSfxAtomic $ SfxMsgFid (bfid tb) SfxRerollNothing
      -- Do not spam the source actor player about the failures.
      return UseId
    (iid, ( ItemFull{ itemBase, itemKindId, itemKind
                    , itemDisco=ItemDiscoFull itemAspect }
          , (_, itemTimer) )) : _ ->
      if | IA.kmConst $ getKindMean itemKindId coItemSpeedup -> do
           execSfxAtomic $ SfxMsgFid (bfid tb) SfxRerollNotRandom
           return UseId
         | otherwise -> do
           let c = CActor target cstore
               kit = (1, take 1 itemTimer)  -- prevent micromanagement
               freq = pure (itemKindId, itemKind)
           execSfx
           identifyIid iid c itemKindId itemKind
           execUpdAtomic $ UpdDestroyItem False iid itemBase kit c
           dungeon <- getsState sdungeon
           let maxLid = fst $ maximumBy (Ord.comparing (ldepth . snd))
                            $ EM.assocs dungeon
               roll100 :: Int -> m (ItemKnown, ItemFullKit)
               roll100 n = do
                 m2 <- rollItemAspect freq maxLid
                 case m2 of
                   Nothing ->
                     error "effectRerollItem: can't create rerolled item"
                   Just i2@(ItemKnown _ ar2 _, _) ->
                     if ar2 == itemAspect && n > 0
                     then roll100 (n - 1)
                     else return i2
           (itemKnown, (itemFull, _)) <- roll100 100
           void $ registerItem True (itemFull, kit) itemKnown c
           return UseUp
    _ -> error "effectRerollItem: server ignorant about an item"

-- ** DupItem

-- Can't apply to the item itself (any copies).
effectDupItem :: MonadServerAtomic m => m () -> ItemId -> ActorId -> m UseResult
effectDupItem execSfx iidOriginal target = do
  tb <- getsState $ getActorBody target
  let cstore = CGround  -- beware of other options, e.g., creating in eqp
                        -- and not setting timeout to a random value
  kitAss <- getsState $ kitAssocs target [cstore]
  case filter ((/= iidOriginal) . fst) kitAss of
    [] -> do
      execSfxAtomic $ SfxMsgFid (bfid tb) SfxDupNothing
      -- Do not spam the source actor player about the failures.
      return UseId
    (iid, ( itemFull@ItemFull{itemKindId, itemKind}
          , _ )) : _ -> do
      let arItem = aspectRecordFull itemFull
      if | IA.checkFlag Ability.Unique arItem -> do
           execSfxAtomic $ SfxMsgFid (bfid tb) SfxDupUnique
           return UseId
         | maybe False (> 0) $ lookup IK.VALUABLE $ IK.ifreq itemKind -> do
           execSfxAtomic $ SfxMsgFid (bfid tb) SfxDupValuable
           return UseId
         | otherwise -> do
           let c = CActor target cstore
           execSfx
           identifyIid iid c itemKindId itemKind
           execUpdAtomic $ UpdSpotItem True iid (1, []) c
           return UseUp

-- ** Identify

effectIdentify :: MonadServerAtomic m
               => m () -> ItemId -> ActorId -> m UseResult
effectIdentify execSfx iidOriginal target = do
  COps{coItemSpeedup} <- getsState scops
  discoAspect <- getsState sdiscoAspect
  -- The actor that causes the application does not determine what item
  -- is identifiable, becuase it's the target actor that identifies
  -- his possesions.
  tb <- getsState $ getActorBody target
  sClient <- getsServer $ (EM.! bfid tb) . sclientStates
  let tryFull store as = case as of
        [] -> return False
        (iid, _) : rest | iid == iidOriginal -> tryFull store rest  -- don't id itself
        (iid, ItemFull{itemBase, itemKindId, itemKind}) : rest -> do
          let arItem = discoAspect EM.! iid
              kindIsKnown = case jkind itemBase of
                IdentityObvious _ -> True
                IdentityCovered ix _ -> ix `EM.member` sdiscoKind sClient
          if iid `EM.member` sdiscoAspect sClient  -- already fully identified
             || IA.isHumanTrinket itemKind  -- hack; keep them non-identified
             || store == CGround && IA.onlyMinorEffects arItem itemKind
               -- will be identified when picked up, so don't bother
             || IA.kmConst (getKindMean itemKindId coItemSpeedup)
                && kindIsKnown
               -- constant aspects and known kind; no need to identify further;
               -- this should normally not be needed, since clients should
               -- identify such items for free
          then tryFull store rest
          else do
            let c = CActor target store
            execSfx
            identifyIid iid c itemKindId itemKind
            return True
      tryStore stores = case stores of
        [] -> do
          execSfxAtomic $ SfxMsgFid (bfid tb) SfxIdentifyNothing
          return UseId  -- the message tells it's ID effect
        store : rest -> do
          allAssocs <- getsState $ fullAssocs target [store]
          go <- tryFull store allAssocs
          if go then return UseUp else tryStore rest
  tryStore [CGround, CStash, CEqp]

identifyIid :: MonadServerAtomic m
            => ItemId -> Container -> ContentId ItemKind -> ItemKind -> m ()
identifyIid iid c itemKindId itemKind =
  unless (IA.isHumanTrinket itemKind) $ do
    discoAspect <- getsState sdiscoAspect
    execUpdAtomic $ UpdDiscover c iid itemKindId $ discoAspect EM.! iid

-- ** Detect

effectDetect :: MonadServerAtomic m
             => m () -> IK.DetectKind -> Int -> ActorId -> Point -> m UseResult
effectDetect execSfx d radius target pos = do
  COps{coitem, coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody target
  lvl <- getLevel $ blid b
  s <- getState
  getKind <- getsState $ flip getIidKindServer
  let lootPredicate p =
        p `EM.member` lfloor lvl
        || (case posToBigAssoc p (blid b) s of
              Nothing -> False
              Just (_, body) ->
                let belongings = EM.keys (beqp body)
                      -- shared stash ignored, because hard to get
                in any belongingIsLoot belongings)
        || any embedHasLoot (EM.keys $ getEmbedBag (blid b) p s)
      itemKindIsLoot = isNothing . lookup IK.UNREPORTED_INVENTORY . IK.ifreq
      belongingIsLoot iid = itemKindIsLoot $ getKind iid
      embedHasLoot iid = any effectHasLoot $ IK.ieffects $ getKind iid
      reported acc _ _ itemKind = acc && itemKindIsLoot itemKind
      effectHasLoot (IK.CreateItem _ cstore grp _) =
        cstore `elem` [CGround, CStash, CEqp]
        && ofoldlGroup' coitem grp reported True
      effectHasLoot IK.PolyItem = True
      effectHasLoot IK.RerollItem = True
      effectHasLoot IK.DupItem = True
      effectHasLoot (IK.OneOf l) = any effectHasLoot l
      effectHasLoot (IK.OnSmash eff) = effectHasLoot eff
      effectHasLoot (IK.AndEffect eff1 eff2) =
        effectHasLoot eff1 || effectHasLoot eff2
      effectHasLoot (IK.OrEffect eff1 eff2) =
        effectHasLoot eff1 || effectHasLoot eff2
      effectHasLoot _ = False
      (predicate, action) = case d of
        IK.DetectAll -> (const True, const $ return False)
        IK.DetectActor -> ((`EM.member` lbig lvl), const $ return False)
        IK.DetectLoot -> (lootPredicate, const $ return False)
        IK.DetectExit ->
          let (ls1, ls2) = lstair lvl
          in ((`elem` ls1 ++ ls2 ++ lescape lvl), const $ return False)
        IK.DetectHidden ->
          let predicateH p = Tile.isHideAs coTileSpeedup $ lvl `at` p
              revealEmbed p = do
                embeds <- getsState $ getEmbedBag (blid b) p
                unless (EM.null embeds) $
                  execUpdAtomic $ UpdSpotItemBag (CEmbed (blid b) p) embeds
              actionH l = do
                let f p = when (p /= pos) $ do
                      let t = lvl `at` p
                      execUpdAtomic $ UpdSearchTile target p t
                      -- This is safe searching; embedded items
                      -- are not triggered, but they are revealed.
                      revealEmbed p
                      case EM.lookup p $ lentry lvl of
                        Nothing -> return ()
                        Just entry ->
                          execUpdAtomic $ UpdSpotEntry (blid b) [(p, entry)]
                mapM_ f l
                return $! not $ null l  -- KISS, even if client knows all
          in (predicateH, actionH)
        IK.DetectEmbed -> ((`EM.member` lembed lvl), const $ return False)
  effectDetectX d predicate action execSfx radius target

effectDetectX :: MonadServerAtomic m
              => IK.DetectKind -> (Point -> Bool) -> ([Point] -> m Bool)
              -> m () -> Int -> ActorId -> m UseResult
effectDetectX d predicate action execSfx radius target = do
  COps{corule=RuleContent{rXmax, rYmax}} <- getsState scops
  b <- getsState $ getActorBody target
  sperFidOld <- getsServer sperFid
  let perOld = sperFidOld EM.! bfid b EM.! blid b
      Point x0 y0 = bpos b
      perList = filter predicate
        [ Point x y
        | y <- [max 0 (y0 - radius) .. min (rYmax - 1) (y0 + radius)]
        , x <- [max 0 (x0 - radius) .. min (rXmax - 1) (x0 + radius)]
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
    execSfxAtomic $ SfxMsgFid (bfid b) $ SfxVoidDetection d
  return UseUp  -- even if nothing spotted, in itself it's still useful data

-- ** SendFlying

-- | Send the target actor flying like a projectile. If the actors are adjacent,
-- the vector is directed outwards, if no, inwards, if it's the same actor,
-- boldpos is used, if it can't, a random outward vector of length 10
-- is picked.
effectSendFlying :: MonadServerAtomic m
                 => m () -> IK.ThrowMod -> ActorId -> ActorId -> Container
                 -> Maybe Bool
                 -> m UseResult
effectSendFlying execSfx IK.ThrowMod{..} source target c modePush = do
  v <- sendFlyingVector source target modePush
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let eps = 0
      fpos = bpos tb `shift` v
      isEmbed = case c of
        CEmbed{} -> True
        _ -> False
  if bhp tb <= 0  -- avoid dragging around corpses
     || bproj tb && isEmbed then  -- fyling projectiles can't slip on the floor
    return UseDud  -- the impact never manifested
  else if actorWaits tb
          && source /= target
          && isNothing (btrajectory tb) then do
    execSfxAtomic $ SfxMsgFid (bfid sb) $ SfxBracedImmune target
    when (source /= target) $
      execSfxAtomic $ SfxMsgFid (bfid tb) $ SfxBracedImmune target
    return UseUp  -- waste it to prevent repeated throwing at immobile actors
  else do
   COps{corule=RuleContent{rXmax, rYmax}} <- getsState scops
   case bla rXmax rYmax eps (bpos tb) fpos of
    Nothing -> error $ "" `showFailure` (fpos, tb)
    Just [] -> error $ "projecting from the edge of level"
                       `showFailure` (fpos, tb)
    Just (pos : rest) -> do
      weightAssocs <- getsState $ fullAssocs target [CEqp, COrgan]
      let weight = sum $ map (IK.iweight . itemKind . snd) weightAssocs
          path = bpos tb : pos : rest
          (trajectory, (speed, _)) =
            -- Note that the @ThrowMod@ aspect of the actor's trunk is ignored.
            computeTrajectory weight throwVelocity throwLinger path
          ts = Just (trajectory, speed)
      -- Old and new trajectories are not added; the old one is replaced.
      if btrajectory tb == ts
      then return UseId  -- e.g., actor is too heavy; but a jerk is noticeable
      else do
        execSfx
        execUpdAtomic $ UpdTrajectory target (btrajectory tb) ts
        -- If propeller is a projectile, it pushes involuntarily,
        -- so its originator is to blame.
        -- However, we can't easily see whether a pushed non-projectile actor
        -- pushed another due to colliding or voluntarily, so we assign
        -- blame to him.
        originator <- if bproj sb
                      then getsServer $ EM.findWithDefault source source
                                        . strajPushedBy
                      else return source
        modifyServer $ \ser ->
          ser {strajPushedBy = EM.insert target originator $ strajPushedBy ser}
        -- In case of pre-existing pushing, don't touch the time
        -- so that the pending @advanceTimeTraj@ can do its job
        -- (it will, because non-empty trajectory is here set, unless, e.g.,
        -- subsequent effects from the same item change the trajectory).
        when (isNothing $ btrajectory tb) $ do
          -- Set flying time to almost now, so that the push happens ASAP,
          -- because it's the first one, so almost no delay is needed.
          localTime <- getsState $ getLocalTime (blid tb)
          -- But add a slight overhead to avoid displace-slide loops
          -- of 3 actors in a line.
          let overheadTime = timeShift localTime (Delta timeClip)
          modifyServer $ \ser ->
            ser {strajTime =
                   updateActorTime (bfid tb) (blid tb) target overheadTime
                   $ strajTime ser}
        return UseUp

sendFlyingVector :: MonadServerAtomic m
                 => ActorId -> ActorId -> Maybe Bool -> m Vector
sendFlyingVector source target modePush = do
  sb <- getsState $ getActorBody source
  let boldpos_sb = fromMaybe (bpos sb) (boldpos sb)
  if source == target then
    if boldpos_sb == bpos sb then rndToAction $ do
      z <- randomR (-10, 10)
      oneOf [Vector 10 z, Vector (-10) z, Vector z 10, Vector z (-10)]
    else
      return $! vectorToFrom (bpos sb) boldpos_sb
  else do
    tb <- getsState $ getActorBody target
    let pushV = vectorToFrom (bpos tb) (bpos sb)
        pullV = vectorToFrom (bpos sb) (bpos tb)
    return $! case modePush of
                Just True -> pushV
                Just False -> pullV
                Nothing | adjacent (bpos sb) (bpos tb) -> pushV
                Nothing -> pullV

-- ** DropBestWeapon

-- | Make the target actor drop his best weapon.
-- The item itself is immune (any copies).
effectDropBestWeapon :: MonadServerAtomic m
                     => m () -> ItemId -> ActorId -> m UseResult
effectDropBestWeapon execSfx iidOriginal target = do
  tb <- getsState $ getActorBody target
  if bproj tb then return UseDud else do
    localTime <- getsState $ getLocalTime (blid tb)
    kitAssRaw <- getsState $ kitAssocs target [CEqp]
    let kitAss = filter (\(iid, (i, _)) ->
                          IA.checkFlag Ability.Meleeable (aspectRecordFull i)
                          && iid /= iidOriginal) kitAssRaw
        ignoreCharges = True
    case strongestMelee ignoreCharges Nothing localTime kitAss of
      (_, (_, (iid, _))) : _ -> do
        execSfx
        let kit = beqp tb EM.! iid
        dropCStoreItem True False CEqp target tb 1 iid kit
          -- not the whole stack
      [] ->
        return UseDud

-- ** ApplyPerfume

effectApplyPerfume :: MonadServerAtomic m => m () -> ActorId -> m UseResult
effectApplyPerfume execSfx target = do
  tb <- getsState $ getActorBody target
  Level{lsmell} <- getLevel $ blid tb
  unless (EM.null lsmell) $ do
    execSfx
    let f p fromSm = execUpdAtomic $ UpdAlterSmell (blid tb) p fromSm timeZero
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

-- ** VerbNoLonger

effectVerbNoLonger :: MonadServerAtomic m
                   => Bool -> m () -> ActorId -> m UseResult
effectVerbNoLonger effUseAllCopies execSfx source = do
  b <- getsState $ getActorBody source
  when (effUseAllCopies  -- @UseUp@ ensures that if all used, all destroyed
        && not (bproj b)) $  -- no spam when projectiles activate
    execSfx  -- announce that all copies have run out (or whatever message)
  return UseUp  -- help to destroy the copy, even if not all used up

-- ** VerbMsg

effectVerbMsg :: MonadServerAtomic m => m () -> ActorId -> m UseResult
effectVerbMsg execSfx source = do
  b <- getsState $ getActorBody source
  unless (bproj b) execSfx  -- don't spam when projectiles activate
  return UseUp  -- announcing always successful and this helps
                -- to destroy the item

-- ** AndEffect

effectAndEffect :: forall m. MonadServerAtomic m
                => (IK.Effect -> m UseResult) -> IK.Effect -> IK.Effect
                -> m UseResult
effectAndEffect recursiveCall eff1 eff2 = do
  ur1 <- recursiveCall eff1
  when (ur1 == UseUp) $ void $ recursiveCall eff2
  return ur1
  -- no @execSfx@, because individual effects sent them

-- ** OrEffect

effectOrEffect :: forall m. MonadServerAtomic m
               => (IK.Effect -> m UseResult) -> IK.Effect -> IK.Effect
               -> m UseResult
effectOrEffect recursiveCall eff1 eff2 = do
  ur1 <- recursiveCall eff1
  if ur1 == UseUp
  then return UseUp
  else recursiveCall eff2
  -- no @execSfx@, because individual effects sent them
