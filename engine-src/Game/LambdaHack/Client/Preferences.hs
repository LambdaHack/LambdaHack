-- | Actor preferences for targets and actions, based on actor aspects.
module Game.LambdaHack.Client.Preferences
  ( totalUsefulness
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , effectToBenefit
  , averageTurnValue, avgItemDelay, avgItemLife, durabilityMult
  , organBenefit, recBenefit, fakeItem, aspectToBenefit, aspectRecordToBenefit
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

-- | How much AI benefits from applying the effect.
-- The first component is benefit when applied to self, the second
-- is benefit (preferably negative) when applied to enemy.
-- This represents benefit from using the effect every @avgItemDelay@ turns,
-- so if the item is not durable, the value is adjusted down elsewhere.
-- The benefit includes the drawback of having to use the actor's turn,
-- except when there is battle and item is a weapon and so there is usually
-- nothing better to do than to melee, or when the actor is stuck or idle
-- or laying in wait or luring an enemy from a safe distance.
-- So there is less than @averageTurnValue@ included in each benefit,
-- so in case when turn is not spent, e.g, periodic activation or conditions,
-- the difference in value is only slight.
effectToBenefit :: COps -> FactionId -> FactionDict -> IK.Effect
                -> (Double, Double)
effectToBenefit cops fid factionD eff =
  let delta x = (x, x)
  in case eff of
    IK.Burn d -> delta $ -(min 1500 $ 15 * Dice.meanDice d)
      -- often splash damage, armor doesn't block (but HurtMelee doesn't boost)
    IK.Explode IK.S_SINGLE_SPARK -> delta (-1)
                                      -- hardwired; probing and flavour
    IK.Explode IK.S_FRAGRANCE -> (1, -5)  -- hardwired; situational
    IK.Explode _ ->
      -- There is a risk the explosion is focused and harmful to self
      -- or not focused and beneficial to nearby foes, but not to self.
      -- It's too costly to analyze, so we assume applying an exploding
      -- item is a bad idea and it's better to project it at foes.
      -- Due to this assumption, healing explosives should be wrapped
      -- in @OnSmash@, or else they are counted as an incentive for throwing
      -- an item at foes, which in that case is counterproductive.
      delta (-100)
    IK.RefillHP p ->
      delta $ if p > 0
              then min 2000 (20 * fromIntegral p)
              else max (-1000) (10 * fromIntegral p)
        -- one HP healed is worth a bit more than one HP dealt to enemy,
        -- because if the actor survives, he may deal damage many times;
        -- however, AI is mostly for non-heroes that fight in suicidal crowds,
        -- so the two values are kept close enough to maintain berserk approach
    IK.RefillCalm p ->
      ( if p > 0
        then min 100 (fromIntegral p)
          -- this may cause ice to be attractive to AI,
          -- but it doesn't trigger it due to no @ConsideredByAI@
        else if p >= -5
             then max (-100) (fromIntegral p)
             else max (-1500) (15 * fromIntegral p)
          -- big Calm drains are incredibly dangerous, so don't be stupid
          -- and don't self-inflict them, particularly if you are an intelligent
          -- high-HP actor, which is likely if you collect and apply items
      , if p > 0
        then min 100 (fromIntegral p)
        else max (-500) (5 * fromIntegral p) )
          -- quite a powerful weapon, especially against high-HP foes
    IK.Dominate -> (0, -100)  -- I obtained an actor with, say 10HP,
                              -- worth 200, and enemy lost him, another 100;
                              -- divided by 3, because impression needed first
    IK.Impress -> (0, -20)  -- this causes heroes to waste a crucial resource
                            -- but makes aliens more aggresive than defensive
    IK.PutToSleep -> (10, -50)  -- can affect friends, but more often enemies
    IK.Yell -> (-1, -2)  -- usually uncontrollably wakes up enemies, so bad
    IK.Summon grp d ->  -- contrived by not checking if enemies also control
                        -- that group; safe for normal dungeon crawl content;
                        -- not correct for symmetric scenarios, but let it be
      let ben = Dice.meanDice d * 200  -- the new actor can have, say, 10HP
          fact = factionD EM.! fid
          friendlyHasGrp fid2 =
            isFriend fid fact fid2
            && grp `elem` fgroups (gplayer $ factionD EM.! fid2)
      in -- Prefer applying summoning items to flinging them; the actor gets
         -- spawned further from for, but it's more robust
         if any friendlyHasGrp $ EM.keys factionD
         then (ben, -1)
         else (-ben * 3, 1)  -- the foe may spawn during battle and gang up
    IK.Ascend{} -> (-99, 99)
      -- note the reversed values: only change levels sensibly, in teams,
      -- and don't remove enemy too far, he may be easy to kill
      -- and may have loot
    IK.Escape{} -> (-9999, 9999)  -- even if can escape, loots first and then
                                  -- handles escape as a special case
    -- The following two are expensive, because they ofen activate
    -- while in melee, in which case each turn is worth x HP, where x
    -- is the average effective weapon damage in the game, which would
    -- be ~5. (Plus a huge risk factor for any non-spawner faction.)
    -- So, each turn in battle is worth ~100. And on average, in and out
    -- of battle, let's say each turn is worth ~10.
    IK.Paralyze d -> delta $ -20 * Dice.meanDice d  -- clips
    IK.ParalyzeInWater d -> delta $ -10 * Dice.meanDice d  -- clips; resistable
    IK.InsertMove d -> delta $ 10 * Dice.meanDice d  -- turns
    IK.Teleport d -> if Dice.meanDice d <= 8
                     then (0, 0)    -- annoying either way
                     else (-9, -1)  -- for self, don't derail exploration
                                    -- for foes, fight with one less at a time
    IK.CreateItem _ COrgan IK.CONDITION _ ->
      (1, -1)  -- varied, big bunch, but try to create it anyway
    IK.CreateItem _ COrgan grp timer ->  -- assumed temporary
      let turnTimer = IK.foldTimer 1 Dice.meanDice Dice.meanDice timer
            -- copy count used instead of timer for organs with many copies
          (total, count) = organBenefit turnTimer grp cops fid factionD
      in delta $ total / fromIntegral count
           -- the same when created in me and in foe
           -- average over all matching grps; simplified: rarities ignored
    IK.CreateItem _ _ IK.TREASURE _ -> (100, 0)  -- assumed not temporary
    IK.CreateItem _ _ IK.COMMON_ITEM _ -> (70, 0)
    IK.CreateItem _ _ IK.CRAWL_ITEM _ -> (70, 0)
    IK.CreateItem _ _ IK.ANY_SCROLL _ -> (50, 0)
    IK.CreateItem _ _ IK.ANY_GLASS _ -> (50, 0)
    IK.CreateItem _ _ IK.ANY_POTION _ -> (50, 0)
    IK.CreateItem _ _ IK.EXPLOSIVE _ -> (50, 0)
    IK.CreateItem _ _ IK.ANY_JEWELRY _ -> (100, 0)
    IK.CreateItem _ _ grp _ ->  -- assumed not temporary and @grp@ tiny
      let (total, count) = recBenefit grp cops fid factionD
      in (total / fromIntegral count, 0)
    IK.DestroyItem{} -> delta (-10)  -- potentially harmful
    IK.ConsumeItems{} -> delta (-10)  -- potentially harmful
    IK.DropItem _ _ COrgan IK.CONDITION ->
      delta 30  -- save for curing own bad conditions
    IK.DropItem ngroup kcopy COrgan grp ->  -- assumed temporary
      -- Simplified: we assume actor has an average number of copies
      -- (and none have yet run out, e.g., prompt curing of poisoning)
      -- of a single kind of organ (and so @ngroup@ doesn't matter)
      -- of average benefit and that @kcopy@ is such that all copies
      -- are dropped. Separately we add bonuses for @ngroup@ and @kcopy@.
      -- Remaining time of the organ is arbitrarily assumed to be 20 turns.
      let turnTimer = 20
          (total, count) = organBenefit turnTimer grp cops fid factionD
          boundBonus n = if n == maxBound then 10 else 0
      in delta $ boundBonus ngroup + boundBonus kcopy
                 - total / fromIntegral count
                   -- the same when dropped from me and foe
    IK.DropItem{} -> delta (-10)  -- depends a lot on what is dropped
    IK.Discharge d -> if Dice.infDice d == 0
                      then (1, 0)  -- may fizzle, so AI never uses (could loop)
                      else delta $ 10 - Dice.meanDice d  -- context-dependent
    IK.PolyItem -> (1, 0)  -- may fizzle, so AI never uses (could loop)
    IK.RerollItem -> (1, 0)  -- may fizzle, so AI never uses (could loop)
    IK.DupItem -> (1, 0)  -- may fizzle, so AI never uses (could loop)
    IK.Identify -> (1, 0)  -- may fizzle, so AI never uses (could loop)
    IK.Detect IK.DetectAll radius -> (fromIntegral radius * 2, 0)
    IK.Detect IK.DetectLoot radius -> (fromIntegral radius * 2, 0)
    IK.Detect IK.DetectExit radius -> (fromIntegral radius / 2, 0)
    IK.Detect _ radius -> (fromIntegral radius, 0)
    IK.SendFlying _ -> (0, -100)  -- very context dependent, but lack of control
    IK.PushActor _ -> (0, -100)   -- is deadly on some maps, leading to harm;
    IK.PullActor _ -> (0, -100)   -- pushing others may crush them against wall
    IK.DropBestWeapon -> delta $ -50  -- often a whole turn wasted == InsertMove
    IK.ApplyPerfume -> delta 0  -- depends on smell sense of friends and foes
    IK.OneOf effs ->
      let bs = map (effectToBenefit cops fid factionD) effs
          f (self, foe) (accSelf, accFoe) = (self + accSelf, foe + accFoe)
          (effSelf, effFoe) = foldr f (0, 0) bs
      in (effSelf / fromIntegral (length bs), effFoe / fromIntegral (length bs))
    IK.OnSmash _ -> delta 0
      -- can be beneficial; we'd need to analyze explosions, range, etc.
    IK.OnCombine eff1 -> effectToBenefit cops fid factionD eff1
    IK.OnUser eff1 ->
      let (effSelf, effFoe) = effectToBenefit cops fid factionD eff1
      in (effSelf, - effFoe)
    IK.AndEffect eff1 _ -> effectToBenefit cops fid factionD eff1
      -- for simplicity; so in content make sure to place initial animations
      -- among normal effects, not at the start of composite effect
      -- (animations should not fail, after all), and start composite
      -- effect with the main thing
    IK.OrEffect eff1 _ -> effectToBenefit cops fid factionD eff1
    IK.SeqEffect effs -> effectToBenefits cops fid factionD effs
    IK.VerbNoLonger{} -> delta 0  -- flavour only, no benefit
    IK.VerbMsg{} -> delta 0  -- flavour only, no benefit
    IK.VerbMsgFail{} -> delta 0

effectToBenefits :: COps -> FactionId -> FactionDict -> [IK.Effect]
                 -> (Double, Double)
effectToBenefits cops fid factionD effs =
  let effPairs = map (effectToBenefit cops fid factionD) effs
      f (self, foe) (accSelf, accFoe) = (self + accSelf, foe + accFoe)
  in foldr f (0, 0) effPairs

-- See the comment for @Paralyze@.
averageTurnValue :: Double
averageTurnValue = 10

-- Average delay between desired item uses. Some items are best activated
-- every turn, e.g., healing (but still, on average, the activation would be
-- useless some of the time, namely when HP is at max, which is rare,
-- or when some combat boost is already lasting, which is probably also rare).
-- However, e.g., for detection, activating every few turns is enough.
-- Also, sometimes actor has many activable items, so he doesn't want to use
-- the less powerful ones as often as when they are alone.
-- For weapons, it depends. Sometimes a weapon with disorienting effect
-- should be used once every couple of turns and stronger raw damage
-- weapons all the remaining time. In other cases a single weapon
-- with a devastating effect would ideally be available each turn.
-- We don't want to undervalue rarely used items with long timeouts
-- and we think that most interesting gameplay comes from alternating
-- item use, so we arbitrarily set the full value timeout to 3.
avgItemDelay :: Double
avgItemDelay = 3

-- The average time between consumable item being found
-- (and enough skill obtained to use it) and the item
-- not being worth using any more. We specifically ignore
-- item not being used any more, because it is not durable and is consumed.
-- However we do consider actor mortality (especially common for spawners)
-- and item contending with many other very different but valuable items
-- that all vie for the same turn needed to activate them (especially common
-- for non-spawners). Another reason is item getting obsolete or duplicated,
-- by finding a strictly better item or an identical item.
-- The @avgItemLife@ constant only makes sense for items with non-periodic
-- effects, because the effects' benefit is not cumulated
-- by just placing them in equipment and they cost a turn to activate.
-- We set the value to 30, assuming if the actor finds an item, then he is
-- most likely at an unlooted level, so he will find more loot soon,
-- or he is in a battle, so he will die soon (or win even more loot).
avgItemLife :: Double
avgItemLife = 30

-- The value of durable item is this many times higher than non-durable,
-- because the item will on average be activated this many times
-- before it stops being used.
durabilityMult :: Double
durabilityMult = avgItemLife / avgItemDelay

-- We assume the organ is temporary (@Fragile@ and @Periodic@)
-- and also that it doesn't provide any functionality,
-- e.g., detection or raw damage. However, we take into account effects
-- knowing in some temporary organs, e.g., poison or regeneration,
-- they are triggered at each item copy destruction. They are applied to self,
-- hence we take the self component of valuation. We multiply by the count
-- of created/dropped organs, because for conditions it determines
-- how many times the effect is applied, before the last copy expires.
--
-- The temporary organs are not durable nor in infnite copies, so to give
-- continous benefit, organ has to be recreated each @turnTimer@ turns.
-- Creation takes a turn, so incurs @averageTurnValue@ cost.
-- That's how the lack of durability impacts their value, not via
-- @durabilityMult@, which however may be applied to organ creating item.
-- So, on average, maintaining the organ costs @averageTurnValue/turnTimer@.
-- So, if an item lasts @averageTurnValue@ and can be created at will, it's
-- almost as valuable as permanent. This makes sense even if the item creating
-- the organ is not durable, but the timer is huge. One may think the lack
-- of durability should be offset by the timer, but remember that average
-- item life @avgItemLife@ is rather low, so either a new item will be found
-- soon and so the long timer doesn't matter or the actor will die
-- or the gameplay context will change (e.g., out of battle) and so the effect
-- will no longer be useful.
--
-- When considering the effects, we just use their standard valuation,
-- despite them not using up actor's turn to be applied each turn,
-- because, similarly as for periodic items, we don't control when they
-- are applied and we can't stop/restart them.
--
-- We assume, only one of the timer and count mechanisms is present at once
-- (@count@ or @turnTimer@ is 1).
-- We assume no organ has effect that drops its group or creates its group;
-- otherwise we'd loop.
organBenefit :: Double -> GroupName ItemKind -> COps -> FactionId -> FactionDict
             -> (Double, Int)
organBenefit turnTimer grp cops@COps{coitem} fid factionD =
  let f (!sacc, !pacc) !p _ !kind =
        let count = Dice.meanDice (IK.icount kind)
            paspect asp =
              fromIntegral p
              * count * turnTimer
                -- the aspect stays for this many turns'
               * aspectToBenefit asp
            peffect eff =
              fromIntegral p
              * count
                -- this many consecutive effects will be generated, if any
              * fst (effectToBenefit cops fid factionD eff)
        in ( sacc + (sum (map paspect $ IK.iaspects kind)
                     + sum (map peffect $ IK.ieffects kind))
             - averageTurnValue  -- the cost of 1 turn spent acquiring the organ
                                 -- (or of inflexibility of periodic items)
           , pacc + p )
  in ofoldlGroup' coitem grp f (0, 0)

-- We assume no item has effect that drops its group or creates its group;
-- otherwise we'd loop.
recBenefit :: GroupName ItemKind -> COps -> FactionId -> FactionDict
           -> (Double, Int)
recBenefit grp cops@COps{coitem, coItemSpeedup} fid factionD =
  let f (!sacc, !pacc) !p !kindId !kind =
        let km = getKindMean kindId coItemSpeedup
            recPickup =
              benPickup $ totalUsefulness cops fid factionD
                                          (fakeItem kindId kind km)
        in ( sacc + Dice.meanDice (IK.icount kind) * recPickup
           , pacc + p )
  in ofoldlGroup' coitem grp f (0, 0)

fakeItem :: ContentId IK.ItemKind -> IK.ItemKind -> IA.KindMean -> ItemFull
fakeItem kindId kind km =
  let jkind    = IdentityObvious kindId
      jfid     = Nothing  -- the default
      jflavour = Flavour (toEnum 0) (toEnum 0) -- dummy
      itemBase = Item{..}
      itemDisco = ItemDiscoMean km
  in ItemFull itemBase kindId kind itemDisco True

-- The value of aspect bonus is supposed to be, roughly, the benefit
-- of having that bonus on actor for one turn (as if equipping didn't cost
-- any time). Comparing or adding this value later on to the benefit of one-time
-- applying the item makes sense, especially if the item is durable,
-- but even if not, as lont as I have many items relative to equipment slots.
-- If I have scarcity of items, the value should be higher, because if I apply
-- a non-durable item, it no longer benefits me, but if I wear it,
-- it can benefit me next turn also. The time cost of equipping balances this
-- to some extent, just as @durabilityMult@ and the equipment slot limit.
--
-- Value of aspects and effects is linked by some deep economic principles
-- which I'm unfortunately ignorant of. E.g., average weapon hits for 5HP,
-- so it's worth 50 per turn, so that should also be the worth per turn
-- of equpping a sword oil that doubles damage via @AddHurtMelee@.
-- Which almost matches up, since 100% effective oil is worth 100.
-- Perhaps oil is worth double (despite cap, etc.), because it's addictive
-- and raw weapon damage is not; so oil stays and old weapons get trashed.
-- However, using the weapon in combat costs 100 (the value of extra
-- battle turn). However, one turn per turn is almost free, because something
-- has to be done to move the time forward. If the oil required wasting a turn
-- to affect next strike, then we'd have two turns per turn, so the cost
-- would be real and 100% oil would not have any significant good or bad effect
-- any more, but 200% oil (if not for the cap) would still be worth it.
--
-- Anyway, that suggests that the current scaling of effect vs aspect values
-- is reasonable. What is even more important is consistency among aspects
-- so that, e.g., a shield or a torch is never equipped by AI, but oil lamp is.
-- Valuation of effects, and more precisely, more the signs than absolute
-- values, ensures that both shield and torch get auto-picked up so that
-- the human player can nevertheless equip them in very special cases.
aspectToBenefit :: IK.Aspect -> Double
aspectToBenefit asp =
  case asp of
    IK.Timeout{} -> 0
    IK.AddSkill Ability.SkMove p -> Dice.meanDice p * 5
    IK.AddSkill Ability.SkMelee p -> Dice.meanDice p * 5
    IK.AddSkill Ability.SkDisplace p -> Dice.meanDice p * 5
    IK.AddSkill Ability.SkAlter p -> Dice.meanDice p * 5
    IK.AddSkill Ability.SkWait p -> Dice.meanDice p * 5
    IK.AddSkill Ability.SkMoveItem p -> Dice.meanDice p * 5
    IK.AddSkill Ability.SkProject p -> Dice.meanDice p * 5
    IK.AddSkill Ability.SkApply p -> Dice.meanDice p * 5
    IK.AddSkill Ability.SkSwimming p -> Dice.meanDice p
    IK.AddSkill Ability.SkFlying p -> Dice.meanDice p
    IK.AddSkill Ability.SkHurtMelee p -> Dice.meanDice p  -- offence favoured
    IK.AddSkill Ability.SkArmorMelee p -> Dice.meanDice p / 4
                                              -- only partial protection
    IK.AddSkill Ability.SkArmorRanged p -> Dice.meanDice p / 8
    IK.AddSkill Ability.SkMaxHP p -> Dice.meanDice p
    IK.AddSkill Ability.SkMaxCalm p -> Dice.meanDice p / 5
    IK.AddSkill Ability.SkSpeed p -> Dice.meanDice p * 25
      -- 1 speed ~ 5% melee; times 5 for no caps, escape, pillar-dancing, etc.;
      -- OTOH, it's 1 extra turn each 20 turns, so 100/20, so 5; figures
    IK.AddSkill Ability.SkSight p -> Dice.meanDice p * 5
    IK.AddSkill Ability.SkSmell p -> Dice.meanDice p
    IK.AddSkill Ability.SkShine p -> Dice.meanDice p * 2
    IK.AddSkill Ability.SkNocto p -> Dice.meanDice p * 30
                                       -- > sight + light; stealth, slots
    IK.AddSkill Ability.SkHearing p -> Dice.meanDice p
    IK.AddSkill Ability.SkAggression _ -> 0  -- dunno
    IK.AddSkill Ability.SkOdor p -> - Dice.meanDice p  -- makes one trackable
    IK.SetFlag{} -> 0
    IK.ELabel{} -> 0
    IK.ToThrow{} -> 0  -- counted elsewhere
    IK.PresentAs{} -> 0
    IK.EqpSlot{} -> 0
    IK.Odds{} -> 0
      -- Should be already rolled; if not, can't tell easily.
      -- In particular, any timeouts there or @Periodic@ flags
      -- would be ignored, so they should be avoided under @Odds@
      -- in not fully-identified items, because they are so crucial
      -- for evaluation.

aspectRecordToBenefit :: IA.AspectRecord -> [Double]
aspectRecordToBenefit arItem =
  map aspectToBenefit $ IA.aspectRecordToList arItem

-- | Compute the whole 'Benefit' structure, containing various facets
-- of AI item preference, for an item with the given effects and aspects.
totalUsefulness :: COps -> FactionId -> FactionDict -> ItemFull -> Benefit
totalUsefulness cops fid factionD itemFull@ItemFull{itemKind, itemSuspect} =
  let arItem = aspectRecordFull itemFull
      -- If the item is periodic, we only add effects to equipment benefit,
      -- because we assume it's in equipment and then
      -- we can't effectively apply it, because it's never recharged,
      -- because it activates as soon as recharged.
      -- We ignore the rare case of a periodic item kept in backpack
      -- to be applied manually. AI is too silly to choose it and we
      -- certainly don't want AI to destroy periodic items out of silliness.
      -- We don't assign a special bonus or malus due to being periodic,
      -- because periodic items are bad in that one can't
      -- activate them at will and they take equipment space,
      -- and good in that one saves a turn, not having
      -- to manually activate them. Additionally, no weapon can be periodic,
      -- because damage would be applied to the fighter, so a large class
      -- of items with timeout is excluded from the consideration.
      -- Generally, periodic seems more helpful on items with low timeout
      -- and obviously beneficial effects, e.g., frequent periodic healing
      -- or nearby detection is better, but infrequent periodic teleportation
      -- or harmful outward explosion is worse. But the rule is not strict
      -- and also dependent on gameplay context of the moment,
      -- hence no numerical value.
      periodic = IA.checkFlag Ability.Periodic arItem
      -- Timeout between 0 and 1 means item usable each turn, so we consider
      -- it equivalent to a permanent item --- one without timeout restriction.
      -- Timeout 2 means two such items are needed to use the effect each turn,
      -- so a single such item may be worth half of the permanent value.
      -- E.g., when item heals 1 HP each turn, that's precisly the calculation.
      timeout = fromIntegral $ IA.aTimeout arItem
      scalePeriodic value = value / max 1 timeout
      -- With non-periodic items, when we need to expend a turn to apply the
      -- item or, e.g., we lose the opportunity to use another weapon if we hit
      -- with this one, the loss of value due to timeout is lower.
      -- Also, by the time cooldown recharges, one of combatants is often dead
      -- or fled, so some effects are no longer useful (but 1 HP gain is).
      -- To balance all that, we consider a square root of timeout
      -- and assume we need to spend turn on other actions at least every other
      -- turn (hence @max 2@). Note that this makes AI like powerful weapons
      -- with high timeout a bit more, though it still prefers low timeouts.
      timeoutSqrt = sqrt $ max 2 timeout
      scaleTimeout v = v / timeoutSqrt
      (effSelf, effFoe) =
        effectToBenefits cops fid factionD (IK.ieffects itemKind)
      -- Durability doesn't have any numerical impact on @eqpSum,
      -- because item is never consumed by just being stored in equipment.
      -- Also no numerical impact for flinging, because we can't fling it again
      -- in the same skirmish and also enemy can pick up and fling back.
      -- Only @benMeleeAvg@ and @benApply@ are affected, regardless if the item
      -- is in equipment or not. As summands of @benPickup@ they should be
      -- impacted by durability, because picking an item to be used
      -- only once is less advantageous than when the item is durable.
      -- For deciding which item to apply or melee with, they should be
      -- impacted, because it makes more sense to use an item that is durable
      -- and save the option for using non-durable item for the future, e.g.,
      -- when both items have timeouts, starting with durable is beneficial,
      -- because it recharges while the non-durable is prepared and used.
      durable = IA.checkFlag Ability.Durable arItem
      -- For applying, we add the self part only.
      benApply = max 0 $  -- because optional; I don't need to apply
        if periodic
        then 0  -- because always in eqp and so never recharged
        else scaleTimeout (effSelf + effDice)
               -- hits self with kintetic dice too, when applying
             / if durable then 1 else durabilityMult
      -- This assumes attacker hurt skill and enemy armor skill balance
      -- and so this value doesn't need to be recomputed at each equipment
      -- change and distributing weapons among AI actors doesn't need
      -- to match each weapon to each actor's equipment. However,
      -- a bad side-effect is that if an actor has terrible hurt skill,
      -- a weapon with high dice is still used by him before a burning weapon.
      -- Unless the opponent has even more terrible armor, unlikely,
      -- the chosen weapon is definitely not the best.
      effDice = - IK.damageUsefulness itemKind
      -- For melee, we add the foe part only.
      benMelee = if periodic
                 then 0  -- because never recharged, so never ready for melee
                 else effFoe + effDice  -- @AddHurtMelee@ already in @eqpSum@
      benMeleeAvg = scaleTimeout benMelee
                    / if durable then 1 else durabilityMult
      -- Experimenting is fun, but it's better to risk foes' skin than ours,
      -- so we only buff flinging, not applying, when item not identified.
      -- It's also more gameplay fun when enemies throw at us rather than
      -- when they use items on themselves.
      benFling = min benFlingRaw $ if itemSuspect then -10 else 0
      -- If periodic, we assume the item was in equipment, so effects
      -- were activated before flinging, so when projectile hits,
      -- it's discharged, so no kintetic damage value nor effect benefit
      -- is added to @benFling@.
      -- However, if item is not periodic, we assume the item was recharged,
      -- and so all the effects are activated at projectile impact,
      -- hence their full value is added to the kinetic damage value.
      benFlingRaw = min 0 $
        if periodic then 0 else effFoe + benFlingDice
      benFlingDice | IK.idamage itemKind == 0 = 0  -- speedup
                   | otherwise = assert (v <= 0) v
       where
        -- We assume victim completely unbuffed and not blocking. If not,
        -- let's hope the actor is similarly buffed to compensate.
        hurtMult = armorHurtCalculation True (IA.aSkills arItem)
                                             Ability.zeroSkills
        dmg = Dice.meanDice $ IK.idamage itemKind
        rawDeltaHP = ceiling $ fromIntegral hurtMult * xD dmg / 100
        -- For simplicity, we ignore range bonus/malus and @Lobable@.
        IK.ThrowMod{IK.throwVelocity} = IA.aToThrow arItem
        speed = speedFromWeight (IK.iweight itemKind) throwVelocity
        v = - fromIntegral (modifyDamageBySpeed rawDeltaHP speed) * 10 / xD 1
          -- 1 damage valued at 10, just as in @damageUsefulness@
      -- If item is periodic, we factor in the self value of effects,
      -- because they are applied to self, whether the actor wants it or not.
      -- We don't add a bonus of @averageTurnValue@ to the value of periodic
      -- effects, even though they save a turn, by being auto-applied,
      -- because on the flip side, player is not in control of the precise
      -- timing of their activation and also occasionally needs to spend a turn
      -- unequipping them to prevent activation. Note also that periodic
      -- activations don't consume the item, whether it's durable or not.
      aspectBenefits = aspectRecordToBenefit arItem
      eqpBens =
        sum $ aspectBenefits ++ [scalePeriodic (effSelf + effDice) | periodic]
      -- Equipped items may incur crippling maluses via aspects (but rather
      -- not via periodic effects). Examples of crippling maluses are zeroing
      -- melee or move skills. AI can't live with those and can't
      -- value those competently against any equally enormous bonuses
      -- the item might provide to compensate and so be even considered.
      cripplingDrawback = not (null aspectBenefits)
                          && minimum aspectBenefits < -20
      eqpSum = eqpBens - if cripplingDrawback then 100 else 0
      -- If a weapon heals enemy at impact, given choice, it won't be used
      -- for melee, but can be equipped anyway, for beneficial aspects.
      -- OTOH, cif it harms wearer too much, it won't be worn
      -- but still may be flung and so may be worth picking up.
      (benInEqp, benPickupRaw)
        | IA.checkFlag Ability.Meleeable arItem
            -- the flag probably known even if item not identified
          && (benMelee < 0 || itemSuspect)
          && eqpSum >= -20 =
          ( True  -- equip, melee crucial and only weapons in eqp can be used
          , eqpSum
            + maximum [benApply, - benMeleeAvg, 0] )  -- apply or melee or not
        | (IA.goesIntoEqp arItem
          || isJust (lookup IK.CONDITION $ IK.ifreq itemKind))
               -- hack to record benefit, to use it in calculations later on
          && (eqpSum > 0 || itemSuspect) =  -- weapon or other equippable
          ( True  -- equip; long time bonus usually outweighs fling or apply
          , eqpSum  -- possibly spent turn equipping, so reap the benefits
            + if durable
              then benApply  -- apply or not but don't fling
              else 0)  -- don't remove from equipment by using up
        | otherwise =
          (False, max benApply (- benFling))  -- apply or fling
      benPickupRaw2 = max benPickupRaw $ if itemSuspect then 10 else 0
      -- If periodic, pick up to deny to foes and sometimes to apply
      -- to activate the first effect only (easier than computing if the first
      -- effect is really beneficial, while all effects detrimental).
      benPickup = if periodic then max 1 benPickupRaw2 else benPickupRaw2
  in Benefit{..}
