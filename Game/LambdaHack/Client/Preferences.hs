-- | Actor preferences for targets and actions, based on actor attributes.
module Game.LambdaHack.Client.Preferences
  ( totalUsefulness
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , effectToBenefit
  , averageTurnValue, avgItemDelay, avgItemLife, durabilityMult
  , organBenefit, recBenefit, fakeItem, aspectToBenefit, recordToBenefit
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Flavour
import           Game.LambdaHack.Common.Frequency
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind

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
-- so in case when turn is not spent, e.g, periodic or temporary effects,
-- the difference in value is only slight.
effectToBenefit :: Kind.COps -> Faction -> IK.Effect -> (Double, Double)
effectToBenefit cops fact eff =
  let delta x = (x, x)
  in case eff of
    IK.ELabel _ -> delta 0
    IK.EqpSlot _ -> delta 0
    IK.Burn d -> delta $ -(min 1500 $ 15 * Dice.meanDice d)
      -- often splash damage, armor doesn't block (but HurtMelee doesn't boost)
    IK.Explode _ -> delta 1  -- depends on explosion, but usually good,
                             -- unless under OnSmash, but they are ignored
    IK.RefillHP p ->
      delta $ if p > 0
              then min 2000 (20 * fromIntegral p)
              else max (-1000) (10 * fromIntegral p)
        -- one HP healed is worth a bit more than one HP dealed to enemy,
        -- because if the actor survives, he may deal damage many times;
        -- however, AI is mostly for non-heroes that fight in suicidal crowds,
        -- so the two values are kept close enough to maintain berserk approach
    IK.RefillCalm p -> delta $ if p > 0
                               then min 9 (fromIntegral p)
                               else max (-9) (fromIntegral p)
    IK.Dominate -> (0, -300)  -- I obtained an actor with, say 10HP,
                              -- worth 200, and enemy lost him, another 100
    IK.Impress -> (5, -50)  -- usually has no effect on self, hence low value
    IK.Summon grp d ->  -- contrived by not taking into account alliances
                        -- and not checking if enemies also control that group
      let ben = Dice.meanDice d * 200  -- the new actor can have, say, 10HP
      in if grp `elem` fgroups (gplayer fact) then (ben, -ben) else (-ben, ben)
    IK.Ascend{} -> (-99, 99)  -- note the reversed values:
                              -- only change levels sensibly, in teams,
                              -- and don't remove enemy too far, he may be
                              -- easy to kill and may have loot
    IK.Escape{} -> (-9999, 9999)  -- even if can escape, loots first and then
                                  -- handles escape as a special case
    -- The following two are expensive, because they ofen activate
    -- while in melee, in which case each turn is worth x HP, where x
    -- is the average effective weapon damage in the game, which would
    -- be ~5. (Plus a huge risk factor for any non-spawner faction.)
    -- So, each turn in battle is worth ~100. And on average, in and out
    -- of battle, let's say each turn is worth ~10.
    IK.Paralyze d -> delta $ -20 * Dice.meanDice d  -- clips
    IK.InsertMove d -> delta $ 100 * Dice.meanDice d  -- turns
    IK.Teleport d -> if Dice.meanDice d <= 8
                     then (1, 0)     -- blink to shoot at foes
                     else (-9, -1)  -- for self, don't derail exploration
                                    -- for foes, fight with one less at a time
    IK.CreateItem COrgan "temporary condition" _ ->
      (1, -1)  -- varied, big bunch, but try to create it anyway
    IK.CreateItem COrgan grp timer ->  -- assumed temporary
      let turnTimer = case timer of
            IK.TimerNone -> averageTurnValue + 1  -- copy count used instead
            IK.TimerGameTurn n -> Dice.meanDice n
            IK.TimerActorTurn n -> Dice.meanDice n
          (total, count) = organBenefit turnTimer grp cops fact
      in delta $ total / fromIntegral count
           -- the same when created in me and in foe
           -- average over all matching grps; simplified: rarities ignored
    IK.CreateItem _ "treasure" _ -> (100, 0)  -- assumed not temporary
    IK.CreateItem _ "useful" _ -> (70, 0)
    IK.CreateItem _ "any scroll" _ -> (50, 0)
    IK.CreateItem _ "any vial" _ -> (50, 0)
    IK.CreateItem _ "potion" _ -> (50, 0)
    IK.CreateItem _ "flask" _ -> (50, 0)
    IK.CreateItem _ grp _ ->  -- assumed not temporary and @grp@ tiny
      let (total, count) = recBenefit grp cops fact
      in (total / fromIntegral count, 0)
    IK.DropItem _ _ COrgan "temporary condition" ->
      (1, -1)  -- varied, big bunch, but try to nullify it anyway
    IK.DropItem ngroup kcopy COrgan grp ->  -- assumed temporary
      -- Simplified: we assume actor has an average number of copies
      -- (and none have yet run out, e.g., prompt curing of poisoning)
      -- of a single kind of organ (and so @ngroup@ doesn't matter)
      -- of average benefit and that @kcopy@ is such that all copies
      -- are dropped. Separately we add bonuses for @ngroup@ and @kcopy@.
      -- Remaining time of the organ is arbitrarily assumed to be 20 turns.
      let turnTimer = 20
          (total, count) = organBenefit turnTimer grp cops fact
          boundBonus n = if n == maxBound then 10 else 0
      in delta $ boundBonus ngroup + boundBonus kcopy
                 - total / fromIntegral count
                   -- the same when dropped from me and foe
    IK.DropItem{} -> delta (-10)  -- depends a lot on what is dropped
    IK.PolyItem -> delta 0  -- AI can't estimate item desirability vs average
    IK.Identify -> delta 0  -- AI doesn't know how to use
    IK.Detect radius -> (fromIntegral radius * 2, 0)
    IK.DetectActor radius -> (fromIntegral radius, 0)
    IK.DetectItem radius -> (fromIntegral radius, 0)
    IK.DetectExit radius -> (fromIntegral radius, 0)
    IK.DetectHidden radius -> (fromIntegral radius, 0)
    IK.SendFlying _ -> (1, -10)  -- very context dependent, but it's better
    IK.PushActor _ -> (1, -10)   -- to be the one that decides and not the one
    IK.PullActor _ -> (1, -10)   -- that is interrupted in the middle of fleeing
    IK.DropBestWeapon -> delta $ -50  -- often a whole turn wasted == InsertMove
    IK.ActivateInv ' ' -> delta $ -200  -- brutal and deadly
    IK.ActivateInv _ -> delta $ -50  -- depends on the items
    IK.ApplyPerfume -> delta 0  -- depends on smell sense of friends and foes
    IK.OneOf efs ->
      let bs = map (effectToBenefit cops fact) efs
          f (self, foe) (accSelf, accFoe) = (self + accSelf, foe + accFoe)
          (effSelf, effFoe) = foldr f (0, 0) bs
      in (effSelf / fromIntegral (length bs), effFoe / fromIntegral (length bs))
    IK.OnSmash _ -> delta 0
      -- can be beneficial; we'd need to analyze explosions, range, etc.
    IK.Recharging _ -> delta 0  -- taken into account separately
    IK.Temporary _ -> delta 0  -- assumed for created organs only
    IK.Unique -> delta 0
    IK.Periodic -> delta 0  -- considered in totalUsefulness
    IK.Composite [] -> delta 0
    IK.Composite (eff1 : _) -> effectToBenefit cops fact eff1
      -- for simplicity; so in content make sure to place initial animations
      -- among normal effects, not at the start of composite effect
      -- (animations should not fail, after all), and start composite
      -- effect with the main thing

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

-- The average time between item being found (and enough skill obtained
-- to use it) and item not being used any more. We specifically ignore
-- item not being used any more, because it is not durable and is consumed.
-- However we do consider actor mortality (especially common for spawners)
-- and item contending with many other very different but valuable items
-- that all vie for the same turn needed to activate them (especially common
-- for non-spawners). Another reason is item getting obsolete or duplicated,
-- by finding a strictly better item or an identical item.
-- The @avgItemLife@ constant only makes sense for items with non-periodic
-- effects, because the effects' benefit is not cumulative
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

-- We assume the organ is temporary (@Temporary@, @Periodic@, @Timeout 0@)
-- and also that it doesn't provide any functionality, e.g., detection
-- or burning or raw damage. However, we take into account recharging
-- effects, knowing in some temporary organs, e.g., poison or regeneration,
-- they are triggered at each item copy destruction. They are applied to self,
-- hence we take the self component of valuation. We multiply by the count
-- of created/dropped organs, because for temporary effects it determines
-- how many times the effect is applied, before the last copy expires.
--
-- The organs are not durable nor in infnite copies, so to give
-- continous benefit, organ has to be recreated each @turnTimer@ turns.
-- Creation takes a turn, so incurs @averageTurnValue@ cost.
-- That's how the lack of durability impacts their value, not via
-- @durabilityMult@, which however may be applied to organ creating item.
-- So, on average, maintaining the organ costs @averageTurnValue/turnTimer@.
-- So, if an item lasts @averageTurnValue@ and it can be created at will,
-- it's as valuable as permanent. This makes sense even if the item creating
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
-- We assume, only one of timer and count mechanisms is present at once.
organBenefit :: Double -> GroupName ItemKind -> Kind.COps -> Faction
             -> (Double, Int)
organBenefit turnTimer grp cops@Kind.COps{coitem=Kind.Ops{ofoldlGroup'}} fact =
  let f (!sacc, !pacc) !p _ !kind =
        let paspect asp = fromIntegral p * aspectToBenefit asp
            peffect eff = fromIntegral p * fst (effectToBenefit cops fact eff)
        in ( sacc + Dice.meanDice (IK.icount kind)
                    * (sum (map paspect $ IK.iaspects kind)
                       + sum (map peffect $ stripRecharging $ IK.ieffects kind))
                  - averageTurnValue / turnTimer
           , pacc + p )
  in ofoldlGroup' grp f (0, 0)

recBenefit :: GroupName ItemKind -> Kind.COps -> Faction -> (Double, Int)
recBenefit grp cops@Kind.COps{coitem=Kind.Ops{ofoldlGroup'}} fact =
  let f (!sacc, !pacc) !p _ !kind =
        let recPickup = benPickup $
              totalUsefulness cops fact (IK.ieffects kind)
                                        (meanAspect kind) (fakeItem kind)
        in ( sacc + Dice.meanDice (IK.icount kind) * recPickup
           , pacc + p )
  in ofoldlGroup' grp f (0, 0)

fakeItem :: IK.ItemKind -> Item
fakeItem kind =
  let jkindIx  = toEnum (-1)  -- dummy
      jlid     = toEnum 0  -- dummy
      jfid     = Nothing  -- the default
      jsymbol  = IK.isymbol kind
      jname    = IK.iname kind
      jflavour = head stdFlav  -- dummy
      jfeature = IK.ifeature kind
      jweight  = IK.iweight kind
      jdamage  = fromJust $ mostFreq $ toFreq "fakeItem" $ IK.idamage kind
  in Item{..}

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
-- so that, e.g., a shield or a torch is neven equipped, but oil lamp is.
-- Valuation of effects, and more precisely, more the signs than absolute
-- values, ensures that both shield and torch get picked up so that
-- the (human) actor can nevertheless equip them in very special cases.
aspectToBenefit :: IK.Aspect -> Double
aspectToBenefit asp =
  case asp of
    IK.Timeout{} -> 0
    IK.AddHurtMelee p -> Dice.meanDice p  -- offence favoured
    IK.AddArmorMelee p -> Dice.meanDice p / 4  -- only partial protection
    IK.AddArmorRanged p -> Dice.meanDice p / 8
    IK.AddMaxHP p -> Dice.meanDice p
    IK.AddMaxCalm p -> Dice.meanDice p / 5
    IK.AddSpeed p -> Dice.meanDice p * 25
      -- 1 speed ~ 5% melee; times 5 for no caps, escape, pillar-dancing, etc.;
      -- also, it's 1 extra turn each 20 turns, so 100/20, so 5; figures
    IK.AddSight p -> Dice.meanDice p * 5
    IK.AddSmell p -> Dice.meanDice p
    IK.AddShine p -> Dice.meanDice p * 2
    IK.AddNocto p -> Dice.meanDice p * 10  -- > sight + light; stealth, slots
    IK.AddAggression{} -> 0
    IK.AddAbility _ p -> Dice.meanDice p * 5

recordToBenefit :: AspectRecord -> [Double]
recordToBenefit aspects = map aspectToBenefit $ aspectRecordToList aspects

-- | Compute the whole 'Benefit' structure, containing various facets
-- of AI item preference, for an item with the given effects and aspects.
--
-- Note: result has non-strict fields, so arguments are forced to avoid leaks.
-- When AI looks at items (including organs) more often, force the fields.
totalUsefulness :: Kind.COps -> Faction -> [IK.Effect] -> AspectRecord -> Item
                -> Benefit
totalUsefulness !cops !fact !effects !aspects !item =
  let effPairs = map (effectToBenefit cops fact) effects
      effDice = - damageUsefulness item
      f (self, foe) (accSelf, accFoe) = (self + accSelf, foe + accFoe)
      (effSelf, effFoe) = foldr f (0, 0) effPairs
      -- Timeout between 0 and 1 means item usable each turn, so we consider
      -- it equivalent to a permanent item --- without timeout restriction.
      -- Timeout 2 means two such items are needed to use the effect each turn,
      -- so a single such item may be worth half of the permanet value.
      -- Hence, we multiply item value by the proportion of the average desired
      -- delay between item uses @avgItemDelay@ and the actual timeout.
      timeout = aTimeout aspects
      (chargeSelf, chargeFoe) =
        let scaleChargeBens bens
              | timeout <= 3 = bens
              | otherwise = map (\eff ->
                  if avgItemDelay >= fromIntegral timeout
                  then eff
                  else eff * avgItemDelay / fromIntegral timeout) bens
            (cself, cfoe) = unzip $ map (effectToBenefit cops fact)
                                        (stripRecharging effects)
        in (scaleChargeBens cself, scaleChargeBens cfoe)
      -- If the item is periodic, we add charging effects to equipment benefit,
      -- but we don't assign periodic bonus or malus, because periodic items
      -- are bad in that one can't activate them at will and they take
      -- equipment space, and good in that one saves a turn, not having
      -- to manually activate them. Additionally, no weapon can be periodic,
      -- because damage would be applied to the fighter, so a large class
      -- of items with timeout is excluded from the consideration.
      -- Generally, periodic seems more helpful on items with low timeout
      -- and obviously beneficial effects, e.g., frequent periodic healing
      -- or nearby detection is better, but infrequent periodic teleportation
      -- or harmful explosion is worse. But the rule is not strict and also
      -- dependent on gameplay context of the moment, hence no numerical value.
      periodic = IK.Periodic `elem` effects
      -- Durability doesn't have any numerical impact to @eqpSum,
      -- because item is never consumed by just being stored in equipment.
      -- Also no numerical impact for flinging, because we can't fling it again
      -- in the same skirmish and also enemy can pick up and fling back.
      -- Only @benMelee@ and @benApply@ are affected, regardless if the item
      -- is in equipment or not. As summands of @benPickup@ they should be
      -- impacted by durability, because picking an item to be used
      -- only once is less advantageous than when the item is durable.
      -- For deciding which item to apply or melee with, they should be
      -- impacted, because it makes more sense to use an item that is durable
      -- and save the option for using non-durable item for the future, e.g.,
      -- when both items have timeouts, starting with durable is beneficial,
      -- because it recharges while the non-durable is prepared and used.
      durable = IK.Durable `elem` jfeature item
      -- If recharging effects not periodic, we add the self part,
      -- because they are applied to self. If they are periodic we can't
      -- effectively apply them, becasue they are never recharged,
      -- because they activate as soon as recharged.
      benApply = max 0 $  -- because optional; I don't need to apply
        (effSelf + effDice  -- hits self with dice too, when applying
         + if periodic then 0 else sum chargeSelf)
        / if durable then 1 else durabilityMult
      -- For melee, we add the foe part.
      benMelee = min 0 $
        (effFoe + effDice  -- @AddHurtMelee@ already in @eqpSum@
         + if periodic then 0 else sum chargeFoe)
        / if durable then 1 else durabilityMult
      -- The periodic effects, if any, are activated when projectile flies,
      -- but not when it hits, so they are not added to @benFling@.
      -- However, if item is not periodic, the recharging effects
      -- are activated at projectile impact, hence their value is added.
      benFling = min 0 $
        effFoe + benFlingDice -- nothing in @eqpSum@; normally not worn
        + if periodic then 0 else sum chargeFoe
      benFlingDice | jdamage item == 0 = 0  -- speedup
                   | otherwise = assert (v <= 0) v
       where
        hurtMult = 100 + min 99 (max (-99) (aHurtMelee aspects))
          -- assumes no enemy armor and no block
        dmg = Dice.meanDice (jdamage item)
        rawDeltaHP = ceiling $ fromIntegral hurtMult * xD dmg / 100
        -- For simplicity, we ignore range bonus/malus and @Lobable@.
        IK.ThrowMod{IK.throwVelocity} = strengthToThrow item
        speed = speedFromWeight (jweight item) throwVelocity
        v = - fromIntegral (modifyDamageBySpeed rawDeltaHP speed) * 10 / xD 1
          -- 1 damage valued at 10, just as in @damageUsefulness@
      -- For equipment benefit, we take into account only the self
      -- value of the recharging effects, because they applied to self.
      -- We don't add a bonus @averageTurnValue@ to the value of periodic
      -- effects, even though they save a turn, by being auto-applied,
      -- because on the flip side, player is not in control of the precise
      -- timing of their activation and also occasionally needs to spend a turn
      -- unequipping them to prevent activation. Note also that periodic
      -- activations don't consume the item, whether it's durable or not.
      eqpBens = recordToBenefit aspects
                ++ if periodic then chargeSelf else []
      sumBens = sum eqpBens
      -- Equipped items may incur crippling maluses via aspects and periodic
      -- effects. Examples of crippling maluses are, e.g., such that make melee
      -- impossible or moving impossible. AI can't live with those and can't
      -- value those competently against bonuses the item provides.
      cripplingDrawback = not (null eqpBens) && minimum eqpBens < -20
      eqpSum = sumBens - if cripplingDrawback then 100 else 0
      -- If a weapon heals enemy at impact, it won't be used for melee
      -- (but can be equipped anyway). If it harms wearer too much,
      -- won't be worn but still may be flung, etc.
      (benInEqp, benPickup)
        | isMelee item && benMelee < 0 && eqpSum >= -20 =
          ( True  -- equip, melee crucial, and only weapons in eqp can be used
          , if durable
            then eqpSum
                 + max benApply (- benMelee)  -- apply or melee or not
            else - benMelee)  -- melee is predominant
        | goesIntoEqp item && eqpSum > 0 =  -- weapon or other equippable
          ( True  -- equip; long time bonus usually outweighs fling or apply
          , eqpSum  -- possibly spent turn equipping, so reap the benefits
            + if durable
              then benApply  -- apply or not but don't fling
              else 0)  -- don't remove from equipment by using up
        | otherwise =
          (False, max benApply (- benFling))  -- apply or fling
  in Benefit{..}
