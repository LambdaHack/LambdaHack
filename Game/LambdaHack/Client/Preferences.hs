-- | Actor preferences for targets and actions based on actor attributes.
module Game.LambdaHack.Client.Preferences
  ( totalUsefulness
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , effectToBenefit, organBenefit, aspectToBenefit, recordToBenefit
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind

-- | How much AI benefits from applying the effect. Multipllied by item p.
-- Negative means harm to the enemy when thrown at him. Effects with zero
-- benefit won't ever be used, neither actively nor passively.
-- The first component is benefit when applied to self, the second
-- is benefit (preferably negative) when applied to enemy.
effectToBenefit :: Kind.COps -> Faction -> IK.Effect -> (Int, Int)
effectToBenefit cops fact eff =
  let delta x = (x, x)
  in case eff of
    IK.ELabel _ -> delta 0
    IK.EqpSlot _ -> delta 0
    IK.Burn d -> delta $ -(min 1500 $ 15 * Dice.meanDice d)
      -- often splash damage, armor doesn't block (but HurtMelee doesn't boost)
    IK.Explode _ -> delta 0  -- depends on explosion
    IK.RefillHP p ->
      delta $ if p > 0 then min 2000 (20 * p) else max (-1000) (10 * p)
        -- one HP healed is worth a bit more than one HP dealed to enemy,
        -- because if the actor survives, he may deal damage many times;
        -- however, AI is mostly for non-heroes that fight in suicidal crowds,
        -- so the two values are kept close enough to maintain berserk approach
    IK.RefillCalm p -> delta $ if p > 0 then min 9 p else max (-9) p
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
    IK.Paralyze d -> delta $ -10 * Dice.meanDice d  -- clips
    IK.InsertMove d -> delta $ 50 * Dice.meanDice d  -- turns
    IK.Teleport d -> if Dice.meanDice d <= 8
                     then (1, 0)   -- blink to shoot at foes
                     else (-9, -1)  -- for self, don't derail exploration
                                    -- for foes, fight with one less at a time
    IK.CreateItem COrgan grp _ ->
      let (total, count) = organBenefit grp cops fact
      in delta $ total `divUp` count  -- the same when created in me and in foe
        -- average over all matching grp; simplified: rarities ignored
    IK.CreateItem{} -> (50, 0)
    IK.DropItem _ _ COrgan grp ->
      -- Contrived: we assume actor has an average number of copies
      -- of one kind of organ of average benefit and all are dropped.
      let (total, count) = organBenefit grp cops fact
      in delta $ - total `divUp` count  -- the same when dropped from me and foe
    IK.DropItem _ _ _ _ -> delta (-10)  -- depends a lot on what is dropped
    IK.PolyItem -> delta 0  -- AI can't estimate item desirability vs average
    IK.Identify -> delta 0  -- AI doesn't know how to use
    IK.Detect radius -> (radius * 2, 0)
    IK.DetectActor radius -> (radius, 0)
    IK.DetectItem radius -> (radius, 0)
    IK.DetectExit radius -> (radius, 0)
    IK.DetectHidden radius -> (radius, 0)
    IK.SendFlying _ -> (1, -10)  -- very context dependent, but it's better
    IK.PushActor _ -> (1, -10)   -- to be the one that decides and not the one
    IK.PullActor _ -> (1, -10)   -- that is interrupted in the middle of fleeing
    IK.DropBestWeapon -> delta $ -50  -- often a whole turn wasted == InsertMove
    IK.ActivateInv ' ' -> delta $ -200  -- brutal and deadly
    IK.ActivateInv _ -> delta $ -50  -- depends on the items
    IK.ApplyPerfume -> delta 0  -- depends on smell sense of friends and foes
    IK.OneOf efs ->
      let bs = map (effectToBenefit cops fact) efs
      in if any (\(friend, foe) -> friend < -10 || foe > 10) bs
         then delta 0  -- mixed blessing; can't tell how bad/good this is
         else let f (friend, foe) (accFriend, accFoe) =
                    (friend + accFriend, foe + accFoe)
                  (allFriend, allFoe) = foldr f (0, 0) bs
              in (allFriend `divUp` length bs, allFoe `divUp` length bs)
    IK.OnSmash _ -> delta 0
      -- can be beneficial; we'd need to analyze explosions, range, etc.
    IK.Recharging _ -> delta 0  -- taken into account separately
    IK.Temporary _ -> delta 0  -- assumed for created organs only
    IK.Unique -> delta 0
    IK.Periodic -> delta 0  -- considered in totalUsefulness

-- We assume the organ is temporary and quasi-periodic with timeout 0
-- and also that it doesn't provide any functionality, e.g., detection
-- or burning or raw damage. However, we take into account recharging
-- effects, knowing in some temporary organs, e.g., poison or regeneration,
-- they are triggered at each item copy destruction. They are applied to self,
-- hence we take the friendly component of valuation.
organBenefit :: GroupName ItemKind -> Kind.COps -> Faction -> (Int, Int)
organBenefit t cops@Kind.COps{coitem=Kind.Ops{ofoldlGroup'}} fact =
  let f (!sacc, !pacc) !p _ !kind =
        let paspect asp = p * aspectToBenefit asp
            peffect eff = p * fst (effectToBenefit cops fact eff)
        in ( sacc + sum (map paspect $ IK.iaspects kind)
                  + sum (map peffect $ stripRecharging $ IK.ieffects kind)
           , pacc + p )
  in ofoldlGroup' t f (0, 0)

-- | Return the value to add to effect value.
aspectToBenefit :: IK.Aspect -> Int
aspectToBenefit asp =
  case asp of
    IK.Timeout{} -> 0
    IK.AddHurtMelee p -> Dice.meanDice p
    IK.AddArmorMelee p -> Dice.meanDice p `divUp` 4
    IK.AddArmorRanged p -> Dice.meanDice p `divUp` 8
    IK.AddMaxHP p -> Dice.meanDice p
    IK.AddMaxCalm p -> Dice.meanDice p `divUp` 5
    IK.AddSpeed p -> Dice.meanDice p * 50
      -- 1 speed ~ 5% melee; times 10 for no caps, escape, pillar-dancing, etc.
    IK.AddSight p -> Dice.meanDice p * 5
    IK.AddSmell p -> Dice.meanDice p
    IK.AddShine p -> Dice.meanDice p * 2
    IK.AddNocto p -> Dice.meanDice p * 10  -- > sight + light; stealth, slots
    IK.AddAggression{} -> 0
    IK.AddAbility _ p -> Dice.meanDice p * 5

recordToBenefit :: AspectRecord -> [Int]
recordToBenefit aspects = map aspectToBenefit $ aspectRecordToList aspects

-- Result has non-strict fields, so arguments are forced to avoid leaks.
-- When AI looks at items (including organs) more often, force the fields.
totalUsefulness :: Kind.COps -> Faction -> [IK.Effect] -> AspectRecord -> Item
                -> Benefit
totalUsefulness !cops !fact !effects !aspects !item =
  let effPairs = map (effectToBenefit cops fact) effects
      effDice = - damageUsefulness item
      f (friend, foe) (accFriend, accFoe) = (friend + accFriend, foe + accFoe)
      (effFriend, effFoe) = foldr f (0, 0) effPairs
      -- Timeout between 0 and 1 means item usable each turn, so we consider
      -- it equivalent to a permanent item --- without timeout restriction.
      -- Timeout 2 means two such items are needed to use the effect each turn,
      -- so a single such item may be worth half of the permanet value.
      -- However, e.g., for detection, activating every few turns is enough.
      -- For weapons, it depends. Sometimes a weapon with disorienting effect
      -- should be used once every couple of turns and stronger raw damage
      -- weapons all the remaining time. In other cases a single weapon
      -- with a devastating effect would ideally be available each turn.
      -- We don't want to undervalue rarely used items with long timeouts
      -- and we think that most interesting gameplay comes from alternating
      -- item use, so we arbitrarily set the full value timeout to 3.
      timeout = aTimeout aspects
      (chargeFriend, chargeFoe) =
        let scaleChargeBens bens
              | timeout <= 3 = bens
              | otherwise = map (\eff -> min eff (eff * 3 `divUp` timeout)) bens
            (cfriend, cfoe) = unzip $ map (effectToBenefit cops fact)
                                          (stripRecharging effects)
        in (scaleChargeBens cfriend, scaleChargeBens cfoe)
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
      -- If recharging effects not periodic, we add the friend part,
      -- because they are applied to self. If they are periodic we can't
      -- effectively apply them, becasue they are never recharged,
      -- because they activate as soon as recharged.
      benApply = effFriend + effDice  -- hits self with dice too, when applying
                 + if periodic then 0 else sum chargeFriend
      -- For melee, we add the foe part.
      benMelee = effFoe + effDice  -- @AddHurtMelee@ already in @eqpSum@
                 + if periodic then 0 else sum chargeFoe
      -- The periodic effects, if any, are activated when projectile flies,
      -- but not when it hits, so they are not added to @benFling@.
      -- However, if item is not periodic, the recharging effects
      -- are activated at projectile impact, hence their value is added.
      benFling = effFoe + benFlingDice -- nothing in @eqpSum@; normally not worn
                 + if periodic then 0 else sum chargeFoe
      benFlingDice | jdamage item <= 0 = 0  -- speedup
                   | otherwise = min 0 $
        let hurtMult = 100 + min 99 (max (-99) (aHurtMelee aspects))
              -- assumes no enemy armor and no block
            dmg = Dice.meanDice (jdamage item)
            rawDeltaHP = fromIntegral hurtMult * xM dmg `divUp` 100
            -- For simplicity, we ignore range bonus/malus and @Lobable@.
            IK.ThrowMod{IK.throwVelocity} = strengthToThrow item
            speed = speedFromWeight (jweight item) throwVelocity
        in fromEnum $ - modifyDamageBySpeed rawDeltaHP speed * 10 `div` oneM
             -- 1 damage valued at 10, just as in @damageUsefulness@
      -- For equipment benefit, we take into account only the friendly
      -- value of the recharging effects, because they applied to self.
      eqpBens = recordToBenefit aspects
                ++ if periodic then chargeFriend else []
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
          ( True
          , eqpSum                                -- equip
            + max 0 (max benApply (- benMelee)))  -- and apply or melee or none
        | goesIntoEqp item && eqpSum > 0 =  -- weapon or other equippable
          ( True
          , eqpSum             -- equip
            + max 0 benApply)  -- and apply or not but don't fling (no unequip)
        | otherwise =
          (False, max 0 (max benApply (- benFling)))  -- apply or fling
  in Benefit{..}
