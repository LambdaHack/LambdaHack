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
    IK.Burn d -> delta $ -(min 200 $ 15 * Dice.meanDice d)
                   -- often splash damage, armor doesn't block, etc.
    IK.Explode _ -> delta 0  -- depends on explosion
    IK.RefillHP p ->
      delta $ if p > 0 then min 99 (10 * p) else max (-99) (10 * p)
    IK.RefillCalm p -> delta $ if p > 0 then min 9 p else max (-9) p
    IK.Dominate -> delta $ -200
    IK.Impress -> (5, -10)
    IK.Summon grp d ->  -- contrived by not taking into account alliances
                        -- and not checking if enemies also control that group
      let ben = Dice.meanDice d * 200
      in if grp `elem` fgroups (gplayer fact) then (ben, -ben) else (-ben, ben)
    IK.Ascend{} -> (1, 0)      -- low, to only change levels sensibly, in teams
    IK.Escape{} -> (10000, 0)  -- AI wants to win; spawners to guard
    IK.Paralyze d -> delta $ -10 * Dice.meanDice d
    IK.InsertMove d -> delta $ 50 * Dice.meanDice d
    IK.Teleport d -> if Dice.meanDice d <= 8
                     then (1, 0) -- blink to shoot at foes
                     else (0, -1)  -- get rid of the foe
    IK.CreateItem COrgan grp _ ->
      let (total, count) = organBenefit grp cops fact
      in delta $ total `divUp` count  -- the same when created in me and in foe
        -- average over all matching grp; simplified: rarities ignored
    IK.CreateItem{} -> (30, 0)
    IK.DropItem _ _ COrgan grp ->
      -- Contrived: we assume actor has an average number of copies
      -- of one kind of organ of average benefit.
      let (total, count) = organBenefit grp cops fact
      in delta $ - total `divUp` count
           -- the same when dropped from me and from foe
    IK.DropItem _ _ _ _ -> delta $ -15
    IK.PolyItem -> delta $ 0  -- AI can't estimate item desirability vs average
    IK.Identify -> delta $ 0  -- AI doesn't know how to use
    IK.Detect radius -> (radius * 2, 0)
    IK.DetectActor radius -> (radius, 0)
    IK.DetectItem radius -> (radius, 0)
    IK.DetectExit radius -> (radius, 0)
    IK.DetectHidden radius -> (radius, 0)
    IK.SendFlying _ -> (1, -10)
    IK.PushActor _ -> (1, -10)
    IK.PullActor _ -> (1, -10)
    IK.DropBestWeapon -> delta $ -50
    IK.ActivateInv ' ' -> delta $ -100
    IK.ActivateInv _ -> delta $ -50
    IK.ApplyPerfume -> delta $ 0  -- depends on smell sense of friends and foes
    IK.OneOf efs ->
      let bs = map (effectToBenefit cops fact) efs
      in if any (\(friend, foe) -> friend < -10 || foe > 10) bs
         then delta 0  -- mixed blessing
         else let f (friend, foe) (accFriend, accFoe) =
                    (friend + accFriend, foe + accFoe)
                  (allFriend, allFoe) = foldr f (0, 0) bs
              in (allFriend `divUp` length bs, allFoe `divUp` length bs)
    IK.OnSmash _ -> delta 0
      -- can be beneficial; we'd need to analyze explosions
    IK.Recharging e -> effectToBenefit cops fact e
      -- simplified: no deduction for the need to recharge
    IK.Temporary _ -> delta 0
    IK.Unique -> delta 0
    IK.Periodic -> delta 0

-- We ignore the possibility of, e.g., temporary extra clawed limb, etc.,
-- so we don't take into account idamage of the item kinds.
organBenefit :: GroupName ItemKind -> Kind.COps -> Faction -> (Int, Int)
organBenefit t cops@Kind.COps{coitem=Kind.Ops{ofoldlGroup'}} fact =
  let f (!sacc, !pacc) !p _ !kind =
        let paspect asp = p * aspectToBenefit cops asp
            peffect eff = p * fst (effectToBenefit cops fact eff)
              -- only the effect on the owner of the organ matters
        in ( sacc + sum (map paspect $ IK.iaspects kind)
                  + sum (map peffect $ IK.ieffects kind)
           , pacc + p )
  in ofoldlGroup' t f (0, 0)

-- | Return the value to add to effect value.
aspectToBenefit :: Kind.COps -> IK.Aspect -> Int
aspectToBenefit _cops asp =
  case asp of
    IK.Timeout{} -> 0
    IK.AddHurtMelee p -> Dice.meanDice p
    IK.AddArmorMelee p -> Dice.meanDice p `divUp` 5
    IK.AddArmorRanged p -> Dice.meanDice p `divUp` 10
    IK.AddMaxHP p -> Dice.meanDice p
    IK.AddMaxCalm p -> Dice.meanDice p `div` 5
    IK.AddSpeed p -> Dice.meanDice p * 10000
    IK.AddSight p -> Dice.meanDice p * 10
    IK.AddSmell p -> Dice.meanDice p * 10
    IK.AddShine p -> Dice.meanDice p
    IK.AddNocto p -> Dice.meanDice p * 50
    IK.AddAggression{} -> 0
    IK.AddAbility _ p -> Dice.meanDice p * 5

recordToBenefit :: Kind.COps -> AspectRecord -> [Int]
recordToBenefit cops aspects =
  map (aspectToBenefit cops) $ aspectRecordToList aspects

-- | Determine
-- 1. the total benefit from picking an item up (to use or to put in equipment)
-- 2. the benefit of applied the item to self
-- 3. the (usually negative) benefit of flinging an item at an opponent
--    or meleeing with it
--
-- Result has non-strict fields, so arguments are forced to avoid leaks.
-- When AI looks at items (including organs) more often, force the fields.
totalUsefulness :: Kind.COps -> Faction -> [IK.Effect] -> AspectRecord -> Item
                -> (Int, (Int, Int))
totalUsefulness !cops !fact !effects !aspects !item =
  let effPairs = map (effectToBenefit cops fact) effects
      effDice = -(min 150 (10 * Dice.meanDice (jdamage item)))
      f (friend, foe) (accFriend, accFoe) = (friend + accFriend, foe + accFoe)
      (effFriendEf, effFoeEf) = foldr f (0, 0) effPairs
      (effFriend, effFoe) = (effFriendEf + effDice, effFoeEf + effDice)
      aspBens = recordToBenefit cops aspects
      periodicEffBens = map (fst . effectToBenefit cops fact)
                            (stripRecharging effects)
      timeout = aTimeout aspects
      periodicBens | timeout == 0 = []
                   | otherwise =
        map (\eff -> eff * 10 `divUp` timeout) periodicEffBens
      eqpBens = aspBens ++ periodicBens
      sumBens = sum eqpBens
      -- We don't take into account mixed blessing from effects
      -- but they are rare. However, permanent mixed blessings from aspects
      -- are more often obviously bad, unlike periodic or on-use (effect) ones.
      mixedBlessing =
        not (null eqpBens)
        && (sumBens > 0 && minimum eqpBens < -10
            || sumBens < 0 && maximum eqpBens > 10)
      eqpSum = if mixedBlessing
               then 0  -- significant mixed blessings out of AI control
               else sumBens
      isWeapon = isMelee item
      totalSum
        | isWeapon && effFoe < 0 =  -- if the weapon heals the enemy, it
                                    -- won't be used (but can be equipped)
          eqpSum                      -- equip
          + max effFriend (- effFoe)  -- and apply or melee with
        | not $ goesIntoEqp item = max effFriend (- effFoe)  -- apply or fling
        | otherwise = effFriend + eqpSum  -- no fling and equip at the same time
            -- healing weapon gets positive value here
  in (totalSum, (effFriend, effFoe))
