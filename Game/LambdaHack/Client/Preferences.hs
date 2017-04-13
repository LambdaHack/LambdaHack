-- | Actor preferences for targets and actions based on actor attributes.
module Game.LambdaHack.Client.Preferences
  ( totalUsefulness, totalUse, effectToBenefit
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
effectToBenefit :: Kind.COps -> Faction -> IK.Effect -> Int
effectToBenefit cops fact eff =
  case eff of
    IK.ELabel _ -> 0
    IK.EqpSlot _ -> 0
    IK.Burn d -> -(min 200 $ 15 * Dice.meanDice d)
                   -- often splash damage, etc.
    IK.Explode _ -> 0  -- depends on explosion
    IK.RefillHP p -> if p > 0 then min 99 (10 * p) else max (-99) (10 * p)
    IK.RefillCalm p -> if p > 0 then min 9 p else max (-9) p
    IK.Dominate -> -200
    IK.Impress -> -10
    IK.Summon grp d ->  -- contrived by not taking into account alliances
                        -- and not checking if enemies also control that group
      if grp `elem` fgroups (gplayer fact) then Dice.meanDice d * 50 else 0
    IK.Ascend{} -> 1      -- low, to only change levels sensibly, in teams
    IK.Escape{} -> 10000  -- AI wants to win; spawners to guard
    IK.Paralyze d -> -10 * Dice.meanDice d
    IK.InsertMove d -> 50 * Dice.meanDice d
    IK.Teleport d ->
      let p = Dice.meanDice d
      in if p <= 8  -- blink to shoot at foe
         then 1
         else -p  -- get rid of the foe
    IK.CreateItem COrgan grp _ ->
      let (total, count) = organBenefit grp cops fact
      in total `divUp` count  -- average over all matching grp; rarities ignored
    IK.CreateItem{} -> 30
    IK.DropItem _ _ COrgan grp ->
      let (total, _) = organBenefit grp cops fact
      in - total  -- sum over all matching grp; simplification: rarities ignored
    IK.DropItem _ _ _ _ -> -15
    IK.PolyItem -> 0  -- AI can't estimate item desirability vs average
    IK.Identify -> 0  -- AI doesn't know how to use
    IK.Detect radius -> radius * 2
    IK.DetectActor radius -> radius
    IK.DetectItem radius -> radius
    IK.DetectExit radius -> radius
    IK.DetectHidden radius -> radius
    IK.SendFlying _ -> -10  -- but useful on self sometimes, too
    IK.PushActor _ -> -10  -- but useful on self sometimes, too
    IK.PullActor _ -> -10
    IK.DropBestWeapon -> -50
    IK.ActivateInv ' ' -> -100
    IK.ActivateInv _ -> -50
    IK.ApplyPerfume -> 0  -- depends on the smell sense of friends and foes
    IK.OneOf efs -> let bs = map (effectToBenefit cops fact) efs
                    in if any (< -10) bs
                       then 0  -- mixed blessing
                       else sum bs `divUp` length bs
    IK.OnSmash _ -> 0  -- can be beneficial; we'd need to analyze explosions
    IK.Recharging e -> effectToBenefit cops fact e  -- for weapons
    IK.Temporary _ -> 0
    IK.Unique -> 0
    IK.Periodic -> 0

-- We ignore the possibility of, e.g., temporary extra clawed limb, etc.,
-- so we don't take into account idamage of the item kinds.
organBenefit :: GroupName ItemKind -> Kind.COps -> Faction -> (Int, Int)
organBenefit t cops@Kind.COps{coitem=Kind.Ops{ofoldlGroup'}} fact =
  let f (!sacc, !pacc) !p _ !kind =
        let paspect asp = p * aspectToBenefit cops asp
            peffect eff = p * effectToBenefit cops fact eff
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

-- | Determine the total benefit from having an item in eqp or inv,
-- according to item type, and also the benefit conferred by equipping the item
-- and from meleeing with it or applying it or throwing it.
totalUsefulness :: Kind.COps -> Faction -> ItemFull -> Maybe (Int, Int)
totalUsefulness cops fact itemFull = case itemDisco itemFull of
  Just ItemDisco{itemAspect=Just aspectRecord, itemKind=IK.ItemKind{ieffects}} ->
    Just $ totalUse cops fact ieffects aspectRecord (itemBase itemFull)
  Just ItemDisco{itemAspectMean, itemKind=IK.ItemKind{ieffects}} ->
    Just $ totalUse cops fact ieffects itemAspectMean (itemBase itemFull)
  _ -> Nothing

totalUse :: Kind.COps -> Faction -> [IK.Effect] -> AspectRecord -> Item
         -> (Int, Int)
totalUse cops fact effects aspects item =
  let effSum = -(min 150 (10 * Dice.meanDice (jdamage item)))
               + sum (map (effectToBenefit cops fact) effects)
      aspBens = map (aspectToBenefit cops) $ aspectRecordToList aspects
      periodicEffBens = map (effectToBenefit cops fact)
                            (stripRecharging effects)
      timeout = aTimeout aspects
      periodicBens | timeout == 0 = []
                   | otherwise =
        map (\eff -> eff * 10 `divUp` timeout) periodicEffBens
      selfBens = aspBens ++ periodicBens
      selfSum = sum selfBens
      mixedBlessing =
        not (null selfBens)
        && (selfSum > 0 && minimum selfBens < -10
            || selfSum < 0 && maximum selfBens > 10)
      isWeapon = isMelee item
      totalSum
        | isWeapon && effSum < 0 = - effSum + selfSum
        | not $ goesIntoEqp item = effSum
        | mixedBlessing =
            0  -- significant mixed blessings out of AI control
        | otherwise = selfSum  -- if the weapon heals the enemy, it
                               -- won't be used but can be equipped
  in (totalSum, effSum)
