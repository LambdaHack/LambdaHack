-- | Actor preferences for targets and actions based on actor attributes.
module Game.LambdaHack.Client.AI.Preferences
  ( totalUsefulness, effectToBenefit
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
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
effectToBenefit :: Kind.COps -> Actor -> AspectRecord -> Faction -> IK.Effect
                -> Int
effectToBenefit cops b ar@AspectRecord{..} fact eff =
  case eff of
    IK.ELabel _ -> 0
    IK.EqpSlot _ -> 0
    IK.Burn d -> -(min 200 $ 15 * Dice.meanDice d)
                   -- often splash damage, etc.
    IK.Explode _ -> 0  -- depends on explosion
    IK.RefillHP p ->
         if p > 0
         then 10 * min p (max 0 $ fromEnum $ (xM aMaxHP - bhp b) `divUp` oneM)
         else max (-99) (11 * p)
    IK.OverfillHP p ->
         if p > 0
         then 11 * min p (max 1 $ fromEnum $ (xM aMaxHP - bhp b) `divUp` oneM)
         else max (-99) (11 * p)
    IK.RefillCalm p ->
         if p > 0
         then min p (max 0 $ fromEnum $ (xM aMaxCalm - bcalm b) `divUp` oneM)
         else max (-20) p
    IK.OverfillCalm p ->
         if p > 0
         then min p (max 1 $ fromEnum $ (xM aMaxCalm - bcalm b) `divUp` oneM)
         else max (-20) p
    IK.Dominate -> -200
    IK.Impress -> -10
    IK.Summon grp d ->  -- contrived by not taking into account alliances
      Dice.meanDice d * 50 * if grp == fgroup (gplayer fact) then 1 else -1
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
      let (total, count) = organBenefit grp cops b ar fact
      in total `divUp` count  -- average over all matching grp; rarities ignored
    IK.CreateItem{} -> 30
    IK.DropItem _ _ COrgan grp ->
      let (total, _) = organBenefit grp cops b ar fact
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
    IK.OneOf efs -> let bs = map (effectToBenefit cops b ar fact) efs
                    in if any (< -10) bs
                       then 0  -- mixed blessing
                       else sum bs `divUp` length bs
    IK.OnSmash _ -> 0  -- can be beneficial; we'd need to analyze explosions
    IK.Recharging e -> effectToBenefit cops b ar fact e  -- for weapons
    IK.Temporary _ -> 0
    IK.Unique -> 0
    IK.Periodic -> 0

-- We ignore the possibility of, e.g., temporary extra clawed limb, etc.,
-- so we don't take into account idamage of the item kinds.
organBenefit :: GroupName ItemKind -> Kind.COps
             -> Actor -> AspectRecord -> Faction
             -> (Int, Int)
organBenefit t cops@Kind.COps{coitem=Kind.Ops{ofoldlGroup'}} b ar fact =
  let f (!sacc, !pacc) !p _ !kind =
        let paspect asp = p * aspectToBenefit cops b asp
            peffect eff = p * effectToBenefit cops b ar fact eff
        in ( sacc + sum (map paspect $ IK.iaspects kind)
                  + sum (map peffect $ IK.ieffects kind)
           , pacc + p )
  in ofoldlGroup' t f (0, 0)

-- | Return the value to add to effect value.
aspectToBenefit :: Kind.COps -> Actor -> IK.Aspect -> Int
aspectToBenefit _cops _b asp =
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
    IK.AddShine p -> Dice.meanDice p * 10
    IK.AddNocto p -> Dice.meanDice p * 50
    IK.AddAbility _ p -> Dice.meanDice p * 5

-- | Determine the total benefit from having an item in eqp or inv,
-- according to item type, and also the benefit conferred by equipping the item
-- and from meleeing with it or applying it or throwing it.
totalUsefulness :: Kind.COps -> Actor -> AspectRecord -> Faction -> ItemFull
                -> Maybe (Int, Int)
totalUsefulness cops b ar fact itemFull =
  let ben effects aspects =
        let effSum = -(min 150
                           (10 * Dice.meanDice (jdamage $ itemBase itemFull)))
                     + sum (map (effectToBenefit cops b ar fact) effects)
            aspBens = map (aspectToBenefit cops b) $ aspectRecordToList aspects
            periodicEffBens = map (effectToBenefit cops b ar fact)
                                  (stripRecharging effects)
            timeout = aTimeout $ aspectRecordFull itemFull
            periodicBens | timeout == 0 = []
                         | otherwise =
              map (\eff -> eff * 10 `divUp` timeout) periodicEffBens
            selfBens = aspBens ++ periodicBens
            selfSum = sum selfBens
            mixedBlessing =
              not (null selfBens)
              && (selfSum > 0 && minimum selfBens < -10
                  || selfSum < 0 && maximum selfBens > 10)
            isWeapon = isMelee $ itemBase itemFull
            totalSum
              | isWeapon && effSum < 0 = - effSum + selfSum
              | not $ goesIntoEqp $ itemBase itemFull = effSum
              | mixedBlessing =
                  0  -- significant mixed blessings out of AI control
              | otherwise = selfSum  -- if the weapon heals the enemy, it
                                     -- won't be used but can be equipped
        in (totalSum, effSum)
  in case itemDisco itemFull of
    Just ItemDisco{itemAspect=Just aspectRecord, itemKind=IK.ItemKind{ieffects}} ->
      Just $ ben ieffects aspectRecord
    Just ItemDisco{itemAspectMean, itemKind=IK.ItemKind{ieffects}} ->
      Just $ ben ieffects itemAspectMean
    _ -> Nothing
