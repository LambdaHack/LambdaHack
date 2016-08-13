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
effectToBenefit :: Kind.COps -> Actor -> [ItemFull] -> Faction
                -> IK.Effect -> Int
effectToBenefit cops b activeItems fact eff =
  let dungeonDweller = not $ fcanEscape $ gplayer fact
  in case eff of
    IK.ELabel _ -> 0
    IK.Hurt d -> -(min 150 $ 10 * Dice.meanDice d)
    IK.Burn d -> -(min 200 $ 15 * Dice.meanDice d)
                   -- often splash damage, etc.
    IK.Explode _ -> 0  -- depends on explosion
    IK.RefillHP p ->
      let hpMax = sumSlotNoFilter IK.EqpSlotAddMaxHP activeItems
      in if p > 0
         -- TODO: when picking up, always deem valuable; when drinking, only if
         -- HP not maxxed.
         then 10 * min p (max 0 $ fromIntegral
                          $ (xM hpMax - bhp b) `divUp` oneM)
         else max (-99) (11 * p)
    IK.OverfillHP p ->
      let hpMax = sumSlotNoFilter IK.EqpSlotAddMaxHP activeItems
      in if p > 0
         then 11 * min p (max 1 $ fromIntegral
                          $ (xM hpMax - bhp b) `divUp` oneM)
         else max (-99) (11 * p)
    IK.RefillCalm p ->
      let calmMax = sumSlotNoFilter IK.EqpSlotAddMaxCalm activeItems
      in if p > 0
         then min p (max 0 $ fromIntegral
                     $ (xM calmMax - bcalm b) `divUp` oneM)
         else max (-20) p
    IK.OverfillCalm p ->
      let calmMax = sumSlotNoFilter IK.EqpSlotAddMaxCalm activeItems
      in if p > 0
         then min p (max 1 $ fromIntegral
                     $ (xM calmMax - bcalm b) `divUp` oneM)
         else max (-20) p
    IK.Dominate -> -200
    IK.Impress -> -10
    IK.CallFriend d -> 100 * Dice.meanDice d
    IK.Summon _ d | dungeonDweller ->
      -- Probably summons friends or crazies.
      -- TODO: should be Negative, to use Calm of enemy, but also positive
      -- to use with own Calm, if needed.
      50 * Dice.meanDice d
    IK.Summon{} -> 0      -- probably generates enemies
    IK.Ascend{} -> 1      -- low, to only change levels sensibly, in teams
                          -- TODO: use if low HP and enemies at hand
    IK.Escape{} -> 10000  -- AI wants to win; spawners to guard
    IK.Paralyze d -> -10 * Dice.meanDice d
    IK.InsertMove d -> 50 * Dice.meanDice d
    IK.Teleport d ->
      let p = Dice.meanDice d
      in if p <= 8  -- blink to shoot at foe
            && dungeonDweller  -- non-dwellers have to explore and escape ASAP
         then 1
         else -p  -- get rid of the foe
    IK.CreateItem COrgan grp _ ->  -- TODO: use the timeout
      let (total, count) = organBenefit grp cops b activeItems fact
      in total `divUp` count  -- average over all matching grp; rarities ignored
    IK.CreateItem{} -> 30  -- TODO
    IK.DropItem COrgan grp True ->  -- calculated for future use, general pickup
      let (total, _) = organBenefit grp cops b activeItems fact
      in - total  -- sum over all matching grp; simplification: rarities ignored
    IK.DropItem _ _ False -> -15
    IK.DropItem _ _ True -> -30
    IK.PolyItem -> 0  -- AI can't estimate item desirability vs average
    IK.Identify -> 0  -- AI doesn't know how to use
    IK.SendFlying _ -> -10  -- but useful on self sometimes, too
    IK.PushActor _ -> -10  -- but useful on self sometimes, too
    IK.PullActor _ -> -10
    IK.DropBestWeapon -> -50
    IK.ActivateInv ' ' -> -100
    IK.ActivateInv _ -> -50
    IK.ApplyPerfume -> 0  -- depends on the smell sense of friends and foes
    IK.OneOf _ -> 1  -- usually a mixed blessing, but slightly beneficial
    IK.OnSmash _ -> 0  -- TOOD: can be beneficial or not; analyze explosions
    IK.Recharging e ->
      -- Used, e.g., in @periodicBens@, which takes timeout into account, too.
      effectToBenefit cops b activeItems fact e
    IK.Temporary _ -> 0

-- TODO: calculating this for "temporary conditions" takes forever
organBenefit :: GroupName ItemKind -> Kind.COps
             -> Actor -> [ItemFull] -> Faction
             -> (Int, Int)
organBenefit t cops@Kind.COps{coitem=Kind.Ops{ofoldrGroup}} b activeItems fact =
  let f p _ kind (sacc, pacc) =
        let paspect asp = p * aspectToBenefit cops b (Dice.meanDice <$> asp)
            peffect eff = p * effectToBenefit cops b activeItems fact eff
        in ( sacc + sum (map paspect $ IK.iaspects kind)
                  + sum (map peffect $ IK.ieffects kind)
           , pacc + p )
  in ofoldrGroup t f (0, 0)

-- | Return the value to add to effect value.
aspectToBenefit :: Kind.COps -> Actor -> IK.Aspect Int -> Int
aspectToBenefit _cops _b asp =
  case asp of
    IK.Unique{} -> 0
    IK.Periodic{} -> 0
    IK.Timeout{} -> 0
    IK.AddHurtMelee p -> p
    IK.AddHurtRanged p | p < 0 -> 0  -- TODO: don't ignore for missiles
    IK.AddHurtRanged p -> p `divUp` 5  -- TODO: should be summed with damage
    IK.AddArmorMelee p -> p `divUp` 5
    IK.AddArmorRanged p -> p `divUp` 10
    IK.AddMaxHP p -> p
    IK.AddMaxCalm p -> p `div` 5
    IK.AddSpeed p -> p * 10000
    IK.AddAbility _ p -> 5 * p
    IK.AddSight p -> p * 10
    IK.AddSmell p -> p * 10
    IK.AddShine p -> p * 10
    IK.AddNocto p -> p * 50

-- | Determine the total benefit from having an item in eqp or inv,
-- according to item type, and also the benefit confered by equipping the item
-- and from meleeing with it or applying it or throwing it.
totalUsefulness :: Kind.COps -> Actor -> [ItemFull] -> Faction -> ItemFull
                -> Maybe (Int, Int)
totalUsefulness cops b activeItems fact itemFull =
  let ben effects aspects =
        let effBens = map (effectToBenefit cops b activeItems fact) effects
            aspBens = map (aspectToBenefit cops b) aspects
            periodicEffBens = map (effectToBenefit cops b activeItems fact)
                                  (allRecharging effects)
            periodicBens =
              case strengthFromEqpSlot IK.EqpSlotPeriodic itemFull of
                Nothing -> []
                Just timeout ->
                  map (\eff -> eff * 10 `divUp` timeout) periodicEffBens
            selfBens = aspBens ++ periodicBens
            selfSum = sum selfBens
            mixedBlessing =
              not (null selfBens)
              && (selfSum > 0 && minimum selfBens < -10
                  || selfSum < 0 && maximum selfBens > 10)
            effSum = sum effBens
            isWeapon = isMeleeEqp itemFull
            totalSum
              | isWeapon && effSum < 0 = - effSum + selfSum
              | not $ goesIntoEqp itemFull = effSum
              | mixedBlessing =
                  0  -- significant mixed blessings out of AI control
              | otherwise = selfSum  -- if the weapon heals the enemy, it
                                     -- won't be used but can be equipped
        in (totalSum, effSum)
  in case itemDisco itemFull of
    Just ItemDisco{ itemAE=Just ItemAspectEffect{jaspects}
                  , itemKind=IK.ItemKind{ieffects} } ->
      Just $ ben ieffects jaspects
    Just ItemDisco{itemKind=IK.ItemKind{iaspects, ieffects}} ->
      let jaspects = map (fmap Dice.meanDice) iaspects
      in Just $ ben ieffects jaspects
    _ -> Nothing
