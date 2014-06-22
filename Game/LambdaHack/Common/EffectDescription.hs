-- | Description of effects. No operation in this module
-- involves state or monad types.
module Game.LambdaHack.Common.EffectDescription
  ( effectToSuffix, aspectToSuffix, featureToSuff
  , kindEffectToSuffix, kindAspectToSuffix
  , affixPower, affixBonus
  ) where

import Control.Exception.Assert.Sugar
import qualified Control.Monad.State as St
import Data.Text (Text)
import qualified Data.Text as T

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Msg

-- | Suffix to append to a basic content name if the content causes the effect.
effectToSuff :: (Show a, Ord a, Num a) => Effect a -> (a -> Text) -> Text
effectToSuff effect f =
  case St.evalState (effectTrav effect $ return . f) () of
    NoEffect -> ""
    Heal p | p > 0 -> "of healing" <+> affixBonus p
    Heal 0 -> assert `failure` effect
    Heal p -> "of wounding" <+> affixBonus p
    Hurt dice t -> "(" <> tshow dice <> ")" <+> t
    Calm p | p > 0 -> "of soothing" <+> affixBonus p
    Calm 0 -> assert `failure` effect
    Calm p -> "of alarming" <+> affixBonus p
    Dominate -> "of domination"
    Impress -> "of impression"
    CallFriend p -> "of aid calling" <+> affixPower p
    Summon t -> "of summoning" <+> t
    CreateItem p -> "of item creation" <+> affixPower p
    ApplyPerfume -> "of rose water"
    Burn{} -> ""  -- often accompanies AddLight, too verbose, too boring
    Blast p -> "of explosion" <+> affixPower p
    Ascend p | p > 0 -> "of ascending" <+> affixPower p
    Ascend 0 -> assert `failure` effect
    Ascend p -> "of descending" <+> affixPower (- p)
    Escape{} -> "of escaping"
    Paralyze t -> "of paralysis" <+> t
    InsertMove t -> "of speed burst" <+> t
    DropBestWeapon -> "of disarming"
    DropEqp ' ' False -> "of equipment dropping"
    DropEqp symbol False ->
      "of equipment '" <> T.singleton symbol <> "' dropping"
    DropEqp ' ' True -> "of equipment smashing"
    DropEqp symbol True ->
      "of equipment '" <> T.singleton symbol <> "' smashing"
    SendFlying ThrowMod{..} ->
      case effect of
        SendFlying tmod -> "of impact" <+> tmodToSuff tmod
        _ -> assert `failure` effect
    PushActor ThrowMod{..} ->
      case effect of
        PushActor tmod -> "of pushing" <+> tmodToSuff tmod
        _ -> assert `failure` effect
    PullActor ThrowMod{..} ->
      case effect of
        PullActor tmod -> "of pulling" <+> tmodToSuff tmod
        _ -> assert `failure` effect
    Teleport t ->
      case effect of
        Teleport p | p > 9 -> "of teleport" <+> t
        Teleport _ -> "of blinking" <+> t
        _ -> assert `failure` effect
    ActivateEqp ' ' -> "of spontaneous activation"
    ActivateEqp symbol ->
      "of spontaneous '" <> T.singleton symbol <> "' activation"
    TimedAspect _ _ ->
      case effect of
        TimedAspect _ aspect -> aspectToSuff aspect f
        _ -> assert `failure` effect

tmodToSuff :: Show a => ThrowMod a -> Text
tmodToSuff ThrowMod{..} = tshow throwVelocity <+> tshow throwLinger

aspectToSuff :: Show a => Aspect a -> (a -> Text) -> Text
aspectToSuff aspect f =
  case St.evalState (aspectTrav aspect $ return . f) () of
    NoAspect -> ""
    Periodic _ ->
      case aspect of
        Periodic n -> "(" <> tshow n <+> "in 100)"
        _ -> assert `failure` aspect
    AddMaxHP t -> "(" <> t <+> "HP)"
    AddMaxCalm t -> "(" <> t <+> "Calm)"
    AddSpeed t -> "of speed" <+> t
    AddSkills t -> "of" <+> tshow t  -- TODO
    ArmorMelee t -> "[" <> t <> "]"
    SightRadius t -> "of sight" <+> t
    SmellRadius t -> "of smell" <+> t
    AddLight t -> "shining" <+> t
    Explode{} -> ""

featureToSuff :: Feature -> Text
featureToSuff feat =
  case feat of
    ChangeTo{} -> ""
    Fragile -> ""
    Durable -> ""
    ToThrow{} -> ""
    Applicable -> ""
    EqpSlot{} -> ""
    Identified -> ""
    Precious -> ""

effectToSuffix :: Effect Int -> Text
effectToSuffix effect = effectToSuff effect affixBonus

aspectToSuffix :: Aspect Int -> Text
aspectToSuffix aspect = aspectToSuff aspect affixBonus

affixPower :: Int -> Text
affixPower p = case compare p 1 of
  EQ -> ""
  LT -> assert `failure` "power less than 1" `twith` p
  GT -> "(+" <> tshow p <> ")"

affixBonus :: Int -> Text
affixBonus p = case compare p 0 of
  EQ -> ""
  LT -> "(" <> tshow p <> ")"
  GT -> "(+" <> tshow p <> ")"

affixDice :: Dice.Dice -> Text
affixDice d = if Dice.minDice d == Dice.maxDice d
               then affixBonus (Dice.minDice d)
               else "(?)"

kindEffectToSuffix :: Effect Dice.Dice -> Text
kindEffectToSuffix effect = effectToSuff effect affixDice

kindAspectToSuffix :: Aspect Dice.Dice -> Text
kindAspectToSuffix aspect = aspectToSuff aspect affixDice
