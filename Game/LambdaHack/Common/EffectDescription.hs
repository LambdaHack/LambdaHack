-- | Description of effects. No operation in this module
-- involves state or monad types.
module Game.LambdaHack.Common.EffectDescription
  ( effectToSuffix, aspectToSuffix, featureToSuff
  , kindEffectToSuffix, kindAspectToSuffix
  ) where

import Control.Exception.Assert.Sugar
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
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
    RefillHP p | p > 0 -> "of healing" <+> wrapInParens (affixBonus p)
    RefillHP 0 -> assert `failure` effect
    RefillHP p -> "of wounding" <+> wrapInParens (affixBonus p)
    Hurt dice t -> wrapInParens (tshow dice) <+> t
    RefillCalm p | p > 0 -> "of soothing" <+> wrapInParens (affixBonus p)
    RefillCalm 0 -> assert `failure` effect
    RefillCalm p -> "of dismaying" <+> wrapInParens (affixBonus p)
    Dominate -> "of domination"
    Impress -> "of impression"
    CallFriend 1 -> "of aid calling"
    CallFriend p -> "of aid calling" <+> wrapInParens (affixBonus p)
    Summon t -> "of summoning" <+> wrapInParens t
    CreateItem 1 -> "of uncovering"
    CreateItem p -> "of uncovering" <+> wrapInParens (affixBonus p)
    ApplyPerfume -> "of rose water"
    Burn p -> "of burning" <+> wrapInParens (affixBonus p)
    Ascend 1 -> "of ascending"
    Ascend p | p > 0 -> "of ascending" <+> wrapInParens (affixBonus p)
    Ascend 0 -> assert `failure` effect
    Ascend (-1) -> "of descending"
    Ascend p -> "of descending" <+> wrapInParens (affixBonus (- p))
    Escape{} -> "of escaping"
    Paralyze t -> "of paralysis" <+> wrapInParens t
    InsertMove t -> "of speed surge" <+> wrapInParens t
    DropBestWeapon -> "of disarming"
    DropEqp ' ' False -> "of equipment dropping"
    DropEqp symbol False -> "of '" <> T.singleton symbol <> "' dropping"
    DropEqp ' ' True -> "of equipment smashing"
    DropEqp symbol True -> "of '" <> T.singleton symbol <> "' smashing"
    SendFlying ThrowMod{..} ->
      case effect of
        SendFlying tmod -> "of impact" <+> tmodToSuff "" tmod
        _ -> assert `failure` effect
    PushActor ThrowMod{..} ->
      case effect of
        PushActor tmod -> "of pushing" <+> tmodToSuff "" tmod
        _ -> assert `failure` effect
    PullActor ThrowMod{..} ->
      case effect of
        PullActor tmod -> "of pulling" <+> tmodToSuff "" tmod
        _ -> assert `failure` effect
    Teleport t ->
      case effect of
        Teleport p | p > 9 -> "of teleport" <+> wrapInParens t
        Teleport _ -> "of blinking" <+> wrapInParens t
        _ -> assert `failure` effect
    ActivateEqp ' ' -> "of equipment burst"
    ActivateEqp symbol -> "of '" <> T.singleton symbol <> "' burst"
    Explode _ -> "of explosion"
    OnSmash _ -> ""  -- conditional effect, TMI
    TimedAspect _ _ ->
      case effect of
        TimedAspect _ aspect -> "keep {" <> aspectToSuff aspect f <> "}"
        _ -> assert `failure` effect

tmodToSuff :: Text -> ThrowMod -> Text
tmodToSuff verb ThrowMod{..} =
  let vSuff | throwVelocity == 100 = ""
            | otherwise = "v=" <> tshow throwVelocity <> "%"
      tSuff | throwLinger == 100 = ""
            | otherwise = "t=" <> tshow throwLinger <> "%"
  in if vSuff == "" && tSuff == "" then ""
     else verb <+> "with" <+> vSuff <+> tSuff

aspectToSuff :: Show a => Aspect a -> (a -> Text) -> Text
aspectToSuff aspect f =
  case St.evalState (aspectTrav aspect $ return . f) () of
    Periodic _ ->
      case aspect of
        Periodic n -> wrapInParens $ tshow n <+> "in 100"
        _ -> assert `failure` aspect
    AddMaxHP t -> wrapInParens $ t <+> "HP"
    AddMaxCalm t -> wrapInParens $ t <+> "Calm"
    AddSpeed t -> wrapInParens $ t <+> "speed"
    AddSkills p -> wrapInParens $ "+" <+> tshow (EM.toList p)
    AddHurtMelee t -> wrapInParens $ t <> "% melee"
    AddHurtRanged  t -> wrapInParens $ t <> "% ranged"
    AddArmorMelee t -> "[" <> t <> "%]"
    AddArmorRanged t -> "{" <> t <> "%}"
    AddSight t -> wrapInParens $ t <+> "sight"
    AddSmell t -> wrapInParens $ t <+> "smell"
    AddLight t -> wrapInParens $ t <+> "light"

featureToSuff :: Feature -> Text
featureToSuff feat =
  case feat of
    ChangeTo t -> wrapInChevrons $ "changes to" <+> t
    Fragile -> wrapInChevrons $ "fragile"
    Durable -> wrapInChevrons $ "durable"
    ToThrow tmod -> wrapInChevrons $ tmodToSuff "flies" tmod
    Applicable -> ""
    EqpSlot{} -> ""
    Identified -> ""
    Precious -> ""

effectToSuffix :: Effect Int -> Text
effectToSuffix effect = effectToSuff effect affixBonus

aspectToSuffix :: Aspect Int -> Text
aspectToSuffix aspect = aspectToSuff aspect affixBonus

affixBonus :: Int -> Text
affixBonus p = case compare p 0 of
  EQ -> ""
  LT -> tshow p
  GT -> "+" <> tshow p

wrapInParens :: Text -> Text
wrapInParens "" = ""
wrapInParens t = "(" <> t <> ")"

wrapInChevrons :: Text -> Text
wrapInChevrons "" = ""
wrapInChevrons t = "<" <> t <> ">"

affixDice :: Dice.Dice -> Text
affixDice d = if Dice.minDice d == Dice.maxDice d
               then affixBonus (Dice.minDice d)
               else "+?"

kindEffectToSuffix :: Effect Dice.Dice -> Text
kindEffectToSuffix effect = effectToSuff effect affixDice

kindAspectToSuffix :: Aspect Dice.Dice -> Text
kindAspectToSuffix aspect = aspectToSuff aspect affixDice
