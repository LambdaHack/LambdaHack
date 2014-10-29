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
import qualified NLP.Miniutter.English as MU

-- import Game.LambdaHack.Common.Actor (ppCStore)
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Time

-- | Suffix to append to a basic content name if the content causes the effect.
effectToSuff :: (Show a, Ord a, Num a)
             => Effect a -> (a -> Text) -> (a -> Maybe Int) -> Text
effectToSuff effect f g =
  rawEffectToSuff (St.evalState (effectTrav effect $ return . f) ())
                  (St.evalState (effectTrav effect $ return . g) ())

rawEffectToSuff :: Effect Text -> Effect (Maybe Int) -> Text
rawEffectToSuff effectText effectMInt =
  case (effectText, effectMInt) of
    (NoEffect t, _) -> t
    (RefillHP p, _) | p > 0 -> "of healing" <+> wrapInParens (affixBonus p)
    (RefillHP 0, _) -> assert `failure` (effectText, effectMInt)
    (RefillHP p, _) -> "of wounding" <+> wrapInParens (affixBonus p)
    (Hurt dice, _) -> wrapInParens (tshow dice)
    (RefillCalm p, _) | p > 0 -> "of soothing" <+> wrapInParens (affixBonus p)
    (RefillCalm 0, _) -> assert `failure` (effectText, effectMInt)
    (RefillCalm p, _) -> "of dismaying" <+> wrapInParens (affixBonus p)
    (Dominate, _) -> "of domination"
    (Impress, _) -> "of impression"
    (_, CallFriend (Just 1)) -> "of aid calling"
    (CallFriend t, _) -> "of aid calling"
                         <+> wrapInParens (dropPlus t <+> "friends")
    (_, Summon _freqs (Just 1)) -> "of summoning"  -- TODO
    (Summon _freqs t, _) -> "of summoning"
                            <+> wrapInParens (dropPlus t <+> "actors")
    (_, CreateItem (Just 1)) -> "of uncovering"
    (CreateItem t, _) -> "of uncovering"
                         <+> wrapInParens (dropPlus t <+> "items")
    (ApplyPerfume, _) -> "of smell removal"
    (Burn p, _) | p <= 0 -> assert `failure` (effectText, effectMInt)
    (Burn p, _) -> wrapInParens (makePhrase [MU.CarWs p "burn"])
    (Ascend 1, _) -> "of ascending"
    (Ascend p, _) | p > 0 ->
      "of ascending" <+> wrapInParens (tshow p <+> "levels")
    (Ascend 0, _) -> assert `failure` (effectText, effectMInt)
    (Ascend (-1), _) -> "of descending"
    (Ascend p, _) ->
      "of descending" <+> wrapInParens (tshow (-p) <+> "levels")
    (Escape{}, _) -> "of escaping"
    (_, Paralyze Nothing) -> "of paralysis (? clips)"
    (_, Paralyze (Just p)) ->
      let clipInTurn = timeTurn `timeFit` timeClip
          seconds = 0.5 * fromIntegral p / fromIntegral clipInTurn :: Double
      in "of paralysis" <+> wrapInParens (tshow seconds <> "s")
    (_, InsertMove Nothing) ->
      "of speed surge (? moves)"
    (_, InsertMove (Just p)) ->
      "of speed surge" <+> wrapInParens (makePhrase [MU.CarWs p "move"])
    (DropBestWeapon, _) -> "of disarming"
    (DropEqp ' ' False, _) -> "of equipment drop"
    (DropEqp symbol False, _) -> "of drop '" <> T.singleton symbol <> "'"
    (DropEqp ' ' True, _) -> "of equipment smash"
    (DropEqp symbol True, _) -> "of smash '" <> T.singleton symbol <> "'"
    (SendFlying tmod, _) -> "of impact" <+> tmodToSuff "" tmod
    (PushActor tmod, _) -> "of pushing" <+> tmodToSuff "" tmod
    (PullActor tmod, _) -> "of pulling" <+> tmodToSuff "" tmod
    (_, Teleport (Just p)) | p <= 1  ->
      assert `failure` (effectText, effectMInt)
    (Teleport t, Teleport (Just p)) | p <= 9  ->
      "of blinking" <+> wrapInParens (dropPlus t <+> "steps")
    (Teleport t, _)->
      "of teleport" <+> wrapInParens (dropPlus t <+> "steps")
    (PolyItem _cstore, _) -> "of repurpose"  -- <+> ppCStore cstore
    (Identify _cstore, _) -> "of identify"  -- <+> ppCStore cstore
    (ActivateInv ' ', _) -> "of inventory burst"
    (ActivateInv symbol, _) -> "of burst '" <> T.singleton symbol <> "'"
    (Explode _, _) -> "of explosion"  -- TODO: first word + explosion? nothing?
    (OneOf l, _) ->
      let subject = if length l <= 5 then "marvel" else "wonder"
      in makePhrase ["of", MU.CardinalWs (length l) subject]
    (OnSmash _, _) -> ""  -- conditional effect, TMI
    (Recharging _, _) -> ""  -- printed inside Periodic or Timeout
    (CreateOrgan k t, _) ->
      let stime = if k == 0 then "" else tshow k <> ":"
      in "(keep" <+> stime <+> tshow t <> ")"
    (Temporary _, _) -> ""
    _ -> assert `failure` (effectText, effectMInt)

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
  rawAspectToSuff $ St.evalState (aspectTrav aspect $ return . f) ()

rawAspectToSuff :: Aspect Text -> Text
rawAspectToSuff aspect =
  case aspect of
    Periodic{} -> ""  -- printed specially
    Timeout{}  -> ""  -- printed specially
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
    ChangeTo t -> wrapInChevrons $ "changes to" <+> tshow t
    Fragile -> wrapInChevrons $ "fragile"
    Durable -> wrapInChevrons $ "durable"
    ToThrow tmod -> wrapInChevrons $ tmodToSuff "flies" tmod
    Identified -> ""
    Applicable -> ""
    EqpSlot{} -> ""
    Precious -> ""
    Tactic tactics -> "overrides tactics to" <+> tshow tactics

dropPlus :: Text -> Text
dropPlus = T.dropWhile (`elem` ['+', '-'])

effectToSuffix :: Effect Int -> Text
effectToSuffix effect = effectToSuff effect affixBonus Just

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
affixDice d = maybe "+?" affixBonus $ Dice.reduceDice d

kindEffectToSuffix :: Effect Dice.Dice -> Text
kindEffectToSuffix effect = effectToSuff effect affixDice Dice.reduceDice

kindAspectToSuffix :: Aspect Dice.Dice -> Text
kindAspectToSuffix aspect = aspectToSuff aspect affixDice
