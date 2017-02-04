-- | Description of effects. No operation in this module
-- involves state or monad types.
module Game.LambdaHack.Common.EffectDescription
  ( effectToSuffix, featureToSuff, kindAspectToSuffix
  , featureToSentence, slotToSentence, affixDice
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ItemKind

-- | Suffix to append to a basic content name if the content causes the effect.
--
-- We show absolute time in seconds, not @moves@, because actors can have
-- different speeds (and actions can potentially take different time intervals).
-- We call the time taken by one player move, when walking, a @move@.
-- @Turn@ and @clip@ are used mostly internally, the former as an absolute
-- time unit.
-- We show distances in @steps@, because one step, from a tile to another
-- tile, is always 1 meter. We don't call steps @tiles@, reserving
-- that term for the context of terrain kinds or units of area.
effectToSuffix :: Effect -> Text
effectToSuffix effect =
  case effect of
    ELabel _ -> ""  -- printed specially
    EqpSlot{} -> ""  -- used in @slotToSentence@ instead
    Burn d -> wrapInParens (tshow d
                            <+> if d > 1 then "burns" else "burn")
    Explode t -> "of" <+> tshow t <+> "explosion"
    RefillHP p | p > 0 ->
      "of limited healing" <+> wrapInParens (affixBonus p)
    RefillHP 0 -> assert `failure` effect
    RefillHP p ->
      "of limited wounding" <+> wrapInParens (affixBonus p)
    OverfillHP p | p > 0 -> "of healing" <+> wrapInParens (affixBonus p)
    OverfillHP 0 -> assert `failure` effect
    OverfillHP p -> "of wounding" <+> wrapInParens (affixBonus p)
    RefillCalm p | p > 0 ->
      "of limited soothing" <+> wrapInParens (affixBonus p)
    RefillCalm 0 -> assert `failure` effect
    RefillCalm p ->
      "of limited dismaying" <+> wrapInParens (affixBonus p)
    OverfillCalm p | p > 0 -> "of soothing" <+> wrapInParens (affixBonus p)
    OverfillCalm 0 -> assert `failure` effect
    OverfillCalm p -> "of dismaying" <+> wrapInParens (affixBonus p)
    Dominate -> "of domination"
    Impress -> "of impression"
    CallFriend 1 -> "of aid calling"
    CallFriend dice -> "of aid calling"
                       <+> wrapInParens (tshow dice <+> "friends")
    Summon _freqs 1 -> "of summoning"  -- TODO
    Summon _freqs dice -> "of summoning"
                          <+> wrapInParens (tshow dice <+> "actors")
    ApplyPerfume -> "of smell removal"
    Ascend 1 -> "of ascending"
    Ascend p | p > 0 ->
      "of ascending for" <+> tshow p <+> "levels"
    Ascend 0 -> assert `failure` effect
    Ascend (-1) -> "of descending"
    Ascend p ->
      "of descending for" <+> tshow (-p) <+> "levels"
    Escape{} -> "of escaping"
    Paralyze dice ->
      let time = case Dice.reduceDice dice of
            Nothing -> tshow dice
            Just p ->
              let clipInTurn = timeTurn `timeFit` timeClip
                  seconds =
                    0.5 * fromIntegral p / fromIntegral clipInTurn :: Double
              in tshow seconds <> "s"
      in "of paralysis for" <+> time
    InsertMove dice ->
      let moves = case Dice.reduceDice dice of
            Nothing -> tshow dice <+> "moves"
            Just p -> makePhrase [MU.CarWs p "move"]
      in "of speed surge for" <+> moves
    Teleport dice | dice <= 0 ->
      assert `failure` effect
    Teleport dice | dice <= 9 ->
      "of blinking" <+> wrapInParens (tshow dice <+> "steps")
    Teleport dice ->
      "of teleport" <+> wrapInParens (tshow dice <+> "steps")
    CreateItem COrgan grp tim ->
      let stime = if tim == TimerNone then "" else "for" <+> tshow tim <> ":"
      in "(keep" <+> stime <+> tshow grp <> ")"
    CreateItem _ grp _ ->
      let object = if grp == "useful" then "" else tshow grp
      in "of" <+> object <+> "uncovering"
    DropItem COrgan grp -> "of nullify" <+> tshow grp
    DropItem _ grp ->
      "of" <+> "drop" <+> tshow grp  -- TMI: <+> ppCStore store
    PolyItem -> "of repurpose on the ground"
    Identify -> "of identify on the ground"
    Detect radius -> "of radius" <+> tshow radius <+> "detection"
    DetectActor radius -> "of radius" <+> tshow radius <+> "actor detection"
    DetectItem radius -> "of radius" <+> tshow radius <+> "item detection"
    DetectExit radius -> "of radius" <+> tshow radius <+> "exit detection"
    SendFlying tmod -> "of impact" <+> tmodToSuff "" tmod
    PushActor tmod -> "of pushing" <+> tmodToSuff "" tmod
    PullActor tmod -> "of pulling" <+> tmodToSuff "" tmod
    DropBestWeapon -> "of disarming"
    ActivateInv ' ' -> "of inventory burst"
    ActivateInv symbol -> "of burst '" <> T.singleton symbol <> "'"
    OneOf l ->
      let subject = if length l <= 5 then "marvel" else "wonder"
      in makePhrase ["of", MU.CardinalWs (length l) subject]
    OnSmash _ -> ""  -- printed inside a separate section
    Recharging _ -> ""  -- printed inside Periodic or Timeout
    Temporary _ -> ""
    Unique -> ""  -- marked by capital letters in name
    Periodic -> ""  -- printed specially

slotToSentence :: EqpSlot -> Text
slotToSentence es = case es of
  EqpSlotMiscBonus -> "Those that don't scorn minor bonuses may equip it."
  EqpSlotAddHurtMelee -> "Veteran melee fighters are known to devote equipment slot to it."
  EqpSlotAddArmorMelee -> "People in risk from melee damage wear it."
  EqpSlotAddArmorRanged -> "People scared of shots in the dark wear it."
  EqpSlotAddMaxHP -> "The frail wear it to increase their Hit Point capacity."
  EqpSlotAddSpeed -> "The slughish equip it to speed up their whole life."
  EqpSlotAddSight -> "The short-sighted wear it to spot their demise sooner."
  EqpSlotLightSource -> "Explorers brave enough to highlight themselves put it in their equipment."
  EqpSlotWeapon -> "Melee fighters consider it for their weapon combo."
  EqpSlotMiscAbility -> "Those that don't scorn uncanny skills may equip it."
  EqpSlotAbMove -> "Those unskilled in movement equip it."
  EqpSlotAbMelee -> "Those unskilled in melee equip it."
  EqpSlotAbDisplace -> "Those unskilled in displacing equip it."
  EqpSlotAbAlter -> "Those unskilled in alteration equip it."
  EqpSlotAbProject -> "Those unskilled in flinging equip it."
  EqpSlotAbApply -> "Those unskilled in applying items equip it."

tmodToSuff :: Text -> ThrowMod -> Text
tmodToSuff verb ThrowMod{..} =
  let vSuff | throwVelocity == 100 = ""
            | otherwise = "v=" <> tshow throwVelocity <> "%"
      tSuff | throwLinger == 100 = ""
            | otherwise = "t=" <> tshow throwLinger <> "%"
  in if vSuff == "" && tSuff == "" then ""
     else verb <+> "with" <+> vSuff <+> tSuff

kindAspectToSuffix :: Aspect -> Text
kindAspectToSuffix aspect =
  case aspect of
    Timeout{} -> ""  -- printed specially
    AddHurtMelee{} -> ""  -- printed together with dice, even if dice is zero
    AddArmorMelee t -> "[" <> affixDice t <> "%]"
    AddArmorRanged t -> "{" <> affixDice t <> "%}"
    AddMaxHP t -> wrapInParens $ affixDice t <+> "HP"
    AddMaxCalm t -> wrapInParens $ affixDice t <+> "Calm"
    AddSpeed t -> wrapInParens $ affixDice t <+> "speed"
    AddSight t -> wrapInParens $ affixDice t <+> "sight"
    AddSmell t -> wrapInParens $ affixDice t <+> "smell"
    AddShine t -> wrapInParens $ affixDice t <+> "shine"
    AddNocto t -> wrapInParens $ affixDice t <+> "night vision"
    AddAbility ab t -> wrapInParens $ affixDice t <+> tshow ab

featureToSuff :: Feature -> Text
featureToSuff feat =
  case feat of
    Fragile -> wrapInChevrons "fragile"
    Lobable -> wrapInChevrons "can be lobbed"
    Durable -> wrapInChevrons "durable"
    ToThrow tmod -> wrapInChevrons $ tmodToSuff "flies" tmod
    Identified -> ""
    Applicable -> ""
    Equipable -> ""
    Meleeable -> ""
    Precious -> ""
    Tactic tactics -> "overrides tactics to" <+> tshow tactics

featureToSentence :: Feature -> Maybe Text
featureToSentence feat =
  case feat of
    Fragile -> Nothing
    Lobable -> Nothing
    Durable -> Nothing
    ToThrow{} -> Nothing
    Identified -> Nothing
    Applicable -> Just "It is meant to be applied."
    Equipable -> Nothing
    Meleeable -> Just "It is considered for melee strikes by default."
    Precious -> Just "It seems precious."
    Tactic{}  -> Nothing

affixBonus :: Int -> Text
affixBonus p = case compare p 0 of
  EQ -> "0"
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
