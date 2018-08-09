-- | Description of effects.
module Game.LambdaHack.Client.UI.EffectDescription
  ( DetailLevel(..), effectToSuffix, detectToObject, detectToVerb
  , slotToSentence, slotToName, slotToDesc, slotToDecorator, statSlots
  , kindAspectToSuffix, aspectToSentence, affixDice
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , tmodToSuff, affixBonus, wrapInParens, wrapInChevrons
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Common.Ability
import           Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.ItemKind

data DetailLevel = DetailLow | DetailMedium | DetailHigh | DetailAll
  deriving (Eq, Ord, Enum, Bounded)

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
effectToSuffix :: DetailLevel -> Effect -> Text
effectToSuffix detailLevel effect =
  case effect of
    Burn d -> wrapInParens (tshow d
                            <+> if Dice.maxDice d > 1 then "burns" else "burn")
    Explode t -> "of" <+> tshow t <+> "explosion"
    RefillHP p | p > 0 -> "of healing" <+> wrapInParens (affixBonus p)
    RefillHP 0 -> error $ "" `showFailure` effect
    RefillHP p -> "of wounding" <+> wrapInParens (affixBonus p)
    RefillCalm p | p > 0 -> "of soothing" <+> wrapInParens (affixBonus p)
    RefillCalm 0 -> error $ "" `showFailure` effect
    RefillCalm p -> "of dismaying" <+> wrapInParens (affixBonus p)
    Dominate -> "of domination"
    Impress -> "of impression"
    Summon grp p -> makePhrase
      [ "of summoning"
      , if p <= 1 then "" else MU.Text $ tshow p
      , MU.Ws $ MU.Text $ tshow grp ]
    ApplyPerfume -> "of smell removal"
    Ascend True -> "of ascending"
    Ascend False -> "of descending"
    Escape{} -> "of escaping"
    Paralyze dice ->
      let time = case Dice.reduceDice dice of
            Nothing -> tshow dice <+> "* 0.05s"
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
    Teleport dice | Dice.maxDice dice <= 9 ->
      "of blinking" <+> wrapInParens (tshow dice)
    Teleport dice -> "of teleport" <+> wrapInParens (tshow dice)
    CreateItem COrgan grp tim ->
      let stime = if isTimerNone tim then "" else "for" <+> tshow tim <> ":"
      in "(keep" <+> stime <+> tshow grp <> ")"
    CreateItem{} -> "of gain"
    DropItem n k store grp ->
      let ntxt = if | n == 1 && k == maxBound -> "one kind of"
                    | n == maxBound && k == maxBound -> "all kinds of"
                    | otherwise -> ""
          (verb, fromStore) =
            if store == COrgan
            then ("nullify", "")
            else ("drop", "from" <+> snd (ppCStore store))
      in "of" <+> verb <+> ntxt <+> tshow grp <+> fromStore
    PolyItem -> "of repurpose on the ground"
    Identify -> "of identify"
    Detect d radius ->
      "of" <+> detectToObject d <+> "location" <+> wrapInParens (tshow radius)
    SendFlying tmod -> "of impact" <+> tmodToSuff "" tmod
    PushActor tmod -> "of pushing" <+> tmodToSuff "" tmod
    PullActor tmod -> "of pulling" <+> tmodToSuff "" tmod
    DropBestWeapon -> "of disarming"
    ActivateInv ' ' -> "of backpack burst"
    ActivateInv symbol -> "of burst '" <> T.singleton symbol <> "'"
    OneOf l ->
      let subject = if length l <= 5 then "marvel" else "wonder"
          header = makePhrase ["of", MU.CardinalWs (length l) subject]
          marvels = T.intercalate ", " $ map (effectToSuffix detailLevel) l
      in if detailLevel >= DetailAll && marvels /= ""
         then header <+> "[" <> marvels <> "]"
         else header
    OnSmash _ -> ""  -- printed inside a separate section
    Recharging _ -> ""  -- printed inside Periodic or Timeout
    Temporary _ -> ""  -- only printed on destruction
    Composite effs -> T.intercalate " and then "
                    $ filter (/= "") $ map (effectToSuffix detailLevel) effs

detectToObject :: DetectKind -> Text
detectToObject d = case d of
  DetectAll -> ""
  DetectActor -> "actor"
  DetectItem -> "item"
  DetectExit -> "exit"
  DetectHidden -> "secret"
  DetectEmbed -> "feature"

detectToVerb :: DetectKind -> Text
detectToVerb d = case d of
  DetectAll -> "map surrounding area"
  DetectActor -> "spot nearby"
  DetectItem -> "locate nearby"
  DetectExit -> "learn nearby"
  DetectHidden -> "uncover nearby"
  DetectEmbed -> "notice nearby"

slotToSentence :: EqpSlot -> Text
slotToSentence es = case es of
  EqpSlotMiscBonus -> "Those that don't scorn minor bonuses may equip it."
  EqpSlotAddHurtMelee -> "Veteran melee fighters are known to devote equipment slot to it."
  EqpSlotAddArmorMelee -> "Worn by people in risk of melee wounds."
  EqpSlotAddArmorRanged -> "People scared of shots in the dark wear it."
  EqpSlotAddMaxHP -> "The frail wear it to increase their Hit Point capacity."
  EqpSlotAddSpeed -> "The slughish equip it to speed up their whole life."
  EqpSlotAddSight -> "The short-sighted wear it to spot their demise sooner."
  EqpSlotLightSource -> "Explorers brave enough to highlight themselves put it in their equipment."
  EqpSlotWeapon -> "Melee fighters pick it for their weapon combo."
  EqpSlotMiscAbility -> "Those that don't scorn uncanny skills may equip it."
  EqpSlotAbMove -> "Those unskilled in movement equip it."
  EqpSlotAbMelee -> "Those unskilled in melee equip it."
  EqpSlotAbDisplace -> "Those unskilled in displacing equip it."
  EqpSlotAbAlter -> "Those unskilled in alteration equip it."
  EqpSlotAbProject -> "Those unskilled in flinging equip it."
  EqpSlotAbApply -> "Those unskilled in applying items equip it."
  _ -> error $ "should not be used in content" `showFailure` es

slotToName :: EqpSlot -> Text
slotToName eqpSlot =
  case eqpSlot of
    EqpSlotMiscBonus -> "misc bonuses"
    EqpSlotAddHurtMelee -> "to melee damage"
    EqpSlotAddArmorMelee -> "melee armor"
    EqpSlotAddArmorRanged -> "ranged armor"
    EqpSlotAddMaxHP -> "max HP"
    EqpSlotAddSpeed -> "speed"
    EqpSlotAddSight -> "sight radius"
    EqpSlotLightSource -> "shine radius"
    EqpSlotWeapon -> "weapon power"
    EqpSlotMiscAbility -> "misc abilities"
    EqpSlotAbMove -> abilityDesc SkMove
    EqpSlotAbMelee -> abilityDesc SkMelee
    EqpSlotAbDisplace -> abilityDesc SkDisplace
    EqpSlotAbAlter -> abilityDesc SkAlter
    EqpSlotAbProject -> abilityDesc SkProject
    EqpSlotAbApply -> abilityDesc SkApply
    EqpSlotAddMaxCalm -> "max Calm"
    EqpSlotAddSmell -> "smell radius"
    EqpSlotAddNocto -> "night vision radius"
    EqpSlotAddAggression -> "aggression level"
    EqpSlotAbWait -> abilityDesc SkWait
    EqpSlotAbMoveItem -> abilityDesc SkMoveItem

abilityDesc :: Skill -> Text
abilityDesc SkMove = "move ability"
abilityDesc SkMelee = "melee ability"
abilityDesc SkDisplace = "displace ability"
abilityDesc SkAlter = "alter tile ability"
abilityDesc SkWait = "wait ability"
abilityDesc SkMoveItem = "manage items ability"
abilityDesc SkProject = "fling ability"
abilityDesc SkApply = "apply ability"
abilityDesc SkHurtMelee = "to melee damage"
abilityDesc SkArmorMelee = "melee armor"
abilityDesc SkArmorRanged = "ranged armor"
abilityDesc SkMaxHP = "max HP"
abilityDesc SkMaxCalm = "max Calm"
abilityDesc SkSpeed = "speed"
abilityDesc SkSight = "sight radius"
abilityDesc SkSmell = "smell radius"
abilityDesc SkShine = "shine radius"
abilityDesc SkNocto = "night vision radius"
abilityDesc SkAggression = "aggression level"

slotToDesc :: EqpSlot -> Text
slotToDesc eqpSlot =
  let statName = slotToName eqpSlot
      capName = "The '" <> statName <> "' stat"
  in capName <+> case eqpSlot of
    EqpSlotMiscBonus -> "represent the total power of assorted stat bonuses for the character."
    EqpSlotAddHurtMelee -> "is a percentage of additional damage dealt by the actor (either a character or a missile) with any weapon. The value is capped at 200%, then the armor percentage of the defender is subtracted from it and the resulting total is capped at 99%."
    EqpSlotAddArmorMelee -> "is a percentage of melee damage avoided by the actor. The value is capped at 200%, then the extra melee damage percentage of the attacker is subtracted from it and the resulting total is capped at 99% (always at least 1% of damage gets through). It includes 50% bonus from being braced for combat, if applicable."
    EqpSlotAddArmorRanged -> "is a percentage of ranged damage avoided by the actor. The value is capped at 200%, then the extra melee damage percentage of the attacker is subtracted from it and the resulting total is capped at 99% (always at least 1% of damage gets through). It includes 25% bonus from being braced for combat, if applicable."
    EqpSlotAddMaxHP -> "is a cap on HP of the actor, except for some rare effects able to overfill HP. At any direct enemy damage (but not, e.g., incremental poisoning damage or wounds inflicted by mishandling a device) HP is cut back to the cap."
    EqpSlotAddSpeed -> "is expressed in meters per second, which corresponds to map location (1m by 1m) per two standard turns (0.5s each). Thus actor at standard speed of 2m/s moves one location per standard turn."
    EqpSlotAddSight -> "is the limit of visibility in light. The radius is measured from the middle of the map location occupied by the character to the edge of the furthest covered location."
    EqpSlotLightSource -> "determines the maximal area lit by the actor. The radius is measured from the middle of the map location occupied by the character to the edge of the furthest covered location."
    EqpSlotWeapon -> "represents the total power of weapons equipped by the character."
    EqpSlotMiscAbility -> "represent the total power of assorted ability bonuses for the character."
    EqpSlotAbMove -> "determines whether the character can move. Actors not capable of movement can't be dominated."
    EqpSlotAbMelee -> "determines whether the character can melee. Actors that can't melee can still cause damage by flinging missiles or by ramming (being pushed) at opponents."
    EqpSlotAbDisplace -> "determines whether the character can displace adjacent actors. In some cases displacing is not possible regardless of ability: when the target is braced, dying, has no move ability or when both actors are supported by adjacent friendly units. Missiles can be displaced always, unless more than one occupies the map location."
    EqpSlotAbAlter -> "determines which kinds of terrain can be altered or triggered by the character. Opening doors and searching suspect tiles require ability 2, some stairs require 3, closing doors requires 4, others require 4 or 5. Actors not smart enough to be capable of using stairs can't be dominated."
    EqpSlotAbProject -> "determines which kinds of items the character can propel. Items that can be lobbed to explode at a precise location, such as flasks, require ability 3. Other items travel until they meet an obstacle and ability 1 is enough to fling them. In some cases, e.g., of too intricate or two awkward items at low Calm, throwing is not possible regardless of the ability value."
    EqpSlotAbApply -> "determines which kinds of items the character can activate. Items that assume literacy require ability 2, others can be used already at ability 1. In some cases, e.g., when the item needs recharging, has no possible effects or is too intricate for the character Calm level, applying may not be possible."
    EqpSlotAddMaxCalm -> "is a cap on Calm of the actor, except for some rare effects able to overfill Calm. At any direct enemy damage (but not, e.g., incremental poisoning damage or wounds inflicted by mishandling a device) Calm is lowered, sometimes very significantly and always at least back down to the cap."
    EqpSlotAddSmell -> "determines the maximal area smelled by the actor. The radius is measured from the middle of the map location occupied by the character to the edge of the furthest covered location."
    EqpSlotAddNocto -> "is the limit of visibility in dark. The radius is measured from the middle of the map location occupied by the character to the edge of the furthest covered location."
    EqpSlotAddAggression -> "represents the willingness of the actor to engage in combat, especially close quarters, and conversely, to break engagement when overpowered."
    EqpSlotAbWait -> "determines whether the character can wait, bracing for comat and potentially blocking the effects of some attacks."
    EqpSlotAbMoveItem -> "determines whether the character can pick up items and manage inventory."

slotToDecorator :: EqpSlot -> Actor -> Int -> Text
slotToDecorator eqpSlot b t =
  let tshow200 n = let n200 = min 200 $ max (-200) n
                   in tshow n200 <> if n200 /= n then "$" else ""
      -- Some values can be negative, for others 0 is equivalent but shorter.
      tshowRadius r = if r == 0 then "0m" else tshow (r - 1) <> ".5m"
      tshowBlock k n = tshow200 $ n + if braced b then k else 0
      showIntWith1 :: Int -> Text
      showIntWith1 k =
        let l = k `div` 10
            x = k - l * 10
        in tshow l <> if x == 0 then "" else "." <> tshow x
  in case eqpSlot of
    EqpSlotMiscBonus -> tshow t
    EqpSlotAddHurtMelee -> tshow200 t <> "%"
    EqpSlotAddArmorMelee -> "[" <> tshowBlock 50 t <> "%]"
    EqpSlotAddArmorRanged -> "{" <> tshowBlock 25 t <> "%}"
    EqpSlotAddMaxHP -> tshow $ max 0 t
    EqpSlotAddSpeed -> showIntWith1 (max minSpeed t) <> "m/s"
    EqpSlotAddSight ->
      let tmax = max 0 t
          tcapped = min (fromEnum $ bcalm b `div` (5 * oneM)) tmax
      in tshowRadius tcapped
         <+> if tcapped == tmax
             then ""
             else "(max" <+> tshowRadius tmax <> ")"
    EqpSlotLightSource -> tshowRadius (max 0 t)
    EqpSlotWeapon -> tshow t
    EqpSlotMiscAbility -> tshow t
    EqpSlotAbMove -> tshow t
    EqpSlotAbMelee -> tshow t
    EqpSlotAbDisplace -> tshow t
    EqpSlotAbAlter -> tshow t
    EqpSlotAbProject -> tshow t
    EqpSlotAbApply -> tshow t
    EqpSlotAddMaxCalm -> tshow $ max 0 t
    EqpSlotAddSmell -> tshowRadius (max 0 t)
    EqpSlotAddNocto -> tshowRadius (max 0 t)
    EqpSlotAddAggression -> tshow t
    EqpSlotAbWait -> tshow t
    EqpSlotAbMoveItem -> tshow t

statSlots :: [EqpSlot]
statSlots = [ EqpSlotAddHurtMelee
            , EqpSlotAddArmorMelee
            , EqpSlotAddArmorRanged
            , EqpSlotAddMaxHP
            , EqpSlotAddMaxCalm
            , EqpSlotAddSpeed
            , EqpSlotAddSight
            , EqpSlotAddSmell
            , EqpSlotLightSource
            , EqpSlotAddNocto
-- WIP:           , EqpSlotAddAggression
            , EqpSlotAbMove
            , EqpSlotAbMelee
            , EqpSlotAbDisplace
            , EqpSlotAbAlter
            , EqpSlotAbWait
            , EqpSlotAbMoveItem
            , EqpSlotAbProject
            , EqpSlotAbApply ]

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
    AddSkill SkMove t -> wrapInParens $ affixDice t <+> "move"
    AddSkill SkMelee t -> wrapInParens $ affixDice t <+> "melee"
    AddSkill SkDisplace t -> wrapInParens $ affixDice t <+> "displace"
    AddSkill SkAlter t -> wrapInParens $ affixDice t <+> "alter"
    AddSkill SkWait t -> wrapInParens $ affixDice t <+> "wait"
    AddSkill SkMoveItem t -> wrapInParens $ affixDice t <+> "manage items"
    AddSkill SkProject t -> wrapInParens $ affixDice t <+> "fling"
    AddSkill SkApply t -> wrapInParens $ affixDice t <+> "apply"
    AddSkill SkHurtMelee _ ->
      ""  -- printed together with dice, even if dice is zero
    AddSkill SkArmorMelee t -> "[" <> affixDice t <> "%]"
    AddSkill SkArmorRanged t -> "{" <> affixDice t <> "%}"
    AddSkill SkMaxHP t -> wrapInParens $ affixDice t <+> "HP"
    AddSkill SkMaxCalm t -> wrapInParens $ affixDice t <+> "Calm"
    AddSkill SkSpeed t -> wrapInParens $ affixDice t <+> "speed"
    AddSkill SkSight t -> wrapInParens $ affixDice t <+> "sight"
    AddSkill SkSmell t -> wrapInParens $ affixDice t <+> "smell"
    AddSkill SkShine t -> wrapInParens $ affixDice t <+> "shine"
    AddSkill SkNocto t -> wrapInParens $ affixDice t <+> "night vision"
    AddSkill SkAggression t -> wrapInParens $ affixDice t <+> "aggression"
    SetFlag Fragile -> wrapInChevrons "fragile"
    SetFlag Lobable -> wrapInChevrons "can be lobbed"
    SetFlag Durable -> wrapInChevrons "durable"
    SetFlag Equipable -> ""
    SetFlag Meleeable -> ""
    SetFlag Precious -> ""
    SetFlag Blast -> ""
    SetFlag Unique -> ""  -- marked by capital letters in name
    SetFlag Periodic -> ""  -- printed specially
    SetFlag MinorEffects -> ""  -- cryptic override
    ELabel{} -> ""  -- too late
    ToThrow tmod -> wrapInChevrons $ tmodToSuff "flies" tmod
    HideAs{} -> ""
    Tactic tactics -> "overrides tactics to" <+> tshow tactics
    EqpSlot{} -> ""  -- used in @slotToSentence@ instead
    Odds{} -> ""

aspectToSentence :: Aspect -> Maybe Text
aspectToSentence feat =
  case feat of
    Timeout{} -> Nothing
    AddSkill{} -> Nothing
    SetFlag Fragile -> Nothing
    SetFlag Lobable -> Nothing
    SetFlag Durable -> Nothing
    SetFlag Equipable -> Nothing
    SetFlag Meleeable ->
      Just "It is considered for melee strikes by default."
    SetFlag Precious -> Just "It seems precious."
    SetFlag Blast -> Nothing
    SetFlag Unique -> Nothing
    SetFlag Periodic -> Nothing
    SetFlag MinorEffects -> Nothing
    ELabel{} -> Nothing
    ToThrow{} -> Nothing
    HideAs{} -> Nothing
    Tactic{}  -> Nothing
    EqpSlot es -> Just $ slotToSentence es
    Odds{} -> Just "Individual specimens sometimes have yet other properties."

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
