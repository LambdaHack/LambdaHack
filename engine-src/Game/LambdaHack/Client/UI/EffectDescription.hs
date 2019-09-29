-- | Description of effects.
module Game.LambdaHack.Client.UI.EffectDescription
  ( DetailLevel(..), effectToSuffix, detectToObject, detectToVerb
  , skillName, skillDesc, skillToDecorator, skillSlots
  , kindAspectToSuffix, aspectToSentence, affixDice
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , slotToSentence, tmodToSuff, affixBonus, wrapInParens, wrapInChevrons
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Definition.Ability
import           Game.LambdaHack.Definition.Defs

data DetailLevel =
    DetailNone | DetailLow | DetailMedium | DetailHigh | DetailAll
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
                            <+> if Dice.supDice d > 1 then "burns" else "burn")
    Explode t -> "of" <+> fromGroupName t <+> "explosion"
    RefillHP p | p > 0 -> "of healing" <+> wrapInParens (affixBonus p)
    RefillHP 0 -> error $ "" `showFailure` effect
    RefillHP p -> "of wounding" <+> wrapInParens (affixBonus p)
    RefillCalm p | p > 0 -> "of soothing" <+> wrapInParens (affixBonus p)
    RefillCalm 0 -> error $ "" `showFailure` effect
    RefillCalm p -> "of dismaying" <+> wrapInParens (affixBonus p)
    Dominate -> "of domination"
    Impress -> "of impression"
    PutToSleep -> "of sleep"
    Yell -> "of alarm"  -- minor, but if under timeout, differentiates items
    Summon grp d -> makePhrase
      [ "of summoning"
      , if Dice.supDice d <= 1 then "" else MU.Text $ tshow d
      , MU.Ws $ MU.Text $ fromGroupName grp ]
    ApplyPerfume -> "of smell removal"
    Ascend True -> "of ascending"
    Ascend False -> "of descending"
    Escape{} -> "of escaping"
    Paralyze dice ->
      let time = case Dice.reduceDice dice of
            Nothing -> tshow dice <+> "* 0.05s"
            Just p ->
              let dt = timeDeltaScale (Delta timeClip) p
              in timeDeltaInSecondsText dt
      in "of paralysis for" <+> time
    ParalyzeInWater dice ->
      let time = case Dice.reduceDice dice of
            Nothing -> tshow dice <+> "* 0.05s"
            Just p ->
              let dt = timeDeltaScale (Delta timeClip) p
              in timeDeltaInSecondsText dt
      in "of retardation for" <+> time
    InsertMove dice ->
      let moves = case Dice.reduceDice dice of
            Nothing -> tshow dice <+> "tenths of a move"
            Just p ->
              let (d, m) = p `divMod` 10
              in if m == 0
                 then makePhrase [MU.CarWs d "move"]
                 else makePhrase [MU.Car1Ws p "tenth", "of a move"]
      in "of speed surge for" <+> moves
    Teleport dice | Dice.supDice dice <= 9 ->
      "of blinking" <+> wrapInParens (tshow dice)
    Teleport dice -> "of teleport" <+> wrapInParens (tshow dice)
    CreateItem COrgan grp tim ->
      let stime = if isTimerNone tim then "" else "for" <+> tshow tim <> ":"
      in "(keep" <+> stime <+> fromGroupName grp <> ")"
    CreateItem{} -> "of gain"
    DestroyItem{} -> "of loss"
    DropItem n k store grp ->
      let (preT, postT) =
            if | n == 1 && k == maxBound -> ("one", "kind")
               | n == maxBound && k == maxBound -> ("all", "kinds")
               | k == 1 || store /= COrgan -> ("", "")
               | k == maxBound -> ("", "condition fully")
               | otherwise -> ("", "condition" <+> tshow k <> "-fold")
          (verb, fromStore) =
            if store == COrgan
            then ("nullify", "")
            else ("drop", "from" <+> snd (ppCStore store))
      in "of" <+> verb <+> preT <+> fromGroupName grp <+> postT <+> fromStore
    PolyItem -> "of repurpose on the ground"
    RerollItem -> "of deeply reshape on the ground"
    DupItem -> "of multiplication on the ground"
    Identify -> "of identify"
    Detect d radius ->
      "of" <+> detectToObject d <+> "location" <+> wrapInParens (tshow radius)
    SendFlying tmod -> "of impact" <+> tmodToSuff "" tmod
    PushActor tmod -> "of pushing" <+> tmodToSuff "" tmod
    PullActor tmod -> "of pulling" <+> tmodToSuff "" tmod
    DropBestWeapon -> "of disarming"
    OneOf l ->
      let subject = if length l <= 5 then "marvel" else "wonder"
          header = makePhrase ["of", MU.CardinalWs (length l) subject]
          marvels = T.intercalate ", " $ map (effectToSuffix detailLevel) l
      in if detailLevel >= DetailAll && marvels /= ""
         then header <+> "[" <> marvels <> "]"
         else header  -- of no wonders :)
    OnSmash _ -> ""  -- printed inside a separate section
    OnCombine _ -> ""  -- printed inside a separate section
    VerbNoLonger _ -> ""  -- no description for a flavour effect
    VerbMsg _ -> ""  -- no description for an effect that prints a description
    AndEffect eff1 eff2 ->
      T.intercalate " and then "
      $ filter (/= "") $ map (effectToSuffix detailLevel) [eff1, eff2]
    OrEffect eff1 eff2 ->
      T.intercalate " or else "
      $ filter (/= "") $ map (effectToSuffix detailLevel) [eff1, eff2]

detectToObject :: DetectKind -> Text
detectToObject d = case d of
  DetectAll -> "detail"
  DetectActor -> "intruder"
  DetectLoot -> "merchandise"
  DetectExit -> "exit"
  DetectHidden -> "secret"
  DetectEmbed -> "feature"

detectToVerb :: DetectKind -> Text
detectToVerb d = case d of
  DetectAll -> "map all"
  DetectActor -> "spot nearby"
  DetectLoot -> "locate nearby"
  DetectExit -> "learn nearby"
  DetectHidden -> "uncover nearby"
  DetectEmbed -> "notice nearby"

slotToSentence :: EqpSlot -> Text
slotToSentence es = case es of
  EqpSlotMove -> "Those unskilled in locomotion equip it."
  EqpSlotMelee -> "Those unskilled in close combat equip it."
  EqpSlotDisplace -> "Those unskilled in moving in crowds equip it."
  EqpSlotAlter -> "Those unskilled in terrain modification equip it."
  EqpSlotWait -> "Those unskilled in watchfulness equip it."
  EqpSlotMoveItem -> "Those unskilled in inventory management equip it."
  EqpSlotProject -> "Those unskilled in item flinging equip it."
  EqpSlotApply -> "Those unskilled in applying items equip it."
  EqpSlotSwimming -> "Useful to any that wade or swim in water."
  EqpSlotFlying -> "Those not afraid to fly, put it on."
  EqpSlotHurtMelee -> "Veteran melee fighters are known to devote equipment slot to it."
  EqpSlotArmorMelee -> "Worn by people in risk of melee wounds."
  EqpSlotArmorRanged -> "People scared of shots in the dark wear it."
  EqpSlotMaxHP -> "The frail wear it to increase their Hit Point capacity."
  EqpSlotSpeed -> "The sluggish equip it to speed up their whole life."
  EqpSlotSight -> "The short-sighted wear it to notice their demise sooner."
  EqpSlotShine -> "Explorers brave enough to highlight themselves put it in their equipment."
  EqpSlotMiscBonus -> "Those that don't scorn minor bonuses may equip it."
  EqpSlotWeaponFast -> "Close range fighters pick it as their mainstay weapon."
  EqpSlotWeaponBig -> "Close range fighters pick it as their opening weapon."

skillName :: Skill -> Text
skillName SkMove = "move stat"
skillName SkMelee = "melee stat"
skillName SkDisplace = "displace stat"
skillName SkAlter = "modify tile stat"
skillName SkWait = "wait stat"
skillName SkMoveItem = "manage items stat"
skillName SkProject = "fling stat"
skillName SkApply = "apply stat"
skillName SkSwimming = "swimming"
skillName SkFlying = "flying"
skillName SkHurtMelee = "to melee damage"
skillName SkArmorMelee = "melee armor"
skillName SkArmorRanged = "ranged armor"
skillName SkMaxHP = "max HP"
skillName SkMaxCalm = "max Calm"
skillName SkSpeed = "speed"
skillName SkSight = "sight radius"
skillName SkSmell = "smell radius"
skillName SkShine = "shine radius"
skillName SkNocto = "noctovision radius"
skillName SkHearing = "hearing radius"
skillName SkAggression = "aggression level"
skillName SkOdor = "odor level"

skillDesc :: Skill -> Text
skillDesc skill =
  let skName = skillName skill
      capSkillName = "The '" <> skName <> "' skill"
      capStatName = "The '" <> T.unwords (init $ T.words skName) <> "' stat"
  in case skill of
    SkMove -> capStatName <+>
      "determines whether the character can move. Actors not capable of movement can't be dominated."
    SkMelee -> capStatName <+>
      "determines whether the character can melee. Actors that can't melee can still cause damage by flinging missiles or by ramming (being pushed) at opponents."
    SkDisplace -> capStatName <+>
      "determines whether the character can displace adjacent actors. In some cases displacing is not possible regardless of skill: when the target is braced, dying, has no move skill or when both actors are supported by adjacent friendly units. Missiles can be displaced always, unless more than one occupies the map location."
    SkAlter -> capStatName <+>
      "determines which kinds of terrain can be modified or triggered by the character. Opening doors and searching suspect tiles require skill 2, some stairs require 3, closing doors requires 4, others require 4 or 5. Actors not smart enough to be capable of using stairs can't be dominated."
    SkWait -> capStatName <+>
      "determines whether the character can wait, brace for combat (potentially blocking the effects of some attacks), sleep and lurk."
    SkMoveItem -> capStatName <+>
      "determines whether the character can pick up items and manage inventory."
    SkProject -> capStatName <+>
      "determines which kinds of items the character can propel. Items that can be lobbed to explode at a precise location, such as flasks, require skill 3. Other items travel until they meet an obstacle and skill 1 is enough to fling them. In some cases, e.g., of too intricate or two awkward items at low Calm, throwing is not possible regardless of the skill value."
    SkApply -> capStatName <+>
      "determines which kinds of items the character can activate. Items that assume literacy require skill 2, others can be used already at skill 1. In some cases, e.g., when the item needs recharging, has no possible effects or is too intricate for the character Calm level, applying may not be possible."
    SkSwimming -> capSkillName <+>
      "is the degree of avoidance of bad effects of terrain containing water, whether shallow or deep."
    SkFlying -> capSkillName <+>
      "is the degree of avoidance of bad effects of any hazards spread on the ground."
    SkHurtMelee -> capSkillName <+>
      "is a percentage of additional damage dealt by the actor (either a character or a missile) with any weapon. The value is capped at 200%, then the armor percentage of the defender is subtracted from it and the resulting total is capped at 99%."
    SkArmorMelee -> capSkillName <+>
      "is a percentage of melee damage avoided by the actor. The value is capped at 200%, then the extra melee damage percentage of the attacker is subtracted from it and the resulting total is capped at 99% (always at least 1% of damage gets through). It includes 50% bonus from being braced for combat, if applicable."
    SkArmorRanged -> capSkillName <+>
      "is a percentage of ranged damage avoided by the actor. The value is capped at 200%, then the extra melee damage percentage of the attacker is subtracted from it and the resulting total is capped at 99% (always at least 1% of damage gets through). It includes 25% bonus from being braced for combat, if applicable."
    SkMaxHP -> capSkillName <+>
      "is a cap on HP of the actor, except for some rare effects able to overfill HP. At any direct enemy damage (but not, e.g., incremental poisoning damage or wounds inflicted by mishandling a device) HP is cut back to the cap."
    SkMaxCalm -> capSkillName <+>
      "is a cap on Calm of the actor, except for some rare effects able to overfill Calm. At any direct enemy damage (but not, e.g., incremental poisoning damage or wounds inflicted by mishandling a device) Calm is lowered, sometimes very significantly and always at least back down to the cap."
    SkSpeed -> capSkillName <+>
      "is expressed in meters per second, which corresponds to map location (1m by 1m) per two standard turns (0.5s each). Thus actor at standard speed of 2m/s moves one location per standard turn."
    SkSight -> capSkillName <+>
      "is the limit of visibility in light. The radius is measured from the middle of the map location occupied by the character to the edge of the furthest covered location."
    SkSmell -> capSkillName <+>
      "determines the maximal area smelled by the actor. The radius is measured from the middle of the map location occupied by the character to the edge of the furthest covered location."
    SkShine -> capSkillName <+>
      "determines the maximal area lit by the actor. The radius is measured from the middle of the map location occupied by the character to the edge of the furthest covered location."
    SkNocto -> capSkillName <+>
      "is the limit of visibility in dark. The radius is measured from the middle of the map location occupied by the character to the edge of the furthest covered location."
    SkHearing -> capSkillName <+>
      "is the limit of hearing. The radius is measured from the middle of the map location occupied by the character to the edge of the furthest covered location."
    SkAggression -> "The '" <> skName <> "' property" <+>
      "represents the willingness of the actor to engage in combat, especially close quarters, and conversely, to break engagement when overpowered."
    SkOdor -> "The '" <> skName <> "' property" <+>
      "represents the ability to communicate (more specifically, communicate one's presence) through personal odor. Zero or less means the odor is not trackable."

skillToDecorator :: Skill -> Actor -> Int -> Text
skillToDecorator skill b t =
  let tshow200 n = let n200 = min 200 $ max (-200) n
                   in tshow n200 <> if n200 /= n then "$" else ""
      -- Some values can be negative, for others 0 is equivalent but shorter.
      tshowRadius r = case compare r 0 of
                        GT -> tshow (r - 1) <> ".5m"
                        EQ -> "0m"
                        LT -> tshow (r + 1) <> ".5m"
  in case skill of
    SkMove -> tshow t
    SkMelee -> tshow t
    SkDisplace -> tshow t
    SkAlter -> tshow t
    SkWait -> tshow t
    SkMoveItem -> tshow t
    SkProject -> tshow t
    SkApply -> tshow t
    SkSwimming -> tshow t
    SkFlying -> tshow t
    SkHurtMelee -> tshow200 t <> "%"
    SkArmorMelee -> "[" <> tshow200 t <> "%]"
    SkArmorRanged -> "{" <> tshow200 t <> "%}"
    SkMaxHP -> tshow $ max 0 t
    SkMaxCalm -> tshow $ max 0 t
    SkSpeed -> T.pack $ displaySpeed t
    SkSight ->
      let tcapped = min (fromEnum $ bcalm b `div` xM 5) t
      in tshowRadius tcapped
         <+> if tcapped == t
             then ""
             else "(max" <+> tshowRadius t <> ")"
    SkSmell -> tshowRadius t
    SkShine -> tshowRadius t
    SkNocto -> tshowRadius t
    SkHearing -> tshowRadius t
    SkAggression -> tshow t
    SkOdor -> tshow t

skillSlots :: [Skill]
skillSlots = [minBound .. maxBound]

tmodToSuff :: Text -> ThrowMod -> Text
tmodToSuff verb ThrowMod{..} =
  let vSuff | throwVelocity == 100 = ""
            | otherwise = "v=" <> tshow throwVelocity <> "%"
      tSuff | throwLinger == 100 = ""
            | otherwise = "t=" <> tshow throwLinger <> "%"
      hSuff | throwHP == 1 = ""
            | otherwise = "pierce=" <> tshow throwHP
  in if vSuff == "" && tSuff == "" && hSuff == "" then ""
     else verb <+> "with" <+> vSuff <+> tSuff <+> hSuff

kindAspectToSuffix :: Aspect -> Text
kindAspectToSuffix aspect =
  case aspect of
    Timeout{} -> ""  -- printed specially
    AddSkill SkMove t -> wrapInParens $ affixDice t <+> "move"
    AddSkill SkMelee t -> wrapInParens $ affixDice t <+> "melee"
    AddSkill SkDisplace t -> wrapInParens $ affixDice t <+> "displace"
    AddSkill SkAlter t -> wrapInParens $ affixDice t <+> "modify"
    AddSkill SkWait t -> wrapInParens $ affixDice t <+> "wait"
    AddSkill SkMoveItem t -> wrapInParens $ affixDice t <+> "manage items"
    AddSkill SkProject t -> wrapInParens $ affixDice t <+> "fling"
    AddSkill SkApply t -> wrapInParens $ affixDice t <+> "apply"
    AddSkill SkSwimming t -> wrapInParens $ affixDice t <+> "swimming"
    AddSkill SkFlying t -> wrapInParens $ affixDice t <+> "flying"
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
    AddSkill SkHearing t -> wrapInParens $ affixDice t <+> "hearing"
    AddSkill SkAggression t -> wrapInParens $ affixDice t <+> "aggression"
    AddSkill SkOdor t -> wrapInParens $ affixDice t <+> "odor"
    SetFlag Fragile -> wrapInChevrons "fragile"
    SetFlag Lobable -> wrapInChevrons "can be lobbed"
    SetFlag Durable -> wrapInChevrons "durable"
    SetFlag Equipable -> ""
    SetFlag Meleeable -> ""
    SetFlag Benign -> ""
    SetFlag Precious -> ""
    SetFlag Blast -> ""
    SetFlag Condition -> ""
    SetFlag Unique -> ""  -- marked by capital letters in name
    SetFlag Periodic -> ""  -- printed specially
    SetFlag MinorEffects -> ""  -- cryptic override
    ELabel{} -> ""  -- too late
    ToThrow tmod -> wrapInChevrons $ tmodToSuff "flies" tmod
    HideAs{} -> ""
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
    SetFlag Benign ->
      Just "It affects the opponent in a benign way."
    SetFlag Precious -> Just "It seems precious."
    SetFlag Blast -> Nothing
    SetFlag Condition -> Nothing
    SetFlag Unique -> Nothing
    SetFlag Periodic -> Nothing
    SetFlag MinorEffects -> Nothing
    ELabel{} -> Nothing
    ToThrow{} -> Nothing
    HideAs{} -> Nothing
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
