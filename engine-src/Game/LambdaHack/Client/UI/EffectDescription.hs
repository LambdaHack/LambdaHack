{-# LANGUAGE DeriveGeneric #-}
-- | Description of effects.
module Game.LambdaHack.Client.UI.EffectDescription
  ( DetailLevel(..), defaultDetailLevel
  , effectToSuffix, detectToObject, detectToVerb
  , skillName, skillDesc, skillToDecorator, skillSlots
  , kindAspectToSuffix, aspectToSentence, affixDice
  , describeToolsAlternative, describeCrafting, wrapInParens
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , conditionToObject, activationFlagToObject, slotToSentence, tmodToSuff
  , affixBonus, wrapInChevrons
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Definition.Ability
import           Game.LambdaHack.Definition.Defs

data DetailLevel = DetailLow | DetailMedium | DetailHigh | DetailAll
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Binary DetailLevel

defaultDetailLevel :: DetailLevel
defaultDetailLevel = DetailAll  -- TODO: take from config file

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
    Explode t -> "of" <+> displayGroupName t <+> "explosion"
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
      , MU.Ws $ MU.Text $ displayGroupName grp ]
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
    CreateItem _ COrgan grp tim ->
      let stime = if isTimerNone tim then "" else "for" <+> tshow tim <> ":"
      in "(keep" <+> stime <+> displayGroupName grp <> ")"
    CreateItem _ _ grp _ ->
      makePhrase ["of gain", MU.AW $ MU.Text $ displayGroupName grp]
    DestroyItem{} -> "of loss"
    ConsumeItems{} -> "of consumption from the ground"
      -- too much noise from crafting
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
      in "of" <+> verb <+> preT <+> displayGroupName grp <+> postT <+> fromStore
    Recharge n dice ->
      let times = if n == 1 then "" else tshow n <+> "times"
      in case Dice.reduceDice dice of
        Nothing -> "of recharge" <+> times
                   <+> "by" <+> tshow dice <+> "* 0.05s"
        Just p -> let dt = timeDeltaScale (Delta timeClip) p
                  in "of recharge" <+> times
                     <+> "by" <+> timeDeltaInSecondsText dt
    Discharge n dice ->
      let times = if n == 1 then "" else tshow n <+> "times"
      in case Dice.reduceDice dice of
        Nothing -> "of discharge" <+> times
                   <+> "by" <+> tshow dice <+> "* 0.05s"
        Just p -> let dt = timeDeltaScale (Delta timeClip) p
                  in "of discharge" <+> times
                     <+> "by" <+> timeDeltaInSecondsText dt
    PolyItem -> "of repurpose on the ground"
    RerollItem -> "of deeply reshape on the ground"
    DupItem -> "of multiplication on the ground"
    Identify -> "of identify"
    Detect d radius ->
      "of" <+> detectToObject d <+> "location" <+> wrapInParens (tshow radius)
    SendFlying tmod -> "of impact" <+> tmodToSuff "" tmod
    PushActor tmod -> "of pushing" <+> tmodToSuff "" tmod
    PullActor tmod -> "of pulling" <+> tmodToSuff "" tmod
    AtMostOneOf effs ->
      let ts = filter (/= "") $ map (effectToSuffix detailLevel) effs
          subject = "marvel"
          header = makePhrase ["of", MU.CardinalWs (length ts) subject]
          sometimes = if length effs > length ts then "(sometimes)" else ""
      in case ts of
        [] -> ""
        [wonder] -> wonder <+> sometimes
        _ | detailLevel < DetailAll -> header
        _ -> header <+> "[" <> T.intercalate ", " ts <> "]" <+> sometimes
    OneOf effs ->
      let ts = filter (/= "") $ map (effectToSuffix detailLevel) effs
          subject = "wonder"
          header = makePhrase ["of", MU.CardinalWs (length ts) subject]
          sometimes = if length effs > length ts then "(sometimes)" else ""
      in case ts of
        [] -> ""
        [wonder] -> wonder <+> sometimes
        _ | detailLevel < DetailAll -> header
        _ -> header <+> "[" <> T.intercalate ", " ts <> "]" <+> sometimes
    OnSmash _ -> ""  -- printed inside a separate section
    OnCombine _ -> ""  -- printed inside a separate section
    OnUser eff -> let t = effectToSuffix detailLevel eff
                  in if T.null t then "" else "(on user:" <+> t <> ")"
    NopEffect -> ""  -- never printed
    AndEffect (ConsumeItems tools raw) eff -> case detailLevel of
      DetailAll ->
       let (tcraft, traw, ttools) = describeCrafting tools raw eff
       in tcraft <+> traw <+> ttools
      DetailHigh -> "of crafting (recipes in lore menu)"
      _ -> "of crafting"
    AndEffect eff1 eff2 ->
      let t = T.intercalate " and then "
              $ nub $ filter (not . T.null)
              $ map (effectToSuffix detailLevel) [eff1, eff2]
      in if T.null t then "of conjunctive processing" else t
    OrEffect eff1 eff2 ->
      let t = T.intercalate " or else "
              $ nub $ filter (not . T.null)
              $ map (effectToSuffix detailLevel) [eff1, eff2]
      in if T.null t then "of alternative processing" else t
    SeqEffect effs ->
      let t = T.intercalate " then "
              $ nub $ filter (not . T.null)
              $ map (effectToSuffix detailLevel) effs
      in if T.null t then "of sequential processing" else t
    When cond eff ->
      let object = conditionToObject cond
          object2 = effectToSuffix detailLevel eff
      in if T.null object2
         then ""  -- no 'conditional processing' --- probably a hack
         else "when" <+> object <+> "then" <+> object2
    Unless cond eff ->
      let object = conditionToObject cond
          object2 = effectToSuffix detailLevel eff
      in if T.null object2
         then ""
         else "unless" <+> object <+> "then" <+> object2
    IfThenElse cond eff1 eff2 ->
      let object = conditionToObject cond
          object1 = effectToSuffix detailLevel eff1
          object2 = effectToSuffix detailLevel eff2
      in if T.null object1 && T.null object2
         then ""
         else "if" <+> object <+> "then" <+> object1 <+> "else" <+> object2
    VerbNoLonger{} -> ""  -- no description for a flavour effect
    VerbMsg{} -> ""  -- no description for an effect that prints a description
    VerbMsgFail{} -> ""

conditionToObject :: Condition -> Text
conditionToObject = \case
  HpLeq n -> "HP <=" <+> tshow n
  HpGeq n -> "HP >=" <+> tshow n
  CalmLeq n -> "Calm <=" <+> tshow n
  CalmGeq n -> "Calm >=" <+> tshow n
  TriggeredBy activationFlag ->
    "activated" <+> activationFlagToObject activationFlag

activationFlagToObject :: ActivationFlag -> Text
activationFlagToObject = \case
  ActivationMeleeable -> "by meleeing"
  ActivationPeriodic -> "periodically"
  ActivationUnderRanged -> "under ranged attack"
  ActivationUnderMelee -> "under melee attack"
  ActivationProjectile -> "when flung"
  ActivationTrigger -> "by triggering"
  ActivationOnSmash -> "on smash"
  ActivationOnCombine -> "when combined"
  ActivationEmbed -> "embedded in terrain"
  ActivationConsume -> "when consumed"

detectToObject :: DetectKind -> Text
detectToObject d = case d of
  DetectAll -> "detail"
  DetectActor -> "intruder"
  DetectLoot -> "merchandise"
  DetectExit -> "exit"
  DetectHidden -> "secret"
  DetectEmbed -> "feature"
  DetectStash -> "stash"

detectToVerb :: DetectKind -> Text
detectToVerb d = case d of
  DetectAll -> "map all"
  DetectActor -> "spot nearby"
  DetectLoot -> "locate nearby"
  DetectExit -> "learn nearby"
  DetectHidden -> "uncover nearby"
  DetectEmbed -> "notice nearby"
  DetectStash -> "locate"

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
skillName SkAlter = "modify terrain stat"
skillName SkWait = "wait stat"
skillName SkMoveItem = "manage items stat"
skillName SkProject = "fling stat"
skillName SkApply = "trigger stat"
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
skillName SkDeflectRanged = "ranged deflection"
skillName SkDeflectMelee = "melee deflection"

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
      "determines which kinds of terrain can be activated and modified by the character. Opening doors and searching suspect tiles require skill 2, some stairs require 3, closing doors requires 4, others require 4 or 5. Actors not smart enough to be capable of using stairs can't be dominated."
    SkWait -> capStatName <+>
      "determines whether the character can wait, brace for combat (potentially blocking the effects of some attacks), sleep and lurk."
    SkMoveItem -> capStatName <+>
      "determines whether the character can pick up items and manage inventory."
    SkProject -> capStatName <+>
      "determines which kinds of items the character can propel. Items that can be lobbed to explode at a precise location, such as flasks, require skill 3. Other items travel until they meet an obstacle and skill 1 is enough to fling them. In some cases, e.g., of too intricate or two awkward items at low Calm, throwing is not possible regardless of the skill value."
    SkApply -> capStatName <+>
      "determines which kinds of items the character can use. Items that assume literacy require skill 2, others can be used already at skill 1. In some cases, e.g., when the item needs recharging, has no possible effects or is too intricate for distracted use, triggering may not be possible."
    SkSwimming -> capSkillName <+>
      "is the degree of avoidance of bad effects of terrain containing water, whether shallow or deep."
    SkFlying -> capSkillName <+>
      "is the degree of avoidance of bad effects of any hazards spread on the ground."
    SkHurtMelee -> capSkillName <+>
      "is a percentage of additional damage dealt by the actor (either a character or a missile) with any weapon. The value is capped at 200% and then the armor percentage of the defender is subtracted from it."
    SkArmorMelee -> capSkillName <+>
      "is a percentage of melee damage avoided by the actor. The value is capped at 200%, then the extra melee damage percentage of the attacker is subtracted from it and the resulting total is capped at 95% (always at least 5% of damage gets through). It includes 50% bonus from being braced for combat, if applicable."
    SkArmorRanged -> capSkillName <+>
      "is a percentage of ranged damage avoided by the actor. The value is capped at 200%, then the extra melee damage percentage of the attacker is subtracted from it and the resulting total is capped at 95% (always at least 5% of damage gets through). It includes 25% bonus from being braced for combat, if applicable."
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
    SkDeflectRanged -> "The '" <> skName <> "' property" <+>
      "tells whether complete invulnerability to ranged attacks, piercing and of every other kind, is effective, and from how many sources."
    SkDeflectMelee -> "The '" <> skName <> "' property" <+>
      "tells whether complete invulnerability to melee attacks, piercing and of every other kind, is effective, and from how many sources."

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
    SkDeflectRanged -> tshow t
    SkDeflectMelee -> tshow t

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
    AddSkill SkApply t -> wrapInParens $ affixDice t <+> "trigger"
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
    AddSkill SkDeflectRanged d ->
      if | Dice.infDice d >= 1 -> wrapInChevrons "deflecting ranged attacks"
         | Dice.supDice d <= -1 -> wrapInChevrons "vulnerable to ranged attacks"
         | otherwise -> ""  -- bad content?
    AddSkill SkDeflectMelee d ->
      if | Dice.infDice d >= 1 -> wrapInChevrons "deflecting melee attacks"
         | Dice.supDice d <= -1 -> wrapInChevrons "vulnerable to melee attacks"
         | otherwise -> ""  -- bad content?
    SetFlag Fragile -> wrapInChevrons "fragile"
    SetFlag Lobable -> wrapInChevrons "can be lobbed"
    SetFlag Durable -> wrapInChevrons "durable"
    SetFlag Equipable -> ""
    SetFlag Benign -> ""
    SetFlag Precious -> ""
    SetFlag Blast -> ""
    SetFlag Condition -> ""
    SetFlag Unique -> ""  -- named specially by the content writer
    SetFlag MetaGame -> ""
    SetFlag MinorEffects -> ""  -- cryptic override
    SetFlag MinorAspects -> ""  -- cryptic override
    SetFlag Meleeable -> ""
    SetFlag Periodic -> ""  -- printed specially
    SetFlag UnderRanged -> wrapInChevrons "applied under ranged attack"
    SetFlag UnderMelee -> wrapInChevrons "applied under melee attack"
    ELabel{} -> ""  -- too late
    ToThrow tmod -> wrapInChevrons $ tmodToSuff "flies" tmod
    PresentAs{} -> ""
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
    SetFlag Benign -> Just "It affects the opponent in a benign way."
    SetFlag Precious -> Just "It seems precious."
    SetFlag Blast -> Nothing
    SetFlag Condition -> Nothing
    SetFlag Unique -> Just "It is one of a kind."
    SetFlag MetaGame -> Just "It's characteristic to a person and easy to recognize once learned, even under very different circumstances."
    SetFlag MinorEffects -> Nothing
    SetFlag MinorAspects -> Nothing
    SetFlag Meleeable -> Just "It is considered for melee strikes."
    SetFlag Periodic -> Nothing
    SetFlag UnderRanged -> Nothing
    SetFlag UnderMelee -> Nothing
    ELabel{} -> Nothing
    ToThrow{} -> Nothing
    PresentAs{} -> Nothing
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

describeTools :: [(Int, GroupName ItemKind)] -> MU.Part
describeTools =
  let carAWs (k, grp) = MU.CarAWs k (MU.Text $ displayGroupName grp)
  in MU.WWandW . map carAWs

describeToolsAlternative :: [[(Int, GroupName ItemKind)]] -> Text
describeToolsAlternative grps =
  T.intercalate " or " $ map (\grp -> makePhrase [describeTools grp])
                       $ filter (not . null) grps

describeCrafting :: [(Int, GroupName ItemKind)]
                 -> [(Int, GroupName ItemKind)]
                 -> Effect
                 -> (Text, Text, Text)
describeCrafting tools raw eff =
  let unCreate (CreateItem (Just k) _ grp _) = [(k, grp)]
      unCreate (SeqEffect effs) = concatMap unCreate effs
      unCreate _ = []
      grpsCreate = unCreate eff
      tcraft = makePhrase $
        "of crafting"
        : (if null grpsCreate
           then ["nothing"]
           else [describeTools grpsCreate])
      traw = makePhrase $
        if null raw
        then []
        else ["from", describeTools raw]
      ttools = makePhrase $
        if null tools
        then []
        else ["using", describeTools tools]
  in (tcraft, traw, ttools)
