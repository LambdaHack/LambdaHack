-- | Descriptions of items.
module Game.LambdaHack.Client.UI.ItemDescription
  ( partItem, partItemShort, partItemShortest, partItemTrunk, partItemHigh
  , partItemWs, partItemWsShort, partItemWsRanged
  , partItemShortAW, partItemMediumAW, partItemShortWownW
  , viewItem, itemDesc
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , partItemN, textAllPowers, partItemWsR
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

-- | The part of speech describing the item parameterized by the number
-- of effects/aspects to show.
partItemN :: FactionId -> FactionDict -> Bool -> DetailLevel -> Int
          -> Time -> ItemFull -> ItemQuant
          -> (MU.Part, MU.Part)
partItemN side factionD ranged detailLevel maxWordsToShow localTime
          itemFull@ItemFull{itemBase, itemKind, itemSuspect}
          (itemK, itemTimer) =
  let flav = flavourToName $ jflavour itemBase
      arItem = aspectRecordFull itemFull
      timeout = IA.aTimeout arItem
      temporary = IA.checkFlag Ability.Fragile arItem
                  && IA.checkFlag Ability.Periodic arItem
      lenCh = itemK - ncharges localTime itemFull (itemK, itemTimer)
      charges | lenCh == 0 || temporary = ""
              | itemK == 1 && lenCh == 1 = "(charging)"
              | itemK == lenCh = "(all charging)"
              | otherwise = "(" <> tshow lenCh <+> "charging)"
      skipRecharging = detailLevel <= DetailLow && lenCh >= itemK
      (powerTsRaw, rangedDamage) =
        textAllPowers detailLevel skipRecharging itemFull
      powerTs = powerTsRaw ++ if ranged then rangedDamage else []
      lsource = case jfid itemBase of
        Just fid | IK.iname itemKind `elem` ["impressed"] ->
          ["by" <+> if fid == side
                    then "us"
                    else gname (factionD EM.! fid)]
        _ -> []
      ts = lsource
           ++ take maxWordsToShow powerTs
           ++ [ "(...)" | length powerTs > maxWordsToShow
                          && detailLevel > DetailLow
                          && maxWordsToShow > 0 ]
           ++ [charges | maxWordsToShow > 1]
      name | temporary =
             let adj = if timeout == 0 then "temporarily" else "impermanent"
             in adj <+> IK.iname itemKind
           | itemSuspect = flav <+> IK.iname itemKind
           | otherwise = IK.iname itemKind
      capName = if IA.checkFlag Ability.Unique arItem
                then MU.Capitalize $ MU.Text name
                else MU.Text name
  in (capName, if maxWordsToShow == 0
               then MU.Text $ IA.aELabel arItem
               else MU.Phrase $ map MU.Text ts)

-- TODO: simplify the code a lot
textAllPowers :: DetailLevel -> Bool -> ItemFull -> ([Text], [Text])
textAllPowers detailLevel skipRecharging
              itemFull@ItemFull{itemKind, itemDisco} =
  let arItem = aspectRecordFull itemFull
      -- To handle both the cases of item identified and not, we represent
      -- aspects as a list with dice, not a record of integers as in @arItem@.
      -- If item fully known, the dice will be trivial and will display
      -- the same as integers would, so nothing is lost.
      -- If item not known fully and timeouts or any crucial flags
      -- are under @Odds@, they are ignored, so they should be avoided
      -- under @Odds@ in not fully-identified items.
      aspectsFull = case itemDisco of
        ItemDiscoMean IA.KindMean{..} | kmConst ->
          IA.aspectRecordToList kmMean  -- exact and collated
        ItemDiscoMean{} -> IK.iaspects itemKind
          -- doesn't completely lose the @Odds@ case, so better than
          -- the above, even if does not collate multiple skill bonuses
        ItemDiscoFull iAspect -> IA.aspectRecordToList iAspect
      mtimeout = find IK.timeoutAspect aspectsFull
      elab = IA.aELabel arItem
      periodic = IA.checkFlag Ability.Periodic arItem
      hurtMeleeAspect :: IK.Aspect -> Bool
      hurtMeleeAspect (IK.AddSkill Ability.SkHurtMelee _) = True
      hurtMeleeAspect _ = False
      active = IA.goesIntoEqp arItem
      splitA :: DetailLevel -> [IK.Aspect] -> [Text]
      splitA detLev aspects =
        let ppA = kindAspectToSuffix
            ppE = effectToSuffix detLev
            reduce_a = maybe "?" tshow . Dice.reduceDice
            restEs | detLev >= DetailHigh
                     || not (IA.checkFlag Ability.MinorEffects arItem) =
                     IK.ieffects itemKind
                   | otherwise = []
            (smashEffs, noSmashEffs) = partition IK.onSmashEffect restEs
            unSmash (IK.OnSmash eff) = eff
            unSmash eff = eff
            onSmashTs = T.intercalate " " $ filter (not . T.null)
                        $ map (ppE . unSmash) smashEffs
            (combineEffsRaw, noSmashOrCombineEffs) =
              partition IK.onCombineEffect noSmashEffs
            -- Avoid repeating long crafting recipes.
            combineEffsUn = map unCombine combineEffsRaw
            combineEffs = combineEffsUn \\ noSmashOrCombineEffs
            unCombine (IK.OnCombine eff) = eff
            unCombine eff = eff
            onCombineTs = T.intercalate " " $ filter (not . T.null)
                        $ map ppE combineEffs
            rechargingTs = T.intercalate " "
                           $ [damageText | IK.idamage itemKind /= 0]
                             ++ filter (not . T.null)
                                       (map ppE noSmashOrCombineEffs)
            fragile = IA.checkFlag Ability.Fragile arItem
            periodicText =
              if periodic && not skipRecharging && not (T.null rechargingTs)
              then case (mtimeout, fragile) of
                     (Nothing, True) ->
                       "(each turn until gone:" <+> rechargingTs <> ")"
                     (Nothing, False) ->
                       "(each turn:" <+> rechargingTs <> ")"
                         -- timeout 0, so it just fires each turn and it's not
                         -- fragile, so a copy is not destroyed each turn
                     (Just (IK.Timeout t), True) ->
                       "(every" <+> reduce_a t <+> "until gone:"
                       <+> rechargingTs <> ")"
                     (Just (IK.Timeout t), False) ->
                       "(every" <+> reduce_a t <> ":" <+> rechargingTs <> ")"
                     _ -> error $ "" `showFailure` mtimeout
              else ""
            ppERestEs = if periodic
                        then [periodicText]
                        else map ppE noSmashOrCombineEffs
            aes = if active
                  then map ppA aspects ++ ppERestEs
                  else ppERestEs ++ map ppA aspects
            onSmash = if T.null onSmashTs then ""
                      else "(on smash:" <+> onSmashTs <> ")"
            onCombine = if T.null onCombineTs
                        then if combineEffsUn /= combineEffs
                             then "(on combine: some of the above)"
                             else ""
                        else "(on combine:" <+> onCombineTs <> ")"
            -- Either exact value or dice of @SkHurtMelee@ needed,
            -- never the average, so @arItem@ not consulted directly.
            -- If item not known fully and @SkHurtMelee@ under @Odds@,
            -- it's ignored.
            damageText = case find hurtMeleeAspect aspects of
              Just (IK.AddSkill Ability.SkHurtMelee hurtMelee) ->
                (if IK.idamage itemKind == 0
                 then "0d0"
                 else tshow (IK.idamage itemKind))
                <> affixDice hurtMelee <> "%"
              _ -> if IK.idamage itemKind == 0
                   then ""
                   else tshow (IK.idamage itemKind)
            timeoutText = case mtimeout of
              Nothing -> ""
              Just (IK.Timeout t) -> "(cooldown" <+> reduce_a t <> ")"
                                       -- timeout is called "cooldown" in UI
              _ -> error $ "" `showFailure` mtimeout
       in [ damageText
          | detLev > DetailLow && (not periodic || IK.idamage itemKind == 0) ]
          ++ [timeoutText | detLev > DetailLow && not periodic]
          ++ if detLev >= DetailLow
             then aes ++ if detLev >= DetailAll
                         then [onSmash, onCombine]
                         else []
             else []
      hurtMult = armorHurtCalculation True (IA.aSkills arItem)
                                           Ability.zeroSkills
      dmg = Dice.meanDice $ IK.idamage itemKind
      rawDeltaHP = ceiling $ fromIntegral hurtMult * xD dmg / 100
      IK.ThrowMod{IK.throwVelocity} = IA.aToThrow arItem
      speed = speedFromWeight (IK.iweight itemKind) throwVelocity
      pdeltaHP = modifyDamageBySpeed rawDeltaHP speed
      rangedDamageDesc = if pdeltaHP == 0
                         then []
                         else ["{avg" <+> show64With2 pdeltaHP <+> "ranged}"]
        -- Note that avg melee damage would be too complex to display here,
        -- because in case of @MOwned@ the owner is different than leader,
        -- so the value would be different than when viewing the item.
      splitTry ass =
        let splits = map (`splitA` ass) [minBound..maxBound]
            splitsToTry = drop (fromEnum detailLevel) splits
            splitsValid | T.null elab = filter (/= []) splitsToTry
                        | otherwise = splitsToTry
        in concat $ take 1 splitsValid
      aspectDescs =
        let aMain IK.AddSkill{} = True
            aMain _ = False
            (aspectsMain, aspectsAux) = partition aMain aspectsFull
        in filter (/= "")
           $ elab
             : splitTry aspectsMain
             ++ if detailLevel >= DetailAll
                then map kindAspectToSuffix aspectsAux
                else []
  in (aspectDescs, rangedDamageDesc)

-- | The part of speech describing the item.
partItem :: FactionId -> FactionDict -> Time -> ItemFull -> ItemQuant
         -> (MU.Part, MU.Part)
partItem side factionD = partItemN side factionD False DetailMedium 4

partItemShort :: FactionId -> FactionDict -> Time -> ItemFull -> ItemQuant
              -> (MU.Part, MU.Part)
partItemShort side factionD = partItemN side factionD False DetailLow 4

partItemShortest :: FactionId -> FactionDict -> Time -> ItemFull -> ItemQuant
                 -> (MU.Part, MU.Part)
partItemShortest side factionD = partItemN side factionD False DetailLow 1

partItemTrunk :: FactionId -> FactionDict -> Time -> ItemFull -> ItemQuant
              -> (MU.Part, MU.Part)
partItemTrunk side factionD = partItemN side factionD False DetailLow 0

partItemHigh :: FactionId -> FactionDict -> Time -> ItemFull -> ItemQuant
             -> (MU.Part, MU.Part)
partItemHigh side factionD = partItemN side factionD False DetailAll 100

-- The @count@ can be different than @itemK@ in @ItemFull@, e.g., when picking
-- a subset of items to drop.
partItemWsR :: FactionId -> FactionDict -> Bool -> DetailLevel -> Int -> Time
            -> ItemFull -> ItemQuant
            -> MU.Part
partItemWsR side factionD ranged detail count localTime itemFull kit =
  let (name, powers) =
        partItemN side factionD ranged detail 4 localTime itemFull kit
      arItem = aspectRecordFull itemFull
      periodic = IA.checkFlag Ability.Periodic arItem
      condition = IA.checkFlag Ability.Condition arItem
      maxCount = Dice.supDice $ IK.icount $ itemKind itemFull
  in if | condition && count == 1 -> MU.Phrase [name, powers]
        | condition && not periodic && maxCount > 1 ->
            let percent = 100 * count `divUp` maxCount
                amount = tshow count <> "-strong"
                         <+> "(" <> tshow percent <> "%)"
            in MU.Phrase [MU.Text amount, name, powers]
        | condition ->
            MU.Phrase [MU.Text $ tshow count <> "-fold", name, powers]
        | IA.checkFlag Ability.Unique arItem && count == 1 ->
            MU.Phrase ["the", name, powers]
        | otherwise -> MU.Phrase [MU.CarAWs count name, powers]

partItemWs :: FactionId -> FactionDict -> Int -> Time -> ItemFull -> ItemQuant
           -> MU.Part
partItemWs side factionD = partItemWsR side factionD False DetailMedium

partItemWsShort :: FactionId -> FactionDict -> Int -> Time -> ItemFull
                -> ItemQuant
                -> MU.Part
partItemWsShort side factionD = partItemWsR side factionD False DetailLow

partItemWsRanged :: FactionId -> FactionDict -> Int -> Time -> ItemFull
                 -> ItemQuant
                 -> MU.Part
partItemWsRanged side factionD = partItemWsR side factionD True DetailMedium

partItemShortAW :: FactionId -> FactionDict -> Time -> ItemFull -> ItemQuant
                -> MU.Part
partItemShortAW side factionD localTime itemFull kit =
  let (name, _) = partItemShort side factionD localTime itemFull kit
      arItem = aspectRecordFull itemFull
  in if IA.checkFlag Ability.Unique arItem
     then MU.Phrase ["the", name]
     else MU.AW name

partItemMediumAW :: FactionId -> FactionDict -> Time -> ItemFull -> ItemQuant
                 -> MU.Part
partItemMediumAW side factionD localTime itemFull kit =
  let (name, powers) =
        partItemN side factionD False DetailMedium 100 localTime itemFull kit
      arItem = aspectRecordFull itemFull
  in if IA.checkFlag Ability.Unique arItem
     then MU.Phrase ["the", name, powers]
     else MU.AW $ MU.Phrase [name, powers]

partItemShortWownW :: FactionId -> FactionDict -> MU.Part -> Time -> ItemFull
                   -> ItemQuant
                   -> MU.Part
partItemShortWownW side factionD partA localTime itemFull kit =
  let (name, _) = partItemShort side factionD localTime itemFull kit
  in MU.WownW partA name

viewItem :: ItemFull -> Color.AttrCharW32
{-# INLINE viewItem #-}
viewItem itemFull =
  Color.attrChar2ToW32 (flavourToColor $ jflavour $ itemBase itemFull)
                       (IK.isymbol $ itemKind itemFull)

itemDesc :: Bool -> FactionId -> FactionDict -> Int -> CStore -> Time -> LevelId
         -> ItemFull -> ItemQuant
         -> AttrString
itemDesc markParagraphs side factionD aHurtMeleeOfOwner store localTime jlid
         itemFull@ItemFull{itemBase, itemKind, itemDisco, itemSuspect} kit =
  let (name, powers) = partItemHigh side factionD localTime itemFull kit
      arItem = aspectRecordFull itemFull
      npowers = makePhrase [name, powers]
      IK.ThrowMod{IK.throwVelocity, IK.throwLinger} = IA.aToThrow arItem
      speed = speedFromWeight (IK.iweight itemKind) throwVelocity
      range = rangeFromSpeedAndLinger speed throwLinger
      tspeed | IA.checkFlag Ability.Condition arItem
               || IK.iweight itemKind == 0 = ""
             | speed < speedLimp = "When thrown, it drops at once."
             | speed < speedWalk = "When thrown, it drops after one meter."
             | otherwise =
               "Can be thrown at"
               <+> T.pack (displaySpeed $ fromSpeed speed)
               <> if throwLinger /= 100
                  then " dropping after" <+> tshow range <> "m."
                  else "."
      tsuspect = ["You are unsure what it does." | itemSuspect]
      (desc, aspectSentences, damageAnalysis) =
        let aspects = case itemDisco of
              ItemDiscoMean IA.KindMean{..} | kmConst ->
                IA.aspectRecordToList kmMean  -- exact and collated
              ItemDiscoMean{} -> IK.iaspects itemKind
                -- doesn't completely lose the @Odds@ case, so better than
                -- the above, even if does not collate multiple skill bonuses
              ItemDiscoFull iAspect -> IA.aspectRecordToList iAspect
            sentences = tsuspect ++ mapMaybe aspectToSentence aspects
            aHurtMeleeOfItem = IA.getSkill Ability.SkHurtMelee arItem
            meanDmg = ceiling $ Dice.meanDice (IK.idamage itemKind)
            dmgAn = if meanDmg <= 0 then "" else
              let multRaw = aHurtMeleeOfOwner
                            + if store `elem` [CEqp, COrgan]
                              then 0
                              else aHurtMeleeOfItem
                  mult = 100 + min 99 (max (-99) multRaw)
                  minDeltaHP = xM meanDmg `divUp` 100
                  rawDeltaHP = fromIntegral mult * minDeltaHP
                  pmult = 100 + min 99 (max (-99) aHurtMeleeOfItem)
                  prawDeltaHP = fromIntegral pmult * minDeltaHP
                  pdeltaHP = modifyDamageBySpeed prawDeltaHP speed
                  mDeltaHP = modifyDamageBySpeed minDeltaHP speed
              in "Against defenceless foes you'd inflict around"
                   -- rounding and non-id items
                 <+> tshow meanDmg
                 <> "*" <> tshow mult <> "%"
                 <> "=" <> show64With2 rawDeltaHP
                 <+> "melee damage (min" <+> show64With2 minDeltaHP
                 <> ") and"
                 <+> tshow meanDmg
                 <> "*" <> tshow pmult <> "%"
                 <> "*" <> "speed^2"
                 <> "/" <> tshow (fromSpeed speedThrust `divUp` 10) <> "^2"
                 <> "=" <> show64With2 pdeltaHP
                 <+> "ranged damage (min" <+> show64With2 mDeltaHP
                 <> ") with it"
                 <> if Dice.infDice (IK.idamage itemKind)
                       == Dice.supDice (IK.idamage itemKind)
                    then "."
                    else "on average."
        in (IK.idesc itemKind, T.intercalate " " sentences, tspeed <+> dmgAn)
      weight = IK.iweight itemKind
      (scaledWeight, unitWeight)
        | weight > 1000 =
          (tshow $ fromIntegral weight / (1000 :: Double), "kg")
        | otherwise = (tshow weight, "g")
      onLevel = "on level" <+> tshow (abs $ fromEnum jlid) <> "."
      discoFirst = (if IA.checkFlag Ability.Unique arItem
                    then "Discovered"
                    else "First seen")
                   <+> onLevel
      whose fid = gname (factionD EM.! fid)
      sourceDesc =
        case jfid itemBase of
          Just fid | IA.checkFlag Ability.Condition arItem ->
            "Caused by" <+> (if fid == side then "us" else whose fid)
            <> ". First observed" <+> onLevel
          Just fid ->
            "Coming from" <+> whose fid
            <> "." <+> discoFirst
          _ -> discoFirst
      ikitNames = map (fromGroupName . fst) $ filter ((== COrgan) . snd)
                                            $ IK.ikit itemKind
      ikitDesc | null ikitNames = ""
               | otherwise = makeSentence
        [ "the actor also has organs of this kind:"
        , MU.Text $ T.intercalate ", " ikitNames ]
      colorSymbol = viewItem itemFull
      blurb =
        ((" "
          <> npowers
          <> (if markParagraphs then ":\n\n" else ": ")
          <> desc
          <> (if markParagraphs && not (T.null desc) then "\n\n" else ""))
         <+> (if weight > 0
              then makeSentence
                     ["Weighs around", MU.Text scaledWeight <> unitWeight]
              else ""))
        <+> aspectSentences
        <+> sourceDesc
        <+> damageAnalysis
        <> (if markParagraphs && not (T.null ikitDesc) then "\n\n" else "\n")
        <> ikitDesc
  in colorSymbol : textToAS blurb
