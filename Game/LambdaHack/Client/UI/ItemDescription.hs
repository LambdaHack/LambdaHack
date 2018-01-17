-- | Descripitons of items.
module Game.LambdaHack.Client.UI.ItemDescription
  ( partItem, partItemShort, partItemHigh, partItemWs, partItemWsRanged
  , partItemShortAW, partItemMediumAW, partItemShortWownW
  , viewItem, itemDesc
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , show64With2, partItemN, textAllAE, partItemWsR
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import           Data.Int (Int64)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.Overlay
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Flavour
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemStrongest
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK

show64With2 :: Int64 -> Text
show64With2 n =
  let k = 100 * n `div` oneM
      l = k `div` 100
      x = k - l * 100
  in tshow l
     <> if | x == 0 -> ""
           | x < 10 -> ".0" <> tshow x
           | otherwise -> "." <> tshow x

-- | The part of speech describing the item parameterized by the number
-- of effects/aspects to show..
partItemN :: FactionId -> FactionDict -> Bool -> DetailLevel -> Int
          -> Time -> ItemFull
          -> (Bool, Bool, MU.Part, MU.Part)
partItemN side factionD ranged detailLevel n localTime
          itemFull@ItemFull{itemBase} =
  let genericName = jname itemBase
  in case itemDisco itemFull of
    Nothing ->
      let flav = flavourToName $ jflavour itemBase
      in (False, False, MU.Text $ flav <+> genericName, "")
    Just iDisco ->
      let timeout = aTimeout $ aspectRecordFull itemFull
          timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
          temporary = not (null $ itemTimer itemFull) && timeout == 0
          charging startT = timeShift startT timeoutTurns > localTime
          it1 = filter charging (itemTimer itemFull)
          lenCh = length it1
          timer | lenCh == 0 || temporary = ""
                | itemK itemFull == 1 && lenCh == 1 = "(charging)"
                | itemK itemFull == lenCh = "(all charging)"
                | otherwise = "(" <> tshow lenCh <+> "charging)"
          skipRecharging = detailLevel <= DetailLow
                           && lenCh >= itemK itemFull
          (effTsRaw, rangedDamage) =
            textAllAE detailLevel skipRecharging itemFull
          effTs = effTsRaw ++ if ranged then rangedDamage else []
          lsource = case jfid itemBase of
            Just fid | jname itemBase `elem` ["impressed"] ->
              ["by" <+> if fid == side
                        then "us"
                        else gname (factionD EM.! fid)]
            _ -> []
          ts = lsource
               ++ take n effTs
               ++ ["(...)" | length effTs > n]
               ++ [timer]
          unique = IK.Unique `elem` IK.ieffects (itemKind iDisco)
          name | temporary = "temporarily" <+> genericName
               | otherwise = genericName
          capName = if unique
                    then MU.Capitalize $ MU.Text name
                    else MU.Text name
      in ( not (null lsource) || temporary
         , unique, capName, MU.Phrase $ map MU.Text ts )

textAllAE :: DetailLevel -> Bool -> ItemFull -> ([Text], [Text])
textAllAE detailLevel skipRecharging ItemFull{itemBase, itemDisco} =
  let features | detailLevel >= DetailAll =
                   map featureToSuff $ sort $ jfeature itemBase
               | otherwise = []
  in case itemDisco of
    Nothing -> (features, [])
    Just ItemDisco{itemKind, itemAspect} ->
      let timeoutAspect :: IK.Aspect -> Bool
          timeoutAspect IK.Timeout{} = True
          timeoutAspect _ = False
          hurtMeleeAspect :: IK.Aspect -> Bool
          hurtMeleeAspect IK.AddHurtMelee{} = True
          hurtMeleeAspect _ = False
          elabel :: IK.Effect -> Bool
          elabel IK.ELabel{} = True
          elabel _ = False
          active = goesIntoEqp itemBase
          splitAE :: DetailLevel -> [IK.Aspect] -> [IK.Effect] -> [Text]
          splitAE detLev aspects effects =
            let ppA = kindAspectToSuffix
                ppE = effectToSuffix detLev
                reduce_a = maybe "?" tshow . Dice.reduceDice
                periodic = IK.Periodic `elem` IK.ieffects itemKind
                mtimeout = find timeoutAspect aspects
                -- Effects are not sorted, because they fire in the order
                -- specified.
                restAs = sort aspects
                aes = if active
                      then map ppA restAs ++ map ppE effects
                      else map ppE effects ++ map ppA restAs
                rechargingTs = T.intercalate " " $ filter (not . T.null)
                               $ map ppE $ stripRecharging effects
                onSmashTs = T.intercalate " " $ filter (not . T.null)
                            $ map ppE $ stripOnSmash effects
                durable = IK.Durable `elem` jfeature itemBase
                fragile = IK.Fragile `elem` jfeature itemBase
                periodicOrTimeout =
                  if | skipRecharging || T.null rechargingTs -> ""
                     | periodic -> case mtimeout of
                         Nothing | durable && not fragile ->
                           "(each turn:" <+> rechargingTs <> ")"
                         Nothing ->
                           "(each turn until gone:" <+> rechargingTs <> ")"
                         Just (IK.Timeout t) ->
                           "(every" <+> reduce_a t <> ":"
                           <+> rechargingTs <> ")"
                         _ -> error $ "" `showFailure` mtimeout
                     | otherwise -> case mtimeout of
                         Nothing -> ""
                         Just (IK.Timeout t) ->
                           "(timeout" <+> reduce_a t <> ":"
                           <+> rechargingTs <> ")"
                         _ -> error $ "" `showFailure` mtimeout
                onSmash = if T.null onSmashTs then ""
                          else "(on smash:" <+> onSmashTs <> ")"
                elab = case find elabel effects of
                  Just (IK.ELabel t) -> [t]
                  _ -> []
                damage = case find hurtMeleeAspect restAs of
                  Just (IK.AddHurtMelee hurtMelee) ->
                    (if jdamage itemBase == 0
                     then "0d0"
                     else tshow (jdamage itemBase))
                    <> affixDice hurtMelee <> "%"
                  _ -> if jdamage itemBase == 0
                       then ""
                       else tshow (jdamage itemBase)
            in filter (/= "")
               $ elab ++ if detLev >= DetailHigh
                            || detLev >= DetailMedium && null elab
                         then [periodicOrTimeout] ++ [damage] ++ aes
                              ++ [onSmash | detLev >= DetailAll]
                         else [damage]
          splitTry ass eff =
            let splits = map (\detLev -> splitAE detLev ass eff)
                             [minBound..maxBound]
                splitsToTry = drop (fromEnum detailLevel) splits
            in case filter (/= []) splitsToTry of
                 detNonEmpty : _ -> detNonEmpty
                 [] -> []
          aets = case itemAspect of
            Just aspectRecord ->
              splitTry (aspectRecordToList aspectRecord) (IK.ieffects itemKind)
            Nothing ->
              splitTry (IK.iaspects itemKind) (IK.ieffects itemKind)
          IK.ThrowMod{IK.throwVelocity} = strengthToThrow itemBase
          speed = speedFromWeight (jweight itemBase) throwVelocity
          meanDmg = ceiling $ Dice.meanDice (jdamage itemBase)
          minDeltaHP = xM meanDmg `divUp` 100
          aHurtMeleeOfItem = case itemAspect of
            Just aspectRecord -> aHurtMelee aspectRecord
            Nothing -> case find hurtMeleeAspect (IK.iaspects itemKind) of
              Just (IK.AddHurtMelee d) -> ceiling $ Dice.meanDice d
              _ -> 0
          pmult = 100 + min 99 (max (-99) aHurtMeleeOfItem)
          prawDeltaHP = fromIntegral pmult * minDeltaHP
          pdeltaHP = modifyDamageBySpeed prawDeltaHP speed
          rangedDamage = if pdeltaHP == 0
                         then []
                         else ["{avg" <+> show64With2 pdeltaHP <+> "ranged}"]
          -- Note that avg melee damage would be too complex to display here,
          -- because in case of @MOwned@ the owner is different than leader,
          -- so the value would be different than when viewing the item.
      in (aets ++ features, rangedDamage)

-- | The part of speech describing the item.
partItem :: FactionId -> FactionDict -> Time -> ItemFull
         -> (Bool, Bool, MU.Part, MU.Part)
partItem side factionD = partItemN side factionD False DetailMedium 4

partItemShort :: FactionId -> FactionDict -> Time -> ItemFull
              -> (Bool, Bool, MU.Part, MU.Part)
partItemShort side factionD = partItemN side factionD False DetailLow 4

partItemHigh :: FactionId -> FactionDict -> Time -> ItemFull
             -> (Bool, Bool, MU.Part, MU.Part)
partItemHigh side factionD = partItemN side factionD False DetailAll 100

-- The @count@ can be different than @itemK@ in @ItemFull@, e.g., when picking
-- a subset of items to drop.
partItemWsR :: FactionId -> FactionDict -> Bool -> Int -> Time -> ItemFull
            -> (Bool, MU.Part)
partItemWsR side factionD ranged count localTime itemFull =
  let (temporary, unique, name, stats) =
        partItemN side factionD ranged DetailMedium 4 localTime itemFull
      tmpCondition = isTmpCondition $ itemBase itemFull
  in ( temporary
     , if | temporary && count == 1 -> MU.Phrase [name, stats]
          | temporary ->
              MU.Phrase [MU.Text $ tshow count <> "-fold", name, stats]
          | unique && count == 1 -> MU.Phrase ["the", name, stats]
          | tmpCondition -> MU.Phrase [name, stats]
          | otherwise -> MU.Phrase [MU.CarWs count name, stats] )

partItemWs :: FactionId -> FactionDict -> Int -> Time -> ItemFull
           -> (Bool, MU.Part)
partItemWs side factionD = partItemWsR side factionD False

partItemWsRanged :: FactionId -> FactionDict -> Int -> Time -> ItemFull
                 -> (Bool, MU.Part)
partItemWsRanged side factionD = partItemWsR side factionD True

partItemShortAW :: FactionId -> FactionDict -> Time -> ItemFull -> MU.Part
partItemShortAW side factionD localTime itemFull =
  let (_, unique, name, _) = partItemShort side factionD localTime itemFull
  in if unique
     then MU.Phrase ["the", name]
     else MU.AW name

partItemMediumAW :: FactionId -> FactionDict -> Time -> ItemFull -> MU.Part
partItemMediumAW side factionD localTime itemFull =
  let (_, unique, name, stats) =
        partItemN side factionD False DetailMedium 100 localTime itemFull
  in if unique
     then MU.Phrase ["the", name, stats]
     else MU.AW $ MU.Phrase [name, stats]

partItemShortWownW :: FactionId -> FactionDict -> MU.Part -> Time -> ItemFull
                   -> MU.Part
partItemShortWownW side factionD partA localTime itemFull =
  let (_, _, name, _) = partItemShort side factionD localTime itemFull
  in MU.WownW partA name

viewItem :: Item -> Color.AttrCharW32
{-# INLINE viewItem #-}
viewItem item =
  Color.attrChar2ToW32 (flavourToColor $ jflavour item) (jsymbol item)

itemDesc :: Bool -> FactionId -> FactionDict -> Int -> CStore -> Time
         -> ItemFull
         -> AttrLine
itemDesc markParagraphs side factionD aHurtMeleeOfOwner store localTime
         itemFull@ItemFull{itemBase} =
  let (_, unique, name, stats) = partItemHigh side factionD localTime itemFull
      nstats = makePhrase [name, stats]
      IK.ThrowMod{IK.throwVelocity, IK.throwLinger} = strengthToThrow itemBase
      speed = speedFromWeight (jweight itemBase) throwVelocity
      range = rangeFromSpeedAndLinger speed throwLinger
      tspeed | isTmpCondition itemBase = ""
             | speed < speedLimp = "When thrown, it drops at once."
             | speed < speedWalk = "When thrown, it travels only one meter and drops immediately."
             | otherwise =
               "When thrown, it flies with speed of"
               <+> tshow (fromSpeed speed `div` 10)
               <> if throwLinger /= 100
                  then " m/s and range" <+> tshow range <+> "m."
                  else " m/s."
      (desc, featureSentences, damageAnalysis) = case itemDisco itemFull of
        Nothing -> ("This item is as unremarkable as can be.", "", tspeed)
        Just ItemDisco{itemKind, itemAspect} ->
          let sentences = mapMaybe featureToSentence (IK.ifeature itemKind)
              hurtMeleeAspect :: IK.Aspect -> Bool
              hurtMeleeAspect IK.AddHurtMelee{} = True
              hurtMeleeAspect _ = False
              aHurtMeleeOfItem = case itemAspect of
                Just aspectRecord -> aHurtMelee aspectRecord
                Nothing -> case find hurtMeleeAspect (IK.iaspects itemKind) of
                  Just (IK.AddHurtMelee d) -> ceiling $ Dice.meanDice d
                  _ -> 0
              meanDmg = ceiling $ Dice.meanDice (jdamage itemBase)
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
                in "Against defenceless targets you would inflict around"
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
                   <> if Dice.minDice (jdamage itemBase)
                         == Dice.maxDice (jdamage itemBase)
                      then "."
                      else "on average."
          in (IK.idesc itemKind, T.intercalate " " sentences, tspeed <+> dmgAn)
      eqpSlotSentence = case strengthEqpSlot itemFull of
        Just es -> slotToSentence es
        Nothing -> ""
      weight = jweight itemBase
      (scaledWeight, unitWeight)
        | weight > 1000 =
          (tshow $ fromIntegral weight / (1000 :: Double), "kg")
        | otherwise = (tshow weight, "g")
      onLevel = "on level" <+> tshow (abs $ fromEnum $ jlid itemBase) <> "."
      discoFirst = (if unique then "Discovered" else "First seen")
                   <+> onLevel
      whose fid = gname (factionD EM.! fid)
      sourceDesc =
        case jfid itemBase of
          Just fid | isTmpCondition itemBase ->
            "Caused by" <+> (if fid == side then "us" else whose fid)
            <> ". First observed" <+> onLevel
          Just fid ->
            "Coming from" <+> whose fid
            <> "." <+> discoFirst
          _ -> discoFirst
      colorSymbol = viewItem itemBase
      blurb =
        ((" "
          <> nstats
          <> (if markParagraphs then ":\n\n" else ": ")
          <> desc
          <> (if markParagraphs && not (T.null desc) then "\n\n" else ""))
         <+> (if weight > 0
              then makeSentence ["Weighs", MU.Text scaledWeight <> unitWeight]
              else ""))
        <+> featureSentences
        <+> eqpSlotSentence
        <+> sourceDesc
        <+> damageAnalysis
  in colorSymbol : textToAL blurb
