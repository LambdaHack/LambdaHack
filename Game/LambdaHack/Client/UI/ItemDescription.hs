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
import qualified Game.LambdaHack.Common.ItemAspect as IA
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
-- of effects/aspects to show.
partItemN :: FactionId -> FactionDict -> Bool -> DetailLevel -> Int
          -> Time -> ItemFull
          -> (Bool, Bool, MU.Part, MU.Part)
partItemN side factionD ranged detailLevel n localTime
          itemFull@ItemFull{..} =
  let genericName = IK.iname itemKind
      flav = flavourToName $ jflavour itemBase
      timeout = IA.aTimeout $ aspectRecordFull itemFull
      timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
      temporary = not (null itemTimer) && timeout == 0
      charging startT = timeShift startT timeoutTurns > localTime
      it1 = filter charging itemTimer
      lenCh = length it1
      timer | lenCh == 0 || temporary = ""
            | itemK == 1 && lenCh == 1 = "(charging)"
            | itemK == lenCh = "(all charging)"
            | otherwise = "(" <> tshow lenCh <+> "charging)"
      skipRecharging = detailLevel <= DetailLow && lenCh >= itemK
      (effTsRaw, rangedDamage) =
        textAllAE detailLevel skipRecharging itemFull
      effTs = effTsRaw ++ if ranged then rangedDamage else []
      lsource = case jfid itemBase of
        Just fid | IK.iname itemKind `elem` ["impressed"] ->
          ["by" <+> if fid == side
                    then "us"
                    else gname (factionD EM.! fid)]
        _ -> []
      ts = lsource
           ++ take n effTs
           ++ ["(...)" | length effTs > n]
           ++ [timer]
      unique = IK.Unique `elem` IK.ieffects itemKind
      name | temporary = "temporarily" <+> genericName
           | itemSuspect = flav <+> genericName
           | otherwise = genericName
      capName = if unique
                then MU.Capitalize $ MU.Text name
                else MU.Text name
  in ( not (null lsource) || temporary
     , unique, capName, MU.Phrase $ map MU.Text ts )

textAllAE :: DetailLevel -> Bool -> ItemFull -> ([Text], [Text])
textAllAE detailLevel skipRecharging itemFull@ItemFull{..} =
  let features | detailLevel >= DetailAll =
                   map featureToSuff $ sort $ IK.ifeature itemKind
               | otherwise = []
      aets = case itemDisco of
        ItemDiscoMean{} ->  -- faster than @aspectRecordToList@ of mean
          splitTry (IK.iaspects itemKind) (IK.ieffects itemKind)
        ItemDiscoFull{itemAspect} ->
          splitTry (IA.aspectRecordToList itemAspect) (IK.ieffects itemKind)
      timeoutAspect :: IA.Aspect -> Bool
      timeoutAspect IA.Timeout{} = True
      timeoutAspect _ = False
      hurtMeleeAspect :: IA.Aspect -> Bool
      hurtMeleeAspect IA.AddHurtMelee{} = True
      hurtMeleeAspect _ = False
      elabel :: IK.Effect -> Bool
      elabel IK.ELabel{} = True
      elabel _ = False
      active = goesIntoEqp itemFull
      splitAE :: DetailLevel -> [IA.Aspect] -> [IK.Effect] -> [Text]
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
                           $ map ppE $ IK.stripRecharging effects
            onSmashTs = T.intercalate " " $ filter (not . T.null)
                        $ map ppE $ IK.stripOnSmash effects
            durable = IK.Durable `elem` IK.ifeature itemKind
            fragile = IK.Fragile `elem` IK.ifeature itemKind
            periodicOrTimeout =
              if | skipRecharging || T.null rechargingTs -> ""
                 | periodic -> case mtimeout of
                     Nothing | durable && not fragile ->
                       "(each turn:" <+> rechargingTs <> ")"
                     Nothing ->
                       "(each turn until gone:" <+> rechargingTs <> ")"
                     Just (IA.Timeout t) ->
                       "(every" <+> reduce_a t <> ":"
                       <+> rechargingTs <> ")"
                     _ -> error $ "" `showFailure` mtimeout
                 | otherwise -> case mtimeout of
                     Nothing -> ""
                     Just (IA.Timeout t) ->
                       "(timeout" <+> reduce_a t <> ":"
                       <+> rechargingTs <> ")"
                     _ -> error $ "" `showFailure` mtimeout
            onSmash = if T.null onSmashTs then ""
                      else "(on smash:" <+> onSmashTs <> ")"
            elab = case find elabel effects of
              Just (IK.ELabel t) -> [t]
              _ -> []
            damage = case find hurtMeleeAspect restAs of
              Just (IA.AddHurtMelee hurtMelee) ->
                (if IK.idamage itemKind == 0
                 then "0d0"
                 else tshow (IK.idamage itemKind))
                <> affixDice hurtMelee <> "%"
              _ -> if IK.idamage itemKind == 0
                   then ""
                   else tshow (IK.idamage itemKind)
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
      IK.ThrowMod{IK.throwVelocity} = strengthToThrow itemFull
      speed = speedFromWeight (IK.iweight itemKind) throwVelocity
      meanDmg = ceiling $ Dice.meanDice (IK.idamage itemKind)
      minDeltaHP = xM meanDmg `divUp` 100
      aHurtMeleeOfItem = IA.aHurtMelee $ aspectRecordFull itemFull
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
      tmpCondition = isTmpCondition itemFull
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

viewItem :: ItemFull -> Color.AttrCharW32
{-# INLINE viewItem #-}
viewItem itemFull =
  Color.attrChar2ToW32 (flavourToColor $ jflavour $ itemBase itemFull)
                       (IK.isymbol $ itemKind itemFull)

itemDesc :: Bool -> FactionId -> FactionDict -> Int -> CStore -> Time
         -> ItemFull
         -> AttrLine
itemDesc markParagraphs side factionD aHurtMeleeOfOwner store localTime
         itemFull@ItemFull{..} =
  let (_, unique, name, stats) = partItemHigh side factionD localTime itemFull
      nstats = makePhrase [name, stats]
      IK.ThrowMod{IK.throwVelocity, IK.throwLinger} = strengthToThrow itemFull
      speed = speedFromWeight (IK.iweight itemKind) throwVelocity
      range = rangeFromSpeedAndLinger speed throwLinger
      tspeed | isTmpCondition itemFull = ""
             | speed < speedLimp = "When thrown, it drops at once."
             | speed < speedWalk = "When thrown, it travels only one meter and drops immediately."
             | otherwise =
               "When thrown, it flies with speed of"
               <+> tshow (fromSpeed speed `div` 10)
               <> if throwLinger /= 100
                  then " m/s and range" <+> tshow range <+> "m."
                  else " m/s."
      tsuspect = ["You are unsure what it does." | itemSuspect]
      (desc, featureSentences, damageAnalysis) =
        let sentences = tsuspect ++ mapMaybe featureToSentence (IK.ifeature itemKind)
            aHurtMeleeOfItem = IA.aHurtMelee $ aspectRecordFull itemFull
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
                 <> if Dice.minDice (IK.idamage itemKind)
                       == Dice.maxDice (IK.idamage itemKind)
                    then "."
                    else "on average."
        in (IK.idesc itemKind, T.intercalate " " sentences, tspeed <+> dmgAn)
      eqpSlotSentence = case strengthEqpSlot itemFull of
        Just es -> slotToSentence es
        Nothing -> ""
      weight = IK.iweight itemKind
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
          Just fid | isTmpCondition itemFull ->
            "Caused by" <+> (if fid == side then "us" else whose fid)
            <> ". First observed" <+> onLevel
          Just fid ->
            "Coming from" <+> whose fid
            <> "." <+> discoFirst
          _ -> discoFirst
      colorSymbol = viewItem itemFull
      blurb =
        ((" "
          <> nstats
          <> (if markParagraphs then ":\n\n" else ": ")
          <> desc
          <> (if markParagraphs && not (T.null desc) then "\n\n" else ""))
         <+> (if weight > 0
              then makeSentence
                     ["Weighs around", MU.Text scaledWeight <> unitWeight]
              else ""))
        <+> featureSentences
        <+> eqpSlotSentence
        <+> sourceDesc
        <+> damageAnalysis
  in colorSymbol : textToAL blurb
