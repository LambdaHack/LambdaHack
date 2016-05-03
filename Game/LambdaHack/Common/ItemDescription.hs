-- | Descripitons of items.
module Game.LambdaHack.Common.ItemDescription
  ( partItemN, partItem, partItemWs, partItemAW, partItemMediumAW, partItemWownW
  , textAllAE, viewItem
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.EffectDescription
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK

-- | The part of speech describing the item parameterized by the number
-- of effects/aspects to show..
partItemN :: Int -> Int -> CStore -> Time -> ItemFull
          -> (Bool, MU.Part, MU.Part)
partItemN fullInfo n c localTime itemFull =
  let genericName = jname $ itemBase itemFull
  in case itemDisco itemFull of
    Nothing ->
      let flav = flavourToName $ jflavour $ itemBase itemFull
      in (False, MU.Text $ flav <+> genericName, "")
    Just iDisco ->
      let (toutN, it1) = case strengthFromEqpSlot IK.EqpSlotTimeout itemFull of
            Nothing -> (0, [])
            Just timeout ->
              let timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
                  charging startT = timeShift startT timeoutTurns > localTime
              in (timeout, filter charging (itemTimer itemFull))
          len = length it1
          chargingAdj | toutN == 0 = "temporary"
                      | otherwise = "charging"
          timer | len == 0 = ""
                | itemK itemFull == 1 && len == 1 = "(" <> chargingAdj <> ")"
                | otherwise = "(" <> tshow len <+> chargingAdj <> ")"
          skipRecharging = fullInfo <= 4 && len >= itemK itemFull
          effTs = filter (not . T.null)
                  $ textAllAE fullInfo skipRecharging c itemFull
          ts = take n effTs
               ++ ["(...)" | length effTs > n]
               ++ [timer]
          isUnique aspects = IK.Unique `elem` aspects
          unique = case iDisco of
            ItemDisco{itemAE=Just ItemAspectEffect{jaspects}} ->
              isUnique jaspects
            ItemDisco{itemKind} ->
              isUnique $ IK.iaspects itemKind
          capName = if unique
                    then MU.Capitalize $ MU.Text genericName
                    else MU.Text genericName
      in (unique, capName, MU.Phrase $ map MU.Text ts)

-- | The part of speech describing the item.
partItem :: CStore -> Time -> ItemFull -> (Bool, MU.Part, MU.Part)
partItem = partItemN 5 4

textAllAE :: Int -> Bool -> CStore -> ItemFull -> [Text]
textAllAE fullInfo skipRecharging cstore ItemFull{itemBase, itemDisco} =
  let features | fullInfo >= 9 = map featureToSuff $ sort $ jfeature itemBase
               | otherwise = []
  in case itemDisco of
    Nothing -> features
    Just ItemDisco{itemKind, itemAE} ->
      let periodicAspect :: IK.Aspect a -> Bool
          periodicAspect IK.Periodic = True
          periodicAspect _ = False
          timeoutAspect :: IK.Aspect a -> Bool
          timeoutAspect IK.Timeout{} = True
          timeoutAspect _ = False
          noEffect :: IK.Effect -> Bool
          noEffect IK.ELabel{} = True
          noEffect _ = False
          hurtEffect :: IK.Effect -> Bool
          hurtEffect (IK.Hurt _) = True
          hurtEffect (IK.Burn _) = True
          hurtEffect _ = False
          notDetail :: IK.Effect -> Bool
          notDetail IK.Explode{} = fullInfo >= 6
          notDetail _ = True
          active = cstore `elem` [CEqp, COrgan]
                   || cstore == CGround && isJust (strengthEqpSlot itemBase)
          splitAE :: (Num a, Show a, Ord a)
                  => (a -> Text)
                  -> [IK.Aspect a] -> (IK.Aspect a -> Text)
                  -> [IK.Effect] -> (IK.Effect -> Text)
                  -> [Text]
          splitAE reduce_a aspects ppA effects ppE =
            let mperiodic = find periodicAspect aspects
                mtimeout = find timeoutAspect aspects
                mnoEffect = find noEffect effects
                restAs = sort aspects
                (hurtEs, restEs) = partition hurtEffect $ sort
                                   $ filter notDetail effects
                aes = if active
                      then map ppA restAs ++ map ppE restEs
                      else map ppE restEs ++ map ppA restAs
                rechargingTs = T.intercalate (T.singleton ' ')
                               $ filter (not . T.null)
                               $ map ppE $ stripRecharging restEs
                onSmashTs = T.intercalate (T.singleton ' ')
                            $ filter (not . T.null)
                            $ map ppE $ stripOnSmash restEs
                durable = IK.Durable `elem` jfeature itemBase
                periodicOrTimeout = case mperiodic of
                  _ | skipRecharging || T.null rechargingTs -> ""
                  Just IK.Periodic ->
                    case mtimeout of
                      Just (IK.Timeout 0) | not durable ->
                        "(each turn until gone:"
                        <+> rechargingTs <> ")"
                      Just (IK.Timeout t) ->
                        "(every" <+> reduce_a t <> ":"
                        <+> rechargingTs <> ")"
                      _ -> ""
                  _ -> case mtimeout of
                    Just (IK.Timeout t) ->
                      "(timeout" <+> reduce_a t <> ":"
                      <+> rechargingTs <> ")"
                    _ -> ""
                onSmash = if T.null onSmashTs then ""
                          else "(on smash:" <+> onSmashTs <> ")"
                noEff = case mnoEffect of
                  Just (IK.ELabel t) -> [t]
                  _ -> []
            in noEff ++ if fullInfo >= 5 || fullInfo >= 2 && null noEff
                        then [periodicOrTimeout] ++ map ppE hurtEs ++ aes
                             ++ [onSmash | fullInfo >= 7]
                        else map ppE hurtEs
          aets = case itemAE of
            Just ItemAspectEffect{jaspects, jeffects} ->
              splitAE tshow
                      jaspects aspectToSuffix
                      jeffects effectToSuffix
            Nothing ->
              splitAE (maybe "?" tshow . Dice.reduceDice)
                      (IK.iaspects itemKind) kindAspectToSuffix
                      (IK.ieffects itemKind) kindEffectToSuffix
      in aets ++ features

-- TODO: use kit
partItemWs :: Int -> CStore -> Time -> ItemFull -> MU.Part
partItemWs count c localTime itemFull =
  let (unique, name, stats) = partItem c localTime itemFull
  in if unique && count == 1
     then MU.Phrase ["the", name, stats]
     else MU.Phrase [MU.CarWs count name, stats]

partItemAW :: CStore -> Time -> ItemFull -> MU.Part
partItemAW c localTime itemFull =
  let (unique, name, stats) = partItemN 4 4 c localTime itemFull
  in if unique
     then MU.Phrase ["the", name, stats]
     else MU.AW $ MU.Phrase [name, stats]

partItemMediumAW :: CStore -> Time -> ItemFull -> MU.Part
partItemMediumAW c localTime itemFull =
  let (unique, name, stats) = partItemN 5 100 c localTime itemFull
  in if unique
     then MU.Phrase ["the", name, stats]
     else MU.AW $ MU.Phrase [name, stats]

partItemWownW :: MU.Part -> CStore -> Time -> ItemFull -> MU.Part
partItemWownW partA c localTime itemFull =
  let (_, name, stats) = partItemN 4 4 c localTime itemFull
  in MU.WownW partA $ MU.Phrase [name, stats]

viewItem :: Item -> (Char, Color.Attr)
viewItem item = ( jsymbol item
                , Color.defAttr {Color.fg = flavourToColor $ jflavour item} )
