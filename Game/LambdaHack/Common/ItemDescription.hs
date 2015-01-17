-- | Descripitons of items.
module Game.LambdaHack.Common.ItemDescription
  ( partItemN, partItem, partItemWs, partItemAW, partItemWownW
  , itemDesc, textAllAE, viewItem
  ) where

import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.EffectDescription
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK

-- TODO: remove _lid if still unused after some time
-- | The part of speech describing the item parameterized by the number
-- of effects/aspects to show..
partItemN :: Bool -> Int -> CStore -> LevelId -> Time -> ItemFull
          -> (Bool, MU.Part, MU.Part)
partItemN fullInfo n c _lid localTime itemFull =
  let genericName = jname $ itemBase itemFull
  in case itemDisco itemFull of
    Nothing ->
      let flav = flavourToName $ jflavour $ itemBase itemFull
      in (False, MU.Text $ flav <+> genericName, "")
    Just iDisco ->
      let effTs = filter (not . T.null) $ textAllAE fullInfo c itemFull
          it1 = case strengthFromEqpSlot IK.EqpSlotTimeout itemFull of
            Nothing -> []
            Just timeout ->
              let timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
                  f startT = timeShift startT timeoutTurns > localTime
              in filter f (itemTimer itemFull)
          len = length it1
          timer | len == 0 = ""
                | itemK itemFull == 1 && len == 1 = "(charging)"
                | otherwise = "(" <> tshow len <+> "charging" <> ")"
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
partItem :: CStore -> LevelId -> Time -> ItemFull -> (Bool, MU.Part, MU.Part)
partItem = partItemN False 4

textAllAE :: Bool -> CStore -> ItemFull -> [Text]
textAllAE fullInfo cstore ItemFull{itemBase, itemDisco} =
  let features | fullInfo = map featureToSuff $ sort $ jfeature itemBase
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
          noEffect IK.NoEffect{} = True
          noEffect _ = False
          hurtEffect :: IK.Effect -> Bool
          hurtEffect (IK.Hurt _) = True
          hurtEffect _ = False
          notDetail :: IK.Effect -> Bool
          notDetail IK.Explode{} = fullInfo
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
                aes = map ppE hurtEs
                      ++ if active
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
                  _ | T.null rechargingTs -> ""
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
                  Just (IK.NoEffect t) -> [t]
                  _ -> []
            in noEff ++ [periodicOrTimeout] ++ aes ++ [onSmash | fullInfo]
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
partItemWs :: Int -> CStore -> LevelId -> Time -> ItemFull -> MU.Part
partItemWs count c lid localTime itemFull =
  let (unique, name, stats) = partItem c lid localTime itemFull
  in if unique && count == 1
     then MU.Phrase ["the", name, stats]
     else MU.Phrase [MU.CarWs count name, stats]

partItemAW :: CStore -> LevelId -> Time -> ItemFull -> MU.Part
partItemAW c lid localTime itemFull =
  let (unique, name, stats) = partItem c lid localTime itemFull
  in if unique
     then MU.Phrase ["the", name, stats]
     else MU.AW $ MU.Phrase [name, stats]

partItemWownW :: MU.Part -> CStore -> LevelId -> Time -> ItemFull -> MU.Part
partItemWownW partA c lid localTime itemFull =
  let (_, name, stats) = partItem c lid localTime itemFull
  in MU.WownW partA $ MU.Phrase [name, stats]

itemDesc :: CStore -> LevelId -> Time -> ItemFull -> Overlay
itemDesc c lid localTime itemFull =
  let (_, name, stats) = partItemN True 99 c lid localTime itemFull
      nstats = makePhrase [name, stats]
      desc = case itemDisco itemFull of
        Nothing -> "This item is as unremarkable as can be."
        Just ItemDisco{itemKind} -> IK.idesc itemKind
      weight = jweight (itemBase itemFull)
      (scaledWeight, unitWeight) =
        if weight > 1000
        then (tshow $ fromIntegral weight / (1000 :: Double), "kg")
        else (tshow weight, "g")
      ln = abs $ fromEnum $ jlid (itemBase itemFull)
      colorSymbol = uncurry (flip Color.AttrChar) (viewItem $ itemBase itemFull)
      f = Color.AttrChar Color.defAttr
      lxsize = fst normalLevelBound + 1  -- TODO
      blurb =
        "D"  -- dummy
        <+> nstats
        <> ":"
        <+> desc
        <+> makeSentence ["Weighs", MU.Text scaledWeight <> unitWeight]
        <+> makeSentence ["First found on level", MU.Text $ tshow ln]
      splitBlurb = splitText lxsize blurb
      attrBlurb = map (map f . T.unpack) splitBlurb
  in encodeOverlay $ (colorSymbol : tail (head attrBlurb)) : tail attrBlurb

viewItem :: Item -> (Char, Color.Attr)
viewItem item = ( jsymbol item
                , Color.defAttr {Color.fg = flavourToColor $ jflavour item} )
