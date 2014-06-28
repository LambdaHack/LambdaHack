-- | Descripitons of items.
module Game.LambdaHack.Common.ItemDescription
  ( partItemN, partItem, partItemWs, partItemAW, partItemWownW
  , itemDesc, textAllAE
  ) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.EffectDescription
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.ItemKind

-- | The part of speech describing the item parameterized by the number
-- of effects/aspects to show..
partItemN :: Bool -> Int -> CStore -> ItemFull -> (MU.Part, MU.Part)
partItemN fullInfo n cstore itemFull =
  let genericName = jname $ itemBase itemFull
  in case itemDisco itemFull of
    Nothing ->
      let flav = flavourToName $ jflavour $ itemBase itemFull
      in (MU.Text $ flav <+> genericName, "")
    Just _ ->
      let effTs = filter (not . T.null) $ textAllAE fullInfo cstore itemFull
          ts = take n effTs ++ if length effTs > n then ["(...)"] else []
      in (MU.Text genericName, MU.Phrase $ map MU.Text ts)

-- | The part of speech describing the item.
partItem :: CStore -> ItemFull -> (MU.Part, MU.Part)
partItem = partItemN False 3

textAllAE :: Bool -> CStore -> ItemFull -> [Text]
textAllAE fullInfo cstore itemFull@ItemFull{itemBase, itemDisco} =
  let features | fullInfo = map featureToSuff $ jfeature itemBase
               | otherwise = []
  in case itemDisco of
    Nothing -> features
    Just ItemDisco{itemKind, itemAE} ->
      let (aspects, effects) =  case itemAE of
            Just ItemAspectEffect{jaspects, jeffects} ->
              ( map aspectToSuffix jaspects
              , map effectToSuffix jeffects )
            Nothing -> ( map kindAspectToSuffix $ iaspects itemKind
                       , map kindEffectToSuffix $ ieffects itemKind )
          active = cstore `elem` [CEqp, COrgan]
                   || cstore == CGround && isJust (strengthEqpSlot itemBase)
          periodic = isJust
                     $ strengthFromEqpSlot Effect.EqpSlotPeriodic itemFull
          weapon = maybe False ((== Effect.EqpSlotWeapon) . fst)
                   $ strengthEqpSlot itemBase
      in if weapon || not active
         then effects ++ if fullInfo || active then aspects else []
         else aspects ++ if fullInfo || periodic then effects else []
         ++ features

partItemWs :: Int -> CStore -> ItemFull -> MU.Part
partItemWs count cstore itemFull =
  let (name, stats) = partItem cstore itemFull
  in MU.Phrase [MU.CarWs count name, stats]

partItemAW :: CStore -> ItemFull -> MU.Part
partItemAW cstore itemFull =
  let (name, stats) = partItem cstore itemFull
  in MU.AW $ MU.Phrase [name, stats]

partItemWownW :: MU.Part -> CStore -> ItemFull -> MU.Part
partItemWownW partA cstore itemFull =
  let (name, stats) = partItem cstore itemFull
  in MU.WownW partA $ MU.Phrase [name, stats]

-- TODO: also print some data from kind and from item
itemDesc :: CStore -> ItemFull -> Text
itemDesc cstore itemFull =
  let (name, stats) = partItemN True 99 cstore itemFull
      nstats = makePhrase [name, stats MU.:> ":"]
  in case itemDisco itemFull of
    Nothing -> nstats <+> "This item is as unremarkable as can be."
    Just ItemDisco{itemKind} -> nstats <+> idesc itemKind
