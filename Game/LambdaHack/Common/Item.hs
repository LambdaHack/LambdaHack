{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
-- No operation in this module
-- involves the 'State' or 'Action' type.
-- TODO: Document after it's rethought and rewritten wrt separating
-- inventory manangement and items proper.
module Game.LambdaHack.Common.Item
  ( -- * The @Item@ type
    ItemId, Item(..), seedToAspectsEffects
    -- * Item strength
  , strengthAspect, strengthEffect, strengthFeature
  , strengthMelee, strengthArmor, strengthRegen, strengthStead, strengthLight
  , strengthLingering, strengthToThrow, isFragile
  , strongestItem, strongestSword, strongestShield
  , strongestRegen, strongestStead, strongestLight
    -- * Item discovery types
  , ItemKindIx, Discovery, ItemSeed, ItemAspectEffect(..), DiscoAE
  , KisOn, ItemFull(..), ItemDisco(..), itemK, itemIsOn, itemNoDisco
    -- * Inventory management types
  , ItemBag, ItemDict, ItemKnown
    -- * Textual description
  , partItem, partItemWs, partItemAW, partItemWownW, itemDesc, textAllAE
    -- * Assorted
  , totalRange, computeTrajectory, itemTrajectory
  ) where

import Control.Exception.Assert.Sugar
import qualified Control.Monad.State as St
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Hashable as Hashable
import qualified Data.Ix as Ix
import Data.List
import qualified Data.Ord as Ord
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU
import System.Random (mkStdGen)

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.ItemFeature as IF
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind

-- | A unique identifier of an item in the dungeon.
newtype ItemId = ItemId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- | An index of the kind id of an item. Clients have partial knowledge
-- how these idexes map to kind ids. They gain knowledge by identifying items.
newtype ItemKindIx = ItemKindIx Int
  deriving (Show, Eq, Ord, Enum, Ix.Ix, Hashable.Hashable, Binary)

-- | The map of item kind indexes to item kind ids.
-- The full map, as known by the server, is a bijection.
type Discovery = EM.EnumMap ItemKindIx (Kind.Id ItemKind)

-- | A seed for rolling aspects and effects of an item
-- Clients have partial knowledge of how item ids map to the seeds.
-- They gain knowledge by identifying items.
newtype ItemSeed = ItemSeed Int
  deriving (Show, Eq, Ord, Enum, Hashable.Hashable, Binary)

data ItemAspectEffect = ItemAspectEffect
  { jaspects :: ![Aspect Int]  -- ^ the aspects of the item
  , jeffects :: ![Effect Int]  -- ^ the effects when activated
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary ItemAspectEffect

instance Hashable.Hashable ItemAspectEffect

-- | The map of item ids to item aspects and effects.
-- The full map is known by the server.
type DiscoAE = EM.EnumMap ItemId ItemAspectEffect

type KisOn = (Int, Bool)

data ItemDisco = ItemDisco
  { itemKindId :: Kind.Id ItemKind
  , itemKind   :: ItemKind
  , itemAE     :: Maybe ItemAspectEffect
  }
  deriving Show

data ItemFull = ItemFull
  { itemBase  :: Item
  , itemKisOn :: KisOn
  , itemDisco :: Maybe ItemDisco
  }
  deriving Show

itemK :: ItemFull -> Int
itemK = fst . itemKisOn

itemIsOn :: ItemFull -> Bool
itemIsOn = snd . itemKisOn

itemNoDisco :: (Item, KisOn) -> ItemFull
itemNoDisco (itemBase, itemKisOn) =
  ItemFull {itemBase, itemKisOn, itemDisco=Nothing}

-- | Game items in actor possesion or strewn around the dungeon.
-- The fields @jsymbol@, @jname@ and @jflavour@ make it possible to refer to
-- and draw an unidentified item. Full information about item is available
-- through the @jkindIx@ index as soon as the item is identified.
data Item = Item
  { jkindIx  :: !ItemKindIx    -- ^ index pointing to the kind of the item
  , jlid     :: !LevelId       -- ^ the level on which item was created
  , jsymbol  :: !Char          -- ^ individual map symbol
  , jname    :: !Text          -- ^ individual generic name
  , jflavour :: !Flavour       -- ^ individual flavour
  , jfeature :: ![IF.Feature]  -- ^ other properties
  , jweight  :: !Int           -- ^ weight in grams, obvious enough
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable.Hashable Item

instance Binary Item

seedToAspectsEffects :: ItemSeed -> ItemKind -> Int -> Int
                     -> ItemAspectEffect
seedToAspectsEffects (ItemSeed itemSeed) kind ln depth =
  let castD = castDice ln depth
      rollAE = do
        aspects <- mapM (flip aspectTrav castD) (iaspects kind)
        effects <- mapM (flip effectTrav castD) (ieffects kind)
        return (aspects, effects)
      (jaspects, jeffects) = St.evalState rollAE (mkStdGen itemSeed)
  in ItemAspectEffect{..}

type ItemBag = EM.EnumMap ItemId KisOn

-- | All items in the dungeon (including in actor inventories),
-- indexed by item identifier.
type ItemDict = EM.EnumMap ItemId Item

type ItemKnown = (Item, ItemAspectEffect)

strongestItem :: Ord b => Bool -> [(ItemId, ItemFull)] -> (ItemFull -> [b])
              -> [(b, (ItemId, ItemFull))]
strongestItem onlyOn is p =
  let pv (iid, item) = map (\v -> (v, (iid, item))) (p item)
      onlyIs = if onlyOn then filter (itemIsOn . snd) is else is
      pis = concatMap pv onlyIs
  in sortBy (flip $ Ord.comparing fst) pis

dice999 :: Dice.Dice -> Int
dice999 d = if Dice.minDice d == Dice.maxDice d
            then Dice.minDice d
            else 999

strengthAspect :: (Aspect Int -> [b]) -> ItemFull -> [b]
strengthAspect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAE = Just ItemAspectEffect{jaspects}} ->
      concatMap f jaspects
    Just ItemDisco{itemKind=ItemKind{iaspects}} ->
      -- Default for unknown power is 999 to encourage experimenting.
      let trav x = St.evalState (aspectTrav x (return . dice999)) ()
      in concatMap f $ map trav iaspects
    Nothing -> []

strengthEffect :: (Effect Int -> [b]) -> ItemFull -> [b]
strengthEffect f itemFull =
  case itemDisco itemFull of
    Just ItemDisco{itemAE = Just ItemAspectEffect{jeffects}} ->
      concatMap f jeffects
    Just ItemDisco{itemKind=ItemKind{ieffects}} ->
      -- Default for unknown power is 999 to encourage experimenting.
      let trav x = St.evalState (effectTrav x (return . dice999)) ()
      in concatMap f $ map trav ieffects
    Nothing -> []

strengthFeature :: (IF.Feature -> [b]) -> Item -> [b]
strengthFeature f item = concatMap f (jfeature item)

strengthMelee :: Kind.COps -> ItemFull -> [Int]
strengthMelee Kind.COps{corule} itemFull =
  if jsymbol (itemBase itemFull) `elem` ritemMelee (Kind.stdRuleset corule)
  then let p (Hurt d k) = [floor (Dice.meanDice d) + k]
           p _ = []
       in strengthEffect p itemFull
  else []

strongestSword :: Kind.COps -> Bool -> [(ItemId, ItemFull)]
               -> [(Int, (ItemId, ItemFull))]
strongestSword cops onlyOn is = strongestItem onlyOn is (strengthMelee cops)

strengthArmor :: ItemFull -> [Int]
strengthArmor =
  let p (ArmorMelee k) = [k]
      p _ = []
  in strengthAspect p

strongestShield :: Bool -> [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestShield onlyOn is = strongestItem onlyOn is strengthArmor

strengthRegen :: ItemFull -> [Int]
strengthRegen =
  let p (Regeneration k) = [k]
      p _ = []
  in strengthAspect p

strongestRegen :: Bool -> [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestRegen onlyOn is = strongestItem onlyOn is strengthRegen

strengthStead :: ItemFull -> [Int]
strengthStead =
  let p (Steadfastness k) = [k]
      p _ = []
  in strengthAspect p

strongestStead :: Bool -> [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestStead onlyOn is = strongestItem onlyOn is strengthStead

strengthLight :: Item -> [Int]
strengthLight =
  let p (IF.Light k) = [k]
      p _ = []
  in strengthFeature p

strongestLight :: Bool -> [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestLight onlyOn is = strongestItem onlyOn is (strengthLight . itemBase)

strengthLingering :: Item -> Int
strengthLingering item =
  let p (IF.Linger percent) = [percent]
      p _ = []
  in case strengthFeature p item of
    [] -> 100
    [percent] -> percent
    vs -> assert `failure` (vs, item)

strengthToThrow :: Item -> Int
strengthToThrow item =
  let p (IF.ToThrow percent) = [percent]
      p _ = []
  in case strengthFeature p item of
    [] -> 0
    [percent] -> percent
    vs -> assert `failure` (vs, item)

isFragile :: Item -> Bool
isFragile item =
  let p IF.Fragile = [()]
      p _ = []
  in case strengthFeature p item of
    [] -> False
    [()] -> True
    vss -> assert `failure` (vss, item)

-- | The part of speech describing the item.
partItem :: ItemFull -> (MU.Part, MU.Part)
partItem itemFull =
  let genericName = jname $ itemBase itemFull
  in case itemDisco itemFull of
    Nothing ->
      let flav = flavourToName $ jflavour $ itemBase itemFull
      in (MU.Text $ flav <+> genericName, "")
    Just _ ->
      let effTs = textAllAE itemFull
          effectFirst = case filter (not . T.null) effTs of
            [] -> ""
            effT : _ -> effT
          effectExtra = case filter (not . T.null) effTs of
            [] -> ""
            [_] -> ""
            [_, effT] -> "(" <> effT <> ")"
            [_, effT1, effT2] -> "(" <> effT1 <+> ", " <+> effT2 <> ")"
            _ -> "(of many effects)"
          turnedOff | itemIsOn itemFull = ""
                    | otherwise = "{OFF}"  -- TODO: mark with colour
      in ( MU.Text genericName
         , MU.Text $ effectFirst <+> effectExtra <+> turnedOff )

textAllAE :: ItemFull -> [Text]
textAllAE ItemFull{itemBase, itemDisco} =
  case itemDisco of
    Nothing -> [""]
    Just ItemDisco{itemKind, itemAE} -> case itemAE of
      Just ItemAspectEffect{jaspects, jeffects} ->
        map aspectToSuffix jaspects
        ++ map effectToSuffix jeffects
        ++ map IF.featureToSuff (jfeature itemBase)
      Nothing -> map kindAspectToSuffix (iaspects itemKind)
                 ++ map kindEffectToSuffix (ieffects itemKind)
                 ++ map IF.featureToSuff (jfeature itemBase)

partItemWs :: Int -> ItemFull -> MU.Part
partItemWs count itemFull =
  let (name, stats) = partItem itemFull
  in MU.Phrase [MU.CarWs count name, stats]

partItemAW :: ItemFull -> MU.Part
partItemAW itemFull =
  let (name, stats) = partItem itemFull
  in MU.AW $ MU.Phrase [name, stats]

partItemWownW :: MU.Part -> ItemFull -> MU.Part
partItemWownW partA itemFull =
  let (name, stats) = partItem itemFull
  in MU.WownW partA $ MU.Phrase [name, stats]

-- TODO: also print some data from kind and from item
itemDesc :: ItemFull -> Text
itemDesc itemFull =
  let (name, stats) = partItem itemFull
      nstats = makePhrase [name, stats MU.:> ":"]
  in case itemDisco itemFull of
    Nothing -> nstats <+> "This item is as unremarkable as can be."
    Just ItemDisco{itemKind} -> nstats <+> idesc itemKind

totalRange :: Item -> Int
totalRange item =
  let linger = strengthLingering item
      speed = speedFromWeight (jweight item) (strengthToThrow item)
  in rangeFromSpeedAndLinger speed linger

computeTrajectory :: Int -> Int -> Int -> [Point] -> ([Vector], Speed)
computeTrajectory weight toThrow linger path =
  let speed = speedFromWeight weight toThrow
      trange = rangeFromSpeedAndLinger speed linger
      btrajectory = take trange $ pathToTrajectory path
  in (btrajectory, speed)

itemTrajectory :: Item -> [Point] -> ([Vector], Speed)
itemTrajectory item path =
  computeTrajectory (jweight item) (strengthToThrow item)
                    (strengthLingering item) path
