{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
-- No operation in this module
-- involves the 'State' or 'Action' type.
-- TODO: Document after it's rethought and rewritten wrt separating
-- inventory manangement and items proper.
module Game.LambdaHack.Common.Item
  ( -- * The @Item@ type
    ItemId, Item(..), buildItem, newItem, seedToAspectsEffects
    -- * Item strength
  , strengthMelee, strengthArmor, strengthRegen, strengthStead, strengthLight
  , strengthLingering, strengthToThrow, groupsExplosive, isFragile
  , strongestItem
  , strongestSword, strongestShield
  , strongestRegen, strongestStead, strongestLight
    -- * Item discovery types
  , ItemKindIx, Discovery, DiscoRev, serverDiscos
  , ItemSeed, ItemSeedDict, ItemAspectEffect(..), DiscoAE, ItemFull
    -- * The @FlavourMap@ type
  , FlavourMap, emptyFlavourMap, dungeonFlavourMap
    -- * Inventory management types
  , ItemBag, ItemDict, ItemKnown, ItemRev
    -- * Textual description
  , partItem, partItemWs, partItemAW, partItemWownW, itemDesc, textAllAE
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Hashable as Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Ix as Ix
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU
import System.Random (mkStdGen)

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Frequency
import qualified Game.LambdaHack.Common.ItemFeature as IF
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
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

-- | The reverse map to @Discovery@, needed for item creation.
type DiscoRev = EM.EnumMap (Kind.Id ItemKind) ItemKindIx

-- | A seed for rolling aspects and effects of an item
-- Clients have partial knowledge of how item ids map to the seeds.
-- They gain knowledge by identifying items.
newtype ItemSeed = ItemSeed Int
  deriving (Show, Eq, Ord, Enum, Hashable.Hashable, Binary)

-- | The map of item ids to item seeds.
-- The full map is known by the server.
type ItemSeedDict = EM.EnumMap ItemId ItemSeed

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

type ItemFull = ( Item
                , Maybe ( (Kind.Id ItemKind, ItemKind)
                        , Maybe ItemAspectEffect ) )

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
  , jisOn    :: !Bool          -- ^ the item is turned on
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable.Hashable Item

instance Binary Item

serverDiscos :: Kind.Ops ItemKind -> Rnd (Discovery, DiscoRev)
serverDiscos Kind.Ops{obounds, ofoldrWithKey} = do
  let ixs = map ItemKindIx $ take (Ix.rangeSize obounds) [0..]
      shuffle :: Eq a => [a] -> Rnd [a]
      shuffle [] = return []
      shuffle l = do
        x <- oneOf l
        fmap (x :) $ shuffle (delete x l)
  shuffled <- shuffle ixs
  let f ik _ (ikMap, ikRev, ix : rest) =
        (EM.insert ix ik ikMap, EM.insert ik ix ikRev, rest)
      f ik  _ (ikMap, _, []) =
        assert `failure` "too short ixs" `twith` (ik, ikMap)
      (discoS, discoRev, _) =
        ofoldrWithKey f (EM.empty, EM.empty, shuffled)
  return (discoS, discoRev)

-- | Build an item with the given stats.
buildItem :: FlavourMap -> DiscoRev -> Kind.Id ItemKind -> ItemKind -> LevelId
          -> Item
buildItem (FlavourMap flavour) discoRev ikChosen kind jlid =
  let jkindIx  = discoRev EM.! ikChosen
      jsymbol  = isymbol kind
      jname    = iname kind
      jflavour =
        case iflavour kind of
          [fl] -> fl
          _ -> flavour EM.! ikChosen
      jfeature = ifeature kind
      jweight = iweight kind
      jisOn = True
  in Item{..}

-- | Generate an item based on level.
newItem :: Kind.Ops ItemKind -> FlavourMap -> DiscoRev
        -> Frequency Text -> LevelId -> Int -> Int
        -> Rnd (ItemKnown, ItemSeed, Int)
newItem coitem@Kind.Ops{opick, okind}
        flavour discoRev itemFreq jlid ln depth = do
  itemGroup <- frequency itemFreq
  let castItem :: Int -> Rnd (ItemKnown, ItemSeed, Int)
      castItem 0 | nullFreq itemFreq = assert `failure` "no fallback items"
                                              `twith` (itemFreq, ln, depth)
      castItem 0 = do
        let newFreq = setFreq itemFreq itemGroup 0
        newItem coitem flavour discoRev newFreq jlid ln depth
      castItem count = do
        ikChosen <- fmap (fromMaybe $ assert `failure` itemGroup)
                    $ opick itemGroup (const True)
        let kind = okind ikChosen
        jcount <- castDice ln depth (icount kind)
        if jcount == 0 then
          castItem $ count - 1
        else do
          seed <- fmap ItemSeed random
          return ( ( buildItem flavour discoRev ikChosen kind jlid
                   , seedToAspectsEffects seed kind ln depth )
                 , seed
                 , jcount )
  castItem 10

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

-- | Flavours assigned by the server to item kinds, in this particular game.
newtype FlavourMap = FlavourMap (EM.EnumMap (Kind.Id ItemKind) Flavour)
  deriving (Show, Binary)

emptyFlavourMap :: FlavourMap
emptyFlavourMap = FlavourMap EM.empty

-- | Assigns flavours to item kinds. Assures no flavor is repeated,
-- except for items with only one permitted flavour.
rollFlavourMap :: Kind.Id ItemKind -> ItemKind
               -> Rnd (EM.EnumMap (Kind.Id ItemKind) Flavour, S.Set Flavour)
               -> Rnd (EM.EnumMap (Kind.Id ItemKind) Flavour, S.Set Flavour)
rollFlavourMap key ik rnd =
  let flavours = iflavour ik
  in if length flavours == 1
     then rnd
     else do
       (assocs, available) <- rnd
       let proper = S.fromList flavours `S.intersection` available
       flavour <- oneOf (S.toList proper)
       return (EM.insert key flavour assocs, S.delete flavour available)

-- | Randomly chooses flavour for all item kinds for this game.
dungeonFlavourMap :: Kind.Ops ItemKind -> Rnd FlavourMap
dungeonFlavourMap Kind.Ops{ofoldrWithKey} =
  liftM (FlavourMap . fst) $
    ofoldrWithKey rollFlavourMap (return (EM.empty, S.fromList stdFlav))

type ItemBag = EM.EnumMap ItemId Int

-- | All items in the dungeon (including in actor inventories),
-- indexed by item identifier.
type ItemDict = EM.EnumMap ItemId Item

type ItemKnown = (Item, ItemAspectEffect)

-- | Reverse item map, for item creation, to keep items and item identifiers
-- in bijection.
type ItemRev = HM.HashMap ItemKnown ItemId

strongestItem :: Ord b => [(ItemId, a)] -> (a -> [b]) -> [(b, (ItemId, a))]
strongestItem is p =
  let pv (iid, item) = map (\v -> (v, (iid, item))) (p item)
      pis = concatMap pv is
  in sortBy (flip $ Ord.comparing fst) pis

strengthAspect :: (Aspect Int -> [b]) -> ItemFull -> [b]
strengthAspect f itemFull =
  case itemFull of
    (_, Just (_, Just ItemAspectEffect{jaspects})) -> concatMap f jaspects
    (_, Just ((_, ItemKind{iaspects}), Nothing)) ->
      -- Default for unknown power is 999 to encourage experimenting.
      let trav x = St.evalState (aspectTrav x (return . const 999)) ()
      in concatMap f $ map trav iaspects
    (_, Nothing) -> []

strengthEffect :: (Effect Int -> [b]) -> ItemFull -> [b]
strengthEffect f itemFull =
  case itemFull of
    (_, Just (_, Just ItemAspectEffect{jeffects})) -> concatMap f jeffects
    (_, Just ((_, ItemKind{ieffects}), Nothing)) ->
      -- Default for unknown power is 999 to encourage experimenting.
      let trav x = St.evalState (effectTrav x (return . const 999)) ()
      in concatMap f $ map trav ieffects
    (_, Nothing) -> []

strengthFeature :: (IF.Feature -> [b]) -> Item -> [b]
strengthFeature f item = concatMap f (jfeature item)

strengthMelee :: Kind.COps -> ItemFull -> [Int]
strengthMelee Kind.COps{corule} itemFull@(item, _) =
  if jsymbol item `elem` ritemMelee (Kind.stdRuleset corule)
  then let p (Hurt d k) = [floor (Dice.meanDice d) + k]
           p _ = []
       in strengthEffect p itemFull
  else []

strongestSword :: Kind.COps -> [(ItemId, ItemFull)]
               -> [(Int, (ItemId, ItemFull))]
strongestSword cops is =
  strongestItem (filter (jisOn . fst . snd) is) (strengthMelee cops)

strengthArmor :: ItemFull -> [Int]
strengthArmor =
  let p (ArmorMelee k) = [k]
      p _ = []
  in strengthAspect p

strongestShield :: [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestShield is = strongestItem (filter (jisOn . fst . snd) is) strengthArmor

strengthRegen :: ItemFull -> [Int]
strengthRegen =
  let p (Regeneration k) = [k]
      p _ = []
  in strengthAspect p

strongestRegen :: [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestRegen is = strongestItem (filter (jisOn . fst . snd) is) strengthRegen

strengthStead :: ItemFull -> [Int]
strengthStead =
  let p (Steadfastness k) = [k]
      p _ = []
  in strengthAspect p

strongestStead :: [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestStead is = strongestItem (filter (jisOn . fst . snd) is) strengthStead

strengthLight :: Item -> [Int]
strengthLight =
  let p (IF.Light k) = [k]
      p _ = []
  in strengthFeature p

strongestLight :: [(ItemId, Item)] -> [(Int, (ItemId, Item))]
strongestLight is = strongestItem (filter (jisOn . snd) is) strengthLight

isFragile :: Item -> Bool
isFragile item =
  let p IF.Fragile = [()]
      p _ = []
  in case strengthFeature p item of
    [] -> False
    [()] -> True
    vss -> assert `failure` (vss, item)

groupsExplosive :: ItemFull -> [Text]
groupsExplosive =
  let p (Explode cgroup) = [cgroup]
      p _ = []
  in strengthAspect p

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

-- | The part of speech describing the item.
partItem :: ItemFull -> (MU.Part, MU.Part)
partItem (item, mfull) =
  let genericName = jname item
  in case mfull of
    Nothing ->
      let flav = flavourToName $ jflavour item
      in (MU.Text $ flav <+> genericName, "")
    Just _ ->
      let effTs = textAllAE (item, mfull)
          effectText = case filter (not . T.null) effTs of
            [] -> ""
            [effT] -> effT
            [effT1, effT2] -> effT1 <+> "and" <+> effT2
            _ -> "of many effects"
          turnedOff | jisOn item = ""
                    | otherwise = "{OFF}"  -- TODO: mark with colour
      in (MU.Text genericName, MU.Text $ effectText <+> turnedOff)

textAllAE :: ItemFull -> [Text]
textAllAE (item, mfull) =
  case mfull of
    Nothing -> [""]
    Just ((_, kind), miae) -> case miae of
      Just ItemAspectEffect{jaspects, jeffects} ->
        map aspectToSuffix jaspects
        ++ map effectToSuffix jeffects
        ++ map IF.featureToSuff (jfeature item)
      Nothing -> map kindAspectToSuffix (iaspects kind)
                 ++ map kindEffectToSuffix (ieffects kind)
                 ++ map IF.featureToSuff (jfeature item)

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
  in case itemFull of
    (_, Nothing) -> nstats <+> "This item is as unremarkable as can be."
    (_, Just ((_, kind), _)) -> nstats <+> idesc kind
