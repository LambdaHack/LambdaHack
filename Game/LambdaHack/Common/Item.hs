{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
-- No operation in this module
-- involves the 'State' or 'Action' type.
-- TODO: Document after it's rethought and rewritten wrt separating
-- inventory manangement and items proper.
module Game.LambdaHack.Common.Item
  ( -- * The @Item@ type
    ItemId, Item(..), buildItem, newItem, seedToAspectsEffects
    -- * Item search
  , strongestItem
  , strongestSword, strongestShield
  , strongestRegen, strongestStead, strongestLight
  , pMelee, pArmor, pRegen, pStead, pLight
   -- * Item discovery types
  , ItemKindIx, Discovery, DiscoRev, serverDiscos
  , ItemSeed, ItemSeedDict, ItemAspectEffect(..), DiscoAE, ItemFull
    -- * The @FlavourMap@ type
  , FlavourMap, emptyFlavourMap, dungeonFlavourMap
    -- * Inventory management types
  , ItemBag, ItemDict, ItemKnown, ItemRev
    -- * Textual description
  , partItem, partItemWs, partItemAW, partItemWownW, itemDesc
    -- * Assorted
  , isFragile, isExplosive, isLingering, isToThrow
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

strongestItem :: [(ItemId, a)] -> (a -> Maybe Int)
              -> [(Int, (ItemId, a))]
strongestItem is p =
  let kis = mapMaybe (\(iid, item) -> case p item of
                         Nothing -> Nothing
                         Just v -> Just (v, (iid, item))) is
  in sortBy (flip $ Ord.comparing fst) kis

pMelee :: Kind.COps -> ItemFull -> Maybe Int
pMelee Kind.COps{corule}
       (item, Just (_, Just ItemAspectEffect{jeffects})) =
  if jsymbol item `elem` ritemMelee (Kind.stdRuleset corule)
  then let getP (Hurt d k) _ = Just $ floor (Dice.meanDice d) + k
           getP _ acc = acc
       in foldr getP Nothing jeffects
  else Nothing
pMelee _ _ = Nothing

strongestSword :: Kind.COps -> [(ItemId, ItemFull)]
               -> [(Int, (ItemId, ItemFull))]
strongestSword cops is =
  strongestItem (filter (jisOn . fst . snd) is) (pMelee cops)

pArmor :: ItemFull -> Maybe Int
pArmor (_, Just (_, Just ItemAspectEffect{jaspects})) =
  let getP (ArmorMelee k) _ = Just k
      getP _ acc = acc
  in foldr getP Nothing jaspects
pArmor _ = Nothing

strongestShield :: [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestShield is = strongestItem (filter (jisOn . fst . snd) is) pArmor

pRegen :: ItemFull -> Maybe Int
pRegen (_, Just (_, Just ItemAspectEffect{jaspects})) =
  let getP (Regeneration k) _ = Just k
      getP _ acc = acc
  in foldr getP Nothing jaspects
pRegen _ = Nothing

strongestRegen :: [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestRegen is = strongestItem (filter (jisOn . fst . snd) is) pRegen

pStead :: ItemFull -> Maybe Int
pStead (_, Just (_, Just ItemAspectEffect{jaspects})) =
  let getP (Steadfastness k) _ = Just k
      getP _ acc = acc
  in foldr getP Nothing jaspects
pStead _ = Nothing

strongestStead :: [(ItemId, ItemFull)] -> [(Int, (ItemId, ItemFull))]
strongestStead is = strongestItem (filter (jisOn . fst . snd) is) pStead

pLight :: Item -> Maybe Int
pLight = let getP (IF.Light k) _ = Just k
             getP _ acc = acc
         in foldr getP Nothing . jfeature

strongestLight :: [(ItemId, Item)] -> [(Int, (ItemId, Item))]
strongestLight is = strongestItem (filter (jisOn . snd) is) pLight

isFragile :: Item -> Bool
isFragile = let getTo IF.Fragile _acc = True
                getTo _ acc = acc
            in foldr getTo False . jfeature

isExplosive :: ItemFull -> Maybe Text
isExplosive (_, Just (_, Just ItemAspectEffect{jaspects})) =
  let getTo (Explode cgroup) _acc = Just cgroup
      getTo _ acc = acc
  in foldr getTo Nothing jaspects
isExplosive _ = Nothing

isLingering :: Item -> Int
isLingering = let getTo (IF.Linger percent) _acc = percent
                  getTo _ acc = acc
              in foldr getTo 100 . jfeature

isToThrow :: Item -> Int
isToThrow = let getTo (IF.ToThrow percent) _acc = percent
                getTo _ acc = acc
            in foldr getTo 0 . jfeature

-- | The part of speech describing the item.
partItem :: ItemFull -> (MU.Part, MU.Part)
partItem (item, mfull) =
  let genericName = jname item
      flav = flavourToName $ jflavour item
  in case mfull of
    Nothing -> (MU.Text $ flav <+> genericName, "")
    Just ((_, kind), miae) ->
      let effTs = case miae of
            Just ItemAspectEffect{jaspects, jeffects} ->
              map aspectToSuffix jaspects
              ++ map effectToSuffix jeffects
              ++ map IF.featureToSuff (jfeature item)
            Nothing -> map kindAspectToSuffix (iaspects kind)
                       ++ map kindEffectToSuffix (ieffects kind)
                       ++ map IF.featureToSuff (jfeature item)
          effectText = case filter (not . T.null) effTs of
            [] -> ""
            [effT] -> effT
            [effT1, effT2] -> effT1 <+> "and" <+> effT2
            _ -> "of many effects"
          turnedOff | jisOn item = ""
                    | otherwise = "{OFF}"  -- TODO: mark with colour
      in (MU.Text genericName, MU.Text $ effectText <+> turnedOff)

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
