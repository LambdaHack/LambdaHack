{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
-- No operation in this module
-- involves the 'State' or 'Action' type.
-- TODO: Document after it's rethought and rewritten wrt separating
-- inventory manangement and items proper.
module Game.LambdaHack.Common.Item
  ( -- * The @Item@ type
    ItemId, Item(..), jkind, buildItem, newItem
    -- * Item search
  , strongestItem, strongestItems
  , strongestSword, strongestShield
  , strongestRegen, strongestStead, strongestLight
  , pMelee, pArmor, pRegen, pStead, pLight
   -- * Item discovery types
  , ItemKindIx, Discovery, DiscoRev, serverDiscos
    -- * The @FlavourMap@ type
  , FlavourMap, emptyFlavourMap, dungeonFlavourMap
    -- * Inventory management types
  , ItemBag, ItemDict, ItemRev
    -- * Textual description
  , partItem, partItemWs, partItemAW, partItemWownW, itemDesc
    -- * Assorted
  , isFragile, isExplosive, isLingering, isToThrow
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
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

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Frequency
import qualified Game.LambdaHack.Common.ItemFeature as IF
import qualified Game.LambdaHack.Common.Kind as Kind
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

-- | Game items in actor possesion or strewn around the dungeon.
-- The fields @jsymbol@, @jname@ and @jflavour@ make it possible to refer to
-- and draw an unidentified item. Full information about item is available
-- through the @jkindIx@ index as soon as the item is identified.
data Item = Item
  { jkindIx  :: !ItemKindIx    -- ^ index pointing to the kind of the item
  , jsymbol  :: !Char          -- ^ individual map symbol
  , jname    :: !Text          -- ^ individual generic name
  , jflavour :: !Flavour       -- ^ individual flavour
  , jaspects :: ![Aspect Int]  -- ^ the aspects of the item
  , jeffects :: ![Effect Int]  -- ^ the effects when activated
  , jfeature :: ![IF.Feature]  -- ^ other properties
  , jweight  :: !Int           -- ^ weight in grams, obvious enough
  , jisOn    :: !Bool          -- ^ the item is turned on
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable.Hashable Item

instance Binary Item

-- | Recover a kind id of an item, if identified.
jkind :: Discovery -> Item -> Maybe (Kind.Id ItemKind)
jkind disco i = EM.lookup (jkindIx i) disco

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
buildItem :: FlavourMap -> DiscoRev
          -> Kind.Id ItemKind -> ItemKind -> [Aspect Int] -> [Effect Int]
          -> Item
buildItem (FlavourMap flavour) discoRev ikChosen kind jaspects jeffects =
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
        -> Frequency Text -> Int -> Int
        -> Rnd (Item, Int, ItemKind)
newItem coitem@Kind.Ops{opick, okind} flavour discoRev itemFreq ln depth = do
  itemGroup <- frequency itemFreq
  let castItem :: Int -> Rnd (Item, Int, ItemKind)
      castItem 0 | nullFreq itemFreq = assert `failure` "no fallback items"
                                              `twith` (itemFreq, ln, depth)
      castItem 0 = do
        let newFreq = setFreq itemFreq itemGroup 0
        newItem coitem flavour discoRev newFreq ln depth
      castItem count = do
        ikChosen <- fmap (fromMaybe $ assert `failure` itemGroup)
                    $ opick itemGroup (const True)
        let kind = okind ikChosen
        jcount <- castDice ln depth (icount kind)
        if jcount == 0 then
          castItem $ count - 1
        else do
          aspects <- mapM (flip aspectTrav (castDice ln depth)) (iaspects kind)
          effects <- mapM (flip effectTrav (castDice ln depth)) (ieffects kind)
          return ( buildItem flavour discoRev ikChosen kind aspects effects
                 , jcount
                 , kind )
  castItem 10

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

-- | Reverse item map, for item creation, to keep items and item identifiers
-- in bijection.
type ItemRev = HM.HashMap Item ItemId

strongestItem :: [(ItemId, Item)] -> (Item -> Maybe Int)
              -> [(Int, (ItemId, Item))]
strongestItem is p =
  let kis = mapMaybe (\(iid, item) -> case p item of
                         Nothing -> Nothing
                         Just v -> Just (v, (iid, item))) is
  in sortBy (flip $ Ord.comparing fst) kis

strongestItems :: [(Int, (ItemId, Item))] -> (Item -> Maybe Int)
               -> [(Int, (Int, (ItemId, Item)))]
strongestItems is p =
  let kis = mapMaybe (\(k, (iid, item)) -> case p item of
                         Nothing -> Nothing
                         Just v -> Just (v, (k, (iid, item)))) is
  in sortBy (flip $ Ord.comparing fst) kis

pMelee :: Kind.COps -> Discovery -> Item -> Maybe Int
pMelee Kind.COps{coitem=_coitem, corule} disco i =
  case jkind disco i of
    Just _kid | jsymbol i `elem` ritemMelee (Kind.stdRuleset corule) ->
      let getP (Hurt d k) _ = Just $ floor (Dice.meanDice d) + k
          getP _ acc = acc
      in foldr getP Nothing (jeffects i)
    _ -> Nothing

strongestSword :: Kind.COps -> Discovery -> [(ItemId, Item)]
               -> [(Int, (ItemId, Item))]
strongestSword cops disco is =
  strongestItem (filter (jisOn . snd) is) $ pMelee cops disco

pArmor :: Item -> Maybe Int
pArmor = let getP (ArmorMelee k) _ = Just k
             getP _ acc = acc
         in foldr getP Nothing . jaspects

strongestShield :: [(ItemId, Item)] -> [(Int, (ItemId, Item))]
strongestShield is = strongestItem (filter (jisOn . snd) is) pArmor

pRegen :: Item -> Maybe Int
pRegen = let getP (Regeneration k) _ = Just k
             getP _ acc = acc
         in foldr getP Nothing . jaspects

strongestRegen :: [(ItemId, Item)] -> [(Int, (ItemId, Item))]
strongestRegen is = strongestItem (filter (jisOn . snd) is) pRegen

pStead :: Item -> Maybe Int
pStead = let getP (Steadfastness k) _ = Just k
             getP _ acc = acc
         in foldr getP Nothing . jaspects

strongestStead :: [(ItemId, Item)] -> [(Int, (ItemId, Item))]
strongestStead is = strongestItem (filter (jisOn . snd) is) pStead

pLight :: Item -> Maybe Int
pLight = let getP (Light k) _ = Just k
             getP _ acc = acc
         in foldr getP Nothing . jaspects

strongestLight :: [(ItemId, Item)] -> [(Int, (ItemId, Item))]
strongestLight is = strongestItem (filter (jisOn . snd) is) pLight

isFragile :: Item -> Bool
isFragile item = let getTo IF.Fragile _acc = True
                     getTo _ acc = acc
                 in foldr getTo False (jfeature item)
                    || isJust (isExplosive item)

isExplosive :: Item -> Maybe Text
isExplosive = let getTo (Explode cgroup) _acc = Just cgroup
                  getTo _ acc = acc
              in foldr getTo Nothing . jaspects

isLingering :: Item -> Int
isLingering = let getTo (IF.Linger percent) _acc = percent
                  getTo _ acc = acc
              in foldr getTo 100 . jfeature

isToThrow :: Item -> Int
isToThrow = let getTo (IF.ToThrow percent) _acc = percent
                getTo _ acc = acc
            in foldr getTo 0 . jfeature

-- | The part of speech describing the item.
partItem :: Kind.Ops ItemKind -> Discovery -> Item -> (MU.Part, MU.Part)
partItem _coitem disco i =
  let genericName = jname i
      flav = flavourToName $ jflavour i
  in case jkind disco i of
    Nothing ->
      -- TODO: display the aspects even if item identity not known
      -- and deduce item identity if there's nothing except the aspects
      (MU.Text $ flav <+> genericName, "")
    Just _kid ->
      let effTs = map aspectToSuffix (jaspects i)
                  ++ map effectToSuffix (jeffects i)
          efffectText = case filter (not . T.null) effTs of
            [] -> ""
            [effT] -> effT
            _ -> "of many effects"
          turnedOff | jisOn i = ""
                    | otherwise = "{OFF}"  -- TODO: mark with colour
      in (MU.Text genericName, MU.Text $ efffectText <+> turnedOff)

partItemWs :: Kind.Ops ItemKind -> Discovery -> Int -> Item -> MU.Part
partItemWs coitem disco jcount i =
  let (name, stats) = partItem coitem disco i
  in MU.Phrase [MU.CarWs jcount name, stats]

partItemAW :: Kind.Ops ItemKind -> Discovery -> Item -> MU.Part
partItemAW coitem disco i =
  let (name, stats) = partItem coitem disco i
  in MU.AW $ MU.Phrase [name, stats]

partItemWownW :: Kind.Ops ItemKind -> MU.Part -> Discovery -> Item -> MU.Part
partItemWownW coitem partA disco i =
  let (name, stats) = partItem coitem disco i
  in MU.WownW partA $ MU.Phrase [name, stats]

-- TODO: also print some data from kind and from item
itemDesc :: Kind.Ops ItemKind -> Discovery -> Item -> Text
itemDesc coitem@Kind.Ops{okind} disco i =
  case jkind disco i of
    Nothing -> "This item is as unremarkable as can be."
    Just ik ->
      let (name, stats) = partItem coitem disco i
      in makePhrase [name, stats MU.:> ":"] <+> idesc (okind ik)
