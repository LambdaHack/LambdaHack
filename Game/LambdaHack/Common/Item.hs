{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
-- No operation in this module involves the state or any of our custom monads.
module Game.LambdaHack.Common.Item
  ( -- * The @Item@ type
    ItemId, Item(..), seedToAspectsEffects
    -- * Item discovery types
  , ItemKindIx, Discovery, ItemSeed, ItemAspectEffect(..), DiscoAE
  , KisOn, ItemFull(..), ItemDisco(..), itemK, itemIsOn, itemNoDisco
    -- * Inventory management types
  , ItemBag, ItemDict, ItemKnown
    -- * Textual description
  , partItem, partItemWs, partItemAW, partItemWownW, itemDesc, textAllAE
  ) where

import qualified Control.Monad.State as St
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Hashable as Hashable
import qualified Data.Ix as Ix
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU
import System.Random (mkStdGen)

import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.ItemFeature as IF
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ItemKind

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
            [_, effT1, effT2] -> "(" <> effT1 <> "," <+> effT2 <> ")"
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
