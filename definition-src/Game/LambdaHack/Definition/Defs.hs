{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TypeFamilies #-}
-- | Basic types for content definitions.
module Game.LambdaHack.Definition.Defs
  ( GroupName, toGroupName, fromGroupName
  , Freqs, Rarity, linearInterpolation
  , ContentId, toContentId, contentIdIndex
  , CStore(..), ppCStore, ppCStoreIn, verbCStore
  , SLore(..), ItemDialogMode(..), ppSLore, headingSLore
  , ppItemDialogMode, ppItemDialogModeIn, ppItemDialogModeFrom
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.DeepSeq
import           Data.Binary
import           Data.Hashable
import           Data.String (IsString (..))
import qualified Data.Text as T
import           GHC.Generics (Generic)

import qualified Game.LambdaHack.Core.PointArray as PointArray

-- If ever needed, we can use a symbol table here, since content
-- is never serialized. But we'd need to cover the few cases
-- (e.g., @litemFreq@) where @GroupName@ goes into savegame.
newtype GroupName a = GroupName {fromGroupName :: Text}
  deriving (Show, Eq, Ord, Hashable, Binary, Generic)

instance IsString (GroupName a) where
  fromString = GroupName . T.pack

instance NFData (GroupName a)

toGroupName :: Text -> GroupName a
{-# INLINE toGroupName #-}
toGroupName = GroupName

-- | For each group that the kind belongs to, denoted by a @GroupName@
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs a = [(GroupName a, Int)]

-- | Rarity on given depths.
type Rarity = [(Double, Int)]

-- We assume @dataset@ is sorted and between 0 and 10.
linearInterpolation :: Int -> Int -> Rarity -> Int
linearInterpolation !levelDepth !totalDepth !dataset =
  let findInterval :: (Double, Int) -> Rarity -> ((Double, Int), (Double, Int))
      findInterval x1y1 [] = (x1y1, (11, 0))
      findInterval !x1y1 ((!x, !y) : rest) =
        if fromIntegral levelDepth * 10 <= x * fromIntegral totalDepth
        then (x1y1, (x, y))
        else findInterval (x, y) rest
      ((x1, y1), (x2, y2)) = findInterval (0, 0) dataset
  in ceiling
     $ fromIntegral y1
       + fromIntegral (y2 - y1)
         * (fromIntegral levelDepth * 10 - x1 * fromIntegral totalDepth)
         / ((x2 - x1) * fromIntegral totalDepth)

-- | Content identifiers for the content type @c@.
newtype ContentId c = ContentId Word16
  deriving (Show, Eq, Ord, Enum, Binary, Generic)

instance PointArray.UnboxRepClass (ContentId k) where
  type UnboxRep (ContentId k) = Word16
  toUnboxRepUnsafe (ContentId k) = k
  fromUnboxRep = ContentId

instance Hashable (ContentId c)

toContentId :: Word16 -> ContentId c
{-# INLINE toContentId #-}
toContentId = ContentId

contentIdIndex :: ContentId k -> Int
{-# INLINE contentIdIndex #-}
contentIdIndex (ContentId k) = fromEnum k

-- | Actor's item stores.
data CStore =
    CGround
  | COrgan
  | CEqp
  | CInv
  | CSha
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary CStore

instance NFData CStore

ppCStore :: CStore -> (Text, Text)
ppCStore CGround = ("on", "the ground")
ppCStore COrgan = ("in", "body")
ppCStore CEqp = ("in", "equipment")
ppCStore CInv = ("in", "pack")  -- "inventory pack" overflows text too easily
ppCStore CSha = ("in", "shared stash")

ppCStoreIn :: CStore -> Text
ppCStoreIn c = let (tIn, t) = ppCStore c in tIn <+> t

verbCStore :: CStore -> Text
verbCStore CGround = "drop"
verbCStore COrgan = "implant"
verbCStore CEqp = "equip"
verbCStore CInv = "pack"
verbCStore CSha = "stash"

-- | Item slot and lore categories.
data SLore =
    SItem
  | SOrgan
  | STrunk
  | SCondition
  | SBlast
  | SEmbed
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary SLore

instance NFData SLore

data ItemDialogMode =
    MStore CStore  -- ^ a leader's store
  | MOrgans        -- ^ leader's organs
  | MOwned         -- ^ all party's items
  | MSkills        -- ^ not items, but determined by leader's items
  | MLore SLore    -- ^ not party's items, but all known generalized items
  | MPlaces        -- ^ not items at all, but definitely a lore
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData ItemDialogMode

instance Binary ItemDialogMode

ppSLore :: SLore -> Text
ppSLore SItem = "item"
ppSLore SOrgan = "organ"
ppSLore STrunk = "creature"
ppSLore SCondition = "condition"
ppSLore SBlast = "blast"
ppSLore SEmbed = "terrain"

headingSLore :: SLore -> Text
headingSLore SItem = "miscellaneous item"
headingSLore SOrgan = "vital anatomic organ"
headingSLore STrunk = "living creature"
headingSLore SCondition = "momentary bodily condition"
headingSLore SBlast = "explosion blast particle"
headingSLore SEmbed = "landmark feature"

ppItemDialogMode :: ItemDialogMode -> (Text, Text)
ppItemDialogMode (MStore cstore) = ppCStore cstore
ppItemDialogMode MOrgans = ("in", "body")
ppItemDialogMode MOwned = ("in", "our possession")
ppItemDialogMode MSkills = ("among", "skills")
ppItemDialogMode (MLore slore) = ("among", ppSLore slore <+> "lore")
ppItemDialogMode MPlaces = ("among", "place lore")

ppItemDialogModeIn :: ItemDialogMode -> Text
ppItemDialogModeIn c = let (tIn, t) = ppItemDialogMode c in tIn <+> t

ppItemDialogModeFrom :: ItemDialogMode -> Text
ppItemDialogModeFrom c = let (_tIn, t) = ppItemDialogMode c in "from" <+> t
