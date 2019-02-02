{-# LANGUAGE DeriveGeneric #-}
-- | The type of item aspects and its operations.
module Game.LambdaHack.Common.Container
  ( CStore(..), Container(..), SLore(..), ItemDialogMode(..)
  , ppCStore, ppCStoreIn, verbCStore, ppContainer, ppSLore, headingSLore
  , ppItemDialogMode, ppItemDialogModeIn, ppItemDialogModeFrom
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.DeepSeq
import Data.Binary
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Types
import Game.LambdaHack.Common.Point

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

-- | Item container type.
data Container =
    CFloor LevelId Point
  | CEmbed LevelId Point
  | CActor ActorId CStore
  | CTrunk FactionId LevelId Point   -- ^ for bootstrapping actor bodies
  deriving (Show, Eq, Ord, Generic)

instance Binary Container

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

ppContainer :: Container -> Text
ppContainer CFloor{} = "nearby"
ppContainer CEmbed{} = "embedded nearby"
ppContainer (CActor _ cstore) = ppCStoreIn cstore
ppContainer c@CTrunk{} = error $ "" `showFailure` c

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
