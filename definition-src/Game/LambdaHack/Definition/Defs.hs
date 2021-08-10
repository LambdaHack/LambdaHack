{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Basic types for content definitions.
module Game.LambdaHack.Definition.Defs
  ( X, Y, GroupName(..)
  , Freqs, Rarity, linearInterpolation
  , ContentId, toContentId, fromContentId, contentIdIndex
  , ContentSymbol, toContentSymbol, displayContentSymbol
  , CStore(..), ppCStore, ppCStoreIn, verbCStore
  , SLore(..), ItemDialogMode(..), ppSLore, headingSLore
  , ppItemDialogMode, ppItemDialogModeIn, ppItemDialogModeFrom
  , Direction(..)
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Control.DeepSeq
import Data.Binary
import Data.Hashable
import GHC.Generics (Generic)

-- | X spacial dimension for points and vectors.
type X = Int

-- | Y xpacial dimension for points and vectors.
type Y = Int

-- If ever needed, we can use a symbol table here, since content
-- is never serialized. But we'd need to cover the few cases
-- (e.g., @litemFreq@) where @GroupName@ goes into savegame.
newtype GroupName a = GroupName {fromGroupName :: Text}
  deriving (Show, Eq, Ord, Hashable, Binary, NFData)

-- | For each group that the kind belongs to, denoted by a @GroupName@
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs a = [(GroupName a, Int)]

-- | Rarity on given depths. The first element of the pair is normally
-- in (0, 10] interval and, e.g., if there are 20 levels, 0.5 represents
-- the first level and 10 the last. Exceptionally, it may be larger than 10,
-- meaning appearance in the dungeon is not possible under normal circumstances
-- and the value remains constant above the interval bound.
type Rarity = [(Double, Int)]

-- We assume depths are greater or equal to one and the rarity @dataset@
-- is non-empty, sorted and the first elements of the pairs are positive.
-- The convention for adding implicit outer intervals is that
-- the value increases linearly, starting from 0 at 0. Similarly,
-- if the last interval ends before 10, the value drops linearly,
-- in a way that would reach 0 a step after 10, but staying constant
-- from 10 onward. If the last interval ends after 10, the value stays constant
-- after the interval's upper bound.
--
-- Note that rarity [(1, 1)] means constant value 1 only thanks to @ceiling@.
-- OTOH, [(1, 10)] is not equivalent to [(10/150, 10)] in a 150-deep dungeon,
-- since its value at the first level is drastically lower. This only
-- matters if content creators mix the two notations, so care must be taken
-- in such cases. Otherwise, for any given level, all kinds scale consistently
-- and the simpler notation just paintes the dungeon in larger strokes.
linearInterpolation :: Int -> Int -> Rarity -> Int
linearInterpolation !levelDepthInt !totalDepthInt !dataset =
  let levelDepth10 = intToDouble $ levelDepthInt * 10
      totalDepth = intToDouble totalDepthInt
      findInterval :: (Double, Int) -> Rarity -> ((Double, Int), (Double, Int))
      findInterval x1y1@(x1Last, y1Last) [] =  -- we are past the last interval
        let stepLevel = 10 / totalDepth
              -- this is the distance representing one level, the same
              -- as the distance from 0 to the representation of level 1
            yConstant = if x1Last >= 10
                        then y1Last
                        else ceiling (intToDouble y1Last * stepLevel
                                      / (10 + stepLevel - x1Last))
              -- this is the value of the interpolation formula at the end
              -- with y2 == 0, levelDepth10 == totalDepth * 10,
              -- and x2 == 10 + stepLevel
        in if levelDepthInt > totalDepthInt  -- value stays constant
           then ((x1Last, yConstant), (x1Last + 1, yConstant))
                  -- this artificial interval is enough to emulate
                  -- the value staying constant indefinitely
           else (x1y1, (10 + stepLevel, 0))
      findInterval !x1y1 ((!x, !y) : rest) =
        if levelDepth10 <= x * totalDepth
        then (x1y1, (x, y))
        else findInterval (x, y) rest
      ((x1, y1), (x2, y2)) = findInterval (0, 0) dataset
  in y1 + ceiling
            (intToDouble (y2 - y1) * (levelDepth10 - x1 * totalDepth)
             / ((x2 - x1) * totalDepth))

-- | Content identifiers for the content type @c@.
newtype ContentId c = ContentId Word16
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

toContentId :: Word16 -> ContentId c
{-# INLINE toContentId #-}
toContentId = ContentId

fromContentId :: ContentId c -> Word16
{-# INLINE fromContentId #-}
fromContentId (ContentId k) = k

contentIdIndex :: ContentId k -> Int
{-# INLINE contentIdIndex #-}
contentIdIndex (ContentId k) = fromEnum k

-- TODO: temporary, not to break compilation too soon:
type ContentSymbol a = Char
toContentSymbol :: Char -> ContentSymbol c
toContentSymbol = id
displayContentSymbol :: ContentSymbol c -> Char
displayContentSymbol = id

-- TODO: The intended definitions. Error they are going to cause will
-- point out all the remaining item symbols hardwired in the engine
-- and make any future accidental hardwiring harder.
-- TODO2: extend to other content kinds than item kinds.
{-
-- | An abstract view on the symbol of a content item definition.
-- Hiding the constructor prevents hardwiring symbols inside the engine
-- by accident (this is still possible via conversion functions,
-- if one insists, so the abstraction is leaky, but that's fine).
newtype ContentSymbol a = ContentSymbol Char
  deriving (Show, Eq, Ord)

-- | This is a 1-1 inclusion. Don't use, if an equal named symbol already
-- exists in rules content.
toContentSymbol :: Char -> ContentSymbol c
{-# INLINE toContentSymbol #-}
toContentSymbol = ContentSymbol

-- | This does not need to be 1-1, so should not be used in place of the
-- 'Eq' instance, etc.
displayContentSymbol :: ContentSymbol c -> Char
{-# INLINE displayContentSymbol #-}
displayContentSymbol (ContentSymbol c) = c
-}

-- | Actor's item stores.
data CStore =
    CGround
  | COrgan
  | CEqp
  | CStash
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary CStore

instance NFData CStore

ppCStore :: CStore -> (Text, Text)
ppCStore CGround = ("on", "the ground")
ppCStore COrgan = ("in", "body")
ppCStore CEqp = ("in", "equipment outfit")
ppCStore CStash = ("in", "shared inventory stash")

ppCStoreIn :: CStore -> Text
ppCStoreIn c = let (tIn, t) = ppCStore c in tIn <+> t

verbCStore :: CStore -> Text
verbCStore CGround = "remove"
verbCStore COrgan = "implant"
verbCStore CEqp = "equip"
verbCStore CStash = "stash"

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
  | MPlaces        -- ^ places; not items at all, but definitely a lore
  | MModes         -- ^ scenarios; not items at all, but definitely a lore
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
headingSLore STrunk = "autonomous entity"
headingSLore SCondition = "momentary bodily condition"
headingSLore SBlast = "explosion blast particle"
headingSLore SEmbed = "landmark feature"

ppItemDialogMode :: ItemDialogMode -> (Text, Text)
ppItemDialogMode (MStore cstore) = ppCStore cstore
ppItemDialogMode MOrgans = ("in", "body")
ppItemDialogMode MOwned = ("among", "our total team belongings")
ppItemDialogMode MSkills = ("among", "skills")
ppItemDialogMode (MLore slore) = ("among", ppSLore slore <+> "lore")
ppItemDialogMode MPlaces = ("among", "place lore")
ppItemDialogMode MModes = ("among", "adventure lore")

ppItemDialogModeIn :: ItemDialogMode -> Text
ppItemDialogModeIn c = let (tIn, t) = ppItemDialogMode c in tIn <+> t

ppItemDialogModeFrom :: ItemDialogMode -> Text
ppItemDialogModeFrom c = let (_tIn, t) = ppItemDialogMode c in "from" <+> t

data Direction = Forward | Backward
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData Direction

instance Binary Direction
