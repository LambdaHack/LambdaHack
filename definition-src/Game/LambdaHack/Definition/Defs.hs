{-# LANGUAGE DeriveGeneric #-}
-- | Basic types for content definitions.
module Game.LambdaHack.Definition.Defs
  ( GroupName, displayGroupName
  , ContentId, contentIdIndex
  , ContentSymbol, displayContentSymbol
  , X, Y
  , Freqs, renameFreqs
  , Rarity, linearInterpolation
  , CStore(..), ppCStore, ppCStoreIn, verbCStore
  , SLore(..), ItemDialogMode(..), ppSLore, headingSLore
  , ppItemDialogMode, ppItemDialogModeIn, ppItemDialogModeFrom
  , Direction(..)
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Control.DeepSeq
import Data.Binary
import GHC.Generics (Generic)

import Game.LambdaHack.Definition.DefsInternal

-- | X spacial dimension for points and vectors.
type X = Int

-- | Y xpacial dimension for points and vectors.
type Y = Int

-- | For each group that the kind belongs to, denoted by a @GroupName@
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs c = [(GroupName c, Int)]

renameFreqs :: (Text -> Text) -> Freqs c -> Freqs c
renameFreqs f = map (first (GroupName . f . fromGroupName))

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
  | SBody  -- contains the sum of @SOrgan@, @STrunk@ and @SCondition@
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
ppSLore SBody = "body"

headingSLore :: SLore -> Text
headingSLore SItem = "miscellaneous item"
headingSLore SOrgan = "vital anatomic organ"
headingSLore STrunk = "autonomous entity"
headingSLore SCondition = "momentary bodily condition"
headingSLore SBlast = "explosion blast particle"
headingSLore SEmbed = "landmark feature"
headingSLore SBody = "body part"

ppItemDialogMode :: ItemDialogMode -> (Text, Text)
ppItemDialogMode (MStore cstore) = ppCStore cstore
ppItemDialogMode MOrgans = ("in", "body")
ppItemDialogMode MOwned = ("among", "our total team belongings")
ppItemDialogMode MSkills = ("among", "skills")
ppItemDialogMode (MLore SBody) = ("in", "body")
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
