{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Very basic types for content definitions with their internals exposed.
module Game.LambdaHack.Definition.DefsInternal
  ( GroupName(..), displayGroupName
  , ContentId, toContentId, fromContentId, contentIdIndex
  , ContentSymbol, toContentSymbol, displayContentSymbol
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Control.DeepSeq
import Data.Binary
import Data.Hashable

-- If ever needed, we can use a symbol table here, since content
-- is never serialized. But we'd need to cover the few cases
-- (e.g., @litemFreq@) where @GroupName@ goes into savegame.
newtype GroupName c = GroupName {fromGroupName :: Text}
  deriving (Show, Eq, Ord, Hashable, Binary, NFData)

-- | This does not need to be 1-1, so should not be used in place of the
-- 'Eq' instance, etc.
displayGroupName :: GroupName c -> Text
displayGroupName = fromGroupName

-- | Content identifiers for the content type @c@.
newtype ContentId c = ContentId Word16
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

toContentId :: Word16 -> ContentId c
{-# INLINE toContentId #-}
toContentId = ContentId

fromContentId :: ContentId c -> Word16
{-# INLINE fromContentId #-}
fromContentId (ContentId k) = k

contentIdIndex :: ContentId c -> Int
{-# INLINE contentIdIndex #-}
contentIdIndex (ContentId k) = fromEnum k

-- TODO: temporary, not to break compilation too soon:
--{--
type ContentSymbol c = Char
toContentSymbol :: Char -> ContentSymbol c
toContentSymbol = id
displayContentSymbol :: ContentSymbol c -> Char
displayContentSymbol = id
--}

-- TODO: The intended definitions. Error they are going to cause will
-- point out all the remaining item symbols hardwired in the engine
-- and make any future accidental hardwiring harder.
-- TODO2: extend to other content kinds than item kinds.
{-
-- | An abstract view on the symbol of a content item definition.
-- Hiding the constructor prevents hardwiring symbols inside the engine
-- by accident (this is still possible via conversion functions,
-- if one insists, so the abstraction is leaky, but that's fine).
newtype ContentSymbol c = ContentSymbol Char
  deriving (Show, Eq, Ord, Binary, NFData) -- Generic?

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
--}
