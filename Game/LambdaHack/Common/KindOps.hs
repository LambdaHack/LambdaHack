{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
-- | General content types and operations.
module Game.LambdaHack.Common.KindOps
  ( Id(Id), Ops(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random

-- | Content identifiers for the content type @c@.
newtype Id c = Id Word8
  deriving (Show, Eq, Ord, Enum, Bounded, Binary)

-- | Content operations for the content of type @a@.
data Ops a = Ops
  { okind         :: !(Id a -> a)  -- ^ content element at given id
  , ouniqGroup    :: !(GroupName a -> Id a)
                                   -- ^ the id of the unique member of
                                   --   a singleton content group
  , opick         :: !(GroupName a -> (a -> Bool) -> Rnd (Maybe (Id a)))
                                   -- ^ pick a random id belonging to a group
                                   --   and satisfying a predicate
  , ofoldrWithKey :: !(forall b. (Id a -> a -> b -> b) -> b -> b)
                                   -- ^ fold over all content elements of @a@
  , ofoldlWithKey' :: !(forall b. (b -> Id a -> a -> b) -> b -> b)
                                   -- ^ fold strictly over all content @a@
  , ofoldrGroup   :: !(forall b.
                     GroupName a -> (Int -> Id a -> a -> b -> b) -> b -> b)
                                   -- ^ fold over the given group only
  , olength       :: !Int          -- ^ size of content @a@
  }
