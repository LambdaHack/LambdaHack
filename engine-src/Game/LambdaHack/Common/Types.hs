{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Abstract identifiers for the main types in the engine. This is imported
-- by modules that don't need to know the internal structure
-- of the types. As a side effect, this prevents mutual dependencies
-- among modules.
module Game.LambdaHack.Common.Types
  ( ItemId, FactionId, LevelId, ActorId
  , Container(..)
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Binary
import Data.Hashable
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Point
import Game.LambdaHack.Definition.Defs

-- | A unique identifier of an item in the dungeon.
newtype ItemId = ItemId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- | A unique identifier of a faction in a game. It's assigned in the order
-- from game mode roster, starting from one. We keep the @FactionId@
-- and @TeamContinuity@ types separate mostly to let @FactionId@ reflect
-- the order, which influences starting faction positions, etc.
-- We use @TeamContinuity@ for dictionaries containing teams that may
-- or may not be active factions in the current game, while @FactionId@ are
-- used only for factions in the game (in particular, because they vary
-- depending on order in game mode roster, while @TeamContinuity@ are stable).
newtype FactionId = FactionId Int
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

-- | Abstract level identifiers.
newtype LevelId = LevelId Int
  deriving (Show, Eq, Ord, Hashable, Binary)

instance Enum LevelId where
  fromEnum (LevelId n) = n
  toEnum = LevelId  -- picks the main branch of the dungeon

-- | A unique identifier of an actor in the dungeon.
newtype ActorId = ActorId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- | Item container type.
data Container =
    CFloor LevelId Point
  | CEmbed LevelId Point
  | CActor ActorId CStore
  | CTrunk FactionId LevelId Point   -- ^ for bootstrapping actor bodies
  deriving (Show, Eq, Ord, Generic)

instance Binary Container
