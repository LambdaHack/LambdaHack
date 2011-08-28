module Tile where

import Control.Monad

import Data.Binary
import Data.List as L

import Item
import qualified TileKind
import WorldLoc

data Tile = Tile
              { tkind     :: !TileKind.TileKindId
              , tteleport :: Maybe WorldLoc  -- TODO
              , tsecret   :: Maybe Int  -- TODO
              , titems    :: [Item] }
  deriving Show

instance Binary Tile where
  put (Tile t l s is) = put t >> put l >> put s >> put is
  get = liftM4 Tile get get get get

tterrain = tkind

unknownTile :: Tile
unknownTile = Tile TileKind.unknownId Nothing Nothing []

-- | blocks moves and vision
closed :: Tile -> Bool
closed = not . open

floor :: Tile -> Bool
floor = TileKind.isFloor . tterrain

-- | The tile can be a door, but the player can't tell.
-- TODO: perhaps just compare the letter and colour.
canBeDoor :: Tile -> Bool
canBeDoor t =
  case TileKind.deDoor $ tterrain t of
    Just (Just True) -> True
    _ ->
      TileKind.isRock (tterrain t) ||
      TileKind.isUnknown (tterrain t)

isUnknown :: Tile -> Bool
isUnknown = TileKind.isUnknown . tterrain

-- | allows moves and vision
open :: Tile -> Bool
open = TileKind.isOpen . tterrain

-- | is lighted on its own
light :: Tile -> Bool
light = TileKind.isAlight . tterrain

-- | marks an exit from a room
isExit :: Tile -> Bool
isExit = TileKind.isExit . tterrain
