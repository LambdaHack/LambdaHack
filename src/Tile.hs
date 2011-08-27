module Tile where

import Control.Monad

import Data.Binary
import Data.List as L

import Item
import qualified Terrain
import WorldLoc

data Tile = Tile
              { tterrain :: Terrain.Terrain
              , tteleport :: Maybe WorldLoc  -- TODO
              , titems   :: [Item] }
  deriving Show

instance Binary Tile where
  put (Tile t l is) = put t >> put l >> put is
  get = liftM3 Tile get get get

unknownTile :: Tile
unknownTile = Tile Terrain.unknown Nothing []

-- | blocks moves and vision
closed :: Tile -> Bool
closed = not . open

floor :: Tile -> Bool
floor = Terrain.isFloor . tterrain

-- | The tile can be a door, but the player can't tell.
-- TODO: perhaps just compare the letter and colour.
canBeDoor :: Tile -> Bool
canBeDoor t =
  case Terrain.deDoor $ tterrain t of
    Just (Just n) | n > 0 -> True
    _ ->
      Terrain.isRock (tterrain t) ||
      Terrain.isUnknown (tterrain t)

isUnknown :: Tile -> Bool
isUnknown = Terrain.isUnknown . tterrain

-- | allows moves and vision
open :: Tile -> Bool
open = Terrain.isOpen . tterrain

-- | is lighted on its own
light :: Tile -> Bool
light = Terrain.isAlight . tterrain

-- | marks an exit from a room
isExit :: Tile -> Bool
isExit = Terrain.isExit . tterrain
