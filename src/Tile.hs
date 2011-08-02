module Tile where

import Control.Monad

import Data.Binary
import Data.List as L

import Item
import qualified Terrain

data Tile = Tile
              { tterrain :: Terrain.Terrain,
                titems   :: [Item] }
  deriving Show

instance Binary Tile where
  put (Tile t is) = put t >> put is
  get = liftM2 Tile get get

unknownTile :: Tile
unknownTile = Tile Terrain.unknown []

-- | blocks moves and vision
closed :: Tile -> Bool
closed = not . open

floor :: Tile -> Bool
floor = Terrain.isFloor . tterrain

canBeDoor :: Tile -> Bool
canBeDoor t =
  case Terrain.deDoor $ tterrain t of
    Just o | secret o -> True
    _ ->
      Terrain.isRock (tterrain t) ||
      Terrain.isUnknown (tterrain t)

secret :: Maybe Int -> Bool
secret (Just n) | n /= 0 = True
secret _ = False

isUnknown :: Tile -> Bool
isUnknown = Terrain.isUnknown . tterrain

toOpen :: Bool -> Maybe Int
toOpen True = Nothing
toOpen False = Just 0

-- | allows moves and vision
open :: Tile -> Bool
open = Terrain.isOpen . tterrain

-- | is lighted on its own
light :: Tile -> Bool
light = Terrain.isAlight . tterrain

-- | marks an exit from a room
isExit :: Tile -> Bool
isExit = Terrain.isExit . tterrain
