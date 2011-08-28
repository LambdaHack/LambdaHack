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

unknownTile :: Tile
unknownTile = Tile TileKind.unknownId Nothing Nothing []

-- | The tile can be a door, but the player can't tell.
-- TODO: perhaps just compare the letter and colour.
canBeDoor :: Tile -> Bool
canBeDoor t =
  case TileKind.deDoor $ tkind t of
    Just (Just True) -> True
    _ ->
      TileKind.isRock (tkind t) ||
      TileKind.isUnknown (tkind t)

isUnknown :: Tile -> Bool
isUnknown = TileKind.isUnknown . tkind

hasFeature :: TileKind.Feature -> Tile -> Bool
hasFeature f t = L.elem f (TileKind.ufeature . TileKind.getKind . tkind $ t)

-- | Does not block vision
isClear :: Tile -> Bool
isClear = hasFeature TileKind.Clear

-- | Does not block land movement
isWalkable :: Tile -> Bool
isWalkable = hasFeature TileKind.Walkable

-- | Is lit on its own.
isLit :: Tile -> Bool
isLit = hasFeature TileKind.Lit

-- | Provides an exit from a room.
isExit :: Tile -> Bool
isExit = hasFeature TileKind.Exit

-- | Is a good candidate to deposit items, replace by other tiles, etc.
isBoring :: Tile -> Bool
isBoring t =
  let fs = TileKind.ufeature (TileKind.getKind (tkind t))
      optional = [TileKind.Exit, TileKind.Lit]
      mandatory = [TileKind.Walkable, TileKind.Clear]
  in fs \\ optional `L.elem` L.permutations mandatory
