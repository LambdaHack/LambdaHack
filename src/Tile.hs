module Tile where

import Control.Monad

import Data.Binary
import qualified Data.List as L

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

-- | The player can't tell if the tile is a secret door or not.
canBeSecretDoor :: Tile -> Bool
canBeSecretDoor t =
  let u = TileKind.getKind (tkind t)
      s = TileKind.getKind TileKind.doorSecretId
  in TileKind.usymbol u == TileKind.usymbol s &&
     TileKind.uname u == TileKind.uname s &&
     TileKind.ucolor u == TileKind.ucolor s &&
     TileKind.ucolor2 u == TileKind.ucolor2 s

isUnknown :: Tile -> Bool
isUnknown t = tkind t == TileKind.unknownId

isOpening :: Tile -> Bool
isOpening t = tkind t == TileKind.openingId

hasFeature :: TileKind.Feature -> Tile -> Bool
hasFeature f t = f `elem` (TileKind.ufeature . TileKind.getKind . tkind $ t)

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
  in fs L.\\ optional `L.elem` L.permutations mandatory
