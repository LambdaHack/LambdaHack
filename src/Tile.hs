module Tile where

import Control.Monad

import Data.Binary
import qualified Data.List as L

import Item
import TileKind
import WorldLoc
import qualified Kind

data Tile = Tile
  { tkind     :: !(Kind.Id TileKind)
  , tteleport :: Maybe WorldLoc  -- TODO
  , tsecret   :: Maybe Int  -- TODO
  , titems    :: [Item]
  }
  deriving Show

instance Binary Tile where
  put (Tile t l s is) = put t >> put l >> put s >> put is
  get = liftM4 Tile get get get get

wallId, openingId, floorLightId, floorDarkId, unknownId, doorOpenId, doorClosedId, doorSecretId, stairsLightUpId, stairsLightDownId, stairsDarkUpId, stairsDarkDownId :: Kind.Id TileKind
wallId = Kind.getId (\ t -> usymbol t == '#' && (L.null $ ufeature t))
openingId = Kind.getId (\ t -> usymbol t == '.' && kindHasFeature Exit t)
floorLightId = Kind.getId (\ t -> usymbol t == '.' && kindHas [Lit] [Exit] t)
floorDarkId = Kind.getId (\ t -> usymbol t == '.' && kindHas [] [Exit, Lit] t)
unknownId = Kind.getId ((== ' ') . usymbol)
doorOpenId = Kind.getId (kindHasFeature Closable)
doorClosedId = Kind.getId (kindHasFeature Openable)
doorSecretId = Kind.getId (kindHasFeature Hidden)
stairsLightUpId = Kind.getId (kindHas [Lit, Climbable] [])
stairsLightDownId = Kind.getId (kindHas [Lit, Descendable] [])
stairsDarkUpId = Kind.getId (kindHas [Climbable] [Lit])
stairsDarkDownId = Kind.getId (kindHas [Descendable] [Lit])

unknownTile :: Tile
unknownTile = Tile unknownId Nothing Nothing []

-- | The player can't tell if the tile is a secret door or not.
canBeSecretDoor :: Tile -> Bool
canBeSecretDoor t =
  let u = Kind.getKind (tkind t)
      s = Kind.getKind doorSecretId
  in usymbol u == usymbol s &&
     uname u == uname s &&
     ucolor u == ucolor s &&
     ucolor2 u == ucolor2 s

isUnknown :: Tile -> Bool
isUnknown t = tkind t == unknownId

isOpening :: Tile -> Bool
isOpening t = tkind t == openingId

kindHasFeature :: Feature -> TileKind -> Bool
kindHasFeature f t = f `elem` ufeature t

kindHas :: [Feature] -> [Feature] -> TileKind -> Bool
kindHas yes no t = L.all (flip kindHasFeature t) yes &&
                   not (L.any (flip kindHasFeature t) no)

hasFeature :: Feature -> Tile -> Bool
hasFeature f t = kindHasFeature f (Kind.getKind $ tkind t)

-- | Does not block vision
isClear :: Tile -> Bool
isClear = hasFeature Clear

-- | Does not block land movement
isWalkable :: Tile -> Bool
isWalkable = hasFeature Walkable

-- | Is lit on its own.
isLit :: Tile -> Bool
isLit = hasFeature Lit

-- | Provides an exit from a room.
isExit :: Tile -> Bool
isExit = hasFeature Exit

-- | Is a good candidate to deposit items, replace by other tiles, etc.
isBoring :: Tile -> Bool
isBoring t =
  let fs = ufeature (Kind.getKind (tkind t))
      optional = [Exit, Lit]
      mandatory = [Walkable, Clear]
  in fs L.\\ optional `L.elem` L.permutations mandatory
