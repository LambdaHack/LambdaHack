module Tile where

import Control.Monad

import Data.Binary
import qualified Data.List as L

import Item
import Content.TileKind
import qualified Feature as F
import WorldLoc
import qualified Kind

data Tile = Tile
  { tkind     :: !(Kind.Id TileKind)
  , tteleport :: TeleLoc  -- TODO
  , tsecret   :: Maybe Int  -- TODO
  , titems    :: [Item]
  }
  deriving Show

type TeleLoc = Maybe WorldLoc

instance Binary Tile where
  put (Tile t l s is) = put t >> put l >> put s >> put is
  get = liftM4 Tile get get get get

wallId, openingId, floorLightId, floorDarkId, unknownId, doorOpenId, doorClosedId, doorSecretId, stairsLightUpId, stairsLightDownId, stairsDarkUpId, stairsDarkDownId :: Kind.Id TileKind
wallId = Kind.getId (\ t -> usymbol t == '#' && (L.null $ ufeature t))
openingId = Kind.getId (\ t -> usymbol t == '.' && kindHasFeature F.Exit t)
floorLightId =
  Kind.getId (\ t -> usymbol t == '.' && kindHas [F.Lit] [F.Exit] t)
floorDarkId =
  Kind.getId (\ t -> usymbol t == '.' && kindHas [] [F.Exit, F.Lit] t)
unknownId = Kind.getId ((== ' ') . usymbol)
doorOpenId = Kind.getId (kindHasFeature F.Closable)
doorClosedId = Kind.getId (kindHasFeature F.Openable)
doorSecretId = Kind.getId (kindHasFeature F.Hidden)
stairsLightUpId = Kind.getId (kindHas [F.Lit, F.Climbable] [])
stairsLightDownId = Kind.getId (kindHas [F.Lit, F.Descendable] [])
stairsDarkUpId = Kind.getId (kindHas [F.Climbable] [F.Lit])
stairsDarkDownId = Kind.getId (kindHas [F.Descendable] [F.Lit])

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

kindHasFeature :: F.Feature -> TileKind -> Bool
kindHasFeature f t = f `elem` ufeature t

kindHas :: [F.Feature] -> [F.Feature] -> TileKind -> Bool
kindHas yes no t = L.all (flip kindHasFeature t) yes &&
                   not (L.any (flip kindHasFeature t) no)

hasFeature :: F.Feature -> Tile -> Bool
hasFeature f t = kindHasFeature f (Kind.getKind $ tkind t)

-- | Does not block vision
isClear :: Tile -> Bool
isClear = hasFeature F.Clear

-- | Does not block land movement
isWalkable :: Tile -> Bool
isWalkable = hasFeature F.Walkable

-- | Is lit on its own.
isLit :: Tile -> Bool
isLit = hasFeature F.Lit

-- | Provides an exit from a room.
isExit :: Tile -> Bool
isExit = hasFeature F.Exit

-- | Is a good candidate to deposit items, replace by other tiles, etc.
isBoring :: Tile -> Bool
isBoring t =
  let fs = ufeature (Kind.getKind (tkind t))
      optional = [F.Exit, F.Lit]
      mandatory = [F.Walkable, F.Clear]
  in fs L.\\ optional `L.elem` L.permutations mandatory
