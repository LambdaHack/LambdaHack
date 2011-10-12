module Tile where

import Control.Monad

import Data.Binary
import qualified Data.List as L

import Item
import TileKind
import WorldLoc
import qualified Kind
import Geometry

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
doorSecretId = Kind.getId (kindHas [Change '+'] [Closable])
stairsLightUpId = Kind.getId (kindHas [Lit, Climbable] [])
stairsLightDownId = Kind.getId (kindHas [Lit, Descendable] [])
stairsDarkUpId = Kind.getId (kindHas [Climbable] [Lit])
stairsDarkDownId = Kind.getId (kindHas [Descendable] [Lit])

stairs :: Bool -> VDir -> Kind.Id TileKind
stairs True Up    = stairsLightUpId
stairs True Down  = stairsLightDownId
stairs False Up   = stairsDarkUpId
stairs False Down = stairsDarkDownId

door :: Maybe Int -> Kind.Id TileKind
door Nothing  = doorOpenId
door (Just 0) = doorClosedId
door (Just _) = doorSecretId

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

-- TODO: clean these up

deStairs :: Kind.Id TileKind -> Maybe VDir
deStairs t =
  let isCD f = case f of Climbable -> True; Descendable -> True; _ -> False
      fk = ufeature (Kind.getKind t)
  in case L.filter isCD fk of
       [Climbable] -> Just Up
       [Descendable] -> Just Down
       _ -> Nothing

deDoor :: Kind.Id TileKind -> Maybe (Maybe Bool)
deDoor t
  | Closable `elem` ufeature (Kind.getKind t) = Just Nothing
  | Openable `elem` ufeature (Kind.getKind t) = Just (Just False)
  | let isSecret f = case f of Secret _ -> True; _ -> False
    in L.any isSecret (ufeature (Kind.getKind t)) = Just (Just True) -- TODO
  | otherwise = Nothing
