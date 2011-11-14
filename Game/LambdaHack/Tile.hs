module Game.LambdaHack.Tile where

import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import Data.Binary

import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Geometry

newtype SecretStrength = SecretStrength{secretStrength :: Time}
  deriving (Show, Eq, Ord)
instance Binary SecretStrength where
  put = put . secretStrength
  get = fmap SecretStrength get

-- TODO: remove this file

wallId, openingId, floorLightId, floorDarkId, unknownId, doorOpenId, doorClosedId, doorSecretId, stairsUpId, stairsDownId :: Kind.Id TileKind
wallId = Kind.getId (\ t -> tsymbol t == '#' && (L.null $ tfeature t))
openingId = Kind.getId (\ t -> tsymbol t == '.' && kindHasFeature F.Exit t)
floorLightId =
  Kind.getId (\ t -> tsymbol t == '.' && kindHas [F.Lit] [F.Exit] t)
floorDarkId =
  Kind.getId (\ t -> tsymbol t == '.' && kindHas [] [F.Exit, F.Lit] t)
unknownId = Kind.getId ((== ' ') . tsymbol)
doorOpenId = Kind.getId (kindHasFeature F.Closable)
doorClosedId = Kind.getId (kindHasFeature F.Openable)
doorSecretId = Kind.getId (kindHasFeature F.Hidden)
stairsUpId = Kind.getId (kindHas [F.Lit, F.Climbable] [])
stairsDownId = Kind.getId (kindHas [F.Lit, F.Descendable] [])

-- | The player can't tell if the tile is a secret door or not.
canBeSecretDoor :: Kind.COps -> Kind.Id TileKind -> Bool
canBeSecretDoor Kind.COps{cotile=Kind.Ops{ofindKind}} t =
  let u = ofindKind t
      s = ofindKind doorSecretId
  in tsymbol u == tsymbol s &&
     tname u == tname s &&
     tcolor u == tcolor s &&
     tcolor2 u == tcolor2 s

isUnknown :: Kind.Id TileKind -> Bool
isUnknown t = t == unknownId

isOpening :: Kind.Id TileKind -> Bool
isOpening t = t == openingId

kindHasFeature :: F.Feature -> TileKind -> Bool
kindHasFeature f t = f `elem` tfeature t

kindHas :: [F.Feature] -> [F.Feature] -> TileKind -> Bool
kindHas yes no t = L.all (flip kindHasFeature t) yes &&
                   not (L.any (flip kindHasFeature t) no)

hasFeature ::Kind.COps ->  F.Feature -> Kind.Id TileKind -> Bool
hasFeature Kind.COps{cotile=Kind.Ops{ofindKind}} f t =
  kindHasFeature f (ofindKind t)

-- | Does not block vision. Essential for efficiency of FOV, hence tabulated.
clearTab :: Kind.COps -> A.UArray (Kind.Id TileKind) Bool
clearTab Kind.COps{cotile=Kind.Ops{ofoldrWithKey, obounds}} =
  let f _ k acc = kindHasFeature F.Clear k : acc
      clearAssocs = ofoldrWithKey f []
  in A.listArray obounds clearAssocs

-- TODO: now it's created many times, optimize
isClear :: Kind.COps -> Kind.Id TileKind -> Bool
isClear scops =
  let tab = clearTab scops
  in (tab A.!)

-- | Is lit on its own. Essential for efficiency of Perception, hence tabulated.
litTab :: Kind.COps -> A.UArray (Kind.Id TileKind) Bool
litTab Kind.COps{cotile=Kind.Ops{ofoldrWithKey, obounds}} =
  let f _ k acc = kindHasFeature F.Lit k : acc
      litAssocs = ofoldrWithKey f []
  in A.listArray obounds litAssocs

-- TODO: now it's created many times, optimize
isLit :: Kind.COps -> Kind.Id TileKind -> Bool
isLit scops =
  let tab = litTab scops
  in (tab A.!)

-- | Does not block land movement.
isWalkable :: Kind.COps -> Kind.Id TileKind -> Bool
isWalkable cops = hasFeature cops F.Walkable


-- | Provides an exit from a room.
isExit :: Kind.COps -> Kind.Id TileKind -> Bool
isExit cops = hasFeature cops F.Exit

-- | Is a good candidate to deposit items, replace by other tiles, etc.
isBoring :: Kind.COps -> Kind.Id TileKind -> Bool
isBoring Kind.COps{cotile=Kind.Ops{ofindKind}} t =
  let fs = tfeature (ofindKind t)
      optional = [F.Exit, F.Lit]
      mandatory = [F.Walkable, F.Clear]
  in fs L.\\ optional `L.elem` L.permutations mandatory
