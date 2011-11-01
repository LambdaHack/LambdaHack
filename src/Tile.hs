module Tile where

import qualified Data.List as L
import qualified Data.Array.Unboxed as A

import Content.TileKind
import qualified Feature as F
import qualified Kind

-- TODO: remove this file

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

-- | The player can't tell if the tile is a secret door or not.
canBeSecretDoor :: Kind.Id TileKind -> Bool
canBeSecretDoor t =
  let u = Kind.getKind t
      s = Kind.getKind doorSecretId
  in usymbol u == usymbol s &&
     uname u == uname s &&
     ucolor u == ucolor s &&
     ucolor2 u == ucolor2 s

isUnknown :: Kind.Id TileKind -> Bool
isUnknown t = t == unknownId

isOpening :: Kind.Id TileKind -> Bool
isOpening t = t == openingId

kindHasFeature :: F.Feature -> TileKind -> Bool
kindHasFeature f t = f `elem` ufeature t

kindHas :: [F.Feature] -> [F.Feature] -> TileKind -> Bool
kindHas yes no t = L.all (flip kindHasFeature t) yes &&
                   not (L.any (flip kindHasFeature t) no)

hasFeature :: F.Feature -> Kind.Id TileKind -> Bool
hasFeature f t = kindHasFeature f (Kind.getKind t)

-- | Does not block vision. Essential for efficiency of FOV, hence tabulated.
clearTab :: A.UArray (Kind.Id TileKind) Bool
clearTab = let f _ k acc = kindHasFeature F.Clear k : acc
               clearAssocs = Kind.foldrWithKey f []
           in A.listArray Kind.boundsId clearAssocs

isClear :: Kind.Id TileKind -> Bool
isClear i = clearTab A.! i

-- | Is lit on its own. Essential for efficiency of Perception, hence tabulated.
litTab :: A.UArray (Kind.Id TileKind) Bool
litTab = let f _ k acc = kindHasFeature F.Lit k : acc
             litAssocs = Kind.foldrWithKey f []
         in A.listArray Kind.boundsId litAssocs

isLit :: Kind.Id TileKind -> Bool
isLit i = litTab A.! i

-- | Does not block land movement.
isWalkable :: Kind.Id TileKind -> Bool
isWalkable = hasFeature F.Walkable


-- | Provides an exit from a room.
isExit :: Kind.Id TileKind -> Bool
isExit = hasFeature F.Exit

-- | Is a good candidate to deposit items, replace by other tiles, etc.
isBoring :: Kind.Id TileKind -> Bool
isBoring t =
  let fs = ufeature (Kind.getKind t)
      optional = [F.Exit, F.Lit]
      mandatory = [F.Walkable, F.Clear]
  in fs L.\\ optional `L.elem` L.permutations mandatory
