{-# LANGUAGE CPP #-}
-- | Operations concerning dungeon level tiles.
--
-- Unlike for many other content types, there is no type @Tile@,
-- of particular concrete tiles in the dungeon,
-- corresponding to 'TileKind' (the type of kinds of terrain tiles).
-- This is because the tiles are too numerous and there's not enough
-- storage space for a well-rounded @Tile@ type, on one hand,
-- and on the other hand, tiles are accessed
-- too often in performance critical code
-- to try to compress their representation and/or recompute them.
-- Instead, of defining a @Tile@ type, we express various properties
-- of concrete tiles by arrays or sparse EnumMaps, as appropriate.
--
-- Actors at normal speed (2 m/s) take one turn to move one tile (1 m by 1 m).
module Game.LambdaHack.Common.Tile
  ( SmellTime
  , kindHasFeature, hasFeature
  , isClear, isLit, isWalkable
  , isPassable, isPassableNoSuspect, isPassableNoClosed, isDoor, isSuspect
  , isExplorable, lookSimilar, speedup
  , openTo, closeTo, embedItems, causeEffects, revealAs, hideAs
  , isOpenable, isClosable, isChangeable, isEscape, isStair, ascendTo
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , createTab, accessTab
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Array.Unboxed as A

import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.TileKind (Tab (..), TileKind, TileSpeedup (..))
import qualified Game.LambdaHack.Content.TileKind as TK

-- | The last time a hero left a smell in a given tile. To be used
-- by monsters that hunt by smell.
type SmellTime = Time

createTab :: Kind.Ops TileKind -> (TileKind -> Bool) -> Tab
createTab Kind.Ops{ofoldrWithKey, obounds} p =
  let f _ k acc = p k : acc
      clearAssocs = ofoldrWithKey f []
  in Tab $ A.listArray obounds clearAssocs

accessTab :: Tab -> Kind.Id TileKind -> Bool
{-# INLINE accessTab #-}
accessTab (Tab tab) ki = tab A.! ki

-- | Whether a tile kind has the given feature.
kindHasFeature :: TK.Feature -> TileKind -> Bool
{-# INLINE kindHasFeature #-}
kindHasFeature f t = f `elem` TK.tfeature t

-- | Whether a tile kind (specified by its id) has the given feature.
hasFeature :: Kind.Ops TileKind -> TK.Feature -> Kind.Id TileKind -> Bool
{-# INLINE hasFeature #-}
hasFeature Kind.Ops{okind} f t = kindHasFeature f (okind t)

-- | Whether a tile does not block vision.
-- Essential for efficiency of "FOV", hence tabulated.
isClear :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isClear #-}
isClear TileSpeedup{isClearTab} = accessTab isClearTab

-- | Whether a tile is lit on its own.
-- Essential for efficiency of "Perception", hence tabulated.
isLit :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isLit #-}
isLit TileSpeedup{isLitTab} = accessTab isLitTab

-- | Whether actors can walk into a tile.
-- Essential for efficiency of pathfinding, hence tabulated.
isWalkable :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isWalkable #-}
isWalkable TileSpeedup{isWalkableTab} =
  accessTab isWalkableTab

-- | Whether actors can walk into a tile, perhaps opening a door first,
-- perhaps a hidden door.
-- Essential for efficiency of pathfinding, hence tabulated.
isPassable :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isPassable #-}
isPassable TileSpeedup{isPassableTab} =
  accessTab isPassableTab

-- | Whether actors can walk into a tile, perhaps trying to open
-- a hidden door first.
-- Essential for efficiency of pathfinding, hence tabulated.
isPassableNoSuspect :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isPassableNoSuspect #-}
isPassableNoSuspect TileSpeedup{isPassableNoSuspectTab} =
  accessTab isPassableNoSuspectTab

-- | Whether actors can walk into a tile, perhaps opening a door first,
-- perhaps trying a hidden door.
-- Essential for efficiency of pathfinding, hence tabulated.
isPassableNoClosed :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isPassableNoClosed #-}
isPassableNoClosed TileSpeedup{isPassableNoClosedTab} =
  accessTab isPassableNoClosedTab

-- | Whether a tile is a door, open or closed.
-- Essential for efficiency of pathfinding, hence tabulated.
isDoor :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isDoor #-}
isDoor TileSpeedup{isDoorTab} = accessTab isDoorTab

-- | Whether a tile is suspect.
-- Essential for efficiency of pathfinding, hence tabulated.
isSuspect :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isSuspect #-}
isSuspect TileSpeedup{isSuspectTab} =
  accessTab isSuspectTab

-- | Whether a tile kind (specified by its id) has a ChangeTo feature.
-- Essential for efficiency of pathfinding, hence tabulated.
isChangeable :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isChangeable #-}
isChangeable TileSpeedup{isChangeableTab} =
  accessTab isChangeableTab

-- | Whether one can easily explore a tile, possibly finding a treasure
-- or a clue. Doors can't be explorable since revealing a secret tile
-- should not change it's (walkable and) explorable status.
-- Door status should not depend on whether they are open or not
-- so that a foe opening a door doesn't force us to backtrack to explore it.
isExplorable :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isExplorable #-}
isExplorable coTileSpeedup t =
  (isWalkable coTileSpeedup t || isClear coTileSpeedup t) && not (isDoor coTileSpeedup t)

-- | The player can't tell one tile from the other.
lookSimilar :: TileKind -> TileKind -> Bool
{-# INLINE lookSimilar #-}
lookSimilar t u =
  TK.tsymbol t == TK.tsymbol u &&
  TK.tname   t == TK.tname   u &&
  TK.tcolor  t == TK.tcolor  u &&
  TK.tcolor2 t == TK.tcolor2 u

speedup :: Bool -> Kind.Ops TileKind -> TileSpeedup
speedup allClear cotile =
  -- Vectors pack bools as Word8 by default. No idea if the extra memory
  -- taken makes random lookups more or less efficient, so not optimizing
  -- further, until I have benchmarks.
  let isClearTab | allClear = createTab cotile
                              $ not . kindHasFeature TK.Impenetrable
                 | otherwise = createTab cotile
                               $ kindHasFeature TK.Clear
      isLitTab = createTab cotile $ not . kindHasFeature TK.Dark
      isWalkableTab = createTab cotile $ kindHasFeature TK.Walkable
      isPassableTab = createTab cotile $ isPassableKind True True
      isPassableNoSuspectTab = createTab cotile $ isPassableKind False True
      isPassableNoClosedTab = createTab cotile $ isPassableKind False False
      isDoorTab = createTab cotile $ \tk ->
        let getTo TK.OpenTo{} = True
            getTo TK.CloseTo{} = True
            getTo _ = False
        in any getTo $ TK.tfeature tk
      isSuspectTab = createTab cotile $ kindHasFeature TK.Suspect
      isChangeableTab = createTab cotile $ \tk ->
        let getTo TK.ChangeTo{} = True
            getTo _ = False
        in any getTo $ TK.tfeature tk
  in TileSpeedup {..}

isPassableKind :: Bool -> Bool -> TileKind -> Bool
isPassableKind passSuspect passOpenable tk =
  let getTo TK.Walkable = True
      getTo TK.OpenTo{} | passOpenable = True
      getTo TK.ChangeTo{} | passOpenable = True  -- can change to passable and may have loot
      getTo TK.Suspect | passSuspect = True
      getTo _ = False
  in any getTo $ TK.tfeature tk

openTo :: Kind.Ops TileKind -> Kind.Id TileKind -> Rnd (Kind.Id TileKind)
openTo Kind.Ops{okind, opick} t = do
  let getTo (TK.OpenTo grp) acc = grp : acc
      getTo _ acc = acc
  case foldr getTo [] $ TK.tfeature $ okind t of
    [] -> return t
    groups -> do
      grp <- oneOf groups
      fromMaybe (assert `failure` grp) <$> opick grp (const True)

closeTo :: Kind.Ops TileKind -> Kind.Id TileKind -> Rnd (Kind.Id TileKind)
closeTo Kind.Ops{okind, opick} t = do
  let getTo (TK.CloseTo grp) acc = grp : acc
      getTo _ acc = acc
  case foldr getTo [] $ TK.tfeature $ okind t of
    [] -> return t
    groups -> do
      grp <- oneOf groups
      fromMaybe (assert `failure` grp) <$> opick grp (const True)

embedItems :: Kind.Ops TileKind -> Kind.Id TileKind -> [GroupName ItemKind]
embedItems Kind.Ops{okind} t =
  let getTo (TK.Embed eff) acc = eff : acc
      getTo _ acc = acc
  in foldr getTo [] $ TK.tfeature $ okind t

causeEffects :: Kind.Ops TileKind -> Kind.Id TileKind -> [IK.Effect]
causeEffects Kind.Ops{okind} t =
  let getTo (TK.Cause eff) acc = eff : acc
      getTo _ acc = acc
  in foldr getTo [] $ TK.tfeature $ okind t

revealAs :: Kind.Ops TileKind -> Kind.Id TileKind -> Rnd (Kind.Id TileKind)
revealAs Kind.Ops{okind, opick} t = do
  let getTo (TK.RevealAs grp) acc = grp : acc
      getTo _ acc = acc
  case foldr getTo [] $ TK.tfeature $ okind t of
    [] -> return t
    groups -> do
      grp <- oneOf groups
      fromMaybe (assert `failure` grp) <$> opick grp (const True)

hideAs :: Kind.Ops TileKind -> Kind.Id TileKind -> Kind.Id TileKind
hideAs Kind.Ops{okind, ouniqGroup} t =
  let getTo (TK.HideAs grp) _ = Just grp
      getTo _ acc = acc
  in case foldr getTo Nothing (TK.tfeature (okind t)) of
       Nothing    -> t
       Just grp -> ouniqGroup grp

-- | Whether a tile kind (specified by its id) has an OpenTo feature.
isOpenable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isOpenable Kind.Ops{okind} t =
  let getTo TK.OpenTo{} = True
      getTo _ = False
  in any getTo $ TK.tfeature $ okind t

-- | Whether a tile kind (specified by its id) has a CloseTo feature.
isClosable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isClosable Kind.Ops{okind} t =
  let getTo TK.CloseTo{} = True
      getTo _ = False
  in any getTo $ TK.tfeature $ okind t

isEscape :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isEscape cotile t = let isEffectEscape IK.Escape{} = True
                        isEffectEscape _ = False
                    in any isEffectEscape $ causeEffects cotile t

isStair :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isStair cotile t = let isEffectAscend IK.Ascend{} = True
                       isEffectAscend _ = False
                   in any isEffectAscend $ causeEffects cotile t

ascendTo :: Kind.Ops TileKind -> Kind.Id TileKind -> [Int]
ascendTo cotile t =
  let getTo (IK.Ascend k) acc = k : acc
      getTo _ acc = acc
  in foldr getTo [] (causeEffects cotile t)
