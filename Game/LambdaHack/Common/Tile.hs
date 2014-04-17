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
  , isClear, isLit, isWalkable, isPassable, isDoor, isSuspect
  , isExplorable, lookSimilar, speedup
  , openTo, closeTo, causeEffects, revealAs, hideAs
  , isOpenable, isClosable, isChangeable, isEscape, isStair
  ) where

import Control.Exception.Assert.Sugar
import Data.Maybe

import qualified Game.LambdaHack.Common.Effect as Effect
import qualified Game.LambdaHack.Common.Feature as F
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.TileKind

-- | The last time a hero left a smell in a given tile. To be used
-- by monsters that hunt by smell.
type SmellTime = Time

-- | Whether a tile kind has the given feature.
kindHasFeature :: F.Feature -> TileKind -> Bool
{-# INLINE kindHasFeature #-}
kindHasFeature f t = f `elem` tfeature t

-- | Whether a tile kind (specified by its id) has the given feature.
hasFeature :: Kind.Ops TileKind -> F.Feature -> Kind.Id TileKind -> Bool
{-# INLINE hasFeature #-}
hasFeature Kind.Ops{okind} f t = kindHasFeature f (okind t)

-- | Whether a tile does not block vision.
-- Essential for efficiency of "FOV", hence tabulated.
isClear :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
{-# INLINE isClear #-}
isClear Kind.Ops{ospeedup = Just Kind.TileSpeedup{isClearTab}} =
  \k -> Kind.accessTab isClearTab k
isClear cotile = assert `failure` "no speedup" `twith` Kind.obounds cotile

-- | Whether a tile is lit on its own.
-- Essential for efficiency of "Perception", hence tabulated.
isLit :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
{-# INLINE isLit #-}
isLit Kind.Ops{ospeedup = Just Kind.TileSpeedup{isLitTab}} =
  \k -> Kind.accessTab isLitTab k
isLit cotile = assert `failure` "no speedup" `twith` Kind.obounds cotile

-- | Whether actors can walk into a tile.
-- Essential for efficiency of pathfinding, hence tabulated.
isWalkable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
{-# INLINE isWalkable #-}
isWalkable Kind.Ops{ospeedup = Just Kind.TileSpeedup{isWalkableTab}} =
  \k -> Kind.accessTab isWalkableTab k
isWalkable cotile = assert `failure` "no speedup" `twith` Kind.obounds cotile

-- | Whether actors can walk into a tile, perhaps opening a door first,
-- perhaps a hidden door.
-- Essential for efficiency of pathfinding, hence tabulated.
isPassable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
{-# INLINE isPassable #-}
isPassable Kind.Ops{ospeedup = Just Kind.TileSpeedup{isPassableTab}} =
  \k -> Kind.accessTab isPassableTab k
isPassable cotile = assert `failure` "no speedup" `twith` Kind.obounds cotile

-- | Whether a tile is a door, open or closed.
-- Essential for efficiency of pathfinding, hence tabulated.
isDoor :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
{-# INLINE isDoor #-}
isDoor Kind.Ops{ospeedup = Just Kind.TileSpeedup{isDoorTab}} =
  \k -> Kind.accessTab isDoorTab k
isDoor cotile = assert `failure` "no speedup" `twith` Kind.obounds cotile

-- | Whether a tile is suspect.
-- Essential for efficiency of pathfinding, hence tabulated.
isSuspect :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
{-# INLINE isSuspect #-}
isSuspect Kind.Ops{ospeedup = Just Kind.TileSpeedup{isSuspectTab}} =
  \k -> Kind.accessTab isSuspectTab k
isSuspect cotile = assert `failure` "no speedup" `twith` Kind.obounds cotile

-- | Whether a tile kind (specified by its id) has a ChangeTo feature.
-- Essential for efficiency of pathfinding, hence tabulated.
isChangeable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
{-# INLINE isChangeable #-}
isChangeable Kind.Ops{ospeedup = Just Kind.TileSpeedup{isChangeableTab}} =
  \k -> Kind.accessTab isChangeableTab k
isChangeable cotile = assert `failure` "no speedup" `twith` Kind.obounds cotile

-- | Whether a tile can be explored, possibly yielding a treasure.
-- Note that non-walkable tiles can hold treasure, e.g., caches,
-- so they should be set changeable to make them explorable.
isExplorable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
{-# INLINE isExplorable #-}
isExplorable cotile t = isWalkable cotile t || isDoor cotile t

-- | The player can't tell one tile from the other.
lookSimilar :: TileKind -> TileKind -> Bool
{-# INLINE lookSimilar #-}
lookSimilar t u =
  tsymbol t == tsymbol u &&
  tname   t == tname   u &&
  tcolor  t == tcolor  u &&
  tcolor2 t == tcolor2 u

speedup :: Bool -> Kind.Ops TileKind -> Kind.Speedup TileKind
speedup allClear cotile =
  -- Vectors pack bools as Word8 by default. No idea if the extra memory
  -- taken makes random lookups more or less efficient, so not optimizing
  -- further, until I have benchmarks.
  let isClearTab | allClear = Kind.createTab cotile
                              $ not . kindHasFeature F.Impenetrable
                 | otherwise = Kind.createTab cotile
                               $ kindHasFeature F.Clear
      isLitTab = Kind.createTab cotile $ not . kindHasFeature F.Dark
      isWalkableTab = Kind.createTab cotile $ kindHasFeature F.Walkable
      isPassableTab = Kind.createTab cotile $ \tk ->
        let getTo F.Walkable = True
            getTo F.OpenTo{} = True
            getTo F.Suspect = True
            getTo _ = False
        in any getTo $ tfeature tk
      isDoorTab = Kind.createTab cotile $ \tk ->
        let getTo F.OpenTo{} = True
            getTo F.CloseTo{} = True
            getTo _ = False
        in any getTo $ tfeature tk
      isSuspectTab = Kind.createTab cotile $ kindHasFeature F.Suspect
      isChangeableTab = Kind.createTab cotile $ \tk ->
        let getTo F.ChangeTo{} = True
            getTo _ = False
        in any getTo $ tfeature tk
  in Kind.TileSpeedup {..}

openTo :: Kind.Ops TileKind -> Kind.Id TileKind -> Rnd (Kind.Id TileKind)
openTo Kind.Ops{okind, opick} t = do
  let getTo (F.OpenTo group) acc = group : acc
      getTo _ acc = acc
  case foldr getTo [] $ tfeature $ okind t of
    [] -> return t
    groups -> do
      group <- oneOf groups
      fmap (fromMaybe $ assert `failure` group)
        $ opick group (const True)

closeTo :: Kind.Ops TileKind -> Kind.Id TileKind -> Rnd (Kind.Id TileKind)
closeTo Kind.Ops{okind, opick} t = do
  let getTo (F.CloseTo group) acc = group : acc
      getTo _ acc = acc
  case foldr getTo [] $ tfeature $ okind t of
    [] -> return t
    groups -> do
      group <- oneOf groups
      fmap (fromMaybe $ assert `failure` group)
        $ opick group (const True)

causeEffects :: Kind.Ops TileKind -> Kind.Id TileKind -> [Effect.Effect Int]
causeEffects Kind.Ops{okind} t = do
  let getTo (F.Cause eff) acc = eff : acc
      getTo _ acc = acc
  foldr getTo [] $ tfeature $ okind t

revealAs :: Kind.Ops TileKind -> Kind.Id TileKind -> Rnd (Kind.Id TileKind)
revealAs Kind.Ops{okind, opick} t = do
  let getTo (F.RevealAs group) acc = group : acc
      getTo _ acc = acc
  case foldr getTo [] $ tfeature $ okind t of
    [] -> return t
    groups -> do
      group <- oneOf groups
      fmap (fromMaybe $ assert `failure` group)
        $ opick group (const True)

hideAs :: Kind.Ops TileKind -> Kind.Id TileKind -> Kind.Id TileKind
hideAs Kind.Ops{okind, ouniqGroup} t =
  let getTo (F.HideAs group) _ = Just group
      getTo _ acc = acc
  in case foldr getTo Nothing (tfeature (okind t)) of
       Nothing    -> t
       Just group -> ouniqGroup group

-- | Whether a tile kind (specified by its id) has an OpenTo feature.
isOpenable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isOpenable Kind.Ops{okind} t =
  let getTo F.OpenTo{} = True
      getTo _ = False
  in any getTo $ tfeature $ okind t

-- | Whether a tile kind (specified by its id) has a CloseTo feature.
isClosable :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isClosable Kind.Ops{okind} t =
  let getTo F.CloseTo{} = True
      getTo _ = False
  in any getTo $ tfeature $ okind t

isEscape :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isEscape cotile t = let isEffectEscape Effect.Escape{} = True
                        isEffectEscape _ = False
                    in any isEffectEscape $ causeEffects cotile t

isStair :: Kind.Ops TileKind -> Kind.Id TileKind -> Bool
isStair cotile t = let isEffectAscend Effect.Ascend{} = True
                       isEffectAscend _ = False
                   in any isEffectAscend $ causeEffects cotile t
