{-# LANGUAGE FlexibleContexts #-}
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
  ( kindHasFeature, hasFeature, isClear, isLit, isWalkable, isDoor, isSuspect
  , isExplorable, isOftenItem, isOftenActor, isNoItem, isNoActor, isEasyOpen
  , speedup, alterMinSkill, alterMinWalk, openTo, closeTo, embeddedItems
  , revealAs, obscureAs, hideAs, buildAs
  , isEasyOpenKind, isOpenable, isClosable, isChangeable
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , createTab, createTabWithKey, accessTab
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Content.TileKind (TileKind, TileSpeedup (..),
                                         isUknownSpace)
import qualified Game.LambdaHack.Content.TileKind as TK

createTab :: U.Unbox a => Kind.Ops TileKind -> (TileKind -> a) -> TK.Tab a
createTab Kind.Ops{ofoldrWithKey, olength} prop =
  let f _ t acc = prop t : acc
  in TK.Tab $ U.fromListN (fromEnum olength) $ ofoldrWithKey f []

createTabWithKey :: U.Unbox a
                 => Kind.Ops TileKind -> (Kind.Id TileKind -> TileKind -> a)
                 -> TK.Tab a
createTabWithKey Kind.Ops{ofoldrWithKey, olength} prop =
  let f k t acc = prop k t : acc
  in TK.Tab $ U.fromListN (fromEnum olength) $ ofoldrWithKey f []

-- Unsafe indexing is pretty safe here, because we guard the vector
-- with the newtype.
accessTab :: U.Unbox a => TK.Tab a -> Kind.Id TileKind -> a
{-# INLINE accessTab #-}
accessTab (TK.Tab tab) ki = tab `U.unsafeIndex` fromEnum ki

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

-- | Whether a tile has ambient light --- is lit on its own.
-- Essential for efficiency of "Perception", hence tabulated.
isLit :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isLit #-}
isLit TileSpeedup{isLitTab} = accessTab isLitTab

-- | Whether actors can walk into a tile.
-- Essential for efficiency of pathfinding, hence tabulated.
isWalkable :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isWalkable #-}
isWalkable TileSpeedup{isWalkableTab} = accessTab isWalkableTab

-- | Whether a tile is a door, open or closed.
-- Essential for efficiency of pathfinding, hence tabulated.
isDoor :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isDoor #-}
isDoor TileSpeedup{isDoorTab} = accessTab isDoorTab

-- | Whether a tile is suspect.
-- Essential for efficiency of pathfinding, hence tabulated.
isSuspect :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isSuspect #-}
isSuspect TileSpeedup{isSuspectTab} = accessTab isSuspectTab

-- | Whether a tile kind (specified by its id) has a ChangeTo feature.
-- Essential for efficiency of pathfinding, hence tabulated.
isChangeable :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isChangeable #-}
isChangeable TileSpeedup{isChangeableTab} = accessTab isChangeableTab

isOftenItem :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isOftenItem #-}
isOftenItem TileSpeedup{isOftenItemTab} = accessTab isOftenItemTab

isOftenActor:: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isOftenActor #-}
isOftenActor TileSpeedup{isOftenActorTab} = accessTab isOftenActorTab

isNoItem :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isNoItem #-}
isNoItem TileSpeedup{isNoItemTab} = accessTab isNoItemTab

isNoActor :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isNoActor #-}
isNoActor TileSpeedup{isNoActorTab} = accessTab isNoActorTab

-- | Whether a tile kind (specified by its id) has an OpenTo feature
-- and reasonable alter min skill.
isEasyOpen :: TileSpeedup -> Kind.Id TileKind -> Bool
{-# INLINE isEasyOpen #-}
isEasyOpen TileSpeedup{isEasyOpenTab} = accessTab isEasyOpenTab

alterMinSkill :: TileSpeedup -> Kind.Id TileKind -> Int
{-# INLINE alterMinSkill #-}
alterMinSkill TileSpeedup{alterMinSkillTab} =
  fromEnum . accessTab alterMinSkillTab

alterMinWalk :: TileSpeedup -> Kind.Id TileKind -> Int
{-# INLINE alterMinWalk #-}
alterMinWalk TileSpeedup{alterMinWalkTab} =
  fromEnum . accessTab alterMinWalkTab

-- | Whether one can easily explore a tile, possibly finding a treasure
-- or a clue. Doors can't be explorable since revealing a secret tile
-- should not change it's (walkable and) explorable status.
-- Door status should not depend on whether they are open or not
-- so that a foe opening a door doesn't force us to backtrack to explore it.
isExplorable :: TileSpeedup -> Kind.Id TileKind -> Bool
isExplorable coTileSpeedup t =
  (isWalkable coTileSpeedup t || isClear coTileSpeedup t) && not (isDoor coTileSpeedup t)

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
      isOftenItemTab = createTab cotile $ kindHasFeature TK.OftenItem
      isOftenActorTab = createTab cotile $ kindHasFeature TK.OftenActor
      isNoItemTab = createTab cotile $ kindHasFeature TK.NoItem
      isNoActorTab = createTab cotile $ kindHasFeature TK.NoActor
      isEasyOpenTab = createTab cotile isEasyOpenKind
      alterMinSkillTab = createTabWithKey cotile alterMinSkillKind
      alterMinWalkTab = createTabWithKey cotile alterMinWalkKind
  in TileSpeedup {..}

-- Check that alter can be used, if not, @maxBound@.
-- For now, we assume only items with @Embed@ may have embedded items,
-- whether inserted at dungeon creation or later on.
alterMinSkillKind :: Kind.Id TileKind -> TileKind -> Word8
alterMinSkillKind _k tk =
  let getTo TK.OpenTo{} = True
      getTo TK.CloseTo{} = True
      getTo TK.ChangeTo{} = True
      getTo TK.HideAs{} = True  -- in case tile swapped, but server sends hidden
      getTo TK.Embed{} = True
      getTo TK.Suspect = True
      getTo _ = False
  in if any getTo $ TK.tfeature tk then TK.talter tk else maxBound

-- How high alter skill is needed to make it walkable. If already
-- walkable, put @0@, if can't, put @maxBound@.
alterMinWalkKind :: Kind.Id TileKind -> TileKind -> Word8
alterMinWalkKind k tk =
  let getTo TK.OpenTo{} = True
      getTo TK.Suspect = True
      getTo TK.ChangeTo{} = True
      getTo _ = False
  in if | kindHasFeature TK.Walkable tk -> 0
        | isUknownSpace k -> TK.talter tk
        | any getTo $ TK.tfeature tk -> TK.talter tk
        | otherwise -> maxBound

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

embeddedItems :: Kind.Ops TileKind -> Kind.Id TileKind -> [GroupName ItemKind]
embeddedItems Kind.Ops{okind} t =
  let getTo (TK.Embed eff) acc = eff : acc
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

obscureAs :: Kind.Ops TileKind -> Kind.Id TileKind -> Rnd (Kind.Id TileKind)
obscureAs Kind.Ops{okind, opick} t = do
  let getTo (TK.ObscureAs grp) acc = grp : acc
      getTo _ acc = acc
  case foldr getTo [] $ TK.tfeature $ okind t of
    [] -> return t
    groups -> do
      grp <- oneOf groups
      fromMaybe (assert `failure` grp) <$> opick grp (const True)

hideAs :: Kind.Ops TileKind -> Kind.Id TileKind -> Kind.Id TileKind
hideAs Kind.Ops{okind, ouniqGroup} t =
  let getTo TK.HideAs{} = True
      getTo _ = False
  in case find getTo $ TK.tfeature $ okind t of
       Just (TK.HideAs grp) -> ouniqGroup grp
       _ -> t

buildAs :: Kind.Ops TileKind -> Kind.Id TileKind -> Kind.Id TileKind
buildAs Kind.Ops{okind, ouniqGroup} t =
  let getTo TK.BuildAs{} = True
      getTo _ = False
  in case find getTo $ TK.tfeature $ okind t of
       Just (TK.BuildAs grp) -> ouniqGroup grp
       _ -> t

isEasyOpenKind :: TileKind -> Bool
isEasyOpenKind tk =
  let getTo TK.OpenTo{} = True
      getTo _ = False
  in TK.talter tk < 10 && any getTo (TK.tfeature tk)

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
