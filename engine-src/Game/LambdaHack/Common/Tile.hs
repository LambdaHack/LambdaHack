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
  ( -- * Tile property lookup speedup tables and their construction
    TileSpeedup(..), Tab(..), speedupTile
    -- * Speedup property lookups
  , isClear, isLit, isHideout, isWalkable, isDoor, isChangable
  , isSuspect, isHideAs, consideredByAI, isExplorable
  , isVeryOftenItem, isCommonItem, isOftenActor, isNoItem, isNoActor
  , isEasyOpen, isEmbed, isAquatic, alterMinSkill, alterMinWalk
    -- * Slow property lookups
  , kindHasFeature, openTo, closeTo, embeddedItems, revealAs
  , obscureAs, hideAs, buildAs
  , isEasyOpenKind, isOpenable, isClosable, isModifiable
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , createTab, createTabWithKey, accessTab, alterMinSkillKind, alterMinWalkKind
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Vector.Unboxed as U
import           Data.Word (Word8)

import           Game.LambdaHack.Content.ItemKind (ItemKind)
import           Game.LambdaHack.Content.TileKind (TileKind, isUknownSpace)
import qualified Game.LambdaHack.Content.TileKind as TK
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Definition.ContentData
import           Game.LambdaHack.Definition.Defs

-- | A lot of tabulated maps from tile kind identifier to a property
-- of the tile kind.
data TileSpeedup = TileSpeedup
  { isClearTab          :: Tab Bool
  , isLitTab            :: Tab Bool
  , isHideoutTab        :: Tab Bool
  , isWalkableTab       :: Tab Bool
  , isDoorTab           :: Tab Bool
  , isOpenableTab       :: Tab Bool
  , isClosableTab       :: Tab Bool
  , isChangableTab      :: Tab Bool
  , isModifiableWithTab :: Tab Bool
  , isSuspectTab        :: Tab Bool
  , isHideAsTab         :: Tab Bool
  , consideredByAITab   :: Tab Bool
  , isVeryOftenItemTab  :: Tab Bool
  , isCommonItemTab     :: Tab Bool
  , isOftenActorTab     :: Tab Bool
  , isNoItemTab         :: Tab Bool
  , isNoActorTab        :: Tab Bool
  , isEasyOpenTab       :: Tab Bool
  , isEmbedTab          :: Tab Bool
  , isAquaticTab        :: Tab Bool
  , alterMinSkillTab    :: Tab Word8
  , alterMinWalkTab     :: Tab Word8
  }

-- Vectors of booleans can be slower than arrays, because they are not packed,
-- but with growing cache sizes they may as well turn out faster at some point.
-- The advantage of vectors are exposed internals, in particular unsafe
-- indexing. Also, in JS, bool arrays are obviously not packed.
-- An option: https://github.com/Bodigrim/bitvec
-- | A map morally indexed by @ContentId TileKind@.
newtype Tab a = Tab (U.Vector a)

createTab :: U.Unbox a => ContentData TileKind -> (TileKind -> a) -> Tab a
createTab cotile prop = Tab $ U.convert $ omapVector cotile prop

createTabWithKey :: U.Unbox a
                 => ContentData TileKind
                 -> (ContentId TileKind -> TileKind -> a)
                 -> Tab a
createTabWithKey cotile prop = Tab $ U.convert $ oimapVector cotile prop

-- Unsafe indexing is pretty safe here, because we guard the vector
-- with the newtype.
accessTab :: U.Unbox a => Tab a -> ContentId TileKind -> a
{-# INLINE accessTab #-}
accessTab (Tab tab) ki = tab `vectorUnboxedUnsafeIndex` contentIdIndex ki

speedupTile :: Bool -> ContentData TileKind -> TileSpeedup
speedupTile allClear cotile =
  -- Vectors pack bools as Word8 by default. No idea if the extra memory
  -- taken makes random lookups more or less efficient, so not optimizing
  -- further, until I have benchmarks.
  let isClearTab | allClear = createTab cotile $ (/= maxBound) . TK.talter
                 | otherwise = createTab cotile $ kindHasFeature TK.Clear
      isLitTab = createTab cotile $ not . kindHasFeature TK.Dark
      isHideoutTab = createTab cotile $ \tk ->
        kindHasFeature TK.Walkable tk  -- implies not unknown
        && kindHasFeature TK.Dark tk
      isWalkableTab = createTab cotile $ kindHasFeature TK.Walkable
      isDoorTab = createTab cotile $ \tk ->
        let getTo (TK.OpenTo grp) acc = grp : acc
            getTo _ acc = acc
        in case foldr getTo [] $ TK.tfeature tk of
          [grp] | oisSingletonGroup cotile grp ->
            TK.isClosableKind $ okind cotile $ ouniqGroup cotile grp
          _ -> let getTo2 (TK.CloseTo grp) acc = grp : acc
                   getTo2 _ acc = acc
               in case foldr getTo2 [] $ TK.tfeature tk of
                 [grp] | oisSingletonGroup cotile grp ->
                   TK.isOpenableKind $ okind cotile $ ouniqGroup cotile grp
                 _ -> False
      isOpenableTab = createTab cotile TK.isOpenableKind
      isClosableTab = createTab cotile TK.isClosableKind
      isChangableTab = createTab cotile $ \tk ->
        let getTo TK.ChangeTo{} = True
            getTo _ = False
        in any getTo $ TK.tfeature tk
      isModifiableWithTab = createTab cotile $ \tk ->
        let getTo TK.OpenWith{} = True
            getTo TK.CloseWith{} = True
            getTo TK.ChangeWith{} = True
            getTo _ = False
        in any getTo $ TK.tfeature tk
      isSuspectTab = createTab cotile TK.isSuspectKind
      isHideAsTab = createTab cotile $ \tk ->
        let getTo TK.HideAs{} = True
            getTo _ = False
        in any getTo $ TK.tfeature tk
      consideredByAITab = createTab cotile $ kindHasFeature TK.ConsideredByAI
      isVeryOftenItemTab = createTab cotile $ kindHasFeature TK.VeryOftenItem
      isCommonItemTab = createTab cotile $ \tk ->
        kindHasFeature TK.OftenItem tk || kindHasFeature TK.VeryOftenItem tk
      isOftenActorTab = createTab cotile $ kindHasFeature TK.OftenActor
      isNoItemTab = createTab cotile $ kindHasFeature TK.NoItem
      isNoActorTab = createTab cotile $ kindHasFeature TK.NoActor
      isEasyOpenTab = createTab cotile isEasyOpenKind
      isEmbedTab = createTab cotile $ \tk ->
        let getTo TK.Embed{} = True
            getTo _ = False
        in any getTo $ TK.tfeature tk
      isAquaticTab = createTab cotile $ \tk ->
        maybe False (> 0) $ lookup TK.AQUATIC $ TK.tfreq tk
      alterMinSkillTab = createTabWithKey cotile alterMinSkillKind
      alterMinWalkTab = createTabWithKey cotile alterMinWalkKind
  in TileSpeedup {..}

-- Check that alter can be used, if not, @maxBound@.
-- For now, we assume only items with @Embed@ may have embedded items,
-- whether inserted at dungeon creation or later on.
-- This is used by UI and server to validate (sensibility of) altering.
-- See the comment for @alterMinWalkKind@ regarding @HideAs@.
alterMinSkillKind :: ContentId TileKind -> TileKind -> Word8
alterMinSkillKind _k tk =
  let getTo TK.OpenTo{} = True
      getTo TK.CloseTo{} = True
      getTo TK.ChangeTo{} = True
      getTo TK.OpenWith{} = True
      getTo TK.CloseWith{} = True
      getTo TK.ChangeWith{} = True
      getTo TK.HideAs{} = True  -- in case tile swapped, but server sends hidden
      getTo TK.RevealAs{} = True
      getTo TK.ObscureAs{} = True
      getTo TK.Embed{} = True
      getTo TK.ConsideredByAI = True
      getTo _ = False
  in if any getTo $ TK.tfeature tk then TK.talter tk else maxBound

-- How high alter skill is needed to make it walkable. If already
-- walkable, put @0@, if can't, put @maxBound@. Used only by AI and Bfs
-- We don't include @HideAs@, because it's very unlikely anybody swapped
-- the tile while AI was not looking so AI can assume it's still uninteresting.
-- Pathfinding in UI will also not show such tile as passable, which is OK.
-- If a human player has a suspicion the tile was swapped, he can check
-- it manually, disregarding the displayed path hints.
alterMinWalkKind :: ContentId TileKind -> TileKind -> Word8
alterMinWalkKind k tk =
  let getTo TK.OpenTo{} = True
-- enable when AI and humans can cope with unreachable areas
--      getTo TK.OpenWith{} = True
--        -- opening this may not be possible, but AI has to try, for there may
--        -- be no other path
      getTo TK.RevealAs{} = True
      getTo TK.ObscureAs{} = True
      getTo _ = False
  in if | kindHasFeature TK.Walkable tk -> 0
        | isUknownSpace k -> TK.talter tk
        | any getTo $ TK.tfeature tk -> TK.talter tk
        | otherwise -> maxBound

-- | Whether a tile does not block vision.
-- Essential for efficiency of "FOV", hence tabulated.
isClear :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isClear #-}
isClear TileSpeedup{isClearTab} = accessTab isClearTab

-- | Whether a tile has ambient light --- is lit on its own.
-- Essential for efficiency of "Perception", hence tabulated.
isLit :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isLit #-}
isLit TileSpeedup{isLitTab} = accessTab isLitTab

-- | Whether a tile is a good hideout: walkable and dark.
isHideout :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isHideout #-}
isHideout TileSpeedup{isHideoutTab} = accessTab isHideoutTab

-- | Whether actors can walk into a tile.
-- Essential for efficiency of pathfinding, hence tabulated.
isWalkable :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isWalkable #-}
isWalkable TileSpeedup{isWalkableTab} = accessTab isWalkableTab

-- | Whether a tile is a door, open or closed.
-- Essential for efficiency of pathfinding, hence tabulated.
isDoor :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isDoor #-}
isDoor TileSpeedup{isDoorTab} = accessTab isDoorTab

-- | Whether a tile kind (specified by its id) has an @OpenTo@ feature.
isOpenable :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isOpenable #-}
isOpenable TileSpeedup{isOpenableTab} = accessTab isOpenableTab

-- | Whether a tile kind (specified by its id) has a @CloseTo@ feature.
isClosable :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isClosable #-}
isClosable TileSpeedup{isClosableTab} = accessTab isClosableTab

-- | Whether a tile is changable.
isChangable :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isChangable #-}
isChangable TileSpeedup{isChangableTab} = accessTab isChangableTab

-- | Whether a tile is modifiable with some items.
isModifiableWith :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isModifiableWith #-}
isModifiableWith TileSpeedup{isModifiableWithTab} =
  accessTab isModifiableWithTab

-- | Whether a tile is suspect.
-- Essential for efficiency of pathfinding, hence tabulated.
isSuspect :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isSuspect #-}
isSuspect TileSpeedup{isSuspectTab} = accessTab isSuspectTab

isHideAs :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isHideAs #-}
isHideAs TileSpeedup{isHideAsTab} = accessTab isHideAsTab

consideredByAI :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE consideredByAI #-}
consideredByAI TileSpeedup{consideredByAITab} = accessTab consideredByAITab

-- | Whether one can easily explore a tile, possibly finding a treasure,
-- either spawned there or dropped there by a (dying from poison) foe.
-- Doors can't be explorable since revealing a secret tile
-- should not change it's explorable status. Also, door explorable status
-- should not depend on whether they are open or not, so that
-- a foe opening a door doesn't force us to backtrack to explore it.
-- Still, a foe that digs through a wall will affect our exploration counter
-- and if content lets walls contain threasure, such backtraking makes sense.
isExplorable :: TileSpeedup -> ContentId TileKind -> Bool
isExplorable coTileSpeedup t =
  isWalkable coTileSpeedup t && not (isDoor coTileSpeedup t)

isVeryOftenItem :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isVeryOftenItem #-}
isVeryOftenItem TileSpeedup{isVeryOftenItemTab} = accessTab isVeryOftenItemTab

isCommonItem :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isCommonItem #-}
isCommonItem TileSpeedup{isCommonItemTab} = accessTab isCommonItemTab

isOftenActor :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isOftenActor #-}
isOftenActor TileSpeedup{isOftenActorTab} = accessTab isOftenActorTab

isNoItem :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isNoItem #-}
isNoItem TileSpeedup{isNoItemTab} = accessTab isNoItemTab

isNoActor :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isNoActor #-}
isNoActor TileSpeedup{isNoActorTab} = accessTab isNoActorTab

-- | Whether a tile kind (specified by its id) has an @OpenTo@ feature
-- or is walkable even without opening.
isEasyOpen :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isEasyOpen #-}
isEasyOpen TileSpeedup{isEasyOpenTab} = accessTab isEasyOpenTab

isEmbed :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isEmbed #-}
isEmbed TileSpeedup{isEmbedTab} = accessTab isEmbedTab

isAquatic :: TileSpeedup -> ContentId TileKind -> Bool
{-# INLINE isAquatic #-}
isAquatic TileSpeedup{isAquaticTab} = accessTab isAquaticTab

alterMinSkill :: TileSpeedup -> ContentId TileKind -> Int
{-# INLINE alterMinSkill #-}
alterMinSkill TileSpeedup{alterMinSkillTab} =
  fromEnum . accessTab alterMinSkillTab

alterMinWalk :: TileSpeedup -> ContentId TileKind -> Int
{-# INLINE alterMinWalk #-}
alterMinWalk TileSpeedup{alterMinWalkTab} =
  fromEnum . accessTab alterMinWalkTab

-- | Whether a tile kind has the given feature.
kindHasFeature :: TK.Feature -> TileKind -> Bool
{-# INLINE kindHasFeature #-}
kindHasFeature f t = f `elem` TK.tfeature t

openTo :: ContentData TileKind -> ContentId TileKind -> Rnd (ContentId TileKind)
openTo cotile t = do
  let getTo (TK.OpenTo grp) acc = grp : acc
      getTo _ acc = acc
  case foldr getTo [] $ TK.tfeature $ okind cotile t of
    [grp] -> fromMaybe (error $ "" `showFailure` grp)
             <$> opick cotile grp (const True)
    _ -> return t

closeTo :: ContentData TileKind -> ContentId TileKind
        -> Rnd (ContentId TileKind)
closeTo cotile t = do
  let getTo (TK.CloseTo grp) acc = grp : acc
      getTo _ acc = acc
  case foldr getTo [] $ TK.tfeature $ okind cotile t of
    [grp] -> fromMaybe (error $ "" `showFailure` grp)
             <$> opick cotile grp (const True)
    _ -> return t

embeddedItems :: ContentData TileKind -> ContentId TileKind
              -> [GroupName ItemKind]
embeddedItems cotile t =
  let getTo (TK.Embed igrp) acc = igrp : acc
      getTo _ acc = acc
  in foldr getTo [] $ TK.tfeature $ okind cotile t

revealAs :: ContentData TileKind -> ContentId TileKind
         -> Rnd (ContentId TileKind)
revealAs cotile t = do
  let getTo (TK.RevealAs grp) acc = grp : acc
      getTo _ acc = acc
  case foldr getTo [] $ TK.tfeature $ okind cotile t of
    [] -> return t
    groups -> do
      grp <- oneOf groups
      fromMaybe (error $ "" `showFailure` grp) <$> opick cotile grp (const True)

obscureAs :: ContentData TileKind -> ContentId TileKind
          -> Rnd (ContentId TileKind)
obscureAs cotile t = do
  let getTo (TK.ObscureAs grp) acc = grp : acc
      getTo _ acc = acc
  case foldr getTo [] $ TK.tfeature $ okind cotile $ buildAs cotile t of
    [] -> return t
    groups -> do
      grp <- oneOf groups
      fromMaybe (error $ "" `showFailure` grp) <$> opick cotile grp (const True)

hideAs :: ContentData TileKind -> ContentId TileKind
       -> Maybe (ContentId TileKind)
hideAs cotile t =
  let getTo TK.HideAs{} = True
      getTo _ = False
  in case find getTo $ TK.tfeature $ okind cotile t of
       Just (TK.HideAs grp) -> Just $ ouniqGroup cotile grp
       _ -> Nothing

buildAs :: ContentData TileKind -> ContentId TileKind -> ContentId TileKind
buildAs cotile t =
  let getTo TK.BuildAs{} = True
      getTo _ = False
  in case find getTo $ TK.tfeature $ okind cotile t of
       Just (TK.BuildAs grp) -> ouniqGroup cotile grp
       _ -> t

isEasyOpenKind :: TileKind -> Bool
isEasyOpenKind tk =
  let getTo TK.OpenTo{} = True
      getTo TK.Walkable = True  -- very easy open
      getTo _ = False
  in TK.talter tk < 10 && any getTo (TK.tfeature tk)

isModifiable :: TileSpeedup -> ContentId TileKind -> Bool
isModifiable coTileSpeedup t = isOpenable coTileSpeedup t
                               || isClosable coTileSpeedup t
                               || isChangable coTileSpeedup t
                               || isModifiableWith coTileSpeedup t
                               || isSuspect coTileSpeedup t
