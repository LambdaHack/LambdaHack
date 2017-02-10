{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of terrain tiles.
module Game.LambdaHack.Content.TileKind
  ( TileKind(..), Feature(..)
  , validateSingleTileKind, validateAllTileKind, actionFeatures
  , TileSpeedup(..), Tab(..), isUknownSpace, unknownId, talterForStairs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.DeepSeq
import Data.Binary
import Data.Hashable
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Color
import qualified Game.LambdaHack.Common.KindOps as KindOps
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK

-- | The type of kinds of terrain tiles. See @Tile.hs@ for explanation
-- of the absence of a corresponding type @Tile@ that would hold
-- particular concrete tiles in the dungeon.
-- Note that tile names (and any other content names) should not be plural
-- (that would lead to "a stairs"), so "road with cobblestones" is fine,
-- but "granite cobblestones" is wrong.
--
-- Tile kind for unknown space has the minimal @KindOps.Id@ index.
-- The @talter@ for unknown space is @1@ and no other tile kind has that value.
data TileKind = TileKind
  { tsymbol  :: !Char         -- ^ map symbol
  , tname    :: !Text         -- ^ short description
  , tfreq    :: !(Freqs TileKind)  -- ^ frequency within groups
  , tcolor   :: !Color        -- ^ map color
  , tcolor2  :: !Color        -- ^ map color when not in FOV
  , talter   :: !Word8        -- ^ minimal skill needed to alter the tile
  , tfeature :: ![Feature]    -- ^ properties
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound

-- | All possible terrain tile features.
data Feature =
    Embed !(GroupName ItemKind)  -- ^ embed an item of this group, to cause effects (WIP)
  | Cause !IK.Effect             -- ^ causes the effect when triggered;
                                 --   more succint than @Embed@, but will
                                 --   probably get supplanted by @Embed@
  | OpenTo !(GroupName TileKind)    -- ^ goes from a closed to an open tile when altered
  | CloseTo !(GroupName TileKind)   -- ^ goes from an open to a closed tile when altered
  | ChangeTo !(GroupName TileKind)  -- ^ alters tile, but does not change walkability
  | HideAs !(GroupName TileKind)    -- ^ when hidden, looks as a tile of the group
  | RevealAs !(GroupName TileKind)  -- ^ if secret, can be revealed to belong to the group

  | Walkable             -- ^ actors can walk through
  | Clear                -- ^ actors can see through
  | Dark                 -- ^ is not lit with an ambient light
  | Suspect              -- ^ may not be what it seems (clients only)
  | Impenetrable         -- ^ can never be excavated nor seen through

  | OftenItem            -- ^ initial items often generated there
  | OftenActor           -- ^ initial actors often generated there
  | NoItem               -- ^ no items ever generated there
  | NoActor              -- ^ no actors ever generated there
  | Indistinct           -- ^ is allowed to have the same look as another tile
  | Trail                -- ^ used for visible trails throughout the level
  | Spice                -- ^ in place normal legend and in override,
                         --   don't roll a tile kind only once per place,
                         --   but roll for each position; one non-spicy and
                         --   at most one spicy is rolled per place and then
                         --   one of the two is rolled for each position
  deriving (Show, Eq, Ord, Generic)

instance Binary Feature

instance Hashable Feature

instance NFData Feature

data TileSpeedup = TileSpeedup
  { isClearTab       :: !(Tab Bool)
  , isLitTab         :: !(Tab Bool)
  , isWalkableTab    :: !(Tab Bool)
  , isDoorTab        :: !(Tab Bool)
  , isSuspectTab     :: !(Tab Bool)
  , isChangeableTab  :: !(Tab Bool)
  , isOftenItemTab   :: !(Tab Bool)
  , isOftenActorTab  :: !(Tab Bool)
  , isNoItemTab      :: !(Tab Bool)
  , isNoActorTab     :: !(Tab Bool)
  , hasCausesTab     :: !(Tab Bool)
  , alterMinSkillTab :: !(Tab Word8)
  , alterMinWalkTab  :: !(Tab Word8)
  }

-- Vectors of booleans can be slower than arrays, because they are not packed,
-- but with growing cache sizes they may as well turn out faster at some point.
-- The advantage of vectors are exposed internals, in particular unsafe
-- indexing. Also, in JS bool arrays are obviously not packed.
newtype Tab a = Tab (U.Vector a)  -- morally indexed by @Id a@

isUknownSpace :: KindOps.Id TileKind -> Bool
{-# INLINE isUknownSpace #-}
isUknownSpace tt = KindOps.Id 0 == tt

unknownId :: KindOps.Id TileKind
{-# INLINE unknownId #-}
unknownId = KindOps.Id 0

-- | Validate a single tile kind.
validateSingleTileKind :: TileKind -> [Text]
validateSingleTileKind TileKind{..} =
  [ "suspect tile is walkable" | Walkable `elem` tfeature
                                 && Suspect `elem` tfeature ]

-- | Validate all tile kinds.
--
-- If tiles look the same on the map, the description and the substantial
-- features should be the same, too. Otherwise, the player has to inspect
-- manually all the tiles of that kind, or even experiment with them,
-- to see if any is special. This would be tedious. Note that iiles may freely
-- differ wrt dungeon generation, AI preferences, etc.
validateAllTileKind :: [TileKind] -> [Text]
validateAllTileKind lt =
  let listVis f = map (\kt -> ( ( tsymbol kt
                                  , Suspect `elem` tfeature kt
                                  , f kt
                                  )
                                , [kt] ) ) lt
      mapVis :: (TileKind -> Color) -> M.Map (Char, Bool, Color) [TileKind]
      mapVis f = M.fromListWith (++) $ listVis f
      minorVariant [] = assert `failure` "no TileKind content" `twith` lt
      minorVariant (hd : tl) =  -- probably just a dark variant of the tile
        all (== actionFeatures True hd) (map (actionFeatures True) tl)
      confusions f = filter (any ((Indistinct `notElem`) . tfeature))
                     $ filter (not . minorVariant) $ M.elems $ mapVis f
  in case confusions tcolor ++ confusions tcolor2 of
    [] -> []
    cfs -> ["tile confusions detected:" <+> tshow cfs]

-- | Features of tiles that differentiate them substantially from one another.
-- By tile content validation condition, this means the player
-- can tell such tile apart, and only looking at the map, not tile name.
-- So if running uses this function, it won't stop at places that the player
-- can't himself tell from other places, and so running does not confer
-- any advantages, except UI convenience. Hashes are accurate enough
-- for our purpose, given that we use arbitrary heuristics anyway.
actionFeatures :: Bool -> TileKind -> IS.IntSet
actionFeatures markSuspect t =
  let f feat = case feat of
        Embed{} -> Just feat
        Cause{} -> Just feat
        OpenTo{} -> Just $ OpenTo ""  -- if needed, remove prefix/suffix
        CloseTo{} -> Just $ CloseTo ""
        ChangeTo{} -> Just $ ChangeTo ""
        Walkable -> Just feat
        Clear -> Just feat
        Suspect -> if markSuspect then Just feat else Nothing
        Impenetrable -> Just feat
        Trail -> Just feat  -- doesn't affect tile behaviour, but important
        HideAs{} -> Nothing
        RevealAs{} -> Nothing
        Dark -> Nothing  -- not important any longer, after FOV computed
        OftenItem -> Nothing
        OftenActor -> Nothing
        NoItem -> Nothing
        NoActor -> Nothing
        Indistinct -> Nothing
        Spice -> Nothing
  in IS.fromList $ map hash $ mapMaybe f $ tfeature t

talterForStairs :: Word8
talterForStairs = 3

-- Alter skill schema:
-- 0  can be altered by everybody (currently no such thing)
-- 1  unknown only
-- 2  openable and suspect
-- 3  stairs
-- 4  closable
-- 5  changeable (e.g., caches)
-- 10  weak obstructions
-- 50  considerable obstructions
-- 100  walls
-- maxBound  impenetrable walls, etc., can never be altered
