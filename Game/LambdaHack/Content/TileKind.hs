{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of terrain tiles.
module Game.LambdaHack.Content.TileKind
  ( TileKind(..), Feature(..)
  , validateSingleTileKind, validateAllTileKind, actionFeatures
  , TileSpeedup(..), Tab(..), isUknownSpace, unknownId
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.DeepSeq
import qualified Data.Array.Unboxed as A
import Data.Binary
import Data.Hashable
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
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
  | OftenActor           -- ^ initial actors and stairs often generated there
  | NoItem               -- ^ no items ever generated there
  | NoActor              -- ^ no actors nor stairs ever generated there
  | Trail                -- ^ used for visible trails throughout the level
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
  , alterMinSkillTab :: !(Tab Word8)
  , alterMinWalkTab  :: !(Tab Word8)
  }

newtype Tab a = Tab (A.UArray (KindOps.Id TileKind) a)

isUknownSpace :: KindOps.Id TileKind -> Bool
{-# INLINE isUknownSpace #-}
isUknownSpace tt = minBound == tt

unknownId :: KindOps.Id TileKind
{-# INLINE unknownId #-}
unknownId = minBound

-- TODO: (spans multiple contents) check that all posible solid place
-- fences have hidden counterparts.
-- | Validate a single tile kind.
validateSingleTileKind :: TileKind -> [Text]
validateSingleTileKind TileKind{..} =
  [ "suspect tile is walkable" | Walkable `elem` tfeature
                                 && Suspect `elem` tfeature ]

-- TODO: verify that OpenTo, CloseTo and ChangeTo are assigned as specified.
-- TODO: verify that tile kind for "unknown space" has index 0 and talter is 1
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
      namesUnequal [] = assert `failure` "no TileKind content" `twith` lt
      namesUnequal (hd : tl) =
        -- Catch if at least one is different.
        any (/= tname hd) (map tname tl)
        -- TODO: calculate actionFeatures only once for each tile kind
        || any (/= actionFeatures True hd) (map (actionFeatures True) tl)
      confusions f = filter namesUnequal $ M.elems $ mapVis f
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
  in IS.fromList $ map hash $ mapMaybe f $ tfeature t
