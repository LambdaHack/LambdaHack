{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of terrain tiles.
module Game.LambdaHack.Content.TileKind
  ( pattern S_UNKNOWN_SPACE, pattern LEGEND_LIT, pattern LEGEND_DARK, pattern S_UNKNOWN_OUTER_FENCE, pattern S_BASIC_OUTER_FENCE, pattern AQUATIC
  , TileKind(..), Feature(..)
  , makeData
  , isUknownSpace, unknownId
  , isSuspectKind, isOpenableKind, isClosableKind
  , talterForStairs, floorSymbol
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle, validateAll
  , validateDups, mandatoryGroups, mandatoryGroupsSingleton
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.DeepSeq
import           Data.Binary
import qualified Data.Char as Char
import           Data.Hashable
import           GHC.Generics (Generic)

import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.ContentData
import Game.LambdaHack.Definition.Defs

-- | The type of kinds of terrain tiles. See @Tile.hs@ for explanation
-- of the absence of a corresponding type @Tile@ that would hold
-- particular concrete tiles in the dungeon.
-- Note that tile names (and any other content names) should not be plural
-- (that would lead to "a stairs"), so "road with cobblestones" is fine,
-- but "granite cobblestones" is wrong.
--
-- Tile kind for unknown space has the minimal @ContentId@ index.
-- The @talter@ for unknown space is @1@ and no other tile kind has that value.
data TileKind = TileKind
  { tsymbol  :: Char         -- ^ map symbol
  , tname    :: Text         -- ^ short description
  , tfreq    :: Freqs TileKind  -- ^ frequency within groups
  , tcolor   :: Color        -- ^ map color
  , tcolor2  :: Color        -- ^ map color when not in FOV
  , talter   :: Word8        -- ^ minimal skill needed to alter the tile
  , tfeature :: [Feature]    -- ^ properties; order matters
  }
  deriving Show  -- No Eq and Ord to make extending logically sound

-- | All possible terrain tile features.
data Feature =
    Embed (GroupName ItemKind)
      -- ^ initially an item of this group is embedded;
      --   we assume the item has effects and is supposed to be triggered
  | OpenTo (GroupName TileKind)
      -- ^ goes from a closed to closed or open tile when altered
  | CloseTo (GroupName TileKind)
      -- ^ goes from an open to open or closed tile when altered
  | ChangeTo (GroupName TileKind)
      -- ^ alters tile, but does not change walkability
  | OpenWith Bool [(Int, GroupName ItemKind)] (GroupName TileKind)
      -- ^ alters tile, as before, using up all listed items from the ground
      --   and equipment; the list never empty; for simplicity, such tiles
      --   are never taken into account when pathfinding; the boolean
      --   determines if projectiles may trigger the action
  | CloseWith Bool [(Int, GroupName ItemKind)] (GroupName TileKind)
  | ChangeWith Bool [(Int, GroupName ItemKind)] (GroupName TileKind)
  | HideAs (GroupName TileKind)
      -- ^ when hidden, looks as the unique tile of the group
  | BuildAs (GroupName TileKind)
      -- ^ when generating, may be transformed to the unique tile of the group
  | RevealAs (GroupName TileKind)
      -- ^ when generating in opening, can be revealed to belong to the group
  | ObscureAs (GroupName TileKind)
      -- ^ when generating in solid wall, can be revealed to belong to the group
  | Walkable             -- ^ actors can walk through
  | Clear                -- ^ actors can see through
  | Dark                 -- ^ is not lit with an ambient light
  | OftenItem            -- ^ initial items often generated there
  | VeryOftenItem        -- ^ initial items very often generated there
  | OftenActor           -- ^ initial actors often generated there
  | NoItem               -- ^ no items ever generated there
  | NoActor              -- ^ no actors ever generated there
  | ConsideredByAI       -- ^ even if otherwise uninteresting, taken into
                         --   account for triggering by AI
  | Trail                -- ^ used for visible trails throughout the level
  | Spice                -- ^ in place normal legend and in override,
                         --   don't roll a tile kind only once per place,
                         --   but roll for each position; one non-spicy
                         --   (according to frequencies of non-spicy) and
                         --   at most one spicy (according to their frequencies)
                         --   is rolled per place and then, once for each
                         --   position, one of the two is semi-randomly chosen
                         --   (according to their individual frequencies only)
  deriving (Show, Eq, Ord, Generic)

instance Binary Feature

instance Hashable Feature

instance NFData Feature

-- | Validate a single tile kind.
validateSingle :: TileKind -> [Text]
validateSingle t@TileKind{..} =
  [ "suspect tile is walkable" | Walkable `elem` tfeature
                                 && isSuspectKind t ]
  ++ [ "openable tile is open" | Walkable `elem` tfeature
                                 && isOpenableKind t ]
  ++ [ "closable tile is closed" | Walkable `notElem` tfeature
                                   && isClosableKind t ]
  ++ [ "walkable tile is considered for triggering by AI"
     | Walkable `elem` tfeature
       && ConsideredByAI `elem` tfeature ]
  ++ [ "trail tile not walkable" | Walkable `notElem` tfeature
                                   && Trail `elem` tfeature ]
  ++ [ "OftenItem and NoItem on a tile" | OftenItem `elem` tfeature
                                          && NoItem `elem` tfeature ]
  ++ [ "OftenActor and NoActor on a tile" | OftenItem `elem` tfeature
                                            && NoItem `elem` tfeature ]
  ++ (let f :: Feature -> Bool
          f OpenTo{} = True
          f CloseTo{} = True
          f ChangeTo{} = True
          f _ = False
          ts = filter f tfeature
      in [ "more than one OpenTo, CloseTo and ChangeTo specification"
         | length ts > 1 ])
  ++ (let f :: Feature -> Bool
          f HideAs{} = True
          f _ = False
          ts = filter f tfeature
      in ["more than one HideAs specification" | length ts > 1])
  ++ (let f :: Feature -> Bool
          f BuildAs{} = True
          f _ = False
          ts = filter f tfeature
      in ["more than one BuildAs specification" | length ts > 1])
  ++ concatMap (validateDups t)
       [ Walkable, Clear, Dark, OftenItem, OftenActor, NoItem, NoActor
       , ConsideredByAI, Trail, Spice ]

validateDups :: TileKind -> Feature -> [Text]
validateDups TileKind{..} feat =
  let ts = filter (== feat) tfeature
  in ["more than one" <+> tshow feat <+> "specification" | length ts > 1]

-- | Validate all tile kinds.
--
-- We don't check it any more, but if tiles look the same on the map
-- (symbol and color), their substantial features should be the same, too,
-- unless there is a good reason they shouldn't. Otherwise the player has
-- to inspect manually all the tiles with this look to see if any is special.
-- This tends to be tedious. Note that tiles may freely differ wrt text blurb,
-- dungeon generation rules, AI preferences, etc., whithout causing the tedium.
validateAll :: [TileKind] -> ContentData TileKind -> [Text]
validateAll content cotile =
  let f :: Feature -> Bool
      f HideAs{} = True
      f BuildAs{} = True
      f _ = False
      wrongFooAsGroups =
        [ cgroup
        | k <- content
        , let (cgroup, notSingleton) = case find f (tfeature k) of
                Just (HideAs grp) | not $ oisSingletonGroup cotile grp ->
                  (grp, True)
                Just (BuildAs grp) | not $ oisSingletonGroup cotile grp ->
                  (grp, True)
                _ -> (undefined, False)
        , notSingleton
        ]
  in [ "HideAs or BuildAs groups not singletons:" <+> tshow wrongFooAsGroups
     | not $ null wrongFooAsGroups ]
     ++ [ "unknown tile (the first) should be the unknown one"
        | talter (head content) /= 1
          || tname (head content) /= "unknown space" ]
     ++ [ "no tile other than the unknown (the first) should require skill 1"
        | all (\tk -> talter tk == 1) (tail content) ]
     ++ [ "only unknown tile may have talter 1"
        | any ((== 1) . talter) $ tail content ]

-- * Mandatory item groups

mandatoryGroupsSingleton :: [GroupName TileKind]
mandatoryGroupsSingleton =
       [S_UNKNOWN_SPACE, S_UNKNOWN_OUTER_FENCE, S_BASIC_OUTER_FENCE]

pattern S_UNKNOWN_SPACE, S_UNKNOWN_OUTER_FENCE, S_BASIC_OUTER_FENCE :: GroupName TileKind

mandatoryGroups :: [GroupName TileKind]
mandatoryGroups =
       [LEGEND_LIT, LEGEND_DARK]

pattern LEGEND_LIT, LEGEND_DARK :: GroupName TileKind

pattern S_UNKNOWN_SPACE = GroupName "unknown space"
pattern S_UNKNOWN_OUTER_FENCE = GroupName "unknown outer fence"
pattern S_BASIC_OUTER_FENCE = GroupName "basic outer fence"
pattern LEGEND_LIT = GroupName "legendLit"
pattern LEGEND_DARK = GroupName "legendDark"

-- * Optional item groups

pattern AQUATIC :: GroupName TileKind

pattern AQUATIC = GroupName "aquatic"

isUknownSpace :: ContentId TileKind -> Bool
{-# INLINE isUknownSpace #-}
isUknownSpace tt = toContentId 0 == tt

unknownId :: ContentId TileKind
{-# INLINE unknownId #-}
unknownId = toContentId 0

isSuspectKind :: TileKind -> Bool
isSuspectKind t =
  let getTo RevealAs{} = True
      getTo ObscureAs{} = True
      getTo _ = False
  in any getTo $ tfeature t

isOpenableKind :: TileKind -> Bool
isOpenableKind t =
  let getTo OpenTo{} = True
      getTo _ = False
  in any getTo $ tfeature t

isClosableKind :: TileKind -> Bool
isClosableKind t =
  let getTo CloseTo{} = True
      getTo _ = False
  in any getTo $ tfeature t

talterForStairs :: Word8
talterForStairs = 3

floorSymbol :: Char.Char
floorSymbol = Char.chr 183

-- Alter skill schema:
-- 0  can be altered by everybody (escape)
-- 1  unknown only
-- 2  openable and suspect
-- 3  stairs
-- 4  closable
-- 5  changeable (e.g., caches)
-- 10  weak obstructions
-- 50  considerable obstructions
-- 100  walls
-- maxBound  impenetrable walls, etc., can never be altered

makeData :: [TileKind] -> [GroupName TileKind] -> [GroupName TileKind]
         -> ContentData TileKind
makeData content groupNamesAtMostOne groupNames =
  makeContentData "TileKind" tname tfreq validateSingle validateAll content
                  []
                  (mandatoryGroupsSingleton ++ groupNamesAtMostOne)
                  (mandatoryGroups ++ groupNames)
