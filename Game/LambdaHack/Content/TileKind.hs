{-# LANGUAGE DeriveGeneric #-}
-- | The type of kinds of terrain tiles.
module Game.LambdaHack.Content.TileKind
  ( TileKind(..), Feature(..)
  , validateSingleTileKind, validateAllTileKind, actionFeatures
  , TileSpeedup(..), Tab(..), isUknownSpace, unknownId
  , isSuspectKind, isOpenableKind, isClosableKind, talterForStairs, floorSymbol
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.DeepSeq
import Data.Binary
import qualified Data.Char as Char
import Data.Hashable
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Color
import qualified Game.LambdaHack.Common.KindOps as KindOps
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind (ItemKind)

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
    Embed !(GroupName ItemKind)
      -- ^ initially an item of this group is embedded;
      --   we assume the item has effects and is supposed to be triggered
  | OpenTo !(GroupName TileKind)
      -- ^ goes from a closed to (randomly closed or) open tile when altered
  | CloseTo !(GroupName TileKind)
      -- ^ goes from an open to (randomly opened or) closed tile when altered
  | ChangeTo !(GroupName TileKind)
      -- ^ alters tile, but does not change walkability
  | HideAs !(GroupName TileKind)
      -- ^ when hidden, looks as the unique tile of the group

  -- The following three are only used in dungeon generation.
  | BuildAs !(GroupName TileKind)
      -- ^ when generating, may be transformed to the unique tile of the group
  | RevealAs !(GroupName TileKind)
      -- ^ when generating in opening, can be revealed to belong to the group
  | ObscureAs !(GroupName TileKind)
      -- ^ when generating in solid wall, can be revealed to belong to the group

  | Walkable             -- ^ actors can walk through
  | Clear                -- ^ actors can see through
  | Dark                 -- ^ is not lit with an ambient light

  | OftenItem            -- ^ initial items often generated there
  | OftenActor           -- ^ initial actors often generated there
  | NoItem               -- ^ no items ever generated there
  | NoActor              -- ^ no actors ever generated there
  | Indistinct           -- ^ is allowed to have the same look as another tile
  | ConsideredByAI       -- ^ even if otherwise uninteresting, taken into
                         --   account for triggering by AI
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
  { isClearTab        :: !(Tab Bool)
  , isLitTab          :: !(Tab Bool)
  , isWalkableTab     :: !(Tab Bool)
  , isDoorTab         :: !(Tab Bool)
  , isChangableTab    :: !(Tab Bool)
  , isSuspectTab      :: !(Tab Bool)
  , isHideAsTab       :: !(Tab Bool)
  , consideredByAITab :: !(Tab Bool)
  , isOftenItemTab    :: !(Tab Bool)
  , isOftenActorTab   :: !(Tab Bool)
  , isNoItemTab       :: !(Tab Bool)
  , isNoActorTab      :: !(Tab Bool)
  , isEasyOpenTab     :: !(Tab Bool)
  , alterMinSkillTab  :: !(Tab Word8)
  , alterMinWalkTab   :: !(Tab Word8)
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
validateSingleTileKind t@TileKind{..} =
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
       , Indistinct, ConsideredByAI, Trail, Spice ]

validateDups :: TileKind -> Feature -> [Text]
validateDups TileKind{..} feat =
  let ts = filter (== feat) tfeature
  in ["more than one" <+> tshow feat <+> "specification" | length ts > 1]

isSuspectKind :: TileKind -> Bool
isSuspectKind t =
  let getTo RevealAs{} = True
      getTo ObscureAs{} = True
      getTo _ = False
  in any getTo $ tfeature t

isOpenableKind ::TileKind -> Bool
isOpenableKind t =
  let getTo OpenTo{} = True
      getTo _ = False
  in any getTo $ tfeature t

isClosableKind :: TileKind -> Bool
isClosableKind t =
  let getTo CloseTo{} = True
      getTo _ = False
  in any getTo $ tfeature t

-- | Validate all tile kinds.
--
-- If tiles look the same on the map (symbol and color), their substantial
-- features should be the same, too. Otherwise, the player has to inspect
-- manually all the tiles of that kind, or even experiment with them,
-- to see if any is special. This would be tedious. Note that tiles may freely
-- differ wrt text blurb, dungeon generation, AI preferences, etc.
validateAllTileKind :: [TileKind] -> [Text]
validateAllTileKind lt =
  let listVis f = map (\kt -> ( (tsymbol kt, f kt)
                              , [(kt, actionFeatures True kt)] )) lt
      mapVis :: (TileKind -> Color)
             -> M.Map (Char, Color) [(TileKind, IS.IntSet)]
      mapVis f = M.fromListWith (++) $ listVis f
      isConfused [] =  assert `failure` lt
      isConfused [_] = False
      isConfused (hd : tl) =
        any ((Indistinct `notElem`) . tfeature . fst) (hd : tl)
        && any ((/= snd hd) . snd) tl
      confusions f = filter isConfused $ M.elems $ mapVis f
  in [ "first tile should be the unknown one"
     | talter (head lt) /= 1 || tname (head lt) /= "unknown space" ]
     ++ [ "only unknown tile may have talter 1"
        | any ((== 1) . talter) $ tail lt ]
     ++ case confusions tcolor ++ confusions tcolor2 of
       [] -> []
       cfs -> ["tile confusions detected:" <+> tshow cfs]

-- | Features of tiles that differentiate them substantially from one another.
-- The intention is the player can easily tell such tiles apart by their
-- behaviour and only looking at the map, not tile name nor description.
-- So if running uses this function, it won't stop at places that the player
-- can't himself tell from other places, and so running does not confer
-- any advantages, except UI convenience. Hashes are accurate enough
-- for our purpose, given that we use arbitrary heuristics anyway.
actionFeatures :: Bool -> TileKind -> IS.IntSet
actionFeatures markSuspect t =
  let stripLight grp = maybe grp toGroupName
                       $ maybe (T.stripSuffix "Dark" $ tshow grp) Just
                       $ T.stripSuffix "Lit" $ tshow grp
      f feat = case feat of
        Embed{} -> Just feat
        OpenTo grp -> Just $ OpenTo $ stripLight grp
        CloseTo grp -> Just $ CloseTo $ stripLight grp
        ChangeTo grp -> Just $ ChangeTo $ stripLight grp
        Walkable -> Just feat
        Clear -> Just feat
        HideAs{} -> Nothing
        BuildAs{} -> Nothing
        RevealAs{} -> if markSuspect then Just feat else Nothing
        ObscureAs{} -> if markSuspect then Just feat else Nothing
        Dark -> Nothing  -- not important any longer, after FOV computed
        OftenItem -> Nothing
        OftenActor -> Nothing
        NoItem -> Nothing
        NoActor -> Nothing
        Indistinct -> Nothing
        ConsideredByAI -> Nothing
        Trail -> Just feat  -- doesn't affect tile behaviour, but important
        Spice -> Nothing
  in IS.fromList $ map hash $ mapMaybe f $ tfeature t

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
