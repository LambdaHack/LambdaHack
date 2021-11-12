{-# LANGUAGE DeriveGeneric #-}
-- | Factions taking part in the game, e.g., a hero faction, a monster faction
-- and an animal faction.
module Game.LambdaHack.Common.Faction
  ( FactionDict, Faction(..), Diplomacy(..)
  , Status(..), Challenge(..)
  , tshowDiplomacy, tshowChallenge, gleader, isHorrorFact, noRunWithMulti
  , bannedPointmanSwitchBetweenLevels, isFoe, isFriend
  , difficultyBound, difficultyDefault, difficultyCoeff
  , defaultChallenge, possibleActorFactions, ppContainer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , Dipl
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.FactionKind
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind (ModeKind)
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

-- | All factions in the game, indexed by faction identifier.
type FactionDict = EM.EnumMap FactionId Faction

-- | The faction datatype.
data Faction = Faction
  { gkind     :: FactionKind
      -- ^ the player spec for this faction, do not update!
      -- it is morally read-only, but not represented
      -- as @ContentId FactionKind@, because it's very small
      -- and it's looked up often enough in the code and during runtime;
      -- a side-effect is that if content changes mid-game, this stays;
      -- if we ever have thousands of factions in a single game,
      -- e.g., one for each separately spawned herd of animals, change this
  , gname     :: Text            -- ^ individual name
  , gcolor    :: Color.Color     -- ^ color of numbered actors
  , gdoctrine :: Ability.Doctrine
                                 -- ^ non-pointmen behave according to this
  , gunderAI  :: Bool            -- ^ whether the faction is under AI control
  , ginitial  :: [(Int, Int, GroupName ItemKind)]  -- ^ initial actors
  , gdipl     :: Dipl            -- ^ diplomatic standing
  , gquit     :: Maybe Status    -- ^ cause of game end/exit
  , _gleader  :: Maybe ActorId   -- ^ the leader of the faction; don't use
                                 --   in place of sleader on clients
  , gstash    :: Maybe (LevelId, Point)
                                 -- ^ level and position of faction's
                                 --   shared inventory stash
  , gvictims  :: EM.EnumMap (ContentId ItemKind) Int  -- ^ members killed
  }
  deriving (Show, Eq, Generic)

instance Binary Faction

-- | Diplomacy states. Higher overwrite lower in case of asymmetric content.
data Diplomacy =
    Unknown
  | Neutral
  | Alliance
  | War
  deriving (Show, Eq, Ord, Enum, Generic)

instance Binary Diplomacy

type Dipl = EM.EnumMap FactionId Diplomacy

-- | Current game status.
data Status = Status
  { stOutcome :: Outcome  -- ^ current game outcome
  , stDepth   :: Int      -- ^ depth of the final encounter
  , stNewGame :: Maybe (GroupName ModeKind)
                          -- ^ new game group to start, if any
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary Status

-- | The difficulty level influencess HP of either the human player or the AI.
-- The challenges restrict some abilities of the human player only.
data Challenge = Challenge
  { cdiff   :: Int   -- ^ game difficulty level (HP bonus or malus)
  , cfish   :: Bool  -- ^ cold fish challenge (no healing from enemies)
  , cgoods  :: Bool  -- ^ ready goods challenge (crafting disabled)
  , cwolf   :: Bool  -- ^ lone wolf challenge (only one starting character)
  , ckeeper :: Bool  -- ^ finder keeper challenge (ranged attacks disabled)
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary Challenge

tshowDiplomacy :: Diplomacy -> Text
tshowDiplomacy Unknown = "unknown to each other"
tshowDiplomacy Neutral = "in neutral diplomatic relations"
tshowDiplomacy Alliance = "allied"
tshowDiplomacy War = "at war"

tshowChallenge :: Challenge -> Text
tshowChallenge Challenge{..} =
  "("
  <> T.intercalate ", "
    (["difficulty" <+> tshow cdiff | cdiff /= difficultyDefault]
     ++ ["cold fish" | cfish]
     ++ ["ready goods" | cgoods]
     ++ ["lone wolf" | cwolf]
     ++ ["finder keeper" | ckeeper])
  <> ")"

gleader :: Faction -> Maybe ActorId
gleader = _gleader

-- | Tell whether the faction consists of summoned horrors only.
--
-- Horror player is special, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a skirmish game between two hero factions land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
isHorrorFact :: Faction -> Bool
isHorrorFact fact = IK.HORROR `elem` fgroups (gkind fact)

-- A faction where other actors move at once or where some of leader change
-- is automatic can't run with multiple actors at once. That would be
-- overpowered or too complex to keep correct.
--
-- Note that this doesn't take into account individual actor skills,
-- so this is overly restrictive and, OTOH, sometimes running will fail
-- or behave wierdly regardless. But it's simple and easy to understand
-- by the UI user.
noRunWithMulti :: Faction -> Bool
noRunWithMulti fact =
  let skillsOther = fskillsOther $ gkind fact
  in Ability.getSk Ability.SkMove skillsOther >= 0
     || bannedPointmanSwitchBetweenLevels fact
     || not (fhasPointman (gkind fact))

bannedPointmanSwitchBetweenLevels :: Faction -> Bool
bannedPointmanSwitchBetweenLevels = fspawnsFast . gkind

-- | Check if factions are at war. Assumes symmetry.
isFoe :: FactionId -> Faction -> FactionId -> Bool
isFoe fid1 fact1 fid2 =
  fid1 /= fid2  -- shortcut
  && War == EM.findWithDefault Unknown fid2 (gdipl fact1)

-- | Check if factions are allied. Assumes symmetry.
isAlly :: Faction -> FactionId -> Bool
{-# INLINE isAlly #-}
isAlly fact1 fid2 = Alliance == EM.findWithDefault Unknown fid2 (gdipl fact1)

-- | Check if factions are allied or are the same faction. Assumes symmetry.
isFriend :: FactionId -> Faction -> FactionId -> Bool
isFriend fid1 fact1 fid2 = fid1 == fid2 || isAlly fact1 fid2

difficultyBound :: Int
difficultyBound = 9

difficultyDefault :: Int
difficultyDefault = (1 + difficultyBound) `div` 2

-- The function is its own inverse.
difficultyCoeff :: Int -> Int
difficultyCoeff n = difficultyDefault - n

defaultChallenge :: Challenge
defaultChallenge = Challenge { cdiff = difficultyDefault
                             , cfish = False
                             , cgoods = False
                             , cwolf = False
                             , ckeeper = False }

possibleActorFactions :: [GroupName ItemKind] -> ItemKind -> FactionDict
                      -> [(FactionId, Faction)]
possibleActorFactions itemGroups itemKind factionD =
  let candidatesFromGroups grps =
        let f (_, fact) = any (`elem` fgroups (gkind fact)) grps
        in filter f $ EM.assocs factionD
      allCandidates =
        [ candidatesFromGroups itemGroups  -- when origin known/matters
        , candidatesFromGroups $ map fst $ IK.ifreq itemKind  -- otherwise
        , filter (isHorrorFact . snd) $ EM.assocs factionD  -- fall back
        , EM.assocs factionD  -- desperate fall back
        ]
  in case filter (not . null) allCandidates of
    [] -> []
    candidates : _ -> candidates

ppContainer :: FactionDict -> Container -> Text
ppContainer factionD (CFloor lid p) =
  let f fact = case gstash fact of
        Just (slid, sp) | slid == lid && sp == p -> Just $ gname fact
        _ -> Nothing
  in case mapMaybe f $ EM.elems factionD of
    [] -> "nearby"
    [t] -> "in the shared inventory stash of" <+> t
    _ -> "in a shared zone of interests"
ppContainer _ CEmbed{} = "embedded nearby"
ppContainer _ (CActor _ cstore) = ppCStoreIn cstore
ppContainer _ c@CTrunk{} = error $ "" `showFailure` c
