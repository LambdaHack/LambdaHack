{-# LANGUAGE DeriveGeneric #-}
-- | Factions taking part in the game: e.g., two human players controlling
-- the hero faction battling the monster and the animal factions.
module Game.LambdaHack.Common.Faction
  ( FactionId, FactionDict, Faction(..), Diplomacy(..), Status(..)
  , Target(..), TGoal(..), tgtKindDescription
  , isHorrorFact, nameOfHorrorFact
  , noRunWithMulti, isAIFact, autoDungeonLevel, automatePlayer
  , isAtWar, isAllied
  , difficultyBound, difficultyDefault, difficultyCoeff, difficultyInverse
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , Dipl
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Content.ModeKind

-- | All factions in the game, indexed by faction identifier.
type FactionDict = EM.EnumMap FactionId Faction

data Faction = Faction
  { gname     :: !Text            -- ^ individual name
  , gcolor    :: !Color.Color     -- ^ color of actors or their frames
  , gplayer   :: !Player          -- ^ the player spec for this faction
  , ginitial  :: ![(Int, Int, GroupName ItemKind)]  -- ^ initial actors
  , gdipl     :: !Dipl            -- ^ diplomatic mode
  , gquit     :: !(Maybe Status)  -- ^ cause of game end/exit
  , gleader   :: !(Maybe ActorId) -- ^ the leader of the faction
  , gsha      :: !ItemBag         -- ^ faction's shared inventory
  , gvictims  :: !(EM.EnumMap (Kind.Id ItemKind) Int)  -- ^ members killed
  , gvictimsD :: !(EM.EnumMap (Kind.Id ModeKind)
                              (IM.IntMap (EM.EnumMap (Kind.Id ItemKind) Int)))
      -- ^ members killed in the past, by game mode and difficulty level
  }
  deriving (Show, Eq, Ord, Generic)

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
  { stOutcome :: !Outcome  -- ^ current game outcome
  , stDepth   :: !Int      -- ^ depth of the final encounter
  , stNewGame :: !(Maybe (GroupName ModeKind))
                           -- ^ new game group to start, if any
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary Status

-- | The type of na actor target.
data Target =
    TEnemy !ActorId !Bool
    -- ^ target an actor; cycle only trough seen foes, unless the flag is set
  | TPoint !TGoal !LevelId !Point  -- ^ target a concrete spot
  | TVector !Vector         -- ^ target position relative to actor
  deriving (Show, Eq, Ord, Generic)

instance Binary Target

data TGoal =
    TEnemyPos !ActorId !Bool
    -- ^ last seen position of the targeted actor
  | TEmbed !ItemBag !Point
    -- ^ in @TPoint (TEmbed bag p) _ q@ usually @bag@ is embbedded in @p@
    --   and @q@ is an adjacent open tile
  | TItem !ItemBag
  | TSmell
  | TUnknown
  | TKnown
  | TAny
  deriving (Show, Eq, Ord, Generic)

instance Binary TGoal

tgtKindDescription :: Target -> Text
tgtKindDescription tgt = case tgt of
  TEnemy _ True -> "at actor"
  TEnemy _ False -> "at enemy"
  TPoint{} -> "at position"
  TVector{} -> "with a vector"

-- | Tell whether the faction consists of summoned horrors only.
--
-- Horror player is special, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a skirmish game between two hero factions land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
isHorrorFact :: Faction -> Bool
isHorrorFact fact = fgroup (gplayer fact) == nameOfHorrorFact

nameOfHorrorFact :: GroupName ItemKind
nameOfHorrorFact = toGroupName "horror"

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
  let skillsOther = fskillsOther $ gplayer fact
  in EM.findWithDefault 0 Ability.AbMove skillsOther >= 0
     || case fleaderMode (gplayer fact) of
          LeaderNull -> True
          LeaderAI AutoLeader{} -> True
          LeaderUI AutoLeader{..} -> autoDungeon || autoLevel

isAIFact :: Faction -> Bool
isAIFact fact =
  case fleaderMode (gplayer fact) of
    LeaderNull -> True
    LeaderAI _ -> True
    LeaderUI _ -> False

autoDungeonLevel :: Faction -> (Bool, Bool)
autoDungeonLevel fact = case fleaderMode (gplayer fact) of
                          LeaderNull -> (False, False)
                          LeaderAI AutoLeader{..} -> (autoDungeon, autoLevel)
                          LeaderUI AutoLeader{..} -> (autoDungeon, autoLevel)

automatePlayer :: Bool -> Player -> Player
automatePlayer st pl =
  let autoLeader False Player{fleaderMode=LeaderAI auto} = LeaderUI auto
      autoLeader True Player{fleaderMode=LeaderUI auto} = LeaderAI auto
      autoLeader _ Player{fleaderMode} = fleaderMode
  in pl {fleaderMode = autoLeader st pl}

-- | Check if factions are at war. Assumes symmetry.
isAtWar :: Faction -> FactionId -> Bool
isAtWar fact fid = War == EM.findWithDefault Unknown fid (gdipl fact)

-- | Check if factions are allied. Assumes symmetry.
isAllied :: Faction -> FactionId -> Bool
isAllied fact fid = Alliance == EM.findWithDefault Unknown fid (gdipl fact)

difficultyBound :: Int
difficultyBound = 9

difficultyDefault :: Int
difficultyDefault = (1 + difficultyBound) `div` 2

-- The function is its own inverse.
difficultyCoeff :: Int -> Int
difficultyCoeff n = difficultyDefault - n

-- The function is its own inverse.
difficultyInverse :: Int -> Int
difficultyInverse n = difficultyBound + 1 - n
