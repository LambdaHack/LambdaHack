{-# LANGUAGE CPP #-}
-- | Factions taking part in the game: e.g., two human players controlling
-- the hero faction battling the monster and the animal factions.
module Game.LambdaHack.Common.Faction
  ( FactionId, FactionDict, Faction(..), Diplomacy(..), Outcome(..), Status(..)
  , Target(..)
  , isHorrorFact
  , canMoveFact, noRunWithMulti, isAIFact, autoDungeonLevel, automatePlayer
  , isAtWar, isAllied
  , difficultyBound, difficultyDefault, difficultyCoeff
#ifdef EXPOSE_INTERNAL
  , Dipl
#endif
  ) where

import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)

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
  { gname    :: !Text            -- ^ individual name
  , gcolor   :: !Color.Color     -- ^ color of actors or their frames
  , gplayer  :: !(Player Int)    -- ^ the player spec for this faction
  , gdipl    :: !Dipl            -- ^ diplomatic mode
  , gquit    :: !(Maybe Status)  -- ^ cause of game end/exit
  , gleader  :: !(Maybe (ActorId, Maybe Target))
                                 -- ^ the leader of the faction and his target
  , gsha     :: !ItemBag         -- ^ faction's shared inventory
  , gvictims :: !(EM.EnumMap (Kind.Id ItemKind) Int)  -- ^ members killed
  }
  deriving (Show, Eq, Ord)

-- | Diplomacy states. Higher overwrite lower in case of assymetric content.
data Diplomacy =
    Unknown
  | Neutral
  | Alliance
  | War
  deriving (Show, Eq, Ord, Enum)

type Dipl = EM.EnumMap FactionId Diplomacy

-- | Outcome of a game.
data Outcome =
    Killed    -- ^ the faction was eliminated
  | Defeated  -- ^ the faction lost the game in another way
  | Camping   -- ^ game is supended
  | Conquer   -- ^ the player won by eliminating all rivals
  | Escape    -- ^ the player escaped the dungeon alive
  | Restart   -- ^ game is restarted
  deriving (Show, Eq, Ord, Enum)

-- | Current game status.
data Status = Status
  { stOutcome :: !Outcome  -- ^ current game outcome
  , stDepth   :: !Int      -- ^ depth of the final encounter
  , stNewGame :: !(Maybe (GroupName ModeKind))
                           -- ^ new game group to start, if any
  }
  deriving (Show, Eq, Ord)

-- | The type of na actor target.
data Target =
    TEnemy !ActorId !Bool
    -- ^ target an actor; cycle only trough seen foes, unless the flag is set
  | TEnemyPos !ActorId !LevelId !Point !Bool
    -- ^ last seen position of the targeted actor
  | TPoint !LevelId !Point  -- ^ target a concrete spot
  | TVector !Vector         -- ^ target position relative to actor
  deriving (Show, Eq, Ord)

-- | Tell whether the faction consists of summoned horrors only.
--
-- Horror player is special, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a duel game between two hero players land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
-- Actors that can be summoned should have "horror" in their @ifreq@ set.
isHorrorFact :: Faction -> Bool
isHorrorFact fact = fgroup (gplayer fact) == "horror"

canMoveFact :: Faction -> Bool -> Bool
canMoveFact fact isLeader =
  let skillsOther = fskillsOther $ gplayer fact
  in isLeader || EM.findWithDefault 0 Ability.AbMove skillsOther > 0

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
  in EM.findWithDefault 0 Ability.AbMove skillsOther > 0
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

automatePlayer :: Bool -> Player a -> Player a
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

instance Binary Faction where
  put Faction{..} = do
    put gname
    put gcolor
    put gplayer
    put gdipl
    put gquit
    put gleader
    put gsha
    put gvictims
  get = do
    gname <- get
    gcolor <- get
    gplayer <- get
    gdipl <- get
    gquit <- get
    gleader <- get
    gsha <- get
    gvictims <- get
    return $! Faction{..}

instance Binary Diplomacy where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Binary Outcome where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Binary Status where
  put Status{..} = do
    put stOutcome
    put stDepth
    put stNewGame
  get = do
    stOutcome <- get
    stDepth <- get
    stNewGame <- get
    return $! Status{..}

instance Binary Target where
  put (TEnemy a permit) = putWord8 0 >> put a >> put permit
  put (TEnemyPos a lid p permit) =
    putWord8 1 >> put a >> put lid >> put p >> put permit
  put (TPoint lid p) = putWord8 2 >> put lid >> put p
  put (TVector v) = putWord8 3 >> put v
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM2 TEnemy get get
      1 -> liftM4 TEnemyPos get get get get
      2 -> liftM2 TPoint get get
      3 -> liftM TVector get
      _ -> fail "no parse (Target)"
