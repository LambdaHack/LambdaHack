-- | Factions taking part in the game: e.g., two human players controlling
-- the hero faction battling the monster and the animal factions.
module Game.LambdaHack.Common.Faction
  ( FactionId, FactionDict, Faction(..), Diplomacy(..), Outcome(..), Status(..)
  , Target(..)
  , isHorrorFact
  , canMoveFact, otherMoveFact, noRunWithMulti, isAtWar, isAllied
  , difficultyBound, difficultyDefault, difficultyCoeff
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
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind

-- | All factions in the game, indexed by faction identifier.
type FactionDict = EM.EnumMap FactionId Faction

data Faction = Faction
  { gname    :: !Text            -- ^ individual name
  , gcolor   :: !Color.Color     -- ^ color of actors or their frames
  , gplayer  :: !Player          -- ^ the player spec for this faction
  , gdipl    :: !Dipl            -- ^ diplomatic mode
  , gquit    :: !(Maybe Status)  -- ^ cause of game end/exit
  , gleader  :: !(Maybe (ActorId, Maybe Target))
                                 -- ^ the leader of the faction and his target
  , gsha     :: !ItemBag         -- ^ faction's shared inventory
  , gvictims :: !(EM.EnumMap (Kind.Id ItemKind) Int)  -- ^ members killed
  }
  deriving (Show, Eq)

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
  { stOutcome :: !Outcome            -- ^ current game outcome
  , stDepth   :: !Int                -- ^ depth of the final encounter
  , stNewGame :: !(Maybe GroupName)  -- ^ new game group to start, if any
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
  deriving (Show, Eq)

-- | Tell whether the faction consists of summoned horrors only.
isHorrorFact :: Faction -> Bool
isHorrorFact fact = fgroup (gplayer fact) == "horror"

canMoveFact :: Faction -> Bool -> Bool
canMoveFact fact isLeader =
  let skillsLeader = fskillsLeader $ gplayer fact
      skillsOther = fskillsOther $ gplayer fact
  in if isLeader
     then EM.findWithDefault 0 Ability.AbMove skillsLeader > 0
     else EM.findWithDefault 0 Ability.AbMove skillsOther > 0

otherMoveFact :: Faction -> Bool
otherMoveFact fact =
  let skillsOther = fskillsOther $ gplayer fact
  in EM.findWithDefault 0 Ability.AbMove skillsOther > 0

-- A faction where leader doesn't move (but, e.g., only fires)
-- or other actors move at once or where some of leader change
-- is automatic can't run with multiple actors at once. That would be
-- overpowered or too complex to keep correct.
--
-- Note that this doesn't take into account individual actor skills,
-- so this is overly restrictive and, OTOH, sometimes running will fail
-- or behave wierdly regardless. But it's simple and easy to understand
-- by the UI user.
noRunWithMulti :: Faction -> Bool
noRunWithMulti fact =
  let skillsLeader = fskillsLeader $ gplayer fact
      skillsOther = fskillsOther $ gplayer fact
  in EM.findWithDefault 0 Ability.AbMove skillsLeader <= 0
     || EM.findWithDefault 0 Ability.AbMove skillsOther > 0
     || case fhasLeader (gplayer fact) of
          LeaderMode{..} -> autoDungeon || autoLevel
          LeaderNull -> False

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
