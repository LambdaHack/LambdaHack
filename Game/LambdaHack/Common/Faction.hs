-- | Factions taking part in the game: e.g., two human players controlling
-- the hero faction battling the monster and the animal factions.
module Game.LambdaHack.Common.Faction
  ( FactionId, FactionDict, Faction(..), Diplomacy(..), Status(..)
  , isHumanFact, usesAIFact, isSpawningFact, isAtWar, isAllied
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.StrategyKind

-- | All factions in the game, indexed by faction identifier.
type FactionDict = EM.EnumMap FactionId Faction

data Faction = Faction
  { gkind     :: !(Kind.Id FactionKind)   -- ^ the kind of the faction
  , gname     :: !Text                    -- ^ individual name
  , gAiLeader :: !(Maybe (Kind.Id StrategyKind))
                                          -- ^ AI for the leaders;
                                          -- Nothing means human-controlled
  , gAiMember :: !(Maybe (Kind.Id StrategyKind))
                                          -- ^ AI to use for other actors;
                                          -- Nothing means human-controlled
  , gdipl     :: !Dipl                    -- ^ diplomatic mode
  , gquit     :: !(Maybe Status)          -- ^ cause of game end/exit
  , gleader   :: !(Maybe ActorId)
  , gcolor    :: !Color.Color             -- ^ color of actors or their frames
  , ginitial  :: !Int                     -- ^ number of initial actors
  }
  deriving (Show, Eq)

-- | Diplomacy states. Higher overwrite lower in case of assymetric content.
data Diplomacy =
    Unknown
  | Neutral
  | Alliance
  | War
  deriving (Show, Eq, Ord)

type Dipl = EM.EnumMap FactionId Diplomacy

-- | Current result of the game.
data Status =
    Killed !Actor  -- ^ the faction was eliminated; this is the last actor
  | Defeated       -- ^ the faction otherwise lost the game
  | Camping        -- ^ game is supended
  | Conquer        -- ^ the player won by eliminating all rivals
  | Escape         -- ^ the player escaped the dungeon alive
  | Restart !Text  -- ^ game is restarted
  deriving (Show, Eq, Ord)

-- | Tell whether the faction is controlled (at least partially) by a human.
isHumanFact :: Faction -> Bool
isHumanFact fact = isNothing (gAiLeader fact) || isNothing (gAiMember fact)

-- | Tell whether the faction uses AI to control any of its actors.
usesAIFact :: Faction -> Bool
usesAIFact fact = isJust (gAiLeader fact) || isJust (gAiMember fact)

-- | Tell whether the faction can spawn actors.
isSpawningFact :: Kind.COps -> Faction -> Bool
isSpawningFact Kind.COps{cofact=Kind.Ops{okind}} fact =
  fspawn (okind $ gkind fact) > 0

-- | Check if factions are at war. Assumes symmetry.
isAtWar :: Faction -> FactionId -> Bool
isAtWar fact fid = War == EM.findWithDefault Unknown fid (gdipl fact)

-- | Check if factions are allied. Assumes symmetry.
isAllied :: Faction -> FactionId -> Bool
isAllied fact fid = Alliance == EM.findWithDefault Unknown fid (gdipl fact)

instance Binary Diplomacy where
  put Unknown  = putWord8 0
  put Neutral  = putWord8 1
  put Alliance = putWord8 2
  put War      = putWord8 3
  get = do
    tag <- getWord8
    case tag of
      0 -> return Unknown
      1 -> return Neutral
      2 -> return Alliance
      3 -> return War
      _ -> fail "no parse (Diplomacy)"

instance Binary Status where
  put (Killed body) = putWord8 0 >> put body
  put Defeated = putWord8 1
  put Camping = putWord8 2
  put Conquer = putWord8 3
  put Escape = putWord8 4
  put (Restart t) = putWord8 5 >> put t
  get = do
    tag <- getWord8
    case tag of
      0 -> fmap Killed get
      1 -> return Defeated
      2 -> return Camping
      3 -> return Conquer
      4 -> return Escape
      5 -> fmap Restart get
      _ -> fail "no parse (Status)"

instance Binary Faction where
  put Faction{..} = do
    put gkind
    put gname
    put gAiLeader
    put gAiMember
    put gdipl
    put gquit
    put gleader
    put gcolor
    put ginitial
  get = do
    gkind <- get
    gname <- get
    gAiLeader <- get
    gAiMember <- get
    gdipl <- get
    gquit <- get
    gleader <- get
    gcolor <- get
    ginitial <- get
    return Faction{..}
