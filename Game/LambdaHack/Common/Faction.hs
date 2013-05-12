{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
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
import GHC.Generics (Generic)

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
                                          -- ^ AI to use for other actors
                                          -- Nothing means human-controlled
  , gdipl     :: !Dipl                    -- ^ diplomatic state
  , gquit     :: !(Maybe (Bool, Status))  -- ^ cause of game end/exit
  , gleader   :: !(Maybe ActorId)
  , gcolor    :: !Color.Color             -- ^ color of actors or their frames
  }
  deriving (Show, Eq)

-- | Diplomacy states. Higher overwrite lower in case of assymetric content.
data Diplomacy =
    Unknown
  | Neutral
  | Alliance
  | War
  deriving (Show, Eq, Ord, Generic)

type Dipl = EM.EnumMap FactionId Diplomacy

-- | Current result of the game.
data Status =
    Killed !LevelId  -- ^ the player lost the game on the given level
  | Camping          -- ^ game is supended
  | Victor           -- ^ the player won
  | Restart          -- ^ the player quits and starts a new game
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

instance Binary Diplomacy

instance Binary Status where
  put (Killed ln) = putWord8 0 >> put ln
  put Camping     = putWord8 1
  put Victor      = putWord8 2
  put Restart     = putWord8 3
  get = do
    tag <- getWord8
    case tag of
      0 -> fmap Killed get
      1 -> return Camping
      2 -> return Victor
      3 -> return Restart
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
  get = do
    gkind <- get
    gname <- get
    gAiLeader <- get
    gAiMember <- get
    gdipl <- get
    gquit <- get
    gleader <- get
    gcolor <- get
    return Faction{..}
