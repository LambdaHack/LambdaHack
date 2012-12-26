-- | Factions taking part in the game: e.g., two players controlling
-- the hero faction battling the monster and the animal factions.
module Game.LambdaHack.Faction
  ( FactionId, Faction(..)
  ) where

import Data.Binary
import Data.Text (Text)

import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.StrategyKind
import qualified Game.LambdaHack.Kind as Kind

data Faction = Faction
  { gkind       :: !(Kind.Id FactionKind)   -- ^ the kind of the faction
  , gname       :: !(Maybe Text)            -- ^ individual name
  , gAiSelected :: !(Maybe (Kind.Id StrategyKind))
                                            -- ^ AI for the selected actor;
                                            -- human-controlled, if Nothing
  , gAiIdle     :: !(Kind.Id StrategyKind)  -- ^ AI to use for idle actors
  , genemy      :: ![Text]  -- ^ currently in war with such factions
  , gally       :: ![Text]  -- ^ currently allied with such factions
  }
  deriving Show

instance Binary Faction where
  put Faction{..} = do
    put gkind
    put gname
    put gAiSelected
    put gAiIdle
    put genemy
    put gally
  get = do
    gkind <- get
    gname <- get
    gAiSelected <- get
    gAiIdle <- get
    genemy <- get
    gally <- get
    return Faction{..}

-- | A unique identifier of a faction in a game.
type FactionId = Int
