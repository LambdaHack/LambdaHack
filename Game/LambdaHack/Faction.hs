-- | Factions taking part in the game: e.g., two players controlling
-- the hero faction battling the monster and the animal factions.
module Game.LambdaHack.Faction
  ( FactionId, Faction(..), Status(..), FactionDict
  ) where

import Data.Binary
import Data.Text (Text)
import qualified Data.IntMap as IM

import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.StrategyKind
import qualified Game.LambdaHack.Kind as Kind

data Faction = Faction
  { gkind       :: !(Kind.Id FactionKind)   -- ^ the kind of the faction
  , gname       :: !Text                    -- ^ individual name
  , gAiSelected :: !(Maybe (Kind.Id StrategyKind))
                                            -- ^ AI for the selected actor;
                                            -- human-controlled, if Nothing
  , gAiIdle     :: !(Kind.Id StrategyKind)  -- ^ AI to use for idle actors
  , genemy      :: ![Text]  -- ^ currently in war with such factions
  , gally       :: ![Text]  -- ^ currently allied with such factions
  , gquit       :: !(Maybe (Bool, Status))  -- ^ cause of game end/exit
  }
  deriving Show

-- | Current result of the game.
data Status =
    Killed !LevelId  -- ^ the player lost the game on the given level
  | Camping          -- ^ game is supended
  | Victor           -- ^ the player won
  | Restart          -- ^ the player quits and starts a new game
  deriving (Show, Eq, Ord)

-- | All factions in the game, indexed by faction identifier.
type FactionDict = IM.IntMap Faction

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
    put gAiSelected
    put gAiIdle
    put genemy
    put gally
    put gquit
  get = do
    gkind <- get
    gname <- get
    gAiSelected <- get
    gAiIdle <- get
    genemy <- get
    gally <- get
    gquit <- get
    return Faction{..}

-- | A unique identifier of a faction in a game.
type FactionId = Int
