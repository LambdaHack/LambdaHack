-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
module Game.LambdaHack.Common.Action
  ( MonadReadState(..)
  , getLevel, nUI
  ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

class (Monad m, Functor m) => MonadReadState m where
  getState    :: m State
  getsState   :: (State -> a) -> m a

getLevel :: MonadReadState m => LevelId -> m Level
getLevel lid = getsState $ (EM.! lid) . sdungeon

nUI :: MonadReadState m => m Int
nUI = do
  factionD <- getsState sfactionD
  return $! length $ filter (playerUI . gplayer) $ EM.elems factionD
