-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
module Game.LambdaHack.Common.Action
  ( -- * Action monads
    MonadActionRO(..), MonadAction(..), MonadAtomic(..)
  , broadcastCmdAtomic,  broadcastSfxAtomic
    -- * Shorthands
  , getLevel, nUI
  ) where

import qualified Data.EnumMap.Strict as EM
import Data.Key (mapWithKeyM_)

import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

class (Monad m, Functor m) => MonadActionRO m where
  getState    :: m State
  getsState   :: (State -> a) -> m a

class MonadActionRO m => MonadAction m where
  modifyState :: (State -> State) -> m ()
  putState    :: State -> m ()

class MonadActionRO m => MonadAtomic m where
  execAtomic    :: Atomic -> m ()
  execCmdAtomic :: CmdAtomic -> m ()
  execCmdAtomic = execAtomic . CmdAtomic
  execSfxAtomic :: SfxAtomic -> m ()
  execSfxAtomic = execAtomic . SfxAtomic

broadcastCmdAtomic :: MonadAtomic m
                   => (FactionId -> CmdAtomic) -> m ()
broadcastCmdAtomic fcmd = do
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> execCmdAtomic $ fcmd fid) factionD

broadcastSfxAtomic :: MonadAtomic m
                   => (FactionId -> SfxAtomic) -> m ()
broadcastSfxAtomic fcmd = do
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> execSfxAtomic $ fcmd fid) factionD

getLevel :: MonadActionRO m => LevelId -> m Level
getLevel lid = getsState $ (EM.! lid) . sdungeon

nUI :: MonadActionRO m => m Int
nUI = do
  factionD <- getsState sfactionD
  return $! length $ filter (playerUI . gplayer) $ EM.elems factionD
