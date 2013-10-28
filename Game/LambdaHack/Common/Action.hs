{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
module Game.LambdaHack.Common.Action
  ( -- * Action monads
    MonadActionRO(..), MonadAction(..), MonadAtomic(..)
    -- * Shorthands
  , getLevel, nUI
    -- * Assorted
  , serverSaveName
  ) where

import Control.Monad.Writer.Strict (WriterT, lift)
import qualified Data.EnumMap.Strict as EM
import Data.Monoid

import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

class (Monad m, Functor m) => MonadActionRO m where
  getState    :: m State
  getsState   :: (State -> a) -> m a

instance (Monoid a, MonadActionRO m) => MonadActionRO (WriterT a m) where
  getState    = lift getState
  getsState   = lift . getsState

instance MonadActionRO m => Show (WriterT a m b) where
  show _ = "an action"

class MonadActionRO m => MonadAction m where
  modifyState :: (State -> State) -> m ()
  putState    :: State -> m ()

class MonadActionRO m => MonadAtomic m where
  execAtomic    :: Atomic -> m ()
  execCmdAtomic :: CmdAtomic -> m ()
  execCmdAtomic = execAtomic . CmdAtomic
  execSfxAtomic :: SfxAtomic -> m ()
  execSfxAtomic = execAtomic . SfxAtomic

getLevel :: MonadActionRO m => LevelId -> m Level
getLevel lid = getsState $ (EM.! lid) . sdungeon

nUI :: MonadActionRO m => m Int
nUI = do
  factionD <- getsState sfactionD
  return $ length $ filter (playerUI . gplayer) $ EM.elems factionD

serverSaveName :: String
serverSaveName = "server.sav"
