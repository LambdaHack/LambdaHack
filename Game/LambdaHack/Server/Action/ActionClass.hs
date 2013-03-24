{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- | Basic type classes for server game actions.
-- This module should not be imported anywhere except in 'Action'
-- and 'TypeAction'.
module Game.LambdaHack.Server.Action.ActionClass where

import Game.LambdaHack.Action
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.ClientCmd
import Game.LambdaHack.Server.State

class MonadActionRO m => MonadServer m where
  getServer    :: m StateServer
  getsServer   :: (StateServer -> a) -> m a
  modifyServer :: (StateServer -> StateServer) -> m ()
  putServer    :: StateServer -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a

class MonadServer m => MonadServerConn m where
  getDict      :: m ConnDict
  getsDict     :: (ConnDict -> a) -> m a
  modifyDict   :: (ConnDict -> ConnDict) -> m ()
  putDict      :: ConnDict -> m ()

class MonadServer m => MonadServerAtomic m where
  execAtomic    :: Atomic -> m ()
  execCmdAtomic :: CmdAtomic -> m ()
  execCmdAtomic = execAtomic . CmdAtomic
  execSfxAtomic :: SfxAtomic -> m ()
  execSfxAtomic = execAtomic . SfxAtomic
