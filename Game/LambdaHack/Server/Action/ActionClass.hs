-- | Basic type classes for server game actions.
-- This module should not be imported anywhere except in 'Action'
-- and 'TypeAction'.
module Game.LambdaHack.Server.Action.ActionClass where

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Server.State

class MonadActionRO m => MonadServer m where
  getServer    :: m StateServer
  getsServer   :: (StateServer -> a) -> m a
  modifyServer :: (StateServer -> StateServer) -> m ()
  putServer    :: StateServer -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a
  saveServer   :: m ()

class MonadServer m => MonadConnServer m where
  getDict      :: m ConnServerDict
  getsDict     :: (ConnServerDict -> a) -> m a
  modifyDict   :: (ConnServerDict -> ConnServerDict) -> m ()
  putDict      :: ConnServerDict -> m ()

saveName :: String
saveName = serverSaveName
