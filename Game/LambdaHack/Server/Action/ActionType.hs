{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.Server.Action.ActionType
  ( ActionSer, executorSer
  ) where

import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Action
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Server.Action.ActionClass
import Game.LambdaHack.Server.State
import Game.LambdaHack.State

data SerState = SerState
  { serState  :: !State        -- ^ current global state
  , serServer :: !StateServer  -- ^ current server state
  , serDict   :: !ConnDict     -- ^ client-server connection information
  }

-- | Server state transformation monad.
newtype ActionSer a = ActionSer {runActionSer :: StateT SerState IO a}
  deriving (Monad, Functor)

instance MonadActionRO ActionSer where
  getState    = ActionSer $ gets serState
  getsState f = ActionSer $ gets $ f . serState

instance MonadAction ActionSer where
  modifyState f =
    ActionSer $ modify $ \serS -> serS {serState = f $ serState serS}
  putState    s =
    ActionSer $ modify $ \serS -> serS {serState = s}

instance MonadServer ActionSer where
  getServer      = ActionSer $ gets $ serServer
  getsServer   f = ActionSer $ gets $ f . serServer
  modifyServer f =
    ActionSer $ modify $ \serS -> serS {serServer = f $ serServer serS}
  putServer    s =
    ActionSer $ modify $ \serS -> serS {serServer = s}
  liftIO         = ActionSer . IO.liftIO

instance MonadServerConn ActionSer where
  getDict      = ActionSer $ gets $ serDict
  getsDict   f = ActionSer $ gets $ f . serDict
  modifyDict f =
    ActionSer $ modify $ \serS -> serS {serDict = f $ serDict serS}
  putDict    s =
    ActionSer $ modify $ \serS -> serS {serDict = s}

-- | Run an action in the @IO@ monad, with undefined state.
executorSer :: ActionSer () -> IO ()
executorSer m = evalStateT (runActionSer m)
                  SerState { serState = emptyState
                           , serServer = emptyStateServer
                           , serDict = EM.empty
                           }
