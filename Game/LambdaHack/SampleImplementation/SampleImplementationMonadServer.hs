{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.SampleImplementation.SampleImplementationMonadServer
  ( executorSer
  ) where

import Control.Applicative
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import System.FilePath

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Response
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import Game.LambdaHack.Server.AtomicSemSer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

data SerState = SerState
  { serState  :: !State           -- ^ current global state
  , serServer :: !StateServer     -- ^ current server state
  , serDict   :: !ConnServerDict  -- ^ client-server connection information
  , serToSave :: !(Save.ChanSave (State, StateServer))
                                  -- ^ connection to the save thread
  }

-- | Server state transformation monad.
newtype SerImplementation a =
    SerImplementation {runSerImplementation :: StateT SerState IO a}
  deriving (Monad, Functor, Applicative)

instance MonadActionRO SerImplementation where
  getState    = SerImplementation $ gets serState
  getsState f = SerImplementation $ gets $ f . serState

instance MonadAction SerImplementation where
  modifyState f = SerImplementation $ state $ \serS ->
    let newSerS = serS {serState = f $ serState serS}
    in newSerS `seq` ((), newSerS)
  putState    s = SerImplementation $ state $ \serS ->
    let newSerS = serS {serState = s}
    in newSerS `seq` ((), newSerS)

instance MonadServer SerImplementation where
  getServer      = SerImplementation $ gets serServer
  getsServer   f = SerImplementation $ gets $ f . serServer
  modifyServer f = SerImplementation $ state $ \serS ->
    let newSerS = serS {serServer = f $ serServer serS}
    in newSerS `seq` ((), newSerS)
  putServer    s = SerImplementation $ state $ \serS ->
    let newSerS = serS {serServer = s}
    in newSerS `seq` ((), newSerS)
  liftIO         = SerImplementation . IO.liftIO
  saveServer     = SerImplementation $ do
    s <- gets serState
    ser <- gets serServer
    toSave <- gets serToSave
    IO.liftIO $ Save.saveToChan toSave (s, ser)

instance MonadConnServer SerImplementation where
  getDict      = SerImplementation $ gets serDict
  getsDict   f = SerImplementation $ gets $ f . serDict
  modifyDict f =
    SerImplementation $ modify $ \serS -> serS {serDict = f $ serDict serS}
  putDict    s =
    SerImplementation $ modify $ \serS -> serS {serDict = s}

-- | The game-state semantics of atomic game commands
-- as computed on the server.
instance MonadAtomic SerImplementation where
  execAtomic = atomicSendSem

-- | Run an action in the @IO@ monad, with undefined state.
executorSer :: SerImplementation () -> IO ()
executorSer m =
  let saveFile (_, ser) =
        fromMaybe "save" (ssavePrefixSer (sdebugSer ser))
        <.> saveName
      exe serToSave =
        evalStateT (runSerImplementation m)
          SerState { serState = emptyState
                   , serServer = emptyStateServer
                   , serDict = EM.empty
                   , serToSave
                   }
  in Save.wrapInSaves saveFile exe
