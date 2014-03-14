{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.SampleImplementation.SampleImplementationMonadClient
  ( executorCli
  ) where

import Control.Applicative
import Control.Concurrent.STM
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Maybe
import System.FilePath

import Game.LambdaHack.Atomic
import Game.LambdaHack.Atomic.HandleCmdAtomicWrite
import Game.LambdaHack.Atomic.MonadAtomic
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.Response
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State

data CliState c d = CliState
  { cliState   :: !State             -- ^ current global state
  , cliClient  :: !StateClient       -- ^ current client state
  , cliDict    :: !(ChanServer c d)  -- ^ this client connection information
  , cliToSave  :: !(Save.ChanSave (State, StateClient))
                                     -- ^ connection to the save thread
  , cliSession :: SessionUI          -- ^ UI setup data, empty for AI clients
  }

-- | Server state transformation monad.
newtype CliImplementation c d a =
    CliImplementation {runCliImplementation :: StateT (CliState c d) IO a}
  deriving (Monad, Functor, Applicative)

instance MonadReadState (CliImplementation c d) where
  getState    = CliImplementation $ gets cliState
  getsState f = CliImplementation $ gets $ f . cliState

instance MonadWriteState (CliImplementation c d) where
  modifyState f = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliState = f $ cliState cliS}
    in newCliS `seq` ((), newCliS)
  putState    s = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliState = s}
    in newCliS `seq` ((), newCliS)

instance MonadClient (CliImplementation c d) where
  getClient      = CliImplementation $ gets cliClient
  getsClient   f = CliImplementation $ gets $ f . cliClient
  modifyClient f = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliClient = f $ cliClient cliS}
    in newCliS `seq` ((), newCliS)
  putClient    s = CliImplementation $ state $ \cliS ->
    let newCliS = cliS {cliClient = s}
    in newCliS `seq` ((), newCliS)
  liftIO         = CliImplementation . IO.liftIO
  saveClient     = CliImplementation $ do
    s <- gets cliState
    cli <- gets cliClient
    toSave <- gets cliToSave
    IO.liftIO $ Save.saveToChan toSave (s, cli)

instance MonadClientUI (CliImplementation c d) where
  getsSession f  = CliImplementation $ gets $ f . cliSession

instance MonadClientReadServer c (CliImplementation c d) where
  readServer     = CliImplementation $ do
    ChanServer{fromServer} <- gets cliDict
    IO.liftIO $ atomically . readTQueue $ fromServer

instance MonadClientWriteServer d (CliImplementation c d) where
  writeServer scmd = CliImplementation $ do
    ChanServer{toServer} <- gets cliDict
    IO.liftIO $ atomically . writeTQueue toServer $ scmd

-- | The game-state semantics of atomic game commands
-- as computed on clients. Special effects (@SfxAtomic@) don't modify state.
instance MonadAtomic (CliImplementation c d) where
  execAtomic (CmdAtomic cmd) = cmdAtomicSem cmd
  execAtomic (SfxAtomic _) = return ()

-- | Init the client, then run an action, with a given session,
-- state and history, in the @IO@ monad.
executorCli :: CliImplementation c d ()
            -> SessionUI -> State -> StateClient -> ChanServer c d
            -> IO ()
executorCli m cliSession cliState cliClient cliDict =
  let saveFile (_, cli2) =
        fromMaybe "save" (ssavePrefixCli (sdebugCli cli2))
        <.> saveName (sside cli2) (sisAI cli2)
      exe cliToSave =
        evalStateT (runCliImplementation m) CliState{..}
  in Save.wrapInSaves saveFile exe
