{-# LANGUAGE FunctionalDependencies, RankNTypes #-}
-- | Basic type classes for game actions.
-- This module should not be imported anywhere except in 'Action'
-- and 'TypeAction'.
module Game.LambdaHack.Client.Action.ActionClass where

import Control.Monad.Writer.Strict (WriterT (WriterT), lift, runWriterT)
import Data.Monoid
import qualified Game.LambdaHack.Common.Key as K

import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Frontend (FrontReq)

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
-- Auxiliary AI and computer player clients have no @sfs@ nor @sbinding@.
data SessionUI = SessionUI
  { sfconn   :: !ConnFrontend  -- ^ connection with the frontend
  , sbinding :: !Binding       -- ^ binding of keys to commands
  }

-- | Connection method between a client and a frontend.
data ConnFrontend = ConnFrontend
  { readConnFrontend  :: MonadClientUI m => m K.KM
      -- ^ read a keystroke received from the frontend
  , writeConnFrontend :: MonadClientUI m => FrontReq -> m ()
      -- ^ write a UI request to the frontend
  }

class MonadActionRO m => MonadClient m where
  getClient    :: m StateClient
  getsClient   :: (StateClient -> a) -> m a
  modifyClient :: (StateClient -> StateClient) -> m ()
  putClient    :: StateClient -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a
  saveClient   :: m ()

instance (Monoid a, MonadClient m) => MonadClient (WriterT a m) where
  getClient    = lift getClient
  getsClient   = lift . getsClient
  modifyClient = lift . modifyClient
  putClient    = lift . putClient
  liftIO       = lift . liftIO
  saveClient   = lift saveClient

class MonadClient m => MonadClientUI m where
  getsSession  :: (SessionUI -> a) -> m a

instance (Monoid a, MonadClientUI m) => MonadClientUI (WriterT a m) where
  getsSession  = lift . getsSession

class MonadClient m => MonadClientReadServer c m | m -> c where
  readServer  :: m c

class MonadClient m => MonadClientWriteServer d m | m -> d where
  writeServer  :: d -> m ()

-- | The bottom of the action monads class semilattice.
class MonadClient m => MonadClientAbort m where
  -- Set the current exception handler. First argument is the handler,
  -- second is the computation the handler scopes over.
  tryWith      :: (Msg -> m a) -> m a -> m a
  -- Abort with the given message.
  abortWith    :: Msg -> m a

instance (Monoid a, MonadClientAbort m) => MonadClientAbort (WriterT a m) where
  tryWith exc m = WriterT $ tryWith (runWriterT . exc) (runWriterT m)
  abortWith = lift . abortWith

saveName :: FactionId -> Bool -> String
saveName side isAI =
  let n = fromEnum side  -- we depend on the numbering hack to number saves
  in (if n > 0
      then "human_" ++ show n
      else "computer_" ++ show (-n))
     ++ if isAI then ".ai.sav" else ".ui.sav"
