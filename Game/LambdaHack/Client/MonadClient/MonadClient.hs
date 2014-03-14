{-# LANGUAGE FunctionalDependencies, RankNTypes #-}
-- | Basic type classes for game actions.
-- This module should not be imported anywhere except in 'Action'
-- and 'TypeAction'.
module Game.LambdaHack.Client.MonadClient.MonadClient where

import Control.Concurrent

import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Frontend (FrontReq)

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
-- Auxiliary AI and computer player clients have no @sfs@ nor @sbinding@.
data SessionUI = SessionUI
  { sfconn   :: !ConnFrontend  -- ^ connection with the frontend
  , sbinding :: !Binding       -- ^ binding of keys to commands
  , sescMVar :: !(Maybe (MVar ()))
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

class MonadClient m => MonadClientUI m where
  getsSession  :: (SessionUI -> a) -> m a

class MonadClient m => MonadClientReadServer c m | m -> c where
  readServer  :: m c

class MonadClient m => MonadClientWriteServer d m | m -> d where
  writeServer  :: d -> m ()

saveName :: FactionId -> Bool -> String
saveName side isAI =
  let n = fromEnum side  -- we depend on the numbering hack to number saves
  in (if n > 0
      then "human_" ++ show n
      else "computer_" ++ show (-n))
     ++ if isAI then ".ai.sav" else ".ui.sav"
