{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.Server.Action.ActionType
  ( FunActionSer, ActionSer, executorSer
  ) where

import Control.Monad.Reader.Class
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Msg
import Game.LambdaHack.Server.Action.ActionClass
import Game.LambdaHack.State

-- | The type of the function inside any server action.
type FunActionSer a =
   Pers                               -- ^ cached perception
   -> (State -> StateServer -> ConnDict -> a -> IO ())
                                      -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current global state
   -> StateServer                     -- ^ current server state
   -> ConnDict                        -- ^ client-server connection information
   -> IO ()

-- | Actions of player-controlled characters and of any other actors.
newtype ActionSer a = ActionSer {runActionSer :: FunActionSer a}

-- | Invokes the action continuation on the provided argument.
returnActionSer :: a -> ActionSer a
returnActionSer x = ActionSer (\_p k _a s ser d -> k s ser d x)

-- | Distributes the session and shutdown continuation,
-- threads the state and history.
bindActionSer :: ActionSer a -> (a -> ActionSer b) -> ActionSer b
bindActionSer m f = ActionSer (\p k a s ser d ->
                          let next ns nser nd x =
                                runActionSer (f x) p k a ns nser nd
                          in runActionSer m p next a s ser d)

instance Monad ActionSer where
  return = returnActionSer
  (>>=)  = bindActionSer

-- TODO: make sure fmap is inlinded and all else is inlined in this file
instance Functor ActionSer where
  fmap f m =
    ActionSer (\p k a s ser d ->
               runActionSer m p (\s' ser' d' ->
                                   k s' ser' d'. f) a s ser d)

instance Show (ActionSer a) where
  show _ = "an action"

instance MonadActionAbort ActionSer where
  tryWith exc m =
    ActionSer (\p k a s ser d ->
             let runA msg = runActionSer (exc msg) p k a s ser d
             in runActionSer m p k runA s ser d)
  abortWith msg = ActionSer (\_p _k a _s _ser _d -> a msg)

instance MonadReader Pers ActionSer where
  ask       = ActionSer (\p k _a s ser d -> k s ser d p)
  local f m = ActionSer (\p k a s ser d ->
                         runActionSer m (f p) k a s ser d)

instance MonadActionRO ActionSer where
  getState  = ActionSer (\_p k _a s ser d -> k s ser d s)
  getsState = (`fmap` getState)

instance MonadAction ActionSer where
  modifyState f = ActionSer (\_p k _a s ser d -> k (f s) ser d ())
  putState      = modifyState . const

instance MonadServerRO ActionSer where
  getServer  = ActionSer (\_p k _a s ser d -> k s ser d ser)
  getsServer = (`fmap` getServer)

instance MonadServer ActionSer where
  modifyServer f = ActionSer (\_p k _a s ser d -> k s (f ser) d ())
  putServer      = modifyServer . const
  liftIO x       = ActionSer (\_p k _a s ser d -> x >>= k s ser d)

instance MonadServerChan ActionSer where
  getDict        = ActionSer (\_p k _a s ser d -> k s ser d d)
  getsDict       = (`fmap` getDict)
  modifyDict f   = ActionSer (\_p k _a s ser d -> k s ser (f d) ())
  putDict        = modifyDict . const

-- | Run an action, with a given session, state and history, in the @IO@ monad.
executorSer :: ActionSer () -> Pers -> State -> StateServer -> ConnDict
            -> IO ()
executorSer m pers s ser d =
  runActionSer m
    pers
    (\_ _ _ _ -> return ())  -- final continuation returns result
    (\msg -> let err = "unhandled server abort for side" <+> showT (getSide s)
                       <+> ":" <+> msg
             in fail $ T.unpack err)
    s
    ser
    d
