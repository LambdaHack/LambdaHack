{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere except in 'Action'
-- to expose the executor to any code using the library.
module Game.LambdaHack.Server.Action.ActionType
  ( FunActionSer, ActionSer, executorSer
  ) where

import Game.LambdaHack.Action
import Game.LambdaHack.Server.Action.ActionClass
import Game.LambdaHack.Server.State
import Game.LambdaHack.State

-- | The type of the function inside any server action.
type FunActionSer a =
   (State -> StateServer -> ConnDict -> a -> IO ())
                                      -- ^ continuation
   -> State                           -- ^ current global state
   -> StateServer                     -- ^ current server state
   -> ConnDict                        -- ^ client-server connection information
   -> IO ()

-- | Server parts of actions of human and computer player characters.
newtype ActionSer a = ActionSer {runActionSer :: FunActionSer a}

-- | Invokes the action continuation on the provided argument.
returnActionSer :: a -> ActionSer a
returnActionSer x = ActionSer (\k s ser d -> k s ser d x)

-- | Distributes the session and shutdown continuation,
-- threads the state and history.
bindActionSer :: ActionSer a -> (a -> ActionSer b) -> ActionSer b
bindActionSer m f = ActionSer (\k s ser d ->
                          let next ns nser nd x =
                                runActionSer (f x) k ns nser nd
                          in runActionSer m next s ser d)

instance Monad ActionSer where
  return = returnActionSer
  (>>=)  = bindActionSer

-- TODO: make sure fmap is inlinded and all else is inlined in this file
instance Functor ActionSer where
  fmap f m =
    ActionSer (\k s ser d ->
               runActionSer m (\s' ser' d' ->
                                   k s' ser' d'. f) s ser d)

instance Show (ActionSer a) where
  show _ = "an action"

instance MonadActionRO ActionSer where
  getState  = ActionSer (\k s ser d -> k s ser d s)
  getsState = (`fmap` getState)

instance MonadAction ActionSer where
  modifyState f = ActionSer (\k s ser d -> k (f s) ser d ())
  putState      = modifyState . const

instance MonadServer ActionSer where
  getServer  = ActionSer (\k s ser d -> k s ser d ser)
  getsServer = (`fmap` getServer)
  modifyServer f = ActionSer (\k s ser d -> k s (f ser) d ())
  putServer      = modifyServer . const
  liftIO x       = ActionSer (\k s ser d -> x >>= k s ser d)

instance MonadServerChan ActionSer where
  getDict        = ActionSer (\k s ser d -> k s ser d d)
  getsDict       = (`fmap` getDict)
  modifyDict f   = ActionSer (\k s ser d -> k s ser (f d) ())
  putDict        = modifyDict . const

-- | Run an action in the @IO@ monad, with undefined state.
executorSer :: ActionSer () -> IO ()
executorSer m =
  runActionSer m
    (\_ _ _ _ -> return ())
    undefined
    undefined
    undefined
