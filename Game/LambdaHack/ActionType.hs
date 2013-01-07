{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
-- | The main game action monad type implementation. Just as any other
-- component of the library, this implementation can be substituted.
-- This module should not be imported anywhere in the library.
module Game.LambdaHack.ActionType
  ( FunAction, Action, executor
  ) where

import Control.Arrow (first, second)
import Control.Monad.Reader.Class
import qualified Data.IntMap as IM
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.ActionClass
import Game.LambdaHack.Binding
import Game.LambdaHack.Config
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.State

-- | The type of the function inside any full-power action.
type FunAction a =
   Session                            -- ^ client session setup data
   -> Pers                            -- ^ cached perception
   -> (State -> StateServer -> StateDict -> a -> IO ())
                                      -- ^ continuation
   -> (Msg -> IO ())                  -- ^ failure/reset continuation
   -> State                           -- ^ current globalstate
   -> StateServer                     -- ^ current server state
   -> StateDict                       -- ^ current state of all factions
   -> IO ()

-- | Actions of player-controlled characters and of any other actors.
newtype Action a = Action {runAction :: FunAction a}

-- | Invokes the action continuation on the provided argument.
returnAction :: a -> Action a
returnAction x = Action (\_c _p k _a s ser d -> k s ser d x)

-- | Distributes the session and shutdown continuation,
-- threads the state and cli.
bindAction :: Action a -> (a -> Action b) -> Action b
bindAction m f = Action (\c p k a s ser d ->
                          let next ns nser nd x =
                                runAction (f x) c p k a ns nser nd
                          in runAction m c p next a s ser d)

instance Monad Action where
  return = returnAction
  (>>=)  = bindAction

-- TODO: make sure fmap is inlinded and all else is inlined in this file
instance Functor Action where
  fmap f m =
    Action (\c p k a s ser d ->
               runAction m c p (\s' ser' d' ->
                                   k s' ser' d'. f) a s ser d)

instance MonadReader Pers Action where
  ask       = Action (\_c p k _a s ser d -> k s ser d p)
  local f m = Action (\c p k a s ser d ->
                         runAction m c (f p) k a s ser d)

instance Show (Action a) where
  show _ = "an action"

instance MonadActionRoot Action where
  tryWith exc m =
    Action (\c p k a s ser d ->
             let runA msg = runAction (exc msg) c p k a s ser d
             in runAction m c p k runA s ser d)
  abortWith msg = Action (\_c _p _k a _s _ser _d -> a msg)

instance MonadServerRO Action where
  getGlobal  = Action (\_c _p k _a s ser d -> k s ser d s)
  getsGlobal = (`fmap` getGlobal)
  getServer  = Action (\_c _p k _a s ser d -> k s ser d ser)
  getsServer = (`fmap` getServer)

instance MonadClientRO Action where
  getsSession f = Action (\c _p k _a s ser d -> k s ser d (f c))
  getClient  = do
    sside <- getsGlobal sside
    d <- getDict
    return $! fst $! d IM.! sside
  getsClient = (`fmap` getClient)
  getLocal   = do
    sside <- getsGlobal sside
    d <- getDict
    return $! snd $! d IM.! sside
  getsLocal  = (`fmap` getLocal)

instance MonadClientServerRO Action where

instance MonadActionRO Action where
  getDict  = Action (\_c _p k _a s ser d -> k s ser d d)
  getsDict = (`fmap` getDict)

instance MonadActionIO Action where
  liftIO x = Action (\_c _p k _a s ser d -> x >>= k s ser d)

instance MonadServer Action where
  modifyGlobal f = Action (\_c _p k _a s ser d -> k (f s) ser d ())
  putGlobal      = modifyGlobal . const
  modifyServer f = Action (\_c _p k _a s ser d -> k s (f ser) d ())
  putServer      = modifyServer . const
  -- Temporary hook until all clients save their local state separately.
  getForSaveGame = Action (\_c _p k _a s ser d -> k s ser d d)

instance MonadClient Action where
  modifyClient f = do
    sside <- getsGlobal sside
    modifyDict (IM.adjust (first f) sside)
  putClient      = modifyClient . const
  modifyLocal f  = do
    sside <- getsGlobal sside
    modifyDict (IM.adjust (second f) sside)
  putLocal       = modifyLocal . const

instance MonadClientServer Action where

instance MonadAction Action where
  modifyDict f   = Action (\_c _p k _a s ser d -> k s ser (f d) ())
  putDict      = modifyDict . const

-- | Run an action, with a given session, state and cli, in the @IO@ monad.
executor :: Action ()
         -> FrontendSession -> Kind.COps -> Binding -> ConfigUI
         -> State -> StateServer -> StateDict -> IO ()
executor m sfs scops sbinding sconfigUI s ser d =
  runAction m
    Session{..}
    (dungeonPerception scops (sconfig ser) (sdebugSer ser) s)
    (\_ _ _ _ -> return ())  -- final continuation returns result
    (\msg -> fail $ T.unpack $ "unhandled abort:" <+> msg)  -- e.g., in AI code
    s
    ser
    d
