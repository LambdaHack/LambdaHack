{-# LANGUAGE OverloadedStrings #-}
-- | Binding of keys to commands implemented with the 'Action' monad.
module Game.LambdaHack.BindingAction
  ( stdBinding
  ) where

import Control.Monad.State hiding (State, get, gets, state)
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import Data.Tuple (swap)

import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.ActorState
import Game.LambdaHack.Binding
import Game.LambdaHack.Command
import Game.LambdaHack.CommandAction
import Game.LambdaHack.Config
import Game.LambdaHack.EffectAction
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Level
import Game.LambdaHack.Running
import Game.LambdaHack.State

heroSelection :: MonadAction m
              => [((K.Key, K.Modifier), (Text, Bool, WriterT Frames m ()))]
heroSelection =
  let select k = do
        s <- get
        case tryFindHeroK s k of
          Nothing  -> abortWith "No such member of the party."
          Just aid -> void $ selectPlayer aid
      heroSelect k = ( (K.Char (Char.intToDigit k), K.NoModifier)
                     , ("", False, select k)
                     )
  in fmap heroSelect [0..9]

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: (MonadIO m, MonadAction m)
           => ConfigUI                  -- ^ game config
           -> Binding (WriterT Frames m ())  -- ^ concrete binding
stdBinding !config@ConfigUI{configMacros} =
  let kmacro = M.fromList $ configMacros
      cmdList = configCmds config
      semList = semanticsCmds cmdList
      moveWidth f = do
        lxsize <- gets (lxsize . slevel)
        move $ f lxsize
      runWidth f = do
        lxsize <- gets (lxsize . slevel)
        run (f lxsize, 0)
      -- Targeting cursor movement and others are wrongly marked as timed;
      -- fixed in their definitions by rewinding time.
      cmdDir = K.moveBinding moveWidth runWidth
  in Binding
  { kcmd   = M.fromList $
             cmdDir ++
             heroSelection ++
             semList ++
             [ -- Debug commands.
               ((K.Char 'r', K.Control), ("", False, modify cycleMarkVision)),
               ((K.Char 'o', K.Control), ("", False, modify toggleOmniscient)),
               ((K.Char 'i', K.Control), ("", False, gets (lmeta . slevel)
                                                     >>= abortWith))
             ]
  , kmacro
  , kmajor = L.map fst $ L.filter (majorCmd . snd) cmdList
  , kdir   = L.map fst cmdDir
  , krevMap = M.fromList $ map swap cmdList
  }
