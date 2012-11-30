{-# LANGUAGE OverloadedStrings #-}
-- | Binding of keys to commands implemented with the 'Action' monad.
module Game.LambdaHack.BindingAction
  ( stdBinding
  ) where

import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Char as Char
import Data.Tuple (swap)
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.State
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.Level
import Game.LambdaHack.Actions
import Game.LambdaHack.Running
import Game.LambdaHack.EffectAction
import Game.LambdaHack.Binding
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.ActorState
import Game.LambdaHack.Command
import Game.LambdaHack.CommandAction

heroSelection :: [((K.Key, K.Modifier), (Text, Bool, ActionFrame ()))]
heroSelection =
  let select k = do
        s <- get
        case tryFindHeroK s k of
          Nothing -> abortWith "No such member of the party."
          Just aid -> selectPlayer aid >> returnNoFrame ()
      heroSelect k = ( (K.Char (Char.intToDigit k), K.NoModifier)
                     , ("", False, select k)
                     )
  in fmap heroSelect [0..9]

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: Config.CP                 -- ^ game config
           -> Binding (ActionFrame ())  -- ^ concrete binding
stdBinding config =
  let section = Config.getItems config "macros"
      !kmacro = macroKey section
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
               ((K.Char 'r', K.Control), ("", False, modify cycleMarkVision
                                                     >> returnNoFrame ())),
               ((K.Char 'o', K.Control), ("", False, modify toggleOmniscient
                                                     >> returnNoFrame ())),
               ((K.Char 'i', K.Control), ("", False, gets (lmeta . slevel)
                                                     >>= abortWith))
             ]
  , kmacro
  , kmajor = L.map fst $ L.filter (majorCmd . snd) cmdList
  , kdir   = L.map fst cmdDir
  , krevMap = M.fromList $ map swap cmdList
  }
