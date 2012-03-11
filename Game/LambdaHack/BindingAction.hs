-- | Binding of keys to commands implemented with the 'Action' monad.
module Game.LambdaHack.BindingAction
  ( stdBinding
  ) where

import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Char as Char

import Game.LambdaHack.Action
import Game.LambdaHack.State
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.Level
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Actions
import Game.LambdaHack.Running
import Game.LambdaHack.EffectAction
import Game.LambdaHack.Binding
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.ActorState
import Game.LambdaHack.Command

configCmd :: Config.CP -> [(K.Key, Cmd)]
configCmd config =
  let section = Config.getItems config "commands"
      mkKey s =
        case K.keyTranslate s of
          K.Unknown _ -> assert `failure` ("unknown command key " ++ s)
          key -> key
      mkCmd s = read s :: Cmd
      mkCommand (key, def) = (mkKey key, mkCmd def)
  in L.map mkCommand section

semanticsCmd :: [(K.Key, Cmd)]
             -> (Cmd -> ActionFrame ())
             -> (Cmd -> String)
             -> [((K.Key, K.Modifier), (String, Bool, ActionFrame ()))]
semanticsCmd cmdList cmdS cmdD =
  let mkDescribed cmd =
        let semantics = if timedCmd cmd
                        then checkCursor $ cmdS cmd
                        else cmdS cmd
        in (cmdD cmd, timedCmd cmd, semantics)
      mkCommand (key, def) = ((key, K.NoModifier), mkDescribed def)
  in L.map mkCommand cmdList

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: ActionFrame () -> ActionFrame ()
checkCursor h = do
  cursor <- gets scursor
  slid <- gets slid
  if creturnLn cursor == slid
    then h
    else abortWith "this command does not work on remote levels"

heroSelection :: [((K.Key, K.Modifier), (String, Bool, ActionFrame ()))]
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
           -> (Cmd -> ActionFrame ())   -- ^ semantics of abstract commands
           -> (Cmd -> String)           -- ^ description of abstract commands
           -> Binding (ActionFrame ())  -- ^ concrete binding
stdBinding config cmdS cmdD =
  let section = Config.getItems config "macros"
      !kmacro = macroKey section
      cmdList = configCmd config
      semList = semanticsCmd cmdList cmdS cmdD
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
  }
