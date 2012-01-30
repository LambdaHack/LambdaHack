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
import Game.LambdaHack.Actor
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
             -> (Cmd -> Action ())
             -> (Cmd -> String)
             -> [(K.Key, (String, Action ()))]
semanticsCmd cmdList cmdS cmdD =
  let mkDescribed cmd =
        let semantics = if timedCmd cmd
                        then checkCursor $ cmdS cmd
                        else cmdS cmd
        in (cmdD cmd, semantics)
      mkCommand (key, def) = (key, mkDescribed def)
  in L.map mkCommand cmdList

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: Action () -> Action ()
checkCursor h = do
  cursor <- gets scursor
  slid <- gets slid
  if creturnLn cursor == slid
    then h
    else abortWith "this command does not work on remote levels"

heroSelection :: [(K.Key, (String, Action ()))]
heroSelection =
  let heroSelect k = (K.Char (Char.intToDigit k),
                      ("", selectPlayer (AHero k) >> return ()))
  in fmap heroSelect [0..9]

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: Config.CP            -- ^ game config
           -> (Cmd -> Action ())   -- ^ semantics of abstract commands
           -> (Cmd -> String)      -- ^ description of abstract commands
           -> Binding (Action ())  -- ^ concrete binding
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
  in Binding
  { kcmd   = M.fromList $
             K.moveBinding moveWidth runWidth ++
             heroSelection ++
             semList ++
             [ -- debug commands, TODO:access them from a common menu or prefix
               (K.Char 'R', ("", modify cycleMarkVision)),
               (K.Char 'O', ("", modify toggleOmniscient)),
               (K.Char 'I', ("", gets (lmeta . slevel) >>= abortWith))
             ]
  , kmacro
  , kmajor = L.map fst $ L.filter (majorCmd . snd) cmdList
  , ktimed = L.map fst $ L.filter (timedCmd . snd) cmdList
  }
