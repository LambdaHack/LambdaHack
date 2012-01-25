module Game.LambdaHack.BindingAction
  ( stdKeybinding
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
import Game.LambdaHack.Keybinding
import qualified Game.LambdaHack.Keys as K
import Game.LambdaHack.Actor
import Game.LambdaHack.Command

configCommands :: Config.CP -> [(K.Key, Cmd)]
configCommands config =
  let section = Config.getItems config "commands"
      mkKey s =
        case K.keyTranslate s of
          K.Unknown _ -> assert `failure` ("unknown command key " ++ s)
          key -> key
      mkCmd s = read s :: Cmd
      mkCommand (key, def) = (mkKey key, mkCmd def)
  in L.map mkCommand section

semanticsCommands :: [(K.Key, Cmd)]
                  -> (Cmd -> Action ())
                  -> (Cmd -> String)
                  -> [(K.Key, (String, Action ()))]
semanticsCommands cmdList cmdS cmdD =
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

stdKeybinding :: Config.CP
              -> (Cmd -> Action ())
              -> (Cmd -> String)
              -> Keybinding (Action ())
stdKeybinding config cmdS cmdD =
  let section = Config.getItems config "macros"
      !kmacro = macroKey section
      cmdList = configCommands config
      semList = semanticsCommands cmdList cmdS cmdD
      moveWidth f = do
        lxsize <- gets (lxsize . slevel)
        move $ f lxsize
      runWidth f = do
        lxsize <- gets (lxsize . slevel)
        run (f lxsize, 0)
  in Keybinding
  { kcmd   = M.fromList $
             K.moveBinding moveWidth runWidth ++
             heroSelection ++
             semList ++
             [ -- debug commands, TODO: access them from a common menu or prefix
               (K.Char 'R', ("", modify toggleVision)),
               (K.Char 'O', ("", modify toggleOmniscient)),
               (K.Char 'I', ("", gets (lmeta . slevel) >>= abortWith))
             ]
  , kmacro
  , kmajor = L.map fst $ L.filter (majorCmd . snd) cmdList
  , ktimed = L.map fst $ L.filter (timedCmd . snd) cmdList
  }
