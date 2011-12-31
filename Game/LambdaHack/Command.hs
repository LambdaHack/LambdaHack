module Game.LambdaHack.Command where

import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Char as Char

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.ItemAction
import Game.LambdaHack.Grammar
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.EffectAction
import Game.LambdaHack.Keybindings
import qualified Game.LambdaHack.Keys as K
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.State
import Game.LambdaHack.Version
import Game.LambdaHack.Dir
import qualified Game.LambdaHack.Feature as F

data Cmd =
    Apply     { verb :: Verb, object :: Object, syms :: [Char] }
  | Project   { verb :: Verb, object :: Object, syms :: [Char] }
  | Trigger   { verb :: Verb, object :: Object, feature :: F.Feature }
  | Pickup
  | Drop
  | Inventory
  | Ascend
  | Descend
  | TgtFloor
  | TgtEnemy
  | GameSave
  | GameQuit
  | Cancel
  | Accept
  | History
  | CfgDump
  | HeroCycle
  | Version
  | Help
  | Wait
  deriving (Show, Read)

moveDirCommand, runDirCommand :: Described (Dir -> Action ())
moveDirCommand   = Described "move in direction" move
runDirCommand    = Described "run in direction"  (\ dir -> run (dir, 0))

heroSelection :: [(K.Key, Described (Action ()))]
heroSelection =
  let heroSelect k = (K.Char (Char.intToDigit k),
                      Undescribed $
                      selectPlayer (AHero k) >> return ())
  in fmap heroSelect [0..9]

cmdSemantics :: Cmd -> Action ()
cmdSemantics cmd = case cmd of
  Apply verb obj syms -> checkCursor $ playerApplyGroupItem verb obj syms
  Project verb obj syms -> checkCursor $ playerProjectGroupItem verb obj syms
  Trigger _verb _obj feat -> checkCursor $ playerTriggerTile feat
  Pickup ->    checkCursor pickupItem
  Drop ->      checkCursor dropItem
  Inventory -> inventory
  Ascend ->    lvlGoUp True
  Descend ->   lvlGoUp False
  TgtFloor ->  targetFloor
  TgtEnemy ->  checkCursor targetMonster
  GameSave ->  saveGame
  GameQuit ->  quitGame
  Cancel ->    cancelCurrent
  Accept ->    acceptCurrent displayHelp
  History ->   displayHistory
  CfgDump ->   dumpConfig
  HeroCycle -> cycleHero
  Version ->   abortWith version
  Help ->      displayHelp
  Wait ->      playerAdvanceTime

cmdDescription :: Cmd -> Maybe String
cmdDescription cmd = case cmd of
  Apply verb obj _syms -> Just $ verb ++ " " ++ addIndefinite obj
  Project verb obj _syms -> Just $ verb ++ " " ++ addIndefinite obj
  Trigger verb obj _feat -> Just $ verb ++ " " ++ addIndefinite obj
  Pickup ->    Just "get an object"
  Drop ->      Just "drop an object"
  Inventory -> Just "display inventory"
  Ascend ->    Just "ascend a level"
  Descend ->   Just "descend a level"
  TgtFloor ->  Just "target location"
  TgtEnemy ->  Just "target monster"
  GameSave ->  Just "save and exit the game"
  GameQuit ->  Just "quit without saving"
  Cancel ->    Just "cancel action"
  Accept ->    Just "accept choice"
  History ->   Just "display previous messages"
  CfgDump ->   Just "dump current configuration"
  HeroCycle -> Just "cycle among heroes on level"
  Version ->   Just "display game version"
  Help ->      Just "display help"
  Wait ->      Nothing

configCommands :: Config.CP
               -> (Cmd -> Action ())
               -> (Cmd -> Maybe String)
               -> [(K.Key, Described (Action ()))]
configCommands config cmdS cmdD =
  let section = Config.getItems config "commands"
      mkKey s =
        case K.keyTranslate s of
          K.Unknown _ -> assert `failure` ("unknown command key " ++ s)
          key -> key
      mkDescribed s =
        let cmd = read s :: Cmd
        in case cmdD cmd of
          Nothing -> Undescribed $ cmdS cmd
          Just d  -> Described d $ cmdS cmd
      mkCommand (key, def) = (mkKey key, mkDescribed def)
  in L.map mkCommand section

stdKeybindings :: Config.CP
               -> (Cmd -> Action ())
               -> (Cmd -> Maybe String)
               -> Keybindings (Action ())
stdKeybindings config cmdS cmdD = Keybindings
  { kdir   = moveDirCommand,
    kudir  = runDirCommand,
    kother = M.fromList $
             heroSelection ++
             configCommands config cmdS cmdD ++
             [ -- debug commands, TODO: access them from a common menu or prefix
               (K.Char 'R', Undescribed $ modify toggleVision),
               (K.Char 'O', Undescribed $ modify toggleOmniscient),
               (K.Char 'I', Undescribed $ gets (lmeta . slevel) >>= abortWith)
             ]
  }
