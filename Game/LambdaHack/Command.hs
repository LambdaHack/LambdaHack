module Game.LambdaHack.Command where

import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
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

type Verb = String
type Noun = String

data Cmd =
    Apply     { verb :: Verb, noun :: Noun, syms :: [Char] }
  | Project   { verb :: Verb, noun :: Noun, syms :: [Char] }
  | Close
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

-- | Display command help. TODO: Should be defined in Actions module.
displayHelp :: Action ()
displayHelp = do
  let coImage (_, macros, _) k =
        let domain = M.keysSet macros
        in if k `S.member` domain
           then []
           else k : [ from | (from, to) <- M.assocs macros, to == k ]
  aliases <- session (return . coImage)
  config  <- gets sconfig
  let helpString = keyHelp aliases (stdKeybindings config)
  messageOverlayConfirm "Basic keys:" helpString
  abort

heroSelection :: [(K.Key, Command)]
heroSelection =
  let heroSelect k = (K.Char (Char.intToDigit k),
                      Undescribed $
                      selectPlayer (AHero k) >> return ())
  in fmap heroSelect [0..9]

configCommands :: Config.CP -> [(K.Key, Command)]
configCommands config =
  let section = Config.getItems config "commands"
      mkKey s =
        case K.keyTranslate s of
          K.Unknown _ -> assert `failure` ("unknown command key " ++ s)
          key -> key
      mkCmd s =
        case read s :: Cmd of
          Apply verb noun syms ->
            let prompt = verb ++ " " ++ addIndefinite noun
                command = checkCursor $ playerApplyGroupItem verb noun syms
            in Described prompt command
          Project verb noun syms ->
            let prompt = verb ++ " " ++ addIndefinite noun
                command = checkCursor $ playerProjectGroupItem verb noun syms
            in Described prompt command
          Close ->     Described "close a door"      (checkCursor closeDoor)
          Pickup ->    Described "get an object"     (checkCursor pickupItem)
          Drop ->      Described "drop an object"    (checkCursor dropItem)
          Inventory -> Described "display inventory" inventory
          Ascend ->    Described "ascend a level"    (lvlGoUp True)
          Descend ->   Described "descend a level"   (lvlGoUp False)
          TgtFloor ->  Described "target location"   targetFloor
          TgtEnemy ->  Described "target monster"    (checkCursor targetMonster)
          GameSave ->  Described "save and exit the game" saveGame
          GameQuit ->  Described "quit without saving" quitGame
          Cancel ->    Described "cancel action"     cancelCurrent
          Accept ->    Described "accept choice"     (acceptCurrent displayHelp)
          History ->   Described "display previous messages" displayHistory
          CfgDump ->   Described "dump current configuration" dumpConfig
          HeroCycle -> Described "cycle among heroes on level" cycleHero
          Version ->   Described "display game version" (abortWith version)
          Help ->      Described "display help"      displayHelp
          Wait ->      Undescribed playerAdvanceTime
      mkCommand (key, def) = (mkKey key, mkCmd def)
  in L.map mkCommand section

-- TODO: Keep in session, instead of recomputing before each command.
stdKeybindings :: Config.CP -> Keybindings
stdKeybindings config = Keybindings
  { kdir   = moveDirCommand,
    kudir  = runDirCommand,
    kother = M.fromList $
             heroSelection ++
             configCommands config ++
             [ -- debug commands, TODO: access them from a common menu or prefix
               (K.Char 'R', Undescribed $ modify toggleVision),
               (K.Char 'O', Undescribed $ modify toggleOmniscient),
               (K.Char 'I', Undescribed $ gets (lmeta . slevel) >>= abortWith)
             ]
  }
