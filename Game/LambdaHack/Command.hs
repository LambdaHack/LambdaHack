-- | Abstract syntax of player commands and their semantics.
module Game.LambdaHack.Command
  ( Cmd, majorCmd, timedCmd, cmdSemantics, cmdDescription
  ) where

import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.ItemAction
import Game.LambdaHack.Grammar
import Game.LambdaHack.EffectAction
import Game.LambdaHack.State
import qualified Game.LambdaHack.Feature as F

-- | Abstract syntax of player commands. The type is abstract, but the values
-- are created outside this module via the Read class (from config file) .
data Cmd =
    Apply       { verb :: Verb, object :: Object, syms :: [Char] }
  | Project     { verb :: Verb, object :: Object, syms :: [Char] }
  | TriggerDir  { verb :: Verb, object :: Object, feature :: F.Feature }
  | TriggerTile { verb :: Verb, object :: Object, feature :: F.Feature }
  | Pickup
  | Drop
  | Inventory
  | TgtFloor
  | TgtEnemy
  | TgtAscend Int
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

-- | Major commands land on the first page of command help.
majorCmd :: Cmd -> Bool
majorCmd cmd = case cmd of
  Apply{}       -> True
  Project{}     -> True
  TriggerDir{}  -> True
  TriggerTile{} -> True
  Pickup        -> True
  Drop          -> True
  Inventory     -> True
  GameSave      -> True
  GameQuit      -> True
  Help          -> True
  _             -> False

-- TODO: Advance time automatically for these, but somehow advance
-- time for monsters, too. Perhaps wait until monsters use commands, too
-- (or rather the micro-commands to be added in the future).

-- | Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
timedCmd :: Cmd -> Bool
timedCmd cmd = case cmd of
  Apply{}       -> True
  Project{}     -> True
  TriggerDir{}  -> True
  TriggerTile{} -> True
  Pickup        -> True
  Drop          -> True
  Wait          -> True
  _             -> False

-- | The semantics of player commands in terms of the @Action@ monad.
cmdSemantics :: Cmd -> Action ()
cmdSemantics cmd = case cmd of
  Apply{..}       -> playerApplyGroupItem verb object syms
  Project{..}     -> playerProjectGroupItem verb object syms
  TriggerDir{..}  -> playerTriggerDir feature
  TriggerTile{..} -> playerTriggerTile feature
  Pickup ->    pickupItem
  Drop ->      dropItem
  Inventory -> inventory
  TgtFloor ->  targetFloor   TgtPlayer
  TgtEnemy ->  targetMonster TgtPlayer
  TgtAscend k -> tgtAscend k
  GameSave ->  saveGame
  GameQuit ->  quitGame
  Cancel ->    cancelCurrent
  Accept ->    acceptCurrent displayHelp
  History ->   displayHistory
  CfgDump ->   dumpConfig
  HeroCycle -> cycleHero
  Version ->   gameVersion
  Help ->      displayHelp
  Wait ->      playerAdvanceTime

-- | Description of player commands.
cmdDescription :: Cmd -> Maybe String
cmdDescription cmd = case cmd of
  Apply{..}       -> Just $ verb ++ " " ++ addIndefinite object
  Project{..}     -> Just $ verb ++ " " ++ addIndefinite object
  TriggerDir{..}  -> Just $ verb ++ " " ++ addIndefinite object
  TriggerTile{..} -> Just $ verb ++ " " ++ addIndefinite object
  Pickup ->    Just "get an object"
  Drop ->      Just "drop an object"
  Inventory -> Just "display inventory"
  TgtFloor ->  Just "target location"
  TgtEnemy ->  Just "target monster"
  TgtAscend k | k == 1  -> Just $ "target next shallower level"
  TgtAscend k | k >= 2  -> Just $ "target " ++ show k    ++ " levels shallower"
  TgtAscend k | k == -1 -> Just $ "target next deeper level"
  TgtAscend k | k <= -2 -> Just $ "target " ++ show (-k) ++ " levels deeper"
  TgtAscend _ -> error $ "void level change in targeting mode in config file"
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
