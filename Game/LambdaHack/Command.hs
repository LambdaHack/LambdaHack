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
    -- These take time:
    Apply       { verb :: Verb, object :: Object, syms :: [Char] }
  | Project     { verb :: Verb, object :: Object, syms :: [Char] }
  | TriggerDir  { verb :: Verb, object :: Object, feature :: F.Feature }
  | TriggerTile { verb :: Verb, object :: Object, feature :: F.Feature }
  | Pickup
  | Drop
  | Wait
    -- These do not take time:
  | Inventory
  | TgtFloor
  | TgtEnemy
  | TgtAscend Int
  | EpsIncr Bool
  | GameSave
  | GameQuit
  | Cancel
  | Accept
  | History
  | CfgDump
  | HeroCycle
  | Version
  | Help
  | Redraw
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
  GameSave      -> True
  GameQuit      -> True
  Wait          -> True
  _             -> False

-- | The semantics of player commands in terms of the @Action@ monad.
cmdSemantics :: Cmd -> ActionFrame ()
cmdSemantics cmd = case cmd of
  Apply{..}       -> inFrame $ playerApplyGroupItem verb object syms
  Project{..}     -> playerProjectGroupItem verb object syms
  TriggerDir{..}  -> inFrame $ playerTriggerDir feature verb
  TriggerTile{..} -> inFrame $ playerTriggerTile feature
  Pickup ->    inFrame $ pickupItem
  Drop ->      inFrame $ dropItem
  Wait ->      inFrame $ return ()

  Inventory -> inventory
  TgtFloor ->  targetFloor   TgtExplicit
  TgtEnemy ->  targetMonster TgtExplicit
  TgtAscend k -> tgtAscend k
  EpsIncr b -> inFrame $ epsIncr b
  GameSave ->  inFrame $ saveExit
  GameQuit ->  inFrame $ quitGame
  Cancel ->    cancelCurrent displayMainMenu
  Accept ->    acceptCurrent displayHelp
  History ->   displayHistory
  CfgDump ->   inFrame $ dumpConfig
  HeroCycle -> inFrame $ cycleHero
  Version ->   inFrame $ gameVersion2
  Help ->      displayHelp
  Redraw ->    inFrame $ redraw

-- | Description of player commands.
cmdDescription :: Cmd -> String
cmdDescription cmd = case cmd of
  Apply{..}       -> verb ++ " " ++ addIndefinite object
  Project{..}     -> verb ++ " " ++ addIndefinite object
  TriggerDir{..}  -> verb ++ " " ++ addIndefinite object
  TriggerTile{..} -> verb ++ " " ++ addIndefinite object
  Pickup ->    "get an object"
  Drop ->      "drop an object"
  Wait ->      ""

  Inventory -> "display inventory"
  TgtFloor ->  "target location"
  TgtEnemy ->  "target monster"
  TgtAscend k | k == 1  -> "target next shallower level"
  TgtAscend k | k >= 2  -> "target " ++ show k    ++ " levels shallower"
  TgtAscend k | k == -1 -> "target next deeper level"
  TgtAscend k | k <= -2 -> "target " ++ show (-k) ++ " levels deeper"
  TgtAscend _ -> error "void level change in targeting mode in config file"
  EpsIncr True  -> "swerve targeting line"
  EpsIncr False -> "unswerve targeting line"
  GameSave ->  "save and exit the game"
  GameQuit ->  "quit without saving"
  Cancel ->    "cancel action"
  Accept ->    "accept choice"
  History ->   "display previous messages"
  CfgDump ->   "dump current configuration"
  HeroCycle -> "cycle among heroes on level"
  Version ->   "display game version"
  Help ->      "display help"
  Redraw ->    "clear messages"
