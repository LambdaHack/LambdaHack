-- | Abstract syntax of player commands.
module Game.LambdaHack.Command
  ( Cmd(..), majorCmd, timedCmd, cmdDescription
  ) where

import Game.LambdaHack.Grammar
import qualified Game.LambdaHack.Feature as F

-- | Abstract syntax of player commands. The type is abstract, but the values
-- are created outside this module via the Read class (from config file) .
data Cmd =
    -- These take time.
    Apply       { verb :: Verb, object :: Object, syms :: [Char] }
  | Project     { verb :: Verb, object :: Object, syms :: [Char] }
  | TriggerDir  { verb :: Verb, object :: Object, feature :: F.Feature }
  | TriggerTile { verb :: Verb, object :: Object, feature :: F.Feature }
  | Pickup
  | Drop
  | Wait
  | GameSave
    -- These do not take time, or not quite.
  | Inventory
  | TgtFloor
  | TgtEnemy
  | TgtAscend Int
  | EpsIncr Bool
  | GameExit
  | GameRestart
  | Cancel
  | Accept
  | Clear
  | History
  | CfgDump
  | HeroCycle
  | Help
  deriving (Show, Read, Eq, Ord)

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
  GameExit      -> True
  GameRestart   -> True
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
  Wait          -> True
  GameExit      -> True  -- takes time, then rewinds time
  GameRestart   -> True  -- takes time, then resets state
  _             -> False

-- | Description of player commands.
cmdDescription :: Cmd -> String
cmdDescription cmd = case cmd of
  Apply{..}       -> verb ++ " " ++ addIndefinite object
  Project{..}     -> verb ++ " " ++ addIndefinite object
  TriggerDir{..}  -> verb ++ " " ++ addIndefinite object
  TriggerTile{..} -> verb ++ " " ++ addIndefinite object
  Pickup    -> "get an object"
  Drop      -> "drop an object"
  Wait      -> ""
  GameSave  -> "save game"

  Inventory -> "display inventory"
  TgtFloor  -> "target location"
  TgtEnemy  -> "target monster"
  TgtAscend k | k == 1  -> "target next shallower level"
  TgtAscend k | k >= 2  -> "target " ++ show k    ++ " levels shallower"
  TgtAscend k | k == -1 -> "target next deeper level"
  TgtAscend k | k <= -2 -> "target " ++ show (-k) ++ " levels deeper"
  TgtAscend _ -> error "void level change in targeting mode in config file"
  EpsIncr True  -> "swerve targeting line"
  EpsIncr False -> "unswerve targeting line"
  GameExit  -> "save and exit"
  GameRestart -> "restart game"
  Cancel    -> "cancel action"
  Accept    -> "accept choice"
  Clear     -> "clear messages"
  History   -> "display previous messages"
  CfgDump   -> "dump current configuration"
  HeroCycle -> "cycle among heroes on level"
  Help      -> "display help"
