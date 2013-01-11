{-# LANGUAGE OverloadedStrings #-}
-- | Abstract syntax of server and client commands.
module Game.LambdaHack.Command
  ( CmdSer(..), Cmd(..)
  , majorCmd, minorCmd, timedCmd, cmdDescription
  ) where

import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Actor
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector
import Game.LambdaHack.VectorXY

-- | Abstract syntax of server commands.
data CmdSer =
    ApplySer ActorId MU.Part Item
  | ProjectSer ActorId Point MU.Part Item
  | TriggerSer ActorId Point
  | PickupSer ActorId Item Char
  | DropSer ActorId Item
  | WaitSer ActorId
  | MoveSer ActorId Vector
  | RunSer ActorId Vector
  | GameExitSer
  | GameRestartSer
  | GameSaveSer
  | CfgDumpSer
  deriving Show

-- | Abstract syntax of player commands.
data Cmd =
    -- These usually take time.
    Apply       { verb :: MU.Part, object :: MU.Part, syms :: [Char] }
  | Project     { verb :: MU.Part, object :: MU.Part, syms :: [Char] }
  | TriggerDir  { verb :: MU.Part, object :: MU.Part, feature :: F.Feature }
  | TriggerTile { verb :: MU.Part, object :: MU.Part, feature :: F.Feature }
  | Pickup
  | Drop
  | Wait
  | Move VectorXY
  | Run VectorXY
  | GameExit
  | GameRestart
    -- These do not take time, or not quite.
  | GameSave
  | Inventory
  | TgtFloor
  | TgtEnemy
  | TgtAscend Int
  | EpsIncr Bool
  | Cancel
  | Accept
  | Clear
  | History
  | CfgDump
  | HeroCycle
  | HeroBack
  | Help
  | SelectHero Int
  | DebugArea
  | DebugOmni
  | DebugSmell
  | DebugVision
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
  GameExit      -> True
  GameRestart   -> True
  GameSave      -> True
  Inventory     -> True
  Help          -> True
  _             -> False

-- | Minor commands land on the second page of command help.
minorCmd :: Cmd -> Bool
minorCmd cmd = case cmd of
  TgtFloor    -> True
  TgtEnemy    -> True
  TgtAscend{} -> True
  EpsIncr{}   -> True
  Cancel      -> True
  Accept      -> True
  Clear       -> True
  History     -> True
  CfgDump     -> True
  HeroCycle   -> True
  HeroBack    -> True
  _           -> False

-- | Commands that usually take time are marked as such in help.
-- Whether they take time in a particular situation is decided
-- each time in 'cmdAction'.
timedCmd :: Cmd -> Bool
timedCmd cmd = case cmd of
  Apply{}       -> True
  Project{}     -> True
  TriggerDir{}  -> True
  TriggerTile{} -> True
  Pickup        -> True
  Drop          -> True
  Wait          -> True
  Move{}        -> True
  Run{}         -> True
  GameExit      -> True  -- takes time, then rewinds time
  GameRestart   -> True  -- takes time, then resets state
  _             -> False

-- | Description of player commands.
cmdDescription :: Cmd -> Text
cmdDescription cmd = case cmd of
  Apply{..}       -> makePhrase [verb, MU.AW object]
  Project{..}     -> makePhrase [verb, MU.AW object]
  TriggerDir{..}  -> makePhrase [verb, MU.AW object]
  TriggerTile{..} -> makePhrase [verb, MU.AW object]
  Pickup      -> "get an object"
  Drop        -> "drop an object"
  Move{}      -> "move"
  Run{}       -> "run"
  Wait        -> "wait"
  GameExit    -> "save and exit"
  GameRestart -> "restart game"
  GameSave    -> "save game"

  Inventory   -> "display inventory"
  TgtFloor    -> "target position"
  TgtEnemy    -> "target monster"
  TgtAscend k | k == 1  -> "target next shallower level"
  TgtAscend k | k >= 2  -> "target" <+> showT k    <+> "levels shallower"
  TgtAscend k | k == -1 -> "target next deeper level"
  TgtAscend k | k <= -2 -> "target" <+> showT (-k) <+> "levels deeper"
  TgtAscend _ ->
    assert `failure` ("void level change in targeting in config file" :: Text)
  EpsIncr True  -> "swerve targeting line"
  EpsIncr False -> "unswerve targeting line"
  Cancel    -> "cancel action"
  Accept    -> "accept choice"
  Clear     -> "clear messages"
  History   -> "display previous messages"
  CfgDump   -> "dump current configuration"
  HeroCycle -> "cycle among heroes on level"
  HeroBack  -> "cycle among heroes in the dungeon"
  Help      -> "display help"
  SelectHero{} -> "select hero"
  DebugArea    -> "debug visible area"
  DebugOmni    -> "debug omniscience"
  DebugSmell   -> "debug smell"
  DebugVision  -> "debug vision modes"
