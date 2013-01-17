{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Abstract syntax human player commands.
module Game.LambdaHack.Client.CmdHuman
  ( CmdHuman(..)
  , majorCmdHuman, minorCmdHuman, noRemoteCmdHuman, cmdDescription
  ) where

import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Msg
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.VectorXY

-- | Abstract syntax of player commands.
data CmdHuman =
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
    -- These do not take time.
  | GameExit
  | GameRestart
  | GameSave
  | CfgDump
  | Inventory
  | TgtFloor
  | TgtEnemy
  | TgtAscend Int
  | EpsIncr Bool
  | Cancel
  | Accept
  | Clear
  | History
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
majorCmdHuman :: CmdHuman -> Bool
majorCmdHuman cmd = case cmd of
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
minorCmdHuman :: CmdHuman -> Bool
minorCmdHuman cmd = case cmd of
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

-- | Commands that are forbidden on a remote level, because they
-- would usually take time when invoked on one.
-- Not that movement commands are not included, because they take time
-- on normal levels, but don't take time on remote levels, that is,
-- in targeting mode.
noRemoteCmdHuman :: CmdHuman -> Bool
noRemoteCmdHuman cmd = case cmd of
  Apply{}       -> True
  Project{}     -> True
  TriggerDir{}  -> True
  TriggerTile{} -> True
  Pickup        -> True
  Drop          -> True
  Wait          -> True
  _             -> False

-- | Description of player commands.
cmdDescription :: CmdHuman -> Text
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
  CfgDump     -> "dump current configuration"
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
  HeroCycle -> "cycle among heroes on level"
  HeroBack  -> "cycle among heroes in the dungeon"
  Help      -> "display help"
  SelectHero{} -> "select hero"
  DebugArea    -> "debug visible area"
  DebugOmni    -> "debug omniscience"
  DebugSmell   -> "debug smell"
  DebugVision  -> "debug vision modes"
