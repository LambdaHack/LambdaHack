{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- | Abstract syntax human player commands.
module Game.LambdaHack.Client.HumanCmd
  ( HumanCmd(..)
  , majorHumanCmd, minorHumanCmd, noRemoteHumanCmd, cmdDescription
  ) where

import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.VectorXY
import Game.LambdaHack.Utils.Assert

-- | Abstract syntax of player commands.
data HumanCmd =
    -- These usually take time.
    Move VectorXY
  | Run VectorXY
  | Wait
  | Pickup
  | Drop
  | Project     { verb :: MU.Part, object :: MU.Part, syms :: [Char] }
  | Apply       { verb :: MU.Part, object :: MU.Part, syms :: [Char] }
  | TriggerDir  { verb :: MU.Part, object :: MU.Part, feature :: F.Feature }
  | TriggerTile { verb :: MU.Part, object :: MU.Part, feature :: F.Feature }
    -- These do not take time.
  | GameRestart Text
  | GameExit
  | GameSave
  | CfgDump
    -- These do not notify the server.
  | SelectHero Int
  | MemberCycle
  | MemberBack
  | Inventory
  | TgtFloor
  | TgtEnemy
  | TgtAscend Int
  | EpsIncr Bool
  | Cancel
  | Accept
  | Clear
  | History
  | MarkVision
  | MarkSmell
  | MarkSuspect
  | Help
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary HumanCmd

-- | Major commands land on the first page of command help.
majorHumanCmd :: HumanCmd -> Bool
majorHumanCmd cmd = case cmd of
  Pickup        -> True
  Drop          -> True
  Project{}     -> True
  Apply{}       -> True
  TriggerDir{}  -> True
  TriggerTile{} -> True
  GameExit      -> True
  GameSave      -> True
  Inventory     -> True
  Help          -> True
  _             -> False

-- | Minor commands land on the second page of command help.
minorHumanCmd :: HumanCmd -> Bool
minorHumanCmd cmd = case cmd of
--  CfgDump     -> True
  MemberCycle -> True
  MemberBack  -> True
  TgtFloor    -> True
  TgtEnemy    -> True
  TgtAscend{} -> True
  EpsIncr{}   -> True
  Cancel      -> True
  Accept      -> True
--  Clear       -> True
  History     -> True
  MarkVision  -> True
  MarkSmell   -> True
  MarkSuspect -> True
  _           -> False

-- | Commands that are forbidden on a remote level, because they
-- would usually take time when invoked on one.
-- Not that movement commands are not included, because they take time
-- on normal levels, but don't take time on remote levels, that is,
-- in targeting mode.
noRemoteHumanCmd :: HumanCmd -> Bool
noRemoteHumanCmd cmd = case cmd of
  Wait          -> True
  Pickup        -> True
  Drop          -> True
  Project{}     -> True
  Apply{}       -> True
  TriggerDir{}  -> True
  TriggerTile{} -> True
  _             -> False

-- | Description of player commands.
cmdDescription :: HumanCmd -> Text
cmdDescription cmd = case cmd of
  Move{}      -> "move"
  Run{}       -> "run"
  Wait        -> "wait"
  Pickup      -> "get an object"
  Drop        -> "drop an object"
  Project{..}     -> makePhrase [verb, MU.AW object]
  Apply{..}       -> makePhrase [verb, MU.AW object]
  TriggerDir{..}  -> makePhrase [verb, MU.AW object]
  TriggerTile{..} -> makePhrase [verb, MU.AW object]

  GameRestart t -> "new" <+> t <+> "game"
  GameExit    -> "save and exit"
  GameSave    -> "save game"
  CfgDump     -> "dump current configuration"

  SelectHero{} -> "select hero"
  MemberCycle -> "cycle among heroes on the level"
  MemberBack  -> "cycle among heroes in the dungeon"
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
  Cancel      -> "cancel action"
  Accept      -> "accept choice"
  Clear       -> "clear messages"
  History     -> "display previous messages"
  MarkVision  -> "mark visible area"
  MarkSmell   -> "mark smell"
  MarkSuspect -> "mark suspect terrain"
  Help        -> "display help"
