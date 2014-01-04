-- | Abstract syntax human player commands.
module Game.LambdaHack.Client.HumanCmd
  ( HumanCmd(..), Trigger(..)
  , majorHumanCmd, minorHumanCmd, noRemoteHumanCmd, cmdDescription
  ) where

import Control.Exception.Assert.Sugar
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Vector

-- | Abstract syntax of player commands.
data HumanCmd =
    -- These usually take time.
    Move !Vector
  | Run !Vector
  | Wait
  | Pickup
  | Drop
  | Project     ![Trigger]
  | Apply       ![Trigger]
  | AlterDir    ![Trigger]
  | TriggerTile ![Trigger]
  | Repeat !Int
    -- These do not take time.
  | GameRestart !Text
  | GameExit
  | GameSave
  | GameDifficultyCycle
  -- These do not notify the server.
  | PickLeader !Int
  | MemberCycle
  | MemberBack
  | Inventory
  | TgtFloor
  | TgtEnemy
  | TgtAscend !Int
  | EpsIncr !Bool
  | SelectActor
  | SelectNone
  | Cancel
  | Accept
  | Clear
  | History
  | MarkVision
  | MarkSmell
  | MarkSuspect
  | Help
  deriving (Eq, Ord, Show, Read)

data Trigger =
    ApplyItem {verb :: !MU.Part, object :: !MU.Part, symbol :: !Char}
  | AlterFeature {verb :: !MU.Part, object :: !MU.Part, feature :: !F.Feature}
  | TriggerFeature {verb :: !MU.Part, object :: !MU.Part, feature :: !F.Feature}
  deriving (Show, Read, Eq, Ord)

-- | Major commands land on the first page of command help.
majorHumanCmd :: HumanCmd -> Bool
majorHumanCmd cmd = case cmd of
  Pickup        -> True
  Drop          -> True
  Project{}     -> True
  Apply{}       -> True
  AlterDir{}    -> True
  TriggerTile{} -> True
  Inventory     -> True
  Help          -> True
  Cancel        -> True
  Accept        -> True
  _             -> False

-- | Minor commands land on the second page of command help.
minorHumanCmd :: HumanCmd -> Bool
minorHumanCmd cmd = case cmd of
  Repeat{}    -> True
  MemberCycle -> True
  MemberBack  -> True
  TgtFloor    -> True
  TgtEnemy    -> True
  TgtAscend{} -> True
  EpsIncr{}   -> True
  SelectActor -> True
  SelectNone  -> True
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
  AlterDir{}    -> True
  TriggerTile{} -> True
  Repeat{}      -> True
  _             -> False

-- | Description of player commands.
cmdDescription :: HumanCmd -> Text
cmdDescription cmd = case cmd of
  Move{}      -> "move"
  Run{}       -> "run"
  Wait        -> "wait"
  Pickup      -> "get an object"
  Drop        -> "drop an object"
  Project ts     -> triggerDescription ts
  Apply ts       -> triggerDescription ts
  AlterDir ts -> triggerDescription ts
  TriggerTile ts -> triggerDescription ts
  Repeat n    -> "play back last command" <+> showT n <+> "times"

  GameRestart t -> "new" <+> t <+> "game"
  GameExit    -> "save and exit"
  GameSave    -> "save game"
  GameDifficultyCycle -> "cycle next game difficulty"

  PickLeader{} -> "pick leader"
  MemberCycle -> "cycle among party members on the level"
  MemberBack  -> "cycle among party members in the dungeon"
  Inventory   -> "display inventory"
  TgtFloor    -> "target position"
  TgtEnemy    -> "target monster"
  TgtAscend k | k == 1  -> "target next shallower level"
  TgtAscend k | k >= 2  -> "target" <+> showT k    <+> "levels shallower"
  TgtAscend k | k == -1 -> "target next deeper level"
  TgtAscend k | k <= -2 -> "target" <+> showT (-k) <+> "levels deeper"
  TgtAscend _ ->
    assert `failure` "void level change when targeting" `twith` cmd
  EpsIncr True  -> "swerve targeting line"
  EpsIncr False -> "unswerve targeting line"
  SelectActor -> "select (or deselect) a party member"
  SelectNone  -> "deselect (or select) all on the level"
  Cancel      -> "cancel action"
  Accept      -> "accept choice"
  Clear       -> "clear messages"
  History     -> "display player diary"
  MarkVision  -> "mark visible area"
  MarkSmell   -> "mark smell"
  MarkSuspect -> "mark suspect terrain"
  Help        -> "display help"

triggerDescription :: [Trigger] -> Text
triggerDescription [] = "trigger a thing"
triggerDescription (t : _) = makePhrase [verb t, MU.AW $ object t]
