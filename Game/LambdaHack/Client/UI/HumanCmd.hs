-- | Abstract syntax human player commands.
module Game.LambdaHack.Client.UI.HumanCmd
  ( CmdCategory(..), HumanCmd(..), Trigger(..)
  , noRemoteHumanCmd, categoryDescription, cmdDescription
  ) where

import Control.Exception.Assert.Sugar
import Data.Maybe
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Actor (verbCStore)
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK

data CmdCategory =
    CmdMenu | CmdMove | CmdItem | CmdTgt | CmdAuto | CmdMeta | CmdMouse
  | CmdInternal | CmdDebug | CmdMinimal
  deriving (Show, Read, Eq)

categoryDescription :: CmdCategory -> Text
categoryDescription CmdMenu = "Main Menu"
categoryDescription CmdMove = "Terrain exploration and alteration"
categoryDescription CmdItem = "Item use"
categoryDescription CmdTgt = "Targeting"
categoryDescription CmdAuto = "Automation"
categoryDescription CmdMeta = "Assorted"
categoryDescription CmdMouse = "Mouse"
categoryDescription CmdInternal = "Internal"
categoryDescription CmdDebug = "Debug"
categoryDescription CmdMinimal = "Minimal cheat sheet for casual play"

-- | Abstract syntax of player commands.
data HumanCmd =
    -- Global.
    -- These usually take time.
    Move !Vector
  | Run !Vector
  | Wait
  | MoveItem ![CStore] !CStore !(Maybe MU.Part) !MU.Part !Bool
  | Project     ![Trigger]
  | Apply       ![Trigger]
  | AlterDir    ![Trigger]
  | TriggerTile ![Trigger]
  | RunOnceAhead
  | MoveOnceToCursor
  | RunOnceToCursor
  | ContinueToCursor
    -- Below this line, commands do not take time.
  | GameRestart !(GroupName ModeKind)
  | GameExit
  | GameSave
  | Tactic
  | Automate
    -- Local.
    -- Below this line, commands do not notify the server.
  | GameDifficultyCycle
  | PickLeader !Int
  | MemberCycle
  | MemberBack
  | DescribeItem !CStore
  | AllOwned
  | SelectActor
  | SelectNone
  | Clear
  | Repeat !Int
  | Record
  | History
  | MarkVision
  | MarkSmell
  | MarkSuspect
  | Help
  | MainMenu
  | Macro !Text ![String]
    -- These are mostly related to targeting.
  | MoveCursor !Vector !Int
  | TgtFloor
  | TgtEnemy
  | TgtAscend !Int
  | EpsIncr !Bool
  | TgtClear
  | CursorUnknown
  | CursorItem
  | CursorStair !Bool
  | Cancel
  | Accept
  | CursorPointerFloor
  | CursorPointerEnemy
  | TgtPointerFloor
  | TgtPointerEnemy
  deriving (Show, Read, Eq, Ord)

data Trigger =
    ApplyItem {verb :: !MU.Part, object :: !MU.Part, symbol :: !Char}
  | AlterFeature {verb :: !MU.Part, object :: !MU.Part, feature :: !TK.Feature}
  | TriggerFeature
      {verb :: !MU.Part, object :: !MU.Part, feature :: !TK.Feature}
  deriving (Show, Read, Eq, Ord)

-- | Commands that are forbidden on a remote level, because they
-- would usually take time when invoked on one.
-- Note that some commands that take time are not included,
-- because they don't take time in targeting mode.
noRemoteHumanCmd :: HumanCmd -> Bool
noRemoteHumanCmd cmd = case cmd of
  Wait          -> True
  MoveItem{}    -> True
  Apply{}       -> True
  AlterDir{}    -> True
  MoveOnceToCursor -> True
  RunOnceToCursor  -> True
  ContinueToCursor -> True
  _             -> False

-- | Description of player commands.
cmdDescription :: HumanCmd -> Text
cmdDescription cmd = case cmd of
  Move v      -> "move" <+> compassText v
  Run v       -> "run" <+> compassText v
  Wait        -> "wait"
  MoveItem _ store2 mverb object _ ->
    let verb = fromMaybe (MU.Text $ verbCStore store2) mverb
    in makePhrase [verb, object]
  Project ts  -> triggerDescription ts
  Apply ts    -> triggerDescription ts
  AlterDir ts -> triggerDescription ts
  TriggerTile ts -> triggerDescription ts
  RunOnceAhead -> "run once ahead"
  MoveOnceToCursor -> "move one step towards the cursor"
  RunOnceToCursor -> "run one step towards the cursor"
  ContinueToCursor -> "continue towards the cursor"

  GameRestart t ->
    -- TODO: use mname for the game mode instead of t
    makePhrase ["new", MU.Capitalize $ MU.Text $ tshow t, "game"]
  GameExit    -> "save and exit"
  GameSave    -> "save game"
  Tactic      -> "cycle tactic of non-leader team members (WIP)"
  Automate    -> "automate faction (ESC to retake control)"

  GameDifficultyCycle -> "cycle difficulty of the next game"
  PickLeader{} -> "pick leader"
  MemberCycle -> "cycle among party members on the level"
  MemberBack  -> "cycle among all party members"
  DescribeItem CGround -> "describe items on the ground"
  DescribeItem COrgan -> "describe organs"
  DescribeItem CEqp -> "describe equipment of the leader"
  DescribeItem CInv -> "describe backpack inventory of the leader"
  DescribeItem CSha -> "describe the shared party stash"
  AllOwned    -> "describe all owned items"
  SelectActor -> "select (or deselect) a party member"
  SelectNone  -> "deselect (or select) all on the level"
  Clear       -> "clear messages"
  Repeat 1    -> "voice again the recorded commands"
  Repeat n    -> "voice the recorded commands" <+> tshow n <+> "times"
  Record      -> "start recording commands"
  History     -> "display player diary"
  MarkVision  -> "mark visible zone"
  MarkSmell   -> "mark smell clues"
  MarkSuspect -> "mark suspect terrain"
  Help        -> "display help"
  MainMenu    -> "display the Main Menu"
  Macro t _   -> t

  MoveCursor v 1 -> "move cursor" <+> compassText v
  MoveCursor v k ->
    "move cursor up to" <+> tshow k <+> "steps" <+> compassText v
  TgtFloor -> "cycle targeting mode"
  TgtEnemy -> "target enemy"
  TgtAscend k | k == 1  -> "target next shallower level"
  TgtAscend k | k >= 2  -> "target" <+> tshow k    <+> "levels shallower"
  TgtAscend k | k == -1 -> "target next deeper level"
  TgtAscend k | k <= -2 -> "target" <+> tshow (-k) <+> "levels deeper"
  TgtAscend _ -> assert `failure` "void level change when targeting"
                        `twith` cmd
  EpsIncr True   -> "swerve targeting line"
  EpsIncr False  -> "unswerve targeting line"
  TgtClear       -> "clear target/cursor"
  CursorUnknown  -> "set cursor to the closest unknown spot"
  CursorItem     -> "set cursor to the closest item"
  CursorStair up -> "set cursor to the closest stairs" <+> if up then "up" else "down"
  Cancel -> "cancel action, open Main Menu"
  Accept -> "accept choice"
  CursorPointerFloor -> "set cursor to floor under pointer"
  CursorPointerEnemy -> "set cursor to enemy under pointer"
  TgtPointerFloor -> "target floor under pointer"
  TgtPointerEnemy -> "target enemy under pointer"

triggerDescription :: [Trigger] -> Text
triggerDescription [] = "trigger a thing"
triggerDescription (t : _) = makePhrase [verb t, object t]
