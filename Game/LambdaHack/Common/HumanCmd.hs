-- | Abstract syntax human player commands.
module Game.LambdaHack.Common.HumanCmd
  ( CmdCategory(..), HumanCmd(..), Trigger(..)
  , noRemoteHumanCmd, categoryDescription, cmdDescription
  ) where

import Control.Exception.Assert.Sugar
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Vector

data CmdCategory =
    CmdMenu | CmdMove | CmdItem | CmdTgt | CmdAuto | CmdMeta | CmdDebug
  deriving (Show, Read, Eq)

categoryDescription :: CmdCategory -> Text
categoryDescription CmdMenu = "Main Menu"
categoryDescription CmdMove = "Movement and terrain alteration"
categoryDescription CmdItem = "Item use"
categoryDescription CmdTgt = "Targeting"
categoryDescription CmdAuto = "Automation"
categoryDescription CmdMeta = "Assorted"
categoryDescription CmdDebug = "Debug"

-- | Abstract syntax of player commands.
data HumanCmd =
    -- These usually take time.
    Move !Vector
  | Run !Vector
  | Wait
  | MoveItem ![CStore] !CStore !Text !Text !Bool
  | Project     ![Trigger]
  | Apply       ![Trigger]
  | AlterDir    ![Trigger]
  | TriggerTile ![Trigger]
  | StepToTarget
  | Resend
    -- Below this line, commands do not take time.
  | GameRestart !Text
  | GameExit
  | GameSave
  | Automate
    -- Below this line, commands do not notify the server.
  | GameDifficultyCycle
  | PickLeader !Int
  | MemberCycle
  | MemberBack
  | Inventory
  | Equipment
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
  | TgtUnknown
  | TgtItem
  | TgtStair !Bool
  | TgtAscend !Int
  | EpsIncr !Bool
  | TgtClear
  | Cancel
  | Accept
  deriving (Show, Read, Eq, Ord)

data Trigger =
    ApplyItem {verb :: !MU.Part, object :: !MU.Part, symbol :: !Char}
  | AlterFeature {verb :: !MU.Part, object :: !MU.Part, feature :: !F.Feature}
  | TriggerFeature {verb :: !MU.Part, object :: !MU.Part, feature :: !F.Feature}
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
  StepToTarget  -> True
  Resend        -> True
  _             -> False

-- | Description of player commands.
cmdDescription :: HumanCmd -> Text
cmdDescription cmd = case cmd of
  Move v      -> "move" <+> compassText v
  Run v       -> "run" <+> compassText v
  Wait        -> "wait"
  MoveItem _ _ verb object _ -> verb <+> object
  Project ts  -> triggerDescription ts
  Apply ts    -> triggerDescription ts
  AlterDir ts -> triggerDescription ts
  TriggerTile ts -> triggerDescription ts
  StepToTarget -> "make one step towards the target"
  Resend      -> "resend last server command"

  GameRestart t -> "new" <+> t <+> "game"
  GameExit    -> "save and exit"
  GameSave    -> "save game"
  Automate    -> "automate faction (ESC to retake control)"

  GameDifficultyCycle -> "cycle difficulty of the next game"
  PickLeader{} -> "pick leader"
  MemberCycle -> "cycle among party members on the level"
  MemberBack  -> "cycle among party members in the dungeon"
  Inventory   -> "display shared inventory"
  Equipment   -> "display personal equipment"
  AllOwned    -> "display all owned items"
  SelectActor -> "select (or deselect) a party member"
  SelectNone  -> "deselect (or select) all on the level"
  Clear       -> "clear messages"
  Repeat 1    -> "play back last keys"
  Repeat n    -> "play back last keys" <+> tshow n <+> "times"
  Record      -> "start recording a macro"
  History     -> "display player diary"
  MarkVision  -> "mark visible area"
  MarkSmell   -> "mark smell"
  MarkSuspect -> "mark suspect terrain"
  Help        -> "display help"
  MainMenu    -> "display the Main Menu"
  Macro t _   -> t

  MoveCursor v 1 -> "move cursor" <+> compassText v
  MoveCursor v k ->
    "move cursor up to" <+> tshow k <+> "steps" <+> compassText v
  TgtFloor    -> "cycle targeting mode"
  TgtEnemy    -> "target enemy"
  TgtUnknown  -> "target the closest unknown spot"
  TgtItem     -> "target the closest item"
  TgtStair up -> "target the closest stairs" <+> if up then "up" else "down"
  TgtAscend k | k == 1  -> "target next shallower level"
  TgtAscend k | k >= 2  -> "target" <+> tshow k    <+> "levels shallower"
  TgtAscend k | k == -1 -> "target next deeper level"
  TgtAscend k | k <= -2 -> "target" <+> tshow (-k) <+> "levels deeper"
  TgtAscend _ -> assert `failure` "void level change when targeting"
                        `twith` cmd
  EpsIncr True  -> "swerve targeting line"
  EpsIncr False -> "unswerve targeting line"
  TgtClear    -> "clear target/cursor"
  Cancel      -> "cancel action"
  Accept      -> "accept choice"

triggerDescription :: [Trigger] -> Text
triggerDescription [] = "trigger a thing"
triggerDescription (t : _) = makePhrase [verb t, object t]
