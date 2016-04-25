{-# LANGUAGE DeriveGeneric #-}
-- | Abstract syntax human player commands.
module Game.LambdaHack.Client.UI.HumanCmd
  ( CmdCategory(..), CmdArea(..), HumanCmd(..), Trigger(..)
  , noRemoteHumanCmd, categoryDescription, cmdDescription
  ) where

import Control.DeepSeq
import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Actor (verbCStore)
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK

data CmdCategory =
    CmdMainMenu | CmdSettingsMenu
  | CmdMove | CmdItem | CmdTgt | CmdMeta | CmdMouse
  | CmdInternal | CmdDebug | CmdMinimal
  deriving (Show, Read, Eq, Generic)

instance NFData CmdCategory

instance Binary CmdCategory

categoryDescription :: CmdCategory -> Text
categoryDescription CmdMainMenu = "Main Menu"
categoryDescription CmdSettingsMenu = "Settings Menu"
categoryDescription CmdMove = "Terrain exploration and alteration"
categoryDescription CmdItem = "Item use"
categoryDescription CmdTgt = "Aiming and targeting"
categoryDescription CmdMeta = "Assorted"
categoryDescription CmdMouse = "Mouse"
categoryDescription CmdInternal = "Internal"
categoryDescription CmdDebug = "Debug"
categoryDescription CmdMinimal = "The minimal command set"

data CmdArea =
    CaMessage
  | CaMapLeader
  | CaMapParty
  | CaMap
  | CaArenaName
  | CaXhairDesc
  | CaSelected
  | CaLeaderStatus
  | CaTargetDesc
  | CaRectangle !(X, Y, X, Y)
  | CaUnion !CmdArea !CmdArea
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData CmdArea

instance Binary CmdArea

-- | Abstract syntax of player commands.
data HumanCmd =
    -- Meta.
    ReplaceFail !Text !HumanCmd
  | Alias !Text !HumanCmd
  | Macro ![String]
  | ByArea ![(CmdArea, HumanCmd)]  -- if outside the areas, do nothing
  | ByAimMode {notAiming :: !HumanCmd, aiming :: !HumanCmd}
  | ByItemMode {notChosen :: !HumanCmd, chosen :: !HumanCmd}
  | ComposeIfLeft !HumanCmd !HumanCmd
  | ComposeIfEmpty !HumanCmd !HumanCmd
    -- Global.
    -- These usually take time.
  | Wait
  | Move !Vector
  | Run !Vector
  | RunOnceAhead
  | MoveOnceToCursor
  | RunOnceToCursor
  | ContinueToCursor
  | MoveItem ![CStore] !CStore !(Maybe MU.Part) !MU.Part !Bool
  | Project     ![Trigger]
  | Apply       ![Trigger]
  | AlterDir    ![Trigger]
  | TriggerTile ![Trigger]
  | Help !(Maybe Text)
  | MainMenu
  | GameDifficultyIncr
    -- Below this line, commands do not take time.
  | GameRestart !(GroupName ModeKind)
  | GameExit
  | GameSave
  | Tactic
  | Automate
    -- Local. Below this line, commands do not notify the server.
  | Clear
  | ChooseItem !ItemDialogMode
  | PickLeader !Int
  | PickLeaderWithPointer
  | MemberCycle
  | MemberBack
  | SelectActor
  | SelectNone
  | SelectWithPointer
  | Repeat !Int
  | Record
  | History
  | MarkVision
  | MarkSmell
  | MarkSuspect
  | SettingsMenu
    -- These are mostly related to aiming.
  | Cancel
  | Accept
  | TgtClear
  | MoveCursor !Vector !Int
  | TgtFloor
  | TgtEnemy
  | TgtAscend !Int
  | EpsIncr !Bool
  | CursorUnknown
  | CursorItem
  | CursorStair !Bool
  | CursorPointerFloor
  | CursorPointerEnemy
  | TgtPointerFloor
  | TgtPointerEnemy
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData HumanCmd

instance Binary HumanCmd

data Trigger =
    ApplyItem {verb :: !MU.Part, object :: !MU.Part, symbol :: !Char}
  | AlterFeature {verb :: !MU.Part, object :: !MU.Part, feature :: !TK.Feature}
  | TriggerFeature
      {verb :: !MU.Part, object :: !MU.Part, feature :: !TK.Feature}
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData Trigger

instance Binary Trigger

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
  ReplaceFail _ cmd1 -> cmdDescription cmd1
  Alias t _ -> t
  Macro{} -> ""
  ByArea{} -> ""
  ByAimMode {notAiming} -> cmdDescription notAiming
  ByItemMode {notChosen} -> cmdDescription notChosen
  ComposeIfLeft cmd1 _ -> cmdDescription cmd1
  ComposeIfEmpty cmd1 _ -> cmdDescription cmd1

  Wait        -> "wait"
  Move v      -> "move" <+> compassText v
  Run v       -> "run" <+> compassText v
  RunOnceAhead -> "run once ahead"
  MoveOnceToCursor -> "move one step towards the crosshair"
  RunOnceToCursor -> "run selected one step towards the crosshair"
  ContinueToCursor -> "continue towards the crosshair"
  MoveItem _ store2 mverb object _ ->
    let verb = fromMaybe (MU.Text $ verbCStore store2) mverb
    in makePhrase [verb, object]
  Project ts  -> triggerDescription ts
  Apply ts    -> triggerDescription ts
  AlterDir ts -> triggerDescription ts
  TriggerTile ts -> triggerDescription ts
  Help mstart ->
    let about = maybe "" (\t -> if T.null t then "" else "about" <+> t) mstart
    in "display help" <+> about
  MainMenu    -> "display the Main Menu"
  GameDifficultyIncr -> "cycle next difficulty"

  GameRestart t ->
    -- TODO: use mname for the game mode instead of t
    makePhrase ["new", MU.Capitalize $ MU.Text $ tshow t, "game"]
  GameExit    -> "save and exit"
  GameSave    -> "save game"
  Tactic      -> "cycle henchmen tactic"
  Automate    -> "automate faction"

  Clear       -> "clear messages"
  ChooseItem (MStore CGround) -> "manage items on the ground"
  ChooseItem (MStore COrgan) -> "describe organs of the leader"
  ChooseItem (MStore CEqp) -> "manage equipment of the leader"
  ChooseItem (MStore CInv) -> "manage inventory pack of the leader"
  ChooseItem (MStore CSha) -> "manage the shared party stash"
  ChooseItem MOwned -> "manage all owned items"
  ChooseItem MStats -> "show the stats summary of the leader"
  PickLeader{} -> "pick leader"
  PickLeaderWithPointer -> "pick leader with mouse pointer"
  MemberCycle -> "cycle among party members on the level"
  MemberBack  -> "cycle among all party members"
  SelectActor -> "select (or deselect) a party member"
  SelectNone  -> "deselect (or select) all on the level"
  SelectWithPointer -> "select actors with mouse pointer"
  Repeat 1    -> "voice again the recorded commands"
  Repeat n    -> "voice the recorded commands" <+> tshow n <+> "times"
  Record      -> "start recording commands"
  History     -> "display player diary"
  MarkVision  -> "toggle visible zone"
  MarkSmell   -> "toggle smell clues"
  MarkSuspect -> "toggle suspect terrain"
  SettingsMenu -> "display the Settings Menu"

  Cancel -> "cancel target/action"
  Accept -> "accept target/choice"
  TgtClear       -> "reset target/crosshair"
  MoveCursor v 1 -> "move crosshair" <+> compassText v
  MoveCursor v k ->
    "move crosshair up to" <+> tshow k <+> "steps" <+> compassText v
  TgtFloor -> "cycle aiming styles"
  TgtEnemy -> "aim at an enemy"
  TgtAscend k | k == 1  -> "aim at next shallower level"
  TgtAscend k | k >= 2  -> "aim at" <+> tshow k    <+> "levels shallower"
  TgtAscend k | k == -1 -> "aim at next deeper level"
  TgtAscend k | k <= -2 -> "aim at" <+> tshow (-k) <+> "levels deeper"
  TgtAscend _ -> assert `failure` "void level change when aiming"
                        `twith` cmd
  EpsIncr True   -> "swerve the aiming line"
  EpsIncr False  -> "unswerve the aiming line"
  CursorUnknown  -> "set crosshair to the closest unknown spot"
  CursorItem     -> "set crosshair to the closest item"
  CursorStair up -> "set crosshair to the closest stairs"
                    <+> if up then "up" else "down"
  CursorPointerFloor -> "set crosshair to floor under pointer"
  CursorPointerEnemy -> "set crosshair to enemy under pointer"
  TgtPointerFloor -> "enter aiming mode and describe a tile"
  TgtPointerEnemy -> "enter aiming mode and describe an enemy"

triggerDescription :: [Trigger] -> Text
triggerDescription [] = "trigger a thing"
triggerDescription (t : _) = makePhrase [verb t, object t]
