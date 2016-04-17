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
    Macro !Text ![String]
  | Alias !Text !HumanCmd
  | ByArea !Text ![(CmdArea, HumanCmd)]  -- if outside the areas, do nothing
  | ByMode !Text !HumanCmd !HumanCmd
  | Sequence !Text !Text ![HumanCmd]

    -- Global.
    -- These usually take time.
  | Move !Vector
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
  | ChooseItem !ItemDialogMode
  | GameDifficultyIncr
  | PickLeader !Int
  | PickLeaderWithPointer
  | MemberCycle
  | MemberBack
  | SelectActor
  | SelectNone
  | Clear
  | SelectWithPointer
  | Repeat !Int
  | Record
  | History
  | MarkVision
  | MarkSmell
  | MarkSuspect
  | Help
  | MainMenu
  | SettingsMenu
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
  Macro t _   -> t
  Alias t _   -> t
  ByArea t _  -> t
  ByMode t _ _ -> t
  Sequence t _ _ -> t

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
  MoveOnceToCursor -> "move one step towards the crosshair"
  RunOnceToCursor -> "run selected one step towards the crosshair"
  ContinueToCursor -> "continue towards the crosshair"

  GameRestart t ->
    -- TODO: use mname for the game mode instead of t
    makePhrase ["new", MU.Capitalize $ MU.Text $ tshow t, "game"]
  GameExit    -> "save and exit"
  GameSave    -> "save game"
  Tactic      -> "cycle henchmen tactic"
  Automate    -> "automate faction"
  GameDifficultyIncr -> "cycle next difficulty"

  ChooseItem (MStore CGround) -> "manage items on the ground"
  ChooseItem (MStore COrgan) -> "describe organs of the leader"
  ChooseItem (MStore CEqp) -> "manage equipment of the leader"
  ChooseItem (MStore CInv) -> "manage inventory pack of the leader"
  ChooseItem (MStore CSha) -> "manage the shared party stash"
  ChooseItem MOwned -> "describe all owned items"
  ChooseItem MStats -> "show the stats summary of the leader"
  PickLeader{} -> "pick leader"
  PickLeaderWithPointer -> "pick leader with mouse pointer"
  MemberCycle -> "cycle among party members on the level"
  MemberBack  -> "cycle among all party members"
  SelectActor -> "select (or deselect) a party member"
  SelectNone  -> "deselect (or select) all on the level"
  Clear       -> "clear messages"
  SelectWithPointer -> "select actors with mouse pointer"
  Repeat 1    -> "voice again the recorded commands"
  Repeat n    -> "voice the recorded commands" <+> tshow n <+> "times"
  Record      -> "start recording commands"
  History     -> "display player diary"
  MarkVision  -> "toggle visible zone"
  MarkSmell   -> "toggle smell clues"
  MarkSuspect -> "toggle suspect terrain"
  Help        -> "display help"
  MainMenu    -> "display the Main Menu"
  SettingsMenu -> "display the Settings Menu"

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
  TgtClear       -> "reset target/crosshair"
  CursorUnknown  -> "set crosshair to the closest unknown spot"
  CursorItem     -> "set crosshair to the closest item"
  CursorStair up -> "set crosshair to the closest stairs"
                    <+> if up then "up" else "down"
  Cancel -> "cancel target/action"
  Accept -> "accept target/choice"
  CursorPointerFloor -> "set crosshair to floor under pointer"
  CursorPointerEnemy -> "set crosshair to enemy under pointer"
  TgtPointerFloor -> "enter aiming mode and describe a tile"
  TgtPointerEnemy -> "enter aiming mode and describe an enemy"

triggerDescription :: [Trigger] -> Text
triggerDescription [] = "trigger a thing"
triggerDescription (t : _) = makePhrase [verb t, object t]
