{-# LANGUAGE DeriveGeneric #-}
-- | Abstract syntax human player commands.
module Game.LambdaHack.Client.UI.HumanCmd
  ( CmdTriple, CmdCategory(..), CmdArea(..), HumanCmd(..), Trigger(..)
  , noRemoteHumanCmd, categoryDescription
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK

type CmdTriple = ([CmdCategory], Text, HumanCmd)

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
  | ChooseItemProject ![Trigger]
  | ChooseItemApply ![Trigger]
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
  | TgtTgt
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
