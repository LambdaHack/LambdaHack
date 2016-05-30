{-# LANGUAGE DeriveGeneric #-}
-- | Abstract syntax human player commands.
module Game.LambdaHack.Client.UI.HumanCmd
  ( CmdCategory(..), categoryDescription
  , CmdArea(..), areaDescription
  , CmdTriple, HumanCmd(..), noRemoteHumanCmd
  , Trigger(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.DeepSeq
import Data.Binary
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK

data CmdCategory =
    CmdMainMenu | CmdSettingsMenu | CmdItemMenu
  | CmdMove | CmdItem | CmdAim | CmdMeta | CmdMouse
  | CmdInternal | CmdDebug | CmdMinimal
  deriving (Show, Read, Eq, Generic)

instance NFData CmdCategory

instance Binary CmdCategory

categoryDescription :: CmdCategory -> Text
categoryDescription CmdMainMenu = "Main Menu"
categoryDescription CmdSettingsMenu = "Settings Menu"
categoryDescription CmdItemMenu = "Item Manipulation Menu"
categoryDescription CmdMove = "Terrain exploration and alteration"
categoryDescription CmdItem = "Item use"
categoryDescription CmdAim = "Aiming"
categoryDescription CmdMeta = "Assorted"
categoryDescription CmdMouse = "Mouse"
categoryDescription CmdInternal = "Internal"
categoryDescription CmdDebug = "Debug"
categoryDescription CmdMinimal = "The minimal command set"

-- The constructors are sorted, roughly, wrt inclusion, then top to bottom,
-- the left to right.
data CmdArea =
    CaMessage
  | CaMapLeader
  | CaMapParty
  | CaMap
  | CaArenaName
  | CaPercentSeen
  | CaXhairDesc
  | CaSelected
  | CaLeaderStatus
  | CaTargetDesc
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData CmdArea

instance Binary CmdArea

areaDescription :: CmdArea -> Text
areaDescription ca = case ca of
  CaMessage -> "message line"
  CaMapLeader -> "leader on the map"
  CaMapParty -> "party on the map"
  CaMap -> "the map area"
  CaArenaName -> "current arena name"
  CaPercentSeen -> "map percent seen"
  CaXhairDesc -> "xhair description"
  CaSelected -> "party roster"
  CaLeaderStatus -> "leader status"
  CaTargetDesc -> "target description"

type CmdTriple = ([CmdCategory], Text, HumanCmd)

-- | Abstract syntax of player commands.
data HumanCmd =
    -- Meta.
    ReplaceFail !Text !HumanCmd
  | Macro ![String]
  | ByArea ![(CmdArea, HumanCmd)]  -- if outside the areas, do nothing
  | ByAimMode {exploration :: !HumanCmd, aiming :: !HumanCmd}
  | ByItemMode {notChosen :: !HumanCmd, chosen :: !HumanCmd}
  | ComposeIfLocal !HumanCmd !HumanCmd
  | ComposeUnlessError !HumanCmd !HumanCmd
  | LoopOnNothing !HumanCmd
    -- Global.
    -- These usually take time.
  | Wait
  | MoveDir !Vector
  | RunDir !Vector
  | RunOnceAhead
  | MoveOnceToXhair
  | RunOnceToXhair
  | ContinueToXhair
  | MoveItem ![CStore] !CStore !(Maybe MU.Part) !MU.Part !Bool
  | Project     ![Trigger]
  | Apply       ![Trigger]
  | AlterDir    ![Trigger]
  | TriggerTile ![Trigger]
  | Help
  | ItemMenu
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
  | ChooseItemMenu !ItemDialogMode
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
  | MoveXhair !Vector !Int
  | AimTgt
  | AimFloor
  | AimEnemy
  | AimAscend !Int
  | EpsIncr !Bool
  | XhairUnknown
  | XhairItem
  | XhairStair !Bool
  | XhairPointerFloor
  | XhairPointerEnemy
  | AimPointerFloor
  | AimPointerEnemy
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData HumanCmd

instance Binary HumanCmd

-- | Commands that are forbidden on a remote level, because they
-- would usually take time when invoked on one.
-- Note that some commands that take time are not included,
-- because they don't take time in aiming mode.
noRemoteHumanCmd :: HumanCmd -> Bool
noRemoteHumanCmd cmd = case cmd of
  Wait          -> True
  MoveItem{}    -> True
  Apply{}       -> True
  AlterDir{}    -> True
  MoveOnceToXhair -> True
  RunOnceToXhair  -> True
  ContinueToXhair -> True
  _             -> False

data Trigger =
    ApplyItem {verb :: !MU.Part, object :: !MU.Part, symbol :: !Char}
  | AlterFeature {verb :: !MU.Part, object :: !MU.Part, feature :: !TK.Feature}
  | TriggerFeature
      {verb :: !MU.Part, object :: !MU.Part, feature :: !TK.Feature}
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData Trigger

instance Binary Trigger
