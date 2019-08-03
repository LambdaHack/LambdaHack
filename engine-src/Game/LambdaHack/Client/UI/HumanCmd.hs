{-# LANGUAGE DeriveGeneric #-}
-- | Abstract syntax of human player commands.
module Game.LambdaHack.Client.UI.HumanCmd
  ( CmdCategory(..), categoryDescription
  , CmdArea(..), areaDescription
  , CmdTriple, AimModeCmd(..), HumanCmd(..)
  , TriggerItem(..), TriggerTile(..)
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.DeepSeq
import           Data.Binary
import           GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.TileKind as TK
import           Game.LambdaHack.Definition.Defs

data CmdCategory =
    CmdMainMenu | CmdDashboard | CmdItemMenu
  | CmdMove | CmdItem | CmdAim | CmdMeta | CmdMouse
  | CmdInternal | CmdNoHelp | CmdDebug | CmdMinimal
  deriving (Show, Read, Eq, Generic)

instance NFData CmdCategory

instance Binary CmdCategory

categoryDescription :: CmdCategory -> Text
categoryDescription CmdMainMenu = "Main menu"
categoryDescription CmdDashboard = "Dashboard"
categoryDescription CmdItemMenu = "Item menu commands"
categoryDescription CmdMove = "Terrain exploration and modification commands"
categoryDescription CmdItem = "Complete item-related commands"
categoryDescription CmdAim = "Complete aiming commands"
categoryDescription CmdMeta = "Assorted commands"
categoryDescription CmdMouse = "Mouse"
categoryDescription CmdInternal = "Internal"
categoryDescription CmdNoHelp = "Ignored in help"
categoryDescription CmdDebug = "Debug"
categoryDescription CmdMinimal = "The minimal command set"

-- The constructors are sorted, roughly, wrt inclusion, then top to bottom,
-- the left to right.
-- | Symbolic representation of areas of the screen used to define the meaning
-- of mouse button presses relative to where the mouse points to.
data CmdArea =
    CaMessage
  | CaMapLeader
  | CaMapParty
  | CaMap
  | CaLevelNumber
  | CaArenaName
  | CaPercentSeen
  | CaXhairDesc
  | CaSelected
  | CaCalmGauge
  | CaCalmValue
  | CaHPGauge
  | CaHPValue
  | CaLeaderDesc
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData CmdArea

instance Binary CmdArea

areaDescription :: CmdArea -> Text
areaDescription ca = case ca of
  CaMessage ->      "message line"
  CaMapLeader ->    "pointman spot"
  CaMapParty ->     "party on map"
  CaMap ->          "the map area"
  CaLevelNumber ->  "level number"
  CaArenaName ->    "level caption"
  CaPercentSeen ->  "percent seen"
  CaXhairDesc ->    "crosshair info"
  CaSelected ->     "party roster"
  CaCalmGauge ->    "Calm gauge"
  CaCalmValue ->    "Calm value"
  CaHPGauge ->      "HP gauge"
  CaHPValue ->      "HP Value"
  CaLeaderDesc ->   "pointman info"
  --                 1234567890123

-- | This triple of command categories, description and the command term itself
-- defines the meaning of a human command as entered via a keypress,
-- mouse click or chosen from a menu.
type CmdTriple = ([CmdCategory], Text, HumanCmd)

data AimModeCmd = AimModeCmd {exploration :: HumanCmd, aiming :: HumanCmd}
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData AimModeCmd

instance Binary AimModeCmd

-- | Abstract syntax of human player commands.
data HumanCmd =
    -- Meta.
    Macro [String]
  | ByArea [(CmdArea, HumanCmd)]  -- if outside the areas, do nothing
  | ByAimMode AimModeCmd
  | ComposeIfLocal HumanCmd HumanCmd
  | ComposeUnlessError HumanCmd HumanCmd
  | Compose2ndLocal HumanCmd HumanCmd
  | LoopOnNothing HumanCmd
  | ExecuteIfClear HumanCmd
    -- Global.
    -- These usually take time.
  | Wait
  | Wait10
  | Yell
  | MoveDir Vector
  | RunDir Vector
  | RunOnceAhead
  | MoveOnceToXhair
  | RunOnceToXhair
  | ContinueToXhair
  | MoveItem [CStore] CStore (Maybe MU.Part) Bool
  | Project
  | Apply
  | AlterDir [TriggerTile]
  | AlterWithPointer [TriggerTile]
  | CloseDir
  | Help
  | Hint
  | ItemMenu
  | MainMenu
  | MainMenuAutoOn
  | MainMenuAutoOff
  | Dashboard
    -- Below this line, commands do not take time.
  | GameDifficultyIncr
  | GameWolfToggle
  | GameFishToggle
  | GameScenarioIncr
  | GameRestart
  | GameQuit
  | GameDrop
  | GameExit
  | GameSave
  | Doctrine
  | Automate
  | AutomateToggle
  | AutomateBack
    -- Local. Below this line, commands do not notify the server.
  | ChooseItem ItemDialogMode
  | ChooseItemMenu ItemDialogMode
  | ChooseItemProject [TriggerItem]
  | ChooseItemApply [TriggerItem]
  | PickLeader Int
  | PickLeaderWithPointer
  | MemberCycle
  | MemberBack
  | SelectActor
  | SelectNone
  | SelectWithPointer
  | Repeat Int
  | Record
  | AllHistory
  | LastHistory
  | MarkVision
  | MarkSmell
  | MarkSuspect
  | MarkAnim
  | SettingsMenu
  | ChallengesMenu
  | PrintScreen
    -- These are mostly related to aiming.
  | Cancel
  | Accept
  | ClearTargetIfItemClear
  | ItemClear
  | MoveXhair Vector Int
  | AimTgt
  | AimFloor
  | AimEnemy
  | AimItem
  | AimAscend Int
  | EpsIncr Bool
  | XhairUnknown
  | XhairItem
  | XhairStair Bool
  | XhairPointerFloor
  | XhairPointerEnemy
  | AimPointerFloor
  | AimPointerEnemy
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData HumanCmd

instance Binary HumanCmd

-- | Description of how item manipulation is triggered and communicated
-- to the player.
data TriggerItem =
  TriggerItem {tiverb :: MU.Part, tiobject :: MU.Part, tisymbols :: [Char]}
  deriving (Show, Eq, Ord, Generic)

instance Read TriggerItem where
  readsPrec = error $ "parsing of TriggerItem not implemented" `showFailure` ()

instance NFData TriggerItem

instance Binary TriggerItem

-- | Description of how tile altering is triggered and communicated
-- to the player.
data TriggerTile =
  TriggerTile {ttverb :: MU.Part, ttobject :: MU.Part, ttfeature :: TK.Feature}
  deriving (Show, Eq, Ord, Generic)

instance Read TriggerTile where
  readsPrec = error $ "parsing of TriggerTile not implemented" `showFailure` ()

instance NFData TriggerTile

instance Binary TriggerTile
