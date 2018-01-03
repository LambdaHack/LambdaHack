{-# LANGUAGE DeriveGeneric #-}
-- | Abstract syntax of human player commands.
module Game.LambdaHack.Client.UI.HumanCmd
  ( CmdCategory(..), categoryDescription
  , CmdArea(..), areaDescription
  , CmdTriple, HumanCmd(..)
  , Trigger(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.DeepSeq
import           Data.Binary
import           GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.TileKind as TK

data CmdCategory =
    CmdMainMenu | CmdDashboard | CmdItemMenu
  | CmdMove | CmdItem | CmdAim | CmdMeta | CmdMouse
  | CmdInternal | CmdNoHelp | CmdDebug | CmdMinimal
  deriving (Show, Read, Eq, Generic)

instance NFData CmdCategory

instance Binary CmdCategory

categoryDescription :: CmdCategory -> Text
categoryDescription CmdMainMenu = "Main Menu"
categoryDescription CmdDashboard = "Dashboard"
categoryDescription CmdItemMenu = "Item Menu commands"
categoryDescription CmdMove = "Terrain exploration and alteration"
categoryDescription CmdItem = "Remaining item-related commands"
categoryDescription CmdAim = "Aiming"
categoryDescription CmdMeta = "Assorted"
categoryDescription CmdMouse = "Mouse"
categoryDescription CmdInternal = "Internal"
categoryDescription CmdNoHelp = "Ignored in Help"
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
  | CaHPGauge
  | CaTargetDesc
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData CmdArea

instance Binary CmdArea

areaDescription :: CmdArea -> Text
areaDescription ca = case ca of
  CaMessage ->      "message line"
  CaMapLeader ->    "leader on map"
  CaMapParty ->     "party on map"
  CaMap ->          "the map area"
  CaLevelNumber ->  "level number"
  CaArenaName ->    "level caption"
  CaPercentSeen ->  "percent seen"
  CaXhairDesc ->    "x-hair info"
  CaSelected ->     "party roster"
  CaCalmGauge ->    "Calm gauge"
  CaHPGauge ->      "HP gauge"
  CaTargetDesc ->   "target info"
  --                 1234567890123

-- | This triple of command categories, description and the command term itself
-- defines the meaning of a human command as entered via a keypress,
-- mouse click or chosen from a menu.
type CmdTriple = ([CmdCategory], Text, HumanCmd)

-- | Abstract syntax of human player commands.
data HumanCmd =
    -- Meta.
    Macro [String]
  | ByArea [(CmdArea, HumanCmd)]  -- if outside the areas, do nothing
  | ByAimMode {exploration :: HumanCmd, aiming :: HumanCmd}
  | ByItemMode {ts :: [Trigger], notChosen :: HumanCmd, chosen :: HumanCmd}
  | ComposeIfLocal HumanCmd HumanCmd
  | ComposeUnlessError HumanCmd HumanCmd
  | Compose2ndLocal HumanCmd HumanCmd
  | LoopOnNothing HumanCmd
  | ExecuteIfClear HumanCmd
    -- Global.
    -- These usually take time.
  | Wait
  | Wait10
  | MoveDir Vector
  | RunDir Vector
  | RunOnceAhead
  | MoveOnceToXhair
  | RunOnceToXhair
  | ContinueToXhair
  | MoveItem [CStore] CStore (Maybe MU.Part) Bool
  | Project [Trigger]
  | Apply [Trigger]
  | AlterDir [Trigger]
  | AlterWithPointer [Trigger]
  | Help
  | ItemMenu
  | MainMenu
  | Dashboard
    -- Below this line, commands do not take time.
  | GameDifficultyIncr
  | GameWolfToggle
  | GameFishToggle
  | GameScenarioIncr
  | GameRestart
  | GameExit
  | GameSave
  | Tactic
  | Automate
    -- Local. Below this line, commands do not notify the server.
  | Clear
  | SortSlots
  | ChooseItem ItemDialogMode
  | ChooseItemMenu ItemDialogMode
  | ChooseItemProject [Trigger]
  | ChooseItemApply [Trigger]
  | PickLeader Int
  | PickLeaderWithPointer
  | MemberCycle
  | MemberBack
  | SelectActor
  | SelectNone
  | SelectWithPointer
  | Repeat Int
  | Record
  | History
  | MarkVision
  | MarkSmell
  | MarkSuspect
  | SettingsMenu
  | ChallengesMenu
    -- These are mostly related to aiming.
  | Cancel
  | Accept
  | TgtClear
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
data Trigger =
    ApplyItem {verb :: MU.Part, object :: MU.Part, symbol :: Char}
  | AlterFeature {verb :: MU.Part, object :: MU.Part, feature :: TK.Feature}
  deriving (Show, Eq, Ord, Generic)

instance Read Trigger where
  readsPrec = error $ "parsing of Trigger not implemented" `showFailure` ()

instance NFData Trigger

instance Binary Trigger
