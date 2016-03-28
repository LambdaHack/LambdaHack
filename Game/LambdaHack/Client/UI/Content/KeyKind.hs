-- | The type of key-command mappings to be used for the UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..)
  , defaultCmdLMB, defaultCmdShiftLMB
  , defaultCmdMMB, defaultCmdShiftMMB
  , defaultCmdRMB, defaultCmdShiftRMB
  ) where

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.HumanCmd

-- | Key-command mappings to be used for the UI.
data KeyKind = KeyKind
  { rhumanCommands :: ![(K.KM, ([CmdCategory], HumanCmd))]
      -- ^ default client UI commands
  }

defaultCmdLMB :: HumanCmd
defaultCmdLMB =
  Macro "go to pointer for 100 steps"
        [ "ALT-space", "ALT-minus"
        , "SHIFT-MiddleButtonPress", "CTRL-semicolon"
        , "CTRL-period", "V" ]

defaultCmdShiftLMB :: HumanCmd
defaultCmdShiftLMB =
  Macro "run collectively to pointer for 100 steps"
        [ "ALT-space"
        , "SHIFT-MiddleButtonPress", "CTRL-colon"
        , "CTRL-period", "V" ]

defaultCmdMMB :: HumanCmd
defaultCmdMMB = CursorPointerEnemy

defaultCmdShiftMMB :: HumanCmd
defaultCmdShiftMMB = CursorPointerFloor

defaultCmdRMB :: HumanCmd
defaultCmdRMB = TgtPointerEnemy

defaultCmdShiftRMB :: HumanCmd
defaultCmdShiftRMB = TgtPointerEnemy
