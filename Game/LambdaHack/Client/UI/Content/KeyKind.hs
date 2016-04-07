-- | The type of key-command mappings to be used for the UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..)
  , defaultCmdLMB, defaultCmdMMB, defaultCmdRMB
  , defaultHeroSelect
  ) where

import qualified Data.Char as Char

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Common.Misc

-- | Key-command mappings to be used for the UI.
data KeyKind = KeyKind
  { rhumanCommands :: ![(K.KM, ([CmdCategory], HumanCmd))]
      -- ^ default client UI commands
  }

defaultCmdLMB :: HumanCmd
defaultCmdLMB =
  ByMode "go to pointer for 100 steps"
    (ByArea "normal mode" $ common ++
       [ (CaMapParty, PickLeaderWithPointer)
       , (CaMap, Macro ""
            ["MiddleButtonPress", "CTRL-semicolon", "CTRL-period", "V"]) ])
    (ByArea "aiming mode" $ common ++
       [ (CaMap, TgtPointerEnemy) ])
 where
  common =
    [ (CaMessage, History)
    , (CaMapLeader, Macro "" ["g"])
    , (CaArenaName, Cancel)
    , (CaXhairDesc, TgtEnemy)  -- inits aiming and then cycles enemies
    , (CaSelected, PickLeaderWithPointer)
    , (CaLeaderStatus, DescribeItem (MStore COrgan))
    , (CaTargetDesc, TgtFloor) ]  -- inits aiming and then cycles aim mode

defaultCmdMMB :: HumanCmd
defaultCmdMMB = CursorPointerFloor

defaultCmdRMB :: HumanCmd
defaultCmdRMB =
  ByMode "run collectively to pointer for 100 steps"
    (ByArea "normal mode" $ common ++
       [ (CaMapParty, SelectWithPointer)
       , (CaMap, Macro ""
            ["MiddleButtonPress", "CTRL-colon", "CTRL-period", "V"]) ])
    (ByArea "aiming mode" $ common ++
       [ (CaMap, CursorPointerEnemy) ])
 where
  common =
    [ (CaMessage, Macro "" ["R"])
    , (CaMapLeader, Macro "" ["a"])
    , (CaArenaName, Accept)
    , (CaXhairDesc, TgtEnemy)  -- inits aiming and then cycles enemies
    , (CaSelected, SelectWithPointer)
    , (CaLeaderStatus, DescribeItem MStats)
    , (CaTargetDesc, TgtFloor) ]  -- inits aiming and then cycles aim mode

defaultHeroSelect :: Int -> (String, ([CmdCategory], HumanCmd))
defaultHeroSelect k = ([Char.intToDigit k], ([CmdMeta], PickLeader k))
