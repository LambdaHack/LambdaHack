-- | The type of key-command mappings to be used for the UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..)
  , defaultCmdLMB, defaultCmdMMB, defaultCmdRMB
  , getAscend, descendDrop, chooseAndHelp, defaultHeroSelect
  ) where

import qualified Data.Char as Char

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Common.Misc
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.TileKind as TK

-- | Key-command mappings to be used for the UI.
data KeyKind = KeyKind
  { rhumanCommands :: ![(K.KM, ([CmdCategory], HumanCmd))]
      -- ^ default client UI commands
  }

defaultCmdLMB :: HumanCmd
defaultCmdLMB =
  ByMode "go to pointer for 100 steps or set crosshair"
    (ByArea "normal mode" $ common ++
       [ (CaMapParty, PickLeaderWithPointer)
       , (CaMap, Macro ""
            ["MiddleButtonPress", "CTRL-semicolon", "CTRL-period", "V"]) ])
    (ByArea "aiming mode" $ common ++
       [ (CaMap, TgtPointerEnemy) ])
 where
  common =
    [ (CaMessage, History)
    , (CaMapLeader, getAscend)
    , ( CaArenaName
      , ByMode "cancel target/action or open Main Menu" MainMenu Cancel )
    , (CaXhairDesc, TgtEnemy)  -- inits aiming and then cycles enemies
    , (CaSelected, PickLeaderWithPointer)
    , (CaLeaderStatus, ChooseItem (MStore COrgan))
    , (CaTargetDesc, ChooseItem $ MStore CInv) ]

defaultCmdMMB :: HumanCmd
defaultCmdMMB = CursorPointerFloor

defaultCmdRMB :: HumanCmd
defaultCmdRMB =
  ByMode "run collectively to pointer or set target"
    (ByArea "normal mode" $ common ++
       [ (CaMapParty, SelectWithPointer)
       , (CaMap, Macro ""
            ["MiddleButtonPress", "CTRL-colon", "CTRL-period", "V"]) ])
    (ByArea "aiming mode" $ common ++
       [ (CaMap, Sequence "" "never mind" [CursorPointerEnemy, Accept]) ])
 where
  common =
    [ (CaMessage, Macro "" ["R"])
    , (CaMapLeader, descendDrop)
    , ( CaArenaName
      , ByMode "accept target/choice or open Help" (Help Nothing) Accept )
    , (CaXhairDesc, TgtFloor)  -- inits aiming and then cycles aim mode
    , (CaSelected, SelectWithPointer)
    , (CaLeaderStatus, ChooseItem MStats)
    , (CaTargetDesc, ChooseItem $ MStore CEqp) ]

getAscend :: HumanCmd
getAscend = Sequence "get items or ascend" "cannot get items nor ascend"
  [ MoveItem [CGround] CEqp (Just "get") "items" True
  , ByMode ""
      (TriggerTile
         [ TriggerFeature { verb = "ascend"
                          , object = "a level"
                          , feature = TK.Cause (IK.Ascend 1) }
         , TriggerFeature { verb = "escape"
                          , object = "dungeon"
                          , feature = TK.Cause (IK.Escape 1) } ])
      (TgtAscend 1) ]

descendDrop :: HumanCmd
descendDrop = Sequence "descend or drop items" "cannot descend nor drop items"
  [ ByMode ""
      (TriggerTile
         [ TriggerFeature { verb = "descend"
                          , object = "a level"
                          , feature = TK.Cause (IK.Ascend (-1)) }
         , TriggerFeature { verb = "escape"
                          , object = "dungeon"
                          , feature = TK.Cause (IK.Escape (-1)) } ])
      (TgtAscend (-1))
  , MoveItem [CEqp, CInv, CSha] CGround Nothing "item" False ]

chooseAndHelp :: ItemDialogMode -> HumanCmd
chooseAndHelp dialogMode =
  ComposeIfEmpty (ChooseItem dialogMode) (Help $ Just "f")

defaultHeroSelect :: Int -> (String, ([CmdCategory], HumanCmd))
defaultHeroSelect k =
  ([Char.intToDigit k], ([CmdMeta], Alias "" $ PickLeader k))
