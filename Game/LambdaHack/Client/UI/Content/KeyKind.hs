-- | The type of key-command mappings to be used for the UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..)
  , defaultCmdLMB, defaultCmdMMB, defaultCmdRMB
  , projectFling, getAscend, descendDrop, chooseAndHelp, defaultHeroSelect
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
defaultCmdLMB = Alias "go to pointer for 100 steps or set crosshair" $
  ByMode
    (ByArea $ common ++  -- normal mode
       [ (CaMapParty, PickLeaderWithPointer)
       , (CaMap, Macro
            ["MiddleButtonPress", "CTRL-semicolon", "CTRL-period", "V"]) ])
    (ByArea $ common ++  -- aiming mode
       [ (CaMap, TgtPointerEnemy) ])
 where
  common =
    [ (CaMessage, History)
    , (CaMapLeader, getAscend)
    , (CaArenaName, ByMode MainMenu Cancel)
    , (CaXhairDesc, TgtEnemy)  -- inits aiming and then cycles enemies
    , (CaSelected, PickLeaderWithPointer)
    , (CaLeaderStatus, ChooseItem $ MStore COrgan)
    , (CaTargetDesc, ChooseItem $ MStore CInv) ]

defaultCmdMMB :: HumanCmd
defaultCmdMMB = CursorPointerFloor

defaultCmdRMB :: HumanCmd
defaultCmdRMB = Alias "run collectively to pointer or set target" $
  ByMode
    (ByArea $ common ++
       [ (CaMapParty, SelectWithPointer)
       , (CaMap, Macro
            ["MiddleButtonPress", "CTRL-colon", "CTRL-period", "V"]) ])
    (ByArea $ common ++
       [ (CaMap, ComposeIfLeft TgtPointerEnemy projectFling) ])
 where
  common =
    [ (CaMessage, Macro ["R"])
    , (CaMapLeader, descendDrop)
    , (CaArenaName, ByMode (Help Nothing) Accept)
    , (CaXhairDesc, projectFling)
    , (CaSelected, SelectWithPointer)
    , (CaLeaderStatus, ChooseItem MStats)
    , (CaTargetDesc, ChooseItem $ MStore CEqp) ]

projectFling :: HumanCmd
projectFling = Project [ApplyItem { verb = "fling"
                                  , object = "projectile"
                                  , symbol = ' ' }]

getAscend :: HumanCmd
getAscend = Alias "get items or ascend"
            $ ByMode
  (ReplaceFail "cannot get items nor ascend"
   $ ComposeIfLeft
       (MoveItem [CGround] CEqp (Just "get") "items" True)
       (TriggerTile
          [ TriggerFeature { verb = "ascend"
                           , object = "a level"
                           , feature = TK.Cause (IK.Ascend 1) }
          , TriggerFeature { verb = "escape"
                           , object = "dungeon"
                           , feature = TK.Cause (IK.Escape 1) } ]))
  (TgtAscend 1)

descendDrop :: HumanCmd
descendDrop = Alias "descend or drop items"
              $ ByMode
  (ReplaceFail "cannot descend nor drop items"
   $ ComposeIfLeft
       (TriggerTile
          [ TriggerFeature { verb = "descend"
                           , object = "a level"
                           , feature = TK.Cause (IK.Ascend (-1)) }
          , TriggerFeature { verb = "escape"
                           , object = "dungeon"
                           , feature = TK.Cause (IK.Escape (-1)) } ])
       (MoveItem [CEqp, CInv, CSha] CGround Nothing "item" False))
  (TgtAscend (-1))

chooseAndHelp :: ItemDialogMode -> HumanCmd
chooseAndHelp dialogMode =
  ComposeIfEmpty (ChooseItem dialogMode) (Help $ Just "f")

defaultHeroSelect :: Int -> (String, ([CmdCategory], HumanCmd))
defaultHeroSelect k =
  ([Char.intToDigit k], ([CmdMeta], Alias "" $ PickLeader k))
