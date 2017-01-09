-- | The default game key-command mapping to be used for UI. Can be overridden
-- via macros in the config file.
module Client.UI.Content.KeyKind
  ( standardKeys
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Common.Misc
import qualified Game.LambdaHack.Content.TileKind as TK

-- | Description of default key-command bindings.
--
-- In addition to these commands, mouse and keys have a standard meaning
-- when navigating various menus.
standardKeys :: KeyKind
standardKeys = KeyKind
  { rhumanCommands = map evalKeyDef $
      -- All commands are defined here, except some movement and leader picking
      -- commands. All commands are shown on help screens except debug commands
      -- and macros with empty descriptions.
      -- The order below determines the order on the help screens.
      -- Remember to put commands that show information (e.g., enter aiming
      -- mode) first.

      -- Main Menu
      [ ("n", ([CmdMainMenu], "start new game", GameRestart))
      , ("x", ([CmdMainMenu], "save and exit", GameExit))
      , ("m", ([CmdMainMenu], "enter settings menu", SettingsMenu))
      , ("a", ([CmdMainMenu], "automate faction", Automate))
      , ("?", ([CmdMainMenu], "see command Help", Help))
      , ("Escape", ([CmdMainMenu], "back to playing", Cancel))

      -- Item use, 1st part
      , ("g", addCmdCategory CmdMinimal
              $ addCmdCategory CmdItemMenu $ grabItems "grab items")
      , ("comma", addCmdCategory CmdNoHelp $ grabItems "")
      , ("d", addCmdCategory CmdItemMenu $ dropItems "drop items")
      , ("period", addCmdCategory CmdNoHelp $ dropItems "")
      , ("f", addCmdCategory CmdItemMenu $ projectA flingTs)
      , ("CTRL-f", addCmdCategory CmdItemMenu
                   $ replaceDesc "fling without aiming" $ projectI flingTs)
      , ("a", addCmdCategory CmdItemMenu $ applyI [ApplyItem
                { verb = "apply"
                , object = "consumable"
                , symbol = ' ' }])

      -- Terrain exploration and alteration
      , ("semicolon", ( [CmdMove]
                      , "go to crosshair for 100 steps"
                      , Macro ["CTRL-semicolon", "CTRL-period", "V"] ))
      , ("colon", ( [CmdMove]
                  , "run to crosshair collectively for 100 steps"
                  , Macro ["CTRL-colon", "CTRL-period", "V"] ))
      , ("x", ( [CmdMove]
              , "explore nearest unknown spot"
              , autoexploreCmd ))
      , ("X", ( [CmdMove]
              , "autoexplore 100 times"
              , autoexplore100Cmd ))
      , ("R", ([CmdMove], "rest (wait 100 times)", Macro ["KP_5", "V"]))
      , let triggerClose =
              [ AlterFeature { verb = "close"
                             , object = "door"
                             , feature =
                                 TK.CloseTo "vertical closed door Lit" }
              , AlterFeature { verb = "close"
                             , object = "door"
                             , feature =
                                 TK.CloseTo "horizontal closed door Lit" }
              , AlterFeature { verb = "close"
                             , object = "door"
                             , feature =
                                 TK.CloseTo "vertical closed door Dark" }
              , AlterFeature { verb = "close"
                             , object = "door"
                             , feature =
                                 TK.CloseTo "horizontal closed door Dark" }
              ]
        in ("c", ( [CmdMove, CmdMinimal]
                 , descTs triggerClose
                 , AlterDir triggerClose ))

      -- Item use, continued
      , ("p", addCmdCategory CmdItemMenu
              $ moveItemTriple [CGround, CEqp, CSha] CInv
                               "item into inventory" False)
      , ("e", addCmdCategory CmdItemMenu
              $ moveItemTriple [CGround, CInv, CSha] CEqp
                               "item" False)
      , ("s", addCmdCategory CmdItemMenu
              $ moveItemTriple [CGround, CInv, CEqp] CSha
                               "and share item" False)
      , ("P", ( [CmdMinimal, CmdItem]
              , "manage inventory pack of leader"
              , ChooseItemMenu (MStore CInv) ))
      , ("G", ( [CmdItem]
              , "manage items on the ground"
              , ChooseItemMenu (MStore CGround) ))
      , ("E", ( [CmdItem]
              , "manage equipment of the leader"
              , ChooseItemMenu (MStore CEqp) ))
      , ("S", ( [CmdItem]
              , "manage the shared party stash"
              , ChooseItemMenu (MStore CSha) ))
      , ("A", ( [CmdItem]
              , "manage all owned items"
              , ChooseItemMenu MOwned ))
      , ("@", ( [CmdItem]
              , "describe organs of the leader"
              , ChooseItemMenu (MStore COrgan) ))
      , ("#", ( [CmdItem]
              , "show stat summary of the leader"
              , ChooseItemMenu MStats ))
      , ("q", applyI [ApplyItem { verb = "quaff"
                                , object = "potion"
                                , symbol = '!' }])
      , ("r", applyI [ApplyItem { verb = "read"
                                , object = "scroll"
                                , symbol = '?' }])
      , ("t", projectA [ApplyItem { verb = "throw"
                                  , object = "missile"
                                  , symbol = '|' }])
--      , ("z", projectA [ApplyItem { verb = "zap"
--                                  , object = "wand"
--                                  , symbol = '/' }])

      -- Aiming
      , ("KP_Multiply", ([CmdAim], "cycle x-hair among enemies", AimEnemy))
      , ("!", ([CmdAim], "", AimEnemy))
      , ("KP_Divide", ([CmdAim], "cycle aiming styles", AimFloor))
      , ("/", ([CmdAim], "", AimFloor))
      , ("+", ([CmdAim, CmdMinimal], "swerve the aiming line", EpsIncr True))
      , ("-", ([CmdAim], "unswerve the aiming line", EpsIncr False))
      , ("CTRL-?", ( [CmdAim]
                   , "set crosshair to nearest unknown spot"
                   , XhairUnknown ))
      , ("CTRL-I", ( [CmdAim]
                   , "set crosshair to nearest item"
                   , XhairItem ))
      , ("CTRL-{", ( [CmdAim]
                   , "set x-hair to nearest upstairs"
                   , XhairStair True ))
      , ("CTRL-}", ( [CmdAim]
                   , "set x-hair to nearest downstairs"
                   , XhairStair False ))
      , ("<", ([CmdAim], "ascend aim" , AimAscend 1))
      , ("CTRL-<", ([CmdNoHelp], "ascend aim 10 times" , AimAscend 10))
      , (">", ([CmdAim], "descend aim", AimAscend (-1)))
      , ("CTRL->", ([CmdNoHelp], "descend aim 10 times", AimAscend (-10)))
      , ( "BackSpace"
        , ([CmdAim], "clear target or chosen item", TgtClear) )
      , ("Escape", ( [CmdAim, CmdMinimal]
                   , "cancel aiming/open Main Menu"
                   , ByAimMode {exploration = MainMenu, aiming = Cancel} ))
      , ("Return", ( [CmdAim, CmdMinimal]
                   , "accept target/open Help"
                   , ByAimMode {exploration = Help, aiming = Accept} ))

      -- Assorted
      , ("space", ( [CmdMinimal, CmdMeta]
                  , "clear messages/display history", Clear) )
      , ("?", ([CmdMeta], "display Help", Help))
      , ("F1", ([CmdNoHelp], "", Help))
      , ("Tab", ( [CmdMeta]
                , "cycle among party members on the level"
                , MemberCycle ))
      , ("ISO_Left_Tab", ( [CmdMeta, CmdMinimal]
                         , "cycle among all party members"
                         , MemberBack ))
      , ("=", ( [CmdMinimal, CmdMeta]
              , "select (or deselect) party member", SelectActor) )
      , ("_", ([CmdMeta], "deselect (or select) all on the level", SelectNone))
      , ("v", ([CmdMeta], "voice again the recorded commands", Repeat 1))
      , ("V", repeatTriple 100)
      , ("CTRL-v", repeatTriple 1000)
      , ("CTRL-V", repeatTriple 25)
      , ("'", ([CmdMeta], "start recording commands", Record))

      -- Mouse
      , ("LeftButtonRelease", mouseLMB)
      , ("RightButtonRelease", mouseRMB)
      , ("MiddleButtonRelease", mouseMMB)
      , ("WheelNorth", ([CmdMouse], "swerve the aiming line", Macro ["+"]))
      , ("WheelSouth", ([CmdMouse], "unswerve the aiming line", Macro ["-"]))
      , ( "LeftDblClick"
        , replaceDesc "" $ addCmdCategory CmdNoHelp mouseRMB )

      -- Debug and others not to display in help screens
      , ("CTRL-S", ([CmdDebug], "save game", GameSave))
      , ("CTRL-semicolon", ( [CmdNoHelp]
                           , "move one step towards the crosshair"
                           , MoveOnceToXhair ))
      , ("CTRL-colon", ( [CmdNoHelp]
                       , "run collectively one step towards the crosshair"
                       , RunOnceToXhair ))
      , ("CTRL-period", ( [CmdNoHelp]
                        , "continue towards the crosshair"
                        , ContinueToXhair ))
      , ("CTRL-comma", ([CmdNoHelp], "run once ahead", RunOnceAhead))
      , ("safe1", ( [CmdInternal]
                  , "go to pointer for 100 steps"
                  , goToCmd ))
      , ("safe2", ( [CmdInternal]
                  , "run to pointer collectively"
                  , runToAllCmd ))
      , ("safe3", ( [CmdInternal]
                  , "pick new leader on screen"
                  , PickLeaderWithPointer ))
      , ("safe4", ( [CmdInternal]
                  , "select party member on screen"
                  , SelectWithPointer ))
      , ("safe5", ( [CmdInternal]
                  , "set crosshair to enemy"
                  , AimPointerEnemy ))
      , ("safe6", ( [CmdInternal]
                  , "fling at enemy under pointer"
                  , aimFlingCmd ))
      , ("safe7", ( [CmdInternal]
                  , "open Main Menu"
                  , MainMenu ))
      , ("safe8", ( [CmdInternal]
                  , "cancel aiming"
                  , Cancel ))
      , ("safe9", ( [CmdInternal]
                  , "accept target"
                  , Accept ))
      , ("safe11", ( [CmdInternal]
                   , "grab items"
                   , exploreGrabCmd ))
      , ("safe12", ( [CmdInternal]
                   , "switch view to one level higher"
                   , AimAscend 1 ))
      , ("safe13", ( [CmdInternal]
                   , "drop items"
                   , exploreDropCmd ))
      , ("safe14", ( [CmdInternal]
                   , "switch view to one level lower"
                   , AimAscend (-1) ))
      ]
      ++ map defaultHeroSelect [0..6]
  }
