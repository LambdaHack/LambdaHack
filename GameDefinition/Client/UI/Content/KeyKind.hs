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
              $ addCmdCategory CmdItemMenu $ grabItems "grab item(s)")
      , ("comma", grabItems "")
      , ("d", addCmdCategory CmdItemMenu $ dropItems "drop item(s)")
      , ("period", dropItems "")
      , ("f", addCmdCategory CmdItemMenu $ projectA flingTs)
      , ("C-f", addCmdCategory CmdItemMenu
                $ replaceDesc "fling without aiming" $ projectI flingTs)
      , ("a", addCmdCategory CmdItemMenu $ applyI [ApplyItem
                { verb = "apply"
                , object = "consumable"
                , symbol = ' ' }])
      , ("C-a", addCmdCategory CmdItemMenu
                $ replaceDesc "apply and keep choice" $ applyIK [ApplyItem
                  { verb = "apply"
                  , object = "consumable"
                  , symbol = ' ' }])

      -- Terrain exploration and alteration
      , ("semicolon", ( [CmdMove]
                      , "go to crosshair for 25 steps"
                      , Macro ["C-semicolon", "C-period", "C-V"] ))
      , ("colon", ( [CmdMove]
                  , "run to crosshair collectively for 25 steps"
                  , Macro ["C-colon", "C-period", "C-V"] ))
      , ("x", ( [CmdMove]
              , "explore nearest unknown spot"
              , autoexploreCmd ))
      , ("X", ( [CmdMove]
              , "autoexplore 25 times"
              , autoexplore25Cmd ))
      , ("R", ([CmdMove], "rest (wait 25 times)", Macro ["KP_5", "C-V"]))
      , ("C-R", ( [CmdMove], "lurk (wait 0.1 of a turn 100 times)"
                , Macro ["C-KP_5", "V"] ))
      , let triggerClose =
              [ AlterFeature { verb = "close"
                             , object = "door"
                             , feature =
                                 TK.CloseTo "closed vertical door Lit" }
              , AlterFeature { verb = "close"
                             , object = "door"
                             , feature =
                                 TK.CloseTo "closed horizontal door Lit" }
              , AlterFeature { verb = "close"
                             , object = "door"
                             , feature =
                                 TK.CloseTo "closed vertical door Dark" }
              , AlterFeature { verb = "close"
                             , object = "door"
                             , feature =
                                 TK.CloseTo "closed horizontal door Dark" }
              ]
        in ("c", ( [CmdMove, CmdMinimal]
                 , descTs triggerClose
                 , AlterDir triggerClose ))

      -- Item use, continued
      , ("^", ( [CmdItem], "sort items by kind and stats", SortSlots))
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
      , ("~", ( [CmdItem]
              , "display known lore"
              , ChooseItemMenu MLoreItem ))
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
      , ("KP_Multiply", ( [CmdAim, CmdMinimal]
                        , "cycle x-hair among enemies", AimEnemy ))
      , ("!", ([CmdAim], "", AimEnemy))
      , ("\\", ([CmdAim], "cycle aiming styles", AimFloor))
      , ("KP_Divide", ([CmdAim], "cycle x-hair among items", AimItem))
      , ("/", ([CmdAim], "", AimItem))
      , ("+", ([CmdAim, CmdMinimal], "swerve the aiming line", EpsIncr True))
      , ("-", ([CmdAim], "unswerve the aiming line", EpsIncr False))
      , ("C-?", ( [CmdAim]
                , "set crosshair to nearest unknown spot"
                , XhairUnknown ))
      , ("C-I", ( [CmdAim]
                , "set crosshair to nearest item"
                , XhairItem ))
      , ("C-{", ( [CmdAim]
                , "set x-hair to nearest upstairs"
                , XhairStair True ))
      , ("C-}", ( [CmdAim]
                , "set x-hair to nearest downstairs"
                , XhairStair False ))
      , ("<", ([CmdAim], "switch view to one level higher" , AimAscend 1))
      , ("C-<", ( [CmdNoHelp], "switch view to 10 levels higher"
                , AimAscend 10) )
      , (">", ([CmdAim], "switch view to one level lower", AimAscend (-1)))
      , ("C->", ( [CmdNoHelp], "switch view to 10 levels lower"
                , AimAscend (-10)) )
      , ("BackSpace" , ( [CmdAim]
                     , "clear chosen object and target"
                     , ComposeUnlessError ObjectClear TgtClear ))
      , ("Escape", ( [CmdAim, CmdMinimal]
                   , "cancel aiming/open Main Menu"
                   , ByAimMode {exploration = MainMenu, aiming = Cancel} ))
      , ("Return", ( [CmdAim, CmdMinimal]
                   , "accept target/open Help"
                   , ByAimMode {exploration = Help, aiming = Accept} ))

      -- Assorted
      , ("space", ( [CmdMinimal, CmdMeta]
                  , "clear messages/display history", Clear ))
      , ("?", ([CmdMeta], "display Help", Help))
      , ("F1", ([CmdMeta], "", Help))
      , ("Tab", ( [CmdMeta]
                , "cycle among party members on the level"
                , MemberCycle ))
      , ("BackTab", ( [CmdMeta, CmdMinimal]
                  , "cycle among all party members"
                  , MemberBack ))
      , ("=", ( [CmdMinimal, CmdMeta]
              , "select (or deselect) party member", SelectActor) )
      , ("_", ([CmdMeta], "deselect (or select) all on the level", SelectNone))
      , ("v", ([CmdMeta], "voice again the recorded commands", Repeat 1))
      , ("V", repeatTriple 100)
      , ("C-v", repeatTriple 1000)
      , ("C-V", repeatTriple 25)
      , ("'", ([CmdMeta], "start recording commands", Record))

      -- Mouse
      , ("LeftButtonRelease", mouseLMB)
      , ( "C-LeftButtonRelease"
        , replaceDesc "" $ addCmdCategory CmdNoHelp mouseRMB)  -- Mac convention
      , ("RightButtonRelease", mouseRMB)
      , ("MiddleButtonRelease", mouseMMB)
      , ("WheelNorth", ([CmdMouse], "swerve the aiming line", Macro ["+"]))
      , ("WheelSouth", ([CmdMouse], "unswerve the aiming line", Macro ["-"]))
      , ( "LeftDblClick"
        , replaceDesc "" $ addCmdCategory CmdNoHelp mouseRMB )

      -- Debug and others not to display in help screens
      , ("C-S", ([CmdDebug], "save game", GameSave))
      , ("C-semicolon", ( [CmdNoHelp]
                        , "move one step towards the crosshair"
                        , MoveOnceToXhair ))
      , ("C-colon", ( [CmdNoHelp]
                    , "run collectively one step towards the crosshair"
                    , RunOnceToXhair ))
      , ("C-period", ( [CmdNoHelp]
                     , "continue towards the crosshair"
                     , ContinueToXhair ))
      , ("C-comma", ([CmdNoHelp], "run once ahead", RunOnceAhead))
      , ("safe1", ( [CmdInternal]
                  , "go to pointer for 25 steps"
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
      ]
      ++ map defaultHeroSelect [0..6]
  }
