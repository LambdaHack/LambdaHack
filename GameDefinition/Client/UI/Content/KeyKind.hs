-- | The default game key-command mapping to be used for UI. Can be overridden
-- via macros in the config file.
module Client.UI.Content.KeyKind ( standardKeys ) where

import Control.Arrow (first)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Common.Misc
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.TileKind as TK

-- | Description of default key-command bindings.
--
-- In addition to these commands, mouse and keys have a standard meaning
-- when navigating various menus.
standardKeys :: KeyKind
standardKeys = KeyKind
  { rhumanCommands = map (first K.mkKM)
      -- All commands are defined here, except some movement and leader picking
      -- commands. All commands are shown on help screens except debug commands
      -- and macros with empty descriptions.
      -- The order below determines the order on the help screens.
      -- Remember to put commands that show information (e.g., enter targeting
      -- mode) first.

      -- Main Menu
      [ ("Escape", ([CmdMainMenu], Alias "back to playing" Clear))
      , ("?", ([CmdMainMenu], Alias "see command help" Help))
      , ("S", ([CmdMainMenu], Alias "enter settings menu" SettingsMenu))
      , ("X", ([CmdMainMenu], GameExit))
      , ("r", ([CmdMainMenu], GameRestart "raid"))
      , ("s", ([CmdMainMenu], GameRestart "skirmish"))
      , ("a", ([CmdMainMenu], GameRestart "ambush"))
      , ("b", ([CmdMainMenu], GameRestart "battle"))
      , ("c", ([CmdMainMenu], GameRestart "campaign"))
      , ("i", ([CmdMainMenu, CmdDebug], GameRestart "battle survival"))
      , ("f", ([CmdMainMenu, CmdDebug], GameRestart "safari"))
      , ("u", ([CmdMainMenu, CmdDebug], GameRestart "safari survival"))
      , ("d", ([CmdMainMenu, CmdDebug], GameRestart "defense"))
      , ("g", ([CmdMainMenu, CmdDebug], GameRestart "boardgame"))
      , ("D", ([CmdMainMenu], GameDifficultyIncr))
      , ("A", ([CmdMainMenu], Automate))

      -- Settings Menu  -- TODO: add some from ClientOptions
      , ("Escape", ([CmdSettingsMenu], Alias "back to Main Menu" MainMenu))
      , ("T", ([CmdSettingsMenu], Tactic))
      , ("S", ([CmdSettingsMenu], MarkSuspect))
      , ("V", ([CmdSettingsMenu], MarkVision))
      , ("C", ([CmdSettingsMenu], MarkSmell))

      -- Movement and terrain alteration
      , ("less", ([CmdMove, CmdMinimal], TriggerTile
           [ TriggerFeature { verb = "ascend"
                            , object = "a level"
                            , feature = TK.Cause (IK.Ascend 1) }
           , TriggerFeature { verb = "escape"
                            , object = "dungeon"
                            , feature = TK.Cause (IK.Escape 1) } ]))
      , ("CTRL-less", ([CmdInternal], TriggerTile  -- with lifts, not interal
           [ TriggerFeature { verb = "ascend"
                            , object = "10 levels"
                            , feature = TK.Cause (IK.Ascend 10) } ]))
      , ("greater", ([CmdMove, CmdMinimal], TriggerTile
           [ TriggerFeature { verb = "descend"
                            , object = "a level"
                            , feature = TK.Cause (IK.Ascend (-1)) }
           , TriggerFeature { verb = "escape"
                            , object = "dungeon"
                            , feature = TK.Cause (IK.Escape (-1)) } ]))
      , ("CTRL-greater", ([CmdInternal], TriggerTile
           [ TriggerFeature { verb = "descend"
                            , object = "10 levels"
                            , feature = TK.Cause (IK.Ascend (-10)) } ]))
      , ("semicolon",
         ( [CmdMove]
         , Macro "go to crosshair for 100 steps"
                 ["CTRL-semicolon", "CTRL-period", "V"] ))
      , ("colon",
         ( [CmdMove]
         , Macro "run selected to crosshair for 100 steps"
                 ["CTRL-colon", "CTRL-period", "V"] ))
      , ("x",
         ( [CmdMove]
         , Macro "explore the closest unknown spot"
                 [ "CTRL-question"  -- no semicolon
                 , "CTRL-period", "V" ] ))
      , ("X",
         ( [CmdMove]
         , Macro "autoexplore 100 times"
                 ["'", "CTRL-question", "CTRL-period", "'", "V"] ))
      , ("R", ([CmdMove], Macro "rest (wait 100 times)"
                                ["KP_Begin", "V"]))
      , ("c", ([CmdMove, CmdMinimal], AlterDir
           [ AlterFeature { verb = "close"
                          , object = "door"
                          , feature = TK.CloseTo "vertical closed door Lit" }
           , AlterFeature { verb = "close"
                          , object = "door"
                          , feature = TK.CloseTo "horizontal closed door Lit" }
           , AlterFeature { verb = "close"
                          , object = "door"
                          , feature = TK.CloseTo "vertical closed door Dark" }
           , AlterFeature { verb = "close"
                          , object = "door"
                          , feature = TK.CloseTo "horizontal closed door Dark" }
           ]))

      -- Item use
      , ("f", ([CmdItem, CmdItemMenu], Project
           [ApplyItem { verb = "fling"
                      , object = "projectile"
                      , symbol = ' ' }]))
      , ("a", ([CmdItem, CmdItemMenu], Apply
           [ ApplyItem { verb = "apply"
                       , object = "consumable"
                       , symbol = ' ' } ]))
      , ("e", ( [CmdItem, CmdItemMenu]
              , MoveItem [CGround, CInv, CSha] CEqp Nothing "item" False) )
      , ("p", ( [CmdItem, CmdItemMenu]
              , MoveItem [CGround, CEqp, CSha] CInv Nothing
                         "item into inventory" False) )
      , ("s", ( [CmdItem, CmdItemMenu]
              , MoveItem [CGround, CInv, CEqp] CSha Nothing
                         "and share item" False) )
      , ("d", ( [CmdItem, CmdItemMenu]
              , MoveItem [CEqp, CInv, CSha] CGround Nothing "item" False) )
      , ("E", ([CmdItem, CmdMinimal], DescribeItem $ MStore CEqp))
      , ("P", ([CmdItem], DescribeItem $ MStore CInv))
      , ("S", ([CmdItem], DescribeItem $ MStore CSha))
      , ("A", ([CmdItem], DescribeItem MOwned))
      , ("G", ([CmdItem], DescribeItem $ MStore CGround))
      , ("@", ([CmdItem], DescribeItem $ MStore COrgan))
      , ("exclam", ([CmdItem], DescribeItem MStats))
      , ("g", ([CmdItem, CmdMinimal],
               MoveItem [CGround] CEqp (Just "get") "items" True))
      , ("q", ([CmdItem], Apply [ApplyItem { verb = "quaff"
                                           , object = "potion"
                                           , symbol = '!' }]))
      , ("r", ([CmdItem], Apply [ApplyItem { verb = "read"
                                           , object = "scroll"
                                           , symbol = '?' }]))
      , ("t", ([CmdItem], Project [ApplyItem { verb = "throw"
                                             , object = "missile"
                                             , symbol = '|' }]))
--      , ("z", ([CmdItem], Project [ApplyItem { verb = "zap"
--                                             , object = "wand"
--                                             , symbol = '/' }]))

      -- Targeting
      , ("KP_Multiply", ([CmdTgt], TgtEnemy))
      , ("backslash", ([CmdTgt], Alias "" TgtEnemy))
      , ("KP_Divide", ([CmdTgt], TgtFloor))
      , ("bar", ([CmdTgt], Alias "" TgtFloor))
      , ("plus", ([CmdTgt, CmdMinimal], EpsIncr True))
      , ("minus", ([CmdTgt], EpsIncr False))
      , ("CTRL-question", ([CmdTgt], CursorUnknown))
      , ("CTRL-I", ([CmdTgt], CursorItem))
      , ("CTRL-braceleft", ([CmdTgt], CursorStair True))
      , ("CTRL-braceright", ([CmdTgt], CursorStair False))
      , ("BackSpace", ([CmdTgt], TgtClear))
      , ("Escape", ([CmdTgt, CmdMinimal], Cancel))
      , ("Return", ([CmdTgt, CmdMinimal], Accept))

      -- Assorted
      , ("space", ([CmdMeta], Clear))
      , ("question", ([CmdMeta], Help))
      , ("D", ([CmdMeta, CmdMinimal], History))
      , ("Tab", ([CmdMeta], MemberCycle))
      , ("ISO_Left_Tab", ([CmdMeta, CmdMinimal], MemberBack))
      , ("equal", ([CmdMeta], SelectActor))
      , ("underscore", ([CmdMeta], SelectNone))
      , ("v", ([CmdMeta], Repeat 1))
      , ("V", ([CmdMeta], Repeat 100))
      , ("CTRL-v", ([CmdMeta], Repeat 1000))
      , ("CTRL-V", ([CmdMeta], Repeat 25))
      , ("apostrophe", ([CmdMeta], Record))

      -- Mouse
      , ("LeftButtonPress", ([CmdMouse], defaultCmdLMB))
      , ("SHIFT-LeftButtonPress", ([CmdMouse], defaultCmdShiftLMB))
      , ("MiddleButtonPress", ([CmdMouse], defaultCmdMMB))
      , ("SHIFT-MiddleButtonPress", ([CmdMouse], defaultCmdShiftMMB))
      , ("RightButtonPress", ([CmdMouse], defaultCmdRMB))
      , ("SHIFT-RightButtonPress", ([CmdMouse], defaultCmdShiftRMB))

      -- Debug and others not to display in help screens
      , ("CTRL-S", ([CmdDebug], GameSave))
      , ("CTRL-semicolon", ([CmdInternal], MoveOnceToCursor))
      , ("CTRL-colon", ([CmdInternal], RunOnceToCursor))
      , ("CTRL-period", ([CmdInternal], ContinueToCursor))
      , ("CTRL-comma", ([CmdInternal], RunOnceAhead))
      , ("CTRL-LeftButtonPress", ([CmdInternal], defaultCmdShiftLMB))
      , ("CTRL-MiddleButtonPress", ([CmdInternal], defaultCmdShiftMMB))
      , ("CTRL-RightButtonPress", ([CmdInternal], defaultCmdShiftRMB))
      , ("ALT-space", ([CmdInternal], StopIfTgtMode))
      , ("ALT-minus", ([CmdInternal], SelectWithPointer))
     ]
  }
