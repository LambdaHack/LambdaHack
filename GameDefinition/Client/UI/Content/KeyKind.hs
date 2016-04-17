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
  { rhumanCommands = map (first K.mkKM) $
      -- All commands are defined here, except some movement and leader picking
      -- commands. All commands are shown on help screens except debug commands
      -- and macros with empty descriptions.
      -- The order below determines the order on the help screens.
      -- Remember to put commands that show information (e.g., enter targeting
      -- mode) first.

      -- Main Menu
      [ ("Escape", ([CmdMainMenu], Alias "back to playing" Clear))
      , ("?", ([CmdMainMenu], Alias "see command help" (Help $ Just "") ))
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
      , ("less", ([CmdMove, CmdItem, CmdMinimal], getAscend))
      , ("g", ([CmdMove, CmdItem], Alias "" getAscend))
      , ("comma", ([CmdInternal], Alias "" getAscend))
      , ("CTRL-less", ([CmdInternal], TriggerTile  -- with lifts, not interal
           [ TriggerFeature { verb = "ascend"
                            , object = "10 levels"
                            , feature = TK.Cause (IK.Ascend 10) } ]))
      , ("greater", ([CmdMove, CmdItem, CmdMinimal], descendDrop))
      , ("d", ([CmdMove, CmdItem], Alias "" descendDrop))
      , ("period", ([CmdInternal], Alias "" descendDrop))
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
      , ("f", ([CmdItem], Project
           [ApplyItem { verb = "fling"
                      , object = "projectile"
                      , symbol = ' ' }]))
      , ("a", ([CmdItem], Apply
           [ ApplyItem { verb = "apply"
                       , object = "consumable"
                       , symbol = ' ' } ]))
      , ("e", ( [CmdItem]
              , MoveItem [CGround, CInv, CSha] CEqp Nothing "item" False) )
      , ("p", ( [CmdItem]
              , MoveItem [CGround, CEqp, CSha] CInv Nothing
                         "item into inventory" False) )
      , ("s", ( [CmdItem]
              , MoveItem [CGround, CInv, CEqp] CSha Nothing
                         "and share item" False) )
      , ("E", ([CmdItem], chooseAndHelp $ MStore CEqp))
      , ("P", ([CmdItem, CmdMinimal], chooseAndHelp $ MStore CInv))
      , ("S", ([CmdItem], chooseAndHelp $ MStore CSha))
      , ("A", ([CmdItem], chooseAndHelp MOwned))
      , ("G", ([CmdItem], chooseAndHelp $ MStore CGround))
      , ("@", ([CmdItem], chooseAndHelp $ MStore COrgan))
      , ("exclam", ([CmdItem], chooseAndHelp MStats))
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
      , ( "Escape"
        , ( [CmdTgt, CmdMinimal]
          , ByMode "cancel target/action or open Main Menu" MainMenu Cancel ) )
      , ( "Return"
        , ( [CmdTgt, CmdMinimal]
          , ByMode "accept target/choice or open Help"
                   (Help $ Just "") Accept ) )

      -- Assorted
      , ("space", ([CmdMeta], Clear))
      , ("question", ([CmdMeta], Help Nothing))
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
      -- Doubleclick acts as RMB and modifiers as MMB, which is optional.
      , ("LeftButtonPress", ([CmdMouse], defaultCmdLMB))
      , ("SHIFT-LeftButtonPress", ([CmdInternal], defaultCmdMMB))
      , ("CTRL-LeftButtonPress", ([CmdInternal], defaultCmdMMB))
      , ("MiddleButtonPress", ([CmdMouse], defaultCmdMMB))
      , ("RightButtonPress", ([CmdMouse], defaultCmdRMB))

      -- Debug and others not to display in help screens
      , ("CTRL-S", ([CmdDebug], GameSave))
      , ("CTRL-semicolon", ([CmdInternal], MoveOnceToCursor))
      , ("CTRL-colon", ([CmdInternal], RunOnceToCursor))
      , ("CTRL-period", ([CmdInternal], ContinueToCursor))
      , ("CTRL-comma", ([CmdInternal], RunOnceAhead))
      ]
      ++ map defaultHeroSelect [0..6]
  }
