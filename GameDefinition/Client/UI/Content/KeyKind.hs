-- | The default game key-command mapping to be used for UI. Can be overriden
-- via macros in the config file.
module Client.UI.Content.KeyKind ( standardKeys ) where

import Control.Arrow (first)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Common.Effect as Effect
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Misc

standardKeys :: KeyKind
standardKeys = KeyKind
  { rhumanCommands = map (first K.mkKM)
      -- All commands are defined here, except some movement and leader picking
      -- commands. All commands are shown on help screens except debug commands
      -- and macros with empty descriptions.
      -- The order below determines the order on the help screens.
      -- Remember to put commands that show information (e.g., enter targeting
      -- mode) first.

      -- Main Menu, which apart of these includes a few extra commands
      [ ("CTRL-x", ([CmdMenu], GameExit))
      , ("CTRL-u", ([CmdMenu], GameRestart "duel"))
      , ("CTRL-k", ([CmdMenu], GameRestart "skirmish"))
      , ("CTRL-m", ([CmdMenu], GameRestart "ambush"))
      , ("CTRL-b", ([CmdMenu], GameRestart "battle"))
      , ("CTRL-a", ([CmdMenu], GameRestart "campaign"))
      , ("CTRL-d", ([CmdMenu], GameDifficultyCycle))

      -- Movement and terrain alteration
      , ("less", ([CmdMove, CmdMinimal], TriggerTile
           [ TriggerFeature { verb = "ascend"
                            , object = "a level"
                            , feature = F.Cause (Effect.Ascend 1) }
           , TriggerFeature { verb = "escape"
                            , object = "dungeon"
                            , feature = F.Cause (Effect.Escape 1) } ]))
      , ("CTRL-less", ([CmdMove], TriggerTile
           [ TriggerFeature { verb = "ascend"
                            , object = "10 levels"
                            , feature = F.Cause (Effect.Ascend 10) } ]))
      , ("greater", ([CmdMove, CmdMinimal], TriggerTile
           [ TriggerFeature { verb = "descend"
                            , object = "a level"
                            , feature = F.Cause (Effect.Ascend (-1)) }
           , TriggerFeature { verb = "escape"
                            , object = "dungeon"
                            , feature = F.Cause (Effect.Escape (-1)) } ]))
      , ("CTRL-greater", ([CmdMove], TriggerTile
           [ TriggerFeature { verb = "descend"
                            , object = "10 levels"
                            , feature = F.Cause (Effect.Ascend (-10)) } ]))
      , ("semicolon", ([CmdMove], StepToTarget))
      , ("colon", ([CmdMove], Macro "go to target for 100 steps"
                                    ["semicolon", "V"]))
      , ("CTRL-colon", ([CmdMove], Macro "go to target for 10 steps"
                                         ["semicolon", "CTRL-V"]))
      , ("x", ([CmdMove], Macro "explore the closest unknown spot"
                                [ "BackSpace"
                                , "CTRL-question", "semicolon", "V" ]))
      , ("X", ([CmdMove], Macro "autoexplore 100 times"
                                [ "BackSpace"
                                , "'", "CTRL-question", "semicolon", "'"
                                , "V" ]))
      , ("CTRL-X", ([CmdMove], Macro "autoexplore 10 times"
                                      [ "BackSpace"
                                      , "'", "CTRL-question", "semicolon", "'"
                                      , "CTRL-V" ]))
      , ("R", ([CmdMove], Macro "rest (wait 100 times)"
                                ["KP_Begin", "V"]))
      , ("CTRL-R", ([CmdMove], Macro "rest (wait 10 times)"
                                     ["KP_Begin", "CTRL-V"]))
      , ("c", ([CmdMove], AlterDir
           [ AlterFeature { verb = "close"
                          , object = "door"
                          , feature = F.CloseTo "vertical closed door Lit" }
           , AlterFeature { verb = "close"
                          , object = "door"
                          , feature = F.CloseTo "horizontal closed door Lit" }
           , AlterFeature { verb = "close"
                          , object = "door"
                          , feature = F.CloseTo "vertical closed door Dark" }
           , AlterFeature { verb = "close"
                          , object = "door"
                          , feature = F.CloseTo "horizontal closed door Dark" }
           ]))
      , ("period", ([CmdMove], Macro "" ["KP_Begin"]))
      , ("i", ([CmdMove], Macro "" ["KP_Begin"]))

      -- Item use
      , ("E", ([CmdItem], DescribeItem CEqp))
      , ("P", ([CmdItem], DescribeItem CInv))
      , ("S", ([CmdItem], DescribeItem CSha))
      , ("G", ([CmdItem], DescribeItem CGround))
      , ("A", ([CmdItem], AllOwned))
      , ("g", ([CmdItem, CmdMinimal],
               MoveItem [CGround] CEqp "get" "an item" True))
      , ("d", ([CmdItem], MoveItem [CEqp, CInv, CSha] CGround
                                   "drop" "an item" False))
      , ("e", ([CmdItem], MoveItem [CInv, CSha] CEqp
                                   "equip" "an item" False))
      , ("p", ([CmdItem], MoveItem [CEqp, CSha] CInv
                                   "pack" "an item into inventory backpack"
                                   False))
      , ("s", ([CmdItem], MoveItem [CEqp, CInv] CSha
                                   "stash" "and share an item" False))
      , ("a", ([CmdItem, CmdMinimal], Apply
           [ ApplyItem { verb = "activate"
                       , object = "applicable item"
                       , symbol = ' ' }
           , ApplyItem { verb = "quaff"
                       , object = "potion"
                       , symbol = '!' }
           , ApplyItem { verb = "read"
                       , object = "scroll"
                       , symbol = '?' }
           ]))
      , ("q", ([CmdItem], Apply [ApplyItem { verb = "quaff"
                                           , object = "potion"
                                           , symbol = '!' }]))
      , ("r", ([CmdItem], Apply [ApplyItem { verb = "read"
                                           , object = "scroll"
                                           , symbol = '?' }]))
      , ("f", ([CmdItem, CmdMinimal], Project
           [ApplyItem { verb = "fling"
                      , object = "projectable item"
                      , symbol = ' ' }]))
      , ("t", ([CmdItem], Project [ApplyItem { verb = "throw"
                                             , object = "missile"
                                             , symbol = '|' }]))
      , ("z", ([CmdItem], Project [ApplyItem { verb = "zap"
                                             , object = "wand"
                                             , symbol = '/' }]))

      -- Targeting
      , ("KP_Multiply", ([CmdTgt, CmdMinimal], TgtEnemy))
      , ("backslash", ([CmdTgt], Macro "" ["KP_Multiply"]))
      , ("slash", ([CmdTgt], TgtFloor))
      , ("plus", ([CmdTgt], EpsIncr True))
      , ("minus", ([CmdTgt], EpsIncr False))
      , ("BackSpace", ([CmdTgt], TgtClear))
      , ("CTRL-question", ([CmdTgt], TgtUnknown))
      , ("CTRL-I", ([CmdTgt], TgtItem))
      , ("CTRL-braceleft", ([CmdTgt], TgtStair True))
      , ("CTRL-braceright", ([CmdTgt], TgtStair False))

      -- Automation
      , ("equal", ([CmdAuto], SelectActor))
      , ("underscore", ([CmdAuto], SelectNone))
      , ("v", ([CmdAuto], Repeat 1))
      , ("V", ([CmdAuto], Repeat 100))
      , ("CTRL-v", ([CmdAuto], Repeat 1000))
      , ("CTRL-V", ([CmdAuto], Repeat 10))
      , ("apostrophe", ([CmdAuto], Record))
      , ("CTRL-A", ([CmdAuto], Automate))

      -- Assorted
      , ("question", ([CmdMeta], Help))
      , ("D", ([CmdMeta], History))
      , ("T", ([CmdMeta], MarkSuspect))
      , ("Z", ([CmdMeta], MarkVision))
      , ("C", ([CmdMeta], MarkSmell))
      , ("Tab", ([CmdMeta], MemberCycle))
      , ("ISO_Left_Tab", ([CmdMeta], MemberBack))
      , ("space", ([CmdMeta], Clear))
      , ("Escape", ([CmdMeta, CmdMinimal], Cancel))
      , ("Return", ([CmdMeta], Accept))

      -- Debug and others not to display in help screens
      , ("CTRL-s", ([CmdDebug], GameSave))
      , ("CTRL-f", ([CmdDebug], GameRestart "safari"))
      , ("CTRL-e", ([CmdDebug], GameRestart "defense"))
      ]
  }
