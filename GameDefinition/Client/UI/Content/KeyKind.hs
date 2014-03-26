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
      [ ("CTRL-x", (CmdMenu, GameExit))
      , ("CTRL-a", (CmdMenu, GameRestart "campaign"))
      , ("CTRL-k", (CmdMenu, GameRestart "skirmish"))
      , ("CTRL-e", (CmdMenu, GameRestart "defense"))
      , ("CTRL-d", (CmdMenu, GameDifficultyCycle))

      -- Movement and terrain alteration
      , ("less", (CmdMove, TriggerTile
           [ TriggerFeature { verb = "ascend"
                            , object = "a level"
                            , feature = F.Cause (Effect.Ascend 1) }
           , TriggerFeature { verb = "escape"
                            , object = "dungeon"
                            , feature = F.Cause (Effect.Escape 1) } ]))
      , ("CTRL-less", (CmdMove, TriggerTile
           [ TriggerFeature { verb = "ascend"
                            , object = "10 levels"
                            , feature = F.Cause (Effect.Ascend 10) } ]))
      , ("greater", (CmdMove, TriggerTile
           [ TriggerFeature { verb = "descend"
                            , object = "a level"
                            , feature = F.Cause (Effect.Ascend (-1)) }
           , TriggerFeature { verb = "escape"
                            , object = "dungeon"
                            , feature = F.Cause (Effect.Escape (-1)) } ]))
      , ("CTRL-greater", (CmdMove, TriggerTile
           [ TriggerFeature { verb = "descend"
                            , object = "10 levels"
                            , feature = F.Cause (Effect.Ascend (-10)) } ]))
      , ("semicolon", (CmdMove, StepToTarget))
      , ("colon", (CmdMove, Macro "go to target for 100 steps"
                                  ["semicolon", "P"]))
      , ("CTRL-colon", (CmdMove, Macro "go to target for 10 steps"
                                       ["semicolon", "CTRL-P"]))
      , ("x", (CmdMove, Macro "explore the closest unknown spot"
                              [ "BackSpace"
                              , "CTRL-question", "semicolon", "P" ]))
      , ("X", (CmdMove, Macro "autoexplore 100 times"
                              [ "BackSpace"
                              , "'", "CTRL-question", "semicolon", "'"
                              , "P" ]))
      , ("CTRL-X", (CmdMove, Macro "autoexplore 10 times"
                                   [ "BackSpace"
                                   , "'", "CTRL-question", "semicolon", "'"
                                   , "CTRL-P" ]))
      , ("R", (CmdMove, Macro "rest (wait 100 times)"
                              ["KP_Begin", "P"]))
      , ("CTRL-R", (CmdMove, Macro "rest (wait 10 times)"
                                   ["KP_Begin", "CTRL-P"]))
      , ("c", (CmdMove, AlterDir
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
      , ("period", (CmdMove, Macro "" ["KP_Begin"]))
      , ("i", (CmdMove, Macro "" ["KP_Begin"]))

      -- Item use
      , ("I", (CmdItem, Inventory))
      , ("g", (CmdItem, MoveItem [CGround] CEqp "get" "an item" True))
      , ("d", (CmdItem, MoveItem [CEqp, CInv] CGround "drop" "an item" False))
      , ("E", (CmdItem, Equipment))
      , ("e", (CmdItem, MoveItem [CInv, CGround] CEqp
                                 "equip" "an item" False))
      , ("s", (CmdItem, MoveItem [CEqp] CInv
                                 "stash" "and share an item" False))
      , ("G", (CmdItem, Ground))
      , ("A", (CmdItem, AllOwned))
      , ("q", (CmdItem, Apply [ApplyItem { verb = "quaff"
                                         , object = "potion"
                                         , symbol = '!' }]))
      , ("r", (CmdItem, Apply [ApplyItem { verb = "read"
                                         , object = "scroll"
                                         , symbol = '?' }]))
      , ("t", (CmdItem, Project [ApplyItem { verb = "throw"
                                           , object = "missile"
                                           , symbol = '|' }]))
      , ("z", (CmdItem, Project [ApplyItem { verb = "zap"
                                           , object = "wand"
                                           , symbol = '/' }]))

      -- Targeting
      , ("KP_Multiply", (CmdTgt, TgtEnemy))
      , ("backslash", (CmdTgt, Macro "" ["KP_Multiply"]))
      , ("slash", (CmdTgt, TgtFloor))
      , ("plus", (CmdTgt, EpsIncr True))
      , ("minus", (CmdTgt, EpsIncr False))
      , ("BackSpace", (CmdTgt, TgtClear))
      , ("CTRL-question", (CmdTgt, TgtUnknown))
      , ("CTRL-I", (CmdTgt, TgtItem))
      , ("CTRL-braceleft", (CmdTgt, TgtStair True))
      , ("CTRL-braceright", (CmdTgt, TgtStair False))

      -- Automation
      , ("equal", (CmdAuto, SelectActor))
      , ("underscore", (CmdAuto, SelectNone))
      , ("p", (CmdAuto, Repeat 1))
      , ("P", (CmdAuto, Repeat 100))
      , ("CTRL-p", (CmdAuto, Repeat 1000))
      , ("CTRL-P", (CmdAuto, Repeat 10))
      , ("apostrophe", (CmdAuto, Record))
      , ("CTRL-A", (CmdAuto, Automate))

      -- Assorted
      , ("question", (CmdMeta, Help))
      , ("D", (CmdMeta, History))
      , ("T", (CmdMeta, MarkSuspect))
      , ("V", (CmdMeta, MarkVision))
      , ("S", (CmdMeta, MarkSmell))
      , ("Tab", (CmdMeta, MemberCycle))
      , ("ISO_Left_Tab", (CmdMeta, MemberBack))
      , ("space", (CmdMeta, Clear))
      , ("Escape", (CmdMeta, Cancel))
      , ("Return", (CmdMeta, Accept))

      -- Debug and others not to display in help screens
      , ("CTRL-s", (CmdDebug, GameSave))
      , ("CTRL-y", (CmdDebug, Resend))
      ]
  }
