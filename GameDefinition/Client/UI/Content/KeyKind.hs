-- | The default game key-command mapping to be used for UI. Can be overridden
-- via macros in the config file.
module Client.UI.Content.KeyKind
  ( standardKeys
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

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
      -- Remember to put commands that show information (e.g., enter aiming
      -- mode) first.

      -- Main Menu
      [ ("Escape", ([CmdMainMenu], "back to playing", Cancel))
      , ("?", ([CmdMainMenu], "see command help", Help))
      , ("S", ([CmdMainMenu], "enter settings menu", SettingsMenu))
      , ("X", ([CmdMainMenu], "save and exit", GameExit))
      , ("r", gameRestartTriple "raid")
      , ("s", gameRestartTriple "skirmish")
      , ("a", gameRestartTriple "ambush")
      , ("b", gameRestartTriple "battle")
      , ("c", gameRestartTriple "campaign")
      , ("i", addCmdCategory CmdDebug $ gameRestartTriple "battle survival")
      , ("f", addCmdCategory CmdDebug $ gameRestartTriple "safari")
      , ("u", addCmdCategory CmdDebug $ gameRestartTriple "safari survival")
      , ("d", addCmdCategory CmdDebug $ gameRestartTriple "defense")
      , ("g", addCmdCategory CmdDebug $ gameRestartTriple "boardgame")
      , ("D", ([CmdMainMenu], "cycle next difficulty", GameDifficultyIncr))
      , ("A", ([CmdMainMenu], "automate faction", Automate))

      -- Settings Menu  -- TODO: add some from ClientOptions
      , ("Escape", ([CmdSettingsMenu], "back to Main Menu", MainMenu))
      , ("T", ([CmdSettingsMenu], "cycle henchmen tactic", Tactic))
      , ("S", ([CmdSettingsMenu], "toggle suspect terrain", MarkSuspect))
      , ("V", ([CmdSettingsMenu], "toggle visible zone", MarkVision))
      , ("C", ([CmdSettingsMenu], "toggle smell clues", MarkSmell))

      -- Item use, 1st part
      , ("f", addCmdCategory CmdItemMenu $ projectA flingTs)
      , ("CTRL-f", replaceDesc "fling without aiming" $ projectI flingTs)
      , ("a", addCmdCategory CmdItemMenu $ applyI [ApplyItem
                { verb = "apply"
                , object = "consumable"
                , symbol = ' ' }])

      -- Movement and terrain alteration
      , ("<", addCmdCategory CmdItemMenu $ addCmdCategory CmdMinimal
              $ grabAscend "grab items or ascend")
      , ("g", grabAscend "")
      , ("comma", addCmdCategory CmdInternal $ grabAscend "")
      , let triggerAscend10 =
              [TriggerFeature { verb = "ascend"
                              , object = "10 levels"
                              , feature = TK.Cause (IK.Ascend 10) }]
        in ("CTRL-<", ([CmdInternal], descTs triggerAscend10 , ByAimMode
              { notAiming = TriggerTile triggerAscend10
              , aiming = AimAscend 10 }))
      , (">", addCmdCategory CmdItemMenu $ addCmdCategory CmdMinimal
              $ descendDrop "descend or drop items")
      , ("d", descendDrop "")
      , ("period", addCmdCategory CmdInternal $ descendDrop "")
      , let triggerAscendMinus10 =
              [TriggerFeature { verb = "descend"
                              , object = "10 levels"
                              , feature = TK.Cause (IK.Ascend (-10)) }]
        in ("CTRL->", ([CmdInternal], descTs triggerAscendMinus10, ByAimMode
             { notAiming = TriggerTile triggerAscendMinus10
             , aiming = AimAscend (-10) }))
      , ("semicolon", ( [CmdMove]
                      , "go to crosshair for 100 steps"
                      , Macro ["CTRL-semicolon", "CTRL-period", "V"] ))
      , ("colon", ( [CmdMove]
                  , "run selected to crosshair for 100 steps"
                  , Macro ["CTRL-colon", "CTRL-period", "V"] ))
      , ("x", ( [CmdMove]
              , "explore the closest unknown spot"
              , Macro [ "CTRL-?"  -- no semicolon
                      , "CTRL-period", "V" ] ))
      , ("X", ( [CmdMove]
              , "autoexplore 100 times"
              , Macro  ["'", "CTRL-?", "CTRL-period", "'", "V"] ))
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
      , ("e", addCmdCategory CmdItemMenu
              $ moveItemTriple [CGround, CInv, CSha] CEqp Nothing
                               "item" False)
      , ("p", addCmdCategory CmdItemMenu
              $ moveItemTriple [CGround, CEqp, CSha] CInv Nothing
                               "item into inventory" False)
      , ("s", addCmdCategory CmdItemMenu
              $ moveItemTriple [CGround, CInv, CEqp] CSha Nothing
                               "and share item" False)
      , ("E", ( [CmdItem]
              , "manage equipment of the leader"
              , ChooseItemMenu (MStore CEqp) ))
      , ("P", ( [CmdMinimal, CmdItem]
              , "manage inventory pack of the leader"
              , ChooseItemMenu (MStore CInv) ))
      , ("S", ( [CmdItem]
              , "manage the shared party stash"
              , ChooseItemMenu (MStore CSha) ))
      , ("A", ( [CmdItem]
              , "manage all owned items"
              , ChooseItemMenu MOwned ))
      , ("G", ( [CmdItem]
              , "manage items on the ground"
              , ChooseItemMenu (MStore CGround) ))
      , ("@", ( [CmdItem]
              , "describe organs of the leader"
              , ChooseItemMenu (MStore COrgan) ))
      , ("#", ( [CmdItem]
              , "show the stats summary of the leader"
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
      , ("KP_Multiply", ([CmdAim], "aim at an enemy", AimEnemy))
      , ("!", ([CmdAim], "", AimEnemy))
      , ("KP_Divide", ([CmdAim], "cycle aiming styles", AimFloor))
      , ("/", ([CmdAim], "", AimFloor))
      , ("+", ([CmdAim, CmdMinimal], "swerve the aiming line", EpsIncr True))
      , ("-", ([CmdAim], "unswerve the aiming line", EpsIncr False))
      , ("CTRL-?", ( [CmdAim]
                   , "set crosshair to the closest unknown spot"
                   , XhairUnknown ))
      , ("CTRL-I", ( [CmdAim]
                   , "set crosshair to the closest item"
                   , XhairItem ))
      , ("CTRL-{", ( [CmdAim]
                   , "set crosshair to the closest stairs up"
                   , XhairStair True ))
      , ("CTRL-}", ( [CmdAim]
                   , "set crosshair to the closest stairs down"
                   , XhairStair False ))
      , ( "BackSpace"
        , ([CmdAim], "clear target/crosshair/chosen item", TgtClear) )
      , ("Escape", ( [CmdAim, CmdMinimal]
                   , "cancel target/action or open Main Menu"
                   , ByAimMode {notAiming = MainMenu, aiming = Cancel} ))
      , ("Return", ( [CmdAim, CmdMinimal]
                   , "accept target/choice or open Help"
                   , ByAimMode {notAiming = Help, aiming=Accept} ))

      -- Assorted
      , ("space", ([CmdMeta], "clear/display hints and history", Clear))
      , ("?", ([CmdMeta], "display help", Help))
      , ("Tab", ( [CmdMeta]
                , "cycle among party members on the level"
                , MemberCycle ))
      , ("ISO_Left_Tab", ( [CmdMeta, CmdMinimal]
                         , "cycle among all party members"
                         , MemberBack ))
      , ("=", ([CmdMeta], "select (or deselect) a party member", SelectActor))
      , ("_", ([CmdMeta], "deselect (or select) all on the level", SelectNone))
      , ("v", ([CmdMeta], "voice again the recorded commands", Repeat 1))
      , ("V", repeatTriple 100)
      , ("CTRL-v", repeatTriple 1000)
      , ("CTRL-V", repeatTriple 25)
      , ("'", ([CmdMeta], "start recording commands", Record))

      -- Mouse
      -- Doubleclick acts as RMB and modifiers as MMB, which is optional.
      , ("LeftButtonRelease", defaultCmdLMB)
      , ( "LeftDblClick"
        , replaceDesc "" $ addCmdCategory CmdInternal defaultCmdRMB )
      , ( "SHIFT-LeftButtonRelease"
        , replaceDesc "" $ addCmdCategory CmdInternal defaultCmdMMB )
      , ( "CTRL-LeftButtonRelease"
        , replaceDesc "" $ addCmdCategory CmdInternal defaultCmdMMB )
      , ("MiddleButtonRelease", defaultCmdMMB)
      , ("RightButtonRelease", defaultCmdRMB)

      -- Debug and others not to display in help screens
      , ("CTRL-S", ([CmdDebug], "save game", GameSave))
      , ("CTRL-semicolon", ( [CmdInternal]
                           , "move one step towards the crosshair"
                           , MoveOnceToXhair ))
      , ("CTRL-colon", ( [CmdInternal]
                       , "run selected one step towards the crosshair"
                       , RunOnceToXhair ))
      , ("CTRL-period", ( [CmdInternal]
                        , "continue towards the crosshair"
                        , ContinueToXhair ))
      , ("CTRL-comma", ([CmdInternal], "run once ahead", RunOnceAhead))
      ]
      ++ map defaultHeroSelect [0..6]
  }
