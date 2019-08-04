-- | The default game key-command mapping to be used for UI. Can be overridden
-- via macros in the config file.
module Client.UI.Content.Input
  ( standardKeysAndMouse
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , applyTs
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Game.LambdaHack.Client.UI.Content.Input
import           Game.LambdaHack.Client.UI.HumanCmd
import           Game.LambdaHack.Definition.Defs

-- | Description of default key-command bindings.
--
-- In addition to these commands, mouse and keys have a standard meaning
-- when navigating various menus.
standardKeysAndMouse :: InputContentRaw
standardKeysAndMouse = InputContentRaw $ map evalKeyDef $
  -- All commands are defined here, except some movement and leader picking
  -- commands. All commands are shown on help screens except debug commands
  -- and macros with empty descriptions.
  -- The order below determines the order on the help screens.
  -- Remember to put commands that show information (e.g., enter aiming
  -- mode) first.

  -- Main menu
  [ ("s", ([CmdMainMenu], "setup and start new game>", ChallengesMenu))
  , ("x", ([CmdMainMenu], "save and exit to desktop", GameExit))
  , ("v", ([CmdMainMenu], "tweak convenience settings>", SettingsMenu))
  , ("t", ([CmdMainMenu], "toggle autoplay", AutomateToggle))
  , ("?", ([CmdMainMenu], "see command help", Help))
  , ("F12", ([CmdMainMenu], "switch to dashboard", Dashboard))
  , ("Escape", ([CmdMainMenu], "back to playing", AutomateBack))

  -- Minimal command set, in the desired presentation order.
  -- A lot of these are not necessary, but may be familiar to new players.
  -- Also a few non-minimal item commands to keep proper order.
  , ("I", ( [CmdMinimal, CmdItem, CmdDashboard]
          , "manage the shared inventory stash"
          , ChooseItemMenu (MStore CStash) ))
  , ("O", ( [CmdItem, CmdDashboard]
          , "manage the equipment outfit of the pointman"
          , ChooseItemMenu (MStore CEqp) ))
  , ("g", addCmdCategory CmdMinimal $ grabItems "grab item(s)")
  , ("Escape", ( [CmdMinimal, CmdAim]
               , "open main menu/finish aiming"
               , ByAimMode AimModeCmd { exploration =
                                          ExecuteIfClear MainMenuAutoOff
                                      , aiming = Cancel } ))
  , ("C-Escape", ([CmdNoHelp], "", MainMenuAutoOn))
      -- required by frontends; not shown
  , ("Return", ( [CmdMinimal, CmdAim]
               , "open dashboard/accept target"
               , ByAimMode AimModeCmd { exploration = ExecuteIfClear Dashboard
                                      , aiming = Accept } ))
  , ("space", ( [CmdMinimal, CmdMeta]
              , "clear messages and show history"
              , ExecuteIfClear LastHistory ))
  , ("Tab", ( [CmdMove]
            , "cycle among party members on the level"
            , MemberCycle ))
      -- listed here to keep proper order
  , ("BackTab", ( [CmdMinimal, CmdMove]
              , "cycle among all party members"
              , MemberBack ))
  , ("*", ( [CmdMinimal, CmdAim]
          , "cycle crosshair among enemies"
          , AimEnemy ))
  , ("/", ([CmdMinimal, CmdAim], "cycle crosshair among items", AimItem))
  , ("m", ( [CmdMinimal, CmdMove]
          , "modify door by closing it"
          , CloseDir ))
  , ("%", ([CmdMinimal, CmdMeta], "yell/yawn and stop sleeping", Yell))

  -- Item menu, first part of item use commands
  , ("comma", grabItems "")
  , ("r", dropItems "remove item(s)")
  , ("f", addCmdCategory CmdItemMenu $ projectA flingTs)
  , ("C-f", addCmdCategory CmdItemMenu
            $ replaceDesc "auto-fling and keep choice"
            $ projectI flingTs)
  , ("t", addCmdCategory CmdItemMenu $ applyI applyTs)
  , ("C-t", addCmdCategory CmdItemMenu
            $ replaceDesc "trigger item and keep choice" $ applyIK applyTs)
  , ("i", replaceDesc "stash item into shared inventory"
          $ moveItemTriple [CGround, CEqp] CStash "item" False)
  , ("o", replaceDesc "equip item into outfit of the pointman"
          $ moveItemTriple [CGround, CStash] CEqp "item" False)

  -- Terrain exploration and modification
  , ("M", ([CmdMove], "modify any admissible terrain", AlterDir))
  , ("=", ( [CmdMove], "select (or deselect) party member", SelectActor) )
  , ("_", ([CmdMove], "deselect (or select) all on the level", SelectNone))
  , ("semicolon", ( [CmdMove]
                  , "go to crosshair for 25 steps"
                  , Macro ["C-semicolon", "C-quotedbl", "C-V"] ))
  , ("colon", ( [CmdMove]
              , "run to crosshair collectively for 25 steps"
              , Macro ["C-colon", "C-quotedbl", "C-V"] ))
  , ("[", ( [CmdMove]
          , "explore nearest unknown spot"
          , autoexploreCmd ))
  , ("]", ( [CmdMove]
          , "autoexplore 25 times"
          , autoexplore25Cmd ))
  , ("R", ([CmdMove], "rest (wait 25 times)", Macro ["KP_Begin", "C-V"]))
  , ("C-R", ( [CmdMove], "heed (lurk 0.1 turns 100 times)"
            , Macro ["C-KP_Begin", "V"] ))

  -- Remaining @ChooseItemMenu@ instances
  , ("G", ( [CmdItem, CmdDashboard]
          , "manage items on the ground"
          , ChooseItemMenu (MStore CGround) ))
  , ("T", ( [CmdItem, CmdDashboard]
          , "manage our total team belongings"
          , ChooseItemMenu MOwned ))
  , ("@", ( [CmdMeta, CmdDashboard]
          , "describe organs of the pointman"
          , ChooseItemMenu MOrgans ))
  , ("#", ( [CmdMeta, CmdDashboard]
          , "show skill summary of the pointman"
          , ChooseItemMenu MSkills ))
  , ("~", ( [CmdMeta]
          , "display known lore"
          , ChooseItemMenu (MLore SItem) ))

  -- Dashboard, in addition to commands marked above
  , ("safeD0", ([CmdInternal, CmdDashboard], "", Cancel))  -- blank line
  ]
  ++
  map (\(k, slore) -> ("safeD" ++ show (k :: Int)
                      , ( [CmdInternal, CmdDashboard]
                        , "display" <+> ppSLore slore <+> "lore"
                        , ChooseItemMenu (MLore slore) )))
      (zip [1..] [minBound..maxBound])
  ++
  [ ("safeD98", ( [CmdInternal, CmdDashboard]
                , "display place lore"
                , ChooseItemMenu MPlaces) )
  , ("safeD99", ([CmdInternal, CmdDashboard], "", Cancel))  -- blank line

  -- Aiming
  , ("+", ([CmdAim], "swerve the aiming line", EpsIncr True))
  , ("-", ([CmdAim], "unswerve the aiming line", EpsIncr False))
  , ("\\", ([CmdAim], "cycle aiming modes", AimFloor))
  , ("C-?", ( [CmdAim]
            , "set crosshair to nearest unknown spot"
            , XhairUnknown ))
  , ("C-/", ( [CmdAim]
            , "set crosshair to nearest item"
            , XhairItem ))
  , ("C-{", ( [CmdAim]
            , "aim at nearest upstairs"
            , XhairStair True ))
  , ("C-}", ( [CmdAim]
            , "aim at nearest downstairs"
            , XhairStair False ))
  , ("<", ([CmdAim], "move aiming one level up" , AimAscend 1))
  , ("C-<", ( [CmdNoHelp], "move aiming 10 levels up"
            , AimAscend 10) )
  , (">", ([CmdAim], "move aiming one level down", AimAscend (-1)))
      -- 'lower' would be misleading in some games, just as 'deeper'
  , ("C->", ( [CmdNoHelp], "move aiming 10 levels down"
            , AimAscend (-10)) )
  , ("BackSpace" , ( [CmdAim]
                   , "clear chosen item and crosshair"
                   , ComposeUnlessError ClearTargetIfItemClear ItemClear))

  -- Assorted (first few cloned from main menu)
  , ("C-s", ([CmdMeta], "start new game", GameRestart))
  , ("C-x", ([CmdMeta], "save and exit to desktop", GameExit))
  , ("C-q", ([CmdMeta], "quit game and start autoplay", GameQuit))
  , ("C-c", ([CmdMeta], "exit to desktop without saving", GameDrop))
  , ("?", ([CmdMeta], "display help", Hint))
  , ("F1", ([CmdMeta, CmdDashboard], "display help immediately", Help))
  , ("F12", ([CmdMeta], "open dashboard", Dashboard))
  , ("v", ([CmdMeta], "voice again the recorded commands", Repeat 1))
  , ("V", repeatTriple 100)
  , ("C-v", addCmdCategory CmdNoHelp $ replaceDesc "" $ repeatTriple 1000)
  , ("C-V", addCmdCategory CmdNoHelp $ replaceDesc "" $ repeatTriple 25)
  , ("'", ([CmdMeta], "start recording commands", Record))
  , ("C-S", ([CmdMeta], "save game backup", GameSave))
  , ("C-P", ([CmdMeta], "print screen", PrintScreen))

  -- Dashboard, in addition to commands marked above
  , ("safeD101", ([CmdInternal, CmdDashboard], "display history", AllHistory))

  -- Mouse
  , ( "LeftButtonRelease"
    , mouseLMB goToCmd
               "go to pointer for 25 steps/fling at enemy" )
  , ( "S-LeftButtonRelease"
    , mouseLMB runToAllCmd
               "run to pointer collectively for 25 steps/fling at enemy" )
  , ("RightButtonRelease", mouseRMB)
  , ("C-LeftButtonRelease", replaceDesc "" mouseRMB)  -- Mac convention
  , ( "S-RightButtonRelease"
    , ([CmdMouse], "modify terrain at pointer", AlterWithPointer) )
  , ("MiddleButtonRelease", mouseMMB)
  , ("C-RightButtonRelease", replaceDesc "" mouseMMB)
  , ( "C-S-LeftButtonRelease",
      addCmdCategory CmdNoHelp $ replaceDesc "" mouseMMB )
  , ("WheelNorth", ([CmdMouse], "swerve the aiming line", Macro ["+"]))
  , ("WheelSouth", ([CmdMouse], "unswerve the aiming line", Macro ["-"]))

  -- Debug and others not to display in help screens
  , ("C-semicolon", ( [CmdNoHelp]
                    , "move one step towards the crosshair"
                    , MoveOnceToXhair ))
  , ("C-colon", ( [CmdNoHelp]
                , "run collectively one step towards the crosshair"
                , RunOnceToXhair ))
  , ("C-quotedbl", ( [CmdNoHelp]
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
              , "pick new pointman on screen"
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
  , ("safe7", ( [CmdInternal, CmdDashboard]
              , "open main menu"
              , MainMenuAutoOff ))
  , ("safe8", ( [CmdInternal]
              , "cancel aiming"
              , Cancel ))
  , ("safe9", ( [CmdInternal]
              , "accept target"
              , Accept ))
  , ("safe10", ( [CmdInternal]
               , "wait a turn, bracing for impact"
               , Wait ))
  , ("safe11", ( [CmdInternal]
               , "lurk 0.1 of a turn"
               , Wait10 ))
  , ("safe12", ( [CmdInternal]
               , "snap crosshair to enemy"
               , XhairPointerEnemy ))
  ]
  ++ map defaultHeroSelect [0..9]

applyTs :: [TriggerItem]
applyTs = [TriggerItem { tiverb = "trigger"
                       , tiobject = "consumable item"
                       , tisymbols = "!,?/" }]
