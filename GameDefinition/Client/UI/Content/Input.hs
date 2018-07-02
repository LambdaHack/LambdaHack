-- | The default game key-command mapping to be used for UI. Can be overridden
-- via macros in the config file.
module Client.UI.Content.Input
  ( standardKeysAndMouse
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Game.LambdaHack.Client.UI.Content.Input
import           Game.LambdaHack.Client.UI.HandleHelperM (ppSLore)
import           Game.LambdaHack.Client.UI.HumanCmd
import           Game.LambdaHack.Common.Misc
import qualified Game.LambdaHack.Content.TileKind as TK

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
  [ ("e", ([CmdMainMenu], "enter challenges menu>", ChallengesMenu))
  , ("s", ([CmdMainMenu], "start new game", GameRestart))
  , ("x", ([CmdMainMenu], "exit to desktop", GameExit))
  , ("v", ([CmdMainMenu], "visit settings menu>", SettingsMenu))
  , ("a", ([CmdMainMenu], "automate faction", Automate))
  , ("?", ([CmdMainMenu], "see command help", Help))
  , ("F12", ([CmdMainMenu], "go to dashboard", Dashboard))
  , ("Escape", ([CmdMainMenu], "back to playing", Cancel))

  -- Minimal command set, in the desired presentation order
  , ("g", addCmdCategory CmdMinimal $ grabItems "grab item(s)")
      -- actually it's not necessary, ground items menu suffices
  , ("Escape", ( [CmdMinimal, CmdAim]
               , "cancel aiming/open main menu"
               , ByAimMode { exploration = ExecuteIfClear MainMenu
                           , aiming = Cancel } ))
  , ("Return", ( [CmdMinimal, CmdAim]
               , "accept target/open dashboard"
               , ByAimMode { exploration = ExecuteIfClear Dashboard
                           , aiming = Accept } ))
  , ("space", ( [CmdMinimal, CmdMeta]
              , "clear messages/display history"
              , ExecuteIfClear History ))
      -- not necessary, because messages available from dashboard
  , ("BackTab", ( [CmdMinimal, CmdMove]
              , "cycle among all party members"
              , MemberBack ))
  , ("KP_Multiply", ( [CmdMinimal, CmdAim]
                    , "cycle x-hair among enemies"
                    , AimEnemy ))
      -- not necessary, because flinging from item menu enters aiming mode
  , ("C-c", ([CmdMinimal, CmdMove], "open or close or alter", AlterDir []))
  , ("+", ([CmdMinimal, CmdAim], "swerve the aiming line", EpsIncr True))

  -- Item menu, first part of item use commands
  , ("comma", grabItems "")
  , ("d", dropItems "drop item(s)")
  , ("period", dropItems "")
  , ("f", addCmdCategory CmdItemMenu $ projectA flingTs)
  , ("C-f", addCmdCategory CmdItemMenu
            $ replaceDesc "fling without aiming" $ projectI flingTs)
  , ("a", addCmdCategory CmdItemMenu $ applyI [TriggerItem
            { tiverb = "apply"
            , tiobject = "consumable"
            , tisymbols = "!?/" }])
  , ("C-a", addCmdCategory CmdItemMenu
            $ replaceDesc "apply and keep choice" $ applyIK [TriggerItem
              { tiverb = "apply"
              , tiobject = "consumable"
              , tisymbols = "!?/" }])
  , ("p", moveItemTriple [CGround, CEqp, CSha] CInv
                         "item" False)
  , ("e", moveItemTriple [CGround, CInv, CSha] CEqp
                         "item" False)
  , ("s", moveItemTriple [CGround, CInv, CEqp] CSha
                         "and share item" False)

  -- Terrain exploration and alteration
  , ("Tab", ( [CmdMove]
            , "cycle among party members on the level"
            , MemberCycle ))
  , ("c", ([CmdMove], descTs closeDoorTriggers, AlterDir closeDoorTriggers))
  , ("=", ( [CmdMove], "select (or deselect) party member", SelectActor) )
  , ("_", ([CmdMove], "deselect (or select) all on the level", SelectNone))
  , ("semicolon", ( [CmdMove]
                  , "go to x-hair for 25 steps"
                  , Macro ["C-semicolon", "C-/", "C-V"] ))
  , ("colon", ( [CmdMove]
              , "run to x-hair collectively for 25 steps"
              , Macro ["C-colon", "C-/", "C-V"] ))
  , ("x", ( [CmdMove]
          , "explore nearest unknown spot"
          , autoexploreCmd ))
  , ("X", ( [CmdMove]
          , "autoexplore 25 times"
          , autoexplore25Cmd ))
  , ("R", ([CmdMove], "rest (wait 25 times)", Macro ["KP_5", "C-V"]))
  , ("C-R", ( [CmdMove], "lurk (wait 0.1 turns 100 times)"
            , Macro ["C-KP_5", "V"] ))

  -- Item use, continued
  , ("^", ( [CmdItem], "sort items by ownership, kind and stats", SortSlots))
  , ("P", ( [CmdItem, CmdDashboard]
          , "manage item pack of the leader"
          , ChooseItemMenu (MStore CInv) ))
  , ("G", ( [CmdItem, CmdDashboard]
          , "manage items on the ground"
          , ChooseItemMenu (MStore CGround) ))
  , ("E", ( [CmdItem, CmdDashboard]
          , "manage equipment of the leader"
          , ChooseItemMenu (MStore CEqp) ))
  , ("S", ( [CmdItem, CmdDashboard]
          , "manage the shared party stash"
          , ChooseItemMenu (MStore CSha) ))
  , ("A", ( [CmdItem, CmdDashboard]
          , "manage all owned items"
          , ChooseItemMenu MOwned ))
  , ("@", ( [CmdItem, CmdDashboard]
          , "describe organs of the leader"
          , ChooseItemMenu MOrgans ))
  , ("#", ( [CmdItem, CmdDashboard]
          , "show stat summary of the leader"
          , ChooseItemMenu MStats ))
  , ("~", ( [CmdItem]
          , "display known lore"
          , ChooseItemMenu (MLore SItem) ))
  , ("q", addCmdCategory CmdItem $ applyI [TriggerItem
            { tiverb = "quaff"
            , tiobject = "potion"
            , tisymbols = "!" }])
  , ("r", addCmdCategory CmdItem $ applyI [TriggerItem
            { tiverb = "read"
            , tiobject = "scroll"
            , tisymbols = "?" }])

  , ("t", addCmdCategory CmdItem $ projectA
            [ TriggerItem { tiverb = "throw"
                          , tiobject = "missile"
                          , tisymbols = "|" } ])
--  , ("z", projectA [TriggerItem { tiverb = "zap"
--                                , tiobject = "instrument"
--                                , tisymbol = "/" }])

  -- Dashboard, in addition to commands marked above
  , ("safeD0", ([CmdInternal, CmdDashboard], "", Cancel))  -- blank line
  ]
  ++
  map (\(k,  slore) -> ("safeD" ++ show (k :: Int)
                       , ( [CmdInternal, CmdDashboard]
                         , "display" <+> ppSLore slore <+> "lore"
                         , ChooseItemMenu (MLore slore) )))
      (zip [1..] [minBound..maxBound])
  ++
  [ ("safeD99", ([CmdInternal, CmdDashboard], "", Cancel))  -- blank line

  -- Aiming
  , ("!", ([CmdAim], "", AimEnemy))
  , ("KP_Divide", ([CmdAim], "cycle x-hair among items", AimItem))
  , ("/", ([CmdAim], "", AimItem))
  , ("-", ([CmdAim], "unswerve the aiming line", EpsIncr False))
  , ("\\", ([CmdAim], "cycle aiming modes", AimFloor))
  , ("C-?", ( [CmdAim]
            , "set x-hair to nearest unknown spot"
            , XhairUnknown ))
  , ("C-I", ( [CmdAim]
            , "set x-hair to nearest item"
            , XhairItem ))
  , ("C-{", ( [CmdAim]
            , "set x-hair to nearest upstairs"
            , XhairStair True ))
  , ("C-}", ( [CmdAim]
            , "set x-hair to nearest dnstairs"
            , XhairStair False ))
  , ("<", ([CmdAim], "move aiming one level up" , AimAscend 1))
  , ("C-<", ( [CmdNoHelp], "move aiming 10 levels up"
            , AimAscend 10) )
  , (">", ([CmdAim], "move aiming one level down", AimAscend (-1)))
      -- 'lower' would be misleading in some games, just as 'deeper'
  , ("C->", ( [CmdNoHelp], "move aiming 10 levels down"
            , AimAscend (-10)) )
  , ("BackSpace" , ( [CmdAim]
                   , "clear chosen item and target"
                   , ComposeUnlessError ItemClear TgtClear ))

  -- Assorted
  , ("F12", ([CmdMeta], "open dashboard", Dashboard))
  , ("?", ([CmdMeta], "display help", Hint))
  , ("F1", ([CmdMeta], "", Hint))
  , ("v", ([CmdMeta], "voice again the recorded commands", Repeat 1))
  , ("V", repeatTriple 100)
  , ("C-v", repeatTriple 1000)
  , ("C-V", repeatTriple 25)
  , ("'", ([CmdMeta], "start recording commands", Record))
  , ("C-P", ([CmdMeta], "print screen", PrintScreen))

  -- Dashboard, in addition to commands marked above
  , ("safeD100", ([CmdInternal, CmdDashboard], "display help", Help))
  , ("safeD101", ([CmdInternal, CmdDashboard], "display history", History))

  -- Mouse
  , ("LeftButtonRelease", mouseLMB)
  , ("RightButtonRelease", mouseRMB)
  , ("C-LeftButtonRelease", replaceDesc "" mouseRMB)  -- Mac convention
  , ( "C-RightButtonRelease"
    , ([CmdMouse], "open or close or alter at pointer", AlterWithPointer []) )
  , ("MiddleButtonRelease", mouseMMB)
  , ("WheelNorth", ([CmdMouse], "swerve the aiming line", Macro ["+"]))
  , ("WheelSouth", ([CmdMouse], "unswerve the aiming line", Macro ["-"]))

  -- Debug and others not to display in help screens
  , ("C-S", ([CmdDebug], "save game", GameSave))
  , ("C-semicolon", ( [CmdNoHelp]
                    , "move one step towards the x-hair"
                    , MoveOnceToXhair ))
  , ("C-colon", ( [CmdNoHelp]
                , "run collectively one step towards the x-hair"
                , RunOnceToXhair ))
  , ("C-/", ( [CmdNoHelp]
            , "continue towards the x-hair"
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
              , "set x-hair to enemy"
              , AimPointerEnemy ))
  , ("safe6", ( [CmdInternal]
              , "fling at enemy under pointer"
              , aimFlingCmd ))
  , ("safe7", ( [CmdInternal, CmdDashboard]
              , "open main menu"
              , MainMenu ))
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
               , "wait 0.1 of a turn"
               , Wait10 ))
  ]
  ++ map defaultHeroSelect [0..6]

closeDoorTriggers :: [TriggerTile]
closeDoorTriggers =
  [ TriggerTile { ttverb = "close"
                , ttobject = "door"
                , ttfeature = TK.CloseTo "closed vertical door Lit" }
  , TriggerTile { ttverb = "close"
                , ttobject = "door"
                , ttfeature = TK.CloseTo "closed horizontal door Lit" }
  , TriggerTile { ttverb = "close"
                , ttobject = "door"
                , ttfeature = TK.CloseTo "closed vertical door Dark" }
  , TriggerTile { ttverb = "close"
                , ttobject = "door"
                , ttfeature = TK.CloseTo "closed horizontal door Dark" }
  ]
