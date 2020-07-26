-- | The type of definitions of key-command mappings to be used for the UI
-- and shorthands for specifying command triples in the content files.
module Game.LambdaHack.Client.UI.Content.Input
  ( InputContentRaw(..), InputContent(..), makeData
  , evalKeyDef
  , addCmdCategory, replaceDesc, moveItemTriple, repeatTriple, repeatLastTriple
  , mouseLMB, mouseMMB, mouseRMB
  , goToCmd, runToAllCmd, autoexploreCmd, autoexplore25Cmd
  , aimFlingCmd, projectI, projectA, flingTs, applyIK, applyI
  , grabItems, dropItems, descIs, defaultHeroSelect, macroRun25
  , memberCycle, memberCycleLevel
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , replaceCmd, projectICmd, grabCmd, dropCmd
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Char as Char
import qualified Data.Map.Strict as M
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Definition.Defs

-- | Key-command mappings to be specified in content and used for the UI.
newtype InputContentRaw = InputContentRaw [(K.KM, CmdTriple)]

-- | Bindings and other information about human player commands.
data InputContent = InputContent
  { bcmdMap  :: M.Map K.KM CmdTriple   -- ^ binding of keys to commands
  , bcmdList :: [(K.KM, CmdTriple)]    -- ^ the properly ordered list
                                       --   of commands for the help menu
  , brevMap  :: M.Map HumanCmd [K.KM]  -- ^ and from commands to their keys
  }

-- | Create binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
makeData :: Maybe UIOptions   -- ^ UI client options
         -> InputContentRaw  -- ^ default key bindings from the content
         -> InputContent     -- ^ concrete binding
makeData muiOptions (InputContentRaw copsClient) =
  let (uCommands0, uVi0, uLeftHand0) = case muiOptions of
        Just UIOptions{uCommands, uVi, uLeftHand} -> (uCommands, uVi, uLeftHand)
        Nothing -> ([], True, True)
      waitTriple = ([CmdMove], "", Wait)
      wait10Triple = ([CmdMove], "", Wait10)
      moveXhairOr n cmd v = ByAimMode $ AimModeCmd { exploration = cmd v
                                                   , aiming = MoveXhair v n }
      isMainMenu (_, ([CmdMainMenu], _, _)) = True
      isMainMenu _ = False
      rawContent = copsClient ++ uCommands0
      (rawContentMainMenu, rawContentNoMainMenu) =
        partition isMainMenu rawContent
      filteredContent =
        rawContentMainMenu
        ++ ((if uVi0
             then filter (\(k, _) ->
                    k `notElem` [K.mkKM "period", K.mkKM "C-period"])
             else id)
             $ (if uLeftHand0
                then filter (\(k, _) ->
                       k `notElem` [K.mkKM "s", K.mkKM "S"])
                else id)
             $ filter (\(k, _) ->
                 k `notElem` map (K.KM K.NoModifier)
                                 (K.dirMoveNoModifier uVi0 uLeftHand0)
                             ++ map (K.KM K.NoModifier)
                                    (K.dirRunNoModifier uVi0 uLeftHand0)
                             ++ map (K.KM K.Control) K.dirRunControl
                             ++ map (K.KM K.Shift) K.dirRunShift
                             ++ map K.mkKM [ "KP_Begin", "C-KP_Begin"
                                           , "KP_5", "C-KP_5" ])
             $ rawContentNoMainMenu)
      bcmdList =
        -- Users are free to overwrite commands, but at least the defaults
        -- should be non-overlapping with the movement keys.
#ifdef WITH_EXPENSIVE_ASSERTIONS
        assert (rawContentMainMenu ++ rawContentNoMainMenu == filteredContent
                `blame` "duplicate keys"
                `swith` (rawContentMainMenu ++ rawContentNoMainMenu)
                        \\ filteredContent) $
#endif
          filteredContent
          ++ (if uVi0
              then [ (K.mkKM "period", waitTriple)
                   , (K.mkKM "C-period", wait10Triple) ]
              else [])
          ++ (if uLeftHand0
              then [ (K.mkKM "s", waitTriple)
                   , (K.mkKM "S", wait10Triple) ]
              else [])
          ++ [ (K.mkKM "KP_Begin", waitTriple)
             , (K.mkKM "C-KP_Begin", wait10Triple)
             , (K.mkKM "KP_5", wait10Triple)
             , (K.mkKM "C-KP_5", wait10Triple) ]
          ++ K.moveBinding uVi0 uLeftHand0
               (\v -> ([CmdMove], "", moveXhairOr 1 MoveDir v))
               (\v -> ([CmdMove], "", moveXhairOr 10 RunDir v))
      -- This catches repetitions inside input content definitions.
      rejectRepetitions t1 t2 = error $ "duplicate key"
                                        `showFailure` (t1, t2)
  in InputContent
  { bcmdMap = M.fromListWith rejectRepetitions
      [ (k, triple)
      | (k, triple@(cats, _, _)) <- bcmdList
      , all (`notElem` [CmdMainMenu]) cats
      ]
  , bcmdList
  , brevMap = M.fromListWith (flip (++)) $ concat
      [ [(cmd, [k])]
      | (k, (cats, _desc, cmd)) <- bcmdList
      , all (`notElem` [CmdMainMenu, CmdDebug, CmdNoHelp]) cats
      ]
  }

evalKeyDef :: (String, CmdTriple) -> (K.KM, CmdTriple)
evalKeyDef (t, triple@(cats, _, _)) =
  let km = if CmdInternal `elem` cats
           then K.KM K.NoModifier $ K.Unknown t
           else K.mkKM t
  in (km, triple)

addCmdCategory :: CmdCategory -> CmdTriple -> CmdTriple
addCmdCategory cat (cats, desc, cmd) = (cat : cats, desc, cmd)

replaceDesc :: Text -> CmdTriple -> CmdTriple
replaceDesc desc (cats, _, cmd) = (cats, desc, cmd)

replaceCmd :: HumanCmd -> CmdTriple -> CmdTriple
replaceCmd cmd (cats, desc, _) = (cats, desc, cmd)

moveItemTriple :: [CStore] -> CStore -> MU.Part -> Bool -> CmdTriple
moveItemTriple stores1 store2 object auto =
  let verb = MU.Text $ verbCStore store2
      desc = makePhrase [verb, object]
  in ([CmdItemMenu, CmdItem], desc, MoveItem stores1 store2 Nothing auto)

repeatTriple :: Int -> CmdTriple
repeatTriple n = ( [CmdMeta]
                 , if n == 1
                   then "voice recorded macro again"
                   else "voice recorded macro" <+> tshow n <+> "times"
                 , Repeat n )

repeatLastTriple :: Int -> CmdTriple
repeatLastTriple n = ( [CmdMeta]
                     , if n == 1
                       then "voice last action again"
                       else "voice last action" <+> tshow n <+> "times in a row"
                     , RepeatLast n )

-- @AimFloor@ is not there, but @AimEnemy@ and @AimItem@ almost make up for it.
mouseLMB :: HumanCmd -> Text -> CmdTriple
mouseLMB goToOrRunTo desc =
  ([CmdMouse], desc, ByAimMode aimMode)
 where
  aimMode = AimModeCmd
    { exploration = ByArea $ common ++  -- exploration mode
        [ (CaMapLeader, grabCmd)
        , (CaMapParty, PickLeaderWithPointer)
        , (CaMap, goToOrRunTo)
        , (CaArenaName, Dashboard)
        , (CaPercentSeen, autoexploreCmd) ]
    , aiming = ByArea $ common ++  -- aiming mode
        [ (CaMap, aimFlingCmd)
        , (CaArenaName, Accept)
        , (CaPercentSeen, XhairStair True) ] }
  common =
    [ (CaMessage, ExecuteIfClear LastHistory)
    , (CaLevelNumber, AimAscend 1)
    , (CaXhairDesc, AimEnemy)  -- inits aiming and then cycles enemies
    , (CaSelected, PickLeaderWithPointer)
--    , (CaCalmGauge, Macro ["KP_Begin", "C-v"])
    , (CaCalmValue, Yell)
    , (CaHPGauge, Macro ["KP_Begin", "C-v"])
    , (CaHPValue, Wait)
    , (CaLeaderDesc, projectICmd flingTs) ]

mouseMMB :: CmdTriple
mouseMMB = ( [CmdMouse]
           , "snap crosshair to floor under pointer"
           , XhairPointerFloor )

mouseRMB :: CmdTriple
mouseRMB = ( [CmdMouse]
           , "start aiming at enemy under pointer"
           , ByAimMode aimMode )
 where
  aimMode = AimModeCmd
    { exploration = ByArea $ common ++
        [ (CaMapLeader, dropCmd)
        , (CaMapParty, SelectWithPointer)
        , (CaMap, AimPointerEnemy)
        , (CaArenaName, MainMenuAutoOff)
        , (CaPercentSeen, autoexplore25Cmd) ]
    , aiming = ByArea $ common ++
        [ (CaMap, XhairPointerEnemy)  -- hack; same effect, but matches LMB
        , (CaArenaName, Cancel)
        , (CaPercentSeen, XhairStair False) ] }
  common =
    [ (CaMessage, Hint)
    , (CaLevelNumber, AimAscend (-1))
    , (CaXhairDesc, AimItem)
    , (CaSelected, SelectWithPointer)
--    , (CaCalmGauge, Macro ["C-KP_Begin", "A-v"])
    , (CaCalmValue, Yell)
    , (CaHPGauge, Macro ["C-KP_Begin", "A-v"])
    , (CaHPValue, Wait10)
    , (CaLeaderDesc, ComposeUnlessError ClearTargetIfItemClear ItemClear) ]

-- This is duplicated wrt content, instead of included via @semicolon@,
-- because the C- commands are less likely to be modified by the player.
goToCmd :: HumanCmd
goToCmd = Macro ["MiddleButtonRelease", "C-semicolon", "C-quotedbl", "C-v"]

-- This is duplicated wrt content, instead of included via @colon@,
-- because the C- commands are less likely to be modified by the player.
runToAllCmd :: HumanCmd
runToAllCmd = Macro ["MiddleButtonRelease", "C-colon", "C-quotedbl", "C-v"]

autoexploreCmd :: HumanCmd
autoexploreCmd = Macro ["C-?", "C-quotedbl", "C-v"]

autoexplore25Cmd :: HumanCmd
autoexplore25Cmd = Macro ["'", "C-?", "C-quotedbl", "'", "C-V"]

aimFlingCmd :: HumanCmd
aimFlingCmd = ComposeIfLocal AimPointerEnemy (projectICmd flingTs)

projectICmd :: [TriggerItem] -> HumanCmd
projectICmd ts = ComposeUnlessError (ChooseItemProject ts) Project

projectI :: [TriggerItem] -> CmdTriple
projectI ts = ([CmdItem], descIs ts, projectICmd ts)

projectA :: [TriggerItem] -> CmdTriple
projectA ts =
  let fling = Compose2ndLocal Project ItemClear
      flingICmd = ComposeUnlessError (ChooseItemProject ts) fling
  in replaceCmd (ByAimMode AimModeCmd { exploration = AimTgt
                                      , aiming = flingICmd })
                (projectI ts)

flingTs :: [TriggerItem]
flingTs = [TriggerItem { tiverb = "start flinging"
                       , tiobject = "projectiles"
                       , tisymbols = "" }]

applyIK :: [TriggerItem] -> CmdTriple
applyIK ts =
  ([CmdItem], descIs ts, ComposeUnlessError (ChooseItemApply ts) Apply)

applyI :: [TriggerItem] -> CmdTriple
applyI ts =
  let apply = Compose2ndLocal Apply ItemClear
  in ([CmdItem], descIs ts, ComposeUnlessError (ChooseItemApply ts) apply)

grabCmd :: HumanCmd
grabCmd = MoveItem [CGround] CStash (Just "grab") True
            -- @CStash@ is the implicit default; refined in HandleHumanGlobalM

grabItems :: Text -> CmdTriple
grabItems t = ([CmdItemMenu, CmdItem], t, grabCmd)

dropCmd :: HumanCmd
dropCmd = MoveItem [CStash, CEqp] CGround Nothing False

dropItems :: Text -> CmdTriple
dropItems t = ([CmdItemMenu, CmdItem], t, dropCmd)

descIs :: [TriggerItem] -> Text
descIs [] = "trigger an item"
descIs (t : _) = makePhrase [tiverb t, tiobject t]

defaultHeroSelect :: Int -> (String, CmdTriple)
defaultHeroSelect k = ([Char.intToDigit k], ([CmdMeta], "", PickLeader k))

macroRun25 :: [String]
macroRun25 = ["C-comma", "C-v"]

memberCycle :: Direction -> [CmdCategory] -> CmdTriple
memberCycle d cats = ( cats
                     , "cycle"
                       <+> (if d == Backward then "backwards" else "")
                       <+> "among all party members"
                     , MemberCycle d )

memberCycleLevel :: Direction -> [CmdCategory] -> CmdTriple
memberCycleLevel d cats = ( cats
                          , "cycle"
                            <+> (if d == Backward then "backwards" else "")
                            <+> " among party members on the level"
                          , MemberCycleLevel d )
