-- | The type of definitions of key-command mappings to be used for the UI
-- and shorthands for specifying command triples in the content files.
module Game.LambdaHack.Client.UI.Content.Input
  ( InputContentRaw(..), InputContent(..), makeData
  , evalKeyDef
  , addCmdCategory, replaceDesc, moveItemTriple, repeatTriple, repeatLastTriple
  , mouseLMB, mouseMMB, mouseMMBMute, mouseRMB
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
      rawContent = copsClient ++ uCommands0
      movementDefinitions =
        K.moveBinding uVi0 uLeftHand0
          (\v -> ([CmdMove], "", moveXhairOr 1 MoveDir v))
          (\v -> ([CmdMove], "", moveXhairOr 10 RunDir v))
        ++ [ (K.mkKM "KP_Begin", waitTriple)
           , (K.mkKM "C-KP_Begin", wait10Triple)
           , (K.mkKM "KP_5", wait10Triple)
           , (K.mkKM "S-KP_5", wait10Triple)  -- rxvt
           , (K.mkKM "C-KP_5", wait10Triple) ]
        ++ [(K.mkKM "period", waitTriple) | uVi0]
        ++ [(K.mkKM "C-period", wait10Triple) | uVi0]
        ++ [(K.mkKM "s", waitTriple) | uLeftHand0]
        ++ [(K.mkKM "S", wait10Triple) | uLeftHand0]
      -- This is the most common case of duplicate keys and it usually
      -- has an easy solution, so it's tested for first.
      !_A = flip assert () $
        let movementKeys = map fst movementDefinitions
            filteredNoMovement = filter (\(k, _) -> k `notElem` movementKeys)
                                        rawContent
        in rawContent == filteredNoMovement
           `blame` "commands overwrite the enabled movement keys (you can disable some in config file and try again)"
           `swith` rawContent \\ filteredNoMovement
      bcmdList = rawContent ++ movementDefinitions
      -- This catches repetitions (usually) not involving movement keys.
      rejectRepetitions k t1 t2 =
        error $ "duplicate key among command definitions (you can instead disable some movement key sets in config file and overwrite the freed keys)" `showFailure` (k, t1, t2)
  in InputContent
  { bcmdMap = M.fromListWithKey rejectRepetitions bcmdList
  , bcmdList
  , brevMap = M.fromListWith (flip (++)) $ concat
      [ [(cmd, [k])]
      | (k, (cats, _desc, cmd)) <- bcmdList
      , not (null cats)
        && all (/= CmdDebug) cats
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

repeatTriple :: Int -> [CmdCategory] -> CmdTriple
repeatTriple n cats =
  ( cats
  , if n == 1
    then "voice recorded macro again"
    else "voice recorded macro" <+> tshow n <+> "times"
  , Repeat n )

repeatLastTriple :: Int -> [CmdCategory] -> CmdTriple
repeatLastTriple n cats =
  ( cats
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
    [ (CaMessage, AllHistory)
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
           , "snap crosshair to floor under pointer/cycle detail level"
           , XhairPointerFloor )

mouseMMBMute :: CmdTriple
mouseMMBMute = ([CmdMouse], "", XhairPointerMute)

mouseRMB :: CmdTriple
mouseRMB = ( [CmdMouse]
           , "start aiming at enemy under pointer/cycle detail level"
           , ByAimMode aimMode )
 where
  aimMode = AimModeCmd
    { exploration = ByArea $ common ++
        [ (CaMapLeader, dropCmd)
        , (CaMapParty, SelectWithPointer)
        , (CaMap, AimPointerEnemy)
        , (CaArenaName, ExecuteIfClear MainMenuAutoOff)
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
-- because the C- commands are less likely to be modified by the player
-- and so more dependable than @semicolon@, @colon@, etc.
goToCmd :: HumanCmd
goToCmd = Macro ["A-MiddleButtonRelease", "C-semicolon", "C-quotedbl", "C-v"]

-- This is duplicated wrt content, instead of included via @colon@,
-- because the C- commands are less likely to be modified by the player
-- and so more dependable than @semicolon@, @colon@, etc.
runToAllCmd :: HumanCmd
runToAllCmd = Macro ["A-MiddleButtonRelease", "C-colon", "C-quotedbl", "C-v"]

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

-- | flingTs - list containing one flingable projectile
-- >>> flingTs
-- [TriggerItem {tiverb = Text "fling", tiobject = Text "in-range projectile", tisymbols = ""}]
--
-- I question the value of that test. But would Bob Martin like it
-- on the grounds it's like double-bookkeeping?
flingTs :: [TriggerItem]
flingTs = [TriggerItem { tiverb = "fling"
                       , tiobject = "in-range projectile"
                       , tisymbols = [] }]

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
                     , PointmanCycle d )

memberCycleLevel :: Direction -> [CmdCategory] -> CmdTriple
memberCycleLevel d cats = ( cats
                          , "cycle"
                            <+> (if d == Backward then "backwards" else "")
                            <+> " among party members on the level"
                          , PointmanCycleLevel d )
