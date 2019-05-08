-- | The type of definitions of key-command mappings to be used for the UI
-- and shorthands for specifying command triples in the content files.
module Game.LambdaHack.Client.UI.Content.Input
  ( InputContentRaw(..), InputContent(..), makeData
  , evalKeyDef
  , addCmdCategory, replaceDesc, moveItemTriple, repeatTriple
  , mouseLMB, mouseMMB, mouseRMB
  , goToCmd, runToAllCmd, autoexploreCmd, autoexplore25Cmd
  , aimFlingCmd, projectI, projectA, flingTs, applyIK, applyI
  , grabItems, dropItems, descIs, descTs, defaultHeroSelect
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
makeData :: UIOptions        -- ^ UI client options
         -> InputContentRaw  -- ^ default key bindings from the content
         -> InputContent     -- ^ concrete binding
makeData UIOptions{uCommands, uVi, uLaptop} (InputContentRaw copsClient) =
  let waitTriple = ([CmdMove], "", Wait)
      wait10Triple = ([CmdMove], "", Wait10)
      moveXhairOr n cmd v = ByAimMode $ AimModeCmd { exploration = cmd v
                                                   , aiming = MoveXhair v n }
      bcmdList =
        (if | uVi -> filter (\(k, _) ->
              k `notElem` [K.mkKM "period", K.mkKM "C-period"])
            | uLaptop -> filter (\(k, _) ->
              k `notElem` [K.mkKM "i", K.mkKM "C-i", K.mkKM "I"])
            | otherwise -> id) copsClient
        ++ uCommands
        ++ [ (K.mkKM "KP_Begin", waitTriple)
           , (K.mkKM "C-KP_Begin", wait10Triple)
           , (K.mkKM "KP_5", wait10Triple)
           , (K.mkKM "C-KP_5", wait10Triple) ]
        ++ (if | uVi ->
                 [ (K.mkKM "period", waitTriple)
                 , (K.mkKM "C-period", wait10Triple) ]  -- yell on % always
               | uLaptop ->
                 [ (K.mkKM "i", waitTriple)
                 , (K.mkKM "C-i", wait10Triple)
                 , (K.mkKM "I", wait10Triple) ]
               | otherwise ->
                 [])
        ++ K.moveBinding uVi uLaptop
             (\v -> ([CmdMove], "", moveXhairOr 1 MoveDir v))
             (\v -> ([CmdMove], "", moveXhairOr 10 RunDir v))
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
  in ([CmdItemMenu], desc, MoveItem stores1 store2 Nothing auto)

repeatTriple :: Int -> CmdTriple
repeatTriple n = ( [CmdMeta]
                 , "voice recorded commands" <+> tshow n <+> "times"
                 , Repeat n )

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
--    , (CaCalmGauge, Macro ["KP_Begin", "C-V"])
    , (CaCalmValue, Yell)
    , (CaHPGauge, Macro ["KP_Begin", "C-V"])
    , (CaHPValue, Wait)
    , (CaLeaderDesc, projectICmd flingTs) ]

mouseMMB :: CmdTriple
mouseMMB = ( [CmdMouse]
           , "snap x-hair to floor under pointer"
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
--    , (CaCalmGauge, Macro ["C-KP_Begin", "V"])
    , (CaCalmValue, Yell)
    , (CaHPGauge, Macro ["C-KP_Begin", "V"])
    , (CaHPValue, Wait10)
    , (CaLeaderDesc, ComposeUnlessError ClearTargetIfItemClear ItemClear) ]

-- This is duplicated wrt content, instead of included via @semicolon@,
-- because the C- commands are less likely to be modified by the player.
goToCmd :: HumanCmd
goToCmd = Macro ["MiddleButtonRelease", "C-semicolon", "C-quotedbl", "C-V"]

-- This is duplicated wrt content, instead of included via @colon@,
-- because the C- commands are less likely to be modified by the player.
runToAllCmd :: HumanCmd
runToAllCmd = Macro ["MiddleButtonRelease", "C-colon", "C-quotedbl", "C-V"]

autoexploreCmd :: HumanCmd
autoexploreCmd = Macro ["C-?", "C-quotedbl", "C-V"]

autoexplore25Cmd :: HumanCmd
autoexplore25Cmd = Macro ["'", "C-?", "C-quotedbl", "'", "C-V"]

aimFlingCmd :: HumanCmd
aimFlingCmd = ComposeIfLocal AimPointerEnemy (projectICmd flingTs)

projectICmd :: [TriggerItem] -> HumanCmd
projectICmd ts = ComposeUnlessError (ChooseItemProject ts) Project

projectI :: [TriggerItem] -> CmdTriple
projectI ts = ([], descIs ts, projectICmd ts)

projectA :: [TriggerItem] -> CmdTriple
projectA ts =
  let fling = Compose2ndLocal Project ItemClear
      flingICmd = ComposeUnlessError (ChooseItemProject ts) fling
  in replaceCmd (ByAimMode AimModeCmd { exploration = AimTgt
                                      , aiming = flingICmd })
                (projectI ts)

flingTs :: [TriggerItem]
flingTs = [TriggerItem { tiverb = "fling"
                       , tiobject = "projectile"
                       , tisymbols = "" }]

applyIK :: [TriggerItem] -> CmdTriple
applyIK ts =
  ([], descIs ts, ComposeUnlessError (ChooseItemApply ts) Apply)

applyI :: [TriggerItem] -> CmdTriple
applyI ts =
  let apply = Compose2ndLocal Apply ItemClear
  in ([], descIs ts, ComposeUnlessError (ChooseItemApply ts) apply)

grabCmd :: HumanCmd
grabCmd = MoveItem [CGround] CStash (Just "grab") True
            -- @CStash@ is the implicit default; refined in HandleHumanGlobalM

grabItems :: Text -> CmdTriple
grabItems t = ([CmdItemMenu], t, grabCmd)

dropCmd :: HumanCmd
dropCmd = MoveItem [CEqp, CStash] CGround Nothing False

dropItems :: Text -> CmdTriple
dropItems t = ([CmdItemMenu], t, dropCmd)

descIs :: [TriggerItem] -> Text
descIs [] = "trigger an item"
descIs (t : _) = makePhrase [tiverb t, tiobject t]

descTs :: [TriggerTile] -> Text
descTs [] = "alter a tile"
descTs (t : _) = makePhrase [ttverb t, ttobject t]

defaultHeroSelect :: Int -> (String, CmdTriple)
defaultHeroSelect k = ([Char.intToDigit k], ([CmdMeta], "", PickLeader k))
