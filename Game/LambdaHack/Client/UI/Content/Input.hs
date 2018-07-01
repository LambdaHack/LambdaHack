-- | The type of definitions of key-command mappings to be used for the UI
-- and shorthands for specifying command triples in the content files.
module Game.LambdaHack.Client.UI.Content.Input
  ( InputContentData(..), evalKeyDef
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

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.UI.ActorUI (verbCStore)
import           Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Common.Misc

-- | Key-command mappings to be specified in content and used for the UI.
newtype InputContentData = InputContentData [(K.KM, CmdTriple)]

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
mouseLMB :: CmdTriple
mouseLMB =
  ( [CmdMouse]
  , "set x-hair to enemy/go to pointer for 25 steps"
  , ByAimMode
      { exploration = ByArea $ common ++  -- exploration mode
          [ (CaMapLeader, grabCmd)
          , (CaMapParty, PickLeaderWithPointer)
          , (CaMap, goToCmd)
          , (CaArenaName, Dashboard)
          , (CaPercentSeen, autoexploreCmd) ]
      , aiming = ByArea $ common ++  -- aiming mode
          [ (CaMap, AimPointerEnemy)
          , (CaArenaName, Accept)
          , (CaPercentSeen, XhairStair True) ] } )
 where
  common =
    [ (CaMessage, ExecuteIfClear History)
    , (CaLevelNumber, AimAscend 1)
    , (CaXhairDesc, AimEnemy)  -- inits aiming and then cycles enemies
    , (CaSelected, PickLeaderWithPointer)
    , (CaCalmGauge, Macro ["KP_5", "C-V"])
    , (CaHPGauge, Wait)
    , (CaTargetDesc, projectICmd flingTs) ]

mouseMMB :: CmdTriple
mouseMMB = ( [CmdMouse]
           , "snap x-hair to floor under pointer"
           , XhairPointerFloor )

mouseRMB :: CmdTriple
mouseRMB =
  ( [CmdMouse]
  , "fling at enemy/run to pointer collectively for 25 steps"
  , ByAimMode
      { exploration = ByArea $ common ++
          [ (CaMapLeader, dropCmd)
          , (CaMapParty, SelectWithPointer)
          , (CaMap, runToAllCmd)
          , (CaArenaName, MainMenu)
          , (CaPercentSeen, autoexplore25Cmd) ]
      , aiming = ByArea $ common ++
          [ (CaMap, aimFlingCmd)
          , (CaArenaName, Cancel)
          , (CaPercentSeen, XhairStair False) ] } )
 where
  common =
    [ (CaMessage, Hint)
    , (CaLevelNumber, AimAscend (-1))
    , (CaXhairDesc, AimItem)
    , (CaSelected, SelectWithPointer)
    , (CaCalmGauge, Macro ["C-KP_5", "V"])
    , (CaHPGauge, Wait10)
    , (CaTargetDesc, ComposeUnlessError ItemClear TgtClear) ]

goToCmd :: HumanCmd
goToCmd = Macro ["MiddleButtonRelease", "C-semicolon", "C-/", "C-V"]

runToAllCmd :: HumanCmd
runToAllCmd = Macro ["MiddleButtonRelease", "C-colon", "C-/", "C-V"]

autoexploreCmd :: HumanCmd
autoexploreCmd = Macro ["C-?", "C-/", "C-V"]

autoexplore25Cmd :: HumanCmd
autoexplore25Cmd = Macro ["'", "C-?", "C-/", "'", "C-V"]

aimFlingCmd :: HumanCmd
aimFlingCmd = ComposeIfLocal AimPointerEnemy (projectICmd flingTs)

projectICmd :: [TriggerItem] -> HumanCmd
projectICmd ts = ComposeUnlessError (ChooseItemProject ts) Project

projectI :: [TriggerItem] -> CmdTriple
projectI ts = ([], descIs ts, projectICmd ts)

projectA :: [TriggerItem] -> CmdTriple
projectA ts = replaceCmd ByAimMode { exploration = AimTgt
                                   , aiming = projectICmd ts } (projectI ts)

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
grabCmd = MoveItem [CGround] CEqp (Just "grab") True
            -- @CEqp@ is the implicit default; refined in HandleHumanGlobalM

grabItems :: Text -> CmdTriple
grabItems t = ([CmdItemMenu], t, grabCmd)

dropCmd :: HumanCmd
dropCmd = MoveItem [CEqp, CInv, CSha] CGround Nothing False

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
