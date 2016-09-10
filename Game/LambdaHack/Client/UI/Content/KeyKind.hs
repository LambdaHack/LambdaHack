-- | The type of key-command mappings to be used for the UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..), evalKeyDef
  , addCmdCategory, replaceDesc, gameRestartTriple, moveItemTriple, repeatTriple
  , mouseLMB, mouseMMB, mouseRMB
  , goToCmd, runToAllCmd, autoexploreCmd, autoexplore100Cmd
  , aimFlingCmd, projectI, projectA, flingTs, applyI
  , exploreGrabAscendCmd, exploreDescendDropCmd, grabAscend, descendDrop
  , descTs, defaultHeroSelect
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Common.Actor (verbCStore)
import Game.LambdaHack.Common.Misc
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK

-- | Key-command mappings to be used for the UI.
data KeyKind = KeyKind
  { rhumanCommands :: ![(K.KM, CmdTriple)]  -- ^ default client UI commands
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

-- TODO: use mname for the game mode instead of t
gameRestartTriple :: GroupName ModeKind -> CmdTriple
gameRestartTriple t =
  ( [CmdMainMenu]
  , makePhrase ["new", MU.Capitalize $ MU.Text $ tshow t, "game"]
  , GameRestart t )

moveItemTriple :: [CStore] -> CStore -> (Maybe MU.Part) -> MU.Part -> Bool
               -> CmdTriple
moveItemTriple stores1 store2 mverb object auto =
  let verb = fromMaybe (MU.Text $ verbCStore store2) mverb
      desc = makePhrase [verb, object]
  in ([CmdItem], desc, MoveItem stores1 store2 mverb object auto)

repeatTriple :: Int -> CmdTriple
repeatTriple n = ( [CmdMeta]
                 , "voice recorded commands" <+> tshow n <+> "times"
                 , Repeat n )

mouseLMB :: CmdTriple
mouseLMB =
  ( [CmdMouse]
  , "set aiming crosshair/go to pointer for 100 steps"
  , ByAimMode
      { exploration = ByArea $ common ++  -- exploration mode
          [ (CaMapLeader, exploreGrabAscendCmd)
          , (CaMapParty, PickLeaderWithPointer)
          , (CaMap, goToCmd)
          , (CaArenaName, MainMenu)
          , (CaPercentSeen, autoexploreCmd) ]
      , aiming = ByArea $ common ++  -- aiming mode
          [ (CaMapLeader, AimAscend 1)
          , (CaMap, AimPointerEnemy)
          , (CaArenaName, Cancel)
          , (CaPercentSeen, XhairStair True) ] } )
 where
  common =
    [ (CaMessage, Clear)
    , (CaXhairDesc, AimEnemy)  -- inits aiming and then cycles enemies
    , (CaSelected, PickLeaderWithPointer)
    , (CaLeaderStatus, ChooseItemMenu $ MStore COrgan)
    , (CaTargetDesc, ChooseItemMenu $ MStore CInv) ]

mouseMMB :: CmdTriple
mouseMMB = ( [CmdMouse]
           , "snap aiming crosshair to floor under pointer"
           , XhairPointerFloor )

mouseRMB :: CmdTriple
mouseRMB =
  ( [CmdMouse]
  , "set leader target/run to pointer collectively for 100 steps"
  , ByAimMode
      { exploration = ByArea $ common ++
          [ (CaMapLeader, exploreDescendDropCmd)
          , (CaMapParty, SelectWithPointer)
          , (CaMap, runToAllCmd)
          , (CaArenaName, Help)
          , (CaPercentSeen, autoexplore100Cmd)
          , (CaXhairDesc, projectICmd flingTs) ]
      , aiming = ByArea $ common ++
          [ (CaMapLeader, AimAscend (-1))
          , (CaMap, aimFlingCmd)
          , (CaArenaName, Accept)
          , (CaPercentSeen, XhairStair False)
          , (CaXhairDesc, AimFloor) ] } )
 where
  common =
    [ (CaMessage, Macro ["KP_5", "V"])
    , (CaSelected, SelectWithPointer)
    , (CaLeaderStatus, ChooseItemMenu MStats)
    , (CaTargetDesc, ChooseItemMenu $ MStore CEqp) ]

goToCmd :: HumanCmd
goToCmd = Macro ["MiddleButtonRelease", "CTRL-semicolon", "CTRL-period", "V"]

runToAllCmd :: HumanCmd
runToAllCmd = Macro ["MiddleButtonRelease", "CTRL-colon", "CTRL-period", "V"]

autoexploreCmd :: HumanCmd
autoexploreCmd = Macro ["CTRL-?", "CTRL-period", "V"]

autoexplore100Cmd :: HumanCmd
autoexplore100Cmd = Macro ["'", "CTRL-?", "CTRL-period", "'", "V"]

aimFlingCmd :: HumanCmd
aimFlingCmd = ComposeIfLocal AimPointerEnemy (projectICmd flingTs)

projectICmd :: [Trigger] -> HumanCmd
projectICmd ts = ByItemMode
  { notChosen = ComposeUnlessError (ChooseItemProject ts) (Project ts)
  , chosen = Project ts }

projectI :: [Trigger] -> CmdTriple
projectI ts = ([CmdItem], descTs ts, projectICmd ts)

projectA :: [Trigger] -> CmdTriple
projectA ts = replaceCmd (ByAimMode { exploration = AimTgt
                                    , aiming = projectICmd ts }) (projectI ts)

flingTs :: [Trigger]
flingTs = [ApplyItem { verb = "fling"
                     , object = "projectile"
                     , symbol = ' ' }]

applyI :: [Trigger] -> CmdTriple
applyI ts = ([CmdItem], descTs ts, ByItemMode
  { notChosen = ComposeUnlessError (ChooseItemApply ts) (Apply ts)
  , chosen = Apply ts })

grabAscendCmd :: HumanCmd
grabAscendCmd = ByAimMode
  { exploration = exploreGrabAscendCmd
  , aiming = AimAscend 1 }

exploreGrabAscendCmd :: HumanCmd
exploreGrabAscendCmd =
  ComposeIfLocal
    (MoveItem [CGround] CEqp (Just "grab") "items" True)
    (TriggerTile
       [ TriggerFeature { verb = "ascend"
                        , object = "a level"
                        , feature = TK.Cause (IK.Ascend 1) }
       , TriggerFeature { verb = "escape"
                        , object = "dungeon"
                        , feature = TK.Cause (IK.Escape 1) } ])

grabAscend :: Text -> CmdTriple
grabAscend t = ([CmdMove, CmdItem], t, grabAscendCmd)

descendDropCmd :: HumanCmd
descendDropCmd = ByAimMode
  { exploration = exploreDescendDropCmd
  , aiming = AimAscend (-1) }

exploreDescendDropCmd :: HumanCmd
exploreDescendDropCmd =
  ComposeIfLocal
    (TriggerTile
       [ TriggerFeature { verb = "descend"
                        , object = "a level"
                        , feature = TK.Cause (IK.Ascend (-1)) }
       , TriggerFeature { verb = "escape"
                        , object = "dungeon"
                        , feature = TK.Cause (IK.Escape (-1)) } ])
    (MoveItem [CEqp, CInv, CSha] CGround Nothing "item" False)

descendDrop :: Text -> CmdTriple
descendDrop t = ([CmdMove, CmdItem], t, descendDropCmd)

descTs :: [Trigger] -> Text
descTs [] = "trigger a thing"
descTs (t : _) = makePhrase [verb t, object t]

defaultHeroSelect :: Int -> (String, CmdTriple)
defaultHeroSelect k = ([Char.intToDigit k], ([CmdMeta], "", PickLeader k))
