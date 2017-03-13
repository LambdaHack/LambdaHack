-- | The type of key-command mappings to be used for the UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..), evalKeyDef
  , addCmdCategory, replaceDesc, moveItemTriple, repeatTriple
  , mouseLMB, mouseMMB, mouseRMB
  , goToCmd, runToAllCmd, autoexploreCmd, autoexplore25Cmd
  , aimFlingCmd, projectI, projectA, flingTs, applyI
  , grabItems, dropItems, descTs, defaultHeroSelect
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.ActorUI (verbCStore)
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Common.Misc

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

moveItemTriple :: [CStore] -> CStore -> MU.Part -> Bool -> CmdTriple
moveItemTriple stores1 store2 object auto =
  let verb = MU.Text $ verbCStore store2
      desc = makePhrase [verb, object]
  in ([CmdItem], desc, MoveItem stores1 store2 Nothing auto)

repeatTriple :: Int -> CmdTriple
repeatTriple n = ( [CmdMeta]
                 , "voice recorded commands" <+> tshow n <+> "times"
                 , Repeat n )

mouseLMB :: CmdTriple
mouseLMB =
  ( [CmdMouse]
  , "set aiming crosshair/go to pointer for 25 steps"
  , ByAimMode
      { exploration = ByArea $ common ++  -- exploration mode
          [ (CaMapLeader, grabCmd)
          , (CaMapParty, PickLeaderWithPointer)
          , (CaMap, goToCmd)
          , (CaLevelNumber, AimAscend 1)
          , (CaArenaName, MainMenu)
          , (CaPercentSeen, autoexploreCmd) ]
      , aiming = ByArea $ common ++  -- aiming mode
          [ (CaMapLeader, AimAscend 1)
          , (CaMap, AimPointerEnemy)
          , (CaLevelNumber, AimAscend 1)
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
  , "set leader target/run to pointer collectively for 25 steps"
  , ByAimMode
      { exploration = ByArea $ common ++
          [ (CaMapLeader, dropCmd)
          , (CaMapParty, SelectWithPointer)
          , (CaMap, runToAllCmd)
          , (CaLevelNumber, AimAscend (-1))
          , (CaArenaName, Help)
          , (CaPercentSeen, autoexplore25Cmd)
          , (CaXhairDesc, projectICmd flingTs) ]
      , aiming = ByArea $ common ++
          [ (CaMapLeader, AimAscend (-1))
          , (CaMap, aimFlingCmd)
          , (CaLevelNumber, AimAscend (-1))
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
goToCmd = Macro ["MiddleButtonRelease", "C-semicolon", "C-period", "V"]

runToAllCmd :: HumanCmd
runToAllCmd = Macro ["MiddleButtonRelease", "C-colon", "C-period", "V"]

autoexploreCmd :: HumanCmd
autoexploreCmd = Macro ["C-?", "C-period", "V"]

autoexplore25Cmd :: HumanCmd
autoexplore25Cmd = Macro ["'", "C-?", "C-period", "'", "V"]

aimFlingCmd :: HumanCmd
aimFlingCmd = ComposeIfLocal AimPointerEnemy (projectICmd flingTs)

projectICmd :: [Trigger] -> HumanCmd
projectICmd ts = ByItemMode
  { ts
  , notChosen = ComposeUnlessError (ChooseItemProject ts) (Project ts)
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
  { ts
  , notChosen = ComposeUnlessError (ChooseItemApply ts) (Apply ts)
  , chosen = Apply ts })

grabCmd :: HumanCmd
grabCmd = MoveItem [CGround] CEqp (Just "grab") True

grabItems :: Text -> CmdTriple
grabItems t = ([CmdMove, CmdItem], t, grabCmd)

dropCmd :: HumanCmd
dropCmd = MoveItem [CEqp, CInv, CSha] CGround Nothing False

dropItems :: Text -> CmdTriple
dropItems t = ([CmdMove, CmdItem], t, dropCmd)

descTs :: [Trigger] -> Text
descTs [] = "trigger a thing"
descTs (t : _) = makePhrase [verb t, object t]

defaultHeroSelect :: Int -> (String, CmdTriple)
defaultHeroSelect k = ([Char.intToDigit k], ([CmdMeta], "", PickLeader k))
