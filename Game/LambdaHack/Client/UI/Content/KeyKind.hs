-- | The type of key-command mappings to be used for the UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..)
  , addCmdCategory, replaceDesc, gameRestartTriple, moveItemTriple, repeatTriple
  , defaultCmdLMB, defaultCmdMMB, defaultCmdRMB
  , projectI, projectA, flingTs, applyI
  , getAscend, descendDrop, chooseAndHelp, descTs, defaultHeroSelect
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
                 , "voice the recorded commands" <+> tshow n <+> "times"
                 , Repeat n )

defaultCmdLMB :: CmdTriple
defaultCmdLMB =
  ( [CmdMouse]
  , "go to pointer for 100 steps or set crosshair"
  , ByAimMode
      { notAiming = ByArea $ common ++  -- normal mode
          [ (CaMapParty, PickLeaderWithPointer)
          , (CaMap, Macro
               ["MiddleButtonRelease", "CTRL-semicolon", "CTRL-period", "V"])
          , (CaPercentSeen, Macro ["CTRL-?", "CTRL-period", "V"]) ]
      , aiming = ByArea $ common ++  -- aiming mode
          [ (CaMap, AimPointerEnemy)
          , (CaPercentSeen, XhairStair True) ] } )
 where
  common =
    [ (CaMessage, Clear)
    , (CaMapLeader, getAscendCmd)
    , (CaArenaName, ByAimMode {notAiming = MainMenu, aiming = Cancel})
    , (CaXhairDesc, AimEnemy)  -- inits aiming and then cycles enemies
    , (CaSelected, PickLeaderWithPointer)
    , (CaLeaderStatus, ChooseItem $ MStore COrgan)
    , (CaTargetDesc, ChooseItem $ MStore CInv) ]

defaultCmdMMB :: CmdTriple
defaultCmdMMB = ( [CmdMouse]
                , "set crosshair to floor under pointer"
                , XhairPointerFloor )

defaultCmdRMB :: CmdTriple
defaultCmdRMB =
  ( [CmdMouse]
  , "run collectively to pointer or set target"
  , ByAimMode
      { notAiming = ByArea $ common ++
          [ (CaMapParty, SelectWithPointer)
          , (CaMap, Macro
               ["MiddleButtonRelease", "CTRL-colon", "CTRL-period", "V"])
          , (CaPercentSeen, Macro ["'", "CTRL-?", "CTRL-period", "'", "V"])
          , (CaXhairDesc, AimFloor) ]
      , aiming = ByArea $ common ++
          [ (CaMap, ComposeIfLeft AimPointerEnemy (projectICmd flingTs))
          , (CaXhairDesc, (projectICmd flingTs))
          , (CaPercentSeen, XhairStair False) ] } )
 where
  common =
    [ (CaMessage, Macro ["KP_5", "V"])
    , (CaMapLeader, descendDropCmd)
    , (CaArenaName, ByAimMode {notAiming = Help, aiming = Accept})
    , (CaSelected, SelectWithPointer)
    , (CaLeaderStatus, ChooseItem MStats)
    , (CaTargetDesc, ChooseItem $ MStore CEqp) ]

projectICmd :: [Trigger] -> HumanCmd
projectICmd ts = ByItemMode
  { notChosen = ComposeIfEmpty (ChooseItemProject ts) (Project ts)
  , chosen = Project ts }

projectI :: [Trigger] -> CmdTriple
projectI ts = ([CmdItem], descTs ts, projectICmd ts)

projectA :: [Trigger] -> CmdTriple
projectA ts = replaceCmd (ByAimMode { notAiming = AimTgt
                                    , aiming = projectICmd ts }) (projectI ts)

flingTs :: [Trigger]
flingTs = [ApplyItem { verb = "fling"
                     , object = "projectile"
                     , symbol = ' ' }]

applyI :: [Trigger] -> CmdTriple
applyI ts = ([CmdItem], descTs ts, ByItemMode
  { notChosen = ComposeIfEmpty (ChooseItemApply ts) (Apply ts)
  , chosen = Apply ts })

getAscendCmd :: HumanCmd
getAscendCmd = ByAimMode
  { notAiming = ReplaceFail "cannot get items nor ascend" $
      ComposeIfLeft
        (MoveItem [CGround] CEqp (Just "get") "items" True)
        (TriggerTile
           [ TriggerFeature { verb = "ascend"
                            , object = "a level"
                            , feature = TK.Cause (IK.Ascend 1) }
           , TriggerFeature { verb = "escape"
                            , object = "dungeon"
                            , feature = TK.Cause (IK.Escape 1) } ])
  , aiming = AimAscend 1 }

getAscend :: Text -> CmdTriple
getAscend t = ([CmdMove, CmdItem], t, getAscendCmd)

descendDropCmd :: HumanCmd
descendDropCmd = ByAimMode
  { notAiming = ReplaceFail "cannot descend nor drop items" $
      ComposeIfLeft
        (TriggerTile
           [ TriggerFeature { verb = "descend"
                            , object = "a level"
                            , feature = TK.Cause (IK.Ascend (-1)) }
           , TriggerFeature { verb = "escape"
                            , object = "dungeon"
                            , feature = TK.Cause (IK.Escape (-1)) } ])
        (MoveItem [CEqp, CInv, CSha] CGround Nothing "item" False)
  , aiming = AimAscend (-1) }

descendDrop :: Text -> CmdTriple
descendDrop t = ([CmdMove, CmdItem], t, descendDropCmd)

chooseAndHelp :: Text -> ItemDialogMode -> CmdTriple
chooseAndHelp desc dialogMode =
  ([CmdItem], desc, ComposeIfEmpty (ChooseItem dialogMode) Help)

descTs :: [Trigger] -> Text
descTs [] = "trigger a thing"
descTs (t : _) = makePhrase [verb t, object t]

defaultHeroSelect :: Int -> (String, CmdTriple)
defaultHeroSelect k = ([Char.intToDigit k], ([CmdMeta], "", PickLeader k))
