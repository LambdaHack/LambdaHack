{-# LANGUAGE GADTs, OverloadedStrings #-}
-- | Semantics of human player commands.
module Game.LambdaHack.Client.CmdHumanSem
  ( cmdHumanSem
  ) where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.CmdHuman
import Game.LambdaHack.Client.LocalAction
import Game.LambdaHack.Client.MixedAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | The semantics of human player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdHumanSem :: MonadClientUI m
            => CmdHuman -> WriterT Slideshow m (Maybe CmdSer)
cmdHumanSem cmd = do
  Just leaderOld <- getsClient sleader
  bOld <- getsState $ getActorBody leaderOld
  let posOld = bpos bOld
      arenaOld = blid bOld
  cli <- getClient
  loc <- getState
  when (noRemoteCmdHuman cmd) $ checkCursor arenaOld
  msem <- cmdAction cli loc cmd
  Just leaderNew <- getsClient sleader
  bNew <- getsState $ getActorBody leaderNew
  let pos = bpos bNew
      arena = blid bNew
  tgtMode <- getsClient stgtMode
  when (isNothing tgtMode  -- targeting performs a more extensive look
        && (posOld /= pos
            || arenaOld /= arena)) $ do
    lookMsg <- lookAt False True pos ""
    msgAdd lookMsg
  return msem

-- | The basic action for a command and whether it takes time.
cmdAction :: MonadClientUI m
          => StateClient -> State -> CmdHuman
          -> WriterT Slideshow m (Maybe CmdSer)
cmdAction cli s cmd =
  let tgtMode = stgtMode cli
      leader = fromJust $ sleader cli
      sm = getActorBody leader s
      arena = blid sm
      ppos = bpos sm
      tgtLoc = targetToPos cli s
      Level{lxsize} =
        maybe (sdungeon s EM.! arena) ((sdungeon s EM.!) . tgtLevelId) tgtMode
  in case cmd of
    Apply{..} -> fmap Just $ leaderApplyGroupItem verb object syms
    Project{} | isNothing tgtLoc -> retarget >> return Nothing
    Project{..} -> fmap Just $ leaderProjectGroupItem verb object syms
    TriggerDir{..} -> fmap Just $ leaderTriggerDir feature verb
    TriggerTile{..} -> fmap Just $ leaderTriggerTile feature
    Pickup -> fmap Just $ pickupItem
    Drop -> fmap Just $ dropItem
    Wait -> fmap Just $ waitBlock
    Move v | isJust tgtMode ->
      let dir = toDir lxsize v
      in moveCursor dir 1 >> return Nothing
    Move v ->
      let dir = toDir lxsize v
          tpos = ppos `shift` dir
          -- We always see actors from our own faction.
          tgt = posToActor tpos arena s
      in case tgt of
        Just target | bfaction (getActorBody target s) == bfaction sm
                      && not (bproj (getActorBody target s)) ->
          -- Select adjacent actor by bumping into him. Takes no time.
          selectLeader target
            >>= assert `trueM`
                  (leader, target, "leader bumps into himself" :: Text)
            >> return Nothing
        _ -> fmap Just $ movePl dir
    Run v | isJust tgtMode ->
      let dir = toDir lxsize v
      in moveCursor dir 10 >> return Nothing
    Run v ->
      let dir = toDir lxsize v
      in fmap Just $ runPl dir

    GameExit    -> fmap Just $ gameExit
    GameRestart -> fmap Just $ gameRestart
    GameSave    -> fmap Just $ gameSave
    CfgDump     -> fmap Just $ dumpConfig
    Inventory   -> inventory >> return Nothing
    TgtFloor    -> (targetFloor   $ TgtExplicit arena) >> return Nothing
    TgtEnemy    -> (targetEnemy $ TgtExplicit arena) >> return Nothing
    TgtAscend k -> tgtAscend k >> return Nothing
    EpsIncr b   -> epsIncr b >> return Nothing
    Cancel      -> cancelCurrent displayMainMenu >> return Nothing
    Accept      -> acceptCurrent displayHelp >> return Nothing
    Clear       -> clearCurrent >> return Nothing
    History     -> displayHistory >> return Nothing
    MemberCycle -> cycleMember >> return Nothing
    MemberBack  -> backCycleMember >> return Nothing
    Help        -> displayHelp >> return Nothing
    SelectHero k -> selectHero k >> return Nothing
    DebugArea   -> modifyClient toggleMarkVision >> return Nothing
    DebugOmni   -> modifyClient toggleOmniscient >> return Nothing  -- TODO: Server
    DebugSmell  -> modifyClient toggleMarkSmell >> return Nothing
    DebugVision -> undefined {-modifyServer cycleTryFov-}

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadClient m => LevelId -> m ()
checkCursor arena = do
  (lid, _) <- viewedLevel
  when (arena /= lid) $
    abortWith "[targeting] command disabled on a remote level, press ESC to switch back"
