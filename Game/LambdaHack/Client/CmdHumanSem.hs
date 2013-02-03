{-# LANGUAGE GADTs, OverloadedStrings #-}
-- | Semantics of human player commands.
module Game.LambdaHack.Client.CmdHumanSem
  ( cmdSemantics
  ) where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, lift)
import Data.Maybe
import Data.Text (Text)
import qualified Data.EnumMap.Strict as EM

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

-- | The basic action for a command and whether it takes time.
cmdAction :: MonadClientUI m => StateClient -> State -> CmdHuman
          -> WriterT Slideshow m (Maybe CmdSer)
cmdAction cli s cmd =
  let tgtMode = stgtMode cli
      leader = fromJust $ sleader cli
      arena = sarena s
      sm = getActorBody leader s
      ppos = bpos sm
      tgtLoc = targetToPos cli s
      Level{lxsize} =
        maybe (getArena s) ((sdungeon s EM.!) . tgtLevelId) tgtMode
  in case cmd of
    Apply{..} -> lift $ fmap Just $ leaderApplyGroupItem verb object syms
    Project{} | isNothing tgtLoc -> retarget >> return Nothing
    Project{..} -> lift $ fmap Just $ leaderProjectGroupItem verb object syms
    TriggerDir{..} -> lift $ fmap Just $ leaderTriggerDir feature verb
    TriggerTile{..} -> lift $ fmap Just $ leaderTriggerTile feature
    Pickup -> lift $ fmap Just $ pickupItem
    Drop   -> lift $ fmap Just $ dropItem
    Wait   -> lift $ fmap Just $ waitBlock
    Move v | isJust tgtMode ->
      let dir = toDir lxsize v
      in moveCursor dir 1 >> return Nothing
    Move v ->
      let dir = toDir lxsize v
          tpos = ppos `shift` dir
          -- We always see actors from our own faction.
          tgt = posToActor tpos s
      in case tgt of
        Just target | bfaction (getActorBody target s) == sside s
                      && not (bproj (getActorBody target s)) ->
          -- Select adjacent actor by bumping into him. Takes no time.
          selectLeader target arena
            >>= assert `trueM`
                  (leader, target, "leader bumps into himself" :: Text)
            >> return Nothing
        _ -> lift $ fmap Just $ movePl dir
    Run v | isJust tgtMode ->
      let dir = toDir lxsize v
      in moveCursor dir 10 >> return Nothing
    Run v ->
      let dir = toDir lxsize v
      in lift $ fmap Just $ runPl dir

    GameExit    -> lift $ fmap Just $ gameExit
    GameRestart -> lift $ fmap Just $ gameRestart
    GameSave    -> lift $ fmap Just $ gameSave
    CfgDump     -> lift $ fmap Just $ dumpConfig
    Inventory   -> inventory >> return Nothing
    TgtFloor    -> (targetFloor   $ TgtExplicit arena) >> return Nothing
    TgtEnemy    -> (targetEnemy $ TgtExplicit arena) >> return Nothing
    TgtAscend k -> tgtAscend k >> return Nothing
    EpsIncr b   -> lift $ epsIncr b >> return Nothing
    Cancel      -> cancelCurrent displayMainMenu >> return Nothing
    Accept      -> acceptCurrent displayHelp >> return Nothing
    Clear       -> lift $ clearCurrent >> return Nothing
    History     -> displayHistory >> return Nothing
    MemberCycle -> lift $ cycleMember >> return Nothing
    MemberBack  -> lift $ backCycleMember >> return Nothing
    Help        -> displayHelp >> return Nothing
    SelectHero k -> lift $ selectHero k >> return Nothing
    DebugArea   -> modifyClient toggleMarkVision >> return Nothing
    DebugOmni   -> modifyClient toggleOmniscient >> return Nothing  -- TODO: Server
    DebugSmell  -> modifyClient toggleMarkSmell >> return Nothing
    DebugVision -> undefined {-modifyServer cycleTryFov-}

-- | The semantics of human player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdSemantics :: MonadClientUI m => CmdHuman -> WriterT Slideshow m (Maybe CmdSer)
cmdSemantics cmd = do
  Just leaderOld <- getsClient sleader
  arenaOld <- getsState sarena
  posOld <- getsState (bpos . getActorBody leaderOld)
  cli <- getClient
  loc <- getState
  let sem = cmdAction cli loc cmd
  mcmdS <- if noRemoteCmdHuman cmd
           then checkCursor sem
           else sem
  arena <- getsState sarena
  leaderNew <- getsClient sleader
  case leaderNew of
    Nothing -> return ()
    Just leader -> do
      pos <- getsState (bpos . getActorBody leader)
      tgtMode <- getsClient stgtMode
      when (isNothing tgtMode  -- targeting performs a more extensive look
            && (posOld /= pos
                || arenaOld /= arena)) $ do
        lookMsg <- lookAt False True pos ""
        msgAdd lookMsg
  return mcmdS

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadClientRO m
            => WriterT Slideshow m (Maybe CmdSer)
            -> WriterT Slideshow m (Maybe CmdSer)
checkCursor h = do
  arena <- getsState sarena
  (lid, _) <- viewedLevel
  if arena == lid
    then h
    else abortWith "[targeting] command disabled on a remote level, press ESC to switch back"
