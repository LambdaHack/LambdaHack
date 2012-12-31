{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of player commands.
module Game.LambdaHack.CommandAction
  ( cmdSemantics
  ) where

import Control.Monad.Writer.Strict (WriterT, lift)
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Command
import Game.LambdaHack.EffectAction
import Game.LambdaHack.ItemAction
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Running
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | The basic action for a command and whether it takes time.
cmdAction :: MonadAction m => StateClient -> State -> Cmd
          -> (Bool, WriterT Slideshow m ())
cmdAction cli s cmd =
  let targeting = ctargeting (scursor cli)
      pl = splayer s
      sm = getActor pl s
      ploc = bloc sm
      tgtLoc = targetToLoc cli s ploc
  in case cmd of
    Apply{..} -> (True, lift $ playerApplyGroupItem verb object syms)
    Project{} | isNothing tgtLoc -> (False, retarget)
    Project{..} -> (True, lift $ playerProjectGroupItem verb object syms)
    TriggerDir{..} -> (True, lift $ playerTriggerDir feature verb)
    TriggerTile{..} -> (True, lift $ playerTriggerTile feature)
    Pickup -> (True, lift $ pickupItem)
    Drop   -> (True, lift $ dropItem)
    Wait   -> (True, lift $ waitBlock)
    Move v | targeting /= TgtOff ->
      let dir = toDir (lxsize (getArena s)) v
      in (False, moveCursor dir 1)
    Move v ->
      let dir = toDir (lxsize (getArena s)) v
          tloc = ploc `shift` dir
          tgt = locToActor tloc s
      in case tgt of
        Just target | bfaction (getActor target s) == sside s
                      && not (bproj (getActor target s)) ->
          -- Select adjacent hero by bumping into him. Takes no time.
          (False,
           selectPlayer target
           >>= assert `trueM` (pl, target, "player bumps himself" :: Text))
        _ -> (True, lift $ moveOrAttack True pl dir)
    Run v | targeting /= TgtOff ->
      let dir = toDir (lxsize (getArena s)) v
      in (False, moveCursor dir 10)
    Run v ->
      let dir = toDir (lxsize (getArena s)) v
      in (True, lift $ run (dir, 0))
    GameExit    -> (True, lift $ gameExit)     -- takes time, then rewinds time
    GameRestart -> (True, lift $ gameRestart)  -- takes time, then resets state

    GameSave    -> (False, lift $ gameSave)
    Inventory   -> (False, inventory)
    TgtFloor    -> (False, targetFloor   TgtExplicit)
    TgtEnemy    -> (False, targetMonster TgtExplicit)
    TgtAscend k -> (False, tgtAscend k)
    EpsIncr b   -> (False, lift $ epsIncr b)
    Cancel      -> (False, cancelCurrent displayMainMenu)
    Accept      -> (False, acceptCurrent displayHelp)
    Clear       -> (False, lift $ clearCurrent)
    History     -> (False, displayHistory)
    CfgDump     -> (False, lift $ dumpConfig)
    HeroCycle   -> (False, lift $ cycleHero)
    HeroBack    -> (False, lift $ backCycleHero)
    Help        -> (False, displayHelp)
    SelectHero k -> (False, lift $ selectHero k)
    DebugVision -> (False, modifyClient cycleMarkVision)
    DebugOmni   -> (False, modifyClient toggleOmniscient)

-- | The semantics of player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdSemantics :: MonadAction m => StateClient -> State -> Cmd
             -> WriterT Slideshow m Bool
cmdSemantics cli s cmd = do
  let (timed, sem) = cmdAction cli s cmd
  if timed
    then checkCursor sem
    else sem
  return timed

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadActionRO m => WriterT Slideshow m () -> WriterT Slideshow m ()
checkCursor h = do
  cursor <- getsClient scursor
  sarena <- getsLocal sarena
  if creturnLn cursor == sarena
    then h
    else abortWith "[targeting] you inspect a remote level, press ESC to switch back"
