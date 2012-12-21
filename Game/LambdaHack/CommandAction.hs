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
import Game.LambdaHack.Animation (Frames)
import Game.LambdaHack.Command
import Game.LambdaHack.EffectAction
import Game.LambdaHack.ItemAction
import Game.LambdaHack.Level
import Game.LambdaHack.Perception
import Game.LambdaHack.Running
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | The basic action for a command and whether it takes time.
cmdAction :: MonadAction m => State -> Perception -> Cmd
          -> (Bool, WriterT Frames m ())
cmdAction s per cmd =
  let targeting = ctargeting (scursor s)
      pl = splayer s
      sm = getActor pl s
      ploc = bloc sm
      tgtLoc = targetToLoc (totalVisible per) s ploc
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
      let dir = toDir (lxsize (slevel s)) v
      in (False, moveCursor dir 1)
    Move v ->
      let dir = toDir (lxsize (slevel s)) v
          tloc = ploc `shift` dir
          tgt = locToActor tloc s
      in case tgt of
        Just target | bfaction (getActor target s) == sfaction s
                      && not (bproj (getActor target s)) ->
          -- Select adjacent hero by bumping into him. Takes no time.
          (False,
           selectPlayer target
           >>= assert `trueM` (pl, target, "player bumps himself" :: Text))
        _ -> (True, lift $ moveOrAttack True pl dir)
    Run v | targeting /= TgtOff ->
      let dir = toDir (lxsize (slevel s)) v
      in (False, moveCursor dir 10)
    Run v ->
      let dir = toDir (lxsize (slevel s)) v
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
    DebugVision -> (False, modifyServer cycleMarkVision)
    DebugOmni   -> (False, modifyServer toggleOmniscient)
    DebugCave   -> (False, getsServer (lmeta . slevel) >>= abortWith)

-- | The semantics of player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdSemantics :: MonadAction m => State -> Perception -> Cmd
             -> (Bool, WriterT Frames m ())
cmdSemantics s per cmd =
  let (timed, sem) = cmdAction s per cmd
  in if timed
     then (timed, checkCursor sem)
     else (timed, sem)

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadActionPure m => WriterT Frames m () -> WriterT Frames m ()
checkCursor h = do
  cursor <- getsServer scursor
  slid <- getsServer slid
  if creturnLn cursor == slid
    then h
    else abortWith "[targeting] you inspect a remote level, press ESC to switch back"
