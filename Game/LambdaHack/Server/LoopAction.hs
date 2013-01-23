{-# LANGUAGE OverloadedStrings #-}
-- | The main loop of the server, processing human and computer player
-- moves turn by turn.
module Game.LambdaHack.Server.LoopAction (loopSer) where

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Reader.Class
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Ord as Ord
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdCli
import Game.LambdaHack.CmdSer
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.EffectAction
import Game.LambdaHack.Server.SemAction
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Faction

-- | Start a clip (a part of a turn for which one or more frames
-- will be generated). Do whatever has to be done
-- every fixed number of time units, e.g., monster generation.
-- Run the leader and other actors moves. Eventually advance the time
-- and repeat.
loopSer :: MonadServerChan m => (CmdSer -> m ()) -> m ()
loopSer cmdSer = do
  -- Startup.
  quit <- getsState squit
  pers <- ask
  defLoc <- getsState localFromGlobal
  case quit of
    Nothing -> do  -- game restarted
      let bcast = funBroadcastCli (\fid -> RestartCli (pers IM.! fid) defLoc)
      bcast
      withAI bcast
      -- TODO: factor out common parts from restartGame and restoreOrRestart
      faction <- getsState sfaction
      let firstHuman = fst . head $ filter (isHumanFact . snd) $ IM.assocs faction
      switchGlobalSelectedSide firstHuman
      -- Save ASAP in case of crashes and disconnects.
      saveGameBkp
    _ -> do  -- game restored from a savefile
      let bcast = funBroadcastCli (\fid -> ContinueSavedCli (pers IM.! fid))
      bcast
      withAI bcast
  modifyState $ updateQuit $ const Nothing
  -- Loop.
  let loop previousHuman = do
        time <- getsState getTime  -- the end time of this clip, inclusive
        let clipN = (time `timeFit` timeClip)
                    `mod` (timeTurn `timeFit` timeClip)
        -- Regenerate HP and add monsters each turn, not each clip.
        when (clipN == 1) checkEndGame
        when (clipN == 2) regenerateLevelHP
        when (clipN == 3) generateMonster
        nHuman <- handleActors cmdSer timeZero previousHuman
        modifyState (updateTime (timeAdd timeClip))
        endOrLoop (loop nHuman)
  side <- getsState sside
  local (const pers) $ loop side

-- TODO: switch levels alternating between player factions,
-- if there are many and on distinct levels.
-- TODO: If a faction has no actors left in the dungeon,
-- announce game end for this faction. Not sure if here's the right place.
-- TODO: Let the spawning factions that remain duke it out somehow,
-- if the player requests to see it.
-- | If no actor of a non-spawning faction on the level,
-- switch levels. If no level to switch to, end game globally.
checkEndGame :: MonadServerChan m => m ()
checkEndGame = do
  -- Actors on the current level go first so that we don't switch levels
  -- unnecessarily.
  as <- getsState allActorsAnyLevel
  glo <- getState
  let aNotSp = filter (not . isSpawningFaction glo . bfaction . snd . snd) as
  case aNotSp of
    [] -> gameOver True
    (lid, _) : _ ->
      -- Switch to the level (can be the currently selected level, too).
      modifyState $ updateSelectedArena lid

-- TODO: We should replace this structure using a priority search queue/tree.
-- | Perform moves for individual actors, as long as there are actors
-- with the next move time less than or equal to the current time.
-- Some very fast actors may move many times a clip and then
-- we introduce subclips and produce many frames per clip to avoid
-- jerky movement. Otherwise we push exactly one frame or frame delay.
-- We start by updating perception, because the selected level of dungeon
-- has changed since last time (every change, whether by human or AI
-- or @generateMonster@ is followd by a call to @handleActors@).
handleActors :: MonadServerChan m
             => (CmdSer -> m ())
             -> Time  -- ^ start time of current subclip, exclusive
             -> FactionId
             -> m FactionId
handleActors cmdSer subclipStart previousHuman = withPerception $ do
  remember
  Kind.COps{coactor} <- getsState scops
  time <- getsState getTime  -- the end time of this clip, inclusive
   -- Older actors act earlier.
  lactor <- getsState (IM.toList . lactor . getArena)
  gquit <- getsState $ gquit . getSide
  quit <- getsState squit
  let mnext = if null lactor  -- wait until any actor spawned
              then Nothing
              else let -- Actors of the same faction move together.
                       order = Ord.comparing (btime . snd &&& bfaction . snd)
                       (actor, m) = L.minimumBy order lactor
                   in if btime m > time
                      then Nothing  -- no actor is ready for another move
                      else Just (actor, m)
  case mnext of
    _ | isJust quit || isJust gquit -> return previousHuman
    Nothing -> do
      when (subclipStart == timeZero) $
        broadcastPosUI [] $ DisplayDelayCli
      return previousHuman
    Just (actor, m) -> do
      let side = bfaction m
      switchGlobalSelectedSide side
      arena <- getsState sarena
      isHuman <- getsState $ flip isHumanFaction side
      leader <- if isHuman
                then sendQueryCli side $ SetArenaLeaderCli arena actor
                else withAI $ sendQueryCli side $ SetArenaLeaderCli arena actor
      if actor == leader && isHuman
        then do
          -- Human moves always start a new subclip.
          -- TODO: remove or only push to sside?
          broadcastPosUI [] $ DisplayPushCli
          nHuman <- if isHuman && side /= previousHuman
                    then do
                      newRuns <- sendQueryCli side IsRunningCli
                      if newRuns
                        then return previousHuman
                        else do
                          b <- sendQueryUI previousHuman $ FlushFramesCli side
                          if b
                            then return side
                            else return previousHuman
                    else return previousHuman
          (cmdS, leaderNew, arenaNew) <-
            sendQueryUI side $ HandleHumanCli leader
          modifyState $ updateSelectedArena arenaNew
          tryWith (\msg -> do
                      sendUpdateCli side $ ShowMsgCli msg
                      handleActors cmdSer subclipStart nHuman
                  ) $ do
            cmdSer cmdS
            -- Advance time once, after the leader switched perhaps many times.
            -- TODO: this is correct only when all heroes have the same
            -- speed and can't switch leaders by, e.g., aiming a wand
            -- of domination. We need to generalize by displaying
            -- "(next move in .3s [RET]" when switching leaders.
            -- RET waits .3s and gives back control,
            -- Any other key does the .3s wait and the action form the key
            -- at once. This requires quite a bit of refactoring
            -- and is perhaps better done when the other factions have
            -- selected leaders as well.
            squitNew <- getsState squit
            when (timedCmdSer cmdS && isNothing squitNew) $
              maybe (return ()) advanceTime leaderNew
            -- Human moves always start a new subclip.
            lpos <- getsState $ bpos . getActorBody (fromMaybe actor leaderNew)
            broadcastPosUI [lpos] $ DisplayPushCli
            handleActors cmdSer (btime m) nHuman
        else do
--          recordHistory
          advanceTime actor  -- advance time while the actor still alive
          let subclipStartDelta = timeAddFromSpeed coactor m subclipStart
          if isHuman && not (bproj m)
             || subclipStart == timeZero
             || btime m > subclipStartDelta
            then do
              -- Start a new subclip if its our own faction moving
              -- or it's another faction, but it's the first move of
              -- this whole clip or the actor has already moved during
              -- this subclip, so his multiple moves would be collapsed.
              -- TODO: store frames somewhere for each faction and display
              -- the frames only after a "Faction X taking over..." prompt.
              cmdS <- withAI $ sendQueryCli side $ HandleAI actor
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
              tryWith (\msg -> if T.null msg
                               then return ()
                               else assert `failure` msg <> "in AI"
                      )
                      (cmdSer cmdS)
              apos <- getsState $ bpos . getActorBody actor
              broadcastPosUI [apos] $ DisplayPushCli
              handleActors cmdSer (btime m) previousHuman
            else do
              -- No new subclip.
              cmdS <- withAI $ sendQueryCli side $ HandleAI actor
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
              tryWith (\msg -> if T.null msg
                               then return ()
                               else assert `failure` msg <> "in AI"
                      )
                      (cmdSer cmdS)
              handleActors cmdSer subclipStart previousHuman

-- | Advance (or rewind) the move time for the given actor.
advanceTime :: MonadServer m => ActorId -> m ()
advanceTime actor = do
  Kind.COps{coactor} <- getsState scops
  let upd m@Actor{btime} = m {btime = timeAddFromSpeed coactor m btime}
  modifyState $ updateActorBody actor upd
