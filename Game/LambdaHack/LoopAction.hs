{-# LANGUAGE OverloadedStrings #-}
-- | The main loop of the server, processing player and AI moves turn by turn.
module Game.LambdaHack.LoopAction (handleTurn) where

import Control.Arrow ((&&&))
import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Ord as Ord
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Command
import Game.LambdaHack.CommandAction
import Game.LambdaHack.EffectAction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.ServerAction
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

-- | Start a clip (a part of a turn for which one or more frames
-- will be generated). Do whatever has to be done
-- every fixed number of time units, e.g., monster generation.
-- Run the leader and other actors moves. Eventually advance the time
-- and repeat.
handleTurn :: MonadServerChan m => m ()
handleTurn = do
  debug "handleTurn"
  time <- getsState getTime  -- the end time of this clip, inclusive
  let clipN = (time `timeFit` timeClip) `mod` (timeTurn `timeFit` timeClip)
  -- Regenerate HP and add monsters each turn, not each clip.
  when (clipN == 1) checkEndGame
  when (clipN == 2) regenerateLevelHP
  when (clipN == 3) generateMonster
  debug $ "handleTurn: time =" <+> showT time
  handleActors timeZero
  modifyState (updateTime (timeAdd timeClip))
  endOrLoop handleTurn

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
-- has changed since last time (every change, whether by player or AI
-- or @generateMonster@ is followd by a call to @handleActors@).
handleActors :: MonadServerChan m
             => Time  -- ^ start time of current subclip, exclusive
             -> m ()
handleActors subclipStart = withPerception $ do
  debug "handleActors"
  remember
  Kind.COps{coactor} <- getsState scops
  time <- getsState getTime  -- the end time of this clip, inclusive
   -- Older actors act earlier.
  lactor <- getsState (IM.toList . lactor . getArena)
  quit <- getsServer squit
  let mnext = if null lactor  -- wait until any actor spawned
              then Nothing
              else let -- Heroes move first then monsters, then the rest.
                       order = Ord.comparing (btime . snd &&& bfaction . snd)
                       (actor, m) = L.minimumBy order lactor
                   in if btime m > time
                      then Nothing  -- no actor is ready for another move
                      else Just (actor, m)
  case mnext of
    _ | isJust quit -> return ()
    Nothing -> when (subclipStart == timeZero) $
                 broadcastPosCli [] $ DisplayFramesPushCli [Nothing]
    Just (actor, m) -> do
      let side = bfaction m
      switchGlobalSelectedSide side
      arena <- getsState sarena
      leader <- sendQueryCli side $ SetArenaLeaderCli arena actor
      isPlayer <- getsState $ flip isPlayerFaction side
      if actor == leader && isPlayer
        then do
          -- Player moves always start a new subclip.
          broadcastPosCli [] $ DisplayPushCli
          (cmdS, leaderNew, arenaNew) <-
            sendQueryCli side $ HandlePlayerCli leader
          modifyState $ updateSelectedArena arenaNew
          tryWith (\msg -> do
                      sendUpdateCli side $ ShowMsgCli msg
                      handleActors subclipStart
                  )
                  $ do
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
            squitNew <- getsServer squit
            when (timedCmd cmdS && isNothing squitNew) $
              maybe (return ()) advanceTime leaderNew
            handleActors $ btime m
        else do
--          recordHistory
          advanceTime actor  -- advance time while the actor still alive
          let subclipStartDelta = timeAddFromSpeed coactor m subclipStart
          if isPlayer && not (bproj m)
             || subclipStart == timeZero
             || btime m > subclipStartDelta
            then do
              -- Start a new subclip if its our own faction moving
              -- or it's another faction, but it's the first move of
              -- this whole clip or the actor has already moved during
              -- this subclip, so his multiple moves would be collapsed.
              -- TODO: store frames somewhere for each faction and display
              -- the frames only after a "Faction X taking over..." prompt.
              broadcastPosCli [] $ DisplayPushCli
              cmdS <- sendAIQueryCli side $ HandleAI actor
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
              tryWith (\msg -> if T.null msg
                    then return ()
                    else assert `failure` msg <> "in AI") $ cmdSer cmdS
              handleActors $ btime m
            else do
              -- No new subclip.
              cmdS <- sendAIQueryCli side $ HandleAI actor
    -- If the following action aborts, we just advance the time and continue.
    -- TODO: or just fail at each abort in AI code? or use tryWithFrame?
              tryWith (\msg -> if T.null msg
                    then return ()
                    else assert `failure` msg <> "in AI") $ cmdSer cmdS
              handleActors subclipStart

-- | Advance (or rewind) the move time for the given actor.
advanceTime :: MonadServer m => ActorId -> m ()
advanceTime actor = do
  Kind.COps{coactor} <- getsState scops
  let upd m@Actor{btime} = m {btime = timeAddFromSpeed coactor m btime}
  modifyState $ updateActorBody actor upd
