-- | The main loop of the game, processing player and AI moves turn by turn.
module Game.LambdaHack.Turn ( handleTurn ) where

import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.Ord as Ord
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.EffectAction
import qualified Game.LambdaHack.Binding as Binding
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.Strategy
import Game.LambdaHack.StrategyAction
import Game.LambdaHack.Running
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Msg
import Game.LambdaHack.Draw
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Time

-- One turn proceeds through the following functions:
--
-- handleTurn
-- [handleAI, handleMonster] (possibly repeated)
-- handleTurn (again)
--
-- OR:
--
-- handleTurn
-- handlePlayer
-- playerCommand (skipped if player running)
-- [handleAI, handleMonster] (possibly repeated)
-- handleTurn (again)
--
-- What's happening where:
--
-- handleTurn: HP regeneration, monster generation, determine who moves next,
--   dispatch to handlePlayer and handleAI, advance global game time,
--
-- handlePlayer: update perception, remember, display frames,
--   get and process commmands (zero or more), advance time, update smell map
--
-- handleAI: find monsters that can move, update perception, remember,
--   push frames, perhaps a few times, for very fast actors (missiles)
--
-- handleMonster: determine and process monster action, advance monster time

-- | Starting the turn. Do whatever has to be done
-- every fixed number of time unit, e.g., monster generation.
-- Decide if the hero is ready for another move.
-- If yes, run a player move, if not, run an AI move.
-- Eventually advance the time and repeat.
handleTurn :: Action ()
handleTurn = do
  debug "handleTurn"
  time <- gets stime  -- the end time of this turn, inclusive
  let turnN = (time `timeFit` timeTurn) `mod` (timeStep `timeFit` timeTurn)
  -- Regenerate HP and add monsters each step, not each turn.
  when (turnN == 3) regenerateLevelHP
  when (turnN == 8) generateMonster
  ptime <- gets (btime . getPlayerBody)  -- time of player's next move
  debug $ "handleTurn: time check. ptime = "
          ++ show ptime ++ ", time = " ++ show time
  if ptime > time
    then handleAI timeZero -- the hero can't move this turn; monsters move
    else do
      handlePlayer       -- the hero starts this turn
      handleAI timeZero  -- monster follow
  modify (updateTime (timeAdd timeTurn))
  handleTurn

-- TODO: We should replace this structure using a priority search queue/tree.
-- | Perform moves for individual actors not controlled
-- by the player, as long as there are actors
-- with the next move time less than or equal to the current time.
-- Some very fast actors may move many times a turn and then
-- we introduce subturns and produce many frames per turn to avoid
-- jerky movement. Otherwise we push exactly one frame or frame delay.
handleAI :: Time       -- ^ the start time of current subturn, exclusive
         -> Action ()
handleAI subturnStart = do
  debug "handleAI"
  Kind.COps{coactor} <- getCOps
  time <- gets stime  -- the end time of this turn, inclusive
  ms   <- gets (monsterAssocs . slevel)
  ns   <- gets (neutralAssocs . slevel)
  pl   <- gets splayer
  let done = when (subturnStart == timeZero) $ displayFramePush Nothing
      as = ns ++ ms
  if null as
    then done
    else let order = Ord.comparing (btime . snd)
             (actor, m) = L.minimumBy order as
             mtime = btime m
         in if mtime > time || actor == pl
            then done  -- no monster is ready for another move
            else let speed = actorSpeed coactor m
                     ticks = ticksPerMeter speed
                 in if subturnStart == timeZero
                       || mtime > timeAdd subturnStart ticks
                    then do
                      -- That's the first non-player move this turn
                      -- or the monster has already moved this subturn.
                      -- I either case, start a new subturn.
                      startTurn $ do
                        handleMonster actor
                        handleAI mtime
                    else do
                      -- The monster didn't yet move this subturn.
                      handleMonster actor
                      handleAI subturnStart

-- | Handle the move of a single monster.
handleMonster :: ActorId -> Action ()
handleMonster actor = do
  debug "handleMonster"
  advanceTime actor  -- advance time while the actor still alive
  cops  <- getCOps
  state <- get
  per <- getPerception
  -- Run the AI: choses an action from those given by the AI strategy.
  join $ rndToAction $
           frequency (head (runStrategy (strategy cops actor state per
                                         .| wait)))

-- | Handle the move of the hero.
handlePlayer :: Action ()
handlePlayer = do
  debug "handlePlayer"
  startTurn $ do
    pl <- gets splayer
    -- When running, stop if aborted by a disturbance.
    -- Otherwise let the player issue commands, until any of them takes time.
    -- First time, just after pushing frames, ask for commands in Push mode.
    tryWith (\ msg -> stopRunning >> playerCommand msg) $
      ifRunning (\ x -> continueRun x >> advanceTime pl) abort
    addSmell
    -- The command took some time, so other actors act.
    -- We don't let the player act twice a turn, because we assume
    -- player speed is less or equal to one move/turn (200 m/10s).

-- | Determine and process the next player command. The argument is the last
-- abort message due to running, if any.
playerCommand :: Msg -> Action ()
playerCommand msgRunAbort = do
  -- The frame state is now Push.
  Binding.Binding{kcmd, kdir} <- getBinding
  kmPush <- case msgRunAbort of
    "" -> getKeyCommand (Just True)
    _  -> drawPrompt ColorFull msgRunAbort >>= getKeyChoice []
  -- The frame state is now None and remains so between each pair
  -- of lines of @loop@ (but can change within called actions).
  let loop :: (K.Key, K.Modifier) -> Action ()
      loop km = do
        -- On abort, just reset state and call loop again below.
        (timed, frames) <- tryWithFrame (return False) $ do
          -- Messages shown, so update history and reset current report.
          -- On abort, history gets reset to the old value, so nothing changes.
          recordHistory
          -- Look up the key.
          case M.lookup km kcmd of
            Just (_, timed', c) -> do
              oldTargeting <- gets (ctargeting . scursor)
              ((), frs) <- c
              -- Targeting cursor movement and projecting that degenerates
              -- into targeting are wrongly marked as timed; fixed here.
              newTargeting <- gets (ctargeting . scursor)
              let timed = timed'
                          && (km `notElem` kdir || oldTargeting == TgtOff)
                          && (newTargeting == TgtOff || oldTargeting /= TgtOff)
              -- Ensure at least one frame if the command takes no time.
              -- No frames for @abort@, so the code is here, not below.
              if not timed && null (catMaybes frs)
                then do
                  fr <- drawPrompt ColorFull ""
                  return (timed, [Just fr])
                else return (timed, frs)
            Nothing -> let msgKey = "unknown command <" ++ K.showKM km ++ ">"
                       in abortWith msgKey
        -- The command was aborted or successful and if the latter,
        -- possibly took some time.
        if not timed
          then do
            -- If no time taken, rinse and repeat.
            -- Analyse the obtained frames.
            let (mfr, frs) = case reverse $ catMaybes frames of
                  []     -> (Nothing, [])
                  f : fs -> (Just f, reverse fs)
            -- Show, one by one, all but the last frame.
            -- Note: the code that generates the frames is responsible
            -- for inserting the @more@ prompt.
            b <- getOverConfirm frs
            -- Display the last frame while waiting for the next key or,
            -- if there is no next frame, just get the key.
            kmNext <- case mfr of
              Just fr | b -> getKeyChoice [] fr
              _           -> getKeyCommand Nothing
            -- Look up and perform the next command.
            loop kmNext
          else do
            -- If some time should be taken, take it, exit the loop
            -- and let other actors act. No next key needed
            -- and no frames could have been generated.
            pl <- gets splayer
            assert (null frames `blame` length frames) $
              advanceTime pl
  loop kmPush

-- | Advance the move time for the given actor.
advanceTime :: ActorId -> Action ()
advanceTime actor = do
  Kind.COps{coactor} <- getCOps
  let upd m@Actor{btime} =
        let speed = actorSpeed coactor m
            ticks = ticksPerMeter speed
        in m { btime = timeAdd btime ticks }
  updateAnyActor actor upd
  -- A hack to synchronize the whole party:
  pl <- gets splayer
  when (actor == pl) $ do
    let updH a m = if bparty m == heroParty && a /= pl then upd m else m
    modify (updateLevel (updateActorDict (IM.mapWithKey updH)))


-- The issues below are now complicated (?) by the fact that we now generate
-- a game screen frame every tick and a jointed pair of frame+key input
-- for each command that does not take time.
--
-- Design thoughts (in order to get rid or partially rid of the somewhat
-- convoluted design we have): We have three kinds of commands.
--
-- Normal commands: they take time, so after handling the command, state changes,
-- time passes and monsters get to move.
--
-- Instant commands: they take no time, and do not change the state.
--
-- Meta commands: they take no time, but may change the state.
--
-- Ideally, they can all be handled via the same (event) interface. We maintain an
-- event queue where we store what has to be handled next. The event queue is a sorted
-- list where every event contains the timestamp when the event occurs. The current game
-- time is equal to the head element of the event queue. Currently, we only have action
-- events. An actor gets to move on an event. The actor is responsible for reinsterting
-- itself in the event queue. Possible new events may include HP regeneration events,
-- monster generation events, or actor death events.
--
-- If an action does not take any time, the actor just reinserts itself with the current
-- time into the event queue. If the insert algorithm makes sure that later events with
-- the same time get precedence, this will work just fine.
--
-- It's important that we decouple issues like HP regeneration from action events if we
-- do it like that, because otherwise, HP regeneration may occur multiple times.
--
-- Given this scheme, we may get orphaned events: a HP regeneration event for a dead
-- monster may be scheduled. Or a move event for a monster suddenly put to sleep. We
-- therefore have to given handlers the option of accessing and cleaning up the event
-- queue.
