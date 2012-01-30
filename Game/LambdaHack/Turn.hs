-- | The main loop of the game, processing player and AI moves turn by turn.
module Game.LambdaHack.Turn ( handle ) where

import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.Ord as Ord
import qualified Data.IntMap as IM
import qualified Data.Map as M

import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import qualified Game.LambdaHack.Config as Config
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
import qualified Game.LambdaHack.Tile as Tile

-- One turn proceeds through the following functions:
--
-- handle
-- handleAI, handleMonster
-- nextMove
-- handle (again)
--
-- OR:
--
-- handle
-- handlePlayer, playerCommand
-- handleAI, handleMonster
-- nextMove
-- handle (again)
--
-- What's happening where:
--
-- handle: determine who moves next,
--   dispatch to handleAI or handlePlayer
--
-- handlePlayer: remember, display, get and process commmand(s),
--   update smell map, update perception
--
-- handleAI: find monsters that can move
--
-- handleMonster: determine and process monster action, advance monster time
--
-- nextMove: advance global game time, HP regeneration, monster generation
--
-- This is rather convoluted, and the functions aren't named very aptly, so we
-- should clean this up later. TODO.

-- | Decide if the hero is ready for another move.
-- If yes, run a player move, if not, run an AI move.
-- In either case, eventually the next turn is started or the game ends.
handle :: Action ()
handle = do
  debug "handle"
  state <- get
  let ptime = btime (getPlayerBody state)  -- time of player's next move
  let time  = stime state                  -- current game time
  debug $ "handle: time check. ptime = "
          ++ show ptime ++ ", time = " ++ show time
  if ptime > time
    then handleAI  -- the hero can't make a move yet; monsters first
    else handlePlayer    -- it's the hero's turn!

    -- TODO: readd this, but only for the turns when anything moved
    -- and only after a rendering delay is added, so that the move is visible
    -- on modern computers. Use the same delay for running (not disabled now).
    -- We redraw the map even between player moves so that the movements of fast
    -- monsters can be traced on the map.
    -- displayGeneric ColorFull (const "")

-- TODO: We should replace this structure using a priority search queue/tree.
-- | Handle monster moves. Perform moves for individual monsters as long as
-- there are monsters that have a move time which is less than or equal to
-- the current time.
handleAI :: Action ()
handleAI = do
  debug "handleAI"
  time <- gets stime
  ms   <- gets (lmonsters . slevel)
  pl   <- gets splayer
  if IM.null ms
    then nextMove
    else let order  = Ord.comparing (btime . snd)
             (i, m) = L.minimumBy order (IM.assocs ms)
             actor = AMonster i
         in if btime m > time || actor == pl
            then nextMove  -- no monster is ready for another move
            else handleMonster actor

-- | Handle the move of a single monster.
handleMonster :: ActorId -> Action ()
handleMonster actor = do
  debug "handleMonster"
  cops  <- contentOps
  state <- get
  -- Simplification: this is the perception after the last player command
  -- and does not take into account, e.g., other monsters opening doors.
  per <- currentPerception
  -- Run the AI: choses an action from those given by the AI strategy.
  action <-
    rndToAction $
      frequency (head (runStrategy (strategy cops actor state per
                                    .| wait actor)))
  action
  handleAI

-- TODO: nextMove may not be a good name. It's part of the problem of the
-- current design that all of the top-level functions directly call each
-- other, rather than being called by a driver function.
-- | After everything has been handled for the current game time, we can
-- advance the time. Here is the place to do whatever has to be done for
-- every time unit, e.g., monster generation.
nextMove :: Action ()
nextMove = do
  debug "nextMove"
  modify (updateTime (+1))
  regenerateLevelHP
  generateMonster
  handle

-- | Handle the move of the hero.
handlePlayer :: Action ()
handlePlayer = do
  debug "handlePlayer"
  -- Determine perception before running player command, in case monsters
  -- have opened doors, etc.
  withPerception $ do
    remember  -- The hero notices his surroundings, before they get displayed.
    oldPlayerTime <- gets (btime . getPlayerBody)
    playerCommand
    -- At this point, the command was successful and possibly took some time.
    newPlayerTime <- gets (btime . getPlayerBody)
    if newPlayerTime == oldPlayerTime
      then handlePlayer  -- no time taken, repeat
      else do
        state <- get
        pl    <- gets splayer
        let time = stime state
            ploc = bloc (getPlayerBody state)
            sTimeout = Config.get (sconfig state) "monsters" "smellTimeout"
        -- Update smell. Only humans leave a strong scent.
        when (isAHero pl) $
          modify (updateLevel (updateSmell (IM.insert ploc
                                             (Tile.SmellTime
                                                (time + sTimeout)))))
        -- Determine perception to let monsters target heroes.
        withPerception handleAI

-- | Determine and process the next player command.
playerCommand :: Action ()
playerCommand = do
  displayAll -- draw the current surroundings
  history    -- update the message history and reset current message
  tryRepeatedlyWith stopRunning $  -- on abort, just ask for a new command
    ifRunning continueRun $ do
      k <- session nextCommand
      session (\ Session{skeyb} ->
                case M.lookup k (Binding.kcmd skeyb) of
                  Just (_, c)  -> c
                  Nothing ->
                    abortWith $ "unknown command (" ++ show k ++ ")")

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
