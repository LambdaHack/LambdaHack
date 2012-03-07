-- | The main loop of the game, processing player and AI moves turn by turn.
module Game.LambdaHack.Turn ( handle ) where

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
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Msg
import Game.LambdaHack.Draw
import Game.LambdaHack.Content.ActorKind
import qualified Game.LambdaHack.Kind as Kind

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
    then handleAI False  -- the hero can't make a move yet; monsters first
    else handlePlayer    -- it's the hero's turn!

-- TODO: We should replace this structure using a priority search queue/tree.
-- | Handle monster moves. Perform moves for individual monsters as long as
-- there are monsters that have a move time which is less than or equal to
-- the current time.
handleAI :: Bool -> Action ()
handleAI dispAlready = do
  debug "handleAI"
  time <- gets stime
  ms   <- gets (monsterAssocs . slevel)
  ns   <- gets (neutralAssocs . slevel)
  pl   <- gets splayer
  if null ms
    then nextMove dispAlready
    else let order = Ord.comparing (btime . snd)
             (actor, m) = L.minimumBy order (ns ++ ms)
         in if btime m > time || actor == pl
            then nextMove dispAlready  -- no monster is ready for another move
            else handleMonster actor

-- | Handle the move of a single monster.
handleMonster :: ActorId -> Action ()
handleMonster actor = do
  debug "handleMonster"
  startTurn $ do
    advanceTime actor  -- advance time while the actor still alive
    cops  <- getCOps
    state <- get
    per <- getPerception
    -- Run the AI: choses an action from those given by the AI strategy.
    join $ rndToAction $
             frequency (head (runStrategy (strategy cops actor state per
                                           .| wait)))
    handleAI True

-- TODO: nextMove may not be a good name. It's part of the problem of the
-- current design that all of the top-level functions directly call each
-- other, rather than being called by a driver function.
-- | After everything has been handled for the current game time, we can
-- advance the time. Here is the place to do whatever has to be done for
-- every time unit, e.g., monster generation.
nextMove :: Bool -> Action ()
nextMove dispAlready = do
  debug "nextMove"
  unless dispAlready $ displayFramePush Nothing
  modify (updateTime (+1))
  regenerateLevelHP
  generateMonster
  handle

-- | Handle the move of the hero.
handlePlayer :: Action ()
handlePlayer = do
  debug "handlePlayer"
  startTurn $ do
    pl <- gets splayer
    -- If running, stop if aborted by a disturbance.
    -- Otherwise let the player issue commands, until any of them takes time.
    -- First time, just after pushing frames, ask for commands in Push mode.
    tryWith (\ msg -> stopRunning >> playerCommand msg) $
      ifRunning (\ x -> continueRun x >> advanceTime pl) abort
    -- TODO: refactor this:
    state <- get
    pl2    <- gets splayer
    let time = stime state
        ploc = bloc (getPlayerBody state)
        sTimeout = Config.get (sconfig state) "monsters" "smellTimeout"
    -- Update smell. Only humans leave a strong scent.
    when (isAHero state pl2) $
      modify (updateLevel (updateSmell (IM.insert ploc
                                         (Tile.SmellTime
                                            (time + sTimeout)))))
    -- The command took some time, so other actors act.
    handleAI True

-- | Determine and process the next player command, given the last
-- running abort message, if any.
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
            -- Show in turn all but the last frame.
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
  Kind.COps{coactor=Kind.Ops{okind}} <- getCOps
  time <- gets stime
  let upd m = m { btime = time + aspeed (okind (bkind m)) }
  -- A hack to synchronize the whole party:
  pl <- gets splayer
  updateAnyActor actor upd
  when (actor == pl) $ do
    let updH a = if bparty a == heroParty then upd a else a
    modify (updateLevel (updateActor (IM.map updH)))


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
