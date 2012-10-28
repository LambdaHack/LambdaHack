-- | The main loop of the game, processing player and AI moves turn by turn.
module Game.LambdaHack.Turn ( handleGame ) where

import Control.Monad
import Control.Monad.State hiding (State, state)
import Control.Arrow ((&&&))
import qualified Data.List as L
import qualified Data.Ord as Ord
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.EffectAction
import qualified Game.LambdaHack.Binding as Binding
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Level
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

-- One clip proceeds through the following functions:
--
-- handleTurn
-- handleActors
-- handleMonster or handlePlayer
-- handleActors
-- handleMonster or handlePlayer
-- ...
-- handleTurn (again)

-- What's happening where:
--
-- handleTurn: HP regeneration, monster generation, determine who moves next,
--   dispatch to handlePlayer and handleActors, advance global game time
--
-- handleActors: find an actor that can move, advance actor time,
--   update perception, remember, push frame, repeat
--
-- handlePlayer: update perception, remember, display frames,
--   get and process commmands (zero or more), update smell map
--
-- handleMonster: determine and process monster action

-- | Start a clip (a part of a turn for which one or more frames
-- will be generated). Do whatever has to be done
-- every fixed number of time units, e.g., monster generation.
-- Run the player and other actors moves. Eventually advance the time
-- and repeat, and if the game ends or exits, handle the diary
-- and backup savefile.
handleGame :: Action ()
handleGame = handleTurn >> rmBkpSaveDiary

handleTurn :: Action ()
handleTurn = do
  debug "handleTurn"
  time <- gets stime  -- the end time of this clip, inclusive
  let clipN = (time `timeFit` timeClip) `mod` (timeTurn `timeFit` timeClip)
  -- Regenerate HP and add monsters each turn, not each clip.
  when (clipN == 1) regenerateLevelHP
  when (clipN == 3) generateMonster
  ptime <- gets (btime . getPlayerBody)  -- time of player's next move
  debug $ "handleTurn: time check. ptime = "
          ++ show ptime ++ ", time = " ++ show time
  handleActors timeZero
  modify (updateTime (timeAdd timeClip))
  -- FIXME: a hack
  squit <- gets squit
  case squit of
    Just (_, Camping) -> do
      pl <- gets splayer
      advanceTime False pl  -- rewind player time: this is just Save&Exit
    _ -> return ()
  endOrLoop handleTurn



-- TODO: We should replace this structure using a priority search queue/tree.
-- | Perform moves for individual actors not controlled
-- by the player, as long as there are actors
-- with the next move time less than or equal to the current time.
-- Some very fast actors may move many times a clip and then
-- we introduce subclips and produce many frames per clip to avoid
-- jerky movement. Otherwise we push exactly one frame or frame delay.
handleActors :: Time       -- ^ the start time of current subclip, exclusive
             -> Action ()
handleActors subclipStart = do
  debug "handleActors"
  Kind.COps{coactor} <- getCOps
  sfaction <- gets sfaction
  time <- gets stime  -- the end time of this clip, inclusive
  lactor <- gets (allButHeroesAssocs sfaction . slevel)
  pl <- gets splayer
  pbody <- gets getPlayerBody
  squit <- gets squit
  let as = (pl, pbody) : lactor  -- older actors act first
      mnext = if null as  -- wait until any actor spawned
              then Nothing
              else let -- Heroes move first then monsters, then the rest.
                       order = Ord.comparing (btime . snd &&& bfaction . snd)
                       (actor, m) = L.minimumBy order as
                   in if btime m > time
                      then Nothing  -- no actor is ready for another move
                      else Just (actor, m)
  case mnext of
    _ | isJust squit -> return ()
    Nothing -> when (subclipStart == timeZero) $ displayFramePush Nothing
    Just (actor, m) -> do
      advanceTime True actor  -- advance time while the actor still alive
      if actor == pl || bfaction m == sfaction && bai m == Nothing
        then
          -- Player moves always start a new subclip.
          startClip $ do
            handlePlayer
            handleActors $ btime m
        else
          let speed = actorSpeed coactor m
              delta = ticksPerMeter speed
          in if subclipStart == timeZero
                || btime m > timeAdd subclipStart delta
             then
               -- That's the first move this clip
               -- or the monster has already moved this subclip.
               -- I either case, start a new subclip.
               startClip $ do
                 handleMonster actor
                 handleActors $ btime m
             else do
               -- The monster didn't yet move this subclip.
               handleMonster actor
               handleActors subclipStart

-- | Handle the move of a single monster.
handleMonster :: ActorId -> Action ()
handleMonster actor = do
  debug "handleMonster"
  cops  <- getCOps
  state <- get
  per <- getPerception
  body <- gets (getActor actor)
  let strategy = case bai body of
        Nothing -> assert `failure` ("handleMonster: not AI controlled", actor)
        Just AIDefender   -> strategyDefender
        Just AIProjectile -> strategyProjectile
  -- Run the AI: choses an action from those given by the AI strategy.
  join $ rndToAction $
           frequency (head (runStrategy (strategy cops actor state per)))

-- | Handle the move of the hero.
handlePlayer :: Action ()
handlePlayer = do
  debug "handlePlayer"
  -- When running, stop if aborted by a disturbance.
  -- Otherwise let the player issue commands, until any of them takes time.
  -- First time, just after pushing frames, ask for commands in Push mode.
  tryWith (\ msg -> stopRunning >> playerCommand msg) $
    ifRunning continueRun abort
  addSmell

-- | Determine and process the next player command. The argument is the last
-- abort message due to running, if any.
playerCommand :: Msg -> Action ()
playerCommand msgRunAbort = do
  -- The frame state is now Push.
  Binding.Binding{kcmd} <- getBinding
  kmPush <- case msgRunAbort of
    "" -> getKeyCommand (Just True)
    _  -> drawPrompt ColorFull msgRunAbort >>= getKeyFrameCommand
  -- The frame state is now None and remains so between each pair
  -- of lines of @loop@ (but can change within called actions).
  let loop :: (K.Key, K.Modifier) -> Action ()
      loop km = do
        -- Messages shown, so update history and reset current report.
        recordHistory
        -- On abort, just reset state and call loop again below.
        (timed, frames) <- tryWithFrame (return False) $ do
          -- Look up the key.
          case M.lookup km kcmd of
            Just (_, declaredTimed, c) -> do
              ((), frs) <- c
              -- Targeting cursor movement and a few other subcommands
              -- are wrongly marked as timed. This is indicated in their
              -- definitions by setting @snoTime@ flag and used and reset here.
              stakeTime <- gets stakeTime
              let timed = fromMaybe declaredTimed stakeTime
              modify (\ s -> s {stakeTime = Nothing})
              -- Ensure at least one frame, if the command takes no time.
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
              Just fr | b -> getKeyFrameCommand fr
              _           -> getKeyCommand Nothing
            -- Look up and perform the next command.
            loop kmNext
          else do
            -- Exit the loop and let other actors act. No next key needed
            -- and no frames could have been generated.
            assert (null frames `blame` length frames) $
              return ()
  loop kmPush

-- | Advance (or rewind) the move time for the given actor.
advanceTime :: Bool -> ActorId -> Action ()
advanceTime forward actor = do
  Kind.COps{coactor} <- getCOps
  pl <- gets splayer
  sfaction <- gets sfaction
  let upd m@Actor{btime} =
        let speed = actorSpeed coactor m
            ticks = ticksPerMeter speed
            delta | forward   = ticks
                  | otherwise = timeNegate ticks
        in m {btime = timeAdd btime delta}
  updateAnyActor actor upd
  -- A hack to synchronize the whole party:
  body <- gets (getActor actor)
  when (actor == pl) $ do
    let updParty m = if bfaction m == sfaction && bai m == Nothing
                     then m {btime = btime body}
                     else m
    modify (updateLevel (updateActorDict (IM.map updParty)))


-- The issues below are now complicated (?) by the fact that we now generate
-- a game screen frame at least once every clip and a jointed pair
-- of frame+key input for each command that does not take time.
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
