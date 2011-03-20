module Turn where

import Control.Monad
import Control.Monad.State hiding (State)
import Data.List as L
import Data.Map as M
import qualified Data.Ord as Ord
import qualified Data.IntMap as IM
import qualified Data.Char as Char

import Action
import Actions
import Actor
import Command
import qualified Config
import Display2 hiding (display)
import Keybindings
import qualified Keys as K
import Level
import Monster
import Random
import State
import Strategy
import StrategyState
import Version

-- One turn proceeds through the following functions:
--
-- handle
-- handleMonsters, handleMonster
-- nextMove
-- handle (again)
--
-- OR:
--
-- handle
-- handlePlayer, playerCommand
-- handleMonsters, handleMonster
-- nextMove
-- handle (again)
--
-- What's happening where:
--
-- handle: check for hero's death, HP regeneration, determine who moves next,
--   dispatch to handleMonsters or handlePlayer
--
-- handlePlayer: remember, display, get and process commmand(s),
--   advance player time, update smell map, update perception
--
-- handleMonsters: find monsters that can move or die
--
-- handleMonster: determine and process monster action, advance monster time
--
-- nextMove: advance global game time, monster generation
--
-- This is rather convoluted, and the functions aren't named very aptly, so we
-- should clean this up later. TODO.

-- | Decide if the hero is ready for another move. Dispatch to either 'handleMonsters'
-- or 'handlePlayer'.
handle :: Action ()
handle =
  do
    debug "handle"
    state <- get
    pl <- gets splayer
    let ptime = mtime (getPlayerBody state)  -- time of hero's next move
    let time  = stime state            -- current game time
    regenerate pl  -- heroes regenerate even if outside the player's turn
    debug $ "handle: time check. ptime = " ++ show ptime ++ ", time = " ++ show time
    if ptime > time
      then do
             -- the hero can't make a move yet; monsters first
             -- we redraw the map even between player moves so that the movements of fast
             -- monsters can be traced on the map; we disable this functionality if the
             -- player is currently running, as it would slow down the running process
             -- unnecessarily
             ifRunning (const $ return True) displayWithoutMessage
             handleMonsters
      else do
             handlePlayer -- it's the hero's turn!

-- | Handle monster moves. Perform moves for individual monsters as long as
-- there are monsters that have a move time which is less than or equal to
-- the current time.
handleMonsters :: Action ()
handleMonsters =
  do
    debug "handleMonsters"
    time <- gets stime
    ms   <- gets (lmonsters . slevel)
    if IM.null ms
      then nextMove
      else let order  = Ord.comparing (mtime . snd)
               (i, m) = L.minimumBy order (IM.assocs ms)
           in  if mtime m > time
               then nextMove  -- no monster is ready for another move
               else handleMonster (AMonster i)

-- | Handle the move of a single monster.
handleMonster :: Actor -> Action ()
handleMonster actor =
  do
    debug "handleMonster"
    state <- get
    time <- gets stime
    per <- currentPerception
    -- run the AI; it currently returns a direction
    -- TODO: it should return an action
    dir <- liftIO $ rndToIO $
           frequency (head (runStrategy (strategy actor state per .| wait)))
    let waiting = dir == (0,0)
    let nmdir   = if waiting then Nothing else Just dir
    -- advance time and update monster
    updateAnyActor actor $ \ m -> m { mtime = time + mspeed m, mdir = nmdir }
    try $  -- if the following action aborts, we just continue
      if waiting
        then
          -- monster is not moving, let's try to pick up an object
          actorPickupItem actor
        else
          moveOrAttack True True actor dir
    handleMonsters

-- | After everything has been handled for the current game time, we can
-- advance the time. Here is the place to do whatever has to be done for
-- every time unit; currently, that's monster generation.
-- TODO: nextMove may not be a good name. It's part of the problem of the
-- current design that all of the top-level functions directly call each
-- other, rather than being called by a driver function.
nextMove :: Action ()
nextMove =
  do
    debug "nextMove"
    modify (updateTime (+1))
    generateMonster
    handle

-- | Handle the move of the hero.
handlePlayer :: Action ()
handlePlayer =
  do
    debug "handlePlayer"
    remember      -- the hero perceives his (potentially new) surroundings
    -- determine perception before running player command, in case monsters
    -- have opened doors ...
    withPerception playerCommand -- get and process a player command
    -- at this point, the command was successful
    pl <- gets splayer
    advanceTime pl  -- TODO: the command handlers should advance the move time
    state <- get
    let time = stime state
        loc  = mloc (getPlayerBody state)
        smellTimeout = Config.get (sconfig state) "monsters" "smellTimeout"
    -- update smell
    modify (updateLevel (updateSMap (M.insert loc (time + smellTimeout))))
    -- determine player perception and continue with monster moves
    withPerception handleMonsters

-- | Determine and process the next player command.
playerCommand :: Action ()
playerCommand =
  do
    display -- draw the current surroundings
    history -- update the message history and reset current message
    tryRepeatedlyWith stopRunning $ do -- on abort, just ask for a new command
      ifRunning continueRun $ do
        k <- session nextCommand
        handleKey stdKeybindings k

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

-- The remaining functions in this module are individual actions or helper
-- functions.

-- TODO: Should be defined in Command module.
helpCommand      = Described "display help"      displayHelp

-- | Display command help. TODO: Should be defined in Actions module.
displayHelp :: Action ()
displayHelp = messageOverlayConfirm "Basic keys:" helpString >> abort
  where
  helpString = keyHelp stdKeybindings

heroSelection :: [(K.Key, Command)]
heroSelection =
  let heroSelect k = (K.Char (Char.intToDigit k),
                      Undescribed $
                      selectPlayer (AHero k) >> withPerception playerCommand)
  in  fmap heroSelect [0..9]

stdKeybindings :: Keybindings
stdKeybindings = Keybindings
  { kdir   = moveDirCommand,
    kudir  = runDirCommand,
    kother = M.fromList $
             heroSelection ++
             [ -- interaction with the dungeon
               (K.Char 'o',  openCommand),
               (K.Char 'c',  closeCommand),
               (K.Char 's',  searchCommand),

               (K.Char '<',  ascendCommand),
               (K.Char '>',  descendCommand),

               (K.Char '*',  monsterCommand),
               (K.Char '/',  floorCommand),
               (K.Char ':',  floorCommand),  -- synonym for backward compat.
               (K.Tab     ,  Described "cycle among heroes on level" $ cycleHero >> playerCommand),

               -- items
               (K.Char ',',  pickupCommand),
               (K.Char 'd',  dropCommand),
               (K.Char 'i',  inventoryCommand),
               (K.Char 'q',  drinkCommand),
               (K.Char 'f',  fireCommand),
               (K.Char 'a',  applyCommand),

               -- wait
               -- (K.Char ' ',  waitCommand),
               (K.Char '.',  waitCommand),

               -- saving or ending the game
               (K.Char 'S',  saveCommand),
               (K.Char 'Q',  quitCommand),
               (K.Esc     ,  cancelCommand),

               -- debug modes
               (K.Char 'V',  Undescribed $ modify toggleVision     >> withPerception playerCommand),
               (K.Char 'R',  Undescribed $ modify toggleSmell      >> playerCommand),
               (K.Char 'O',  Undescribed $ modify toggleOmniscient >> playerCommand),
               (K.Char 'T',  Undescribed $ modify toggleTerrain    >> playerCommand),
               (K.Char 'I',  Undescribed $ gets (lmeta . slevel) >>= abortWith),

               -- information for the player
               (K.Char 'v',  Undescribed $ abortWith version),
               (K.Char 'M',  historyCommand),
               (K.Char 'D',  dumpCommand),
               (K.Char '?',  helpCommand),
               (K.Return  ,  acceptCommand displayHelp)
             ]
  }
