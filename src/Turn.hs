module Turn where

import System.Time
import Control.Monad
import Control.Monad.State hiding (State)
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Char
import Data.Maybe
import Data.Function

import Action
import State
import Geometry
import Level
import LevelState
import Dungeon
import Monster
import Actor hiding (updateActor)
import Perception
import Item
import ItemState
import Display2 hiding (display)
import Random
import qualified Save as S
import Message
import Version
import Strategy
import StrategyState
import qualified HighScores as H
import Grammar
import qualified Keys as K

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
    let ptime = mtime (splayer state)  -- time of hero's next move
    let time  = stime state            -- current game time
    checkHeroDeath     -- hero can die even if it's not the hero's turn
    regenerate APlayer -- hero can regenerate even if it's not the hero's turn
    debug $ "handle: time check. ptime = " ++ show ptime ++ ", time = " ++ show time
    if ptime > time
      then do
             -- the hero can't make a move yet; monsters first
             -- we redraw the map even between player moves so that the movements of fast
             -- monsters can be traced on the map; we disable this functionality if the
             -- player is currently running, as it would slow down the running process
             -- unnecessarily
             ifRunning (const $ return True) display
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
    ms   <- gets (lmonsters . slevel)
    time <- gets stime
    case ms of
      [] -> nextMove
      (m@(Monster { mtime = mt }) : ms)
        | mt > time  -> -- no monster is ready for another move
                        nextMove
        | mhp m <= 0 -> -- the monster dies
                        do
                          modify (updateLevel (updateMonsters (const ms)))
                          -- place the monster's possessions on the map
                          modify (updateLevel (scatterItems (mitems m) (mloc m)))
                          handleMonsters
        | otherwise  -> -- monster m should move; we temporarily remove m from the level
                        -- TODO: removal isn't nice. Actor numbers currently change during
                        -- a move. This could be cleaned up.
                        do
                          modify (updateLevel (updateMonsters (const ms)))
                          handleMonster m

-- | Handle the move of a single monster.
-- Precondition: monster must not currently be in the monster list of the level.
handleMonster :: Monster -> Action ()
handleMonster m =
  do
    debug "handleMonster"
    state <- get
    let time = stime state
    let ms   = lmonsters (slevel state)
    per <- currentPerception
    -- run the AI; it currently returns a direction; TODO: it should return an action
    dir <- liftIO $ rndToIO $ frequency (head (runStrategy (strategy m state per .| wait)))
    let waiting    = dir == (0,0)
    let nmdir      = if waiting then Nothing else Just dir
    -- advance time and reinsert monster
    let nm         = m { mtime = time + mspeed m, mdir = nmdir }
    let (act, nms) = insertMonster nm ms
    modify (updateLevel (updateMonsters (const nms)))
    let actor      = AMonster act
    try $ -- if the following action aborts, we just continue
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
    playerCommand -- get and process a player command
    -- at this point, the command was successful
    advanceTime APlayer     -- TODO: the command handlers should advance the move time
    state <- get
    let time = stime state
    let loc  = mloc (splayer state)
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

-- | Generate a monster, possibly.
generateMonster :: Action ()
generateMonster =
  do
    lvl    <- gets slevel
    player <- gets splayer
    nlvl   <- liftIO $ rndToIO $ addMonster lvl player
    modify (updateLevel (const nlvl))

-- | Advance the move time for the given actor.
advanceTime :: Actor -> Action ()
advanceTime actor =
  do
    time <- gets stime
    updateActor actor (\ m -> m { mtime = time + mspeed m })

-- | Possibly regenerate HP for the given actor.
regenerate :: Actor -> Action ()
regenerate actor =
  do
    time <- gets stime
    -- TODO: remove hardcoded time interval, regeneration should be an attribute of the monster
    when (time `mod` 1500 == 0) $
      updateActor actor (\ m -> m { mhp = min (mhpmax m) (mhp m + 1) })

-- | Display command help.
displayHelp :: Action ()
displayHelp = messageOverlayConfirm "Basic keys:" helpString >> abort
  where
  helpString = keyHelp stdKeybindings

displayHistory :: Action ()
displayHistory =
  do
    hst <- gets shistory
    messageOverlayConfirm "" (unlines hst)
    abort

data Described a = Described { chelp :: String, caction :: a }
                 | Undescribed { caction :: a }

type Command    = Described (Action ())
type DirCommand = Described (Dir -> Action ())

closeCommand     = Described "close a door"      (openclose False)
openCommand      = Described "open a door"       (openclose True)
pickupCommand    = Described "pick up an object" pickupItem
dropCommand      = Described "drop an object"    dropItem
inventoryCommand = Described "display inventory" inventory
searchCommand    = Described "search for secret doors" search
ascendCommand    = Described "ascend a level"    (lvlchange Up)
descendCommand   = Described "descend a level"   (lvlchange Down)
lookCommand      = Described "look around"       lookAround
drinkCommand     = Described "quaff a potion"    drinkPotion
waitCommand      = Described "wait"              (return ())
saveCommand      = Described "save and quit the game" saveGame
quitCommand      = Described "quit without saving" quitGame
helpCommand      = Described "display help"      displayHelp
historyCommand   = Described "display previous messages" displayHistory

moveDirCommand   = Described "move in direction" move
runDirCommand    = Described "run in direction"  run

-- | Keybindings.
data Keybindings = Keybindings
  { kdir   :: DirCommand,
    kudir  :: DirCommand,
    kother :: M.Map K.Key Command
  }

handleKey :: Keybindings -> K.Key -> Action ()
handleKey kb k =
  do
    handleDirection k (caction $ kdir kb) $
      handleUDirection k (caction $ kudir kb) $
        case M.lookup k (kother kb) of
          Just c  -> caction c
          Nothing -> abortWith $ "unknown command (" ++ K.showKey k ++ ")"

keyHelp :: Keybindings -> String
keyHelp kb =
  let
    fmt k h = replicate 15 ' ' ++ k ++ replicate ((13 - length k) `max` 1) ' ' ++
                                  h ++ replicate ((30 - length h) `max` 1) ' '
    fmts s  = replicate 15 ' ' ++ s ++ replicate ((43 - length s) `max` 1) ' '
    blank   = fmt "" ""
    title   = fmt "key" "command"
    footer  = fmts "(See file PLAYING.markdown.)"
    rest    = [ fmt (K.showKey k) h | (k, Described h _) <- M.toAscList (kother kb) ]
  in
    unlines ([blank, title] ++ rest ++ [blank, footer, blank])

stdKeybindings :: Keybindings
stdKeybindings = Keybindings
  { kdir   = moveDirCommand,
    kudir  = runDirCommand,
    kother = M.fromList $
             [ -- interaction with the dungeon
               (K.Char 'o',  openCommand),
               (K.Char 'c',  closeCommand),
               (K.Char 's',  searchCommand),

               (K.Char '<',  ascendCommand),
               (K.Char '>',  descendCommand),

               (K.Char ':',  lookCommand),

               -- items
               (K.Char ',',  pickupCommand),
               (K.Char 'd',  dropCommand),
               (K.Char 'i',  inventoryCommand),
               (K.Char 'q',  drinkCommand),

               -- wait
               -- (K.Char ' ',  waitCommand),
               (K.Char '.',  waitCommand),

               -- saving or ending the game
               (K.Char 'S',  saveCommand),
               (K.Char 'Q',  quitCommand),
               (K.Esc     ,  Undescribed $ abortWith "Press Q to quit."),

               -- debug modes
               (K.Char 'V',  Undescribed $ modify toggleVision     >> withPerception playerCommand),
               (K.Char 'R',  Undescribed $ modify toggleSmell      >> playerCommand),
               (K.Char 'O',  Undescribed $ modify toggleOmniscient >> playerCommand),
               (K.Char 'T',  Undescribed $ modify toggleTerrain    >> playerCommand),
               (K.Char 'I',  Undescribed $ gets (lmeta . slevel) >>= abortWith),

               -- information for the player
               (K.Char 'v',  Undescribed $ abortWith version),
               (K.Char 'M',  historyCommand),
               (K.Char '?',  helpCommand),
               (K.Return  ,  helpCommand)
             ]
  }

saveGame :: Action ()
saveGame =
  do
    b <- messageYesNo "Really save?"
    if b
      then do
        -- Save the game state
        st <- get
        liftIO $ S.saveGame st
        let total = calculateTotal (splayer st)
        handleScores False False False total
        end
      else abortWith "Game resumed."

quitGame :: Action ()
quitGame =
  do
    b <- messageYesNo "Really quit?"
    if b
      then end -- TODO: why no highscore?
      else abortWith "Game resumed."

move :: Dir -> Action ()
move = moveOrAttack True True APlayer

run :: Dir -> Action ()
run dir =
  do
    modify (updatePlayer (\ p -> p { mdir = Just dir }))
    moveOrAttack False False APlayer dir -- attacks and opening doors disallowed while running

-- | This function implements the actual "logic" of running. It checks if we
-- have to stop running because something interested happened, and it checks
-- if we have to adjust the direction because we're in the corner of a corridor.
continueRun :: Dir -> Action ()
continueRun dir =
  do
    state <- get
    let lvl   @(Level   { lmonsters = ms, lmap = lmap }) = slevel  state
    let player@(Monster { mloc = loc })                  = splayer state
    let mslocs = S.fromList (L.map mloc ms)
    let t      = lmap `at` loc  -- tile at current location
    per <- currentPerception
    msg <- currentMessage
    let monstersVisible = not (S.null (mslocs `S.intersection` pvisible per))
    let newsReported    = not (L.null msg)
    let dirOK           = accessible lmap loc (loc `shift` dir)
    -- What happens next is mostly depending on the terrain we're currently on.
    let exit (Stairs  {}) = True
        exit (Opening {}) = True
        exit (Door    {}) = True
        exit _            = False
    let hop t
          | monstersVisible || newsReported || exit t = abort
        hop Corridor =
          -- in corridors, explore all corners and stop at all crossings
          let ns = L.filter (\ x -> distance (neg dir, x) > 1
                                    && accessible lmap loc (loc `shift` x))
                            moves
              allCloseTo main = L.all (\ d -> distance (main, d) <= 1) ns
          in  case ns of
                [onlyDir] -> run onlyDir  -- can be diagonal
                _         ->
                  -- prefer orthogonal to diagonal dirs, for hero's safety
                  case L.filter (\ x -> not $ diagonal x) ns of
                    [ortoDir]
                      | allCloseTo ortoDir -> run ortoDir
                    _ -> abort
        hop _  -- outside corridors, never change direction
          | not dirOK = abort
        hop _         =
          let ns = L.filter (\ x -> x /= dir && distance (neg dir, x) > 1) moves
              ls = L.map (loc `shift`) ns
              as = L.filter (\ x -> accessible lmap loc x
                                    || openable 0 lmap x) ls
              ts = L.map (tterrain . (lmap `at`)) as
          in  if L.any exit ts then abort else run dir
    hop (tterrain t)

stopRunning :: Action ()
stopRunning = modify (updatePlayer (\ p -> p { mdir = Nothing }))

ifRunning :: (Dir -> Action a) -> Action a -> Action a
ifRunning t e =
  do
    mdir <- gets (mdir . splayer)
    maybe e t mdir

-- | Store current message in the history and reset current message.
history :: Action ()
history =
  do
    msg <- resetMessage
    unless (L.null msg) $
      modify (updateHistory (take 500 . ((msg ++ " "):)))
    -- TODO: make history max configurable

-- | Update player memory.
remember :: Action ()
remember =
  do
    per <- currentPerception
    let vis         = S.toList (pvisible per)
    let rememberLoc = M.update (\ (t,_) -> Just (t,t))
    modify (updateLevel (updateLMap (\ lmap -> foldr rememberLoc lmap vis)))

checkHeroDeath :: Action ()
checkHeroDeath =
  do
    player <- gets splayer
    let php = mhp player
    when (php <= 0) $ do
      messageAdd more
      display
      session getConfirm
      go <- messageMoreConfirm "You die."
      when go $ do
        let total = calculateTotal player
        handleScores True True False total
      end

neverMind :: Action a
neverMind = abortWith "never mind"

-- | Open and close doors
openclose :: Bool -> Action ()
openclose o =
  do
    message "direction?"
    display
    e <- session nextCommand
    handleDirection e openclose' neverMind
  where
    txt = if o then "open" else "closed"
    openclose' dir =
      do
        lvl@Level { lmonsters = ms, lmap = lmap } <- gets slevel
        Monster { mloc = ploc }                   <- gets splayer
        let dloc = shift ploc dir  -- location we act upon
          in case lmap `at` dloc of
               Tile d@(Door hv o') []
                 | secret o'            -> -- door is secret, cannot open or close
                                           neverMind
                 | toOpen (not o) /= o' -> -- door is in unsuitable state
                                           abortWith ("already " ++ txt)
                 | not (unoccupied ms lmap dloc) ->
                                           -- door is blocked by a monster
                                           abortWith "blocked"
                 | otherwise            -> -- door can be opened / closed
                                           let nt    = Tile (Door hv (toOpen o)) []
                                               clmap = M.insert dloc (nt, nt) lmap
                                           in  modify (updateLevel (const (updateLMap (const clmap) lvl)))
               Tile d@(Door hv o') _    -> -- door is jammed by items
                                           abortWith "jammed"
               _                        -> -- there is no door here
                                           neverMind

-- | Perform a level change -- will quit the game if the player leaves
-- the dungeon.
lvlchange :: VDir -> Action ()
lvlchange vdir =
  do
    state <- get
    let lvl   @(Level   { lmap = lmap }) = slevel  state
    let player@(Monster { mloc = ploc }) = splayer state
    case lmap `at` ploc of
      Tile (Stairs _ vdir' next) is
        | vdir == vdir' -> -- stairs are in the right direction
          case next of
            Nothing ->
              -- we are at the "end" of the dungeon
              fleeDungeon
            Just (nln, nloc) ->
              -- perform level change
              do
                -- put back current level
                -- (first put back, then get, in case we change to the same level!)
                let full = putDungeonLevel lvl (sdungeon state)
                -- get new level
                let (new, ndng) = getDungeonLevel nln full
                modify (\ s -> s { sdungeon = ndng, slevel = new })
                modify (updatePlayer (\ p -> p { mloc = nloc }))
      _ -> -- no stairs
        do
          let txt = if vdir == Up then "up" else "down"
          abortWith ("no stairs " ++ txt)

-- | Hero has left the dungeon.
fleeDungeon :: Action ()
fleeDungeon =
  do
    player@(Monster { mitems = items }) <- gets splayer
    let total = calculateTotal player
    if total == 0
      then do
             messageMoreConfirm "Coward!"
             messageMoreConfirm "Next time try to grab some loot before you flee!"
             end
      else do
             let winMsg = "Congratulations, you won! Your loot, worth " ++
                          show total ++ " gold, is:"
             displayItems winMsg True items
             go <- session getConfirm
             when go $ handleScores True False True total
             end

-- | Calculate loot's worth. TODO: move to another module, and refine significantly.
calculateTotal :: Player -> Int
calculateTotal player = L.sum $ L.map price $ mitems player
  where
    price i = if iletter i == Just '$' then icount i else 10 * icount i

-- | Handle current score and display it with the high scores. TODO: simplify. Scores
-- should not be shown during the game, because ultimately the worth of items might give
-- information about the nature of the items.
handleScores :: Bool -> Bool -> Bool -> Int -> Action ()
handleScores write killed victor total =
  unless (total == 0) $ do
    nm   <- gets (lname . slevel)
    cfg  <- gets config
    time <- gets stime
    let points  = if killed then (total + 1) `div` 2 else total
    let current = levelNumber nm   -- TODO: rather use name of level
    curDate <- liftIO getClockTime
    let score   = H.ScoreRecord
                    points (-time) curDate current killed victor
    (placeMsg, slideshow) <- liftIO $ H.register cfg write score
    mapM_ (messageOverlayConfirm placeMsg) slideshow  -- TODO: check this

-- | Search for secret doors
search :: Action ()
search =
  do
    Level   { lmap = lmap } <- gets slevel
    Monster { mloc = ploc } <- gets splayer
    let searchTile (Tile (Door hv (Just n)) x,t') = Just (Tile (Door hv (Just (max (n - 1) 0))) x, t')
        searchTile t                              = Just t
        slmap = foldl (\ l m -> update searchTile (shift ploc m) l) lmap moves
    modify (updateLevel (updateLMap (const slmap)))

-- | Look around at current location
lookAround :: Action a
lookAround =
  do
    state <- get
    let lvl@(Level   { lmap = lmap }) = slevel  state
    let      Monster { mloc = ploc }  = splayer state
    -- general info about current loc
    let lookMsg = lookAt True state lmap ploc
    -- check if there's something lying around at current loc
    let t = lmap `at` ploc
    if length (titems t) <= 2
      then do
             abortWith lookMsg
      else do
             displayItems lookMsg False (titems t)
             session getConfirm
             abortWith ""

-- | Display inventory
inventory :: Action a
inventory =
  do
    player <- gets splayer
    if L.null (mitems player)
      then abortWith "You are not carrying anything"
      else do
             displayItems "This is what you are carrying:" True (mitems player)
             session getConfirm
             abortWith ""

-- | Given item is now known to the player.
discover :: Item -> Action ()
discover i = modify (updateDiscoveries (S.insert (itype i)))

drinkPotion :: Action ()
drinkPotion =
  do
    state <- get
    let lvl   @(Level   { lmap = lmap }) = slevel  state
    let player@(Monster { mloc = ploc }) = splayer state
    if L.null (mitems player)
      then abortWith "You are not carrying anything."
      else do
             i <- getPotion "What to drink?" (mitems player) "inventory"
             case i of
               Just i'@(Item { itype = Potion ptype }) ->
                 do
                   -- only one potion is consumed even if several are joined in the inventory
                   let consumed = i' { icount = 1 }
                   removeFromInventory consumed
                   message (subjectVerbIObject state player "drink" consumed "")
                   -- the potion is identified after drinking
                   discover i'
                   case ptype of
                     PotionWater   -> messageAdd "Tastes like water."
                     PotionHealing -> do
                                        messageAdd "You feel better."
                                        modify (updatePlayer (\ p -> p { mhp = min (mhpmax p) (mhp p + playerHP `div` 4) }))
               Just _  -> abortWith "you cannot drink that"
               Nothing -> neverMind

dropItem :: Action ()
dropItem =
  do
    state <- get
    let player@(Monster { mloc = ploc }) = splayer state
    if L.null (mitems player)
      then abortWith "You are not carrying anything."
      else do
             i <- getAnyItem "What to drop?" (mitems player) "inventory"
             case i of
               Just i' ->
                 do
                   removeFromInventory i'
                   message (subjectVerbIObject state player "drop" i' "")
                   dropItemsAt [i'] ploc
               Nothing -> neverMind

dropItemsAt :: [Item] -> Loc -> Action ()
dropItemsAt is loc = modify (updateLevel (scatterItems is loc))

-- | Remove given item from the hero's inventory.
removeFromInventory :: Item -> Action ()
removeFromInventory i =
  modify (updatePlayer (\ p -> p { mitems = removeItemByLetter i (mitems p) }))

-- | Remove given item from the given location.
removeFromLoc :: Item -> Loc -> Action ()
removeFromLoc i loc =
  modify (updateLevel (\ l -> l { lmap = M.adjust (\ (t, rt) -> (update t, rt)) loc (lmap l) }))
  where
    update t = t { titems = removeItemByType i (titems t) }

-- | Let the player choose any potion. Note that this does not guarantee a potion to be chosen,
-- as the player can override the choice.
getPotion :: String ->  -- prompt
             [Item] ->  -- all objects in question
             String ->  -- how to refer to the collection of objects, e.g. "in your inventory"
             Action (Maybe Item)
getPotion prompt is isn = getItem prompt (\ i -> case itype i of Potion {} -> True; _ -> False)
                                  "Potions" is isn

actorPickupItem :: Actor -> Action ()
actorPickupItem actor =
  do
    state <- get
    per   <- currentPerception
    let lvl@(Level { lmap = lmap }) = slevel state
    let monster   = getActor state actor
    let loc       = mloc monster
    let t         = lmap `at` loc -- the map tile in question
    let perceived = loc `S.member` pvisible per
    let isPlayer  = actor == APlayer
    -- check if something is here to pick up
    case titems t of
      []     -> abortIfWith isPlayer "nothing here"
      (i:rs) -> -- pick up first item; TODO: let player select item; not for monsters
        case assignLetter (iletter i) (mletter monster) (mitems monster) of
          Just l ->
            do
              let (ni, nitems) = joinItem (i { iletter = Just l }) (mitems monster)
              -- message is dependent on who picks up and if the hero can perceive it
              if isPlayer
                then message (letterLabel (iletter ni) ++ objectItem state (icount ni) (itype ni))
                else when perceived $
                       message $ subjectCompoundVerbIObject state monster "pick" "up" i ""
              removeFromLoc i loc
              -- add item to actor's inventory:
              updateActor actor $ \ m ->
                m { mitems = nitems, mletter = maxLetter l (mletter monster) }
          Nothing -> abortIfWith isPlayer "you cannot carry any more"

-- | Replaces the version in Actor module
updateActor :: Actor ->                 -- ^ who to update
               (Monster -> Monster) ->  -- ^ the update
               Action ()
updateActor (AMonster n) f =
  do
    monsters <- gets (lmonsters . slevel)
    let (m, ms) = updateMonster f n monsters
    modify (updateLevel (updateMonsters (const ms)))
updateActor APlayer f =
  modify (updatePlayer f)

pickupItem :: Action ()
pickupItem = actorPickupItem APlayer

-- TODO: I think that player handlers should be wrappers around more general actor handlers, but
-- the actor handlers should be performing specific actions, i.e., already specify the item to be
-- picked up. It doesn't make sense to invoke dialogues for arbitrary actors, and most likely the
-- decision for a monster is based on perceiving a particular item to be present, so it's already
-- known. In actor handlers we should make sure that messages are printed to the player only if the
-- hero can perceive the action.

-- | Let the player choose any item from a list of items.
getAnyItem :: String ->  -- prompt
              [Item] ->  -- all objects in question
              String ->  -- how to refer to the collection of objects, e.g. "in your inventory"
              Action (Maybe Item)
getAnyItem prompt is isn = getItem prompt (const True) "Objects" is isn

-- | Let the player choose a single item from a list of items.
-- TODO: Currently makes reference to the inventory and thereby restricts possible use cases.
getItem :: String ->              -- prompt message
           (Item -> Bool) ->      -- which items to consider suitable
           String ->              -- how to describe suitable objects
           [Item] ->              -- all objects in question
           String ->              -- how to refer to the collection of objects, e.g. "in your inventory"
           Action (Maybe Item)
getItem prompt p ptext is0 isn =
  let is = L.filter p is0
      choice | L.null is = "[*]"
             | otherwise = "[" ++ letterRange (concatMap (maybeToList . iletter) is) ++ " or ?*]"
      r = do
            message (prompt ++ " " ++ choice)
            display
            let h = session nextCommand >>= h'
                h' e = case e of
                         K.Char '?' -> do
                                         -- filter for supposedly suitable objects
                                         b <- displayItems (ptext ++ " " ++ isn) True is
                                         if b then session (getOptionalConfirm (const r) h')
                                              else r
                         K.Char '*' -> do
                                         -- show all objects
                                         b <- displayItems ("Objects " ++ isn) True is0
                                         if b then session (getOptionalConfirm (const r) h')
                                              else r
                         K.Char l   -> return (find (\ i -> maybe False (== l) (iletter i)) is0)
                         _          -> return Nothing
            h
  in r

displayItems :: Message -> Bool -> [Item] -> Action Bool
displayItems msg sorted is =
    do
      state <- get
      let inv = unlines $
                L.map (\ (Item { icount = c, iletter = l, itype = t }) ->
                         letterLabel l ++ objectItem state c t ++ " ")
                      ((if sorted then sortBy (cmpLetter' `on` iletter) else id) is)
      let ovl = inv ++ more
      message msg
      overlay ovl

-- | This function performs a move (or attack) by any actor, i.e., it can handle
-- both monsters and the player.
moveOrAttack :: Bool ->        -- allow attacks?
                Bool ->        -- auto-open doors on move
                Actor ->       -- who's moving?
                Dir ->
                Action ()
moveOrAttack allowAttacks autoOpen actor dir
  | dir == (0,0) =
      -- Moving with no direction is a noop. We include it currently to prevent that
      -- monsters attack themselves by accident.
      return ()
  | otherwise =
    do
      -- We start by looking at the target position.
      state <- get
      let lvl@(Level { lmap = lmap }) = slevel  state
      let player                      = splayer state
      let monster = getActor state actor
      let loc     = mloc monster     -- current location
      let s       = lmap `at` loc    -- tile at current location
      let nloc    = loc `shift` dir  -- target location
      let t       = lmap `at` nloc   -- tile at target location
      let attackedPlayer   = [ APlayer | mloc player == nloc ]
      let attackedMonsters = L.map AMonster $
                             findIndices (\ m -> mloc m == nloc) (lmonsters lvl)
      let attacked :: [Actor]
          attacked = attackedPlayer ++ attackedMonsters
      -- At the moment, we check whether there is a monster before checking accessibility
      -- i.e., we can attack a monster on a blocked location. For instance,
      -- a monster on an open door can be attacked diagonally, and a
      -- monster capable of moving through walls can be attacked from an
      -- adjacent position.
      if not (L.null attacked)
        then if not allowAttacks then abort else do
          -- perform the attack
          mapM_ (actorAttackActor actor) attacked
        else if accessible lmap loc nloc then do
          -- perform the move
          updateActor actor (\ m -> m { mloc = nloc })
          when (actor == APlayer) $ message $ lookAt False state lmap nloc
            -- TODO: seems somewhat dubious to do this here, but perhaps it's ok
        else if autoOpen then
          -- try to check if there's a door we can open
          -- TODO: this should actually reuse some code from openclose
          abort -- undefined
        else abort -- nothing useful we can do

actorAttackActor :: Actor -> Actor -> Action ()
actorAttackActor source target =
  do
    debug "actorAttackActor"
    state <- get
    let sm = getActor state source
    let tm = getActor state target
    -- determine the weapon used for the attack
    let sword = strongestSword (mitems sm)
    -- damage the target
    let newHp  = mhp tm - 3 - sword
    let killed = newHp <= 0
    updateActor target $ \ m ->
      if killed
        then m { mhp = 0, mtime = 0 } -- grant an immediate move to die
        -- TODO: is there a good reason not to let the monster die just here?
        else m { mhp = newHp }
    -- determine how the hero perceives the event; TODO: we have to be more
    -- precise and treat cases where two monsters fight, but only one is visible
    let combatVerb = if killed then "kill" else "hit"
    let swordMsg   = if sword == 0 then "" else
                       " with a (+" ++ show sword ++ ") sword" -- TODO: generate proper message
    let combatMsg  = subjectVerbMObject state sm combatVerb tm swordMsg
    per <- currentPerception
    let perceived  = mloc sm `S.member` pvisible per
    messageAdd $
      if perceived
        then combatMsg
        else "You hear some noises."
