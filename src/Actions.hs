module Actions where

import Control.Monad
import Control.Monad.State hiding (State)
import Data.Function
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S
import System.Time

import Action
import Actor hiding (updateActor)
import Display2 hiding (display)
import Dungeon
import Geometry
import Grammar
import qualified HighScores as H
import Item
import qualified Keys as K
import Level
import LevelState
import Message
import Monster
import Perception
import Random
import qualified Save as S
import State

displayHistory :: Action ()
displayHistory =
  do
    hst <- gets shistory
    messageOverlayConfirm "" (unlines hst)
    abort

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

cancelCurrent :: Action ()
cancelCurrent =
  do
    state <- get
    case slook state of
      Just lk -> do
                   cancelLook lk
      Nothing -> do
                   abortWith "Press Q to quit."

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
    let itemsHere       = not (L.null (titems t))
    let dirOK           = accessible lmap loc (loc `shift` dir)
    -- What happens next is mostly depending on the terrain we're currently on.
    let exit (Stairs  {}) = True
        exit (Opening {}) = True
        exit (Door    {}) = True
        exit _            = False
    let hop t
          | monstersVisible || newsReported || itemsHere || exit t = abort
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

neverMind :: Bool -> Action a
neverMind b = abortIfWith b "never mind"

-- | Open and close doors
openclose :: Bool -> Action ()
openclose o =
  do
    message "direction?"
    display
    e <- session nextCommand
    handleDirection e (actorOpenClose APlayer True o) (neverMind True)

actorOpenClose :: Actor ->
                  Bool ->    -- ^ verbose?
                  Bool ->    -- ^ open?
                  Dir -> Action ()
actorOpenClose actor v o dir =
  do
    let txt = if o then "open" else "closed"
    state <- get
    let lvl@Level { lmonsters = ms, lmap = lmap } = slevel state
    let loc                                       = mloc (getActor state actor)
    let isPlayer  = actor == APlayer
    let isVerbose = v && isPlayer
    let dloc = shift loc dir  -- location we act upon
      in case lmap `at` dloc of
           Tile d@(Door hv o') []
             | secret o' && isPlayer-> -- door is secret, cannot be opened or closed by hero
                                       neverMind isVerbose
             | toOpen (not o) /= o' -> -- door is in unsuitable state
                                       abortIfWith isVerbose ("already " ++ txt)
             | not (unoccupied ms lmap dloc) ->
                                       -- door is blocked by a monster
                                       abortIfWith isVerbose "blocked"
             | otherwise            -> -- door can be opened / closed
                                       -- TODO: print message if action performed by monster and perceived
                                       let nt    = Tile (Door hv (toOpen o)) []
                                           clmap = M.adjust (\ (_, mt) -> (nt, mt)) dloc lmap
                                       in  modify (updateLevel (const (updateLMap (const clmap) lvl)))
           Tile d@(Door hv o') _    -> -- door is jammed by items
                                       abortIfWith isVerbose "jammed"
           _                        -> -- there is no door here
                                       neverMind isVerbose

-- | Perform a level switch to a given level and location.
lvlswitch :: DungeonLoc -> Action ()
lvlswitch (nln, nloc) =
  do
    state <- get
    -- put back current level
    -- (first put back, then get, in case we change to the same level!)
    let full = putDungeonLevel (slevel state) (sdungeon state)
    -- get new level
    let (new, ndng) = getDungeonLevel nln full
    modify (\ s -> s { sdungeon = ndng, slevel = new })
    case slook state of
      Nothing -> modify (updatePlayer (\ p -> p { mloc = nloc }))
      Just (loc, tgt, ln) -> modify (updateLook (const $ Just (nloc, tgt, ln)))

 -- | Attempt a level switch to k levels deeper.
lvldescend :: Loc -> Int -> Action ()
lvldescend loc k =
  do
    state <- get
    case lname (slevel state) of
      LambdaCave n ->
        let nln = n + k
        -- TODO: 10 hardcoded; perhaps change getDungeonLevel?
        in  if nln >= 1 && nln <= 10
            then lvlswitch (LambdaCave nln, loc)
            else abortWith "no more levels in this direction"
      _ -> error "lvlchange"

-- | Attempt a level change via up level and down level keys.
-- Will quit the game if the player leaves the dungeon.
lvlchange :: VDir -> Action ()
lvlchange vdir =
  do
    state <- get
    let map = lmap (slevel state)
        loc = case slook state of
                Nothing -> mloc (splayer state)
                Just (loc, _tgt, _ln) -> loc
    case map `at` loc of
      Tile (Stairs _ vdir' next) is
        | vdir == vdir' -> -- stairs are in the right direction
          case next of
            Nothing ->
              -- we are at the "end" of the dungeon
              fleeDungeon
            Just (nln, nloc) ->
              -- TODO: in loook mode, check if the target stairs are known and  if not, land in the same location as in the current level so as not to reveal stairs location
              lvlswitch (nln, nloc)
      _ -> -- no stairs
        if isNothing (slook state)
        then
          let txt = if vdir == Up then "up" else "down"
          in  abortWith ("no stairs " ++ txt)
        else
          lvldescend loc (if vdir == Up then -1 else 1)

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
    cfg  <- gets sconfig
    time <- gets stime
    let points  = if killed then (total + 1) `div` 2 else total
    let current = levelNumber nm   -- TODO: rather use name of level
    curDate <- liftIO getClockTime
    let score   = H.ScoreRecord
                    points (-time) curDate current killed victor
    (placeMsg, slideshow) <- liftIO $ H.register cfg write score
    messageOverlaysConfirm placeMsg slideshow
    return ()

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

-- | Toggle look mode.
lookAround :: Action ()
lookAround =
  do
    state <- get
    case slook state of
      Just lk -> do
                   cancelLook lk
      Nothing -> do
                   lk <- setLook
                   doLook lk

-- | Set look mode.
setLook :: Action Look
setLook =
  do
    state <- get
    let loc = mloc (splayer state)
        tgt = TNone
        ln  = lname (slevel state)
    modify (updateLook (const $ Just (loc, tgt, ln)))
    return (loc, tgt, ln)

-- | Cancel look mode.
cancelLook :: Look -> Action ()
cancelLook (loc, tgt, ln) =
  do
    lvlswitch (ln, loc)
    modify (updatePlayer (\ p -> p { mtarget = tgt }))
    modify (updateLook (const Nothing))

-- | Perform look around in the current location of the cursor.
-- TODO: depending on tgt or an extra flag, show tile, monster or both
doLook :: Look -> Action ()
doLook (loc, _tgt, _ln) =
  do
    state <- get
    let map = lmap (slevel state)
    -- general info about current loc
        lookMsg = lookAt True state map loc
    -- check if there's something lying around at current loc
        t = map `at` loc
    if length (titems t) <= 2
      then do
             messageAdd lookMsg
      else do
             displayItems lookMsg False (titems t)
             session getConfirm
             messageAdd ""

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
               Nothing -> neverMind True

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
               Nothing -> neverMind True

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
          actorOpenClose actor False True dir
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
    let combatVerb = if killed && target /= APlayer then "kill" else "hit"
    let swordMsg   = if sword == 0 then "" else
                       " with a (+" ++ show sword ++ ") sword" -- TODO: generate proper message
    let combatMsg  = subjectVerbMObject state sm combatVerb tm swordMsg
    per <- currentPerception
    let perceived  = mloc sm `S.member` pvisible per
    messageAdd $
      if perceived
        then combatMsg
        else "You hear some noises."

-- | Generate a monster, possibly.
generateMonster :: Action ()
generateMonster =
  do
    state <- get
    nlvl  <- liftIO $ rndToIO $ addMonster state
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
