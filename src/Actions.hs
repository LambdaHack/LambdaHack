module Actions where

import Control.Monad
import Control.Monad.State hiding (State)
import Data.Function
import Data.List as L
import Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Set as S
import System.Time

import Action
import Display hiding (display)
import Dungeon
import Geometry
import Grammar
import qualified HighScores as H
import Item
import qualified ItemKind
import ItemState
import qualified Keys as K
import Level
import LevelState
import Message
import Movable
import MovableState
import MovableKind
import MovableAdd
import Perception
import Random
import State
import qualified Config
import qualified Save
import Terrain
import qualified Effect

displayHistory :: Action ()
displayHistory =
  do
    hst <- gets shistory
    messageOverlayConfirm "" (unlines hst)
    abort

dumpConfig :: Action ()
dumpConfig =
  do
    config <- gets sconfig
    let fn = "LambdaHack.config.dump"
    liftIO $ Config.dump fn config
    abortWith $ "Current configuration dumped to file " ++ fn ++ "."

saveGame :: Action ()
saveGame =
  do
    b <- messageYesNo "Really save?"
    if b
      then do
        -- Save the game state
        st <- get
        liftIO $ Save.saveGame st
        ln <- gets (lname . slevel)
        let total = calculateTotal st
            status = H.Camping ln
        go <- handleScores False status total
        when go $ messageMore "See you soon, stronger and braver!"
        end
      else abortWith "Game resumed."

quitGame :: Action ()
quitGame =
  do
    b <- messageYesNo "Really quit?"
    if b
      then end -- TODO: why no highscore? no display, because the user may be in a hurry, since he quits the game instead of getting himself killed properly? no score recording, not to polute the scores list with games that the player didn't even want to end honourably?
      else abortWith "Game resumed."

-- | End targeting mode, accepting the current location or not.
endTargeting :: Bool -> Action ()
endTargeting accept = do
  returnLn <- gets (creturnLn . scursor)
  target   <- gets (mtarget . getPlayerBody)
  cloc     <- gets (clocation . scursor)
  lvlswitch returnLn  -- return to the original level of the player
  modify (updateCursor (\ c -> c { ctargeting = False }))
  let isEnemy = case target of TEnemy _ -> True ; _ -> False
  when (not isEnemy) $
    if accept
       then updatePlayerBody (\ p -> p { mtarget = TLoc cloc })
       else updatePlayerBody (\ p -> p { mtarget = TCursor })
  endTargetingMsg

endTargetingMsg :: Action ()
endTargetingMsg = do
  pkind    <- gets (mkind . getPlayerBody)
  target <- gets (mtarget . getPlayerBody)
  state  <- get
  let verb = "target"
      targetMsg = case target of
                    TEnemy a ->
                      case findActorAnyLevel a state of
                        Just (_, m) -> objectMovable (mkind m)
                        Nothing     -> "a long gone adversary"
                    TLoc loc -> "location " ++ show loc
                    TCursor  -> "current cursor position continuously"
  messageAdd $ subjectMovableVerb pkind verb ++ " " ++ targetMsg ++ "."

-- | Cancel something, e.g., targeting mode, resetting the cursor
-- to the position of the player. Chosen target is not invalidated.
cancelCurrent :: Action ()
cancelCurrent = do
  targeting <- gets (ctargeting . scursor)
  if targeting
    then endTargeting False
    else abortWith "Press Q to quit."

-- | Accept something, e.g., targeting mode, keeping cursor where it was.
-- Or perform the default action, if nothing needs accepting.
acceptCurrent :: Action () -> Action ()
acceptCurrent h = do
  targeting <- gets (ctargeting . scursor)
  if targeting
    then endTargeting True
    else h  -- nothing to accept right now

moveCursor :: Dir -> Int -> Action ()
moveCursor dir n = do
  (sy, sx) <- gets (lsize . slevel)
  let upd cursor =
        let (ny, nx) = iterate (`shift` dir) (clocation cursor) !! n
            cloc = (max 1 $ min ny (sy-1), max 1 $ min nx (sx-1))
        in  cursor { clocation = cloc }
  modify (updateCursor upd)
  doLook

move :: Dir -> Action ()
move dir = do
  pl <- gets splayer
  targeting <- gets (ctargeting . scursor)
  if targeting then moveCursor dir 1 else moveOrAttack True True pl dir

run :: Dir -> Action ()
run dir = do
  pl <- gets splayer
  targeting <- gets (ctargeting . scursor)
  if targeting
    then moveCursor dir 10
    else do
      updatePlayerBody (\ p -> p { mdir = Just dir })
      -- attacks and opening doors disallowed while running
      moveOrAttack False False pl dir

-- | This function implements the actual "logic" of running. It checks if we
-- have to stop running because something interested happened, and it checks
-- if we have to adjust the direction because we're in the corner of a corridor.
continueRun :: Dir -> Action ()
continueRun dir =
  do
    state <- get
    loc   <- gets (mloc . getPlayerBody)
    per   <- currentPerception
    msg   <- currentMessage
    ms    <- gets (lmonsters . slevel)
    hs    <- gets (lheroes . slevel)
    lmap  <- gets (lmap . slevel)
    pl    <- gets splayer
    let dms = case pl of
                AMonster n -> IM.delete n ms  -- don't be afraid of yourself
                AHero _ -> ms
        mslocs = S.fromList (L.map mloc (IM.elems dms))
        monstersVisible = not (S.null (mslocs `S.intersection` ptvisible per))
        newsReported    = not (L.null msg)
        t         = lmap `at` loc  -- tile at current location
        itemsHere = not (L.null (titems t))
        heroThere = L.elem (loc `shift` dir) (L.map mloc (IM.elems hs))
        dirOK     = accessible lmap loc (loc `shift` dir)
    -- What happens next is mostly depending on the terrain we're currently on.
    let exit (Stairs  {}) = True
        exit (Opening {}) = True
        exit (Door    {}) = True
        exit _            = False
    let hop t
          | monstersVisible || heroThere
            || newsReported || itemsHere || exit t = abort
        hop Corridor =
          -- in corridors, explore all corners and stop at all crossings
          -- TODO: even in corridors, stop if you run past an exit (rare)
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
stopRunning = updatePlayerBody (\ p -> p { mdir = Nothing })

ifRunning :: (Dir -> Action a) -> Action a -> Action a
ifRunning t e =
  do
    mdir <- gets (mdir . getPlayerBody)
    maybe e t mdir

-- | Store current message in the history and reset current message.
history :: Action ()
history =
  do
    msg <- resetMessage
    config <- gets sconfig
    let historyMax = Config.get config "ui" "historyMax"
    unless (L.null msg) $
      modify (updateHistory (take historyMax . ((msg ++ " "):)))

-- | Update player memory.
remember :: Action ()
remember =
  do
    per <- currentPerception
    let vis         = S.toList (ptvisible per)
    let rememberLoc = M.update (\ (t,_) -> Just (t,t))
    modify (updateLevel (updateLMap (\ lmap -> foldr rememberLoc lmap vis)))

-- | Remove dead heroes, check if game over.
-- For now we only check the selected hero, but if poison, etc.
-- is implemented, we'd need to check all heroes on the level.
checkPartyDeath :: Action ()
checkPartyDeath =
  do
    ahs    <- gets allHeroesAnyLevel
    pl     <- gets splayer
    pbody  <- gets getPlayerBody
    config <- gets sconfig
    when (mhp pbody <= 0) $ do  -- TODO: change to guard? define mzero? Why are the writes to to files performed when I call abort later? That probably breaks the laws of MonadPlus.
      messageAddMore
      go <- messageMoreConfirm $ subjectMovableVerb (mkind pbody) "die" ++ "."
      let firstDeathEnds = Config.get config "heroes" "firstDeathEnds"
      if firstDeathEnds
        then gameOver go
        else case L.filter (\ (actor, _) -> actor /= pl) ahs of
               [] -> gameOver go
               (actor, nln) : _ -> do
                 -- Important invariant: player always has to exist somewhere.
                 -- Make the new actor the player-controlled actor.
                 modify (\ s -> s { splayer = actor })
                 -- Record the original level of the new player.
                 modify (updateCursor (\ c -> c { creturnLn = nln }))
                 -- Now the old player can be safely removed.
                 modify (deleteActor pl)
                 -- Now we can switch to the level of the new player.
                 lvlswitch nln
                 message "The survivors carry on."

-- | End game, showing the ending screens, if requested.
gameOver :: Bool -> Action ()
gameOver showEndingScreens =
  do
    when showEndingScreens $ do
      state <- get
      ln <- gets (lname . slevel)
      let total = calculateTotal state
          status = H.Killed ln
      handleScores True status total
      messageMore "Let's hope another party can save the day!"
    end

neverMind :: Bool -> Action a
neverMind b = abortIfWith b "never mind"

-- | Open and close doors
openclose :: Bool -> Action ()
openclose o =
  do
    message "direction?"
    display
    e  <- session nextCommand
    pl <- gets splayer
    K.handleDirection e (actorOpenClose pl True o) (neverMind True)

actorOpenClose :: Actor ->
                  Bool ->    -- ^ verbose?
                  Bool ->    -- ^ open?
                  Dir -> Action ()
actorOpenClose actor v o dir =
  do
    state <- get
    lmap  <- gets (lmap . slevel)
    pl    <- gets splayer
    body  <- gets (getActor actor)
    let txt = if o then "open" else "closed"
    let hms = levelHeroList state ++ levelMonsterList state
    let loc = mloc body
    let isPlayer  = actor == pl
    let isVerbose = v && isPlayer
    let dloc = shift loc dir  -- location we act upon
      in case lmap `at` dloc of
           Tile d@(Door hv o') []
             | secret o' && isPlayer -> -- door is secret, cannot be opened or closed by the player
                                       neverMind isVerbose
             | maybe o ((|| not o) . (> (niq (mkind body)))) o' ->
                                       -- door is in unsuitable state
                                       abortIfWith isVerbose ("already " ++ txt)
             | not (unoccupied hms dloc) ->
                                       -- door is blocked by a movable
                                       abortIfWith isVerbose "blocked"
             | otherwise            -> -- door can be opened / closed
                                       -- TODO: print message if action performed by monster and perceived
                                       let nt  = Tile (Door hv (toOpen o)) []
                                           adj = M.adjust (\ (_, mt) -> (nt, mt)) dloc
                                       in  modify (updateLevel (updateLMap adj))
           Tile d@(Door hv o') _    -> -- door is jammed by items
                                       abortIfWith isVerbose "jammed"
           _                        -> -- there is no door here
                                       neverMind isVerbose
    advanceTime actor

-- | Perform a level switch to a given level. False, if nothing to do.
lvlswitch :: LevelName -> Action Bool
lvlswitch nln =
  do
    ln <- gets (lname . slevel)
    if (nln == ln)
      then return False
      else do
        level   <- gets slevel
        dungeon <- gets sdungeon
        -- put back current level
        -- (first put back, then get, in case we change to the same level!)
        let full = putDungeonLevel level dungeon
        -- get new level
        let (new, ndng) = getDungeonLevel nln full
        modify (\ s -> s { sdungeon = ndng, slevel = new })
        return True

 -- | Attempt a level switch to k levels deeper.
-- TODO: perhaps set up some level name arithmetics in Level.hs
-- and hide there the fact levels are now essentially Ints.
lvldescend :: Int -> Action ()
lvldescend k =
  do
    state <- get
    let n = levelNumber (lname (slevel state))
        nln = n + k
    when (nln < 1 || nln > sizeDungeon (sdungeon state) + 1) $
      abortWith "no more levels in this direction"
    assertTrue $ liftM (k == 0 ||)  (lvlswitch (LambdaCave nln))

-- | Attempt a level change via up level and down level keys.
-- Will quit the game if the player leaves the dungeon.
lvlchange :: VDir -> Action ()
lvlchange vdir =
  do
    cursor    <- gets scursor
    targeting <- gets (ctargeting . scursor)
    pbody     <- gets getPlayerBody
    pl        <- gets splayer
    map       <- gets (lmap . slevel)
    let loc = if targeting then clocation cursor else mloc pbody
    case map `at` loc of
      Tile (Stairs _ vdir' next) is
        | vdir == vdir' -> -- stairs are in the right direction
          case next of
            Nothing ->
              -- we are at the "end" of the dungeon
              if targeting
              then abortWith "cannot escape dungeon in targeting mode"
              else do
                b <- messageYesNo "Really escape the dungeon?"
                if b
                  then fleeDungeon
                  else abortWith "Game resumed."
            Just (nln, nloc) -> do
              if targeting
                then do
                  -- this assertion says no stairs go back to the same level
                  assertTrue $ lvlswitch nln
                  -- do not freely reveal the other end of the stairs
                  map <- gets (lmap . slevel)  -- lvlswitch modifies map
                  let upd cursor =
                        let cloc = if Level.isUnknown (rememberAt map nloc)
                                   then loc
                                   else nloc
                        in  cursor { clocation = cloc, clocLn = nln }
                  modify (updateCursor upd)
                  doLook
                else do
                  -- Remove the player from the old level.
                  modify (deleteActor pl)
                  -- At this place the invariant that player exists fails.
                  -- Change to the new level (invariant not essential).
                  assertTrue $ lvlswitch nln
                  -- Restore the invariant: add player to the new level.
                  modify (insertActor pl pbody)
                  -- Land the player at the other end of the stairs.
                  updatePlayerBody (\ p -> p { mloc = nloc })
                  -- Change the level of the player recorded in cursor.
                  modify (updateCursor (\ c -> c { creturnLn = nln }))
                  -- Create a backup of the savegame.
                  state <- get
                  liftIO $ Save.saveGame state >> Save.mvBkp (sconfig state)
                  playerAdvanceTime
      _ -> -- no stairs
        if targeting
        then do
          lvldescend (if vdir == Up then -1 else 1)
          ln <- gets (lname . slevel)
          let upd cursor = cursor { clocLn = ln }
          modify (updateCursor upd)
          doLook
        else
          let txt = if vdir == Up then "up" else "down"
          in  abortWith ("no stairs " ++ txt)

-- | Hero has left the dungeon.
fleeDungeon :: Action ()
fleeDungeon =
  do
    state <- get
    let total = calculateTotal state
        items = L.concatMap mitems (levelHeroList state)
    if total == 0
      then do
             go <- messageMoreConfirm "Coward!"
             when go $
               messageMore "Next time try to grab some loot before escape!"
             end
      else do
             let winMsg = "Congratulations, you won! Your loot, worth " ++
                          show total ++ " gold, is:"
             displayItems winMsg True items
             go <- session getConfirm
             when go $ do
               go <- handleScores True H.Victor total
               when go $ messageMore "Can it be done better, though?"
             end

-- | Switches current hero to the next hero on the level, if any, wrapping.
cycleHero :: Action ()
cycleHero =
  do
    pl <- gets splayer
    hs <- gets (lheroes . slevel)
    let i        = case pl of AHero n -> n ; _ -> -1
        (lt, gt) = IM.split i hs
    case IM.keys gt ++ IM.keys lt of
      [] -> abortWith "Cannot select another hero on this level."
      ni : _ -> assertTrue $ selectPlayer (AHero ni)

-- | Selects a movable for the player, based on the actor.
-- Focuses on the hero if level changed. False, if nothing to do.
selectPlayer :: Actor -> Action Bool
selectPlayer actor =
  do
    pl <- gets splayer
    if (actor == pl)
      then return False -- already selected
      else do
        state <- get
        case findActorAnyLevel actor state of
          Nothing -> abortWith $ "No such member of the party."
          Just (nln, pbody) -> do
            -- Make the new actor the player-controlled actor.
            modify (\ s -> s { splayer = actor })
            -- Record the original level of the new player.
            modify (updateCursor (\ c -> c { creturnLn = nln }))
            -- Switch to the level.
            lvlswitch nln
            -- Announce.
            messageAdd $ subjectMovable (mkind pbody) ++ " selected."
            return True

-- | Handle current score and display it with the high scores. Scores
-- should not be shown during the game, because ultimately the worth of items might give
-- information about the nature of the items.
-- False if display of the scores was void or interrupted by the user
handleScores :: Bool -> H.Status -> Int -> Action Bool
handleScores write status total =
  if (total == 0)
  then return False
  else do
    config  <- gets sconfig
    time    <- gets stime
    curDate <- liftIO getClockTime
    let points = case status of
                   H.Killed _ -> (total + 1) `div` 2
                   _ -> total
    let score = H.ScoreRecord points (-time) curDate status
    (placeMsg, slideshow) <- liftIO $ H.register config write score
    messageOverlaysConfirm placeMsg slideshow

-- | Search for secret doors
search :: Action ()
search =
  do
    lmap <- gets (lmap . slevel)
    ploc <- gets (mloc . getPlayerBody)
    let searchTile (Tile (Door hv (Just n)) x, t') =
          (Tile (Door hv (Just (max (n - 1) 0))) x, t')
        searchTile t = t
        f l m = M.adjust searchTile (shift ploc m) l
        slmap = foldl' f lmap moves
    modify (updateLevel (updateLMap (const slmap)))
    playerAdvanceTime

-- | Start the floor targeting mode or toggle between the two floor modes.
targetFloor :: Action ()
targetFloor = do
  target <- gets (mtarget . getPlayerBody)
  let tgt = case target of
              TLoc l -> TLoc l  -- don't forget the old location target too fast
              _ -> TCursor
  updatePlayerBody (\ p -> p { mtarget = tgt })
  setCursor tgt

-- | Start the monster targeting mode. Cycle between monster targets.
-- TODO: also target a monster by moving the cursor, if in target monster mode.
-- TODO: sort monsters by distance to the player.
targetMonster :: Action ()
targetMonster = do
  pl        <- gets splayer
  ms        <- gets (lmonsters . slevel)
  per       <- currentPerception
  target    <- gets (mtarget . getPlayerBody)
  targeting <- gets (ctargeting . scursor)
  let i = case target of
            TEnemy (AMonster n) | targeting -> n  -- try next monster
            TEnemy (AMonster n) -> n - 1  -- try to retarget old monster
            _ -> -1  -- try to target first monster (e.g., number 0)
      dms = case pl of
              AMonster n -> IM.delete n ms  -- don't target yourself
              AHero _ -> ms
      (lt, gt) = IM.split i dms
      gtlt     = IM.assocs gt ++ IM.assocs lt
      lf = L.filter (\ (_, m) -> actorSeesLoc pl (mloc m) per pl) gtlt
      tgt = case lf of
              [] -> target  -- no monsters in sight, stick to last target
              (ni, _) : _ -> TEnemy (AMonster ni)  -- pick the next monster
  updatePlayerBody (\ p -> p { mtarget = tgt })
  setCursor tgt

-- | Set, activate and display cursor information.
setCursor :: Target -> Action ()
setCursor tgt = do
  state <- get
  per   <- currentPerception
  ploc  <- gets (mloc . getPlayerBody)
  ln    <- gets (lname . slevel)
  let upd cursor =
        let cloc = case targetToLoc (ptvisible per) state of
                     Nothing -> ploc
                     Just l  -> l
        in  cursor { ctargeting = True, clocation = cloc, clocLn = ln }
  modify (updateCursor upd)
  doLook

-- | Perform look around in the current location of the cursor.
-- TODO: depending on tgt, show extra info about tile or monster or both
doLook :: Action ()
doLook =
  do
    loc    <- gets (clocation . scursor)
    state  <- get
    lmap   <- gets (lmap . slevel)
    per    <- currentPerception
    target <- gets (mtarget . getPlayerBody)
    let canSee = S.member loc (ptvisible per)
        monsterMsg =
          if canSee
          then case L.find (\ m -> mloc m == loc) (levelMonsterList state) of
                 Just m  -> subjectMovable (mkind m) ++ " is here. "
                 Nothing -> ""
          else ""
        mode = case target of
                 TEnemy _ -> "[targeting monster] "
                 TLoc _   -> "[targeting location] "
                 TCursor  -> "[targeting current] "
        -- general info about current loc
        lookMsg = mode ++ lookAt True canSee state lmap loc monsterMsg
        -- check if there's something lying around at current loc
        t = lmap `at` loc
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
    items <- gets (mitems . getPlayerBody)
    if L.null items
      then abortWith "You are not carrying anything"
      else do
             displayItems "This is what you are carrying:" True items
             session getConfirm
             abortWith ""

-- | Given item is now known to the player.
discover :: Item -> Action ()
discover i = modify (updateDiscoveries (S.insert (ikind i)))

drinkPotion :: Action ()
drinkPotion =
  do
    state <- get
    lmap <- gets (lmap . slevel)
    pbody <- gets getPlayerBody
    ploc <- gets (mloc . getPlayerBody)
    items <- gets (mitems . getPlayerBody)
    pl <- gets splayer
    if L.null items
      then abortWith "You are not carrying anything."
      else do
             i <- getPotion "What to drink?" items "inventory"
             case i of
               Just i'@(Item { ikind = ik })
                | ItemKind.jname (ItemKind.getIK ik) == "potion" ->
                 do
                   -- only one potion is consumed even if several
                   -- are joined in the inventory
                   let consumed = i' { icount = 1 }
                   removeFromInventory consumed
                   message (subjectVerbIObject state pbody "drink" consumed "")
                   -- the potion is identified after drinking
                   itemEffectAction i' pl pl
               Just _  -> abortWith "you cannot drink that"
               Nothing -> neverMind True
    playerAdvanceTime

fireItem :: Action ()
fireItem = do
  state  <- get
  per    <- currentPerception
  pitems <- gets (mitems . getPlayerBody)
  pl     <- gets splayer
  target <- gets (mtarget . getPlayerBody)
  case findItem (\ i -> ItemKind.jname (ItemKind.getIK (ikind i)) == "dart") pitems of
    Just (dart, _) -> do
      let fired = dart { icount = 1 }
      removeFromInventory fired
      case targetToLoc (ptvisible per) state of
        Nothing  -> abortWith "target invalid"
        Just loc ->
          if actorReachesLoc pl loc per pl
            then case locToActor loc state of
                   Just ta -> actorDamageActor pl ta 1 " with a dart"
                   Nothing -> modify (updateLevel (scatterItems [fired] loc))
            else abortWith "target not reachable"
    Nothing -> abortWith "nothing to fire"
  playerAdvanceTime

applyItem :: Action ()
applyItem = do
  pl     <- gets splayer
  state  <- get
  per    <- currentPerception
  pitems <- gets (mitems . getPlayerBody)
  case findItem (\ i -> ItemKind.jname (ItemKind.getIK (ikind i)) == "wand") pitems of
    Just (wand, _) -> do
      let applied = wand { icount = 1 }
      removeFromInventory applied
      case targetToLoc (ptvisible per) state of
        Nothing  -> abortWith "target invalid"
        Just loc ->
          if actorReachesLoc pl loc per pl
            then case locToActor loc state of
                   Just ta -> selectPlayer ta >> return ()
                   Nothing -> abortWith "no living target to affect"
            else abortWith "target not reachable"
    Nothing -> abortWith "nothing to apply"
  playerAdvanceTime

dropItem :: Action ()
dropItem =
  do
    state <- get
    pbody <- gets getPlayerBody
    ploc <- gets (mloc . getPlayerBody)
    items <- gets (mitems . getPlayerBody)
    if L.null items
      then abortWith "You are not carrying anything."
      else do
             i <- getAnyItem "What to drop?" items "inventory"
             case i of
               Just i' ->
                 do
                   removeFromInventory i'
                   message (subjectVerbIObject state pbody "drop" i' "")
                   dropItemsAt [i'] ploc
               Nothing -> neverMind True
    playerAdvanceTime

dropItemsAt :: [Item] -> Loc -> Action ()
dropItemsAt is loc = modify (updateLevel (scatterItems is loc))

-- | Remove given item from the hero's inventory.
removeFromInventory :: Item -> Action ()
removeFromInventory i =
  updatePlayerBody (\ p -> p { mitems = removeItemByLetter i (mitems p) })

-- | Remove given item from the given location.
removeFromLoc :: Item -> Loc -> Action ()
removeFromLoc i loc =
  modify (updateLevel (updateLMap adj))
  where
    adj = M.adjust (\ (t, rt) -> (remove t, rt)) loc
    remove t = t { titems = removeItemByKind i (titems t) }

-- | Let the player choose any potion. Note that this does not guarantee a potion to be chosen,
-- as the player can override the choice.
getPotion :: String ->  -- prompt
             [Item] ->  -- all objects in question
             String ->  -- how to refer to the collection of objects,
                        -- e.g., "in your inventory"
             Action (Maybe Item)
getPotion prompt is isn =
  let choice i = ItemKind.jname (ItemKind.getIK (ikind i)) == "potion"
  in  getItem prompt choice "Potions" is isn

actorPickupItem :: Actor -> Action ()
actorPickupItem actor =
  do
    state <- get
    pl    <- gets splayer
    per   <- currentPerception
    lmap  <- gets (lmap . slevel)
    movable <- gets (getActor actor)
    let loc       = mloc movable
    let t         = lmap `at` loc -- the map tile in question
    let perceived = loc `S.member` ptvisible per
    let isPlayer  = actor == pl
    -- check if something is here to pick up
    case titems t of
      []     -> abortIfWith isPlayer "nothing here"
      (i:rs) -> -- pick up first item; TODO: let player select item; not for monsters
        case assignLetter (iletter i) (mletter movable) (mitems movable) of
          Just l ->
            do
              let (ni, nitems) = joinItem (i { iletter = Just l }) (mitems movable)
              -- message is dependent on who picks up and if the hero can perceive it
              if isPlayer
                then message (letterLabel (iletter ni) ++ objectItem state ni)
                else when perceived $
                       message $ subjectCompoundVerbIObject state movable "pick" "up" i ""
              removeFromLoc i loc
              -- add item to actor's inventory:
              updateAnyActor actor $ \ m ->
                m { mitems = nitems, mletter = maxLetter l (mletter movable) }
          Nothing -> abortIfWith isPlayer "you cannot carry any more"
    advanceTime actor

updateAnyActor :: Actor -> (Movable -> Movable) -> Action ()
updateAnyActor actor f = modify (updateAnyActorBody actor f)

updatePlayerBody :: (Movable -> Movable) -> Action ()
updatePlayerBody f = do
  pl <- gets splayer
  updateAnyActor pl f

pickupItem :: Action ()
pickupItem = do
  pl <- gets splayer
  actorPickupItem pl

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
displayItems msg sorted is = do
  state <- get
  let inv = unlines $
            L.map (\ i ->
                    letterLabel (iletter i) ++ objectItem state i ++ " ")
            ((if sorted then sortBy (cmpLetter' `on` iletter) else id) is)
  let ovl = inv ++ more
  message msg
  overlay ovl

-- | This function performs a move (or attack) by any actor,
-- i.e., it can handle monsters, heroes and both.
moveOrAttack :: Bool ->        -- allow attacks?
                Bool ->        -- auto-open doors on move
                Actor ->       -- who's moving?
                Dir ->
                Action ()
moveOrAttack allowAttacks autoOpen actor dir
  | dir == (0,0) =
      -- Moving with no direction is a noop.
      -- We include it currently to prevent that
      -- monsters attack themselves by accident.
      advanceTime actor
  | otherwise = do
      -- We start by looking at the target position.
      state <- get
      pl    <- gets splayer
      lmap  <- gets (lmap . slevel)
      sm    <- gets (getActor actor)
      let sloc = mloc sm           -- source location
          tloc = sloc `shift` dir  -- target location
      tgt <- gets (locToActor tloc)
      case tgt of
        Just target ->
          if allowAttacks then
            -- Attacking does not require full access, adjacency is enough.
            actorAttackActor actor target
          else if accessible lmap sloc tloc then do
            -- Switching positions requires full access.
            actorRunActor actor target
            when (actor == pl) $ message $ lookAt False True state lmap tloc ""
          else abort
        Nothing ->
          if accessible lmap sloc tloc then do
            -- perform the move; TODO: make this a separate function
            updateAnyActor actor $ \ m -> m { mloc = tloc }
            when (actor == pl) $ message $ lookAt False True state lmap tloc ""
            advanceTime actor
          else if autoOpen then
            -- try to open a door
            actorOpenClose actor False True dir
          else abort

-- | Resolves the result of an actor moving into another. Usually this
-- involves melee attack, but with two heroes it just changes focus.
-- Movables on blocked locations can be attacked without any restrictions.
-- For instance, a movable on an open door can be attacked diagonally,
-- and a movable capable of moving through walls can be attacked from an
-- adjacent position.
actorAttackActor :: Actor -> Actor -> Action ()
actorAttackActor (AHero _) target@(AHero _) =
  -- Select adjacent hero by bumping into him. Takes no time.
  selectPlayer target >> return ()
actorAttackActor source target = do
  sm    <- gets (getActor source)
  let -- Determine the weapon used for the attack.
      weapon = strongestWeapon (mitems sm)
      -- TODO: redo
      damage = case weapon of Just (Item { ikind = ik, ipower = k }) -> (case ItemKind.jeffect (ItemKind.getIK ik) of Effect.AffectHP n -> - n + k; _ -> 3) ; _ -> 3
      weaponMsg = if damage == 3
                  then ""
                  else " with a (+" ++ show (damage - 3) ++ ") sword" -- TODO: generate proper message
  actorDamageActor source target damage weaponMsg
  advanceTime source

actorDamageActor :: Actor -> Actor -> Int -> String -> Action ()
actorDamageActor source target damage weaponMsg =
  do
    when (isAHero target) $ do
      -- Focus on the attacked hero.
      b <- selectPlayer target
      -- Extra prompt, in case many heroes attacked in one turn.
      when b $ messageAddMore >> return ()
    state <- get
    sm <- gets (getActor source)
    tm <- gets (getActor target)
    let -- Damage the target.
        newHP  = mhp tm - damage
        killed = newHP <= 0
        -- Determine how the hero perceives the event.
        -- TODO: we have to be more precise and treat cases
        -- where two monsters fight, but only one is visible.
        combatVerb = if killed then "kill" else "hit"
        combatMsg  = subjectVerbMObject state sm combatVerb tm weaponMsg
    updateAnyActor target $ \ m -> m { mhp = newHP }
    per <- currentPerception
    let perceived  = mloc sm `S.member` ptvisible per
    messageAdd $
      if perceived
        then combatMsg
        else "You hear some noises."
    when killed $ do
      -- Place the actor's possessions on the map.
      dropItemsAt (mitems tm) (mloc tm)
      -- Clean bodies up.
      if target == (splayer state)
        then checkPartyDeath  -- kills the player and checks game over
        else modify (deleteActor target)  -- kills the enemy

-- | Resolves the result of an actor running into another.
-- This involves switching positions of the two movables.
actorRunActor :: Actor -> Actor -> Action ()
actorRunActor source target = do
  pl    <- gets splayer
  sloc  <- gets (mloc . getActor source)  -- source location
  tloc  <- gets (mloc . getActor target)  -- target location
  updateAnyActor source $ \ m -> m { mloc = tloc }
  updateAnyActor target $ \ m -> m { mloc = sloc }
  if source == pl
    then stopRunning  -- do not switch positions repeatedly
    else if isAMonster source && isAHero target
         then do
                -- A hero is run over by an enemy monster; focus on the hero.
                b <- selectPlayer target
                -- Extra prompt, in case many heroes disturbed in one turn.
                when b $ messageAddMore >> return ()
         else return ()
  advanceTime source

-- | Generate a monster, possibly.
generateMonster :: Action ()
generateMonster =
  do  -- TODO: simplify
    state  <- get
    nstate <- liftIO $ rndToIO $ addMonster state
    modify (const nstate)

-- | Advance the move time for the given actor.
advanceTime :: Actor -> Action ()
advanceTime actor =
  do
    time <- gets stime
    updateAnyActor actor $ \ m -> m { mtime = time + (nspeed (mkind m)) }

playerAdvanceTime :: Action ()
playerAdvanceTime = do
  pl   <- gets splayer
  advanceTime pl

-- | Possibly regenerate HP for all movables on the current level.
regenerateLevelHP :: Action ()
regenerateLevelHP =
  do
    time  <- gets stime
    let upd m = if time `mod` (nregen (mkind m)) /= 0
                then m
                else m { mhp = min (nhpMax (mkind m)) (mhp m + 1) }
    -- We really want hero selection to be a purely UI distinction,
    -- so all heroes need to regenerate, not just the player.
    -- Only the heroes on the current level regenerate (others are frozen
    -- in time together with their level). This prevents cheating
    -- via sending one hero to a safe level and waiting there.
    modify (updateLevel (updateHeroes   (IM.map upd)))
    modify (updateLevel (updateMonsters (IM.map upd)))

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The bool result indicates if the actors identify the effect.
effectToAction :: Effect.Effect -> Actor -> Actor -> Int -> Action Bool
effectToAction Effect.NoEffect source target power = return False
effectToAction (Effect.AffectHP n) source target power
  | n > 0 = do
      m <- gets (getActor target)
      if mhp m >= nhpMax (mkind m)
        then return False
        else do
          let upd m = m { mhp = min (nhpMax (mkind m)) (mhp m + n + power) }
          modify (updateAnyActorBody target upd)
          pl <- gets splayer
          when (target == pl) $ messageAdd "You feel better."  -- TODO: msg, if perceived, etc.
          return True
  | n < 0 = return False  -- TODO
  | otherwise = return False  -- TODO
effectToAction Effect.Dominate source target power = return False  -- TODO
effectToAction Effect.SummonFriend source target power = return False  -- TODO
effectToAction Effect.SummonEnemy source target power = return False  -- TODO
effectToAction Effect.ApplyWater _ target _ =
  if isAHero target  -- Monsters ignore water splashed on them.
  then messageAdd "Tastes like water." >> return True
  else return False

-- | The source actor affects the target actor, with a given item.
-- If either actor is a hero, the item may get identified (domination ignored).
itemEffectAction :: Item -> Actor -> Actor -> Action ()
itemEffectAction item source target = do
  let effect = ItemKind.jeffect $ ItemKind.getIK $ ikind item
  b <- effectToAction effect source target (ipower item)
  -- If something happens, the item gets identified.
  when (b && (isAHero source || isAHero target)) $ discover item
