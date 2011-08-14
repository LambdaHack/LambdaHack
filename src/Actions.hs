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
import EffectAction

-- The Action stuff that is independent from ItemAction.hs.
-- (Both depend on EffectAction.hs).

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
    let fn = "config.dump"
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
  lvlSwitch returnLn  -- return to the original level of the player
  modify (updateCursor (\ c -> c { ctargeting = False }))
  let isEnemy = case target of TEnemy _ _ -> True ; _ -> False
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
                    TEnemy a _ll ->
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
                                    || openable 1 lmap x) ls
              ts = L.map (tterrain . (lmap `at`)) as
          in  if L.any exit ts then abort else run dir
    hop (tterrain t)

ifRunning :: (Dir -> Action a) -> Action a -> Action a
ifRunning t e =
  do
    mdir <- gets (mdir . getPlayerBody)
    maybe e t mdir

-- | Update player memory.
remember :: Action ()
remember =
  do
    per <- currentPerception
    let vis         = S.toList (ptvisible per)
    let rememberLoc = M.update (\ (t,_) -> Just (t,t))
    modify (updateLevel (updateLMap (\ lmap -> L.foldr rememberLoc lmap vis)))

-- | Open and close doors
openclose :: Bool -> Action ()
openclose o =
  do
    messageWipeAndSet "direction?"
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
    let openPower = case strongestItem (mitems body) "ring" of
                      Just i  -> niq (mkind body) + ipower i
                      Nothing -> niq (mkind body)
      in case lmap `at` dloc of
           Tile d@(Door hv o') []
             | secret o' && isPlayer -> -- door is secret, cannot be opened or closed by the player
                                       neverMind isVerbose
             | maybe o ((|| not o) . (>= openPower)) o' ->
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

 -- | Attempt a level switch to k levels deeper.
-- TODO: perhaps set up some level name arithmetics in Level.hs
-- and hide there the fact levels are now essentially Ints.
lvlDescend :: Int -> Action ()
lvlDescend k =
  do
    state <- get
    let n = levelNumber (lname (slevel state))
        nln = n + k
    when (nln < 1 || nln > sizeDungeon (sdungeon state) + 1) $
      abortWith "no more levels in this direction"
    assertTrue $ liftM (k == 0 ||)  (lvlSwitch (LambdaCave nln))

-- | Attempt a level change via up level and down level keys.
-- Will quit the game if the player leaves the dungeon.
lvlChange :: VDir -> Action ()
lvlChange vdir =
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
                  assertTrue $ lvlSwitch nln
                  -- do not freely reveal the other end of the stairs
                  map <- gets (lmap . slevel)  -- lvlSwitch modifies map
                  let upd cursor =
                        let cloc = if Level.isUnknown (rememberAt map nloc)
                                   then loc
                                   else nloc
                        in  cursor { clocation = cloc, clocLn = nln }
                  modify (updateCursor upd)
                  doLook
                else tryWith (abortWith "somebody blocks the staircase") $ do
                  -- Remove the player from the old level.
                  modify (deleteActor pl)
                  -- At this place the invariant that the player exists fails.
                  -- Change to the new level (invariant not needed).
                  assertTrue $ lvlSwitch nln
                  -- Add the player to the new level.
                  modify (insertActor pl pbody)
                  -- At this place the invariant is restored again.
                  -- Land the player at the other end of the stairs.
                  updatePlayerBody (\ p -> p { mloc = nloc })
                  -- Change the level of the player recorded in cursor.
                  modify (updateCursor (\ c -> c { creturnLn = nln }))
                  -- Bail out if anybody blocks the staircase.
                  inhabitants <- gets (locToActors nloc)
                  when (length inhabitants > 1) abort
                  -- The invariant "at most one movable on a tile" restored.
                  -- Create a backup of the savegame.
                  state <- get
                  liftIO $ Save.saveGame state >> Save.mvBkp (sconfig state)
                  playerAdvanceTime
      _ -> -- no stairs
        if targeting
        then do
          lvlDescend (if vdir == Up then -1 else 1)
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
             go <- resetMessage >> messageMoreConfirm False "Coward!"
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

-- | Search for secret doors
search :: Action ()
search =
  do
    lmap   <- gets (lmap . slevel)
    ploc   <- gets (mloc . getPlayerBody)
    pitems <- gets (mitems . getPlayerBody)
    let delta = case strongestItem pitems "ring" of
                  Just i  -> 1 + ipower i
                  Nothing -> 1
        searchTile (Tile (Door hv (Just n)) x, t') =
          (Tile (Door hv (Just (max (n - delta) 0))) x, t')
        searchTile t = t
        f l m = M.adjust searchTile (shift ploc m) l
        slmap = L.foldl' f lmap moves
    modify (updateLevel (updateLMap (const slmap)))
    playerAdvanceTime

-- | Start the floor targeting mode or reset the cursor location to the player.
targetFloor :: Action ()
targetFloor = do
  ploc      <- gets (mloc . getPlayerBody)
  target    <- gets (mtarget . getPlayerBody)
  targeting <- gets (ctargeting . scursor)
  let tgt = case target of
              _ | targeting -> TLoc ploc  -- double key press: reset cursor
              TEnemy _ _ -> TCursor  -- forget enemy target, keep the cursor
              t -> t  -- keep the target from previous targeting session
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
            TEnemy (AMonster n) _ | targeting -> n  -- try next monster
            TEnemy (AMonster n) _ -> n - 1  -- try to retarget old monster
            _ -> -1  -- try to target first monster (e.g., number 0)
      dms = case pl of
              AMonster n -> IM.delete n ms  -- don't target yourself
              AHero _ -> ms
      (lt, gt) = IM.split i dms
      gtlt     = IM.assocs gt ++ IM.assocs lt
      lf = L.filter (\ (_, m) -> actorSeesLoc pl (mloc m) per (Just pl)) gtlt
      tgt = case lf of
              [] -> target  -- no monsters in sight, stick to last target
              (ni, nm) : _ -> TEnemy (AMonster ni) (mloc nm)  -- pick the next
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
                 TEnemy _ _ -> "[targeting monster] "
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
            when (actor == pl) $
              messageAdd $ lookAt False True state lmap tloc ""
          else abortWith ""
        Nothing ->
          if accessible lmap sloc tloc then do
            -- perform the move
            updateAnyActor actor $ \ m -> m { mloc = tloc }
            when (actor == pl) $
              messageAdd $ lookAt False True state lmap tloc ""
            advanceTime actor
          else if allowAttacks && actor == pl
                  && canBeDoor (lmap `rememberAt` tloc) then do
            messageAdd "You search your surroundings."  -- TODO: proper msg
            search
          else if autoOpen then
            -- try to open a door
            actorOpenClose actor False True dir
          else abortWith ""

-- | Resolves the result of an actor moving into another. Usually this
-- involves melee attack, but with two heroes it just changes focus.
-- Movables on blocked locations can be attacked without any restrictions.
-- For instance, a movable on an open door can be attacked diagonally,
-- and a movable capable of moving through walls can be attacked from an
-- adjacent position.
-- This function is analogous to zapGroupItem, but for melee
-- and not using up the weapon.
actorAttackActor :: Actor -> Actor -> Action ()
actorAttackActor (AHero _) target@(AHero _) =
  -- Select adjacent hero by bumping into him. Takes no time.
  assertTrue $ selectPlayer target
actorAttackActor source target = do
  state <- get
  sm    <- gets (getActor source)
  tm    <- gets (getActor target)
  per   <- currentPerception
  let groupName = "sword"
      verb = attackToVerb groupName
      sloc = mloc sm
      swordKindIndex = fromJust $ L.elemIndex ItemKind.sword ItemKind.loot
      -- The hand-to-hand "weapon", equivalent to +0 sword.
      h2h = Item swordKindIndex 0 Nothing 1
      str = strongestItem (mitems sm) groupName
      stack  = fromMaybe h2h str
      single = stack { icount = 1 }
      -- The message describes the source part of the action.
      -- TODO: right now it also describes the victim and weapon;
      -- perhaps, when a weapon is equipped, just say "you hit" or "you miss"
      -- and then "nose dies" or "nose yells in pain".
      msg = subjectVerbMObject sm verb tm $
              if isJust str then " with " ++ objectItem state single else ""
  when (sloc `S.member` ptvisible per) $ messageAdd msg
  -- Messages inside itemEffectAction describe the target part.
  itemEffectAction source target single
  advanceTime source

attackToVerb :: String -> String
attackToVerb "sword" = "hit"  -- TODO: "slash"? "pierce"? "swing"?
attackToVerb "mace" = "bludgeon"
attackToVerb _ = "hit"

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
    else if isAMonster source
         then focusIfAHero target
         else return ()
  advanceTime source

-- | Generate a monster, possibly.
generateMonster :: Action ()
generateMonster =
  do  -- TODO: simplify
    state  <- get
    nstate <- liftIO $ rndToIO $ rollMonster state
    modify (const nstate)

-- | Possibly regenerate HP for all movables on the current level.
regenerateLevelHP :: Action ()
regenerateLevelHP =
  do
    time  <- gets stime
    let upd m =
          let regen = nregen (mkind m) `div`
                      case strongestItem (mitems m) "amulet" of
                        Just i  -> ipower i
                        Nothing -> 1
          in if time `mod` regen /= 0
             then m
             else m { mhp = min (nhpMax (mkind m)) (mhp m + 1) }
    -- We really want hero selection to be a purely UI distinction,
    -- so all heroes need to regenerate, not just the player.
    -- Only the heroes on the current level regenerate (others are frozen
    -- in time together with their level). This prevents cheating
    -- via sending one hero to a safe level and waiting there.
    modify (updateLevel (updateHeroes   (IM.map upd)))
    modify (updateLevel (updateMonsters (IM.map upd)))
