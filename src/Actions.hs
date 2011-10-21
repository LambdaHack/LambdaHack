module Actions where

import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.Set as S

import Assert
import Action
import Display hiding (display)
import Dungeon
import Geometry
import Grammar
import qualified HighScores as H
import Item
import qualified Keys as K
import Level
import LevelState
import Actor
import ActorState
import Content.ActorKind
import ActorAdd
import Perception
import Random
import State
import qualified Config
import qualified Save
import EffectAction
import WorldLoc
import Tile  -- TODO: qualified
import qualified Kind
import qualified Feature as F

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
      then end -- no highscore display for quitters
      else abortWith "Game resumed."

-- | End targeting mode, accepting the current location or not.
endTargeting :: Bool -> Action ()
endTargeting accept = do
  returnLn <- gets (creturnLn . scursor)
  target   <- gets (atarget . getPlayerBody)
  cloc     <- gets (clocation . scursor)
  lvlSwitch returnLn  -- return to the original level of the player
  modify (updateCursor (\ c -> c { ctargeting = False }))
  let isEnemy = case target of TEnemy _ _ -> True ; _ -> False
  unless isEnemy $
    if accept
       then updatePlayerBody (\ p -> p { atarget = TLoc cloc })
       else updatePlayerBody (\ p -> p { atarget = TCursor })
  endTargetingMsg

endTargetingMsg :: Action ()
endTargetingMsg = do
  pbody  <- gets getPlayerBody
  state  <- get
  let verb = "target"
      targetMsg = case atarget pbody of
                    TEnemy a _ll ->
                      case findActorAnyLevel a state of
                        Just (_, m) -> objectActor m
                        Nothing     -> "a long gone adversary"
                    TLoc loc -> "location " ++ show loc
                    TCursor  -> "current cursor position continuously"
  messageAdd $ subjectActorVerb pbody verb ++ " " ++ targetMsg ++ "."

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

-- TODO: Think about doing the mode dispatch elsewhere, especially if over
-- time more and more commands need to do the dispatch inside their code
-- (currently only a couple do).
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
      updatePlayerBody (\ p -> p { adir = Just dir })
      -- attacks and opening doors disallowed while running
      moveOrAttack False False pl dir

-- | This function implements the actual "logic" of running. It checks if we
-- have to stop running because something interested happened, and it checks
-- if we have to adjust the direction because we're in the corner of a corridor.
continueRun :: Dir -> Action ()
continueRun dir =
  do
    loc <- gets (aloc . getPlayerBody)
    per <- currentPerception
    msg <- currentMessage
    ms  <- gets (lmonsters . slevel)
    hs  <- gets (lheroes . slevel)
    lm  <- gets (lmap . slevel)
    pl  <- gets splayer
    let dms = case pl of
                AMonster n -> IM.delete n ms  -- don't be afraid of yourself
                AHero _ -> ms
        mslocs = S.fromList (L.map aloc (IM.elems dms))
        monstersVisible = not (S.null (mslocs `S.intersection` ptvisible per))
        newsReported    = not (L.null msg)
        tile      = lm `at` loc  -- tile at current location
        itemsHere = not (L.null (titems tile))
        heroThere = (loc `shift` dir) `elem` L.map aloc (IM.elems hs)
        dirOK     = accessible lm loc (loc `shift` dir)
        isTExit   = Tile.isExit tile
        isWalkableDark = Tile.isWalkable tile && not (Tile.isLit tile)
    -- What happens next is mostly depending on the terrain we're currently on.
    let hop | (monstersVisible || heroThere || newsReported ||
               itemsHere || isTExit) = abort
            | isWalkableDark =
          -- in corridors, explore all corners and stop at all crossings
          -- TODO: even in corridors, stop if you run past an exit (rare)
          let ns = L.filter (\ x -> distance (neg dir, x) > 1
                                    && (accessible lm loc (loc `shift` x))
                                        || openable 1 lm (loc `shift` x))
                            moves
              allCloseTo main = L.all (\ d -> distance (main, d) <= 1) ns
          in  case ns of
                [onlyDir] -> run onlyDir  -- can be diagonal
                _         ->
                  -- prefer orthogonal to diagonal dirs, for hero's safety
                  case L.filter (not . diagonal) ns of
                    [ortoDir]
                      | allCloseTo ortoDir -> run ortoDir
                    _ -> abort
            | not dirOK =
          abort -- outside corridors never change direction
            | otherwise =
          let ns = L.filter (\ x -> x /= dir && distance (neg dir, x) > 1) moves
              ls = L.map (loc `shift`) ns
              as = L.filter (\ x -> accessible lm loc x
                                    || openable 1 lm x) ls
              ts = L.map (lm `at`) as
          in if L.any Tile.isExit ts then abort else run dir
    hop

ifRunning :: (Dir -> Action a) -> Action a -> Action a
ifRunning t e =
  do
    ad <- gets (adir . getPlayerBody)
    maybe e t ad

-- | Update player memory.
remember :: Action ()
remember =
  do
    per <- currentPerception
    let vis         = S.toList (ptvisible per)
    let rememberLoc = M.update (\ (t,_) -> Just (t,t))
    modify (updateLevel (updateLMap (\ lm -> L.foldr rememberLoc lm vis)))

-- | Ask for a direction and close the door, if any
closeDoor :: Action ()
closeDoor = do
  messageReset "direction?"
  display
  e  <- session nextCommand
  K.handleDirection e playerCloseDoor (neverMind True)

-- | Player closes a door. AI never does.
playerCloseDoor :: Dir -> Action ()
playerCloseDoor dir = do
  state <- get
  lm    <- gets (lmap . slevel)
  pl    <- gets splayer
  body  <- gets (getActor pl)
  let hms = levelHeroList state ++ levelMonsterList state
      dloc = shift (aloc body) dir  -- the location we act upon
      t = lm `at` dloc
  if hasFeature F.Closable t
    then
      case Tile.titems t of
        [] ->
          if unoccupied hms dloc
          then let nt  = Tile Tile.doorClosedId Nothing Nothing []
                   adj = M.adjust (\ (_, mt) -> (nt, mt)) dloc
               in modify (updateLevel (updateLMap adj))
          else abortWith "blocked"  -- by monsters or heroes
        _:_ -> abortWith "jammed"  -- by items
    else if hasFeature F.Openable t
         then abortWith "already closed"
         else neverMind True  -- no visible doors (can be secret)
  advanceTime pl

-- | An actor closes a door. Player (hero or monster) or enemy.
actorOpenDoor :: ActorId -> Dir -> Action ()
actorOpenDoor actor dir = do
  lm   <- gets (lmap . slevel)
  pl   <- gets splayer
  body <- gets (getActor actor)
  let dloc = shift (aloc body) dir  -- the location we act upon
      t = lm `at` dloc
      isPlayer = actor == pl
      isVerbose = isPlayer  -- don't report enemy failures, if it's not player
      iq = biq $ Kind.getKind $ akind body
      openPower =
        if isPlayer
        then 1  -- player can't open secret doors
        else case strongestItem (aitems body) "ring" of  -- TODO: hack
               Just i  -> iq + ipower i
               Nothing -> iq
  unless (openable openPower lm dloc) $ neverMind isVerbose
  if hasFeature F.Closable t
    then abortIfWith isVerbose "already open"
    else if not (hasFeature F.Closable t ||
                 hasFeature F.Openable t ||
                 hasFeature F.Hidden t)
         then neverMind isVerbose  -- not doors at all
         else
           let nt  = Tile Tile.doorOpenId Nothing Nothing (Tile.titems t)
               adj = M.adjust (\ (_, mt) -> (nt, mt)) dloc
           in  modify (updateLevel (updateLMap adj))
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
    lvlSwitch (LambdaCave nln)
      >>= (assert `checkM` (k == 0 ||)) (nln, "dungeon has a cycle", k)

-- | Attempt a level change via up level and down level keys.
-- Will quit the game if the player leaves the dungeon.
lvlChange :: VDir -> Action ()
lvlChange vdir =
  do
    cursor    <- gets scursor
    targeting <- gets (ctargeting . scursor)
    pbody     <- gets getPlayerBody
    pl        <- gets splayer
    lm        <- gets (lmap . slevel)
    let loc = if targeting then clocation cursor else aloc pbody
        tile = lm `at` loc
        sdir | hasFeature F.Climbable tile = Just Up
             | hasFeature F.Descendable tile = Just Down
             | otherwise = Nothing
    case sdir of
      Just vdir'
        | vdir == vdir' -> -- stairs are in the right direction
          case tteleport tile of
            Nothing ->
              -- we are at the "end" of the dungeon
              if targeting
              then abortWith "cannot escape dungeon in targeting mode"
              else do
                b <- messageYesNo "Really escape the dungeon?"
                if b
                  then fleeDungeon
                  else abortWith "Game resumed."
            Just (nln, nloc) ->
              if targeting
                then do
                  lvlSwitch nln
                    >>= assert `trueM` (nln, "stairs connect level with itself")
                  -- do not freely reveal the other end of the stairs
                  lm2 <- gets (lmap . slevel)  -- lvlSwitch modifies map
                  let upd cur =
                        let clocation = if isUnknown (rememberAt lm2 nloc)
                                        then loc
                                        else nloc
                        in  cur { clocation, clocLn = nln }
                  modify (updateCursor upd)
                  doLook
                else tryWith (abortWith "somebody blocks the staircase") $ do
                  -- Remove the player from the old level.
                  modify (deleteActor pl)
                  -- At this place the invariant that the player exists fails.
                  -- Change to the new level (invariant not needed).
                  lvlSwitch nln
                    >>= assert `trueM` (nln, "stairs connect level with itself")
                  -- Add the player to the new level.
                  modify (insertActor pl pbody)
                  -- At this place the invariant is restored again.
                  -- Land the player at the other end of the stairs.
                  updatePlayerBody (\ p -> p { aloc = nloc })
                  -- Change the level of the player recorded in cursor.
                  modify (updateCursor (\ c -> c { creturnLn = nln }))
                  -- Bail out if anybody blocks the staircase.
                  inhabitants <- gets (locToActors nloc)
                  when (length inhabitants > 1) abort
                  -- The invariant "at most one actor on a tile" restored.
                  -- Create a backup of the savegame.
                  state <- get
                  liftIO $ Save.saveGame state >> Save.mvBkp (sconfig state)
                  playerAdvanceTime
      _ -> -- no stairs in the right direction
        if targeting
        then do
          lvlDescend (if vdir == Up then -1 else 1)
          clocLn <- gets (lname . slevel)
          let upd cur = cur {clocLn}
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
        items = L.concatMap aitems (levelHeroList state)
    if total == 0
      then do
             go <- messageClear >> messageMoreConfirm ColorFull "Coward!"
             when go $
               messageMore "Next time try to grab some loot before escape!"
             end
      else do
             let winMsg = "Congratulations, you won! Your loot, worth " ++
                          show total ++ " gold, is:"
             displayItems winMsg True items
             go <- session getConfirm
             when go $ do
               go2 <- handleScores True H.Victor total
               when go2 $ messageMore "Can it be done better, though?"
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
      ni : _ -> selectPlayer (AHero ni)
                  >>= assert `trueM` (pl, ni, "hero duplicated")

-- | Search for secret doors
search :: Action ()
search =
  do
    lm     <- gets (lmap . slevel)
    ploc   <- gets (aloc . getPlayerBody)
    pitems <- gets (aitems . getPlayerBody)
    let delta = case strongestItem pitems "ring" of
                  Just i  -> 1 + ipower i
                  Nothing -> 1
        searchTile (t@(Tile _ l s x), t') =
          if hasFeature F.Hidden t
          then
            let k = fromJust s - delta
            in if k > 0
               then (Tile Tile.doorSecretId l (Just k) x, t')
               else (Tile Tile.doorClosedId l Nothing x,  t')
          else (t, t')
        f l m = M.adjust searchTile (shift ploc m) l
        slmap = L.foldl' f lm moves
    modify (updateLevel (updateLMap (const slmap)))
    playerAdvanceTime

-- | Start the floor targeting mode or reset the cursor location to the player.
targetFloor :: Action ()
targetFloor = do
  ploc      <- gets (aloc . getPlayerBody)
  target    <- gets (atarget . getPlayerBody)
  targeting <- gets (ctargeting . scursor)
  let tgt = case target of
              _ | targeting -> TLoc ploc  -- double key press: reset cursor
              TEnemy _ _ -> TCursor  -- forget enemy target, keep the cursor
              t -> t  -- keep the target from previous targeting session
  updatePlayerBody (\ p -> p { atarget = tgt })
  setCursor

-- | Start the monster targeting mode. Cycle between monster targets.
-- TODO: also target a monster by moving the cursor, if in target monster mode.
-- TODO: sort monsters by distance to the player.
targetMonster :: Action ()
targetMonster = do
  pl        <- gets splayer
  ms        <- gets (lmonsters . slevel)
  per       <- currentPerception
  target    <- gets (atarget . getPlayerBody)
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
      lf = L.filter (\ (_, m) -> actorSeesLoc pl (aloc m) per (Just pl)) gtlt
      tgt = case lf of
              [] -> target  -- no monsters in sight, stick to last target
              (na, nm) : _ -> TEnemy (AMonster na) (aloc nm)  -- pick the next
  updatePlayerBody (\ p -> p { atarget = tgt })
  setCursor

-- | Set, activate and display cursor information.
setCursor :: Action ()
setCursor = do
  state <- get
  per   <- currentPerception
  ploc  <- gets (aloc . getPlayerBody)
  clocLn <- gets (lname . slevel)
  let upd cursor =
        let clocation = fromMaybe ploc (targetToLoc (ptvisible per) state)
        in cursor { ctargeting = True, clocation, clocLn }
  modify (updateCursor upd)
  doLook

-- | Perform look around in the current location of the cursor.
-- TODO: depending on tgt, show extra info about tile or monster or both
doLook :: Action ()
doLook =
  do
    loc    <- gets (clocation . scursor)
    state  <- get
    lm     <- gets (lmap . slevel)
    per    <- currentPerception
    target <- gets (atarget . getPlayerBody)
    let canSee = S.member loc (ptvisible per)
        monsterMsg =
          if canSee
          then case L.find (\ m -> aloc m == loc) (levelMonsterList state) of
                 Just m  -> subjectActor m ++ " is here. "
                 Nothing -> ""
          else ""
        mode = case target of
                 TEnemy _ _ -> "[targeting monster] "
                 TLoc _     -> "[targeting location] "
                 TCursor    -> "[targeting current] "
        -- general info about current loc
        lookMsg = mode ++ lookAt True canSee state lm loc monsterMsg
        -- check if there's something lying around at current loc
        t = lm `at` loc
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
                ActorId ->     -- who's moving?
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
      lm    <- gets (lmap . slevel)
      sm    <- gets (getActor actor)
      let sloc = aloc sm           -- source location
          tloc = sloc `shift` dir  -- target location
      tgt <- gets (locToActor tloc)
      case tgt of
        Just target
          | allowAttacks ->
              -- Attacking does not require full access, adjacency is enough.
              actorAttackActor actor target
          | accessible lm sloc tloc -> do
              -- Switching positions requires full access.
              actorRunActor actor target
              when (actor == pl) $
                messageAdd $ lookAt False True state lm tloc ""
          | otherwise -> abortWith ""
        Nothing
          | accessible lm sloc tloc -> do
              -- perform the move
              updateAnyActor actor $ \ body -> body {aloc = tloc}
              when (actor == pl) $
                messageAdd $ lookAt False True state lm tloc ""
              advanceTime actor
          | allowAttacks && actor == pl
            && canBeSecretDoor (lm `rememberAt` tloc)
            -> do
              messageAdd "You search your surroundings."  -- TODO: proper msg
              search
          | autoOpen -> actorOpenDoor actor dir  -- try to open a door
          | otherwise -> abortWith ""

-- | Resolves the result of an actor moving into another. Usually this
-- involves melee attack, but with two heroes it just changes focus.
-- Actors on blocked locations can be attacked without any restrictions.
-- For instance, an actor capable of moving through walls
-- can be attacked from an adjacent position.
-- This function is analogous to zapGroupItem, but for melee
-- and not using up the weapon.
actorAttackActor :: ActorId -> ActorId -> Action ()
actorAttackActor source@(AHero _) target@(AHero _) =
  -- Select adjacent hero by bumping into him. Takes no time.
  selectPlayer target
    >>= assert `trueM` (source, target, "player bumps into himself")
actorAttackActor source target = do
  state <- get
  sm    <- gets (getActor source)
  tm    <- gets (getActor target)
  per   <- currentPerception
  let groupName = "sword"
      verb = attackToVerb groupName
      sloc = aloc sm
      -- The hand-to-hand "weapon", equivalent to +0 sword.
      h2h = Item Item.fistKindId 0 Nothing 1
      str = strongestItem (aitems sm) groupName
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
  itemEffectAction 0 source target single
  advanceTime source

attackToVerb :: String -> String
attackToVerb "sword" = "hit"  -- TODO: "slash"? "pierce"? "swing"?
attackToVerb "mace" = "bludgeon"
attackToVerb _ = "hit"

-- | Resolves the result of an actor running into another.
-- This involves switching positions of the two actors.
actorRunActor :: ActorId -> ActorId -> Action ()
actorRunActor source target = do
  pl   <- gets splayer
  sloc <- gets (aloc . getActor source)  -- source location
  tloc <- gets (aloc . getActor target)  -- target location
  updateAnyActor source $ \ m -> m { aloc = tloc }
  updateAnyActor target $ \ m -> m { aloc = sloc }
  if source == pl
    then stopRunning  -- do not switch positions repeatedly
    else when (isAMonster source) $ focusIfAHero target
  advanceTime source

-- | Generate a monster, possibly.
generateMonster :: Action ()
generateMonster =
  do  -- TODO: simplify
    state  <- get
    nstate <- liftIO $ rndToIO $ rollMonster state
    modify (const nstate)

-- | Possibly regenerate HP for all actors on the current level.
regenerateLevelHP :: Action ()
regenerateLevelHP =
  do
    time <- gets stime
    let upd m =
          let ak = Kind.getKind $ akind m
              regen = bregen ak `div`
                      case strongestItem (aitems m) "amulet" of
                        Just i  -> ipower i
                        Nothing -> 1
          in if time `mod` regen /= 0
             then m
             else addHp 1 m
    -- We really want hero selection to be a purely UI distinction,
    -- so all heroes need to regenerate, not just the player.
    -- Only the heroes on the current level regenerate (others are frozen
    -- in time together with their level). This prevents cheating
    -- via sending one hero to a safe level and waiting there.
    modify (updateLevel (updateHeroes   (IM.map upd)))
    modify (updateLevel (updateMonsters (IM.map upd)))
