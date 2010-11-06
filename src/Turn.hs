module Turn where

import System.Time

import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Char
import Data.Maybe
import Data.Function

import State
import Geometry
import Level
import LevelState
import Dungeon
import Monster
import Actor
import Perception
import Item
import ItemState
import Display2
import Random
import Save
import Message
import Version
import Strategy
import StrategyState
import HighScores

-- | Perform a complete turn (i.e., monster moves etc.)
loop :: Session -> State -> Message -> IO ()
loop session (state@(State { splayer = player@(Monster { mhp = php, mloc = ploc }),
                             stime   = time,
                             slevel  = lvl@(Level nm sz ms smap lmap lmeta) }))
             oldmsg =
  do
    -- update smap
    let nsmap = M.insert ploc (time + smellTimeout) smap
    -- determine player perception
    let per = perception ploc lmap
    -- perform monster moves
    handleMonsters session (updateLevel (const (lvl { lsmell = nsmap })) state) per oldmsg

-- | Handle monster moves. The idea is that we perform moves
--   as long as there are monsters that have a move time which is
--   less than or equal to the current time.
handleMonsters :: Session -> State -> Perception -> Message -> IO ()
handleMonsters session
               (state@(State { stime  = time,
                               slevel = lvl@(Level { lmonsters = ms }) }))
               per oldmsg =
  -- debugging: causes redraw of the current state for every monster move; slow!
  -- displayLevel session lvl per state oldmsg >>
  case ms of
    [] -> -- there are no monsters, just continue
      handlePlayer
    (m@(Monster { mtime = mt }) : ms)
      | mt > time  -> -- all the monsters are not yet ready for another move,
                      -- so continue
                      handlePlayer
      | mhp m <= 0 -> -- the monster dies
                      killMonster m session
                        (updateLevel (updateMonsters (const ms)) state)
                        per oldmsg
      | otherwise  -> -- monster m should move
                      handleMonster m session
                        (updateLevel (updateMonsters (const ms)) state)
                        per oldmsg
    where
      -- good place to do everything that has to be done for every *time*
      -- unit; currently, that's monster generation
      handlePlayer =
        do
          let nstate = state { stime = time + 1 }
          nlvl <- rndToIO (addMonster lvl (splayer nstate))
          handle session (updateLevel (const nlvl) nstate) per oldmsg

-- | Handle death of a single monster: scatter loot, update score, etc.
killMonster :: Monster -> Session -> State -> Perception -> Message ->
                 IO ()
killMonster m session state per oldmsg =
  let nstate = updateLevel (scatterItems (mitems m) (mloc m)) state
  in  handleMonsters session nstate per oldmsg

-- | Handle the move of a single monster.
handleMonster :: Monster -> Session -> State -> Perception -> Message ->
                 IO ()
handleMonster m session
              (state@(State { splayer = player@(Monster { mloc = ploc }),
                              stime   = time,
                              slevel  = lvl@(Level { lmonsters = ms, lsmell = nsmap, lmap = lmap }) }))
              per oldmsg =
  do
    nl <- rndToIO (frequency (head (runStrategy (strategy m state per .| wait))))
    -- choose the action to perform
    let (action, mdir) =
          if nl == (0,0)
          then -- not moving this turn, so let's try to pick up an object
            let pickup continue abort state _per actor _dir =
                  let dummyDisplayCurrent _ _ = return True
                      cont state _msg = continue state ""
                  in  actorPickupItem actor dummyDisplayCurrent cont abort state
            in  (pickup, Nothing)
          else
            (moveOrAttack True, Just nl)

    -- increase the monster move time and set direction
    let nm         = m { mtime = time + mspeed m, mdir = mdir }
        (act, nms) = insertMonster nm ms
        newState   = updateLevel (updateMonsters (const nms)) state

    -- perform action for the current monster and move to the rest
    action
      (\ s msg -> handleMonsters session s per (addMsg oldmsg msg))
      (handleMonsters session newState per oldmsg)
      newState
      per (AMonster act) nl


-- | Display current status and handle the turn of the player.
handle :: Session -> State -> Perception -> Message -> IO ()
handle session (state@(State { splayer = player@(Monster { mhp = php, mdir = pdir, mloc = ploc, mitems = pinv, mtime = ptime }),
                               stime   = time,
                               slevel  = lvl@(Level nm sz ms smap lmap lmeta) }))
               per oldmsg =
    -- check for player death
    if php <= 0
      then do
             displayCurrent (addMsg oldmsg more) Nothing
             getConfirm session
             displayCurrent ("You die." ++ more) Nothing
             go <- getConfirm session
             handleScores go True True False
             shutdown session
      else -- check if the player can make another move yet
           if ptime > time then
             do
               -- do not make intermediate redraws while running
               maybe (displayLevel session per state "" Nothing) (const $ return True) pdir
               handleMonsters session state per oldmsg
           -- NOTE: It's important to call handleMonsters here, not loop,
           -- because loop does all sorts of calculations that are only
           -- really necessary after the player has moved.
      else do
             displayCurrent oldmsg Nothing
             let h = nextCommand session >>= h'
                 h' e =
                       handleDirection e (move h) $
                         handleDirection (L.map toLower e) run $
                         case e of
                           -- interaction with the dungeon
                           "c"       -> close h
                           "s"       -> search h
                           "less"    -> lvlchange Up h
                           "greater" -> lvlchange Down h
                           "colon"   -> lookAround h

                           -- items
                           "comma"   -> wrapHandler pickupItem  h
                           "d"       -> wrapHandler dropItem    h
                           "i"       -> inventory h
                           "q"       -> wrapHandler drinkPotion h

                           -- wait
                           "period"  -> loop session nstate ""

                           -- saving or ending the game
                           "S"       -> userSavesGame
                           "Q"       -> userQuits h
                           "Escape"  -> displayCurrent "Press Q to quit." Nothing >> h

                           -- debug modes
                           "V"       -> handle session (toggleVision     ustate) per oldmsg
                           "R"       -> handle session (toggleSmell      ustate) per oldmsg
                           "O"       -> handle session (toggleOmniscient ustate) per oldmsg
                           "T"       -> handle session (toggleTerrain    ustate) per oldmsg
                           "I"       -> displayCurrent lmeta   Nothing >> h

                           -- information for the player
                           "M"       -> displayCurrent "" (Just $ unlines (shistory mstate) ++ more) >>= \ b ->
                                        if b then getOptionalConfirm session
                                                    (const (displayCurrent "" Nothing >> h)) h'
                                             else displayCurrent "" Nothing >> h
                           "v"       -> displayCurrent version Nothing >> h
                           _
                             | e == "question" || e == "Return" -> displayHelp h

                           -- wrong key
                           _         -> displayCurrent ("unknown command (" ++ e ++ ")") Nothing >> h
             maybe h continueRun pdir

 where

  -- TODO: automatically find and cut from PLAYING.markdown?
  helpString =
    unlines
    ["                                                    "
    ,"               key    command                       "
    ,"               c      close a door                  "
    ,"               d      drop an object                "
    ,"               i      display inventory             "
    ,"               o      open a do                     "
    ,"               s      search for secret doors       "
    ,"               q      quaff a potion                "
    ,"               M      display previous messages     "
    ,"               S      save and quit the game        "
    ,"               Q      quit without saving           "
    ,"               .      wait                          "
    ,"               ,      pick up an object             "
    ,"               :      look around                   "
    ,"               <      ascend a level                "
    ,"               >      descend a level               "
    ,"                                                    "
    ,"               (See file PLAYING.markdown.)         "
    ,"                                                    "
    ," --more--                                           "
    ]
  displayHelp abort =
    do
      displayCurrent "Basic keys:" (Just helpString)
      getConfirm session
      displayCurrent "" Nothing
      abort

  -- we record the oldmsg in the history
  mstate = if L.null oldmsg then state else updateHistory (take 500 . ((oldmsg ++ " "):)) state
    -- TODO: make history max configurable

  reachable = preachable per
  visible   = pvisible per

  displayCurrent :: Message -> Maybe String -> IO Bool
  displayCurrent  = displayLevel session per ustate

  -- update player memory
  nlmap = foldr (\ x m -> M.update (\ (t,_) -> Just (t,t)) x m) lmap (S.toList visible)
  nlvl = updateLMap (const nlmap) lvl

  -- state with updated player memory, but without any passed time
  ustate  = updateLevel (const nlvl) state

  -- update player action time, and regenerate hitpoints
  -- player HP regeneration, TODO: remove hardcoded time interval
  nplayer = player { mtime = time + mspeed player,
                     mhp   = if time `mod` 1500 == 0 then (php + 1) `min` playerHP else php }
  nstate  = updateLevel (const nlvl) (updatePlayer (const nplayer) mstate)

  -- uniform wrapper function for action handlers
  wrapHandler h abort = h session displayCurrent (loop session) abort nstate

  -- display inventory
  inventory abort
    | L.null (mitems player) =
      displayCurrent "You are not carrying anything." Nothing >> abort
    | otherwise =
    do
      displayItems displayCurrent nstate
                   "This is what you are carrying:" True (mitems player)
      getConfirm session
      displayCurrent "" Nothing
      abort  -- looking at inventory doesn't take any time

  -- look around at current location
  lookAround abort =
    do
      -- check if something is here to pick up
      let t = nlmap `at` ploc
      if length (titems t) <= 2
        then displayCurrent (lookAt True nstate nlmap ploc) Nothing >> abort
        else
          do
             displayItems displayCurrent nstate
                          (lookAt True nstate nlmap ploc) False (titems t)
             getConfirm session
             displayCurrent "" Nothing
             abort  -- looking around doesn't take any time

  -- close doors
  close abort =
    do
      displayCurrent "direction?" Nothing
      e <- nextCommand session
      handleDirection e h_continue h_abort
        where
          h_continue = close' abort
          h_abort = displayCurrent "never mind" Nothing >> abort

  close' abort dir =
    let dloc = shift ploc dir
        msg_no_door = displayCurrent "never mind" Nothing >> abort
    in
     case nlmap `at` dloc of
       Tile d@(Door hv o') is
         | secret o' -> msg_no_door
         | toOpen True /= o' ->
           displayCurrent "already closed" Nothing >> abort
         | not (unoccupied ms nlmap dloc) ->
           displayCurrent "blocked" Nothing >> abort
         | otherwise ->  -- ok, we can close the door
           let nt = Tile (Door hv (toOpen False)) is
               clmap = M.insert (shift ploc dir) (nt, nt) nlmap
               clvl  = updateLMap (const clmap) lvl
           in loop session (updateLevel (const clvl) nstate) ""
       _ -> msg_no_door

  -- search for secret doors
  search abort =
    let searchTile (Tile (Door hv (Just n)) x,t') = Just (Tile (Door hv (Just (max (n - 1) 0))) x, t')
        searchTile t                              = Just t
        slmap = foldl (\ l m -> update searchTile (shift ploc m) l) nlmap moves
    in  loop session (updateLevel (const (updateLMap (const slmap) lvl)) nstate) ""

  -- quitting the game
  userQuits h =
    do
      let msg   = "Press Space or Return to permanently abandon the game."
          abort = displayCurrent "Game resumed." Nothing >> h
      displayCurrent (msg ++ " --your choice?--  ") Nothing
      getOptionalConfirm
        session (\ b -> if b then shutdown session else abort) (\ _ -> abort)

  -- saving the game
  userSavesGame =
    do
      displayCurrent ("Saving game." ++ more) Nothing
      go <- getConfirm session
      saveGame mstate
      handleScores go False False False
      shutdown session

  -- flee the dungeon
  fleeDungeon =
    let items   = mitems player
        winMsg  = "Congratulations, you won! Your loot, worth "
                  ++ show total ++ " gold, is:"
    in
     if total == 0
         then do
                displayCurrent ("Chicken!" ++ more) Nothing
                getConfirm session
                displayCurrent
                  ("Next time try to grab some loot before you flee!" ++ more)
                  Nothing
                getConfirm session
                shutdown session
         else do
                displayItems displayCurrent nstate winMsg True items
                go <- getConfirm session
                handleScores go True False True
                shutdown session

  -- calculate loot's worth
  total = L.sum $ L.map price $ items
    where
      price i = if iletter i == Just '$' then icount i else 10 * icount i
      items   = mitems player

  -- handle current score and display it with the high scores
  handleScores go write killed victor =
    let moreM msg s = displayCurrent msg (Just (s ++ more))
                      >> getConfirm session
        points      = if killed then (total + 1) `div` 2 else total
        current     = levelNumber nm
    in
     if not go || total == 0
     then return ()
     else do
       curDate <- getClockTime
       let score = HighScores.ScoreRecord
                   points (-time) curDate current killed victor
       (placeMsg, slideshow) <- HighScores.register write score
       mapM_ (moreM placeMsg) slideshow

  -- perform a level change
  lvlchange vdir abort =
    case nlmap `at` ploc of
      Tile (Stairs _ vdir' next) is
       | vdir == vdir' -> -- ok
          case next of
            Nothing ->
              -- we are at the top (or bottom or lift)
              fleeDungeon
            Just (nln, nloc) ->
              -- perform level change
              do
                -- put back current level
                -- (first put back, then get, in case we change to the same level!)
                let full = putDungeonLevel lvl (sdungeon nstate)
                -- get new level
                    (new, ndng) = getDungeonLevel nln full
                    lstate = nstate { sdungeon = ndng, slevel = new }
                loop session (updatePlayer (const (player { mloc = nloc })) lstate) ""
      _ -> -- no stairs
           let txt = if vdir == Up then "up" else "down" in
           displayCurrent ("no stairs " ++ txt) Nothing >> abort

  -- run into a direction
  abortState = updatePlayer (const $ player { mdir = Nothing }) ustate
  run dir =
    do
      let dirState = if Just dir == mdir nplayer
                     then nstate
                     else updatePlayer
                            (const $ nplayer {mdir = Just dir}) nstate
          abort    = handle session abortState per ""
      moveOrAttack
        False   -- attacks are disallowed while running
        (loop session)
        abort
        dirState per APlayer dir
  continueRun dir =
    if S.null (mslocs `S.intersection` pvisible per)  -- no monsters visible
       && L.null oldmsg  -- no news announced
    then hop (tterrain $ nlmap `at` ploc)
    else abort
      where
        abort  = handle session abortState per oldmsg
        mslocs = S.fromList $ L.map mloc ms
        dirOK  = accessible nlmap ploc (ploc `shift` dir)
        exit (Stairs {})  = True
        exit (Opening {}) = True
        exit (Door {})    = True
        exit _            = False
        hop t
          | exit t    = abort
        hop Corridor  =
          -- in corridors, explore all corners and stop at all crossings
          let ns = L.filter (\ x -> distance (neg dir, x) > 1
                                    && accessible nlmap ploc (ploc `shift` x))
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
              ls = L.map (ploc `shift`) ns
              as = L.filter (\ x -> accessible nlmap ploc x
                                    || openable 0 nlmap x) ls
              ts = L.map (tterrain . (nlmap `at`)) as
          in  if L.any exit ts then abort else run dir

  -- perform a player move
  move abort dir = moveOrAttack
                     True   -- attacks are allowed
                     (loop session)
                     abort
                     nstate per APlayer dir

type PassiveHandler a =
  (Message -> Maybe String -> IO Bool) ->  -- display
  (State -> Message -> IO a) ->            -- success continuation
  IO a ->                                  -- failure continuation
  State -> IO a

type Handler a = Session -> PassiveHandler a

drinkPotion :: Handler a
drinkPotion session displayCurrent continue abort
            state@(State { slevel  = nlvl@(Level { lmap = nlmap }),
                           splayer = nplayer@(Monster { mloc = ploc }) })
    | L.null (mitems nplayer) =
      displayCurrent "You are not carrying anything." Nothing >> abort
    | otherwise =
    do
      i <- getPotions session displayCurrent state
                      "What to drink?" (mitems nplayer)
      case i of
        Just i'@(Item { itype = Potion ptype, icount = ic }) ->
                   let remaining = if ic == 1
                                   then []
                                   else [i' {icount = ic - 1}]
                       iplayer = nplayer { mitems = remaining ++ deleteBy ((==) `on` iletter) i' (mitems nplayer) }
                       t = nlmap `at` ploc
                       msg = subjectMonster (mtype nplayer) ++ " " ++
                             verbMonster (mtype nplayer) "drink" ++ " " ++
                             objectItem state 1 (itype i') ++ ". " ++
                             pmsg ptype
                       pmsg PotionWater   = "Tastes like water."
                       pmsg PotionHealing = "You feel better."
                       fplayer PotionWater   =
                         iplayer
                       fplayer PotionHealing =
                         iplayer { mhp = mhp iplayer + playerHP `div` 5}
                   in  continue (updateLevel       (const nlvl) $
                                 updatePlayer      (const (fplayer ptype)) $
                                 updateDiscoveries (S.insert (itype i')) $
                                 state) msg
        Just _  -> displayCurrent "you cannot drink that" Nothing >> abort
        Nothing -> displayCurrent "never mind" Nothing >> abort

dropItem :: Handler a
dropItem session displayCurrent continue abort
         state@(State { splayer = nplayer@(Monster { mloc = ploc }) })
    | L.null (mitems nplayer) =
      displayCurrent "You are not carrying anything." Nothing >> abort
    | otherwise =
    do
      i <- getAnyItem session displayCurrent state
                      "What to drop?" (mitems nplayer)
      case i of
        Just i' -> let iplayer = nplayer { mitems = deleteBy ((==) `on` iletter) i' (mitems nplayer) }
                       msg = subjectMonster (mtype nplayer) ++ " " ++
                             verbMonster (mtype nplayer) "drop" ++ " " ++
                             objectItem state (icount i') (itype i') ++ "."
                   in  continue (updateLevel (scatterItems [i'] ploc) $
                                 updatePlayer (const iplayer) $
                                 state) msg
        Nothing -> displayCurrent "never mind" Nothing >> abort

actorPickupItem :: Actor -> PassiveHandler a
actorPickupItem actor
  displayCurrent continue abort
  state@(State { slevel = lvl@(Level { lmap = lmap }) }) =
    do
      -- check if something is here to pick up
      let monster = getActor state actor
          loc     = mloc monster
          t       = lmap `at` loc
      case titems t of
        []      ->  displayCurrent "nothing here" Nothing >> abort
        (i:rs)  ->
          case assignLetter (iletter i) (mletter monster) (mitems monster) of
            Just l  ->
              let msg = -- (complete sentence, more adequate for monsters)
                        {-
                        subjectMonster (mtype player) ++ " " ++
                        compoundVerbMonster (mtype player) "pick" "up" ++ " " ++
                        objectItem (icount i) (itype i) ++ "."
                        -}
                        letterLabel (iletter ni)
                        ++ objectItem state (icount ni) (itype ni)
                  nt = t { titems = rs }
                  ntRemember = lmap `rememberAt` loc
                  nlmap = M.insert loc (nt, ntRemember) lmap
                  (ni,nitems) = joinItem (i { iletter = Just l }) (mitems monster)
                  updMonster m = m { mitems  = nitems,
                                     mletter = maxLetter l (mletter monster) }
                  nlvl = updateLMap (const nlmap) lvl
                  nstate = updateLevel (const nlvl) state
              in
               updateActor updMonster (\ _ st -> continue st msg) actor nstate
            Nothing ->
              displayCurrent "cannot carry any more" Nothing >> abort

pickupItem :: Handler a
pickupItem _session displayCurrent continue abort state =
  actorPickupItem APlayer displayCurrent continue abort state

getPotions :: Session ->
              (Message -> Maybe String -> IO Bool) ->
              State ->
              String ->
              [Item] ->
              IO (Maybe Item)
getPotions session displayCurrent state prompt is =
  getItem session displayCurrent  state
          prompt (\ i -> case itype i of Potion {} -> True; _ -> False)
          "Potions in your inventory:" is

getAnyItem :: Session ->
              (Message -> Maybe String -> IO Bool) ->
              State ->
              String ->
              [Item] ->
              IO (Maybe Item)
getAnyItem session displayCurrent state prompt is =
  getItem session displayCurrent state
          prompt (const True) "Objects in your inventory:" is

getItem :: Session ->
           (Message -> Maybe String -> IO Bool) ->
           State ->
           String ->
           (Item -> Bool) ->
           String ->
           [Item] ->
           IO (Maybe Item)
getItem session displayCurrent state prompt p ptext is0 =
  let is = L.filter p is0
      choice | L.null is = "[*]"
             | otherwise = "[" ++ letterRange (concatMap (maybeToList . iletter) is) ++ " or ?*]"
      r = do
            displayCurrent (prompt ++ " " ++ choice) Nothing
            let h = nextCommand session >>= h'
                h' e =
                      case e of
                        "question" -> do
                                        b <- displayItems displayCurrent state
                                                          ptext True is
                                        if b then getOptionalConfirm session (const r) h'
                                             else r
                        "asterisk" -> do
                                        b <- displayItems displayCurrent state
                                                          "Objects in your inventory:" True is0
                                        if b then getOptionalConfirm session (const r) h'
                                             else r
                        [l]        -> return (find (\ i -> maybe False (== l) (iletter i)) is0)
                        _          -> return Nothing
            h
  in r

displayItems :: (Message -> Maybe String -> IO Bool) ->
                State ->
                Message ->
                Bool ->
                [Item] ->
                IO Bool
displayItems displayCurrent state msg sorted is =
    do
      let inv = unlines $
                L.map (\ (Item { icount = c, iletter = l, itype = t }) ->
                         letterLabel l ++ objectItem state c t ++ " ")
                      ((if sorted then sortBy (cmpLetter' `on` iletter) else id) is)
      let ovl = inv ++ more
      displayCurrent msg (Just ovl)


moveOrAttack :: Bool ->                                     -- allow attacks?
                (State -> Message -> IO a) ->               -- success continuation
                IO a ->                                     -- failure continuation
                State ->
                Perception ->                               -- perception of the player
                Actor ->                                    -- who's moving?
                Dir -> IO a
moveOrAttack allowAttacks
             continue abort
             state@(State { slevel  = nlvl@(Level { lmap = nlmap }),
                            splayer = player })
             per actor dir
      -- to prevent monsters from hitting themselves
    | dir == (0,0) = continue state ""
      -- At the moment, we check whether there is a monster before checking accessibility
      -- i.e., we can attack a monster on a blocked location. For instance,
      -- a monster on an open door can be attacked diagonally, and a
      -- monster capable of moving through walls can be attacked from an
      -- adjacent position.
    | not (L.null attacked) =
        if allowAttacks then
          do
            let sword = strongestSword (mitems am)
            let damage m =
                  let newHp = mhp m - 3 - sword
                  in  if newHp <= 0
                      then
                        -- grant an immediate move to die
                        m { mhp = 0, mtime = 0 }
                      else
                        m { mhp = newHp}
            let combatVerb m
                  | mhp m > 0 = "hit"
                  | otherwise = "kill"
            let swordMsg = if sword == 0
                           then ""
                           else " with a (+" ++ show sword ++ ") sword"
            let combatMsg m  = subjectMonster (mtype am) ++ " " ++
                               verbMonster (mtype am) (combatVerb m) ++ " " ++
                               objectMonster (mtype m) ++ swordMsg ++ "."
            let perceivedMsg m
                  | mloc m `S.member` pvisible per = combatMsg m
                  | otherwise                      = "You hear some noises."
            let sortmtime = sortBy (\ x y -> compare (mtime x) (mtime y))
            let updateVictims s msg (a:r) =
                  updateActor damage (\ m s -> updateVictims s
                                                   (addMsg msg (perceivedMsg m)) r)
                              a s
                updateVictims s msg [] = continue s {- (updateMonsters sortmtime l) -} msg
            updateVictims state "" attacked
        else
          abort
      -- Perform a move.
    | accessible nlmap aloc naloc =
        updateActor (\ m -> m { mloc = naloc })
                    (\ _ s -> continue s (if actor == APlayer
                                          then lookAt False state nlmap naloc else ""))
                    actor state
      -- Bump into a door/wall, that is try to open it/examine it
    | allowAttacks =
      case nlmap `at` naloc of
        Tile (Door hv (Just n)) is
          | n > 0 && actor == APlayer ->  -- secret door
            abort  -- nothing interesting spotted on the wall
        Tile (Door hv (Just n)) is ->  -- not secret or not the hero
          let nt = Tile (Door hv Nothing) is
              ntRemember = nlmap `rememberAt` naloc
              clmap = M.insert naloc (nt, ntRemember) nlmap
              clvl  = updateLMap (const clmap) nlvl
          in  continue (updateLevel (const clvl) state) ""
        Tile (Door hv Nothing) is ->  -- open door
          abort  -- already open, so the hero must be bumping diagonally
        _ ->
          abort  -- nothing interesting spotted on the wall
    | otherwise = abort
    where am :: Monster
          am     = getActor state actor
          aloc :: Loc
          aloc   = mloc am
          source = nlmap `at` aloc
          naloc  = shift aloc dir
          target = nlmap `at` naloc
          attackedPlayer   = [ APlayer | mloc player == naloc ]
          attackedMonsters = L.map AMonster $
                             findIndices (\ m -> mloc m == naloc) (lmonsters nlvl)
          attacked :: [Actor]
          attacked = attackedPlayer ++ attackedMonsters
