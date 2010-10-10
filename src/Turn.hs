module Turn where

import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Char
import Data.Maybe
import Data.Function

import State
import Geometry
import Level
import Dungeon
import Monster
import Actor
import Perception
import Item
import Display2
import Random
import Save
import Message
import Version
import Strategy

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
    handleMonsters session (lvl { lsmell = nsmap }) state per oldmsg

-- | Handle monster moves. The idea is that we perform moves
--   as long as there are monsters that have a move time which is
--   less than or equal to the current time.
handleMonsters :: Session -> Level -> State -> Perception -> Message -> IO ()
handleMonsters session lvl@(Level { lmonsters = ms })
               (state@(State { stime = time }))
               per oldmsg =
    -- for debugging: causes redraw of the current state for every monster move; slow!
    -- displayLevel session lvl per state oldmsg >>
    case ms of
      [] -> -- there are no monsters, just continue
            handlePlayer
      (m@(Monster { mtime = mt }) : ms)
         | mt > time  -> -- all the monsters are not yet ready for another move,
                         -- so continue
                            handlePlayer
         | mhp m <= 0 -> -- the monster dies
                            handleMonsters session (updateMonsters (const ms) lvl)
                                           state per oldmsg
         | otherwise  -> -- monster m should move
                            handleMonster m session (updateMonsters (const ms) lvl)
                                          state per oldmsg
  where
    nstate = state { stime = time + 1 }

    -- good place to do everything that has to be done for every *time*
    -- unit; currently, that's monster generation
    handlePlayer =
      do
        nlvl <- rndToIO (addMonster lvl (splayer nstate))
        handle session (updateLevel (const nlvl) nstate) per oldmsg

-- | Handle the move of a single monster.
handleMonster :: Monster -> Session -> Level -> State -> Perception -> Message ->
                 IO ()
handleMonster m session lvl@(Level { lmonsters = ms, lsmell = nsmap, lmap = lmap })
              (state@(State { splayer = player@(Monster { mloc = ploc }), stime = time }))
              per oldmsg =
  do
    nl <- rndToIO (frequency (head (runStrategy (strategy m lvl state per .| wait))))

    -- increase the monster move time and set direction
    let nm = m { mtime = time + mspeed m, mdir = if nl == (0,0) then Nothing else Just nl }
    let (act, nms) = insertMonster nm ms
    let nlvl = updateMonsters (const nms) lvl
    moveOrAttack
      True
      (\ nlvl np msg ->
         handleMonsters session nlvl (updatePlayer (const np) state) per
                        (addMsg oldmsg msg))
      (handleMonsters session nlvl state per oldmsg)
      nlvl player (sassocs state) (sdiscoveries state) per
      (AMonster act)
      nl

strategy :: Monster -> Level -> State -> Perception -> Strategy Loc
strategy m@(Monster { mtype = mt, mloc = me, mdir = mdir })
         lvl@(Level { lmonsters = ms, lsmell = nsmap, lmap = lmap })
         (state@(State { splayer = player@(Monster { mloc = ploc }), stime = time }))
         per =
    case mt of
      Eye     -> eye
      FastEye -> eye
      Nose    -> nose
      _       -> onlyAccessible moveRandomly
  where
    -- we check if the monster is visible by the player rather than if the
    -- player is visible by the monster -- this is more efficient, but
    -- won't be correct in the general situation
    playerVisible      =  me `S.member` pvisible per
    playerAdjacent     =  adjacent me ploc
    towardsPlayer      =  towards (me, ploc)
    onlyTowardsPlayer  =  only (\ x -> distance (towardsPlayer, x) <= 1)
    onlyPreservesDir   =  only (\ x -> maybe True (\ d -> distance (neg d, x) > 1) mdir)
    onlyUnoccupied     =  onlyMoves (unoccupied ms lmap) me
    onlyAccessible     =  onlyMoves (accessible lmap me) me
    smells             =  L.map fst $
                          L.sortBy (\ (_,s1) (_,s2) -> compare s2 s1) $
                          L.filter (\ (_,s) -> s > 0) $
                          L.map (\ x -> (x, nsmap ! (me `shift` x) - time `max` 0)) moves

    eye                =  playerAdjacent .=> return towardsPlayer
                          .| (onlyUnoccupied $ onlyAccessible $
                                 playerVisible  .=> onlyTowardsPlayer moveRandomly
                              .| onlyPreservesDir moveRandomly)

    nose               =  playerAdjacent .=> return towardsPlayer
                          .| (onlyAccessible $
                                 foldr (.|) reject (L.map return smells)
                              .| moveRandomly)

onlyMoves :: (Dir -> Bool) -> Loc -> Strategy Dir -> Strategy Dir
onlyMoves p l = only (\ x -> p (l `shift` x))

moveRandomly :: Strategy Dir
moveRandomly = liftFrequency $ uniform moves

wait :: Strategy Dir
wait = return (0,0)

-- | Display current status and handle the turn of the player.
handle :: Session -> State -> Perception -> Message -> IO ()
handle session (state@(State { splayer = player@(Monster { mhp = php, mdir = pdir, mloc = ploc, mitems = pinv, mtime = ptime }),
                               stime   = time,
                               sassocs = assocs,
                               sdiscoveries = discs,
                               slevel  = lvl@(Level nm sz ms smap lmap lmeta) }))
               per oldmsg =
    -- check for player death
    if php <= 0
      then do
             displayCurrent (addMsg oldmsg more)
             getConfirm session
             displayCurrent ("You die." ++ more)
             getConfirm session
             shutdown session
      else -- check if the player can make another move yet
           if ptime > time then
             do
               -- do not make intermediate redraws while running
               maybe (displayLevel session per state "") (const $ return ()) pdir
               handleMonsters session lvl state per oldmsg
           -- NOTE: It's important to call handleMonsters here, not loop,
           -- because loop does all sorts of calculations that are only
           -- really necessary after the player has moved.
      else do
             displayCurrent oldmsg
             let h = nextCommand session >>= h'
                 h' e =
                       handleDirection e (move h) $
                         handleDirection (L.map toLower e) run $
                         case e of
                           "o"       -> openclose True h
                           "c"       -> openclose False h
                           "s"       -> search h

                           "less"    -> lvlchange Up h
                           "greater" -> lvlchange Down h

                           -- items
                           "comma"   -> pickup h
                           "d"       -> drop h
                           "i"       -> inventory h
                           "q"       -> drink h

                           -- saving or ending the game
                           "S"       -> saveGame mstate >> shutdown session
                           "Q"       -> shutdown session
                           "Escape"  -> displayCurrent "Press Q to quit." >> h

                           -- wait
                           "space"   -> loop session nstate ""
                           "period"  -> loop session nstate ""

                           -- look
                           "colon"   -> lookAround h

                           -- display modes
                           "V"       -> handle session (toggleVision     ustate) per oldmsg
                           "R"       -> handle session (toggleSmell      ustate) per oldmsg
                           "O"       -> handle session (toggleOmniscient ustate) per oldmsg
                           "T"       -> handle session (toggleTerrain    ustate) per oldmsg

                           -- meta information
                           "M"       -> displayCurrent' "" (unlines (shistory mstate) ++ more) >>= \ b ->
                                        if b then getOptionalConfirm session
                                                    (const (displayCurrent "" >> h)) h'
                                             else displayCurrent "" >> h
                           "I"       -> displayCurrent lmeta >> h
                           "v"       -> displayCurrent version >> h

                           s   -> displayCurrent ("unknown command (" ++ s ++ ")") >> h
             maybe h continueRun pdir

 where

  -- we record the oldmsg in the history
  mstate = if L.null oldmsg then state else updateHistory (take 500 . ((oldmsg ++ " "):)) state
    -- TODO: make history max configurable

  reachable = preachable per
  visible   = pvisible per

  displayCurrent :: Message -> IO ()
  displayCurrent  = displayLevel session per ustate

  displayCurrent' :: Message -> String -> IO Bool
  displayCurrent' = displayOverlay session per ustate

  -- update player memory
  nlmap = foldr (\ x m -> M.update (\ (t,_) -> Just (t,t)) x m) lmap (S.toList visible)
  nlvl = updateLMap (const nlmap) lvl

  -- state with updated player memory, but without any passed time
  ustate  = updateLevel (const nlvl) state

  -- update player action time, and regenerate hitpoints
  -- player HP regeneration, TODO: remove hardcoded max and time interval
  nplayer = player { mtime = time + mspeed player,
                     mhp   = if time `mod` 1500 == 0 then (php + 1) `min` playerHP else php }
  nstate  = updateLevel (const nlvl) (updatePlayer (const nplayer) mstate)

  -- picking up items
  pickup abort = pickupItem
                   displayCurrent
                   (\ l p -> loop session (updateLevel (const l) (updatePlayer (const p) nstate)))
                   abort
                   nstate

  -- dropping items
  drop abort = dropItem
                 session
                 displayCurrent
                 displayCurrent'
                 (\ l p -> loop session (updateLevel (const l) (updatePlayer (const p) nstate)))
                 abort
                 nstate

  -- drinking potions
  drink abort = drinkPotion
                  session
                  displayCurrent
                  displayCurrent'
                  (\ l p d -> loop session 
                                (updateLevel (const l) (updateDiscoveries (S.union d) (updatePlayer (const p) nstate))))
                  abort
                  nstate

  -- display inventory
  inventory abort
    | L.null (mitems player) =
      displayCurrent "You are not carrying anything." >> abort
    | otherwise =
    do
      displayItems displayCurrent' assocs discs
                   "This is what you are carrying:" True (mitems player)
      getConfirm session
      displayCurrent ""
      abort  -- looking at inventory doesn't take any time

  -- look around at current location
  lookAround abort =
    do
      -- check if something is here to pick up
      let t = nlmap `at` ploc
      if length (titems t) <= 2
        then displayCurrent (lookAt True assocs discs nlmap ploc) >> abort
        else
          do
             displayItems displayCurrent' assocs discs
                          (lookAt True assocs discs nlmap ploc) False (titems t)
             getConfirm session
             displayCurrent ""
             abort  -- looking around doesn't take any time

  -- open and close doors
  openclose o abort =
    do
      displayCurrent "direction?"
      e <- nextCommand session
      handleDirection e (openclose' o abort) (displayCurrent "never mind" >> abort)
  openclose' o abort dir =
    let txt  = if o then "open" else "closed"
        dloc = shift ploc dir
    in
      case nlmap `at` dloc of
        Tile d@(Door hv o') is
                   | secret o'   -> displayCurrent "never mind" >> abort
                   | toOpen (not o) /= o'
                                 -> displayCurrent ("already " ++ txt) >> abort
                   | not (unoccupied ms nlmap dloc)
                                 -> displayCurrent "blocked" >> abort
                   | otherwise   -> -- ok, we can open/close the door
                                    let nt = Tile (Door hv (toOpen o)) is
                                        clmap = M.insert (shift ploc dir) (nt, nt) nlmap
                                    in loop session (updateLevel (const (updateLMap (const clmap) lvl)) nstate) ""
        _ -> displayCurrent "never mind" >> abort
  -- search for secret doors
  search abort =
    let searchTile (Tile (Door hv (Just n)) x,t') = Just (Tile (Door hv (Just (max (n - 1) 0))) x, t')
        searchTile t                              = Just t
        slmap = foldl (\ l m -> update searchTile (shift ploc m) l) nlmap moves
    in  loop session (updateLevel (const (updateLMap (const slmap) lvl)) nstate) ""
  -- flee the dungeon
  fleeDungeon =
    let items   = mitems player
        price i = if iletter i == Just '$' then icount i else 10 * icount i
        total   = L.sum $ L.map price $ items
        msg     = "Congratulations, you won! Your loot, worth "
                  ++ show total ++ " gold, is:"
    in
     if total == 0
         then do
                displayCurrent ( "Chicken!" ++ more)
                getConfirm session
                displayCurrent
                  ("Next time try to grab some loot before you flee!" ++ more)
                getConfirm session
                shutdown session
         else do
                displayItems displayCurrent' assocs discs msg True items
                getConfirm session
                shutdown session
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
           displayCurrent ("no stairs " ++ txt) >> abort
  -- run into a direction
  run dir =
    do
      let mplayer = nplayer { mdir = Just dir }
          abort   = handle session (updatePlayer (const $ player { mdir = Nothing }) ustate) per ""
      moveOrAttack
        False   -- attacks are disallowed while running
        (\ l p -> loop session (updateLevel (const l) (updatePlayer (const p) nstate)))
        abort
        nlvl mplayer assocs discs per APlayer dir
  continueRun dir =
    let abort = handle session (updatePlayer (const $ player { mdir = Nothing }) ustate) per oldmsg
        dloc  = shift ploc dir
        mslocs = S.fromList $ L.map mloc ms
    in  case (oldmsg, nlmap `at` ploc) of
          (_:_, _)                   -> abort
          (_, Tile (Opening {}) _)   -> abort
          (_, Tile (Door {}) _)      -> abort
          (_, Tile (Stairs {}) _)    -> abort
          _
            | not $ S.null $ mslocs `S.intersection` pvisible per
                                     -> abort  -- mosters visible
          _
            | accessible nlmap ploc dloc ->
                moveOrAttack
                  False    -- attacks are disallowed while running
                  (\ l p -> loop session (updateLevel (const l) (updatePlayer (const p) nstate)))
                  abort
                  nlvl nplayer assocs discs per APlayer dir
          (_, Tile Corridor _)  -- direction change restricted to corridors
            | otherwise ->
                let ns  = L.filter (\ x -> distance (neg dir,x) > 1
                                        && accessible nlmap ploc (ploc `shift` x)) moves
                    sns = L.filter (\ x -> distance (dir,x) <= 1) ns
                in  case ns of
                      [newdir] -> run newdir
                      _        -> case sns of
                                    [newdir] -> run newdir
                                    _        -> abort
          _ -> abort
  -- perform a player move
  move abort dir = moveOrAttack
                     True   -- attacks are allowed
                     (\ l p -> loop session (updateLevel (const l) (updatePlayer (const p) nstate)))
                     abort
                     nlvl nplayer assocs discs per APlayer dir

-- | Drinking potions.
drinkPotion ::   Session ->                                    -- session
                 (Message -> IO ()) ->                         -- how to display
                 (Message -> String -> IO Bool) ->             -- overlay display
                 (Level -> Player -> Discoveries -> Message -> IO a) ->
                                                               -- success continuation
                 IO a ->                                       -- failure continuation
                 State -> IO a
drinkPotion session displayCurrent displayCurrent' continue abort
            (State { slevel  = nlvl@(Level { lmap = nlmap }),
                     splayer = nplayer@(Monster { mloc = ploc }),
                     sassocs = assocs,
                     sdiscoveries = discs })
    | L.null (mitems nplayer) =
      displayCurrent "You are not carrying anything." >> abort
    | otherwise =
    do
      i <- getPotions session displayCurrent displayCurrent' assocs discs
                      "What to drink?" (mitems nplayer)
      case i of
        Just i'@(Item { itype = Potion ptype }) ->
                   let iplayer = nplayer { mitems = deleteBy ((==) `on` iletter) i' (mitems nplayer) }
                       t = nlmap `at` ploc
                       msg = subjectMonster (mtype nplayer) ++ " " ++
                             verbMonster (mtype nplayer) "drink" ++ " " ++
                             objectItem assocs discs (icount i') (itype i') ++ ". " ++
                             pmsg ptype
                       pmsg PotionWater   = "Tastes like water."
                       pmsg PotionHealing = "You feel better."
                       fplayer PotionWater   = iplayer
                       fplayer PotionHealing = iplayer { mhp = 20 }
                   in  continue nlvl
                                (fplayer ptype) (S.singleton (itype i')) msg
        Just _  -> displayCurrent "you cannot drink that" >> abort
        Nothing -> displayCurrent "never mind" >> abort


-- | Dropping items.
dropItem ::   Session ->                                    -- session
              (Message -> IO ()) ->                         -- how to display
              (Message -> String -> IO Bool) ->             -- overlay display
              (Level -> Player -> Message -> IO a) ->       -- success continuation
              IO a ->                                       -- failure continuation
              State -> IO a
dropItem session displayCurrent displayCurrent' continue abort
         (State { slevel  = nlvl@(Level { lmap = nlmap }),
                  splayer = nplayer@(Monster { mloc = ploc }),
                  sassocs = assocs,
                  sdiscoveries = discs })
    | L.null (mitems nplayer) =
      displayCurrent "You are not carrying anything." >> abort
    | otherwise =
    do
      i <- getAnyItem session displayCurrent displayCurrent' assocs discs
                      "What to drop?" (mitems nplayer)
      case i of
        Just i' -> let iplayer = nplayer { mitems = deleteBy ((==) `on` iletter) i' (mitems nplayer) }
                       t = nlmap `at` ploc
                       nt = t { titems = snd (joinItem i' (titems t)) }
                       plmap = M.insert ploc (nt, nt) nlmap
                       msg = subjectMonster (mtype nplayer) ++ " " ++
                             verbMonster (mtype nplayer) "drop" ++ " " ++
                             objectItem assocs discs (icount i') (itype i') ++ "."
                   in  continue (updateLMap (const plmap) nlvl)
                                iplayer msg
        Nothing -> displayCurrent "never mind" >> abort




-- | Picking up items.
pickupItem :: (Message -> IO ()) ->                         -- how to display
              (Level -> Player -> Message -> IO a) ->       -- success continuation
              IO a ->                                       -- failure continuation
              State -> IO a
pickupItem displayCurrent continue abort
           (State { slevel  = nlvl@(Level { lmap = nlmap }),
                    splayer = nplayer@(Monster { mloc = ploc }),
                    sassocs = assocs,
                    sdiscoveries = discs }) =
    do
      -- check if something is here to pick up
      let t = nlmap `at` ploc
      case titems t of
        []      ->  displayCurrent "nothing here" >> abort
        (i:rs)  ->
          case assignLetter (iletter i) (mletter nplayer) (mitems nplayer) of
            Just l  ->
              let msg = -- (complete sentence, more adequate for monsters)
                        {-
                        subjectMonster (mtype player) ++ " " ++
                        compoundVerbMonster (mtype player) "pick" "up" ++ " " ++
                        objectItem (icount i) (itype i) ++ "."
                        -}
                        letterLabel (iletter ni) ++ objectItem assocs discs (icount ni) (itype ni)
                  nt = t { titems = rs }
                  plmap = M.insert ploc (nt, nt) nlmap
                  (ni,nitems) = joinItem (i { iletter = Just l }) (mitems nplayer)
                  iplayer = nplayer { mitems  = nitems,
                                      mletter = maxLetter l (mletter nplayer) }
              in  continue (updateLMap (const plmap) nlvl)
                           iplayer msg
            Nothing -> displayCurrent "cannot carry anymore" >> abort

getPotions :: Session ->
              (Message -> IO ()) ->
              (Message -> String -> IO Bool) ->
              Assocs ->
              Discoveries ->
              String ->
              [Item] ->
              IO (Maybe Item)
getPotions session displayCurrent displayCurrent' assocs discs prompt is =
  getItem session displayCurrent displayCurrent' assocs discs
          prompt (\ i -> case itype i of Potion {} -> True; _ -> False)
          "Potions in your inventory:" is

getAnyItem :: Session ->
              (Message -> IO ()) ->
              (Message -> String -> IO Bool) ->
              Assocs ->
              Discoveries ->
              String ->
              [Item] ->
              IO (Maybe Item)
getAnyItem session displayCurrent displayCurrent' assocs discs prompt is =
  getItem session displayCurrent displayCurrent' assocs discs
          prompt (const True) "Objects in your inventory:" is

getItem :: Session ->
           (Message -> IO ()) ->
           (Message -> String -> IO Bool) ->
           Assocs ->
           Discoveries ->
           String ->
           (Item -> Bool) ->
           String ->
           [Item] ->
           IO (Maybe Item)
getItem session displayCurrent displayCurrent' assocs discs prompt p ptext is0 =
  let is = L.filter p is0
      choice | L.null is = "[*]"
             | otherwise = "[" ++ letterRange (concatMap (maybeToList . iletter) is) ++ " or ?*]"
      r = do
            displayCurrent (prompt ++ " " ++ choice)
            let h = nextCommand session >>= h'
                h' e =
                      case e of
                        "question" -> do
                                        b <- displayItems displayCurrent' assocs discs
                                                          ptext True is
                                        if b then getOptionalConfirm session (const r) h'
                                             else r
                        "asterisk" -> do
                                        b <- displayItems displayCurrent' assocs discs
                                                          "Objects in your inventory:" True is0
                                        if b then getOptionalConfirm session (const r) h'
                                             else r
                        [l]        -> return (find (\ i -> maybe False (== l) (iletter i)) is0)
                        _          -> return Nothing
            h
  in r

displayItems displayCurrent' assocs discs msg sorted is =
    do
      let inv = unlines $
                L.map (\ (Item { icount = c, iletter = l, itype = t }) ->
                         letterLabel l ++ objectItem assocs discs c t ++ " ")
                      ((if sorted then sortBy (cmpLetter' `on` iletter) else id) is)
      let ovl = inv ++ more
      displayCurrent' msg ovl


moveOrAttack :: Bool ->                                     -- allow attacks?
                (Level -> Player -> Message -> IO a) ->     -- success continuation
                IO a ->                                     -- failure continuation
                Level ->                                    -- the level
                Player ->                                   -- the player
                Assocs ->
                Discoveries ->
                Perception ->                               -- perception of the player
                Actor ->                                    -- who's moving?
                Dir -> IO a
moveOrAttack allowAttacks
             continue abort
             nlvl@(Level { lmap = nlmap }) player assocs discs per
             actor dir
      -- to prevent monsters from hitting themselves
    | dir == (0,0) = continue nlvl player ""
      -- At the moment, we check whether there is a monster before checking accessibility
      -- i.e., we can attack a monster on a blocked location. For instance,
      -- a monster on an open door can be attacked diagonally, and a
      -- monster capable of moving through walls can be attacked from an
      -- adjacent position.
    | not (L.null attacked) =
        if allowAttacks then
          do
            let damage m = case mhp m of
                             1  ->  m { mhp = 0, mtime = 0 }  -- grant an immediate move to die
                             h  ->  m { mhp = h - 1 }
            let combatVerb m
                  | mhp m > 0 = "hit"
                  | otherwise = "kill"
            let combatMsg m  = subjectMonster (mtype am) ++ " " ++
                               verbMonster (mtype am) (combatVerb m) ++ " " ++
                               objectMonster (mtype m) ++ "."
            let perceivedMsg m
                  | mloc m `S.member` pvisible per = combatMsg m
                  | otherwise                      = "You hear some noises."
            let sortmtime = sortBy (\ x y -> compare (mtime x) (mtime y))
            let updateVictims l p msg (a:r) =
                  updateActor damage (\ m l p -> updateVictims l p
                                                   (addMsg msg (perceivedMsg m)) r)
                              a l p
                updateVictims l p msg [] = continue l {- (updateMonsters sortmtime l) -} p msg
            updateVictims nlvl player "" attacked
        else
          abort
      -- Perform a move.
    | accessible nlmap aloc naloc =
        updateActor (\ m -> m { mloc = naloc })
                    (\ _ l p -> continue l p (if actor == APlayer
                                              then lookAt False assocs discs nlmap naloc else ""))
                    actor nlvl player
    | otherwise = abort
    where am :: Monster
          am     = getActor nlvl player actor
          aloc :: Loc
          aloc   = mloc am
          source = nlmap `at` aloc
          naloc  = shift aloc dir
          target = nlmap `at` naloc
          attackedPlayer   = [ APlayer | mloc player == naloc ]
          attackedMonsters = L.map AMonster $
                             findIndices (\ m -> mloc m == naloc) (lmonsters nlvl)
          attacked :: [Actor]
          attacked         = attackedPlayer ++ attackedMonsters
