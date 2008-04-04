module Turn where

import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Char

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

-- | Perform a complete turn (i.e., monster moves etc.)
loop :: Session -> Level -> State -> String -> IO ()
loop session (lvl@(Level nm sz ms smap lmap lmeta))
             (state@(State { splayer = player@(Monster { mhp = php, mloc = ploc }), stime = time }))
             oldmsg =
  do
    -- player HP regeneration, TODO: remove hardcoded max and time interval
    let nphp = if time `mod` 1500 == 0 then (php + 1) `min` playerHP else php
    -- update smap
    let nsmap = M.insert ploc (time + smellTimeout) smap
    -- generate new monsters
    nlvl <- rndToIO (addMonster lvl player)
    -- determine player perception
    let per = perception ploc lmap
    -- perform monster moves
    handleMonsters session (nlvl { lsmell = nsmap })
                           (updatePlayer state (\ p -> p { mhp = nphp })) per oldmsg

-- | Handle monster moves. The idea is that we perform moves
--   as long as there are monsters that have a move time which is
--   less than or equal to the current time.
handleMonsters :: Session -> Level -> State -> Perception -> String -> IO ()
handleMonsters session lvl@(Level { lmonsters = ms })
               (state@(State { stime = time }))
               per oldmsg =
    case ms of
      [] -> -- there are no monsters, just continue
            handle session lvl nstate per oldmsg
      (m@(Monster { mtime = mt }) : ms)
         | mt > time  -> -- all the monsters are not yet ready for another move,
                         -- so check the player
                            handle session lvl nstate per oldmsg
         | mhp m <= 0 -> -- the monster dies
                            handleMonsters session (updateMonsters lvl (const ms))
                                           state per oldmsg
         | otherwise  -> -- monster m should move
                            handleMonster m session lvl state per oldmsg
  where
    nstate = state { stime = time + 1 }

-- | Handle the move of a single monster.
handleMonster m session lvl@(Level { lmonsters = ms, lsmell = nsmap, lmap = lmap })
              (state@(State { splayer = player@(Monster { mloc = ploc }), stime = time }))
              per oldmsg =
  do
    -- candidate directions: noses usually move randomly, whereas
    -- eyes favour to keep their old direction
    let ns | mtype m == Nose = moves
           | (mtype m == Eye || mtype m == FastEye) && mloc m `S.member` pvisible per =
               let t = towards (mloc m,ploc)
               in maybe id
                        (\ d -> L.filter (\ x -> distance (t,x) <= 1))
                        (mdir m) moves
           | otherwise       =
               maybe id
                     (\ d -> L.filter (\ x -> distance (neg d,x) > 1)) 
                     (mdir m) moves
    -- those candidate directions that lead to accessible fields
    let fns = (if mtype m == Eye || mtype m == FastEye
                 then L.filter (\ x -> unoccupied ms lmap (mloc m `shift` x))
                 else id) $
              L.filter (\ x -> accessible lmap (mloc m) (mloc m `shift` x)) ns
    -- smells of the accessible fields
    let smells = zip fns
                     (L.map (\ x -> (nsmap ! (mloc m `shift` x) - time) `max` 0) fns)
    -- direction and value of maximum smell
    let msmell = maximumBy (\ (_,s1) (_,s2) -> compare s1 s2) smells
    nl <- if adjacent ploc (mloc m) then
            -- attack player
            return (ploc `shift` neg (mloc m))
          else if (mtype m == Eye || mtype m == FastEye) && not (L.null fns) then
            rndToIO (oneOf fns)
          else if mtype m == Nose && not (L.null smells) && snd msmell > 0 then
            return (fst msmell)
          else if not (L.null fns) then
            rndToIO (oneOf fns)
          else -- fallback: wait
               return (0,0)
    -- increase the monster move time and set direction
    let nm = m { mtime = time + mspeed m, mdir = if nl == (0,0) then Nothing else Just nl }
    let (act, nms) = insertMonster nm ms
    let nlvl = updateMonsters lvl (const nms)
    moveOrAttack
      True
      (\ nlvl np msg ->
         handleMonsters session nlvl (updatePlayer state (const np)) per
                        (addMsg oldmsg msg))
      (handleMonsters session nlvl state per oldmsg)
      nlvl player per
      (AMonster act)
      nl


-- | Display current status and handle the turn of the player.
handle :: Session -> Level -> State -> Perception -> String -> IO ()
handle session (lvl@(Level nm sz ms smap lmap lmeta))
               (state@(State { splayer = player@(Monster { mhp = php, mdir = pdir, mloc = ploc, mitems = pinv, mtime = ptime }), stime = time }))
               per oldmsg =
  do
    -- check for player death
    if php <= 0
      then do
             displayCurrent (addMsg oldmsg ("You die ..." ++ more))
             getConfirm session
             shutdown session
      else -- check if the player can make another move yet
           if ptime > time then loop session nlvl state oldmsg 
      else do
             displayCurrent oldmsg
             let h = do
                       e <- nextEvent session
                       handleDirection e (move h) $ 
                         handleDirection (L.map toLower e) run $
                         handleModifier e h $
                         case e of
                           "o"       -> openclose True h
                           "c"       -> openclose False h
                           "s"       -> search h

                           "less"    -> lvlchange Up h
                           "greater" -> lvlchange Down h

                           "comma"   -> pickup h

                           -- saving or ending the game
                           "S"       -> saveGame lvl state >> shutdown session
                           "Q"       -> shutdown session
                           "Escape"  -> shutdown session

                           -- wait
                           "space"   -> loop session nlvl nstate ""
                           "period"  -> loop session nlvl nstate ""

                           -- look
                           "colon"   -> displayCurrent (lookAt True nlmap ploc) >> h

                           -- display modes
                           "V"       -> handle session nlvl (toggleVision state) per oldmsg
                           "R"       -> handle session nlvl (toggleSmell state) per oldmsg
                           "O"       -> handle session nlvl (toggleOmniscient state) per oldmsg
                           "T"       -> handle session nlvl (toggleTerrain state) per oldmsg

                           -- meta information
                           "M"       -> displayCurrent lmeta >> h
                           "v"       -> displayCurrent version >> h

                           s   -> displayCurrent ("unknown command (" ++ s ++ ")") >> h
             maybe h continueRun pdir

 where

  reachable = preachable per
  visible   = pvisible per

  displayCurrent = displayLevel session nlvl per state

  -- update player memory
  nlmap = foldr (\ x m -> M.update (\ (t,_) -> Just (t,t)) x m) lmap (S.toList visible)
  nlvl = updateLMap lvl (const nlmap)

  -- update player action time
  nplayer = player { mtime = time + mspeed player }
  nstate  = updatePlayer state (const nplayer)

  -- picking up items
  pickup abort =
    do
      -- check if something is here to pick up
      let t = nlmap `at` ploc
      case titems t of
        []      ->  displayCurrent "nothing here" >> abort
        (i:rs)  ->  let msg = subjectMonster (mtype player) ++ " " ++
                              compoundVerbMonster (mtype player) "pick" "up" ++ " " ++
                              objectItem i ++ "."
                        nt = t { titems = rs }
                        plmap = M.insert ploc (nt, nt) nlmap
                        iplayer = nplayer { mitems = i : mitems nplayer }
                    in  loop session (updateLMap lvl (const plmap))
                                     (updatePlayer nstate (const iplayer)) msg

  -- open and close doors
  openclose o abort =
    do
      displayCurrent "direction?"
      e <- nextEvent session
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
                                    in loop session (updateLMap lvl (const clmap)) nstate ""
        _ -> displayCurrent "never mind" >> abort
  -- search for secret doors
  search abort =
    let searchTile (Tile (Door hv (Just n)) x,t') = Just $ (Tile (Door hv (Just (max (n - 1) 0))) x, t')
        searchTile t                              = Just t
        slmap = foldl (\ l m -> update searchTile (shift ploc m) l) nlmap moves
    in  loop session (updateLMap lvl (const slmap)) nstate ""
  -- perform a level change
  lvlchange vdir abort =
    case nlmap `at` ploc of
      Tile (Stairs vdir' next) is
       | vdir == vdir' -> -- ok
          case next of
            Nothing      -> -- exit dungeon
                            shutdown session
            Just (nln, nloc) ->
              -- perform level change
              do
                -- put back current level
                -- (first put back, then get, in case we change to the same level!)
                let full = putDungeonLevel lvl (sdungeon nstate)
                -- get new level
                    (new, ndng) = getDungeonLevel nln full
                    lstate = nstate { sdungeon = ndng }
                loop session new (updatePlayer lstate (const (player { mloc = nloc }))) ""
      _ -> -- no stairs
           let txt = if vdir == Up then "up" else "down" in
           displayCurrent ("no stairs " ++ txt) >> abort
  -- run into a direction
  run dir =
    do
      let mplayer = nplayer { mdir = Just dir }
          abort   = handle session nlvl (updatePlayer state (const $ player { mdir = Nothing })) per ""
      moveOrAttack
        False   -- attacks are disallowed while running
        (\ l p -> loop session l (updatePlayer nstate (const p)))
        abort
        nlvl mplayer per APlayer dir
  continueRun dir =
    let abort = handle session nlvl (updatePlayer state (const $ player { mdir = Nothing })) per oldmsg
        dloc  = shift ploc dir
    in  case (oldmsg, nlmap `at` ploc) of
          (_:_, _)                 -> abort
          (_, Tile (Opening _) _)  -> abort
          (_, Tile (Door _ _) _)   -> abort
          (_, Tile (Stairs _ _) _) -> abort
          _
            | accessible nlmap ploc dloc ->
                moveOrAttack
                  False    -- attacks are disallowed while running
                  (\ l p -> loop session l (updatePlayer nstate (const p)))
                  abort
                  nlvl nplayer per APlayer dir
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
                     (\ l p -> loop session l (updatePlayer nstate (const p)))
                     abort
                     nlvl nplayer per APlayer dir

moveOrAttack :: Bool ->                                     -- allow attacks?
                (Level -> Player -> String -> IO a) ->      -- success continuation
                IO a ->                                     -- failure continuation
                Level ->                                    -- the level
                Player ->                                   -- the player
                Perception ->                               -- perception of the player
                Actor ->                                    -- who's moving?
                Dir -> IO a
moveOrAttack allowAttacks
             continue abort
             nlvl@(Level { lmap = nlmap }) player per
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
                updateVictims l p msg [] = continue l {- (updateMonsters l sortmtime) -} p msg
            updateVictims nlvl player "" attacked
        else
          abort
      -- Perform a move.
    | accessible nlmap aloc naloc = 
        updateActor (\ m -> m { mloc = naloc })
                    (\ _ l p -> continue l p (if actor == APlayer
                                              then lookAt False nlmap naloc else ""))
                    actor nlvl player
    | otherwise = abort
    where am :: Monster
          am     = getActor nlvl player actor
          aloc :: Loc
          aloc   = mloc am
          source = nlmap `at` aloc
          naloc  = shift aloc dir
          target = nlmap `at` naloc
          attackedPlayer   = if mloc player == naloc then [APlayer] else []
          attackedMonsters = L.map AMonster $
                             findIndices (\ m -> mloc m == naloc) (lmonsters nlvl)
          attacked :: [Actor]
          attacked         = attackedPlayer ++ attackedMonsters


