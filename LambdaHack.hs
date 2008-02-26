module Main where

import System.Directory
import System.IO
import Data.Binary
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Char
import Data.Version
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.Random
import Control.Monad
import Control.Exception as E hiding (handle)
import Codec.Compression.Zlib as Z

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import State
import Geometry
import Level
import Dungeon
import Monster
import Item
import FOV
import Display
import Random

savefile = "LambdaHack.save"

main :: IO ()
main = startup start

strictReadCompressedFile :: FilePath -> IO LBS.ByteString
strictReadCompressedFile f =
    do
      h <- openBinaryFile f ReadMode
      c <- LBS.hGetContents h
      let d = Z.decompress c
      LBS.length d `seq` return d

strictDecodeCompressedFile :: Binary a => FilePath -> IO a
strictDecodeCompressedFile f = fmap decode (strictReadCompressedFile f)

encodeCompressedFile :: Binary a => FilePath -> a -> IO ()
encodeCompressedFile f x = LBS.writeFile f (Z.compress (encode x))
  -- note that LBS.writeFile opens the file in binary mode

-- | Either restore a saved game, or setup a new game.
start :: Session -> IO ()
start session =
    do
      -- check if we have a savegame
      x <- doesFileExist savefile
      restored <- if x
                    then
                      E.catch (do
                                 displayBlankConfirm session "Restoring save game"
                                 r <- strictDecodeCompressedFile savefile
                                 removeFile savefile
                                 case r of
                                   (x,y,z) -> (z :: Bool) `seq` return $ Left (x,y))
                              (\ e -> case e of
                                        _ -> return (Right $ "Restore failed: " ++
                                                     (unwords . lines) (show e)))
                    else
                      return $ Right "Welcome to LambdaHack!"  -- new game
      case restored of
        Right msg        -> generate session msg
        Left (lvl,state) -> handle session lvl state (perception_ state lvl)
                                   "Welcome back to LambdaHack."

more :: String
more = " --more--"

-- | Displays a message on a blank screen. Waits for confirmation.
displayBlankConfirm :: Session -> String -> IO ()
displayBlankConfirm session txt =
  let x = txt ++ more
  in  do
        display ((0,0),(0,length x - 1)) session (const (attr, ' ')) x ""
        getConfirm session

-- | Waits for a space or return.
getConfirm :: Session -> IO ()
getConfirm session =
  do
    e <- nextEvent session
    handleModifier e (getConfirm session) $
      case e of
        "space"  -> return ()
        "Return" -> return ()
        _        -> getConfirm session 

-- | Generate the dungeon for a new game, and start the game loop.
generate :: Session -> String -> IO ()
generate session msg =
  do
    -- generate dungeon with 10 levels
    levels <- rndToIO $ mapM (\n -> level defaultLevelConfig $ "The Lambda Cave " ++ show n) [1..10]
    let state = defaultState ((\ (_,x,_) -> x) (head levels))
    let connect [(x,_,_)] = [x Nothing Nothing]
        connect ((x,_,_):ys@((_,u,_):_)) =
                            let (z:zs) = connect ys
                            in  x Nothing (Just (z,u)) : z : zs
    let lvl = head (connect levels)
    handle session lvl state (perception_ state lvl) msg

-- | Perform a complete move (i.e., monster moves etc.)
loop :: Session -> Level -> State -> String -> IO ()
loop session (lvl@(Level nm sz ms smap lmap lmeta))
             (state@(State { splayer = player@(Monster _ php _ ploc _ _), stime = time }))
             oldmsg =
  do
    -- player HP regeneration, TODO: remove hardcoded max
    let nphp = if time `mod` 150 == 0 then (php + 1) `min` playerHP else php
    -- update smap
    let nsmap = M.insert ploc (time + smellTimeout) smap
    -- generate new monsters
    nlvl <- rndToIO (addMonster lvl player)
    -- determine player perception
    let per = perception ploc lmap
    -- perform monster moves
    handleMonsters session (nlvl { lsmell = smap }) state per oldmsg

-- | Handle monster moves. The idea is that we perform moves
--   as long as there are monsters that have a move time which is
--   less than or equal to the current time.
handleMonsters :: Session -> Level -> State -> Perception -> String -> IO ()
handleMonsters session (lvl@(Level nm sz ms nsmap lmap lmeta))
               (state@(State { splayer = player@(Monster { mloc = ploc }), stime = time }))
               per oldmsg =
    case ms of
      [] -> -- there are no monsters, just continue
            handle session lvl nstate per oldmsg
      (m@(Monster { mtime = mt }) : ms)
         | mt > time  -> -- all the monsters are not yet ready for another move
                            handle session lvl nstate per oldmsg
         | mhp m <= 0 -> -- the monster dies
                            handleMonsters session (updateMonsters lvl (const ms))
                                           state per oldmsg
         | otherwise  -> -- monster m should move
             do
               -- candidate directions: noses usually move randomly, whereas
               -- eyes favour to keep their old direction
               let ns | mtype m == Nose = moves
                      | otherwise       =
                          maybe id
                                (\ d -> L.filter (\ x -> distance (neg d,x) > 1)) 
                                (mdir m) moves
               -- those candidate directions that lead to accessible fields
               let fns = L.filter (\ x -> accessible lmap (mloc m) (mloc m `shift` x)) ns
               -- smells of the accessible fields
               let smells = zip fns
                                (L.map (\ x -> (nsmap ! (mloc m `shift` x) - time) `max` 0) fns)
               -- direction and value of maximum smell
               let msmell = maximumBy (\ (_,s1) (_,s2) -> compare s1 s2) smells
               nl <- if adjacent ploc (mloc m) then
                       -- attack player
                       return (ploc `shift` neg (mloc m))
                     else if mtype m == Eye && not (L.null fns) then
                       rndToIO (oneOf fns)
                     else if mtype m == Nose && not (L.null smells) && snd msmell > 0 then
                       return (fst msmell)
                     else if not (L.null fns) then
                       rndToIO (oneOf fns)
                     else -- fallback: wait
                          return (0,0)
               -- increase the monster move time
               let nm = m { mtime = time + 1 }
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
  where
    nstate = state { stime = time + 1 }

insertMonster :: Monster -> [Monster] -> (Int, [Monster])
insertMonster = insertMonster' 0
  where
    insertMonster' n m []      = (n, [m])
    insertMonster' n m (m':ms)
      | mtime m <= mtime m'    = (n, m : m' : ms)
      | otherwise              = let (n', ms') = insertMonster' (n + 1) m ms
                                 in  (n', m' : ms')

addMsg :: String -> String -> String
addMsg [] x  = x
addMsg xs [] = xs
addMsg xs x  = xs ++ " " ++ x

-- | Display current status and handle the turn of the player.
handle :: Session -> Level -> State -> Perception -> String -> IO ()
handle session (lvl@(Level nm sz ms smap lmap lmeta))
               (state@(State { splayer = player@(Monster _ php pdir ploc pinv ptime), stime = time }))
               per oldmsg =
  do
    -- check for player death
    if php <= 0
      then do
             displayCurrent (addMsg oldmsg ("You die ..." ++ more))
             getConfirm session
             shutdown session
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

                           "less"    -> lvlchange Up h
                           "greater" -> lvlchange Down h

                           ","       -> pickup h

                           -- saving or ending the game
                           "S"       -> encodeCompressedFile savefile (lvl,state,False) >>
                                        shutdown session
                           "Q"       -> shutdown session
                           "Escape"  -> shutdown session

                           -- wait
                           "space"   -> loop session nlvl state ""
                           "period"  -> loop session nlvl state ""

                           -- look
                           "colon"   -> displayCurrent (lookAt nlmap ploc) >> h

                           "V"       -> handle session nlvl (toggleVision state) per oldmsg
                           "R"       -> handle session nlvl (toggleSmell state) per oldmsg
                           "O"       -> handle session nlvl (toggleOmniscient state) per oldmsg
                           "T"       -> handle session nlvl (toggleTerrain state) per oldmsg

                           "M"       -> displayCurrent lmeta >> h
                           "v"       -> displayCurrent version >> h

                           s   -> displayCurrent ("unknown command (" ++ s ++ ")") >> h
             maybe h continueRun pdir

 where

  reachable = preachable per
  visible   = pvisible per

  displayCurrent = displayLevel session nlvl per state

  -- update player memory
  nlmap = foldr (\ x m -> M.update (\ (t,_) -> Just (t,flat t)) x m) lmap (S.toList visible)
  nlvl = updateLMap lvl (const nlmap)

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
                        plmap = M.insert ploc (nt, flat nt) nlmap
                        nplayer = player { mitems = i : mitems player }
                    in  loop session (updateLMap lvl (const plmap))
                                     (updatePlayer state (const nplayer)) msg

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
        Tile (Door hv o') is
                   | o /= not o' -> displayCurrent ("already " ++ txt) >> abort
                   | not (unoccupied ms nlmap dloc)
                                 -> displayCurrent "blocked" >> abort
                   | otherwise   -> -- ok, we can open/close the door      
                                    let nt = Tile (Door hv o) is
                                        clmap = M.insert (shift ploc dir) (nt, flat nt) nlmap
                                    in loop session (updateLMap lvl (const clmap)) state ""
        _ -> displayCurrent "never mind" >> abort
  -- perform a level change
  lvlchange vdir abort =
    case nlmap `at` ploc of
      Tile (Stairs vdir' next) is
       | vdir == vdir' -> -- ok
          case next of
            Nothing -> -- exit dungeon
                       shutdown session
            Just (nlvl, nloc) ->
              -- perform level change
              do
                let next' = Just (updateLMap lvl (const $ M.update (\ (_,r) -> Just (Tile (Stairs vdir Nothing) is,r)) ploc nlmap), ploc)
                let new = updateLMap nlvl (M.update (\ (Tile (Stairs d _) is,r) -> Just (Tile (Stairs d next') is,r)) nloc)
                loop session new (updatePlayer state (const (player { mloc = nloc }))) ""
                
      _ -> -- no stairs
           let txt = if vdir == Up then "up" else "down" in
           displayCurrent ("no stairs " ++ txt) >> abort
  -- run into a direction
  run dir =
    do
      let nplayer = player { mdir = Just dir }
          abort   = handle session nlvl state per ""
      moveOrAttack
        False   -- attacks are disallowed while running
        (\ l p -> loop session l (updatePlayer state (const p)))
        abort
        nlvl nplayer per APlayer dir
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
                  (\ l p -> loop session l (updatePlayer state (const p)))
                  abort
                  nlvl player per APlayer dir
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
                     (\ l p -> loop session l (updatePlayer state (const p)))
                     abort
                     nlvl player per APlayer dir

-- | Configurable event handler for the direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as open/close.
handleDirection :: String -> ((Y,X) -> IO ()) -> IO () -> IO ()
handleDirection e h k =
  case e of
    "k" -> h (-1,0)
    "j" -> h (1,0)
    "h" -> h (0,-1)
    "l" -> h (0,1)
    "y" -> h (-1,-1)
    "u" -> h (-1,1)
    "b" -> h (1,-1)
    "n" -> h (1,1)
    _   -> k

-- | Handler that ignores modifier events as they are
--   currently produced by the Gtk frontend.
handleModifier :: String -> IO () -> IO () -> IO ()
handleModifier e h k =
  case e of
    "Shift_R"   -> h
    "Shift_L"   -> h
    "Control_L" -> h
    "Control_R" -> h
    "Super_L"   -> h
    "Super_R"   -> h
    "Menu"      -> h
    "Alt_L"     -> h
    "Alt_R"     -> h
    _           -> k




version :: String
version = showVersion Self.version ++ " (" ++ displayId ++ " frontend)"

data Actor = AMonster Int  -- offset in monster list
           | APlayer
  deriving (Show, Eq)

getActor :: Level -> Player -> Actor -> Monster
getActor lvl p (AMonster n) = lmonsters lvl !! n
getActor lvl p APlayer      = p

updateActor :: (Monster -> Monster) ->                  -- the update
               (Monster -> Level -> Player -> IO a) ->  -- continuation
               Actor ->                                 -- who to update
               Level -> Player -> IO a                  -- transformed continuation
updateActor f k (AMonster n) lvl p = 
  let (m,ms) = updateMonster f n (lmonsters lvl)
  in  k m (updateMonsters lvl (const ms)) p
updateActor f k APlayer      lvl p = k p lvl (f p)

updateMonster :: (Monster -> Monster) -> Int -> [Monster] -> (Monster, [Monster])
updateMonster f n ms =
  case splitAt n ms of
    (pre, x : post) -> let m = f x in (m, pre ++ [m] ++ post)
    xs              -> error "updateMonster"


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
                                              then lookAt nlmap naloc else ""))
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

displayLevel session (lvl@(Level nm sz ms smap nlmap lmeta))
                     per
                     (state@(State { splayer = player@(Monster _ php pdir ploc _ _), stime = time }))
                     msg =
    let
      reachable = preachable per
      visible   = pvisible per
      sSml    = ssensory state == Smell
      sVis    = ssensory state == Vision
      sOmn    = sdisplay state == Omniscient
      sTer    = case sdisplay state of Terrain n -> n; _ -> 0
      lAt     = if sOmn || sTer > 0 then at else rememberAt
      lVision = if sVis
                  then \ vis rea ->
                       if      vis then setBG blue
                       else if rea then setBG magenta
                                   else id
                  else \ vis rea -> id
    in
      display ((0,0),sz) session 
               (\ loc -> let tile = nlmap `lAt` loc
                             sml  = ((smap ! loc) - time) `div` 10
                             vis  = S.member loc visible
                             rea  = S.member loc reachable
                             (rv,ra) = case L.find (\ m -> loc == mloc m) (player:ms) of
                                         _ | sTer > 0          -> viewTerrain sTer (tterrain tile)
                                         Just m | sOmn || vis  -> viewMonster (mtype m) 
                                         _ | sSml && sml >= 0  -> viewSmell sml
                                           | otherwise         -> viewTile tile
                             vision = lVision vis rea
                         in
                           (ra . vision $
                            attr, rv))
              msg
              (take 40 (nm ++ repeat ' ') ++ take 10 ("HP: " ++ show php ++ repeat ' ') ++
               take 10 ("T: " ++ show time ++ repeat ' '))

data Perception =
  Perception { preachable :: Set Loc, pvisible :: Set Loc }

perception_ :: State -> Level -> Perception
perception_ (State { splayer = Monster { mloc = ploc } }) (Level { lmap = lmap }) =
  perception ploc lmap

perception :: Loc -> LMap -> Perception
perception ploc lmap =
  let
    reachable  = fullscan ploc lmap
    actVisible = S.filter (\ loc -> light (lmap `at` loc)) reachable
    pasVisible = S.filter (\ loc -> let (x,p) = passive (lmap `at` loc)
                                    in  any (\ d -> S.member (shift loc d) actVisible) p ||
                                        (not x && adjacent loc ploc))
                                    -- the above "not x" prevents walls from
                                    -- being visible from the outside when
                                    -- adjacent
                          reachable
    visible = S.union pasVisible actVisible
  in
    Perception reachable visible
