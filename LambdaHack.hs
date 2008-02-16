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

savefile = "HHack2.save"

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
        Left (lvl,state) -> handle session lvl state "Welcome back to LambdaHack."

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

generate session msg =
  do
    -- generate dungeon with 10 levels
    levels <- mapM (\n -> level defaultLevelConfig $ "The Lambda Cave " ++ show n) [1..10]
    let state = defaultState ((\ (_,x,_) -> x) (head levels))
    let connect [(x,_,_)] = [x Nothing Nothing]
        connect ((x,_,_):ys@((_,u,_):_)) =
                            let (z:zs) = connect ys
                            in  x Nothing (Just (z,u)) : z : zs
    let lvl = head (connect levels)
    handle session lvl state msg

-- perform a complete move (i.e., monster moves etc.)
loop :: Session -> Level -> State -> String -> IO ()
loop session (lvl@(Level nm sz ms smap lmap lmeta))
             (state@(State { splayer = player@(Monster _ php _ ploc), stime = time }))
             oldmsg =
  do
    -- player HP regeneration, TODO: remove hardcoded max
    let nphp = if time `mod` 150 == 0 then (php + 1) `min` 20 else php
    -- update smap
    let nsmap = M.insert ploc (time + smellTimeout) smap
    -- generate new monsters
    rc <- randomRIO (1,if L.null ms then 5 else 70)
    gms <- if rc == (1 :: Int)
           then do
                  -- TODO: new monsters shouldn't be visible by the player
                  sm <- findLoc lvl (\ l t -> tterrain t == Floor && 
                                              not (l `L.elem` L.map mloc (player : ms)) &&
                                              distance (ploc, l) > 400)
                  rh <- fmap (+1) (randomRIO (1,2))
                  rt <- fmap (\ x -> if x == (1 :: Int) then Nose else Eye) (randomRIO (1,4))
                  let m = Monster rt rh Nothing sm
                  return (m : ms)
           else return ms
    -- perform monster moves
    let monsterMoves ams cplayer cmsg []      = return (ams, cplayer, cmsg)
        monsterMoves ams cplayer cmsg (m:oms) =
                         do
                           let ns | mtype m == Nose = moves
                                  | otherwise       =
                                      maybe id
                                            (\ d -> L.filter (\ x -> distance (neg d,x) > 1)) 
                                            (mdir m) moves
                           let fns = L.filter (\ x -> accessible lmap (mloc m) (mloc m `shift` x)) ns
                           let smells = zip fns
                                            (L.map (\ x -> (nsmap ! (mloc m `shift` x) - time) `max` 0) fns)
                           let msmell = maximumBy
                                          (\ (_,s1) (_,s2) -> compare s1 s2) smells
                           nl <- if adjacent ploc (mloc m) then
                                   -- attack player
                                   return (ploc `shift` neg (mloc m))
                                 else if mtype m == Eye && not (L.null fns) then
                                   do
                                     i <- randomRIO (0, L.length fns - 1)
                                     return (fns !! i)
                                 else if mtype m == Nose && not (L.null smells) && 
                                         snd msmell > 0 then
                                   do
                                     return (fst msmell)
                                 else
                                     liftM2 (,) (randomRIO (-1,1)) (randomRIO (-1,1))
                           -- TODO: now the hack that allows the player move
                           -- function to be used for monsters
                           moveOrAttack
                             True
                             (\ nlvl@(Level { lmonsters = nms }) nm msg ->
                               do
                                 let (p,r) = L.partition (\ m -> mtype m == Player) nms
                                 let h = case p of
                                           [ Monster { mhp = h } ] -> h
                                           _ -> 0  -- player killed
                                 -- TODO: we forget monster damage
                                 monsterMoves (nm { mdir = Just nl } : ams)
                                              (cplayer { mhp = h })
                                              (addMsg cmsg msg) oms
                             ) -- success
                             (lvl { lmonsters = cplayer : (ams ++ oms) })
                             (monsterMoves (m { mdir = Nothing } : ams) cplayer 
                                           cmsg -- (addMsg cmsg (show (nl,fns)))
                                           oms) -- abort 
                             m nl
    (fms, fplayer, fmsg) <- monsterMoves [] (player { mhp = nphp }) oldmsg gms
    handle session (lvl { lmonsters = fms, lsmell = nsmap })
           (state { splayer = fplayer, stime = time + 1 }) fmsg

addMsg [] x  = x
addMsg xs [] = xs
addMsg xs x  = xs ++ " " ++ x

-- display and handle the player
handle :: Session -> Level -> State -> String -> IO ()
handle session (lvl@(Level nm sz ms smap lmap lmeta))
               (state@(State { splayer = player@(Monster _ php pdir ploc), stime = time }))
               oldmsg =
  do
    -- check for player death
    if php <= 0
      then do
             displayCurrent (addMsg oldmsg "You die ...")
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

                           "V"       -> handle session nlvl (toggleVision state) oldmsg
                           "R"       -> handle session nlvl (toggleSmell state) oldmsg
                           "O"       -> handle session nlvl (toggleOmniscient state) oldmsg
                           "T"       -> handle session nlvl (toggleTerrain state) oldmsg

                           "M"       -> displayCurrent lmeta >> h
                           "v"       -> displayCurrent version >> h

                           s   -> displayCurrent ("unknown command (" ++ s ++ ")") >> h
             maybe h continueRun pdir

 where

  displayCurrent msg =
    let
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

  -- determine visible fields
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
  -- update player memory
  nlmap = foldr (\ x m -> M.update (\ (t,_) -> Just (t,flat t)) x m) lmap (S.toList visible)
  nlvl = updateLMap lvl (const nlmap)
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
  run dir = handle session nlvl (updatePlayer state (const $ player { mdir = Just dir })) ""
  continueRun dir =
    let abort = handle session nlvl (updatePlayer state (const $ player { mdir = Nothing })) oldmsg
    in  moveOrAttack False
                     (\ l m -> loop session l (updatePlayer state (const m)))
                     nlvl abort player dir
  -- perform a player move
  move abort dir = moveOrAttack True
                                (\ l m -> loop session l (updatePlayer state (const m)))
                                nlvl abort player dir

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

moveOrAttack :: Bool ->                                     -- allow attacks?
                (Level -> Monster -> String -> IO a) ->     -- success continuation
                Level ->
                IO a ->                                     -- failure continuation
                Monster -> Dir -> IO a
moveOrAttack allowAttacks
             continue nlvl@(Level { lmap = nlmap }) abort player@(Monster { mloc = ploc }) dir
      -- to prevent monsters from hitting themselves
    | dir == (0,0) = abort
      -- at the moment, we check whether there is a monster before checking open-ness
      -- i.e., we could attack a monster on a blocked location
    | not (L.null attacked) =
        let
          victim   = head attacked
          saveds   = tail attacked ++ others
          survivor = case mhp victim of
                       1  ->  []
                       h  ->  [victim { mhp = h - 1 }]
          verb | L.null survivor = "kill"
               | otherwise       = "hit"
        in
          if (mtype victim == Player) || (mtype player == Player) && allowAttacks then
            continue (nlvl { lmonsters = survivor ++ saveds }) player
                     (subjectMonster (mtype player) ++ " " ++
                      verbMonster (mtype player) verb ++ " " ++
                      objectMonster (mtype victim) ++ ".")
          else
            abort  -- currently, we prevent monster from attacking each other

    | accessible nlmap ploc nploc = continue nlvl (player { mloc = nploc }) 
                                             (if mtype player == Player
                                              then lookAt nlmap nploc else "") 
    | otherwise = abort
    where source = nlmap `at` ploc
          nploc  = shift ploc dir
          target = nlmap `at` nploc
          (attacked, others) = L.partition (\ m -> mloc m == nploc) (lmonsters nlvl)

-- | Produces a textual description of the items at a location. It's
-- probably correct to use 'at' rather than 'rememberAt' at this point,
-- although we could argue that 'rememberAt' reflects what the player can
-- perceive more correctly ...
lookAt :: LMap -> Loc -> String
lookAt lvl loc = unwords $ L.map objectItem $ titems (lvl `at` loc)

viewTile :: Tile -> (Char, Attr -> Attr)
viewTile (Tile t [])    = viewTerrain 0 t
viewTile (Tile t (i:_)) = viewItem i

viewItem :: Item -> (Char, Attr -> Attr)
viewItem Ring   = ('=', id)
viewItem Scroll = ('?', id)
viewItem Potion = ('!', id)
viewItem Wand   = ('/', id)
viewItem _      = ('~', id)

-- | The parameter "n" is the level of evolution:
--
-- 0: final
-- 1: stairs added
-- 2: doors added
-- 3: corridors and openings added
-- 4: only rooms
viewTerrain :: Int -> Terrain -> (Char, Attr -> Attr)
viewTerrain n Rock              = (' ', id)
viewTerrain n (Opening d)
  | n <= 3                      = ('.', id)
  | otherwise                   = viewTerrain 0 (Wall d)
viewTerrain n Floor             = ('.', id)
viewTerrain n Unknown           = (' ', id)
viewTerrain n Corridor
  | n <= 3                      = ('#', id)
  | otherwise                   = viewTerrain 0 Rock
viewTerrain n (Wall Horiz)      = ('-', id)
viewTerrain n (Wall Vert)       = ('|', id)
viewTerrain n (Stairs Up _)
  | n <= 1                      = ('<', id)
  | otherwise                   = viewTerrain 0 Floor
viewTerrain n (Stairs Down _)
  | n <= 1                      = ('>', id)
  | otherwise                   = viewTerrain 0 Floor
viewTerrain n (Door d False)
  | n <= 2                      = ('+', setFG yellow)
  | otherwise                   = viewTerrain n (Opening d)
viewTerrain n (Door Horiz True)
  | n <= 2                      = ('|', setFG yellow)
  | otherwise                   = viewTerrain n (Opening Horiz)
viewTerrain n (Door Vert True)
  | n <= 2                      = ('-', setFG yellow)
  | otherwise                   = viewTerrain n (Opening Vert)

viewMonster :: MonsterType -> (Char, Attr -> Attr)
viewMonster Player = ('@', setBG white . setFG black)
viewMonster Eye    = ('e', setFG red)
viewMonster Nose   = ('n', setFG green)

viewSmell :: Int -> (Char, Attr -> Attr)
viewSmell n = let k | n > 9    = '*'
                    | n < 0    = '-'
                    | otherwise = head . show $ n
              in  (k, setFG black . setBG green)
