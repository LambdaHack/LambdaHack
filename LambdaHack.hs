module Main where

import System.Directory
import System.IO
import Data.Binary
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.Random
import Control.Monad
import Control.Exception as E hiding (handle)
import Codec.Compression.Zlib as Z

import Geometry
import Level
import Dungeon
import Monster
import FOV
#ifdef GTK
import Display.Gtk
#else
import Display.Vty
#endif

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
      restored <- E.catch (do
                             r <- strictDecodeCompressedFile savefile
                             removeFile savefile
                             case r of
                               (x,y,t,z) -> (z :: Bool) `seq` return $ Just (x,y,t))
                          (const $ return Nothing)
      case restored of
        Nothing                -> generate session
        Just (lvl,player,time) -> handle session lvl player time "Restored successfully."

generate session =
  do
    -- generate dungeon with 10 levels
    levels <- mapM (\n -> level defaultLevelConfig $ "The Lambda Cave " ++ show n) [1..10]
    let player = Monster Player 10 ((\ (_,x,_) -> x) (head levels))
    let connect [(x,_,_)] = [x Nothing Nothing]
        connect ((x,_,_):ys@((_,u,_):_)) =
                            let (z:zs) = connect ys
                            in  x Nothing (Just (z,u)) : z : zs
    let lvl = head (connect levels)
    handle session lvl player 0 ""

type Time = Int

-- perform a complete move (i.e., monster moves etc.)
loop :: Session -> Level -> Monster -> Time -> String -> IO ()
loop session (lvl@(Level nm sz ms lmap)) player@(Monster _ php ploc) time oldmsg =
  do
    -- generate new monsters
    rc <- randomRIO (1,20)
    gms <- if rc == (1 :: Int)
           then do
                  -- TODO: new monsters shouldn't be visible by the player
                  sm <- findLoc lvl (\ l t -> t == Floor && 
                                              not (l `L.elem` L.map mloc (player : ms)))
                  rh <- fmap (+2) (randomRIO (1,3))
                  let m = Monster Eye rh sm
                  return (m : ms)
           else return ms
    -- perform monster moves
    let monsterMoves ams cplayer cmsg []      = return (ams, cplayer, cmsg)
        monsterMoves ams cplayer cmsg (m:oms) =
                         do
                           ry <- randomRIO (-1,1)
                           rx <- randomRIO (-1,1)
                           -- TODO: now the hack that allows the player move
                           -- function to be used for monsters
                           moveOrAttack 
                             (\ nlvl@(Level { lmonsters = nms }) nm msg ->
                               do
                                 let (p,r) = L.partition (\ m -> mtype m == Player) nms
                                 let h = case p of
                                           [ Monster { mhp = h } ] -> h
                                           _ -> 0  -- player killed
                                 -- TODO: we forget monster damage
                                 monsterMoves (nm : ams) (cplayer { mhp = h })
                                              (addMsg cmsg msg) oms
                             ) -- success
                             (lvl { lmonsters = cplayer : (ams ++ oms) })
                             (monsterMoves (m : ams) cplayer cmsg oms) -- abort 
                             m (ry,rx)
    (fms, fplayer, fmsg) <- monsterMoves [] player oldmsg gms
    handle session (lvl { lmonsters = fms }) fplayer time fmsg

addMsg [] x  = x
addMsg xs [] = xs
addMsg xs x  = xs ++ " " ++ x

-- display and handle the player
handle :: Session -> Level -> Monster -> Time -> String -> IO ()
handle session (lvl@(Level nm sz ms lmap)) player@(Monster _ php ploc) time oldmsg =
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
                         case e of
                           "o" -> openclose True h
                           "c" -> openclose False h

                           "less"    -> lvlchange Up h
                           "greater" -> lvlchange Down h

                           "S"       -> encodeCompressedFile savefile (lvl,player,time,False) >>
                                        shutdown session
                           "Q"       -> shutdown session
                           "Escape"  -> shutdown session

                           "Shift_R" -> h
                           "Shift_L" -> h
                           "Control_L" -> h
                           "Control_R" -> h
                           "Super_L" -> h
                           "Super_R" -> h
                           "Menu"    -> h
                           "Alt_L"   -> h
                           "Alt_R"   -> h

                           "period"  -> loop session nlvl player ntime ""

                           s   -> displayCurrent ("unknown command (" ++ s ++ ")") >> h
             h

 where
  ntime = time + 1

  displayCurrent msg =
    display ((0,0),sz) session 
             (\ loc -> let tile = nlmap `rememberAt` loc
                           vis  = S.member loc visible
                           rea  = S.member loc reachable
                           (rv,ra) = case L.find (\ m -> loc == mloc m) (player:ms) of
                                       Just m | vis -> viewMonster (mtype m) 
                                       _            -> viewTile tile
                       in
                         (({- if      vis then setBG blue
                           else if rea then setBG magenta
                                       else -} id) . ra $
                          attr, rv))
            msg
            (take 40 (nm ++ repeat ' ') ++ take 10 ("HP: " ++ show php ++ repeat ' ') ++
             take 10 ("T: " ++ show time ++ repeat ' '))

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

  -- determine visible fields
  reachable = fullscan ploc lmap
  visible = S.filter (\ loc -> light (lmap `at` loc) || adjacent loc ploc) reachable
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
    let txt = if o then "open" else "closed" in
    case (nlmap `at` shift ploc dir) of
      Door hv o' | o == not o' -> -- ok, we can open/close the door      
                                  let nt = Door hv o
                                      clmap = M.insert (shift ploc dir) (nt, flat nt) nlmap
                                  in loop session (updateLMap lvl (const clmap)) player ntime ""
                 | otherwise   -> displayCurrent ("already " ++ txt) >> abort
      _ -> displayCurrent "never mind" >> abort
  -- perform a level change
  lvlchange vdir abort =
    case nlmap `at` ploc of
      Stairs vdir' next
       | vdir == vdir' -> -- ok
          case next of
            Nothing -> -- exit dungeon
                       shutdown session
            Just (nlvl, nloc) ->
              -- perform level change
              do
                let next' = Just (updateLMap lvl (const $ M.update (\ (_,r) -> Just (Stairs vdir Nothing,r)) ploc nlmap), ploc)
                let new = updateLMap nlvl (M.update (\ (Stairs d _,r) -> Just (Stairs d next',r)) nloc)
                loop session new (player { mloc = nloc }) ntime ""
                
      _ -> -- no stairs
           let txt = if vdir == Up then "up" else "down" in
           displayCurrent ("no stairs " ++ txt) >> abort
  -- perform a player move
  move abort dir = moveOrAttack (\ l m -> loop session l m ntime) nlvl abort player dir

moveOrAttack :: (Level -> Monster -> String -> IO a) ->     -- success continuation
                Level ->
                IO a ->                                     -- failure continuation
                Monster -> Loc -> IO a
moveOrAttack continue nlvl@(Level { lmap = nlmap }) abort player@(Monster { mloc = ploc}) dir
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
          if (mtype victim == Player) || (mtype player == Player) then
            continue (nlvl { lmonsters = survivor ++ saveds }) player
                     (subjectMonster (mtype player) ++ " " ++
                      verbMonster (mtype player) verb ++ " " ++
                      objectMonster (mtype victim) ++ ".")
          else
            abort  -- currently, we prevent monster from attacking each other

    | open target =
        case (source,target) of
          (Door _ _,_) | diagonal dir -> abort -- doors aren't accessible diagonally
          (_,Door _ _) | diagonal dir -> abort -- doors aren't accessible diagonally
          _ -> -- ok
               continue nlvl (player { mloc = nploc }) ""
    | otherwise = abort
    where source = nlmap `at` ploc
          nploc  = shift ploc dir
          target = nlmap `at` nploc
          (attacked, others) = L.partition (\ m -> mloc m == nploc) (lmonsters nlvl)

viewTile :: Tile -> (Char, Attr -> Attr)
viewTile Rock              = (' ', id)
viewTile (Opening _)       = ('.', id)
viewTile Floor             = ('.', id)
viewTile Unknown           = (' ', id)
viewTile Corridor          = ('#', id)
viewTile (Wall Horiz)      = ('-', id)
viewTile (Wall Vert)       = ('|', id)
viewTile (Stairs Up _)     = ('<', id)
viewTile (Stairs Down _)   = ('>', id)
viewTile (Door _ False)    = ('+', setFG yellow)
viewTile (Door Horiz True) = ('|', setFG yellow)
viewTile (Door Vert True)  = ('-', setFG yellow)

viewMonster :: MonsterType -> (Char, Attr -> Attr)
viewMonster Player = ('@', setBG white . setFG black)
viewMonster Eye    = ('e', setFG red)


