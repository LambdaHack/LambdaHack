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
                               (x,y,z) -> (z :: Bool) `seq` return $ Just (x,y))
                          (const $ return Nothing)
      case restored of
        Nothing           -> generate session
        Just (lvl,player) -> loop session lvl player "Restored successfully."

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
    loop session lvl player ""

-- perform a complete move (i.e., monster moves etc.)
loop :: Session -> Level -> Monster -> String -> IO ()
loop session (lvl@(Level nm sz ms lmap)) player@(Monster _ php ploc) oldmsg =
  do
    handle session lvl player oldmsg

-- display and handle event
handle :: Session -> Level -> Monster -> String -> IO ()
handle session (lvl@(Level nm sz ms lmap)) player@(Monster _ php ploc) oldmsg =
  do
    displayCurrent oldmsg
    let h = do
              e <- nextEvent session
              handleDirection e (move h) $ 
                case e of
                  "o" -> openclose True h
                  "c" -> openclose False h

                  "less"    -> lvlchange Up h
                  "greater" -> lvlchange Down h

                  "S"       -> encodeCompressedFile savefile (lvl,player,False) >> shutdown session
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

                  s   -> displayCurrent ("unknown command (" ++ s ++ ")") >> h
    h

 where
  displayCurrent msg =
    display ((0,0),sz) session 
             (\ loc -> let tile = nlmap `rememberAt` loc
                           vis  = S.member loc visible
                           rea  = S.member loc reachable
                           (rv,ra) = case L.find (\ m -> loc == mloc m) (player:ms) of
                                       Just m | vis -> viewMonster (mtype m) 
                                       _            -> viewTile tile
                       in
                         ((if      vis then setBG blue
                           else if rea then setBG magenta
                                       else id) . ra $
                          attr, rv))
            msg
            (take 40 (nm ++ repeat ' ') ++ "HP: " ++ show php)

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
                                  in loop session (updateLMap lvl (const clmap)) player ""
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
                loop session new (player { mloc = nloc }) ""
                
      _ -> -- no stairs
           let txt = if vdir == Up then "up" else "down" in
           displayCurrent ("no stairs " ++ txt) >> abort
  -- perform a player move
  move abort dir = moveOrAttack (loop session) nlvl abort player dir

moveOrAttack :: (Level -> Monster -> String -> IO ()) ->    -- success continuation
                Level ->
                IO () ->                                    -- failure continuation
                Monster -> Loc -> IO ()
moveOrAttack continue nlvl@(Level { lmap = nlmap }) abort player@(Monster { mloc = ploc}) dir
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
          continue (nlvl { lmonsters = survivor ++ saveds }) player
                   (subjectMonster (mtype player) ++ " " ++
                    verbMonster (mtype player) verb ++ " " ++
                    objectMonster (mtype victim) ++ ".")

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
viewMonster Player = ('@', id)
viewMonster Eye    = ('e', setFG red)


