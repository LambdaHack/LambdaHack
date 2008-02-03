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
import Control.Exception as E
import Codec.Compression.Zlib as Z

import Geometry
import Level
import Dungeon
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
        Just (lvl,player) -> loop session lvl player

generate session =
  do
    -- generate dungeon with 10 levels
    levels <- mapM (\n -> level defaultLevelConfig $ "The Lambda Cave " ++ show n) [1..10]
    let player = (\ (_,x,_) -> x) (head levels)
    let connect [(x,_,_)] = [x Nothing Nothing]
        connect ((x,_,_):ys@((_,u,_):_)) =
                            let (z:zs) = connect ys
                            in  x Nothing (Just (z,u)) : z : zs
    let lvl = head (connect levels)
    loop session lvl player

loop :: Session -> Level -> Loc -> IO ()
loop session (lvl@(Level nm sz ms lmap)) player =
  do
    displayCurrent "" 
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
             (\ loc -> let tile  = nlmap `rememberAt` loc
                           (v,a) = view tile
                       in
                       ((if S.member loc visible then setBG blue
                         else if S.member loc reachable then setBG magenta
                         else id) .
                        (if loc == player then id else a) $ attr,
                        if loc == player then '@'
                        else v))
            msg
            nm

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
  reachable = fullscan player lmap
  visible = S.filter (\ loc -> light (lmap `at` loc) || adjacent loc player) reachable
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
    case (nlmap `at` shift player dir) of
      Door hv o' | o == not o' -> -- ok, we can open/close the door      
                                  let nt = Door hv o
                                      clmap = M.insert (shift player dir) (nt, flat nt) nlmap
                                  in loop session (updateLMap lvl (const clmap)) player
                 | otherwise   -> displayCurrent ("already " ++ txt) >> abort
      _ -> displayCurrent "never mind" >> abort
  -- perform a level change
  lvlchange vdir abort =
    case nlmap `at` player of
      Stairs vdir' next
       | vdir == vdir' -> -- ok
          case next of
            Nothing -> -- exit dungeon
                       shutdown session
            Just (nlvl, nloc) ->
              -- perform level change
              do
                let next' = Just (updateLMap lvl (const $ M.update (\ (_,r) -> Just (Stairs vdir Nothing,r)) player nlmap), player)
                let new = updateLMap nlvl (M.update (\ (Stairs d _,r) -> Just (Stairs d next',r)) nloc)
                loop session new nloc
                
      _ -> -- no stairs
           let txt = if vdir == Up then "up" else "down" in
           displayCurrent ("no stairs " ++ txt) >> abort
  -- perform a player move
  move abort dir
    | open target =
        case (source,target) of
          (Door _ _,_) | diagonal dir -> abort -- doors aren't accessible diagonally
          (_,Door _ _) | diagonal dir -> abort -- doors aren't accessible diagonally
          _ -> -- ok
               loop session nlvl (shift player dir)
    | otherwise = abort
    where source = nlmap `at` player
          target = nlmap `at` shift player dir

-- view :: Tile -> (Char, Attr -> Attr)
view Rock              = (' ', id)
view (Opening _)       = ('.', id)
view Floor             = ('.', id)
view Unknown           = (' ', id)
view Corridor          = ('#', id)
view (Wall Horiz)      = ('-', id)
view (Wall Vert)       = ('|', id)
view (Stairs Up _)     = ('<', id)
view (Stairs Down _)   = ('>', id)
view (Door _ False)    = ('+', setFG yellow)
view (Door Horiz True) = ('|', setFG yellow)
view (Door Vert True)  = ('-', setFG yellow)


