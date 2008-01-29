module Main where

import Data.List as L
import Data.Map as M
import Data.Set as S
import Graphics.Vty as V
import System.Random
import Control.Monad

import Level
import Dungeon
import FOV

main :: IO ()
main =
  do
    vty <- V.mkVty
    Main.init vty

-- should at the moment match the level size
screenX = levelX
screenY = levelY

display :: Area -> Vty -> (Loc -> (Attr, Char)) -> IO ()
display ((y0,x0),(y1,x1)) vty f =
    let img = (foldr (<->) V.empty . 
               L.map (foldr (<|>) V.empty . 
                      L.map (\ (x,y) -> let (a,c) = f (y,x) in renderChar a c)))
              [ [ (x,y) | x <- [x0..x1] ] | y <- [y0..y1] ]
    in  V.update vty (Pic NoCursor img)

init vty =
  do
    -- generate random level
    lvl <- level
    let rmap = M.empty -- remembered tiles
    -- generate player position
    player <- findLoc lvl (const open)
    loop vty lvl rmap player

loop vty (lvl@(Level sz lmap)) rmap player =
  do
    -- determine visible fields
    let visible = fullscan player lmap
    -- update player memory
    let nrmap = foldr (\ x m -> M.insert x (findWithDefault Unknown x lmap) m) rmap (S.toList visible)
    display ((0,0),sz) vty 
             (\ loc -> let tile = findWithDefault Unknown loc nrmap
                       in
                       ((if S.member loc visible then
                           if light tile || adjacent loc player then setBG blue
                                                                else setBG magenta
                         else id) attr,
                        if loc == player then '@'
                        else head . show $ tile))
    e <- V.getEvent vty
    case e of
      V.EvKey (KASCII 'k') [] -> move nrmap (-1,0)
      V.EvKey (KASCII 'j') [] -> move nrmap (1,0)
      V.EvKey (KASCII 'h') [] -> move nrmap (0,-1)
      V.EvKey (KASCII 'l') [] -> move nrmap (0,1)
      V.EvKey (KASCII 'y') [] -> move nrmap (-1,-1)
      V.EvKey (KASCII 'u') [] -> move nrmap (-1,1)
      V.EvKey (KASCII 'b') [] -> move nrmap (1,-1)
      V.EvKey (KASCII 'n') [] -> move nrmap (1,1)

      V.EvKey (KASCII 'Q') [] -> shutdown vty
      V.EvKey KEsc _          -> shutdown vty

      _                       -> loop vty lvl nrmap player
 where
  move nrmap dir
    | open (findWithDefault Unknown (shift player dir) lmap) = loop vty lvl nrmap (shift player dir)
    | otherwise = loop vty lvl nrmap player


