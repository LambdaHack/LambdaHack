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
    (lvl0, su0, sd0) <- level
    (lvl1, su1, sd1) <- level
    let lvl0' = lvl0 Nothing (Just (lvl1', su1))
        lvl1' = lvl1 (Just (lvl0', sd0)) Nothing
        lvl = lvl0'
    let rmap = M.empty -- remembered tiles
    -- generate player position
    let player = su0
    loop vty lvl rmap player

loop vty (lvl@(Level sz lmap)) rmap player =
  do
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
      V.EvKey (KASCII 'k') [] -> move (-1,0)
      V.EvKey (KASCII 'j') [] -> move (1,0)
      V.EvKey (KASCII 'h') [] -> move (0,-1)
      V.EvKey (KASCII 'l') [] -> move (0,1)
      V.EvKey (KASCII 'y') [] -> move (-1,-1)
      V.EvKey (KASCII 'u') [] -> move (-1,1)
      V.EvKey (KASCII 'b') [] -> move (1,-1)
      V.EvKey (KASCII 'n') [] -> move (1,1)

      V.EvKey (KASCII '<') [] -> lvlchange Up
      V.EvKey (KASCII '>') [] -> lvlchange Down

      V.EvKey (KASCII 'Q') [] -> shutdown vty
      V.EvKey KEsc _          -> shutdown vty

      _                       -> loop vty lvl nrmap player
 where
  -- determine visible fields
  visible = fullscan player lmap
  -- update player memory
  nrmap = foldr (\ x m -> M.insert x (findWithDefault Unknown x lmap) m) rmap (S.toList visible)
  -- perform a level change
  lvlchange vdir =
    case findWithDefault Unknown player lmap of
      Stairs vdir' next
       | vdir == vdir' -> -- ok
          case next of
            Nothing -> -- exit dungeon
                       shutdown vty
            Just (nlvl@(Level nsz nlmap), nloc) ->
              -- perform level change
              do
                let next' = Just (Level sz (M.insert player (Stairs vdir Nothing) lmap), player)
                let new = Level nsz (M.update (\ (Stairs d _) -> Just (Stairs d next')) nloc nlmap)
                loop vty new M.empty nloc
                
      _                -> -- no stairs
                                      loop vty lvl nrmap player
  -- perform a player move
  move dir
    | open (findWithDefault Unknown (shift player dir) lmap) = loop vty lvl nrmap (shift player dir)
    | otherwise = loop vty lvl nrmap player


