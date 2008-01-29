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
    -- generate player position
    let player = su0
    loop vty lvl player

loop vty (lvl@(Level sz lmap)) player =
  do
    display ((0,0),sz) vty 
             (\ loc -> let tile = nlmap `rememberAt` loc
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

      _                       -> loop vty nlvl player
 where
  -- determine visible fields
  visible = fullscan player lmap
  -- update player memory
  nlmap = foldr (\ x m -> M.update (\ (t,_) -> Just (t,t)) x m) lmap (S.toList visible)
  nlvl = Level sz nlmap
  -- perform a level change
  lvlchange vdir =
    case nlmap `at` player of
      Stairs vdir' next
       | vdir == vdir' -> -- ok
          case next of
            Nothing -> -- exit dungeon
                       shutdown vty
            Just (nlvl@(Level nsz nlmap), nloc) ->
              -- perform level change
              do
                let next' = Just (Level sz (M.update (\ (_,r) -> Just (Stairs vdir Nothing,r)) player lmap), player)
                let new = Level nsz (M.update (\ (Stairs d _,r) -> Just (Stairs d next',r)) nloc nlmap)
                loop vty new nloc
                
      _                -> -- no stairs
                                      loop vty nlvl player
  -- perform a player move
  move dir
    | open (lmap `at` shift player dir) = loop vty nlvl (shift player dir)
    | otherwise = loop vty nlvl player


