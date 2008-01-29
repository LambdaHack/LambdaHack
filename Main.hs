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
    loop vty 0

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

loop vty i =
  do
    -- generate random level
    lvl@(Level sz lmap) <- level
    -- generate player position
    player <- findLoc lvl open
    -- determine visible fields
    let visible = fullscan player lmap
    display ((0,0),sz) vty 
             (\ loc -> let tile = findWithDefault Unknown loc lmap
                       in
                       ((if S.member loc visible then
                           if light tile || adjacent loc player then setBG blue
                                                                else setBG magenta
                         else id) attr,
                        if loc == player then '@'
                        else head . show $ findWithDefault Unknown loc lmap))
    e <- V.getEvent vty
    case e of
      V.EvKey KEsc _ -> shutdown vty
      _              -> loop vty (i+1)



