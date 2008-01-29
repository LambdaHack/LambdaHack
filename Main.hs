module Main where

import Data.List as L
import Data.Map as M
import Data.Set as S
import Graphics.Vty as V
import System.Random
import Control.Monad

import Level
import Dungeon

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
    Level sz lmap <- level
    display ((0,0),sz) vty (\ loc -> (setBG blue attr, head . show $ findWithDefault Unknown loc lmap))
    -- display vty (\ x y -> (setFG (if (x + y) `mod` 2 == 1 then red else green) attr, head . show $ (x + y + i) `mod` 7))
    e <- V.getEvent vty
    case e of
      V.EvKey KEsc _ -> shutdown vty
      _              -> loop vty (i+1)



