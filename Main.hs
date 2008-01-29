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

display :: Vty -> (Int -> Int -> (Attr, Char)) -> IO ()
display vty f =
    let img = (foldr (<->) V.empty . 
               L.map (foldr (<|>) V.empty . 
                      L.map (\ (x,y) -> let (a,c) = f x y in renderChar a c)))
              [ [ (x,y) | x <- [0..levelX] ] | y <- [0..levelY] ]
    in  V.update vty (Pic NoCursor img)

loop vty i =
  do
    l <- level
    display vty (\ x y -> (attr, head . show $ findWithDefault Unknown (y,x) l))
    -- display vty (\ x y -> (setFG (if (x + y) `mod` 2 == 1 then red else green) attr, head . show $ (x + y + i) `mod` 7))
    e <- V.getEvent vty
    case e of
      V.EvKey KEsc _ -> shutdown vty
      _              -> loop vty (i+1)



