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

import Level
import Dungeon
import FOV
import Display.Vty

savefile = "HHack2.save"

main :: IO ()
main = startup start

-- should at the moment match the level size
screenX = levelX
screenY = levelY


strictReadCompressedFile :: FilePath -> IO LBS.ByteString
strictReadCompressedFile f =
    do
      h <- openFile f ReadMode
      c <- LBS.hGetContents h
      let d = Z.decompress c
      LBS.length d `seq` return d

strictDecodeCompressedFile :: Binary a => FilePath -> IO a
strictDecodeCompressedFile f = fmap decode (strictReadCompressedFile f)

encodeCompressedFile :: Binary a => FilePath -> a -> IO ()
encodeCompressedFile f x = LBS.writeFile f (Z.compress (encode x))

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
    levels <- mapM (\n -> level $ "The Lambda Cave " ++ show n) [1..10]
    let player = (\ (_,x,_) -> x) (head levels)
    let connect [(x,_,_)] = [x Nothing Nothing]
        connect ((x,_,_):ys@((_,u,_):_)) =
                            let (z:zs) = connect ys
                            in  x Nothing (Just (z,u)) : z : zs
    let lvl = head (connect levels)
    loop session lvl player

loop session (lvl@(Level nm sz lmap)) player =
  do
    display ((0,0),sz) session 
             (\ loc -> let tile = nlmap `rememberAt` loc
                       in
                       ((if S.member loc visible then setBG blue
                         else if S.member loc reachable then setBG magenta
                         else id) attr,
                        if loc == player then '@'
                        else view tile))
            nm
    e <- nextEvent session
    case e of
      "k" -> move (-1,0)
      "j" -> move (1,0)
      "h" -> move (0,-1)
      "l" -> move (0,1)
      "y" -> move (-1,-1)
      "u" -> move (-1,1)
      "b" -> move (1,-1)
      "n" -> move (1,1)

      "<" -> lvlchange Up
      ">" -> lvlchange Down

      "S" -> shutdown session >> encodeCompressedFile savefile (lvl,player,False)
      "Q" -> shutdown session
      "Escape" -> shutdown session

      _                       -> loop session nlvl player
 where
  -- determine visible fields
  reachable = fullscan player lmap
  visible = S.filter (\ loc -> light (lmap `at` loc) || adjacent loc player) reachable
  -- update player memory
  nlmap = foldr (\ x m -> M.update (\ (t,_) -> Just (t,flat t)) x m) lmap (S.toList visible)
  nlvl = Level nm sz nlmap
  -- perform a level change
  lvlchange vdir =
    case nlmap `at` player of
      Stairs vdir' next
       | vdir == vdir' -> -- ok
          case next of
            Nothing -> -- exit dungeon
                       shutdown session
            Just (nlvl@(Level nnm nsz nlmap), nloc) ->
              -- perform level change
              do
                let next' = Just (Level nm sz (M.update (\ (_,r) -> Just (Stairs vdir Nothing,r)) player lmap), player)
                let new = Level nnm nsz (M.update (\ (Stairs d _,r) -> Just (Stairs d next',r)) nloc nlmap)
                loop session new nloc
                
      _                -> -- no stairs
                                      loop session nlvl player
  -- perform a player move
  move dir
    | open (lmap `at` shift player dir) = loop session nlvl (shift player dir)
    | otherwise = loop session nlvl player


