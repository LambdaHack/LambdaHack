module Terrain where

import Control.Monad

import Data.Binary
import Data.Maybe

import qualified Color
import Geometry

-- TODO: let terrain kinds be defined in a config file. Group them
-- and assign frequency so that they can be used for dungeon building.
-- Goal: Have 2 tileset configs, one small, Rouge/Nethack style,
-- the other big, Angband/UFO style. The problem is that the Rogue walls
-- are very complex, while Angband style is much simpler, and I love KISS. Hmmm.

data Terrain a =
    Rock
  | Opening Pos
  | Floor DL
  | Unknown
  | Corridor
  | Wall Pos
  | Stairs DL VDir (Maybe a)
  | Door Pos (Maybe Int)  -- Nothing: open, Just 0: closed, otherwise secret
  deriving Show

instance Binary VDir where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8

instance Binary a => Binary (Terrain a) where
  put Rock            = putWord8 0
  put (Opening p)     = putWord8 1 >> put p
  put (Floor dl)      = putWord8 2 >> put dl
  put Unknown         = putWord8 3
  put Corridor        = putWord8 4
  put (Wall p)        = putWord8 5 >> put p
  put (Stairs dl d n) = putWord8 6 >> put dl >> put d >> put n
  put (Door p o)      = putWord8 7 >> put p >> put o
  get = do
          tag <- getWord8
          case tag of
            0 -> return Rock
            1 -> liftM Opening get
            2 -> liftM Floor get
            3 -> return Unknown
            4 -> return Corridor
            5 -> liftM Wall get
            6 -> liftM3 Stairs get get get
            7 -> liftM2 Door get get
            _ -> fail "no parse (Terrain)"

instance Eq a => Eq (Terrain a) where
  Rock == Rock = True
  Opening d == Opening d' = d == d'
  Floor l == Floor l' = l == l'
  Unknown == Unknown = True
  Corridor == Corridor = True
  Wall p == Wall p' = p == p'
  Stairs dl d t == Stairs dl' d' t' = dl == dl' && d == d' && t == t'
  Door p o == Door p' o' = p == p' && o == o'
  _ == _ = False

data DL = Dark | Light
  deriving (Eq, Show, Enum, Bounded)

instance Binary DL where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8

-- | All the wall kinds that are possible:
--
--     * 'UL': upper left
--
--     * 'U': upper
--
--     * 'UR': upper right
--
--     * 'L': left
--
--     * 'R': right
--
--     * 'DL': lower left
--
--     * 'D': lower
--
--     * 'DR': lower right
--
--     * 'O': lower right
--
-- I am tempted to add even more (T-pieces and crossings),
-- but currently, we don't need them.
data Pos = UL | U | UR | L | R | DL | D | DR | O
  deriving (Eq, Show, Enum, Bounded)

instance Binary Pos where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8

isFloor :: Terrain a -> Bool
isFloor (Floor _) = True
isFloor _         = False

isUnknown :: Terrain a -> Bool
isUnknown Unknown = True
isUnknown _       = False

-- | allows moves and vision
isOpen :: Terrain a -> Bool
isOpen (Floor {})    = True
isOpen (Opening {}) = True
isOpen (Door _ o)   = isNothing o
isOpen Corridor     = True
isOpen (Stairs {})  = True
isOpen _            = False

fromDL :: DL -> Bool
fromDL Dark = False
fromDL Light = True

toDL :: Bool -> DL
toDL False = Dark
toDL True  = Light

-- | is lighted on its own
isAlight :: Terrain a -> Bool
isAlight (Floor l)      = fromDL l
isAlight (Stairs l _ _) = fromDL l
isAlight _              = False

-- | can be lighted by sourrounding tiles
reflects :: Terrain a -> Bool
reflects (Opening _) = True
reflects (Wall _)    = True
reflects (Door _ _)  = True
reflects _           = False

-- | Maps wall kinds to lists of expected floor positions.
posToDir :: Pos -> [Dir]
posToDir UL = [downright]
posToDir U  = [down]
posToDir UR = [downleft]
posToDir L  = [right]
posToDir R  = [left]
posToDir DL = [upright]
posToDir D  = [up]
posToDir DR = [upleft]
posToDir O  = moves

-- | Passive tiles reflect light from some other (usually adjacent)
-- positions. This function returns the offsets from which light is
-- reflected. Not all passively lighted tiles reflect from all directions.
-- Walls, for instance, cannot usually be seen from the outside.
passive :: Terrain a -> [Dir]
passive (Wall p)          = posToDir p
passive (Opening _)       = moves
passive (Door p Nothing)  = moves
passive (Door p (Just 0)) = moves      -- doors can be seen from all sides
passive (Door p (Just n)) = posToDir p -- secret doors are like walls
passive (Stairs _ _ _)    = moves
passive _                 = []

-- | Perceptible is similar to passive, but describes which tiles can
-- be seen from which adjacent fields in the dark.
perceptible :: Terrain a -> [Dir]
perceptible Rock = []
perceptible p = case passive p of
                 [] -> moves
                 ds -> ds

-- | Produces a textual description for terrain, used if no objects
-- are present.
lookTerrain :: Terrain a -> String
lookTerrain (Floor _)          = "Floor."
lookTerrain Corridor           = "Corridor."
lookTerrain (Opening _)        = "An opening."
lookTerrain (Stairs _ Up _)    = "A staircase up."
lookTerrain (Stairs _ Down _)  = "A staircase down."
lookTerrain (Door _ Nothing)   = "An open door."
lookTerrain (Door _ (Just 0))  = "A closed door."
lookTerrain (Door _ (Just _))  = "A wall."  -- secret
lookTerrain (Wall _ )          = "A wall."
lookTerrain _                  = ""

-- | The parameter "n" is the level of evolution:
--
-- 0: final
-- 1: stairs added
-- 2: doors added
-- 3: corridors and openings added
-- 4: only rooms
--
-- The Bool indicates whether the loc is currently visible.
viewTerrain :: Int -> Bool -> Terrain a -> (Char, Color.Color)
viewTerrain n b t =
  let def =     if b then Color.BrWhite else Color.defFG
      defDark = if b then Color.BrYellow else Color.BrBlack
      defDoor = if b then Color.Yellow else Color.BrBlack
  in case t of
       Rock                -> (' ', def)
       (Opening d)
         | n <= 3          -> ('.', def)
         | otherwise       -> viewTerrain 0 b (Wall d)
       (Floor d)           -> ('.', if d == Light then def else defDark)
       Unknown             -> (' ', def)
       Corridor
         | n <= 3          -> ('#', if b then Color.BrWhite else Color.defFG)
         | otherwise       -> viewTerrain 0 b Rock
       (Wall p)
         | p == O          -> ('O', def)
         | p `elem` [L, R] -> ('|', def)
         | otherwise       -> ('-', def)
       (Stairs d p _)
         | n <= 1          -> (if p == Up then '<' else '>',
                               if d == Light then def else defDark)
         | otherwise       -> viewTerrain 0 b (Floor Dark)
       (Door p (Just 0))
         | n <= 2          -> ('+', defDoor)
         | otherwise       -> viewTerrain n b (Opening p)
       (Door p (Just _))
         | n <= 2          -> viewTerrain n b (Wall p)  -- secret door
         | otherwise       -> viewTerrain n b (Opening p)
       (Door p Nothing)
         | n <= 2          -> (if p `elem` [L, R] then '-' else '|', defDoor)
         | otherwise       -> viewTerrain n b (Opening p)
