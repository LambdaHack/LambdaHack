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
  | Opening
  | Floor DL
  | Unknown
  | Corridor
  | Wall
  | Stairs DL VDir (Maybe a)
  | Door (Maybe Int)  -- Nothing: open, Just 0: closed, otherwise secret
  deriving Show

instance Binary VDir where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8

instance Binary a => Binary (Terrain a) where
  put Rock            = putWord8 0
  put Opening         = putWord8 1
  put (Floor dl)      = putWord8 2 >> put dl
  put Unknown         = putWord8 3
  put Corridor        = putWord8 4
  put Wall            = putWord8 5
  put (Stairs dl d n) = putWord8 6 >> put dl >> put d >> put n
  put (Door o)      = putWord8 7 >> put o
  get = do
          tag <- getWord8
          case tag of
            0 -> return Rock
            1 -> return Opening
            2 -> liftM Floor get
            3 -> return Unknown
            4 -> return Corridor
            5 -> return Wall
            6 -> liftM3 Stairs get get get
            7 -> liftM Door get
            _ -> fail "no parse (Terrain)"

instance Eq a => Eq (Terrain a) where
  Rock == Rock = True
  Opening == Opening = True
  Floor l == Floor l' = l == l'
  Unknown == Unknown = True
  Corridor == Corridor = True
  Wall == Wall = True
  Stairs dl d t == Stairs dl' d' t' = dl == dl' && d == d' && t == t'
  Door o == Door o' = o == o'
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
--     * 'O': a pillar
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

isWall :: Terrain a -> Bool
isWall Wall = True
isWall _    = False

isRock :: Terrain a -> Bool
isRock Rock = True
isRock _    = False

isUnknown :: Terrain a -> Bool
isUnknown Unknown = True
isUnknown _       = False

-- | allows moves and vision
isOpen :: Terrain a -> Bool
isOpen (Floor {})   = True
isOpen Opening      = True
isOpen (Door o)     = isNothing o
isOpen Corridor     = True
isOpen (Stairs {})  = True
isOpen _            = False

-- | marks an exit from a room
isExit :: Terrain a -> Bool
isExit Opening   = True
isExit (Door _)  = True
isExit _         = False

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

-- | Produces a textual description for terrain, used if no objects
-- are present.
lookTerrain :: Terrain a -> String
lookTerrain (Floor _)         = "Floor."
lookTerrain Corridor          = "Corridor."
lookTerrain Opening           = "An opening."
lookTerrain (Stairs _ Up _)   = "A staircase up."
lookTerrain (Stairs _ Down _) = "A staircase down."
lookTerrain (Door Nothing)    = "An open door."
lookTerrain (Door (Just 0))   = "A closed door."
lookTerrain (Door (Just _))   = "A wall."  -- secret
lookTerrain Wall              = "A wall."
lookTerrain _                 = ""

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
       Rock                -> ('#', def)
       Opening
         | n <= 3          -> ('.', def)
         | otherwise       -> viewTerrain 0 b Wall
       (Floor d)           -> ('.', if d == Light then def else defDark)
       Unknown             -> (' ', def)
       Corridor
         | n <= 3          -> ('.', if b then Color.BrWhite else Color.defFG)
         | otherwise       -> viewTerrain 0 b Rock
       Wall                -> ('#', def)
       (Stairs d p _)
         | n <= 1          -> (if p == Up then '<' else '>',
                               if d == Light then def else defDark)
         | otherwise       -> viewTerrain 0 b (Floor Dark)
       (Door (Just 0))
         | n <= 2          -> ('+', defDoor)
         | otherwise       -> viewTerrain n b Opening
       (Door (Just _))
         | n <= 2          -> viewTerrain n b Wall  -- secret door
         | otherwise       -> viewTerrain n b Opening
       (Door Nothing)
         | n <= 2          -> ('\'', defDoor)
         | otherwise       -> viewTerrain n b Opening
