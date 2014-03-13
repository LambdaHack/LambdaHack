{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Actors in the game: heroes, monsters, etc. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Common.Actor
  ( -- * Actor identifiers and related operations
    ActorId, monsterGenChance, partActor
    -- * The@ Acto@r type
  , Actor(..), actorTemplate, timeAddFromSpeed, braced, waitedLastTurn
  , unoccupied, heroKindId, projectileKindId
    -- * Inventory management
  , ItemBag, ItemDict, ItemRev
  , rmFromBag
    -- * Assorted
  , ActorDict, smellTimeout, checkAdjacent
  , mapActorItems_, mapActorInv_, mapActorEqp_
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.Ratio
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind

-- | Actor properties that are changing throughout the game.
-- If they are dublets of properties from @ActorKind@,
-- they are usually modified temporarily, but tend to return
-- to the original value from @ActorKind@ over time. E.g., HP.
data Actor = Actor
  { bkind       :: !(Kind.Id ActorKind)  -- ^ the kind of the actor
  , bsymbol     :: !Char                 -- ^ individual map symbol
  , bname       :: !Text                 -- ^ individual name
  , bcolor      :: !Color.Color          -- ^ individual map color
  , bspeed      :: !Speed                -- ^ individual speed
  , bhp         :: !Int                  -- ^ current hit points
  , bcalm       :: !Int                  -- ^ current calm
  , btrajectory :: !(Maybe [Vector])     -- ^ trajectory the actor must travel
  , bpos        :: !Point                -- ^ current position
  , boldpos     :: !Point                -- ^ previous position
  , blid        :: !LevelId              -- ^ current level
  , boldlid     :: !LevelId              -- ^ previous level
  , binv        :: !ItemBag              -- ^ personal inventory
  , beqp        :: !ItemBag              -- ^ personal equipment
  , btime       :: !Time                 -- ^ absolute time of next action
  , bwait       :: !Bool                 -- ^ is the actor waiting right now?
  , bfid        :: !FactionId            -- ^ faction the actor belongs to
  , bproj       :: !Bool                 -- ^ is a projectile? (shorthand only,
                                         --   this can be deduced from bkind)
  }
  deriving (Show, Eq, Ord)

-- | Chance that a new monster is generated. Currently depends on the
-- number of monsters already present, and on the level. In the future,
-- the strength of the character and the strength of the monsters present
-- could further influence the chance, and the chance could also affect
-- which monster is generated. How many and which monsters are generated
-- will also depend on the cave kind used to build the level.
monsterGenChance :: Int -> Int -> Int -> Rnd Bool
monsterGenChance n' depth' numMonsters =
  -- Mimics @castDeep@.
  let n = abs n'
      depth = abs depth'
      -- On level 1, First 2 monsters appear fast.
      scaledDepth = 5 * n `div` depth
  in chance $ 1%(fromIntegral (100 * (numMonsters - scaledDepth)) `max` 10)

-- | The part of speech describing the actor.
partActor :: Actor -> MU.Part
partActor b = MU.Text $ bname b

-- Actor operations

-- | A template for a new actor.
actorTemplate :: Kind.Id ActorKind -> Char -> Text
              -> Color.Color -> Speed -> Int -> Int -> Maybe [Vector]
              -> Point -> LevelId -> Time -> FactionId -> Bool -> Actor
actorTemplate bkind bsymbol bname bcolor bspeed bhp bcalm btrajectory
              bpos blid btime bfid bproj =
  let boldpos = Point 0 0  -- make sure /= bpos, to tell it didn't switch level
      boldlid = blid
      binv    = EM.empty
      beqp    = EM.empty
      bwait   = False
  in Actor{..}

-- | Add time taken by a single step at the actor's current speed.
timeAddFromSpeed :: Actor -> Time -> Time
timeAddFromSpeed b time =
  let speed = bspeed b
      delta = ticksPerMeter speed
  in timeAdd time delta

-- | Whether an actor is braced for combat this clip.
braced :: Actor -> Bool
braced b = bwait b

-- | The actor waited last turn.
waitedLastTurn :: Actor -> Bool
waitedLastTurn b = bwait b

-- | Checks for the presence of actors in a position.
-- Does not check if the tile is walkable.
unoccupied :: [Actor] -> Point -> Bool
unoccupied actors pos = all (\b -> bpos b /= pos) actors

-- | The unique kind of heroes.
heroKindId :: Kind.Ops ActorKind -> Kind.Id ActorKind
heroKindId Kind.Ops{ouniqGroup} = ouniqGroup "hero"

-- | The unique kind of projectiles.
projectileKindId :: Kind.Ops ActorKind -> Kind.Id ActorKind
projectileKindId Kind.Ops{ouniqGroup} = ouniqGroup "projectile"

-- | How long until an actor's smell vanishes from a tile.
smellTimeout :: Time
smellTimeout = timeScale timeTurn 100

type ItemBag = EM.EnumMap ItemId Int

-- | All items in the dungeon (including in actor inventories),
-- indexed by item identifier.
type ItemDict = EM.EnumMap ItemId Item

-- | All actors on the level, indexed by actor identifier.
type ActorDict = EM.EnumMap ActorId Actor

-- | Reverse item map, for item creation, to keep items and item identifiers
-- in bijection.
type ItemRev = HM.HashMap Item ItemId

rmFromBag :: Int -> ItemId -> ItemBag -> ItemBag
rmFromBag k iid bag =
  let rib Nothing = assert `failure` "rm from empty bag" `twith` (k, iid, bag)
      rib (Just n) = case compare n k of
        LT -> assert `failure` "rm more than there is" `twith` (n, k, iid, bag)
        EQ -> Nothing
        GT -> Just (n - k)
  in EM.alter rib iid bag

checkAdjacent :: Actor -> Actor -> Bool
checkAdjacent sb tb = blid sb == blid tb && adjacent (bpos sb) (bpos tb)

mapActorItems_ :: Monad m => (ItemId -> Int -> m a) -> Actor -> m ()
mapActorItems_ f Actor{binv, beqp} = do
  let is = EM.assocs binv ++ EM.assocs beqp
  mapM_ (uncurry f) is

mapActorInv_ :: Monad m => (ItemId -> Int -> m a) -> Actor -> m ()
mapActorInv_ f Actor{binv} = do
  let is = EM.assocs binv
  mapM_ (uncurry f) is

mapActorEqp_ :: Monad m => (ItemId -> Int -> m a) -> Actor -> m ()
mapActorEqp_ f Actor{beqp} = do
  let is = EM.assocs beqp
  mapM_ (uncurry f) is

instance Binary Actor where
  put Actor{..} = do
    put bkind
    put bsymbol
    put bname
    put bcolor
    put bspeed
    put bhp
    put bcalm
    put btrajectory
    put bpos
    put boldpos
    put blid
    put boldlid
    put binv
    put beqp
    put btime
    put bwait
    put bfid
    put bproj
  get = do
    bkind <- get
    bsymbol <- get
    bname <- get
    bcolor <- get
    bspeed <- get
    bhp <- get
    bcalm <- get
    btrajectory <- get
    bpos <- get
    boldpos <- get
    blid <- get
    boldlid <- get
    binv <- get
    beqp <- get
    btime <- get
    bwait <- get
    bfid <- get
    bproj <- get
    return $! Actor{..}
