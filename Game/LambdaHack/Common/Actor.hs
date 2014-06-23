{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Actors in the game: heroes, monsters, etc. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Common.Actor
  ( -- * Actor identifiers and related operations
    ActorId, monsterGenChance, partActor, partPronoun
    -- * The@ Acto@r type
  , Actor(..), ResDelta(..)
  , bspeed, actorTemplate, timeShiftFromSpeed, braced, waitedLastTurn
  , actorDying, actorNewBorn, hpTooLow, unoccupied, projectileKindId
    -- * Assorted
  , ActorDict, smellTimeout, checkAdjacent
  , mapActorItems_, mapActorInv_, mapActorEqp_
  , ppCStore, ppContainer
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
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
  { -- * Link to the constant properties
    bkind       :: !(Kind.Id ActorKind)  -- ^ the kind of the actor
    -- * Presentation
  , bsymbol     :: !Char                 -- ^ individual map symbol
  , bname       :: !Text                 -- ^ individual name
  , bpronoun    :: !Text                 -- ^ individual pronoun
  , bcolor      :: !Color.Color          -- ^ individual map color
    -- * Resources
  , btime       :: !Time                 -- ^ absolute time of next action
  , bhp         :: !Int                  -- ^ current hit points
  , bhpDelta    :: !ResDelta             -- ^ HP delta this turn
  , bcalm       :: !Int                  -- ^ current calm
  , bcalmDelta  :: !ResDelta             -- ^ calm delta this turn
    -- * Location
  , bpos        :: !Point                -- ^ current position
  , boldpos     :: !Point                -- ^ previous position
  , blid        :: !LevelId              -- ^ current level
  , boldlid     :: !LevelId              -- ^ previous level
  , bfid        :: !FactionId            -- ^ faction the actor belongs to
  , boldfid     :: !FactionId            -- ^ previous faction of the actor
  , btrajectory :: !(Maybe ([Vector], Speed))  -- ^ trajectory the actor must
                                               --   travel and his travel speed
    -- * Items
  , bbody       :: !ItemBag              -- ^ body parts
  , beqp        :: !ItemBag              -- ^ personal equipment
  , binv        :: !ItemBag              -- ^ personal inventory
    -- * Assorted
  , bwait       :: !Bool                 -- ^ is the actor waiting right now?
  , bproj       :: !Bool                 -- ^ is a projectile? (shorthand only,
                                         --   this can be deduced from bkind)
  }
  deriving (Show, Eq, Ord)

data ResDelta = ResDelta
  { resCurrentTurn  :: !Int  -- ^ resource change this player turn
  , resPreviousTurn :: !Int  -- ^ resource change last player turn
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
  -- Mimics @castDice@.
  let n = abs n'
      depth = max 1 $ abs depth'
      -- On level 1, First 2 monsters appear fast.
      scaledDepth = 5 * n `div` depth
  in chance $ 1%(fromIntegral (100 * (numMonsters - scaledDepth)) `max` 10)

-- | The part of speech describing the actor.
partActor :: Actor -> MU.Part
partActor b = MU.Text $ bname b

-- | The part of speech containing the actor pronoun.
partPronoun :: Actor -> MU.Part
partPronoun b = MU.Text $ bpronoun b

-- Actor operations

-- | A template for a new actor.
actorTemplate :: Kind.Id ActorKind -> Char -> Text -> Text
              -> Color.Color -> Int -> Int
              -> Point -> LevelId -> Time -> FactionId -> Bool
              -> Actor
actorTemplate bkind bsymbol bname bpronoun bcolor bhp bcalm
              bpos blid btime bfid bproj =
  let btrajectory = Nothing
      boldpos = Point 0 0  -- make sure /= bpos, to tell it didn't switch level
      boldlid = blid
      beqp    = EM.empty
      binv    = EM.empty
      bbody   = EM.empty
      bwait   = False
      boldfid = bfid
      bhpDelta = ResDelta 0 0
      bcalmDelta = ResDelta 0 0
  in Actor{..}

bspeed :: Kind.COps -> Actor -> Speed
bspeed Kind.COps{coactor=Kind.Ops{okind}} b =
  case btrajectory b of
    Nothing -> toSpeed $ aspeed $ okind $ bkind b
    Just (_, speed) -> speed

-- | Add time taken by a single step at the actor's current speed.
timeShiftFromSpeed :: Kind.COps -> Actor -> Time -> Time
timeShiftFromSpeed cops b time =
  let speed = bspeed cops b
      delta = ticksPerMeter speed
  in timeShift time delta

-- | Whether an actor is braced for combat this clip.
braced :: Actor -> Bool
braced b = bwait b

-- | The actor waited last turn.
waitedLastTurn :: Actor -> Bool
waitedLastTurn b = bwait b

actorDying :: Actor -> Bool
actorDying b = if bproj b
               then bhp b < 0
                    || maybe True (null . fst) (btrajectory b)
               else bhp b <= 0

actorNewBorn :: Actor -> Bool
actorNewBorn b = boldpos b == Point 0 0
                 && not (waitedLastTurn b)
                 && not (btime b < timeTurn)

hpTooLow :: Kind.Ops ActorKind -> Actor -> Bool
hpTooLow Kind.Ops{okind} b =
  let kind = okind $ bkind b
  in bhp b == 1 || 5 * bhp b < amaxHP kind

-- | Checks for the presence of actors in a position.
-- Does not check if the tile is walkable.
unoccupied :: [Actor] -> Point -> Bool
unoccupied actors pos = all (\b -> bpos b /= pos) actors

-- | The unique kind of projectiles.
projectileKindId :: Kind.Ops ActorKind -> Kind.Id ActorKind
projectileKindId Kind.Ops{ouniqGroup} = ouniqGroup "projectile"

-- | How long until an actor's smell vanishes from a tile.
smellTimeout :: Delta Time
smellTimeout = timeDeltaScale (Delta timeTurn) 100

-- | All actors on the level, indexed by actor identifier.
type ActorDict = EM.EnumMap ActorId Actor

checkAdjacent :: Actor -> Actor -> Bool
checkAdjacent sb tb = blid sb == blid tb && adjacent (bpos sb) (bpos tb)

mapActorItems_ :: Monad m => (ItemId -> Int -> m a) -> Actor -> m ()
mapActorItems_ f Actor{binv, beqp, bbody} = do
  let is = EM.assocs beqp ++ EM.assocs binv ++ EM.assocs bbody
  mapM_ (uncurry f) is

mapActorInv_ :: Monad m => (ItemId -> Int -> m a) -> Actor -> m ()
mapActorInv_ f Actor{binv} = do
  let is = EM.assocs binv
  mapM_ (uncurry f) is

mapActorEqp_ :: Monad m => (ItemId -> Int -> m a) -> Actor -> m ()
mapActorEqp_ f Actor{beqp} = do
  let is = EM.assocs beqp
  mapM_ (uncurry f) is

ppCStore :: CStore -> Text
ppCStore CGround = "on the ground"
ppCStore CBody = "in the body"
ppCStore CEqp = "in equipment"
ppCStore CInv = "in inventory"
ppCStore CSha = "in shared stash"

ppContainer :: Container -> Text
ppContainer CFloor{} = "on the ground nearby"
ppContainer (CActor _ cstore) = ppCStore cstore
ppContainer CTrunk{} = "as the trunk"

instance Binary Actor where
  put Actor{..} = do
    put bkind
    put bsymbol
    put bname
    put bpronoun
    put bcolor
    put bhp
    put bhpDelta
    put bcalm
    put bcalmDelta
    put btrajectory
    put bpos
    put boldpos
    put blid
    put boldlid
    put binv
    put beqp
    put bbody
    put btime
    put bwait
    put bfid
    put boldfid
    put bproj
  get = do
    bkind <- get
    bsymbol <- get
    bname <- get
    bpronoun <- get
    bcolor <- get
    bhp <- get
    bhpDelta <- get
    bcalm <- get
    bcalmDelta <- get
    btrajectory <- get
    bpos <- get
    boldpos <- get
    blid <- get
    boldlid <- get
    binv <- get
    beqp <- get
    bbody <- get
    btime <- get
    bwait <- get
    bfid <- get
    boldfid <- get
    bproj <- get
    return $! Actor{..}

instance Binary ResDelta where
  put ResDelta{..} = do
    put resCurrentTurn
    put resPreviousTurn
  get = do
    resCurrentTurn <- get
    resPreviousTurn <- get
    return $! ResDelta{..}
