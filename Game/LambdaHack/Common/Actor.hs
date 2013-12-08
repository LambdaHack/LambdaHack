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
  , ItemBag, ItemInv, InvChar(..), ItemDict, ItemRev
  , allLetters, assignLetter, letterLabel, letterRange, rmFromBag
    -- * Assorted
  , ActorDict, smellTimeout, mapActorItems_
  ) where

import Data.Binary
import Data.Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Utils.Assert

-- | A unique identifier of an actor in the dungeon.
newtype ActorId = ActorId Int
  deriving (Show, Eq, Ord, Enum)

instance Binary ActorId where
  put (ActorId n) = put n
  get = fmap ActorId get

-- | Actor properties that are changing throughout the game.
-- If they are dublets of properties from @ActorKind@,
-- they are usually modified temporarily, but tend to return
-- to the original value from @ActorKind@ over time. E.g., HP.
data Actor = Actor
  { bkind   :: !(Kind.Id ActorKind)  -- ^ the kind of the actor
  , bsymbol :: !Char                 -- ^ individual map symbol
  , bname   :: !Text                 -- ^ individual name
  , bcolor  :: !Color.Color          -- ^ individual map color
  , bspeed  :: !Speed                -- ^ individual speed
  , bhp     :: !Int                  -- ^ current hit points
  , bpath   :: !(Maybe [Vector])     -- ^ path the actor is forced to travel
  , bpos    :: !Point                -- ^ current position
  , boldpos :: !Point                -- ^ previous position
  , blid    :: !LevelId              -- ^ current level
  , bbag    :: !ItemBag              -- ^ items carried
  , binv    :: !ItemInv              -- ^ map from letters to items
  , bletter :: !InvChar              -- ^ next inventory letter
  , btime   :: !Time                 -- ^ absolute time of next action
  , bwait   :: !Time                 -- ^ last bracing expires at this time
  , bfid    :: !FactionId            -- ^ to which faction the actor belongs
  , bproj   :: !Bool                 -- ^ is a projectile? (shorthand only,
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
      scaledDepth = 10 * (n - 1) `div` max 1 (depth - 1)
  in chance $ 1%(fromIntegral (50 * (numMonsters - scaledDepth)) `max` 5)

-- | The part of speech describing the actor.
partActor :: Actor -> MU.Part
partActor b = MU.Text $ bname b

-- Actor operations

-- | A template for a new non-projectile actor.
actorTemplate :: Kind.Id ActorKind -> Char -> Text
              -> Color.Color -> Speed -> Int -> Maybe [Vector]
              -> Point -> LevelId -> Time -> FactionId -> Bool -> Actor
actorTemplate bkind bsymbol bname bcolor bspeed bhp bpath bpos blid btime
              bfid bproj =
  let boldpos = bpos
      bbag    = EM.empty
      binv    = EM.empty
      bletter = InvChar 'a'
      bwait   = timeZero
  in Actor{..}

-- | Add time taken by a single step at the actor's current speed.
timeAddFromSpeed :: Actor -> Time -> Time
timeAddFromSpeed b time =
  let speed = bspeed b
      delta = ticksPerMeter speed
  in timeAdd time delta

-- | Whether an actor is braced for combat this clip. If a foe
-- moves just after this actor in the same time moment, the actor won't block,
-- because @bwait@ is reset to zero in @ageActorA@ before the condition
-- is checked.
braced :: Actor -> Time -> Bool
braced b time = time <= bwait b

-- | The actor most probably waited at most a turn ago (unless his speed
-- was changed, etc.)
waitedLastTurn :: Actor -> Time -> Bool
waitedLastTurn b time = time <= bwait b

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

newtype InvChar = InvChar {invChar :: Char}
  deriving (Show, Eq, Enum)

instance Ord InvChar where
  compare (InvChar x) (InvChar y) =
    compare (isUpper x, toLower x) (isUpper y, toLower y)

instance Binary InvChar where
  put (InvChar x) = put x
  get = fmap InvChar get

type ItemBag = EM.EnumMap ItemId Int

type ItemInv = EM.EnumMap InvChar ItemId

-- | All items in the dungeon (including in actor inventories),
-- indexed by item identifier.
type ItemDict = EM.EnumMap ItemId Item

-- | All actors on the level, indexed by actor identifier.
type ActorDict = EM.EnumMap ActorId Actor

-- | Reverse item map, for item creation, to keep items and item identifiers
-- in bijection.
type ItemRev = HM.HashMap Item ItemId

cmpLetter :: InvChar -> InvChar -> Ordering
cmpLetter (InvChar x) (InvChar y) =
  compare (isUpper x, toLower x) (isUpper y, toLower y)

allLetters :: [InvChar]
allLetters = map InvChar $ ['a'..'z'] ++ ['A'..'Z']

-- | Assigns a letter to an item, for inclusion in the inventory
-- of a hero. Tries to to use the requested letter, if any.
assignLetter :: ItemId -> Maybe InvChar -> Actor -> Maybe InvChar
assignLetter iid r body =
  case lookup iid $ map swap $ EM.assocs $ binv body of
    Just l -> Just l
    Nothing ->  case r of
      Just l | l `elem` allowed -> Just l
      _ -> listToMaybe free
 where
  c = bletter body
  candidates = take (length allLetters)
               $ drop (fromJust (elemIndex c allLetters))
               $ cycle allLetters
  inBag = EM.keysSet $ bbag body
  f l = maybe True (`ES.notMember` inBag) $ EM.lookup l $ binv body
  free = filter f candidates
  allowed = InvChar '$' : free

letterRange :: [InvChar] -> Text
letterRange ls =
  sectionBy (sortBy cmpLetter ls) Nothing
 where
  succLetter c d = ord (invChar d) - ord (invChar c) == 1

  sectionBy []     Nothing       = T.empty
  sectionBy []     (Just (c, d)) = finish (c,d)
  sectionBy (x:xs) Nothing       = sectionBy xs (Just (x, x))
  sectionBy (x:xs) (Just (c, d))
    | succLetter d x             = sectionBy xs (Just (c, x))
    | otherwise                  = finish (c,d) <> sectionBy xs (Just (x, x))

  finish (c, d) | c == d         = T.pack [invChar c]
                | succLetter c d = T.pack [invChar c, invChar d]
                | otherwise      = T.pack [invChar c, '-', invChar d]

letterLabel :: InvChar -> MU.Part
letterLabel c = MU.Text $ T.pack $ invChar c : " -"

rmFromBag :: Int -> ItemId -> ItemBag -> ItemBag
rmFromBag k iid bag =
  let rib Nothing = assert `failure` "rm from empty bag" `twith` (k, iid, bag)
      rib (Just n) = case compare n k of
        LT -> assert `failure` "rm more than there is" `twith` (n, k, iid, bag)
        EQ -> Nothing
        GT -> Just (n - k)
  in EM.alter rib iid bag

mapActorItems_ :: Monad m => (ItemId -> Int -> m a) -> Actor -> m ()
mapActorItems_ f Actor{bbag} = do
  let is = EM.assocs bbag
  mapM_ (uncurry f) is

instance Binary Actor where
  put Actor{..} = do
    put bkind
    put bsymbol
    put bname
    put bcolor
    put bspeed
    put bhp
    put bpath
    put bpos
    put boldpos
    put blid
    put bbag
    put binv
    put bletter
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
    bpath <- get
    bpos <- get
    boldpos <- get
    blid <- get
    bbag <- get
    binv <- get
    bletter <- get
    btime <- get
    bwait <- get
    bfid <- get
    bproj <- get
    return Actor{..}
