{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings
             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Actors in the game: heroes, monsters, etc. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Actor
  ( -- * Actor identifiers and related operations
    ActorId, monsterGenChance, partActor
    -- * The@ Acto@r type
  , Actor(..), actorTemplate, timeAddFromSpeed, braced
  , unoccupied, heroKindId, projectileKindId, actorSpeed
    -- * Inventory management
  , ItemBag, ItemInv, InvChar(..), ItemDict, ItemRev
  , allLetters, assignLetter, letterLabel, letterRange, rmFromBag
    -- * Assorted
  , smellTimeout
  ) where

import Data.Binary
import Data.Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Hashable as Hashable
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple
import Data.Typeable
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.FactionId
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | A unique identifier of an actor in the dungeon.
newtype ActorId = ActorId Int
  deriving (Show, Eq, Ord, Enum, Typeable)

instance Binary ActorId where
  put (ActorId n) = put n
  get = fmap ActorId get

-- TODO: use target (in ClientState) instead of bdirAI to determine goal,
-- even for silly monsters just introduce more randomness,
-- but try to attain the goal. Then remove bdirAI.
-- | Actor properties that are changing throughout the game.
-- If they are dublets of properties from @ActorKind@,
-- they are usually modified temporarily, but tend to return
-- to the original value from @ActorKind@ over time. E.g., HP.
data Actor = Actor
  { bkind    :: !(Kind.Id ActorKind)    -- ^ the kind of the actor
  , bsymbol  :: !(Maybe Char)           -- ^ individual map symbol
  , bname    :: !(Maybe Text)           -- ^ individual name
  , bcolor   :: !(Maybe Color.Color)    -- ^ individual map color
  , bspeed   :: !(Maybe Speed)          -- ^ individual speed
  , bhp      :: !Int                    -- ^ current hit points
  , bdirAI   :: !(Maybe (Vector, Int))  -- ^ direction and distance of running
  , bpath    :: !(Maybe [Vector])       -- ^ path the actor is forced to travel
  , bpos     :: !Point                  -- ^ current position
  , bbag     :: !ItemBag                -- ^ items carried
  , binv     :: !ItemInv                -- ^ map from letters to items
  , bletter  :: !InvChar                -- ^ next inventory letter
  , btime    :: !Time                   -- ^ absolute time of next action
  , bwait    :: !Time                   -- ^ last bracing expires at this time
  , bfaction :: !FactionId              -- ^ to which faction the actor belongs
  , bproj    :: !Bool                   -- ^ is a projectile? (shorthand only,
                                        -- this can be deduced from bkind)
  }
  deriving (Show, Eq)

-- | Chance that a new monster is generated. Currently depends on the
-- number of monsters already present, and on the level. In the future,
-- the strength of the character and the strength of the monsters present
-- could further influence the chance, and the chance could also affect
-- which monster is generated. How many and which monsters are generated
-- will also depend on the cave kind used to build the level.
monsterGenChance :: Int -> Int -> Rnd Bool
monsterGenChance depth numMonsters =
  chance $ 1%(fromIntegral (30 * (numMonsters - depth)) `max` 5)

-- | The part of speech describing the actor.
partActor :: Kind.Ops ActorKind -> Actor -> MU.Part
partActor Kind.Ops{oname} a = MU.Text $ fromMaybe (oname $ bkind a) (bname a)

-- Actor operations

-- | A template for a new non-projectile actor.
actorTemplate :: Kind.Id ActorKind -> Maybe Char -> Maybe Text -> Int -> Point
              -> Time -> FactionId -> Bool -> Actor
actorTemplate bkind bsymbol bname bhp bpos btime bfaction bproj =
  let bcolor  = Nothing
      bspeed  = Nothing
      bpath   = Nothing
      bdirAI  = Nothing
      bbag    = EM.empty
      binv    = EM.empty
      bletter = InvChar 'a'
      bwait   = timeZero
  in Actor{..}

-- | Access actor speed, individual or, otherwise, stock.
actorSpeed :: Kind.Ops ActorKind -> Actor -> Speed
actorSpeed Kind.Ops{okind} m =
  let stockSpeed = aspeed $ okind $ bkind m
  in fromMaybe stockSpeed $ bspeed m

-- | Add time taken by a single step at the actor's current speed.
timeAddFromSpeed :: Kind.Ops ActorKind -> Actor -> Time -> Time
timeAddFromSpeed coactor m time =
  let speed = actorSpeed coactor m
      delta = ticksPerMeter speed
  in timeAdd time delta

-- | Whether an actor is braced for combat this turn.
braced :: Actor -> Time -> Bool
braced m time = time < bwait m

-- | Checks for the presence of actors in a position.
-- Does not check if the tile is walkable.
unoccupied :: [Actor] -> Point -> Bool
unoccupied actors loc =
  all (\ body -> bpos body /= loc) actors

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

-- | Reverse item map, for item creation, to keep items and item identifiers
-- in bijection.
type ItemRev = HM.HashMap Item ItemId

instance (Binary k, Binary v, Eq k, Hashable.Hashable k)
  => Binary (HM.HashMap k v) where
  put ir = put $ HM.toList ir
  get = fmap HM.fromList get

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
               $ drop (fromJust (findIndex (== c) allLetters))
               $ cycle allLetters
  inBag = EM.keysSet $ bbag body
  f l = maybe True (\i -> i `ES.notMember` inBag) $ EM.lookup l $ binv body
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
                | succLetter c d = T.pack $ [invChar c, invChar d]
                | otherwise      = T.pack $ [invChar c, '-', invChar d]

letterLabel :: InvChar -> MU.Part
letterLabel c = MU.Text $ T.pack $ invChar c : " -"

rmFromBag :: Int -> ItemId -> ItemBag -> ItemBag
rmFromBag k iid bag =
  let rib Nothing = assert `failure` (k, iid, bag)
      rib (Just n) = case compare n k of
        LT -> assert `failure` (n, k, iid, bag)
        EQ -> Nothing
        GT -> Just (n - k)
  in EM.alter rib iid bag

instance Binary Actor where
  put Actor{..} = do
    put bkind
    put bsymbol
    put bname
    put bcolor
    put bspeed
    put bhp
    put bdirAI
    put bpath
    put bpos
    put bbag
    put binv
    put bletter
    put btime
    put bwait
    put bfaction
    put bproj
  get = do
    bkind   <- get
    bsymbol <- get
    bname   <- get
    bcolor  <- get
    bspeed  <- get
    bhp     <- get
    bdirAI  <- get
    bpath   <- get
    bpos    <- get
    bbag <- get
    binv    <- get
    bletter <- get
    btime   <- get
    bwait   <- get
    bfaction <- get
    bproj    <- get
    return Actor{..}
