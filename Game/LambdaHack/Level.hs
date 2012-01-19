-- | Inhabited dungeon levels and the operations to query and change them
-- as the game progresses.
module Game.LambdaHack.Level
  ( -- * The @Level@ type and its components
    Party, PartyItem, SmellMap, SecretMap, ItemMap, TileMap, Level(..)
    -- * Level update
  , updateHeroes, updateMonsters, updateHeroItem, updateMonItem
  , updateSmell, updateIMap, updateLMap, updateLRMap, dropItemsAt
    -- * Level query
  , at, rememberAt, atI, rememberAtI
  , stdRuleset, accessible, openable, findLoc, findLocTry
  ) where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.PointXY
import Game.LambdaHack.Point
import Game.LambdaHack.Actor
import Game.LambdaHack.Item
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Random
import Game.LambdaHack.Tile
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind

-- | All actors of a given side on the level.
type Party = IM.IntMap Actor

-- | Items carried by each party member.
type PartyItem = IM.IntMap [Item]

-- | Current smell on map tiles.
type SmellMap = IM.IntMap SmellTime

-- | Current secrecy value on map tiles.
type SecretMap = IM.IntMap SecretStrength

-- | Actual and remembered item lists on map tiles.
type ItemMap = IM.IntMap ([Item], [Item])

-- | Tile kinds on the map.
type TileMap = Kind.Array Loc TileKind

-- | A single, inhabited dungeon level.
data Level = Level
  { lheroes   :: Party      -- ^ all heroes on the level
  , lheroItem :: PartyItem  -- ^ hero items
  , lxsize    :: X          -- ^ width of the level
  , lysize    :: Y          -- ^ height of the level
  , lmonsters :: Party      -- ^ all monsters on the level
  , lmonItem  :: PartyItem  -- ^ monster imtems
  , lsmell    :: SmellMap   -- ^ smells
  , lsecret   :: SecretMap  -- ^ secrecy values
  , litem     :: ItemMap    -- ^ items on the ground
  , lmap      :: TileMap    -- ^ map tiles
  , lrmap     :: TileMap    -- ^ remembered map tiles
  , ldesc     :: String     -- ^ level description for the player
  , lmeta     :: String     -- ^ debug information from cave generation
  , lstairs   :: (Loc, Loc) -- ^ here the stairs (up/down) from other levels end
  }
  deriving Show

-- | Update the hero and monster maps.
updateHeroes, updateMonsters :: (Party -> Party) -> Level -> Level
updateHeroes f lvl = lvl { lheroes = f (lheroes lvl) }
updateMonsters f lvl = lvl { lmonsters = f (lmonsters lvl) }

-- | Update the hero items and monster items maps.
updateHeroItem, updateMonItem :: (PartyItem -> PartyItem) -> Level -> Level
updateHeroItem f lvl = lvl { lheroItem = f (lheroItem lvl) }
updateMonItem f lvl = lvl { lmonItem = f (lmonItem lvl) }

-- | Update the smell map.
updateSmell :: (SmellMap -> SmellMap) -> Level -> Level
updateSmell f lvl = lvl { lsmell = f (lsmell lvl) }

-- | Update the item on the ground map.
updateIMap :: (ItemMap -> ItemMap) -> Level -> Level
updateIMap f lvl = lvl { litem = f (litem lvl) }

-- | Update the tile and remembered tile maps.
updateLMap, updateLRMap :: (TileMap -> TileMap) -> Level -> Level
updateLMap f lvl = lvl { lmap = f (lmap lvl) }
updateLRMap f lvl = lvl { lrmap = f (lrmap lvl) }

-- Note: do not scatter items around, it's too much work for the player.
-- | Place all items on the list at a location on the level.
dropItemsAt :: [Item] -> Loc -> Level -> Level
dropItemsAt [] _loc = id
dropItemsAt items loc =
  let joinItems = L.foldl' (\ acc i -> snd (joinItem i acc))
      adj Nothing = Just (items, [])
      adj (Just (i, ri)) = Just (joinItems items i, ri)
  in  updateIMap (IM.alter adj loc)

instance Binary Level where
  put (Level hs hi sx sy ms mi ls le li lm lrm ld lme lstairs) = do
    put hs
    put hi
    put sx
    put sy
    put ms
    put mi
    put ls
    put le
    put (assert
           (IM.null (IM.filter (\ (is1, is2) ->
                                 L.null is1 && L.null is2) li)
           `blame` li) li)
    put lm
    put lrm
    put ld
    put lme
    put lstairs
  get = do
    hs <- get
    hi <- get
    sx <- get
    sy <- get
    ms <- get
    mi <- get
    ls <- get
    le <- get
    li <- get
    lm <- get
    lrm <- get
    ld <- get
    lme <- get
    lstairs <- get
    return (Level hs hi sx sy ms mi ls le li lm lrm ld lme lstairs)

-- | Query for actual and remembered tile kinds on the map.
at, rememberAt :: Level -> Loc -> (Kind.Id TileKind)
at         Level{lmap}  p = lmap Kind.! p
rememberAt Level{lrmap} p = lrmap Kind.! p

-- Note: representations with 2 maps leads to longer code and slower 'remember'.
-- | Query for actual and remembered items on the ground.
atI, rememberAtI :: Level -> Loc -> [Item]
atI         Level{litem} p = fst $ IM.findWithDefault ([], []) p litem
rememberAtI Level{litem} p = snd $ IM.findWithDefault ([], []) p litem

-- | The standard ruleset used for level operations.
stdRuleset :: Kind.Ops RuleKind -> RuleKind
stdRuleset Kind.Ops{ouniqGroup, okind} = okind $ ouniqGroup "standard"

-- | Check whether one location is accessible from another,
-- using the formula from the standard ruleset.
accessible :: Kind.COps -> Level -> Loc -> Loc -> Bool
accessible Kind.COps{ cotile=Kind.Ops{okind=okind}, corule}
           lvl@Level{lxsize} sloc tloc =
  let check = raccessible $ stdRuleset corule
      src = okind $ lvl `at` sloc
      tgt = okind $ lvl `at` tloc
  in check lxsize sloc src tloc tgt

-- | Check whether the location contains a door of secrecy lower than k
-- and that can be opened according to the standard ruleset.
openable :: Kind.Ops TileKind -> Level -> SecretStrength -> Loc -> Bool
openable cops lvl@Level{lsecret} k target =
  let tgt = lvl `at` target
  in hasFeature cops F.Openable tgt ||
     (hasFeature cops F.Hidden tgt &&
      lsecret IM.! target < k)

-- | Find a random location on the map satisfying a predicate.
findLoc :: TileMap -> (Loc -> (Kind.Id TileKind) -> Bool) -> Rnd Loc
findLoc lmap p =
  let search = do
        loc <- randomR $ Kind.bounds lmap
        let tile = lmap Kind.! loc
        if p loc tile
          then return loc
          else search
  in search

-- | Find a random location on the map satisfying the first predicate and,
-- if the premitted number of attempts suffices, also satisfying the second.
findLocTry :: Int                                -- ^ the number of tries
           -> TileMap                            -- ^ look up in this map
           -> (Loc -> Kind.Id TileKind -> Bool)  -- ^ loop until satisfied
           -> (Loc -> Kind.Id TileKind -> Bool)  -- ^ try only a number of times
           -> Rnd Loc
findLocTry numTries lmap p pTry =
  let search k = do
        loc <- randomR $ Kind.bounds lmap
        let tile = lmap Kind.! loc
        if p loc tile && pTry loc tile
          then return loc
          else if k > 1
            then search (k - 1)
            else findLoc lmap p
  in assert (numTries > 0) $
     search numTries
