{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, RankNTypes,
             TypeFamilies #-}
-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( -- * General content types
    Id, sentinelId, Speedup(..), Ops(..), COps(..), createOps, stdRuleset
    -- * Arrays of content identifiers
  , Array, (!), (//), listArray, array, bounds, foldlArray
  ) where

import qualified Data.Array.Unboxed as A
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Ix as Ix
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency

-- | Content identifiers for the content type @c@.
newtype Id c = Id Word8
  deriving (Show, Eq, Ord, Ix.Ix, Enum)

instance Binary (Id c) where
  put (Id i) = put i
  get = fmap Id get

sentinelId :: Id c
sentinelId = Id 255

-- | Type family for auxiliary data structures for speeding up
-- content operations.
data family Speedup a

data instance Speedup TileKind = TileSpeedup
  { isClearTab :: Id TileKind -> Bool
  , isLitTab   :: Id TileKind -> Bool
  }

-- | Content operations for the content of type @a@.
data Ops a = Ops
  { osymbol       :: Id a -> Char  -- ^ the symbol of a content element at id
  , oname         :: Id a -> Text  -- ^ the name of a content element at id
  , okind         :: Id a -> a     -- ^ the content element at given id
  , ouniqGroup    :: Text -> Id a  -- ^ the id of the unique member of
                                   --   a singleton content group
  , opick         :: Text -> (a -> Bool) -> Rnd (Id a)
                                   -- ^ pick a random id belonging to a group
                                   --   and satisfying a predicate
  , ofoldrWithKey :: forall b. (Id a -> a -> b -> b) -> b -> b
                                   -- ^ fold over all content elements of @a@
  , obounds       :: (Id a, Id a)  -- ^ bounds of identifiers of content @a@
  , ospeedup      :: Speedup a     -- ^ auxiliary speedup components
  }

-- | Create content operations for type @a@ from definition of content
-- of type @a@.
createOps :: forall a. Show a => ContentDef a -> Ops a
createOps ContentDef{getSymbol, getName, getFreq, content, validate} =
  assert (Id (fromIntegral $ length content) < sentinelId) $
  let kindAssocs :: [(Word8, a)]
      kindAssocs = L.zip [0..] content
      kindMap :: EM.EnumMap (Id a) a
      kindMap = EM.fromDistinctAscList $ L.zip [Id 0..] content
      kindFreq :: M.Map Text (Frequency (Id a, a))
      kindFreq =
        let tuples = [ (group, (n, (Id i, k)))
                     | (i, k) <- kindAssocs
                     , (group, n) <- getFreq k, n > 0 ]
            f m (group, nik) = M.insertWith (++) group [nik] m
            lists = L.foldl' f M.empty tuples
            nameFreq group = toFreq $ "opick ('" <> group <> "')"
        in M.mapWithKey nameFreq lists
      okind i = fromMaybe (assert `failure` (i, kindMap))
                $ EM.lookup i kindMap
      correct a = not (T.null (getName a)) && L.all ((> 0) . snd) (getFreq a)
      offenders = validate content
  in assert (allB correct content) $
     assert (L.null offenders `blame` ("content failed validation: " :: Text,
                                       offenders))
     Ops
       { osymbol = getSymbol . okind
       , oname = getName . okind
       , okind = okind
       , ouniqGroup = \ group ->
           let freq = fromMaybe (assert `failure` (group, kindFreq))
                      $ M.lookup group kindFreq
           in case runFrequency freq of
             [(n, (i, _))] | n > 0 -> i
             l -> assert `failure` l
       , opick = \ group p ->
           let freq = fromMaybe (assert `failure` (group, kindFreq))
                      $ M.lookup group kindFreq
           in frequency $ do
             (i, k) <- freq
             breturn (p k) i
             {- with MonadComprehensions:
             frequency [ i | (i, k) <- kindFreq M.! group, p k ]
             -}
       , ofoldrWithKey = \ f z -> L.foldr (\ (i, a) -> f (Id i) a) z kindAssocs
       , obounds = ( fst $ EM.findMin kindMap
                   , fst $ EM.findMax kindMap )
       , ospeedup = undefined  -- define elsewhere
       }

-- | Operations for all content types, gathered together.
data COps = COps
  { coactor :: !(Ops ActorKind)
  , cocave  :: !(Ops CaveKind)
  , cofact  :: !(Ops FactionKind)
  , coitem  :: !(Ops ItemKind)
  , comode  :: !(Ops ModeKind)
  , coplace :: !(Ops PlaceKind)
  , corule  :: !(Ops RuleKind)
  , cotile  :: !(Ops TileKind)
  }

-- | The standard ruleset used for level operations.
stdRuleset :: Ops RuleKind -> RuleKind
stdRuleset Ops{ouniqGroup, okind} = okind $ ouniqGroup "standard"

instance Show COps where
  show _ = "game content"

instance Eq COps where
  (==) _ _ = True

-- | Arrays of content identifiers pointing to the content type @c@,
-- where the identifiers are represented as @Word8@
-- (and so content of type @c@ can have at most 256 elements).
-- The arrays are indexed by type @i@, e.g., a dungeon tile position.
newtype Array i c = Array (A.UArray i Word8)
  deriving Eq

-- TODO: save/restore is still too slow, but we are already past
-- the point of diminishing returns. A dramatic change would be
-- low-level conversion to ByteString and serializing that.
instance (Ix.Ix i, Binary i) => Binary (Array i c) where
  put (Array a) = put a
  get = fmap Array get

instance (Ix.Ix i, Show i) => Show (Array i c) where
  show a = "Kind.Array with bounds " ++ show (bounds a)

-- | Content identifiers array lookup.
(!) :: Ix.Ix i => Array i c -> i -> Id c
(!) (Array a) i = Id $ a A.! i

-- | Construct a content identifiers array updated with the association list.
(//) :: Ix.Ix i => Array i c -> [(i, Id c)] -> Array i c
(//) (Array a) l = Array $ a A.// [(i, e) | (i, Id e) <- l]

-- | Create a content identifiers array from a list of elements.
listArray :: Ix.Ix i => (i, i) -> [Id c] -> Array i c
listArray bds l = Array $ A.listArray bds [e | Id e <- l]

-- | Create a content identifiers array from an association list.
array :: Ix.Ix i => (i, i) -> [(i, Id c)] -> Array i c
array bds l = Array $ A.array bds [(i, e) | (i, Id e) <- l]

-- | Content identifiers array bounds.
bounds :: Ix.Ix i => Array i c -> (i, i)
bounds (Array a) = A.bounds a

-- | Fold left strictly over an array.
foldlArray :: Ix.Ix i => (a -> Id c -> a) -> a -> Array i c -> a
foldlArray f z0 (Array a) = lgo z0 $ A.elems a
 where lgo z []       = z
       lgo z (x : xs) = let fzx = f z (Id x) in fzx `seq` lgo fzx xs
