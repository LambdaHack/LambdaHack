{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies #-}
-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( -- * General content types
    Id, sentinelId, Speedup(..), Ops(..), COps(..), createOps, stdRuleset
    -- * Arrays of content identifiers
  , Array, (!), (//), listArray, bounds, foldlArray
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.Array.IArray as A
import Data.Array.Unboxed (UArray)
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Ix as Ix
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
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
  { okind         :: Id a -> a      -- ^ the content element at given id
  , ouniqGroup    :: Text -> Id a   -- ^ the id of the unique member of
                                    --   a singleton content group
  , opick         :: Text -> (a -> Bool) -> Rnd (Maybe (Id a))
                                    -- ^ pick a random id belonging to a group
                                    --   and satisfying a predicate
  , ofoldrWithKey :: forall b. (Id a -> a -> b -> b) -> b -> b
                                    -- ^ fold over all content elements of @a@
  , obounds       :: !(Id a, Id a)  -- ^ bounds of identifiers of content @a@
  , ospeedup      :: !(Maybe (Speedup a))  -- ^ auxiliary speedup components
  }

-- | Create content operations for type @a@ from definition of content
-- of type @a@.
createOps :: forall a. Show a => ContentDef a -> Ops a
createOps ContentDef{getName, getFreq, content, validate} =
  assert (Id (fromIntegral $ length content) < sentinelId) $
  let kindMap :: EM.EnumMap (Id a) a
      !kindMap = EM.fromDistinctAscList $ zip [Id 0..] content
      kindFreq :: M.Map Text (Frequency (Id a, a))
      kindFreq =
        let tuples = [ (cgroup, (n, (i, k)))
                     | (i, k) <- EM.assocs kindMap
                     , (cgroup, n) <- getFreq k, n > 0 ]
            f m (cgroup, nik) = M.insertWith (++) cgroup [nik] m
            lists = foldl' f M.empty tuples
            nameFreq cgroup = toFreq $ "opick ('" <> cgroup <> "')"
        in M.mapWithKey nameFreq lists
      okind i = fromMaybe (assert `failure` "no kind" `twith` (i, kindMap))
                $ EM.lookup i kindMap
      correct a = not (T.null (getName a)) && all ((> 0) . snd) (getFreq a)
      offenders = validate content
  in assert (allB correct content) $
     assert (null offenders `blame` "content not valid" `twith` offenders)
     -- By this point 'content' can be GCd.
     Ops
       { okind
       , ouniqGroup = \cgroup ->
           let freq = fromMaybe (assert `failure` "no unique group"
                                        `twith` (cgroup, kindFreq))
                      $ M.lookup cgroup kindFreq
           in case runFrequency freq of
             [(n, (i, _))] | n > 0 -> i
             l -> assert `failure` "not unique" `twith` (l, cgroup, kindFreq)
       , opick = \cgroup p ->
           case M.lookup cgroup kindFreq of
             Just freq | not $ nullFreq freq -> fmap Just $ frequency $ do
               (i, k) <- freq
               breturn (p k) i
               {- with MonadComprehensions:
               frequency [ i | (i, k) <- kindFreq M.! cgroup, p k ]
               -}
             _ -> return Nothing
       , ofoldrWithKey = \f z -> foldr (\(i, a) -> f i a) z
                                 $ EM.assocs kindMap
       , obounds = ( fst $ EM.findMin kindMap
                   , fst $ EM.findMax kindMap )
       , ospeedup = Nothing  -- define elsewhere
       }

-- | Operations for all content types, gathered together.
data COps = COps
  { coactor   :: !(Ops ActorKind)
  , cocave    :: !(Ops CaveKind)
  , cofaction :: !(Ops FactionKind)
  , coitem    :: !(Ops ItemKind)
  , comode    :: !(Ops ModeKind)
  , coplace   :: !(Ops PlaceKind)
  , corule    :: !(Ops RuleKind)
  , cotile    :: !(Ops TileKind)
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
-- The arrays are indexed by the X and Y coordinates
-- of the dungeon tile position.
newtype Array c = Array (A.Array Y (UArray X Word8))
  deriving Eq

-- TODO: save/restore is still too slow, but we are already past
-- the point of diminishing returns. A dramatic change would be
-- low-level conversion to ByteString and serializing that.
instance Binary (Array c) where
  put (Array a) = put a
  get = fmap Array get

instance Show (Array c) where
  show _ = "Kind.Array"
--  show a = "Kind.Array with bounds " ++ show (bounds a)

-- | Content identifiers array lookup.
(!) :: Array c -> Point -> Id c -- TDDO: PointXY?
(!) (Array a) p = let PointXY x y = fromPoint 0 p in Id $ a A.! y A.! x

-- TODO: optimize, perhaps after switching to Vectors
-- | Construct a content identifiers array updated with the association list.
(//) :: Array c -> [(PointXY, Id c)] -> Array c
(//) (Array a) l =
  let f b (x, e) = b A.// [(x, e)]
  in Array $ A.accum f a [(y, (x, e)) | (PointXY x y, Id e) <- l]

-- TODO: optimize, perhaps after switching to Vectors
-- | Create a content identifiers array from a list of elements.
listArray :: (PointXY, PointXY) -> [Id c] -> Array c
listArray ((PointXY x0 y0), (PointXY x1 y1)) ids =
  let es = [e | Id e <- ids]
      xsize = x1 - x0
      split [] = []
      split l = let (line, rest) = splitAt xsize l
                in line : split rest
  in Array $ A.listArray (y0, y1) $ map (A.listArray (x0, x1)) $ split es

-- | Create a content identifiers array from an association list.
--array :: (PointXY, PointXY) -> [(PointXY, Id c)] -> Array c
--array bds l = Array $ A.array bds [(i, e) | (i, Id e) <- l]

-- | Content identifiers array bounds.
bounds :: Array c -> (PointXY, PointXY)
bounds (Array a) =
  let (y0, y1) = A.bounds a
      (x0, x1) = A.bounds $ a A.! 0
  in ((PointXY x0 y0), (PointXY x1 y1))

-- | Fold left strictly over an array.
foldlArray :: (a -> Id c -> a) -> a -> Array c -> a
foldlArray f z0 (Array a) = lgo z0 $ concatMap A.elems $ A.elems a
 where lgo z []       = z
       lgo z (x : xs) = let fzx = f z (Id x) in fzx `seq` lgo fzx xs
