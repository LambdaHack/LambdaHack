{-# LANGUAGE FlexibleContexts, StandaloneDeriving, TypeFamilies #-}
-- | Arrays, based on Data.Vector.Unboxed, indexed by @Point@.
module Game.LambdaHack.Common.PointArray
  ( UnboxRepClass(..), Array(..), pindex, punindex
  , (!), accessI, (//), unsafeUpdateA, unsafeWriteA, unsafeWriteManyA
  , replicateA, replicateMA, generateA, generateMA, unfoldrNA, sizeA
  , foldrA, foldrA', foldlA', ifoldrA, ifoldrA', ifoldlA', foldMA', ifoldMA'
  , mapA, imapA, imapMA_, safeSetA, unsafeSetA
  , minIndexA, minLastIndexA, minIndexesA, maxIndexA, maxLastIndexA, forceA
  , fromListA, toListA
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , toUnboxRep
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.Monad.ST.Strict
import           Data.Binary
import           Data.Vector.Binary ()
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM

import Game.LambdaHack.Common.Point

class ( Ord c, Eq (UnboxRep c), Ord (UnboxRep c), Bounded (UnboxRep c)
      , Binary (UnboxRep c), U.Unbox (UnboxRep c) )
      => UnboxRepClass c where
  type UnboxRep c
  type instance UnboxRep c = c
  toUnboxRepUnsafe :: c -> UnboxRep c  -- has to be total
  fromUnboxRep :: UnboxRep c -> c  -- has to be total

instance UnboxRepClass Bool where
  toUnboxRepUnsafe c = c
  fromUnboxRep c = c

instance UnboxRepClass Word8 where
  toUnboxRepUnsafe c = c
  fromUnboxRep c = c

-- | Arrays indexed by @Point@.
data Array c = Array
  { axsize  :: X
  , aysize  :: Y
  , avector :: U.Vector (UnboxRep c)
  }

deriving instance UnboxRepClass c => Eq (Array c)

instance Show (Array c) where
  show a = "PointArray.Array with size " ++ show (axsize a, aysize a)

instance UnboxRepClass c => Binary (Array c) where
  put Array{..} = do
    put axsize
    put aysize
    put avector
  get = do
    axsize <- get
    aysize <- get
    avector <- get
    return $! Array{..}

toUnboxRep :: UnboxRepClass c => c -> UnboxRep c
{-# INLINE toUnboxRep #-}
toUnboxRep c =
#ifdef WITH_EXPENSIVE_ASSERTIONS
  assert (c <= fromUnboxRep maxBound) $
#endif
    toUnboxRepUnsafe c

-- Note that @Ord@ on @Int@ is not monotonic wrt @Ord@ on @Point@.
-- We need to keep it that way, because we want close xs to have close indexes.
pindex :: X -> Point -> Int
{-# INLINE pindex #-}
pindex xsize (Point x y) = x + y * xsize

punindex :: X -> Int -> Point
{-# INLINE punindex #-}
punindex xsize n = let (y, x) = n `quotRem` xsize
                   in Point x y

-- Note: there's no point specializing this to @Point@ arguments,
-- since the extra few additions in @fromPoint@ may be less expensive than
-- memory or register allocations needed for the extra @Int@ in @Point@.
-- | Array lookup.
(!) :: UnboxRepClass c => Array c -> Point -> c
{-# INLINE (!) #-}
(!) Array{..} p = fromUnboxRep $ avector U.! pindex axsize p

accessI :: UnboxRepClass c => Array c -> Int -> UnboxRep c
{-# INLINE accessI #-}
accessI Array{..} p = avector `U.unsafeIndex` p

-- | Construct an array updated with the association list.
(//) :: UnboxRepClass c => Array c -> [(Point, c)] -> Array c
{-# INLINE (//) #-}
(//) Array{..} l = let v = avector U.// map (pindex axsize *** toUnboxRep) l
                   in Array{avector = v, ..}

unsafeUpdateA :: UnboxRepClass c => Array c -> [(Point, c)] -> ()
{-# INLINE unsafeUpdateA #-}
unsafeUpdateA Array{..} l = runST $ do
  vThawed <- U.unsafeThaw avector
  mapM_ (\(p, c) -> VM.write vThawed (pindex axsize p) (toUnboxRep c)) l
  void $ U.unsafeFreeze vThawed

unsafeWriteA :: UnboxRepClass c => Array c -> Point -> c -> ()
{-# INLINE unsafeWriteA #-}
unsafeWriteA Array{..} p c = runST $ do
  vThawed <- U.unsafeThaw avector
  VM.write vThawed (pindex axsize p) (toUnboxRep c)
  void $ U.unsafeFreeze vThawed

unsafeWriteManyA :: UnboxRepClass c => Array c -> [Point] -> c -> ()
{-# INLINE unsafeWriteManyA #-}
unsafeWriteManyA Array{..} l c = runST $ do
  vThawed <- U.unsafeThaw avector
  let d = toUnboxRep c
  mapM_ (\p -> VM.write vThawed (pindex axsize p) d) l
  void $ U.unsafeFreeze vThawed

-- | Create an array from a replicated element.
replicateA :: UnboxRepClass c => X -> Y -> c -> Array c
{-# INLINE replicateA #-}
replicateA axsize aysize c =
  Array{avector = U.replicate (axsize * aysize) $ toUnboxRep c, ..}

-- | Create an array from a replicated monadic action.
replicateMA :: (Monad m, UnboxRepClass c) => X -> Y -> m c -> m (Array c)
{-# INLINE replicateMA #-}
replicateMA axsize aysize m = do
  v <- U.replicateM (axsize * aysize) $ liftM toUnboxRep m
  return $! Array{avector = v, ..}

-- | Create an array from a function.
generateA :: UnboxRepClass c => X -> Y -> (Point -> c) -> Array c
{-# INLINE generateA #-}
generateA axsize aysize f =
  let g n = toUnboxRep $ f $ punindex axsize n
  in Array{avector = U.generate (axsize * aysize) g, ..}

-- | Create an array from a monadic function.
generateMA :: (Monad m, UnboxRepClass c)
           => X -> Y -> (Point -> m c) -> m (Array c)
{-# INLINE generateMA #-}
generateMA axsize aysize fm = do
  let gm n = liftM toUnboxRep $ fm $ punindex axsize n
  v <- U.generateM (axsize * aysize) gm
  return $! Array{avector = v, ..}

unfoldrNA :: UnboxRepClass c => X -> Y -> (b -> (c, b)) -> b -> Array c
{-# INLINE unfoldrNA #-}
unfoldrNA axsize aysize fm b =
  let gm = Just . first toUnboxRep . fm
      v = U.unfoldrN (axsize * aysize) gm b
  in Array {avector = v, ..}

-- | Content identifiers array size.
sizeA :: Array c -> (X, Y)
{-# INLINE sizeA #-}
sizeA Array{..} = (axsize, aysize)

-- | Fold right over an array.
foldrA :: UnboxRepClass c => (c -> a -> a) -> a -> Array c -> a
{-# INLINE foldrA #-}
foldrA f z0 Array{..} =
  U.foldr (\c a-> f (fromUnboxRep c) a) z0 avector

-- | Fold right strictly over an array.
foldrA' :: UnboxRepClass c => (c -> a -> a) -> a -> Array c -> a
{-# INLINE foldrA' #-}
foldrA' f z0 Array{..} =
  U.foldr' (\c a-> f (fromUnboxRep c) a) z0 avector

-- | Fold left strictly over an array.
foldlA' :: UnboxRepClass c => (a -> c -> a) -> a -> Array c -> a
{-# INLINE foldlA' #-}
foldlA' f z0 Array{..} =
  U.foldl' (\a c -> f a (fromUnboxRep c)) z0 avector

-- | Fold left strictly over an array
-- (function applied to each element and its index).
ifoldlA' :: UnboxRepClass c => (a -> Point -> c -> a) -> a -> Array c -> a
{-# INLINE ifoldlA' #-}
ifoldlA' f z0 Array{..} =
  U.ifoldl' (\a n c -> f a (punindex axsize n) (fromUnboxRep c)) z0 avector

-- | Fold right over an array
-- (function applied to each element and its index).
ifoldrA :: UnboxRepClass c => (Point -> c -> a -> a) -> a -> Array c -> a
{-# INLINE ifoldrA #-}
ifoldrA f z0 Array{..} =
  U.ifoldr (\n c a -> f (punindex axsize n) (fromUnboxRep c) a) z0 avector

-- | Fold right strictly over an array
-- (function applied to each element and its index).
ifoldrA' :: UnboxRepClass c => (Point -> c -> a -> a) -> a -> Array c -> a
{-# INLINE ifoldrA' #-}
ifoldrA' f z0 Array{..} =
  U.ifoldr' (\n c a -> f (punindex axsize n) (fromUnboxRep c) a) z0 avector

-- | Fold monadically strictly over an array.
foldMA' :: (Monad m, UnboxRepClass c) => (a -> c -> m a) -> a -> Array c -> m a
{-# INLINE foldMA' #-}
foldMA' f z0 Array{..} =
  U.foldM' (\a c -> f a (fromUnboxRep c)) z0 avector

-- | Fold monadically strictly over an array
-- (function applied to each element and its index).
ifoldMA' :: (Monad m, UnboxRepClass c)
         => (a -> Point -> c -> m a) -> a -> Array c -> m a
{-# INLINE ifoldMA' #-}
ifoldMA' f z0 Array{..} =
  U.ifoldM' (\a n c -> f a (punindex axsize n) (fromUnboxRep c)) z0 avector

-- | Map over an array.
mapA :: (UnboxRepClass c, UnboxRepClass d) => (c -> d) -> Array c -> Array d
{-# INLINE mapA #-}
mapA f Array{..} =
  Array{avector = U.map (toUnboxRep . f . fromUnboxRep) avector, ..}

-- | Map over an array (function applied to each element and its index).
imapA :: (UnboxRepClass c, UnboxRepClass d)
      =>  (Point -> c -> d) -> Array c -> Array d
{-# INLINE imapA #-}
imapA f Array{..} =
  let v = U.imap (\n c ->
                   toUnboxRep $ f (punindex axsize n) (fromUnboxRep c)) avector
  in Array{avector = v, ..}

-- | Map monadically over an array (function applied to each element
-- and its index) and ignore the results.
imapMA_ :: (Monad m, UnboxRepClass c) => (Point -> c -> m ()) -> Array c -> m ()
{-# INLINE imapMA_ #-}
imapMA_ f Array{..} =
  U.imapM_ (\n c -> f (punindex axsize n) (fromUnboxRep c)) avector

-- | Set all elements to the given value, in place.
unsafeSetA :: UnboxRepClass c => c -> Array c -> Array c
{-# INLINE unsafeSetA #-}
unsafeSetA c Array{..} = runST $ do
  vThawed <- U.unsafeThaw avector
  VM.set vThawed (toUnboxRep c)
  vFrozen <- U.unsafeFreeze vThawed
  return $! Array{avector = vFrozen, ..}

-- | Set all elements to the given value, in place, if possible.
safeSetA :: UnboxRepClass c => c -> Array c -> Array c
{-# INLINE safeSetA #-}
safeSetA c Array{..} =
  Array{avector = U.modify (\v -> VM.set v (toUnboxRep c)) avector, ..}

-- | Yield the point coordinates of a minimum element of the array.
-- The array may not be empty.
minIndexA :: UnboxRepClass c => Array c -> Point
{-# INLINE minIndexA #-}
minIndexA Array{..} = punindex axsize $ U.minIndex avector

-- | Yield the point coordinates of the last minimum element of the array.
-- The array may not be empty.
minLastIndexA :: UnboxRepClass c => Array c -> Point
{-# INLINE minLastIndexA #-}
minLastIndexA Array{..} =
  punindex axsize
  $ fst . Bundle.foldl1' imin . Bundle.indexed . G.stream
  $ avector
 where
  imin (i, x) (j, y) = i `seq` j `seq` if x >= y then (j, y) else (i, x)

-- | Yield the point coordinates of all the minimum elements of the array.
-- The array may not be empty.
minIndexesA :: UnboxRepClass c => Array c -> [Point]
{-# INLINE minIndexesA #-}
minIndexesA Array{..} =
  map (punindex axsize)
  $ Bundle.foldr imin [] . Bundle.indexed . G.stream
  $ avector
 where
  imin (i, x) acc = i `seq` if x == minE then i : acc else acc
  minE = U.minimum avector

-- | Yield the point coordinates of the first maximum element of the array.
-- The array may not be empty.
maxIndexA :: UnboxRepClass c => Array c -> Point
{-# INLINE maxIndexA #-}
maxIndexA Array{..} = punindex axsize $ U.maxIndex avector

-- | Yield the point coordinates of the last maximum element of the array.
-- The array may not be empty.
maxLastIndexA :: UnboxRepClass c => Array c -> Point
{-# INLINE maxLastIndexA #-}
maxLastIndexA Array{..} =
  punindex axsize
  $ fst . Bundle.foldl1' imax . Bundle.indexed . G.stream
  $ avector
 where
  imax (i, x) (j, y) = i `seq` j `seq` if x <= y then (j, y) else (i, x)

-- | Force the array not to retain any extra memory.
forceA :: UnboxRepClass c => Array c -> Array c
{-# INLINE forceA #-}
forceA Array{..} = Array{avector = U.force avector, ..}

fromListA :: UnboxRepClass c => X -> Y -> [c] -> Array c
{-# INLINE fromListA #-}
fromListA axsize aysize l =
  Array{avector = U.fromListN (axsize * aysize) $ map toUnboxRep l, ..}

toListA :: UnboxRepClass c => Array c -> [c]
{-# INLINE toListA #-}
toListA Array{..} = map fromUnboxRep $ U.toList avector
