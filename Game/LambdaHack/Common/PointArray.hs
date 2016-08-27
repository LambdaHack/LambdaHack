-- | Arrays, based on Data.Vector.Unboxed, indexed by @Point@.
module Game.LambdaHack.Common.PointArray
  ( Array(..), pindex, punindex
  , (!), accessI, (//)
  , replicateA, replicateMA, generateA, generateMA, unfoldrNA, sizeA
  , foldrA, foldlA', ifoldrA, ifoldrA', ifoldlA', mapA, imapA, mapWithKeyMA
  , safeSetA, unsafeSetA, unsafeUpdateA, unsafeWriteA, unsafeWriteManyA
  , minIndexA, minLastIndexA, minIndexesA, maxIndexA, maxLastIndexA, forceA
  ) where

import Prelude ()

import Control.Arrow (first)
import Game.LambdaHack.Common.Prelude

import Control.Arrow ((***))
import Control.Monad.ST.Strict
import Data.Binary
import Data.Binary.Orphans ()
#if MIN_VERSION_vector(0,11,0)
import qualified Data.Vector.Fusion.Bundle as Bundle
#else
import qualified Data.Vector.Fusion.Stream as Bundle
#endif
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM

import Game.LambdaHack.Common.Point

-- TODO: for now, until there's support for GeneralizedNewtypeDeriving
-- for Unboxed, there's a lot of @Word8@ in place of @c@ here
-- and a constraint @Enum c@ instead of @Unbox c@.

-- TODO: perhaps make them an instance of Data.Vector.Generic?
-- | Arrays indexed by @Point@.
data Array c = Array
  { axsize  :: !X
  , aysize  :: !Y
  , avector :: !(U.Vector Word8)
  }
  deriving Eq

instance Show (Array c) where
  show a = "PointArray.Array with size " ++ show (sizeA a)

cnv :: (Enum a, Enum b) => a -> b
{-# INLINE cnv #-}
cnv = toEnum . fromEnum

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
(!) :: Enum c => Array c -> Point -> c
{-# INLINE (!) #-}
(!) Array{..} p = cnv $ avector U.! pindex axsize p

accessI :: Enum c => Array c -> Int -> c
{-# INLINE accessI #-}
accessI Array{..} p = cnv $ avector `U.unsafeIndex` p

-- | Construct an array updated with the association list.
(//) :: Enum c => Array c -> [(Point, c)] -> Array c
{-# INLINE (//) #-}
(//) Array{..} l = let v = avector U.// map (pindex axsize *** cnv) l
                   in Array{avector = v, ..}

unsafeUpdateA :: Enum c => Array c -> [(Point, c)] -> ()
{-# INLINE unsafeUpdateA #-}
unsafeUpdateA Array{..} l = runST $ do
  vThawed <- U.unsafeThaw avector
  mapM_ (\(p, c) -> VM.write vThawed (pindex axsize p) (cnv c)) l
  void $ U.unsafeFreeze vThawed

unsafeWriteA :: Enum c => Array c -> Point -> c -> ()
{-# INLINE unsafeWriteA #-}
unsafeWriteA Array{..} p c = runST $ do
  vThawed <- U.unsafeThaw avector
  VM.write vThawed (pindex axsize p) (cnv c)
  void $ U.unsafeFreeze vThawed

unsafeWriteManyA :: Enum c => Array c -> [Point] -> c -> ()
{-# INLINE unsafeWriteManyA #-}
unsafeWriteManyA Array{..} l c = runST $ do
  vThawed <- U.unsafeThaw avector
  let d = cnv c
  mapM_ (\p -> VM.write vThawed (pindex axsize p) d) l
  void $ U.unsafeFreeze vThawed

-- | Create an array from a replicated element.
replicateA :: Enum c => X -> Y -> c -> Array c
{-# INLINE replicateA #-}
replicateA axsize aysize c =
  Array{avector = U.replicate (axsize * aysize) $ cnv c, ..}

-- | Create an array from a replicated monadic action.
replicateMA :: Enum c => Monad m => X -> Y -> m c -> m (Array c)
{-# INLINE replicateMA #-}
replicateMA axsize aysize m = do
  v <- U.replicateM (axsize * aysize) $ liftM cnv m
  return $! Array{avector = v, ..}

-- | Create an array from a function.
generateA :: Enum c => X -> Y -> (Point -> c) -> Array c
{-# INLINE generateA #-}
generateA axsize aysize f =
  let g n = cnv $ f $ punindex axsize n
  in Array{avector = U.generate (axsize * aysize) g, ..}

-- | Create an array from a monadic function.
generateMA :: Enum c => Monad m => X -> Y -> (Point -> m c) -> m (Array c)
{-# INLINE generateMA #-}
generateMA axsize aysize fm = do
  let gm n = liftM cnv $ fm $ punindex axsize n
  v <- U.generateM (axsize * aysize) gm
  return $! Array{avector = v, ..}

unfoldrNA :: Enum c => X -> Y -> (b -> (c, b)) -> b -> Array c
{-# INLINE unfoldrNA #-}
unfoldrNA axsize aysize fm b =
  let gm = Just . first cnv . fm
      v = U.unfoldrN (axsize * aysize) gm b
  in Array {avector = v, ..}

-- | Content identifiers array size.
sizeA :: Array c -> (X, Y)
{-# INLINE sizeA #-}
sizeA Array{..} = (axsize, aysize)

-- | Fold right over an array.
foldrA :: Enum c => (c -> a -> a) -> a -> Array c -> a
{-# INLINE foldrA #-}
foldrA f z0 Array{..} =
  U.foldr (\c a-> f (cnv c) a) z0 avector

-- | Fold left strictly over an array.
foldlA' :: Enum c => (a -> c -> a) -> a -> Array c -> a
{-# INLINE foldlA' #-}
foldlA' f z0 Array{..} =
  U.foldl' (\a c -> f a (cnv c)) z0 avector

-- | Fold left strictly over an array
-- (function applied to each element and its index).
ifoldlA' :: Enum c => (a -> Point -> c -> a) -> a -> Array c -> a
{-# INLINE ifoldlA' #-}
ifoldlA' f z0 Array{..} =
  U.ifoldl' (\a n c -> f a (punindex axsize n) (cnv c)) z0 avector

-- | Fold right over an array
-- (function applied to each element and its index).
ifoldrA :: Enum c => (Point -> c -> a -> a) -> a -> Array c -> a
{-# INLINE ifoldrA #-}
ifoldrA f z0 Array{..} =
  U.ifoldr (\n c a -> f (punindex axsize n) (cnv c) a) z0 avector

-- | Fold right strictly over an array
-- (function applied to each element and its index).
ifoldrA' :: Enum c => (Point -> c -> a -> a) -> a -> Array c -> a
{-# INLINE ifoldrA' #-}
ifoldrA' f z0 Array{..} =
  U.ifoldr' (\n c a -> f (punindex axsize n) (cnv c) a) z0 avector

-- | Map over an array.
mapA :: (Enum c, Enum d) => (c -> d) -> Array c -> Array d
{-# INLINE mapA #-}
mapA f Array{..} = Array{avector = U.map (cnv . f . cnv) avector, ..}

-- | Map over an array (function applied to each element and its index).
imapA :: (Enum c, Enum d) => (Point -> c -> d) -> Array c -> Array d
{-# INLINE imapA #-}
imapA f Array{..} =
  let v = U.imap (\n c -> cnv $ f (punindex axsize n) (cnv c)) avector
  in Array{avector = v, ..}

-- | Set all elements to the given value, in place.
unsafeSetA :: Enum c => c -> Array c -> Array c
{-# INLINE unsafeSetA #-}
unsafeSetA c Array{..} = runST $ do
  vThawed <- U.unsafeThaw avector
  VM.set vThawed (cnv c)
  vFrozen <- U.unsafeFreeze vThawed
  return $! Array{avector = vFrozen, ..}

-- | Set all elements to the given value, in place, if possible.
safeSetA :: Enum c => c -> Array c -> Array c
{-# INLINE safeSetA #-}
safeSetA c Array{..} =
  Array{avector = U.modify (\v -> VM.set v (cnv c)) avector, ..}

-- | Map monadically over an array (function applied to each element
-- and its index) and ignore the results.
mapWithKeyMA :: Enum c => Monad m
             => (Point -> c -> m ()) -> Array c -> m ()
{-# INLINE mapWithKeyMA #-}
mapWithKeyMA f Array{..} =
  U.imapM_ (\n c -> f (punindex axsize n) (cnv c)) avector

-- | Yield the point coordinates of a minimum element of the array.
-- The array may not be empty.
minIndexA :: Enum c => Array c -> Point
{-# INLINE minIndexA #-}
minIndexA Array{..} = punindex axsize $ U.minIndex avector

-- | Yield the point coordinates of the last minimum element of the array.
-- The array may not be empty.
minLastIndexA :: Enum c => Array c -> Point
{-# INLINE minLastIndexA #-}
minLastIndexA Array{..} =
  punindex axsize
  $ fst . Bundle.foldl1' imin . Bundle.indexed . G.stream
  $ avector
 where
  imin (i, x) (j, y) = i `seq` j `seq` if x >= y then (j, y) else (i, x)

-- | Yield the point coordinates of all the minimum elements of the array.
-- The array may not be empty.
minIndexesA :: Enum c => Array c -> [Point]
{-# INLINE minIndexesA #-}
minIndexesA Array{..} =
  map (punindex axsize)
  $ Bundle.foldr imin [] . Bundle.indexed . G.stream
  $ avector
 where
  imin (i, x) acc = i `seq` if x == minE then i : acc else acc
  minE = cnv $ U.minimum avector

-- | Yield the point coordinates of the first maximum element of the array.
-- The array may not be empty.
maxIndexA :: Enum c => Array c -> Point
{-# INLINE maxIndexA #-}
maxIndexA Array{..} = punindex axsize $ U.maxIndex avector

-- | Yield the point coordinates of the last maximum element of the array.
-- The array may not be empty.
maxLastIndexA :: Enum c => Array c -> Point
{-# INLINE maxLastIndexA #-}
maxLastIndexA Array{..} =
  punindex axsize
  $ fst . Bundle.foldl1' imax . Bundle.indexed . G.stream
  $ avector
 where
  imax (i, x) (j, y) = i `seq` j `seq` if x <= y then (j, y) else (i, x)

-- | Force the array not to retain any extra memory.
forceA :: Enum c => Array c -> Array c
{-# INLINE forceA #-}
forceA Array{..} = Array{avector = U.force avector, ..}

instance Binary (Array c) where
  put Array{..} = do
    put axsize
    put aysize
    put avector
  get = do
    axsize <- get
    aysize <- get
    avector <- get
    return $! Array{..}
