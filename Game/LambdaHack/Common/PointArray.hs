{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Arrays, based on Data.Vector.Unboxed, indexed by @Point@.
module Game.LambdaHack.Common.PointArray
  ( Array, (!), (//), replicateA, replicateMA, generateMA, sizeA, foldlA
  ) where

import Control.Monad
import Data.Binary
import qualified Data.Vector as B (Vector)
import Data.Vector.Binary ()
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U (Vector)

import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.PointXY

-- TODO: for now, until there's support for GeneralizedNewtypeDeriving
-- for Unboxed, there's a lot of @Word8@ in place of @c@ here
-- and a contraint @Enum c@ instead of @Unbox c@.

-- TODO: perhaps make them an instance of Data.Vector.Generic?
-- | Arrays indexed by @Point@.
newtype Array c = Array (B.Vector (U.Vector Word8))
  deriving (Eq, Binary)

instance Show (Array c) where
  show a = "PointArray.Array with size " ++ show (sizeA a)

cnv :: (Enum a, Enum b) => a -> b
cnv = toEnum . fromEnum

-- Note: there's no point specializing this to @PointXY@ arguments,
-- since the extra few additions in @fromPoint@ may be less expensive than
-- memory or register allocations needed for the extra @Int@ in @PointXY@.
-- | Array lookup.
(!) :: Enum c => Array c -> Point -> c
{-# INLINE (!) #-}
(!) (Array a) p = let PointXY x y = fromPoint p
                  in cnv $ a V.! y V.! x

-- TODO: optimize, either by replacing with a different operation
-- or by thawing all the vectors, updating, freezing.
-- | Construct an array updated with the association list.
(//) :: Enum c => Array c -> [(Point, c)] -> Array c
(//) (Array a) l =
  let f b (x, e) = b V.// [(x, e)]
  in Array $ V.accum f a [ (y, (x, cnv e))
                         | (p, e) <- l, let PointXY x y = fromPoint p ]

-- | Create an array from a replicated element.
replicateA :: Enum c => X -> Y -> c -> Array c
replicateA x y e =
  let line = V.replicate x $ cnv e
  in Array $ V.replicate y line

-- | Create an  array from a replicated monadic action.
replicateMA :: Enum c => Monad m => X -> Y -> m c -> m (Array c)
replicateMA x y m =
  let line = V.replicateM x (liftM cnv m)
  in liftM Array $ V.replicateM y line

-- | Create an array from a monadic function.
generateMA :: Enum c => Monad m => X -> Y -> (Point -> m c) -> m (Array c)
generateMA x y m =
  let me y1 x1 = liftM cnv $ m $ toPoint $ PointXY x1 y1
      mline y1 = V.generateM x (me y1)
  in liftM Array $ V.generateM y mline

-- | Content identifiers array size.
sizeA :: Array c -> (X, Y)
sizeA (Array a) =
  let y = V.length a
      x = V.length (a V.! 0)
  in (x, y)

-- | Fold left strictly over an array.
foldlA :: Enum c => (a -> c -> a) -> a -> Array c -> a
foldlA f z0 (Array a) = lgo z0 $ concatMap V.toList $ V.toList a
 where lgo z []       = z
       lgo z (x : xs) = let fzx = f z (cnv x) in fzx `seq` lgo fzx xs
