{-# LANGUAGE DeriveFoldable, DeriveTraversable, TupleSections #-}
-- | AI strategies to direct actors not controlled directly by human players.
-- No operation in this module involves the @State@ type or any of our
-- client/server monads types.
module Game.LambdaHack.Client.AI.Strategy
  ( Strategy, nullStrategy, liftFrequency
  , (.|), reject, (.=>), only, bestVariant, returN, mapStrategyM
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Control.Applicative
import Data.Int (Int32)

import Game.LambdaHack.Core.Frequency

-- | A strategy is a choice of (non-empty) frequency tables
-- of possible actions.
--
-- Currently, the way we use it, the list could have at most one element
-- (we filter out void frequencies early and only ever access the first).
-- except for the argument of @mapStrategyM@, which may even be process
-- to the end of the list, if no earlier strategies can be transformed
-- into non-null ones.
newtype Strategy a = Strategy { runStrategy :: [Frequency a] }
  deriving (Show, Foldable, Traversable)

_maxBound32 :: Integer
_maxBound32 = toInteger (maxBound :: Int32)

instance Monad Strategy where
  m >>= f  = normalizeStrategy $ Strategy
    [ toFreq name [
#ifdef WITH_EXPENSIVE_ASSERTIONS
                    assert (toInteger p * toInteger q <= _maxBound32)
#endif
                    (p * q, b)
                  | (p, a) <- runFrequency x
                  , y <- runStrategy (f a)
                  , (q, b) <- runFrequency y
                  ]
    | x <- runStrategy m
    , let name = "Strategy_bind (" <> nameFrequency x <> ")"]

instance Functor Strategy where
  fmap f (Strategy fs) = Strategy (map (fmap f) fs)

instance Applicative Strategy where
  {-# INLINE pure #-}
  pure x = Strategy $ return $! uniformFreq "Strategy_pure" [x]
  (<*>) = ap

instance MonadPlus Strategy where
  mzero = Strategy []
  mplus (Strategy xs) (Strategy ys) = Strategy (xs ++ ys)

instance Alternative Strategy where
  (<|>) = mplus
  empty = mzero

normalizeStrategy :: Strategy a -> Strategy a
normalizeStrategy (Strategy fs) = Strategy $ filter (not . nullFreq) fs

nullStrategy :: Strategy a -> Bool
nullStrategy strat = null $ runStrategy strat

-- | Strategy where only the actions from the given single frequency table
-- can be picked.
liftFrequency :: Frequency a -> Strategy a
liftFrequency f = normalizeStrategy $ Strategy $ return f

infixr 2 .|

-- | Strategy with the actions from both argument strategies,
-- with original frequencies.
(.|) :: Strategy a -> Strategy a -> Strategy a
(.|) = mplus

-- | Strategy with no actions at all.
reject :: Strategy a
reject = mzero

infix 3 .=>

-- | Conditionally accepted strategy.
(.=>) :: Bool -> Strategy a -> Strategy a
p .=> m | p         = m
        | otherwise = mzero

-- | Strategy with all actions not satisfying the predicate removed.
-- The remaining actions keep their original relative frequency values.
only :: (a -> Bool) -> Strategy a -> Strategy a
only p s = normalizeStrategy $ do
  x <- s
  p x .=> return x

-- | When better choices are towards the start of the list,
-- this is the best frequency of the strategy.
bestVariant :: Strategy a -> Frequency a
bestVariant (Strategy []) = mzero
bestVariant (Strategy (f : _)) = f

-- | Like 'return', but pick a name of the single frequency.
returN :: Text -> a -> Strategy a
returN name x = Strategy $ return $! uniformFreq name [x]

mapStrategyM :: Monad m => (a -> m (Maybe b)) -> Strategy a -> m (Strategy b)
mapStrategyM f s = do
  let mapFreq freq = do
        let g (k, a) = do
              mb <- f a
              return $! (k,) <$> mb
        lbm <- mapM g $ runFrequency freq
        return $! toFreq "mapStrategyM" $ catMaybes lbm
      ls = runStrategy s
  lt <- mapM mapFreq ls
  return $! normalizeStrategy $ Strategy lt
