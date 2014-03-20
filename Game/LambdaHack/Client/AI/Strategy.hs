{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}
-- | AI strategies to direct actors not controlled directly by human players.
-- No operation in this module involves the 'State' or 'Action' type.
module Game.LambdaHack.Client.AI.Strategy
  ( Strategy, nullStrategy, liftFrequency
  , (.|), reject, (.=>), only, bestVariant, renameStrategy, returN
  ) where

import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable)
import Data.Text (Text)
import Data.Traversable (Traversable)

import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Frequency as Frequency

-- | A strategy is a choice of (non-empty) frequency tables
-- of possible actions.
newtype Strategy a = Strategy { runStrategy :: [Frequency a] }
  deriving (Show, Foldable, Traversable)

-- | Strategy is a monad. TODO: Can we write this as a monad transformer?
instance Monad Strategy where
  {-# INLINE return #-}
  return x = Strategy $ return $! uniformFreq "Strategy_return" [x]
  m >>= f  = normalizeStrategy $ Strategy
    [ toFreq name [ (p * q, b)
                  | (p, a) <- runFrequency x
                  , y <- runStrategy (f a)
                  , (q, b) <- runFrequency y
                  ]
    | x <- runStrategy m
    , let name = "Strategy_bind (" <> nameFrequency x <> ")"]

instance Functor Strategy where
  fmap f (Strategy fs) = Strategy (map (fmap f) fs)

instance Applicative Strategy where
  pure  = return
  (<*>) = ap

instance MonadPlus Strategy where
  mzero = Strategy []
  {-# INLINE mplus #-}
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

-- | Overwrite the description of all frequencies within the strategy.
renameStrategy :: Text -> Strategy a -> Strategy a
renameStrategy newName (Strategy fs) = Strategy $ map (renameFreq newName) fs

-- | Like 'return', but pick a name of the single frequency.
returN :: Text -> a -> Strategy a
returN name x = Strategy $ return $! uniformFreq name [x]
