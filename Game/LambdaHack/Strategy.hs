-- | AI strategies to direct actors not controlled by the player.
-- No operation in this module involves the 'State' or 'Action' type.
module Game.LambdaHack.Strategy
  ( Strategy(..), liftFrequency, (.|), reject, (.=>), only
  ) where

import Control.Monad

import Game.LambdaHack.Utils.Frequency

-- | A strategy is a choice of (non-empty) frequency tables
-- of possible actions.
newtype Strategy a = Strategy { runStrategy :: [Frequency a] }
  deriving Show

-- | Strategy is a monad. TODO: Can we write this as a monad transformer?
instance Monad Strategy where
  return x = Strategy $ return $ uniformFreq "Strategy_return" [x]
  m >>= f  = Strategy $
    filter (not . nullFreq)
    [ toFreq "Strategy_bind" [ (p * q, b)
                             | (p, a) <- runFrequency x
                             , y <- runStrategy (f a)
                             , (q, b) <- runFrequency y
                             ]
    | x <- runStrategy m ]

instance MonadPlus Strategy where
  mzero = Strategy []
  mplus (Strategy xs) (Strategy ys) = Strategy (xs ++ ys)

-- | Strategy where only the actions from the given single frequency table
-- can be picked.
liftFrequency :: Frequency a -> Strategy a
liftFrequency f =
  Strategy $ filter (not . nullFreq) $ return f

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
p .=> m | p          =  m
        | otherwise  =  mzero

-- | Strategy with all actions not satisfying the predicate removed.
-- The remaining action keep their original relative frequency values.
only :: (a -> Bool) -> Strategy a -> Strategy a
only p s = do
  x <- s
  p x .=> return x
