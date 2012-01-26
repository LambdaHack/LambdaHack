-- | AI strategies to direct actors not controlled by the player.
-- No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Strategy where

import Control.Monad

import Game.LambdaHack.Utils.Frequency

-- Monster strategies

-- | A strategy is a choice of frequency tables.
newtype Strategy a = Strategy { runStrategy :: [Frequency a] }
  deriving Show

-- | Strategy is a monad. TODO: Can we write this as a monad transformer?
instance Monad Strategy where
  return x = Strategy $ return $ uniformFreq [x]
  m >>= f  = Strategy $
               filter (not . nullFreq)
               [ toFreq [ (p * q, b)
                        | (p, a) <- runFrequency x
                        , y <- runStrategy (f a)
                        , (q, b) <- runFrequency y
                        ]
               | x <- runStrategy m ]

liftFrequency :: Frequency a -> Strategy a
liftFrequency f =
  Strategy $ filter (not . nullFreq) $ return f

instance MonadPlus Strategy where
  mzero = Strategy []
  mplus (Strategy xs) (Strategy ys) = Strategy (xs ++ ys)

infixr 2 .|

(.|) :: Strategy a -> Strategy a -> Strategy a
(.|) = mplus

reject :: Strategy a
reject = mzero

infix 3 .=>

(.=>) :: Bool -> Strategy a -> Strategy a
p .=> m | p          =  m
        | otherwise  =  mzero

only :: (a -> Bool) -> Strategy a -> Strategy a
only p s = do
  x <- s
  p x .=> return x
