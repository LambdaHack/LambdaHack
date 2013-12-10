{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}
-- | A list of items with relative frequencies of appearance.
module Game.LambdaHack.Utils.Frequency
  ( -- * The @Frequency@ type
    Frequency
    -- * Construction
  , uniformFreq, toFreq
    -- * Transformation
  , scaleFreq, renameFreq, setFreq
    -- * Consumption
  , rollFreq, nullFreq, runFrequency, nameFrequency
  ) where

import Control.Arrow (first, second)
import Control.Monad
import Data.Binary
import Data.Foldable (Foldable)
import Data.Text (Text)
import Data.Traversable (Traversable)
import qualified System.Random as R

import Game.LambdaHack.Common.Msg
import Control.Exception.Assert.Sugar

-- TODO: do not expose runFrequency
-- | The frequency distribution type.
data Frequency a = Frequency
  { nameFrequency :: !Text        -- ^ short description for debug, etc.
  , runFrequency  :: ![(Int, a)]  -- ^ give acces to raw frequency values
  }
  deriving (Show, Eq, Foldable, Traversable)

instance Monad Frequency where
  return x = Frequency "return" [(1, x)]
  Frequency name xs >>= f =
    Frequency ("bind (" <> name <> ")")
              [(p * q, y) | (p, x) <- xs
                          , (q, y) <- runFrequency (f x) ]

instance MonadPlus Frequency where
  mplus (Frequency xname xs) (Frequency yname ys) =
    let name = case (xs, ys) of
          ([], []) -> "[]"
          ([], _ ) -> yname
          (_,  []) -> xname
          _ -> "(" <> xname <> ") ++ (" <> yname <> ")"
    in Frequency name (xs ++ ys)
  mzero = Frequency "[]" []

instance Functor Frequency where
  fmap f (Frequency name xs) = Frequency name (map (second f) xs)

-- | Uniform discrete frequency distribution.
uniformFreq :: Text -> [a] -> Frequency a
uniformFreq name = Frequency name . map (\ x -> (1, x))

-- | Takes a name and a list of frequencies and items
-- into the frequency distribution.
toFreq :: Text -> [(Int, a)] -> Frequency a
toFreq = Frequency

-- | Scale frequecy distribution, multiplying it
-- by a positive integer constant.
scaleFreq :: Show a => Int -> Frequency a -> Frequency a
scaleFreq n (Frequency name xs) =
  assert (n > 0 `blame` "non-positive frequency scale" `twith` (name, n, xs)) $
  Frequency name (map (first (* n)) xs)

-- | Change the description of the frequency.
renameFreq :: Text -> Frequency a -> Frequency a
renameFreq newName fr = fr {nameFrequency = newName}

-- | Set frequency of an element.
setFreq :: Eq a => Frequency a -> a -> Int -> Frequency a
setFreq (Frequency name xs) x n =
  let f (_, y) | y == x = (n, x)
      f my = my
  in Frequency name $ map f xs

-- | Randomly choose an item according to the distribution.
rollFreq :: Show a => Frequency a -> R.StdGen -> (a, R.StdGen)
rollFreq (Frequency name []) _ =
  assert `failure` "choice from an empty frequency" `twith` name
rollFreq (Frequency name [(n, x)]) _ | n <= 0 =
  assert `failure` "singleton void frequency" `twith` (name, n, x)
rollFreq (Frequency _ [(_, x)]) g = (x, g)  -- speedup
rollFreq (Frequency name fs) g =
  assert (sumf > 0 `blame` "frequency with nothing to pick" `twith` (name, fs))
    (frec r fs, ng)
 where
  sumf = sum (map fst fs)
  (r, ng) = R.randomR (1, sumf) g
  frec :: Int -> [(Int, a)] -> a
  frec m []                     = assert `failure` "impossible roll"
                                         `twith` (name, fs, m)
  frec m ((n, x) : _)  | m <= n = x
  frec m ((n, _) : xs)          = frec (m - n) xs

-- | Test if the frequency distribution is empty.
nullFreq :: Frequency a -> Bool
nullFreq (Frequency _ fs) = all (== 0) $ map fst fs

instance Binary a => Binary (Frequency a) where
  put Frequency{..} = do
    put nameFrequency
    put runFrequency
  get = do
    nameFrequency <- get
    runFrequency <- get
    return Frequency{..}
