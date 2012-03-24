-- | Representation of probabilities and random computations.
module Game.LambdaHack.Random
  ( -- * The @Rng@ monad
    Rnd
    -- * Random operations
  , randomR, oneOf, frequency, roll
    -- * Rolling dice
  , RollDice(..), rollDice, maxDice, minDice, meanDice
    -- * Rolling 2D coordinates
  , RollDiceXY(..), rollDiceXY
    -- * Rolling dependent on depth
  , RollDeep, rollDeep, chanceDeep, intToDeep, maxDeep
    -- * Fractional chance
  , Chance, chance
  ) where

import qualified Data.Binary as Binary
import Data.Ratio
import qualified System.Random as R
import Control.Monad
import qualified Data.List as L
import qualified Control.Monad.State as MState

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency

-- | The monad of computations with random generator state.
type Rnd a = MState.State R.StdGen a

-- | Get a random object within a range with a uniform distribution.
randomR :: (R.Random a) => (a, a) -> Rnd a
randomR range = MState.state $ R.randomR range

-- | Get any element of a list with equal probability.
oneOf :: [a] -> Rnd a
oneOf xs = do
  r <- randomR (0, length xs - 1)
  return (xs !! r)

-- | Gen an element according to a frequency distribution.
frequency :: Show a => Frequency a -> Rnd a
frequency fr = MState.state $ rollFreq fr

-- | Roll a single die.
roll :: Int -> Rnd Int
roll x = if x <= 0 then return 0 else randomR (1, x)

-- | Dice: 1d7, 3d3, 1d0, etc.
-- @RollDice a b@ represents @a@ rolls of @b@-sided die.
data RollDice = RollDice Binary.Word8 Binary.Word8
  deriving (Eq, Ord)

instance Show RollDice where
  show (RollDice a b) = show a ++ "d" ++ show b

instance Read RollDice where
  readsPrec d s =
    let (a, db) = L.break (== 'd') s
        av = read a
    in case db of
      'd' : b -> [ (RollDice av bv, rest) | (bv, rest) <- readsPrec d b ]
      _ -> []

-- | Roll dice and sum the results.
rollDice :: RollDice -> Rnd Int
rollDice (RollDice a' 1)  = return $ fromEnum a'  -- optimization
rollDice (RollDice a' b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in liftM sum (replicateM a (roll b))

-- | Maximal value of dice.
maxDice :: RollDice -> Int
maxDice (RollDice a' b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in a * b

-- | Minimal value of dice.
minDice :: RollDice -> Int
minDice (RollDice a' b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in if b == 0 then 0 else a

-- | Mean value of dice.
meanDice :: RollDice -> Rational
meanDice (RollDice a' b') =
  let (a, b) = (fromIntegral a', fromIntegral b')
  in if b' == 0 then 0 else a * (b + 1) % 2

-- | Dice for rolling a pair of integer parameters pertaining to,
-- respectively, the X and Y cartesian 2D coordinates.
data RollDiceXY = RollDiceXY (RollDice, RollDice)
  deriving Show

-- | Roll the two sets of dice.
rollDiceXY :: RollDiceXY -> Rnd (Int, Int)
rollDiceXY (RollDiceXY (xd, yd)) = do
  x <- rollDice xd
  y <- rollDice yd
  return (x, y)

-- | Dice for parameters scaled with current level depth.
-- To the result of rolling the first set of dice we add the second,
-- scaled in proportion to current depth divided by maximal dungeon depth.
type RollDeep = (RollDice, RollDice)

-- | Roll dice scaled with current level depth.
-- Note that at the first level, the scaled dice are always ignored.
rollDeep :: Int -> Int -> RollDeep -> Rnd Int
rollDeep n depth (d1, d2) =
  assert (n > 0 && n <= depth `blame` (n, depth)) $ do
  r1 <- rollDice d1
  r2 <- rollDice d2
  return $ r1 + ((n - 1) * r2) `div` max 1 (depth - 1)

-- | Roll dice scaled with current level depth and return @True@
-- if the results if greater than 50.
chanceDeep :: Int -> Int -> RollDeep -> Rnd Bool
chanceDeep n depth deep = do
  c <- rollDeep n depth deep
  return $ c > 50

-- | Generate a @RollDeep@ that always gives a constant integer.
intToDeep :: Int -> RollDeep
intToDeep 0  = (RollDice 0 0, RollDice 0 0)
intToDeep n' = let n = toEnum n'
               in if n > maxBound || n < minBound
                  then assert `failure` n'
                  else (RollDice n 1, RollDice 0 0)

-- | Maximal value of scaled dice.
maxDeep :: RollDeep -> Int
maxDeep (d1, d2) = maxDice d1 + maxDice d2

-- | Fractional chance.
type Chance = Rational

-- | Give @True@, with probability determined by the fraction.
chance :: Chance -> Rnd Bool
chance r = do
  let n = numerator r
      d = denominator r
  k <- randomR (1, d)
  return (k <= n)
