{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Hacks that haven't found their home yet.
module Game.LambdaHack.Common.Misc
  ( -- * Game object identifiers
    FactionId, LevelId, ActorId
    -- * Assorted
  , GroupName
  , toGroupName, fromGroupName, makePhrase, makeSentence, squashedWWandW
  , appDataDir, xM, xD, minusM, minusM1, minusM2, oneM, tenthM, show64With2
  , workaroundOnMainThreadMVar
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.Concurrent
import           Control.DeepSeq
import           Data.Binary
import qualified Data.Char as Char
import           Data.Hashable
import           Data.Int (Int64)
import qualified Data.Map as M
import           Data.String (IsString (..))
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU
import           System.Directory (getAppUserDataDirectory)
import           System.Environment (getProgName)
import           System.IO.Unsafe (unsafePerformIO)

-- | A unique identifier of a faction in a game.
newtype FactionId = FactionId Int
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

-- | Abstract level identifiers.
newtype LevelId = LevelId Int
  deriving (Show, Eq, Ord, Hashable, Binary)

instance Enum LevelId where
  fromEnum (LevelId n) = n
  toEnum = LevelId  -- picks the main branch of the dungeon

-- | A unique identifier of an actor in the dungeon.
newtype ActorId = ActorId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- If ever needed, we can use a symbol table here, since content
-- is never serialized. But we'd need to cover the few cases
-- (e.g., @litemFreq@) where @GroupName@ goes into savegame.
newtype GroupName a = GroupName {fromGroupName :: Text}
  deriving (Show, Eq, Ord, Hashable, Binary, Generic)

instance IsString (GroupName a) where
  fromString = GroupName . T.pack

instance NFData (GroupName a)

toGroupName :: Text -> GroupName a
{-# INLINE toGroupName #-}
toGroupName = GroupName

-- | Re-exported English phrase creation functions, applied to default
-- irregular word sets.
makePhrase, makeSentence :: [MU.Part] -> Text
makePhrase = MU.makePhrase irregular
makeSentence = MU.makeSentence irregular

irregular :: MU.Irregular
irregular = ( fst MU.defIrregular
              `M.union`
              M.fromList []  -- miniutter default suffices, for now
            , snd MU.defIrregular )

-- | Apply the @WWandW@ constructor, first representing repetitions
-- as @CardinalWs@.
-- The parts are not sorted, only grouped, to keep the order.
-- The internal structure of speech parts is compared, not their string
-- rendering, so some coincidental clashes are avoided (and code is simpler).
squashedWWandW :: [MU.Part] -> (MU.Part, MU.Person)
squashedWWandW parts =
  let repetitions = group parts
      f [] = error $ "empty group" `showFailure` parts
      f [part] = (part, MU.Sg3rd)  -- avoid prefixing hero names with "a"
      f l@(part : _) = (MU.CardinalWs (length l) part, MU.PlEtc)
      cars = map f repetitions
      person = case cars of
        [] -> error $ "empty cars" `showFailure` parts
        [(_, person1)] -> person1
        _ -> MU.PlEtc
  in (MU.WWandW $ map fst cars, person)

-- | Personal data directory for the game. Depends on the OS and the game,
-- e.g., for LambdaHack under Linux it's @~\/.LambdaHack\/@.
appDataDir :: IO FilePath
appDataDir = do
  progName <- getProgName
  let name = takeWhile Char.isAlphaNum progName
  getAppUserDataDirectory name

xM :: Int -> Int64
xM k = fromIntegral k * 1000000

xD :: Double -> Double
xD k = k * 1000000

minusM, minusM1, minusM2, oneM, tenthM :: Int64
minusM = xM (-1)
minusM1 = xM (-1) - 1
minusM2 = xM (-1) - 2
oneM = xM 1
tenthM = 100000

show64With2 :: Int64 -> Text
show64With2 n =
  let k = 100 * n `div` oneM
      l = k `div` 100
      x = k - l * 100
  in tshow l
     <> if | x == 0 -> ""
           | x < 10 -> ".0" <> tshow x
           | otherwise -> "." <> tshow x

-- Global variable for passing the action to run on main thread, if any.
workaroundOnMainThreadMVar :: MVar (IO ())
{-# NOINLINE workaroundOnMainThreadMVar #-}
workaroundOnMainThreadMVar = unsafePerformIO newEmptyMVar
