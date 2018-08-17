{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TypeFamilies #-}
-- | A game requires the engine provided by the library, perhaps customized,
-- and game content, defined completely afresh for the particular game.
-- The possible kinds of content are fixed in the library and all defined
-- within the library source code directory. On the other hand, game content,
-- is defined in the directory hosting the particular game definition.
--
-- Content of a given kind is just a list of content items.
-- After the list is verified and the data preprocessed, it's held
-- in the @ContentData@ datatype.
module Game.LambdaHack.Common.ContentData
  ( ContentId(ContentId), ContentData, Freqs, Rarity
  , contentIdIndex, validateRarity, validFreqs
  , emptyContentData, makeContentData
  , okind, omemberGroup, oisSingletonGroup, ouniqGroup, opick
  , ofoldrWithKey, ofoldlWithKey', ofoldlGroup', omapVector, oimapVector
  , olength, linearInterpolation
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.DeepSeq
import           Data.Binary
import           Data.Function
import           Data.Hashable (Hashable)
import qualified Data.Map.Strict as M
import           Data.Ord
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           Game.LambdaHack.Common.Frequency
import           Game.LambdaHack.Common.Misc
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Random

-- | Content identifiers for the content type @c@.
newtype ContentId c = ContentId Word16
  deriving (Show, Eq, Ord, Enum, Binary, Generic)

instance PointArray.UnboxRepClass (ContentId k) where
  type UnboxRep (ContentId k) = Word16
  toUnboxRepUnsafe (ContentId k) = k
  fromUnboxRep = ContentId

instance NFData (ContentId c)

instance Hashable (ContentId c)

-- | Verified and preprocessed content data of a particular kind.
data ContentData c = ContentData
  { contentVector :: V.Vector c
  , groupFreq     :: M.Map (GroupName c) [(Int, (ContentId c, c))]
  }
  deriving Generic

instance NFData c => NFData (ContentData c)

-- | For each group that the kind belongs to, denoted by a @GroupName@
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs a = [(GroupName a, Int)]

-- | Rarity on given depths.
type Rarity = [(Double, Int)]

maxContentId :: ContentId k
maxContentId = ContentId maxBound

contentIdIndex :: ContentId k -> Int
{-# INLINE contentIdIndex #-}
contentIdIndex (ContentId k) = fromEnum k

validateRarity :: Rarity -> [Text]
validateRarity rarity =
  let sortedRarity = sortBy (comparing fst) rarity
  in [ "rarity not sorted" | sortedRarity /= rarity ]
     ++ [ "rarity depth thresholds not unique"
        | nubBy ((==) `on` fst) sortedRarity /= sortedRarity ]
     ++ [ "rarity depth not between 0 and 10"
        | case (sortedRarity, reverse sortedRarity) of
            ((lowest, _) : _, (highest, _) : _) ->
              lowest <= 0 || highest > 10
            _ -> False ]

validFreqs :: Freqs a -> Bool
validFreqs freqs = all ((> 0) . snd) freqs
                   && let groups = sort $ map fst freqs
                      in all (uncurry (/=)) $ zip groups ("" : groups)
                           -- this also catches empty group names

emptyContentData :: ContentData a
emptyContentData = ContentData V.empty M.empty

makeContentData :: (NFData c, Show c)
                => String
                -> (c -> Text)
                     -- ^ name of the content itme, used for validation
                -> (c -> Freqs c)
                     -- ^ frequency in groups, for validation and preprocessing
                -> (c -> [Text])
                     -- ^ validate a content item and list all offences
                -> ([c] -> ContentData c -> [Text])
                     -- ^ validate the whole defined content of this type
                     -- and list all offence
                -> [c]  -- ^ all content of this type
                -> ContentData c
{-# INLINE makeContentData #-}
makeContentData contentName getName getFreq validateSingle validateAll content =
  let contentVector = V.fromList content
      groupFreq =
        let tuples = [ (cgroup, (n, (i, k)))
                     | (i, k) <- zip (map ContentId [0..]) content
                     , (cgroup, n) <- getFreq k
                     , n > 0 ]
            f m (cgroup, nik) = M.insertWith (++) cgroup [nik] m
        in foldl' f M.empty tuples
      cd = ContentData {..}
      -- Catch all kinds of errors in content ASAP, even in unused items.
      contentData = deepseq cd cd
      singleOffenders = [ (offences, a)
                        | a <- content
                        , let offences = validateSingle a
                                         ++ if T.null (getName a)
                                            then ["empty name"]
                                            else []
                        , not (null offences) ]
      allOffences = validateAll content contentData
      freqsOffenders = filter (not . validFreqs . getFreq) content
  in assert (null freqsOffenders
             `blame` contentName ++ ": some Freqs values not valid"
             `swith` freqsOffenders) $
     assert (null singleOffenders
             `blame` contentName ++ ": some content items not valid"
             `swith` singleOffenders) $
     assert (null allOffences
             `blame` contentName ++ ": the content set is not valid"
             `swith` allOffences) $
     assert (V.length contentVector <= contentIdIndex maxContentId
             `blame` contentName ++ ": the content has too many elements")
     contentData

-- | Content element at given id.
okind :: ContentData a -> ContentId a -> a
{-# INLINE okind #-}
okind ContentData{contentVector} !i = contentVector V.! contentIdIndex i

omemberGroup :: ContentData a -> GroupName a -> Bool
omemberGroup ContentData{groupFreq} cgroup = cgroup `M.member` groupFreq

oisSingletonGroup :: ContentData a -> GroupName a -> Bool
oisSingletonGroup ContentData{groupFreq} cgroup =
  case M.lookup cgroup groupFreq of
    Just [_] -> True
    _ -> False

-- | The id of the unique member of a singleton content group.
ouniqGroup :: Show a => ContentData a -> GroupName a -> ContentId a
ouniqGroup ContentData{groupFreq} !cgroup =
  let freq = let assFail = error $ "no unique group"
                                   `showFailure` (cgroup, groupFreq)
             in M.findWithDefault assFail cgroup groupFreq
  in case freq of
    [(n, (i, _))] | n > 0 -> i
    l -> error $ "not unique" `showFailure` (cgroup, l)

-- | Pick a random id belonging to a group and satisfying a predicate.
opick :: Show a
      => ContentData a
      -> GroupName a -> (a -> Bool) -> Rnd (Maybe (ContentId a))
opick ContentData{groupFreq} !cgroup !p =
  case M.lookup cgroup groupFreq of
    Just freqRaw ->
      let freq = toFreq ("opick ('" <> tshow cgroup <> "')")
                 $ filter (p . snd . snd) freqRaw
      in if nullFreq freq
         then return Nothing
         else Just . fst <$> frequency freq
    _ -> return Nothing

-- | Fold over all content elements of @a@.
ofoldrWithKey :: ContentData a -> (ContentId a -> a -> b -> b) -> b -> b
ofoldrWithKey ContentData{contentVector} f z =
  V.ifoldr (\i c a -> f (ContentId $ toEnum i) c a) z contentVector

-- | Fold strictly over all content @a@.
ofoldlWithKey' :: ContentData a -> (b -> ContentId a -> a -> b) -> b -> b
ofoldlWithKey' ContentData{contentVector} f z =
  V.ifoldl' (\a i c -> f a (ContentId $ toEnum i) c) z contentVector

-- | Fold over the given group only.
ofoldlGroup' :: ContentData a
             -> GroupName a
             -> (b -> Int -> ContentId a -> a -> b) -> b -> b
ofoldlGroup' ContentData{groupFreq} cgroup f z =
  case M.lookup cgroup groupFreq of
    Just freq -> foldl' (\acc (p, (i, a)) -> f acc p i a) z freq
    _ -> error $ "no group '" ++ show cgroup
                              ++ "' among content that has groups "
                              ++ show (M.keys groupFreq)
                 `showFailure` ()

omapVector :: ContentData a -> (a -> b) -> V.Vector b
omapVector d f = V.map f $ contentVector d

oimapVector :: ContentData a -> (ContentId a -> a -> b) -> V.Vector b
oimapVector d f = V.imap (\i a -> f (ContentId $ toEnum i) a) $ contentVector d

-- | Size of content @a@.
olength :: ContentData a -> Int
olength ContentData{contentVector} = V.length contentVector

-- We assume @dataset@ is sorted and between 0 and 10.
linearInterpolation :: Int -> Int -> Rarity -> Int
linearInterpolation !levelDepth !totalDepth !dataset =
  let findInterval :: (Double, Int) -> Rarity -> ((Double, Int), (Double, Int))
      findInterval x1y1 [] = (x1y1, (11, 0))
      findInterval !x1y1 ((!x, !y) : rest) =
        if fromIntegral levelDepth * 10 <= x * fromIntegral totalDepth
        then (x1y1, (x, y))
        else findInterval (x, y) rest
      ((x1, y1), (x2, y2)) = findInterval (0, 0) dataset
  in ceiling
     $ fromIntegral y1
       + fromIntegral (y2 - y1)
         * (fromIntegral levelDepth * 10 - x1 * fromIntegral totalDepth)
         / ((x2 - x1) * fromIntegral totalDepth)
