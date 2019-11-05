-- | A game requires the engine provided by the library, perhaps customized,
-- and game content, defined completely afresh for the particular game.
-- The possible kinds of content are fixed in the library and all defined
-- within the library source code directory. On the other hand, game content,
-- is defined in the directory hosting the particular game definition.
--
-- Content of a given kind is just a list of content items.
-- After the list is verified and the data preprocessed, it's held
-- in the @ContentData@ datatype.
module Game.LambdaHack.Definition.ContentData
  ( ContentData
  , validateRarity, validFreqs
  , emptyContentData, makeContentData
  , okind, omemberGroup, oisSingletonGroup, ouniqGroup, opick
  , ofoldlWithKey', ofoldlGroup', omapVector, oimapVector, olength
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Game.LambdaHack.Core.Frequency
import Game.LambdaHack.Core.Random
import Game.LambdaHack.Definition.Defs

-- | Verified and preprocessed content data of a particular kind.
data ContentData c = ContentData
  { contentVector :: V.Vector c
  , groupFreq     :: M.Map (GroupName c) [(Int, (ContentId c, c))]
  }

maxContentId :: ContentId k
maxContentId = toContentId maxBound

validateRarity :: Rarity -> [Text]
validateRarity rarity =
  let sortedRarity = sortOn fst rarity
  in [ "rarity not sorted" | sortedRarity /= rarity ]
     ++ [ "rarity depth thresholds not unique"
        | map head (groupBy ((==) `on` fst) sortedRarity) /= sortedRarity ]
     ++ [ "rarity depth not between 0 and 10"
        | case (sortedRarity, reverse sortedRarity) of
            ((lowest, _) : _, (highest, _) : _) ->
              lowest <= 0 || highest > 10
            _ -> False ]

validFreqs :: Freqs a -> Bool
validFreqs freqs =
  all ((> 0) . snd) freqs
  && let groups = sort $ map fst freqs
         tailOfGroups = if null groups then groups else tail groups
     in all (uncurry (/=)) $ zip groups tailOfGroups

emptyContentData :: ContentData a
emptyContentData = ContentData V.empty M.empty

makeContentData :: Show c
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
  -- The @force@ is needed for @GHC.Compact@.
  let contentVector = V.force $ V.fromList content
      groupFreq =
        let tuples = [ (cgroup, (n, (i, k)))
                     | (i, k) <- zip (map toContentId [0..]) content
                     , (cgroup, n) <- getFreq k
                     , n > 0 ]
            f !m (!cgroup, !nik) = M.insertWith (++) cgroup [nik] m
        in foldl' f M.empty tuples
      contentData = ContentData {..}
      singleOffenders = [ (offences, a)
                        | a <- content
                        , let offences = validateSingle a
                                         ++ ["empty name" | T.null (getName a)]
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
      let freq = toFreq "opick" $ filter (p . snd . snd) freqRaw
      in if nullFreq freq
         then return Nothing
         else Just . fst <$> frequency freq
    _ -> return Nothing

-- | Fold strictly over all content @a@.
ofoldlWithKey' :: ContentData a -> (b -> ContentId a -> a -> b) -> b -> b
ofoldlWithKey' ContentData{contentVector} f z =
  V.ifoldl' (\ !a !i !c -> f a (toContentId $ toEnum i) c) z contentVector

-- | Fold over the given group only.
ofoldlGroup' :: ContentData a
             -> GroupName a
             -> (b -> Int -> ContentId a -> a -> b) -> b -> b
ofoldlGroup' ContentData{groupFreq} cgroup f z =
  case M.lookup cgroup groupFreq of
    Just freq -> foldl' (\ !acc (!p, (!i, !a)) -> f acc p i a) z freq
    _ -> error $ "no group '" ++ show cgroup
                              ++ "' among content that has groups "
                              ++ show (M.keys groupFreq)
                 `showFailure` ()

omapVector :: ContentData a -> (a -> b) -> V.Vector b
omapVector d f = V.map f $ contentVector d

oimapVector :: ContentData a -> (ContentId a -> a -> b) -> V.Vector b
oimapVector d f = V.imap (\i a -> f (toContentId $ toEnum i) a) $ contentVector d

-- | Size of content @a@.
olength :: ContentData a -> Int
olength ContentData{contentVector} = V.length contentVector
