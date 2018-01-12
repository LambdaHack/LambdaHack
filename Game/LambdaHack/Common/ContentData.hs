{-# LANGUAGE DeriveGeneric #-}
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
  ( ContentData, emptyContentData, makeContentData
  , okind, omemberGroup, ouniqGroup, opick
  , ofoldrWithKey, ofoldlWithKey', ofoldlGroup', olength
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.DeepSeq
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random

-- | Verified and preprocessed content data of a particular kind.
data ContentData c = ContentData
  { contentVector :: V.Vector c
  , groupFreq     :: M.Map (GroupName c) [(Int, (ContentId c, c))]
  }
  deriving Generic

instance NFData c => NFData (ContentData c)

emptyContentData :: ContentData a
emptyContentData = ContentData V.empty M.empty

makeContentData :: (NFData c, Show c)
                => (c -> Text)
                     -- ^ name of the content itme, used for validation
                -> (c -> Freqs c)
                     -- ^ frequency in groups, for validation and preprocessing
                -> (c -> [Text])
                     -- ^ validate a content item and list all offences
                -> ([c] ->  ContentData c -> [Text])
                     -- ^ validate the whole defined content of this type
                     -- and list all offence
                -> [c]  -- ^ all content of this type
                -> ContentData c
{-# INLINE makeContentData #-}
makeContentData getName getFreq validateSingle validateAll content =
  let contentVector = V.fromList content
      groupFreq =
        let tuples = [ (cgroup, (n, (i, k)))
                     | (i, k) <- zip [ContentId 0..] content
                     , (cgroup, n) <- getFreq k
                     , n > 0 ]
            f m (cgroup, nik) = M.insertWith (++) cgroup [nik] m
        in foldl' f M.empty tuples
      cd = ContentData {..}
      -- Catch all kinds of errors in content ASAP, even in unused items.
      contentData = deepseq cd cd
      correct a = not (T.null (getName a)) && all ((> 0) . snd) (getFreq a)
      incorrectOffenders = filter (not . correct) content
      singleOffenders = [ (offences, a)
                        | a <- content
                        , let offences = validateSingle a
                        , not (null offences) ]
      allOffences = validateAll content contentData
  in assert (null incorrectOffenders `blame` "some content items not correct"
                                     `swith` incorrectOffenders) $
     assert (null singleOffenders `blame` "some content items not valid"
                                  `swith` singleOffenders) $
     assert (null allOffences `blame` "the content set not valid"
                              `swith` allOffences) $
     assert (V.length contentVector <= fromEnum (maxBound :: ContentId a))
     contentData

-- | Content element at given id.
okind :: ContentData a -> ContentId a -> a
okind ContentData{contentVector} !i = contentVector V.! fromEnum i

omemberGroup :: ContentData a -> GroupName a -> Bool
omemberGroup ContentData{groupFreq} cgroup = cgroup `M.member` groupFreq

-- | The id of the unique member of a singleton content group.
ouniqGroup :: Show a => ContentData a -> GroupName a -> ContentId a
ouniqGroup ContentData{groupFreq} !cgroup =
  let freq = let assFail = error $ "no unique group"
                                   `showFailure` (cgroup, groupFreq)
             in M.findWithDefault assFail cgroup groupFreq
  in case freq of
    [(n, (i, _))] | n > 0 -> i
    l -> error $ "not unique" `showFailure` (l, cgroup, groupFreq)

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
         else fmap (Just . fst) $ frequency freq
           {- with monadic notation; may produce empty freq:
           (i, k) <- freq
           breturn (p k) i
           -}
           {- with MonadComprehensions:
           frequency [ i | (i, k) <- groupFreq M.! cgroup, p k ]
           -}
    _ -> return Nothing

-- | Fold over all content elements of @a@.
ofoldrWithKey :: ContentData a -> (ContentId a -> a -> b -> b) -> b -> b
ofoldrWithKey ContentData{contentVector} f z =
  V.ifoldr (\i c a -> f (toEnum i) c a) z contentVector

-- | Fold strictly over all content @a@.
ofoldlWithKey' :: ContentData a -> (b -> ContentId a -> a -> b) -> b -> b
ofoldlWithKey' ContentData{contentVector} f z =
  V.ifoldl' (\a i c -> f a (toEnum i) c) z contentVector

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

-- | Size of content @a@.
olength :: ContentData a -> Int
olength ContentData{contentVector} = V.length contentVector
