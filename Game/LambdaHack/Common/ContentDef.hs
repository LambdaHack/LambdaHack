{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A game requires the engine provided by the library, perhaps customized,
-- and game content, defined completely afresh for the particular game.
-- The general type of the content is @ContentDef@ and it has instances
-- for all content kinds, such as items kinds
-- ("Game.LambdaHack.Content.ItemKind").
--
-- The possible kinds are fixed in the library and all defined within
-- the library source code directory. On the other hand, game content,
-- that is the values whose types are @ContentDef@ instances,
-- are defined in the directory hosting the particular game definition.
module Game.LambdaHack.Common.ContentDef
  ( ContentDef(..), makeContentDef
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Game.LambdaHack.Common.Misc

-- | The general type of a particular game content, e.g., item kinds.
data ContentDef a = ContentDef
  { contentVector :: V.Vector a
  , kindFreq      :: M.Map (GroupName a) [(Int, (ContentId a, a))]
  }

makeContentDef :: Show a
               => (a -> Text)
                    -- ^ name of the content itme, used for validation
               -> (a -> [Text])
                    -- ^ validate a content item and list all offences
               -> ([a] -> [Text])
                    -- ^ validate the whole defined content of this type
                    -- and list all offence
               -> (a -> Freqs a)  -- ^ frequency within groups
               -> [a]  -- ^ all content of this type
               -> ContentDef a
{-# INLINE makeContentDef #-}
makeContentDef getName validateSingle validateAll
               getFreq content =
  let correct a = not (T.null (getName a)) && all ((> 0) . snd) (getFreq a)
      singleOffenders = [ (offences, a)
                        | a <- content
                        , let offences = validateSingle a
                        , not (null offences) ]
      allOffences = validateAll content
      kindFreq =
        let tuples = [ (cgroup, (n, (i, k)))
                     | (i, k) <- zip [ContentId 0..] content
                     , (cgroup, n) <- getFreq k
                     , n > 0 ]
            f m (cgroup, nik) = M.insertWith (++) cgroup [nik] m
        in foldl' f M.empty tuples
      contentVector = V.fromList content
  in assert (allB correct content) $
     assert (null singleOffenders `blame` "some content items not valid"
                                  `swith` singleOffenders) $
     assert (null allOffences `blame` "the content set not valid"
                              `swith` allOffences) $
     assert (V.length contentVector <= fromEnum (maxBound :: ContentId a))
     ContentDef {..}
