-- | The type of definitions of screen layout and features.
module Game.LambdaHack.Client.UI.Content.Screen
  ( ScreenContent(..), makeData
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import Game.LambdaHack.Definition.Defs

-- | Screen layout and features definition.
data ScreenContent = ScreenContent
  { rwidth          :: X         -- ^ screen width
  , rheight         :: Y         -- ^ screen height
  , rmainMenuArt    :: [String]   -- ^ the ASCII art for the main menu
  , rintroScreen    :: [String]  -- ^ the intro screen (first help screen) text
  , rmoveKeysScreen :: [String]  -- ^ the fixed move key help blurb
  , rapplyVerbMap   :: EM.EnumMap Char T.Text
                                 -- ^ verbs to use for apply actions
  }

-- | Catch invalid rule kind definitions.
validateSingle :: ScreenContent -> [Text]
validateSingle ScreenContent{rmainMenuArt} =
  let tsGt80 = filter ((> 80) . T.length) $ map T.pack rmainMenuArt
  in case tsGt80 of
     [] -> []
     tGt80 : _ ->
       ["rmainMenuArt has a line with length other than 80:" <> tGt80]

makeData :: ScreenContent -> ScreenContent
makeData sc =
  let singleOffenders = validateSingle sc
  in assert (null singleOffenders
             `blame` "Screen Content" ++ ": some content items not valid"
             `swith` singleOffenders)
     sc
