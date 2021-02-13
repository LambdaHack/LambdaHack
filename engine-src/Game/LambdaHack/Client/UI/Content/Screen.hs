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

import qualified Data.ByteString as BS
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import Game.LambdaHack.Definition.Defs

-- | Screen layout and features definition.
data ScreenContent = ScreenContent
  { rwidth        :: X         -- ^ screen width
  , rheight       :: Y         -- ^ screen height
  , rwrap         :: X         -- ^ wrap messages after this number of columns
  , rwebAddress   :: String    -- ^ an extra blurb line for the main menu
  , rintroScreen  :: ([String], [[String]])
                               -- ^ the intro screen (first help screen) text
                               --   and the rest of the manual
  , rapplyVerbMap :: EM.EnumMap Char T.Text
                               -- ^ verbs to use for apply actions
  , rFontFiles    :: [(FilePath, BS.ByteString)]
                               -- ^ embedded game-supplied font files
  }

-- | Catch invalid rule kind definitions.
validateSingle :: ScreenContent -> [Text]
validateSingle ScreenContent{rwebAddress, rintroScreen} =
  (let tsGt80 = filter ((> 80) . T.length) $ map T.pack [rwebAddress]
   in case tsGt80 of
      [] -> []
      tGt80 : _ -> ["rwebAddress's length is over 80:" <> tGt80])
  ++ (let tsGt41 = filter ((> 41) . T.length) $ map T.pack $ fst rintroScreen
      in case tsGt41 of
         [] -> []
         tGt41 : _ -> ["intro screen has a line with length over 41:" <> tGt41])
  ++ (let tsGt80 = filter ((> 80) . T.length) $ map T.pack $ intercalate [""]
                   $ snd rintroScreen
      in case tsGt80 of
         [] -> []
         tGt80 : _ -> ["manual has a line with length over 80:" <> tGt80])

makeData :: ScreenContent -> ScreenContent
makeData sc =
  let singleOffenders = validateSingle sc
  in assert (null singleOffenders
             `blame` "Screen Content" ++ ": some content items not valid"
             `swith` singleOffenders)
     sc
