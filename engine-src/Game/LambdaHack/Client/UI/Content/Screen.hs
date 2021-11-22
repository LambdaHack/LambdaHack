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

import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.RuleKind as RK
import           Game.LambdaHack.Definition.Defs

-- | Screen layout and features definition.
data ScreenContent = ScreenContent
  { rwidth        :: X         -- ^ screen width
  , rheight       :: Y         -- ^ screen height
  , rwebAddress   :: String    -- ^ an extra blurb line for the main menu
  , rintroScreen  :: ([String], [[String]])
                               -- ^ the intro screen (first help screen) text
                               --   and the rest of the manual
  , rapplyVerbMap :: EM.EnumMap (ContentSymbol ItemKind) T.Text
                               -- ^ verbs to use for apply actions
  , rFontFiles    :: [(FilePath, BS.ByteString)]
                               -- ^ embedded game-supplied font files
  }

-- | Catch invalid rule kind definitions.
validateSingle :: RK.RuleContent -> ScreenContent -> [Text]
validateSingle corule ScreenContent{..} =
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
  -- The following reflect the only current UI implementation.
  ++ [ "rwidth /= RK.rWidthMax" | rwidth /= RK.rWidthMax corule ]
  ++ [ "rheight /= RK.rHeightMax + 3" | rheight /= RK.rHeightMax corule + 3]

makeData :: RK.RuleContent -> ScreenContent -> ScreenContent
makeData corule sc =
  let singleOffenders = validateSingle corule sc
  in assert (null singleOffenders
             `blame` "Screen Content" ++ ": some content items not valid"
             `swith` singleOffenders)
     sc
