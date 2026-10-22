-- | The names the frontends give the screenshots they save, so that
-- a screenshot is named alike whichever frontend takes it.
module Game.LambdaHack.Client.UI.Frontend.Screenshot
  ( screenshotName
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)

-- | The name of a screenshot taken at the given time, seen in the given
-- time zone, with no directory and no extension, each frontend adding
-- the extension of the format it saves: @prtscn@ followed by the first 25
-- characters of the shown local time, spaces turned into underscores
-- and colons into dots.
screenshotName :: TimeZone -> UTCTime -> String
screenshotName timezone utcTime =
  let unspace = map $ \c -> case c of  -- prevent the need for backquoting
        ' ' -> '_'
        ':' -> '.'
        _ -> c
  in "prtscn" <> unspace (take 25 $ show $ utcToLocalTime timezone utcTime)
