-- | Tests for the screenshot names the frontends share
-- (@docs/wasm-frontend-unified-plan.md@, 1.2).
module ScreenshotUnitTests (screenshotUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (TimeOfDay (..), TimeZone (..), timeOfDayToTime, utc)
import Test.Tasty
import Test.Tasty.HUnit

import Game.LambdaHack.Client.UI.Frontend (screenshotName)

screenshotUnitTests :: TestTree
screenshotUnitTests = testGroup "screenshotUnitTests"
  [ testCase "the name is prtscn and the time, cut to 25 characters" $
      screenshotName utc (at 35.123456789)
        @?= "prtscn2026-10-22_13.08.35.12345"
  , testCase "the time is the zone's local time" $
      screenshotName (TimeZone 120 True "CEST") (at 35.123456789)
        @?= "prtscn2026-10-22_15.08.35.12345"
  , testCase "a whole second has no fraction" $
      screenshotName utc (at 35) @?= "prtscn2026-10-22_13.08.35"
  ]
 where
  at seconds =
    UTCTime (fromGregorian 2026 10 22)
            (timeOfDayToTime $ TimeOfDay 13 8 seconds)
