module Main (main) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty

import Game.LambdaHack.Common.Point (setSpeedupHackXSize)

import ActorStateUnitTests
import CellStyleUnitTests
import CommonMUnitTests
import FfiCoverageUnitTests
import FontsUnitTests
import FrameMUnitTests
import FreshnessCase (freshnessOptions)
import FrontendContractUnitTests
import HandleHelperMUnitTests
import HandleHumanGlobalMUnitTests
import HandleHumanLocalMUnitTests
import InputDecisionUnitTests
import InventoryMUnitTests
import ItemDescriptionUnitTests
import ItemKindUnitTests
import ItemRevUnitTests
import LevelUnitTests
import MonadClientUIUnitTests
import OverlayLayoutUnitTests
import ReqFailureUnitTests
import ScreenshotUnitTests
import SessionUIUnitTests
import UnitTestHelpers (stubWidth)

-- | The cases on the stub content, the 'Point' 'Enum' width set once here
-- to its map's, before any fixture is built, as the game sets it at startup.
-- The cases on the sample game's content run under @test/GameSpec.hs@.
main :: IO ()
main = do
  setSpeedupHackXSize stubWidth
  defaultMainWithIngredients
    (includingOptions freshnessOptions : defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [ actorStateUnitTests
                          , cellStyleUnitTests
                          , commonMUnitTests
                          , ffiCoverageUnitTests
                          , fontsUnitTests
                          , frameMUnitTests
                          , frontendContractUnitTests
                          , handleHelperMUnitTests
                          , handleHumanGlobalMUnitTests
                          , handleHumanLocalMUnitTests
                          , inputDecisionUnitTests
                          , inventoryMUnitTests
                          , itemDescriptionUnitTests
                          , itemKindUnitTests
                          , itemRevUnitTests
                          , levelUnitTests
                          , overlayLayoutUnitTests
                          , reqFailureUnitTests
                          , screenshotUnitTests
                          , macroTests
                          , monadClientUIUnitTests
                          ]
