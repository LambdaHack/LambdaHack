module LevelUnitTests (levelUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Test.Tasty
import           Test.Tasty.HUnit

import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.State

import UnitTestHelpers (testLevel, testLevelId)

testDungeonWithLevel :: State
testDungeonWithLevel =
  let singletonDungeonUpdate _ = EM.singleton testLevelId testLevel
      unknownTileState = localFromGlobal emptyState
      oneLevelDungeonState =
        updateDungeon singletonDungeonUpdate unknownTileState
  in oneLevelDungeonState

levelUnitTests :: TestTree
levelUnitTests = testGroup "levelUnitTests"
  [ testCase "testDungeonWithLevel has min level id" $ do
      let ((minKey, _), _) =
            fromJust $ EM.minViewWithKey (sdungeon testDungeonWithLevel)
      minKey @?= testLevelId
  , testCase "testDungeonWithLevel has max level id" $ do
      let ((minKey, _), _) =
            fromJust $ EM.maxViewWithKey (sdungeon testDungeonWithLevel)
      minKey @?= testLevelId
  , testCase "dungeonBounds testDungeonWithLevel returns (0,0)" $ do
      let bounds = dungeonBounds (sdungeon testDungeonWithLevel)
      bounds @?= (testLevelId, testLevelId)
  ]
