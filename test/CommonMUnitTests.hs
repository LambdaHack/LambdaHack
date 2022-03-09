module CommonMUnitTests (commonMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Common.Kind (emptyCOps)
import Game.LambdaHack.Common.Perception (emptyPer)
import Game.LambdaHack.Common.Point (Point (..))

import UnitTestHelpers
  (executorCli, stubCliState, testActor, testLevel, testLevelId)


commonMUnitTests :: TestTree
commonMUnitTests = testGroup "commonMUnitTests"
  [ testCase "getPerFid stubCliState returns emptyPerception" $ do
      result <- executorCli (getPerFid testLevelId) stubCliState
      fst result @?= emptyPer
  , testCase "makeLine, when actor stands at the target position, fails" $
      Nothing @?= makeLine False testActor (Point 0 0) 1 emptyCOps testLevel
  , testCase "makeLine unknownTiles succeeds" $
      Just 1 @?= makeLine False testActor (Point 2 0) 1 emptyCOps testLevel
  ]
