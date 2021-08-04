module PointUnitTests where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Game.LambdaHack.Common.Point

pointUnitTests :: TestTree
pointUnitTests = testGroup "pointUnitTests" 
  [  testCase "originToOrigin_chessDist_equals0" $
        chessDist (Point 0 0) (Point 0 0) @?= 0
  ,  testCase "neg1To1_chessDist_equals2" $
        chessDist (Point (-1) (-1)) (Point 1 1) @?= 2
  ,  testCase "sameColumn_chessDist_equals1" $
        chessDist (Point (-1) 0) (Point (-1) 1) @?= 1
  ,  testCase "sameRow_chessDist_equals1" $
        chessDist (Point (-1) 0) (Point 0 0) @?= 1
  ,  testCase "knightMove_chessDist_equals2" $
        chessDist (Point 1 1) (Point 3 2) @?= 2
  ]

