module LevelUnitTests (levelUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.EnumMap.Strict as EM


import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time

import qualified Game.LambdaHack.Core.Dice as Dice


testLevel :: Level
testLevel = Level
  { lkind = toEnum 0
  , ldepth = Dice.AbsDepth 1
  , lfloor = EM.empty
  , lembed = EM.empty
  , lbig = EM.empty
  , lproj = EM.empty
  , ltile = PointArray.empty
  , lentry = EM.empty
  , larea = trivialArea (Point 0 0)
  , lsmell = EM.empty
  , lstair = ([],[])
  , lescape = []
  , lseen = 0
  , lexpl = 0
  , ltime = timeZero
  , lnight = False
  }

testDungeonWithLevel :: State
testDungeonWithLevel = let singletonDungeonUpdate _ = EM.singleton (toEnum 0) testLevel                          
                           unknownTileState = localFromGlobal emptyState
                           oneLevelDungeonState = updateDungeon singletonDungeonUpdate unknownTileState
                        in oneLevelDungeonState

levelUnitTests :: TestTree
levelUnitTests = testGroup "levelUnitTests" $ 
  [ testCase "testDungeonWithLevel has min level id" $
      do let ((minKey, _), _) = fromJust $ EM.minViewWithKey (sdungeon testDungeonWithLevel)
          in minKey @?= toEnum 0
  , testCase "testDungeonWithLevel has max level id" $
      do let ((minKey, _), _) = fromJust $ EM.maxViewWithKey (sdungeon testDungeonWithLevel)
          in minKey @?= toEnum 0
  , testCase "dungeonBounds testDungeonWithLevel returns (0,0)" $
      do let bounds = dungeonBounds (sdungeon testDungeonWithLevel) 
          in bounds @?= (toEnum 0,toEnum 0)
  ]
