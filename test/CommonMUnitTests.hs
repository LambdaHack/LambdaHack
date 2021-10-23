module CommonMUnitTests (commonMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.EnumMap.Strict as EM


import           Game.LambdaHack.Client.CommonM

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time

import qualified Game.LambdaHack.Core.Dice as Dice

testActor = 
  Actor
    { btrunk = toEnum 0
    , bnumber = Nothing
    , bhp = 0
    , bhpDelta = ResDelta (0,0) (0,0)
    , bcalm = 0
    , bcalmDelta = ResDelta (0,0) (0,0)
    , bpos = Point 0 0
    , boldpos = Nothing
    , blid = toEnum 0
    , bfid = toEnum 0
    , btrajectory = Nothing
    , borgan = EM.empty
    , beqp = EM.empty
    , bweapon = 0
    , bweapBenign = 0
    , bwatch = WWatch
    , bproj = False
    }

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

testUnknownTileLevelState = localFromGlobal emptyState

commonMUnitTests :: TestTree
commonMUnitTests = testGroup "commonMUnitTests" $ 
  [ testCase "makeLine_stubLevel_fails" $
    do
      let eps = 1
          result = makeLine False testActor (Point 0 0) eps emptyCOps testLevel 
       in result @?= Nothing
  , testCase "makeLine_unknownTiles_succeeds" $
    do
      let eps = 1
          testDungeon = sdungeon testUnknownTileLevelState
          (Just testUnknownTileLevel) = EM.lookup (toEnum 0) testDungeon
          result = makeLine False testActor (Point 2 0) eps emptyCOps testUnknownTileLevel 
       in result @?= Just 1
  ]
