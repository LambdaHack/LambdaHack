module HandleHumanLocalMUnitTests (handleHumanLocalMUnitTests) where
-- at some point I'm guessing we'll want our unit test hierarchy to match the file hierarchy

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.Trans.State.Strict as St

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.EnumMap.Strict as EM
import qualified Data.Vector.Unboxed as U

import           Game.LambdaHack.Client.UI.HandleHelperM

import           Game.LambdaHack.Client.UI.MonadClientUI ( MonadClientUI )
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Content.TileKind
import           Game.LambdaHack.Definition.DefsInternal ( toContentSymbol )

import           MonadClientMock

toFactionId :: Int -> FactionId
toFactionId = toEnum


handleHumanLocalMUnitTests :: TestTree 
handleHumanLocalMUnitTests = testGroup "handleHumanLocalMUnitTests" 
  [  testCase "verifyTestLevelSize" $
    do let level = stubLevel -- (Just level) = EM.lookup (toEnum 0) (sdungeon testState)
        in (ltile level) ! (Point 0 0) @?= unknownId
  , testCase "chooseItemProjectHuman" $
    do 
      let testFn = let triggerItems = 
                        [ HumanCmd.TriggerItem{tiverb="verb", tiobject="object", tisymbols=[toContentSymbol 'a', toContentSymbol 'b']}
                        , HumanCmd.TriggerItem{tiverb="verb2", tiobject="object2", tisymbols=[toContentSymbol 'c']}
                        ]
                    in chooseItemProjectHuman (toEnum 1) triggerItems
      result <- executorCli testFn stubCliState
      fst result @?= Nothing --Just FailError {failError="no aim designated"}
  ]

-- chooseItemProjectHuman :: forall m. (MonadClient m, MonadClientUI m) -- line 395

-- - line 533
-- triggerSymbols

-- - line 576
-- chooseItemApplyHuman
