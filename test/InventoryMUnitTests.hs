module InventoryMUnitTests (inventoryMUnitTests) where
-- at some point I'm guessing we'll want our unit test hierarchy to match the file hierarchy

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.EnumMap.Strict as EM
import qualified Data.Vector.Unboxed as U

import           Game.LambdaHack.Client.UI.HandleHelperM

import           Game.LambdaHack.Client.UI.MonadClientUI ( MonadClientUI )
import           Game.LambdaHack.Client.UI.InventoryM
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
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal ( toContentSymbol )

import           MonadClientMock

toFactionId :: Int -> FactionId
toFactionId = toEnum


inventoryMUnitTests :: TestTree 
inventoryMUnitTests = testGroup "inventoryMUnitTests" 
  [ testCase "getFull no stores " $
    do 
      let testFn = getFull (toEnum 1) -- actor id
                            (return SuitsEverything) -- m Suitability
                            (\_ _ _ _ _ -> T.pack "specific prompt")
                            (\_ _ _ _ _ -> T.pack "generic prompt") 
                            [] -- [CStore]
                            False 
                            False
      result <- executorCli testFn stubCliState 
      fst result @?= Left "no items"
  , testCase "getFull no item in eqp store" $
    do
      let testFn = getFull (toEnum 1) -- actor id
                            (return SuitsEverything) -- m Suitability
                            (\_ _ _ _ _ -> T.pack "specific prompt")
                            (\_ _ _ _ _ -> T.pack "generic prompt") 
                            [CEqp]
                            False 
                            False
      result <- executorCli testFn stubCliState 
      fst result @?= Left "no items in equipment outfit"
  , testCase "getFull an item in eqp store" $
    do
      let testFn = getFull (toEnum 1) -- actor id
                            (return SuitsEverything) -- m Suitability
                            (\_ _ _ _ _ -> T.pack "specific prompt")
                            (\_ _ _ _ _ -> T.pack "generic prompt") 
                            [CEqp]
                            False 
                            False
      result <- executorCli testFn stubCliState 
      fst result @?= Left "no items in equipment outfit"
  ]

-- chooseItemProjectHuman :: forall m. (MonadClient m, MonadClientUI m) -- line 395

-- - line 533
-- triggerSymbols

-- - line 576
-- chooseItemApplyHuman
