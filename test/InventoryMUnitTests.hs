module InventoryMUnitTests (inventoryMUnitTests) where
-- at some point I'm guessing we'll want our unit test hierarchy to match the file hierarchy

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.InventoryM
import           Game.LambdaHack.Definition.Defs

import           UnitTestHelpers


inventoryMUnitTests :: TestTree 
inventoryMUnitTests = testGroup "inventoryMUnitTests" 
  [ testCase "getFull no stores " $
    do 
      let testFn = getFull testActorId
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
      let testFn = getFull testActorId
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
      let testFn = getFull testActorId
                            (return SuitsEverything) -- m Suitability
                            (\_ _ _ _ _ -> T.pack "specific prompt")
                            (\_ _ _ _ _ -> T.pack "generic prompt") 
                            [CEqp]
                            False 
                            False
      result <- executorCli testFn testCliStateWithItem  
      fst result @?= Right (CEqp,[(testItemId,(1,[]))])
  ]