module HandleHelperMUnitTests (handleHelperMUnitTests) where
-- at some point I'm guessing we'll want our unit test hierarchy to match the file hierarchy

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.HandleHelperM

import           UnitTestHelpers



handleHelperMUnitTests :: TestTree 
handleHelperMUnitTests = testGroup "handleHelperMUnitTests" 
  [ testCase "partyAfterLeader" $ -- you've got to fight for your right to party
    do let testFunc = partyAfterLeader testActorId 
       partyInMonad <- executorCli testFunc testCliStateWithItem 
       let party = fst partyInMonad
       party @?= []
  ]
