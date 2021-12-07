module HandleHelperMUnitTests (handleHelperMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Game.LambdaHack.Client.UI.HandleHelperM

import UnitTestHelpers

handleHelperMUnitTests :: TestTree
handleHelperMUnitTests = testGroup "handleHelperMUnitTests"
  [ testCase "partyAfterLeader" $ do
      -- You've got to fight for your right to party!
      let testFunc = partyAfterLeader testActorId
      partyInMonad <- executorCli testFunc testCliStateWithItem
      let party = fst partyInMonad
      party @?= []
  ]
