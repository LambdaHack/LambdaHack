module ActorStateUnitTests (actorStateUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Definition.Ability as Ability

import UnitTestHelpers

actorStateUnitTests :: TestTree
actorStateUnitTests = testGroup "actorStateUnitTests"
  [ testCase "getActorBody verify stubCliState has testActor" $
      getActorBody testActorId (cliState stubCliState) @?= testActor
  , testCase "getActorMaxSkills verify stubCliState has zeroSkills" $
      getActorMaxSkills testActorId (cliState stubCliState)
      @?= Ability.zeroSkills
  , testCase "fidActorNotProjGlobalAssocs" $
      fidActorNotProjGlobalAssocs testFactionId (cliState testCliStateWithItem)
      @?= [(testActorId, testActorWithItem)]
  ]
