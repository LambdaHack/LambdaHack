module ActorStateUnitTests (actorStateUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Common.ActorState

import           Game.LambdaHack.Definition.Ability as Ability
import           UnitTestHelpers

actorStateUnitTests :: TestTree 
actorStateUnitTests = testGroup "actorStateUnitTests" 
  [ testCase "getActorBody verify stubCliState has testActor" $
    do getActorBody testActorId (cliState stubCliState) @?= testActor
  , testCase "getActorMaxSkills verify stubCliState has zeroSkills" $
    do getActorMaxSkills testActorId (cliState stubCliState) @?= Ability.zeroSkills
  , testCase "fidActorNotProjGlobalAssocs" $
    do fidActorNotProjGlobalAssocs testFactionId (cliState testCliStateWithItem) @?= [(testActorId, testActorWithItem)]
  ]
