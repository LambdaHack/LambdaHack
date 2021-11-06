module ActorStateUnitTests (actorStateUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Either

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

import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemAspect
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Content.TileKind
import           Game.LambdaHack.Definition.DefsInternal ( toContentId, toContentSymbol )

import           Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Flavour

import           UnitTestHelpers

actorStateUnitTests :: TestTree 
actorStateUnitTests = testGroup "actorStateUnitTests" 
  [ testCase "getActorBody verify stubCliState has testActor" $
    do getActorBody testActorId (cliState stubCliState) @?= testActor
  , testCase "getActorMaxSkills verify stubCliState has zeroSkills" $
    do getActorMaxSkills testActorId (cliState stubCliState) @?= Ability.zeroSkills
  ]
