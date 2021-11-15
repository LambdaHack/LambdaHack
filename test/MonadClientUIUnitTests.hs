module MonadClientUIUnitTests (monadClientUIUnitTests) where
-- at some point I'm guessing we'll want our unit test hierarchy to match the file hierarchy

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Either

import qualified Control.Monad.Trans.State.Strict as St

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.EnumMap.Strict as EM
import qualified Data.Vector.Unboxed as U


import           Game.LambdaHack.Client.UI.HandleHelperM

import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import           Game.LambdaHack.Client.UI.Slideshow

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
import           Game.LambdaHack.Definition.Flavour

import           UnitTestHelpers

toFactionId :: Int -> FactionId
toFactionId = toEnum


monadClientUIUnitTests :: TestTree 
monadClientUIUnitTests = testGroup "handleHumanLocalMUnitTests" 
  [ testCase "getsClient sside" $
    do
      sideInMonad <- executorCli (getsClient sside) stubCliState 
      fst sideInMonad @?= testFactionId
  , testCase "getArenaUI works in stub" $
    do
      levelIdInMonad <- executorCli getArenaUI stubCliState 
      fst levelIdInMonad @?= testLevelId
  , testCase "viewedLevelUI works in stub" $
    do
      levelIdInMonad <- executorCli viewedLevelUI stubCliState
      fst levelIdInMonad @?= testLevelId 
  , testCase "getFontSetup works in stub" $
    do
      fontSetupInMonad <- executorCli getFontSetup stubCliState
      fst fontSetupInMonad @?= multiFontSetup 
  ]
