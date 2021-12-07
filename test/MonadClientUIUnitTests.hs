module MonadClientUIUnitTests (monadClientUIUnitTests) where
-- at some point I'm guessing we'll want our unit test hierarchy to match the file hierarchy

import Prelude ()

import Game.LambdaHack.Core.Prelude


import Test.Tasty
import Test.Tasty.HUnit


import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.Overlay

import           UnitTestHelpers

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
