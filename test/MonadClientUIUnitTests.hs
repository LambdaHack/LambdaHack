module MonadClientUIUnitTests (monadClientUIUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Client.UI.Frontend as Frontend
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Common.ClientOptions (ClientOptions (..))

import UnitTestHelpers

monadClientUIUnitTests :: TestTree
monadClientUIUnitTests = testGroup "monadClientUIUnitTests"
  [ testCase "getsClient sside" $ do
      sideInMonad <- executorCli (getsClient sside) stubCliState
      fst sideInMonad @?= testFactionId
  , testCase "getArenaUI works in stub" $ do
      levelIdInMonad <- executorCli getArenaUI stubCliState
      fst levelIdInMonad @?= testLevelId
  , testCase "viewedLevelUI works in stub" $ do
      levelIdInMonad <- executorCli viewedLevelUI stubCliState
      fst levelIdInMonad @?= testLevelId
  , testCase "getFontSetup works in stub" $ do
      -- getFontSetup only picks multiFontSetup when the frontend supports
      -- it (see MonadClientUI.getFontSetup). stubClientOptions reports the
      -- null-test frontend instead (see UnitTestHelpers), which does not,
      -- so the single-font fallback is the correct, platform-independent
      -- expectation here.
      fontSetupInMonad <- executorCli getFontSetup stubCliState
      fst fontSetupInMonad @?= singleFontSetup
  , testCase "getFontSetup follows the frontend's supportsMultiFont" $ do
      -- multiFontClientOptions reach the frontend chosen at compile time and
      -- their fontset resolves, so the capability alone decides, and every
      -- other frontend lacks it.
      let fontSetupUnder options = fst <$> executorCli getFontSetup
            stubCliState {cliClient = (cliClient stubCliState)
                                        {soptions = options}}
          chosen = Frontend.supportsMultiFont multiFontClientOptions
      fontSetupUnder multiFontClientOptions
        >>= (@?= if chosen then multiFontSetup else singleFontSetup)
#ifndef USE_BROWSER
      chosen @?= True  -- SDL's
#endif
      forM_ [ multiFontClientOptions {sfrontendNull = True}
            , multiFontClientOptions {sfrontendLazy = True}
            , multiFontClientOptions {sfrontendTeletype = True}
#ifndef USE_BROWSER
            , multiFontClientOptions {sfrontendANSI = True}
#endif
            ] $ fontSetupUnder >=> (@?= singleFontSetup)
  ]
