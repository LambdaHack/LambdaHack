-- | The RawFrontend contract's input side: what the engine requires of
-- a frontend's key delivery (@docs/wasm-frontend-unified-plan.md@, the
-- RawFrontend contract practice, whose input-side cases land with 0.1). Each
-- case runs on the frontend 'startupUnderTest' returns: under wasm the real
-- 'Wasm.startup', and natively one built as the lazy startup builds its own,
-- that startup being internal to "Game.LambdaHack.Client.UI.Frontend" until
-- the contract's harness exports it. The keys go in as the SDL and wasm
-- frontends' key handlers deliver them, and the reactor's own @lhKey@, which
-- this command binary cannot call, is @ts-src/run-wasm-game.mjs@'s to cover.
--
-- Each case name begins with the @fe-invariant@ series marker.
module FrontendContractUnitTests (frontendContractUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import qualified Control.Concurrent.STM as STM
import           System.Timeout (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend
  (KeyDecision (..), decideKey)
import           Game.LambdaHack.Client.UI.Frontend.Common
  (RawFrontend (..), resetChanKey, saveKMP)
import           Game.LambdaHack.Client.UI.Key (KMP (..))
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI

#ifdef USE_WASM
import qualified Game.LambdaHack.Client.UI.Frontend.Wasm as Wasm
import           Game.LambdaHack.Common.ClientOptions
#else
import qualified Game.LambdaHack.Client.UI.Frontend.Common as Common
#endif

frontendContractUnitTests :: TestTree
frontendContractUnitTests = testGroup "frontendContractUnitTests"
  [ testCase "fe-invariant: a key shows a waiting frame and is pending" $ do
      rf <- startupUnderTest coscreen
      shown <- frameWaiting rf
      timeout 200000 (readMVar shown) >>= (@?= Nothing)
      deliver rf K.NoModifier (K.Char 'a')
      timeout 5000000 (takeMVar shown) >>= (@?= Just ())
      pressed rf >>= (@?= True)
      pending rf >>= (@?= [K.KM K.NoModifier (K.Char 'a')])
  , testCase "fe-invariant: a dead key shows a waiting frame only" $ do
      rf <- startupUnderTest coscreen
      shown <- frameWaiting rf
      deliver rf K.Shift K.DeadKey
      timeout 5000000 (takeMVar shown) >>= (@?= Just ())
      pressed rf >>= (@?= False)
  , testCase "fe-invariant: Esc empties the keys pending before it" $ do
      rf <- startupUnderTest coscreen
      deliver rf K.NoModifier (K.Char 'a')
      deliver rf K.Shift (K.Char '?')
      deliver rf K.NoModifier K.Esc
      pending rf >>= (@?= [K.escKM])
  ]
 where
  coscreen = emptyScreenContent {rwidth = 7, rheight = 3}

-- | The frontend the cases run on.
startupUnderTest :: ScreenContent -> IO RawFrontend
#ifdef USE_WASM
startupUnderTest coscreen = Wasm.startup coscreen defClientOptions
#else
startupUnderTest coscreen =
  Common.createRawFrontend coscreen (\_ -> return ()) (return ())
#endif

-- | The steps the SDL and wasm frontends' key handlers take with a key
-- they translated.
deliver :: RawFrontend -> K.Modifier -> K.Key -> IO ()
deliver rf modifier key = do
  let KeyDecision{kdModifier, kdResetChan} = decideKey modifier key
  when kdResetChan $ resetChanKey $ fchanKey rf
  saveKMP rf kdModifier key $ PointUI 0 0

-- | A frame displayed and not yet let go by the frame timeout thread,
-- and the next frame's display blocked behind it, as @display@ in
-- "Game.LambdaHack.Client.UI.Frontend" leaves them between steps 1 and 2
-- of its handshake. The MVar returned is filled once that next frame
-- is shown.
frameWaiting :: RawFrontend -> IO (MVar ())
frameWaiting rf = do
  putMVar (fshowNow rf) ()
  shown <- newEmptyMVar
  void $ forkIO $ do
    putMVar (fshowNow rf) ()
    fdisplay rf $ blankSingleFrame $ fcoscreen rf
    putMVar shown ()
  return shown

-- | What @FrontPressed@ returns, reading the same queue.
pressed :: RawFrontend -> IO Bool
pressed rf = not <$> STM.atomically (STM.isEmptyTQueue $ fchanKey rf)

-- | The keys pending, emptying the queue.
pending :: RawFrontend -> IO [K.KM]
pending rf = map kmpKeyMod <$> STM.atomically (STM.flushTQueue $ fchanKey rf)
