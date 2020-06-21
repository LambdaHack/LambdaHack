{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived operations.
module Game.LambdaHack.Client.UI.Frontend
  ( -- * Connection and initialization
    FrontReq(..), ChanFrontend(..), chanFrontendIO
    -- * Re-exported part of the raw frontend
  , frontendName
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , FrontSetup, getKey, fchanFrontend, display, defaultMaxFps, microInSec
  , frameTimeoutThread, lazyStartup, nullStartup, seqFrame
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import           Control.Monad.ST.Strict
import           Data.IORef
import           Data.Kind (Type)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import           Data.Word

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import qualified Game.LambdaHack.Client.UI.Frontend.Chosen as Chosen
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Frontend.Teletype as Teletype
import           Game.LambdaHack.Client.UI.Key (KMP (..))
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Definition.Color as Color

-- | The instructions sent by clients to the raw frontend, indexed
-- by the returned value.
data FrontReq :: Type -> Type where
  -- | Show a frame.
  FrontFrame :: Frame -> FrontReq ()
  -- | Perform an explicit delay of the given length.
  FrontDelay :: Int -> FrontReq ()
  -- | Flush frames, display a frame and ask for a keypress.
  FrontKey :: [K.KM] -> Frame -> FrontReq KMP
  -- | Tell if a keypress is pending.
  FrontPressed :: FrontReq Bool
  -- | Discard a key in the queue, if any.
  FrontDiscardKey :: FrontReq ()
  -- | Discard all keys in the queue.
  FrontResetKeys :: FrontReq ()
  -- | Add a key to the queue.
  FrontAdd :: KMP -> FrontReq ()
  -- | Set in the frontend that it should auto-answer prompts.
  FrontAutoYes :: Bool -> FrontReq ()
  -- | Shut the frontend down.
  FrontShutdown :: FrontReq ()
  -- | Take screenshot.
  FrontPrintScreen :: FrontReq ()

-- | Connection channel between a frontend and a client. Frontend acts
-- as a server, serving keys, etc., when given frames to display.
newtype ChanFrontend = ChanFrontend (forall a. FrontReq a -> IO a)

-- | Machinery allocated for an individual frontend at its startup,
-- unchanged for its lifetime.
data FrontSetup = FrontSetup
  { fautoYesRef   :: IORef Bool
  , fasyncTimeout :: Async ()
  , fdelay        :: MVar Int
  }

-- | Initialize the frontend chosen by the player via client options.
chanFrontendIO :: ScreenContent -> ClientOptions -> IO ChanFrontend
chanFrontendIO coscreen soptions = do
  let startup | sfrontendNull soptions = nullStartup coscreen
              | sfrontendLazy soptions = lazyStartup coscreen
#ifndef REMOVE_TELETYPE
              | sfrontendTeletype soptions = Teletype.startup coscreen
#endif
              | otherwise = Chosen.startup coscreen soptions
      maxFps = fromMaybe defaultMaxFps $ smaxFps soptions
      delta = max 1 $ round $ fromIntegral microInSec / max 0.000001 maxFps
  rf <- startup
  fautoYesRef <- newIORef $ not $ sdisableAutoYes soptions
  fdelay <- newMVar 0
  fasyncTimeout <- async $ frameTimeoutThread delta fdelay rf
  -- Warning: not linking @fasyncTimeout@, so it'd better not crash.
  let fs = FrontSetup{..}
  return $ fchanFrontend fs rf

-- Display a frame, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
getKey :: FrontSetup -> RawFrontend -> [K.KM] -> Frame -> IO KMP
getKey fs rf@RawFrontend{fchanKey} keys frame = do
  autoYes <- readIORef $ fautoYesRef fs
  if autoYes && (null keys || K.spaceKM `elem` keys) then do
    display rf frame
    return $! KMP {kmpKeyMod = K.spaceKM, kmpPointer = K.PointUI 0 0}
  else do
    -- Wait until timeout is up, not to skip the last frame of animation.
    display rf frame
    kmp <- STM.atomically $ STM.readTQueue fchanKey
    if null keys || kmpKeyMod kmp `elem` keys
    then return kmp
    else getKey fs rf keys frame

-- Read UI requests from the client and send them to the frontend,
fchanFrontend :: FrontSetup -> RawFrontend -> ChanFrontend
fchanFrontend fs@FrontSetup{..} rf =
  ChanFrontend $ \case
    FrontFrame frontFrame -> display rf frontFrame
    FrontDelay k -> modifyMVar_ fdelay $ return . (+ k)
    FrontKey frontKeyKeys frontKeyFrame ->
      getKey fs rf frontKeyKeys frontKeyFrame
    FrontPressed -> do
      noKeysPending <- STM.atomically $ STM.isEmptyTQueue (fchanKey rf)
      return $! not noKeysPending
    FrontDiscardKey ->
      void $ STM.atomically $ STM.tryReadTQueue (fchanKey rf)
    FrontResetKeys -> resetChanKey (fchanKey rf)
    FrontAdd kmp -> STM.atomically $ STM.writeTQueue (fchanKey rf) kmp
    FrontAutoYes b -> writeIORef fautoYesRef b
    FrontShutdown -> do
      cancel fasyncTimeout
      -- In case the last frame display is pending:
      void $ tryTakeMVar $ fshowNow rf
      fshutdown rf
    FrontPrintScreen -> fprintScreen rf

display :: RawFrontend -> Frame -> IO ()
display rf@RawFrontend{fshowNow, fcoscreen=ScreenContent{rwidth, rheight}}
        ((m, upd), (ovProp, ovMono)) = do
  let new :: forall s. ST s (G.Mutable U.Vector s Word32)
      new = do
        v <- unFrameBase m
        unFrameForall upd v
        return v
      singleArray = PointArray.Array rwidth rheight (U.create new)
  putMVar fshowNow () -- 1. wait for permission to display; 3. ack
  fdisplay rf $ SingleFrame singleArray ovProp ovMono

defaultMaxFps :: Double
defaultMaxFps = 24

microInSec :: Int
microInSec = 1000000

-- This thread is canceled forcefully, because the @threadDelay@
-- may be much longer than an acceptable shutdown time.
frameTimeoutThread :: Int -> MVar Int -> RawFrontend -> IO ()
frameTimeoutThread delta fdelay RawFrontend{..} = do
  let loop = do
        threadDelay delta
        let delayLoop = do
              delay <- readMVar fdelay
              when (delay > 0) $ do
                threadDelay $ delta * delay
                modifyMVar_ fdelay $ return . subtract delay
                delayLoop
        delayLoop
        let showFrameAndRepeatIfKeys = do
              -- @fshowNow@ is full at this point, unless @saveKM@ emptied it,
              -- in which case we wait below until @display@ fills it
              takeMVar fshowNow  -- 2. permit display
              -- @fshowNow@ is ever empty only here, unless @saveKM@ empties it
              readMVar fshowNow  -- 4. wait for ack before starting delay
              -- @fshowNow@ is full at this point
              noKeysPending <- STM.atomically $ STM.isEmptyTQueue fchanKey
              unless noKeysPending $ do
                void $ swapMVar fdelay 0  -- cancel delays lest they accumulate
                showFrameAndRepeatIfKeys
        showFrameAndRepeatIfKeys
        loop
  loop

-- | The name of the chosen frontend.
frontendName :: ClientOptions -> String
frontendName soptions =
  if | sfrontendNull soptions -> "null test"
     | sfrontendLazy soptions -> "lazy test"
#ifndef REMOVE_TELETYPE
     | sfrontendTeletype soptions -> Teletype.frontendName
#endif
     | otherwise -> Chosen.frontendName

lazyStartup :: ScreenContent -> IO RawFrontend
lazyStartup coscreen = createRawFrontend coscreen (\_ -> return ()) (return ())

nullStartup :: ScreenContent -> IO RawFrontend
nullStartup coscreen = createRawFrontend coscreen seqFrame (return ())

seqFrame :: SingleFrame -> IO ()
seqFrame SingleFrame{singleArray} =
  let seqAttr () attr = Color.colorToRGB (Color.fgFromW32 attr)
                        `seq` Color.bgFromW32 attr
                        `seq` Color.charFromW32 attr == ' '
                        `seq` ()
  in return $! PointArray.foldlA' seqAttr () singleArray
