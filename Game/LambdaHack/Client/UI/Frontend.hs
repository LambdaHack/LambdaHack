{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived operations.
module Game.LambdaHack.Client.UI.Frontend
  ( -- * Connection types
    FrontReq(..), ChanFrontend(..), KMP(..)
    -- * Re-exported part of the raw frontend
  , frontendName
    -- * Derived operations
  , chanFrontendIO
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , seqFrame
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Data.IORef

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Frame
import qualified Game.LambdaHack.Client.UI.Frontend.Chosen as Chosen
import Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Frontend.Std as Std
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | The instructions sent by clients to the raw frontend.
data FrontReq :: * -> * where
  FrontFrame :: {frontFrame :: !SingleFrame} -> FrontReq ()
    -- ^ show a frame
  FrontDelay :: !Int -> FrontReq ()
    -- ^ perform an explicit delay of the given length
  FrontKey :: { frontKeyKeys  :: ![K.KM]
              , frontKeyFrame :: !SingleFrame } -> FrontReq KMP
    -- ^ flush frames, display a frame and ask for a keypress
  FrontPressed :: FrontReq Bool
    -- ^ inspect the fkeyPressed MVar
  FrontDiscard :: FrontReq ()
    -- ^ discard a key in the queue, if any
  FrontAdd :: KMP -> FrontReq ()
    -- ^ add a key to the queue
  FrontAutoYes :: Bool -> FrontReq ()
    -- ^ set in the frontend that it should auto-answer prompts
  FrontShutdown :: FrontReq ()
    -- ^ shut the frontend down

-- | Connection channel between a frontend and a client. Frontend acts
-- as a server, serving keys, etc., when given frames to display.
newtype ChanFrontend = ChanFrontend (forall a. FrontReq a -> IO a)

data FSession = FSession
  { fautoYesRef   :: !(IORef Bool)
  , fasyncTimeout :: !(Async ())
  , fdelay        :: !(MVar Int)
  }

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
getKey :: DebugModeCli -> FSession -> RawFrontend -> [K.KM] -> SingleFrame
       -> IO KMP
getKey sdebugCli fs rf@RawFrontend{fchanKey} keys frame = do
  autoYes <- readIORef $ fautoYesRef fs
  if autoYes && (null keys || K.spaceKM `elem` keys) then do
    display rf frame
    return $! KMP{kmpKeyMod = K.spaceKM, kmpPointer=originPoint}
  else do
    -- Wait until timeout is up, not to skip the last frame of animation.
    display rf frame
    kmp <- STM.atomically $ STM.readTQueue fchanKey
    if null keys || kmpKeyMod kmp `elem` keys
    then return kmp
    else getKey sdebugCli fs rf keys frame

-- | Read UI requests from the client and send them to the frontend,
fchanFrontend :: DebugModeCli -> FSession -> RawFrontend -> ChanFrontend
fchanFrontend sdebugCli fs@FSession{..} rf =
  ChanFrontend $ \req -> case req of
    FrontFrame{..} -> display rf frontFrame
    FrontDelay k -> modifyMVar_ fdelay $ return . (+ k)
    FrontKey{..} -> getKey sdebugCli fs rf frontKeyKeys frontKeyFrame
    FrontPressed -> do
      noKeysPending <- STM.atomically $ STM.isEmptyTQueue (fchanKey rf)
      return $! not noKeysPending
    FrontDiscard ->
      void $ STM.atomically $ STM.tryReadTQueue (fchanKey rf)
    FrontAdd kmp -> STM.atomically $ STM.writeTQueue (fchanKey rf) kmp
    FrontAutoYes b -> writeIORef fautoYesRef b
    FrontShutdown -> do
      cancel fasyncTimeout
      fshutdown rf

display :: RawFrontend -> SingleFrame -> IO ()
display rf@RawFrontend{fshowNow} frontFrame = do
  putMVar fshowNow () -- 1. wait for permission to display; 3. ack
  fdisplay rf frontFrame

defaultMaxFps :: Int
defaultMaxFps = 30

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
        let waitForKeysUsed = do
              -- @fshowNow@ is full at this point, unless @saveKM@ emptied it,
              -- in which case we wait below until @display@ fills it
              takeMVar fshowNow  -- 2. permit display
              -- @fshowNow@ is ever empty only here, unless @saveKM@ empties it
              readMVar fshowNow  -- 4. wait for ack before starting delay
              -- @fshowNow@ is full at this point
              noKeysPending <- STM.atomically $ STM.isEmptyTQueue fchanKey
              unless noKeysPending waitForKeysUsed
        waitForKeysUsed
        loop
  loop

-- | The name of the chosen frontend.
frontendName :: String
frontendName = Chosen.frontendName

lazyStartup :: IO RawFrontend
lazyStartup = createRawFrontend (\_ -> return ()) (return ())

nullStartup :: IO RawFrontend
nullStartup = createRawFrontend seqFrame (return ())

seqFrame :: SingleFrame -> IO ()
seqFrame SingleFrame{singleFrame} =
  let seqAttr () attr = Color.colorToRGB (Color.fgFromW32 attr)
                        `seq` Color.colorToRGB (Color.bgFromW32 attr)
                        `seq` Color.charFromW32 attr == ' '
                        `seq` ()
  in return $! PointArray.foldlA' seqAttr () singleFrame

chanFrontendIO :: DebugModeCli -> IO ChanFrontend
chanFrontendIO sdebugCli = do
  let startup | sfrontendNull sdebugCli = nullStartup
              | sfrontendLazy sdebugCli = lazyStartup
              | sfrontendStd sdebugCli = Std.startup sdebugCli
              | otherwise = Chosen.startup sdebugCli
      maxFps = fromMaybe defaultMaxFps $ smaxFps sdebugCli
      delta = max 1 $ microInSec `div` maxFps
  rf <- startup
  fautoYesRef <- newIORef $ not $ sdisableAutoYes sdebugCli
  fdelay <- newMVar 0
  fasyncTimeout <- async $ frameTimeoutThread delta fdelay rf
  -- Warning: not linking @fasyncTimeout@, so it'd better not crash.
  let fs = FSession{..}
  return $ fchanFrontend sdebugCli fs rf
