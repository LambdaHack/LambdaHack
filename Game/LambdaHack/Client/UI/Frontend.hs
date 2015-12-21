{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived operations.
module Game.LambdaHack.Client.UI.Frontend
  ( -- * Connection types
    FrontReq(..), ChanFrontend(..)
    -- * Re-exported part of the raw frontend
  , frontendName
    -- * Derived operations
  , chanFrontend
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Monad (void)
import Data.IORef
import Data.Maybe

import qualified Game.LambdaHack.Client.Key as K
import qualified Game.LambdaHack.Client.UI.Frontend.Chosen as Chosen
import Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Frontend.Std as Std
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Msg

-- | The instructions sent by clients to the raw frontend.
data FrontReq :: * -> * where
  FrontFrame :: {frontFrame :: !SingleFrame} -> FrontReq ()
    -- ^ show a frame
  FrontDelay :: FrontReq ()
    -- ^ perform a single explicit delay
  FrontKey :: { frontKeyKeys  :: ![K.KM]
              , frontKeyFrame :: !SingleFrame } -> FrontReq K.KM
    -- ^ flush frames, display a frame and ask for a keypress
  FrontPressed :: FrontReq Bool
    -- ^ inspect and reset the fkeyPressed MVar
  FrontAutoYes :: Bool -> FrontReq ()
    -- ^ set in the frontend that it should auto-answer prompts
  FrontShutdown :: FrontReq ()
    -- ^ shut the frontend down

-- | Connection channel between a frontend and a client. Frontend acts
-- as a server, serving keys, etc., when given frames to display.
newtype ChanFrontend = ChanFrontend (forall a. FrontReq a -> IO a)

data FSession = FSession
  { fautoYesRef   :: !(IORef Bool)
  , ftimeout      :: !(MVar Int)
  , fasyncTimeout :: !(Async ())
  , fdelay        :: !(IORef Bool)
  }

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: DebugModeCli -> FSession -> RawFrontend -> [K.KM] -> SingleFrame
             -> IO K.KM
promptGetKey sdebugCli fs rf@RawFrontend{fchanKey} [] frame = do
  display sdebugCli fs rf frame
  STM.atomically $ STM.readTQueue fchanKey
promptGetKey sdebugCli fs@FSession{fautoYesRef} rf@RawFrontend{fchanKey} keys frame = do
  autoYes <- readIORef fautoYesRef
  if autoYes && K.spaceKM `elem` keys then do
    fdisplay rf frame
    return K.spaceKM
  else do
    -- Wait until timeout is up, not to skip the last frame of animation.
    display sdebugCli fs rf frame
    km <- STM.atomically $ STM.readTQueue fchanKey
    if km{K.pointer=Nothing} `elem` keys
    then return km
    else promptGetKey sdebugCli fs rf keys frame

-- | Read UI requests from the client and send them to the frontend,
fchanFrontend :: DebugModeCli -> FSession -> RawFrontend -> ChanFrontend
fchanFrontend sdebugCli fs@FSession{..} rf =
  ChanFrontend $ \req -> case req of
    FrontFrame{..} -> display sdebugCli fs rf frontFrame
    FrontDelay -> writeIORef fdelay True
    FrontKey{..} -> promptGetKey sdebugCli fs rf frontKeyKeys frontKeyFrame
    FrontPressed -> do
      noKeysPending <- STM.atomically $ STM.isEmptyTQueue (fchanKey rf)
      return $! not noKeysPending
    FrontAutoYes b -> writeIORef fautoYesRef b
    FrontShutdown -> do
      cancel fasyncTimeout
      fshutdown rf

display :: DebugModeCli -> FSession -> RawFrontend -> SingleFrame -> IO ()
display DebugModeCli{smaxFps}
        FSession{..}
        rf@RawFrontend{fshowNow}
        frontFrame = do
  let maxFps = fromMaybe defaultMaxFps smaxFps
      baseTime = microInSec `div` maxFps
  delay <- readIORef fdelay
  delta <- if delay then do
             writeIORef fdelay False
             return $! 2 * baseTime
           else
             return $! baseTime
  takeMVar fshowNow
  noKeysPending <- STM.atomically $ STM.isEmptyTQueue (fchanKey rf)
  if noKeysPending then
    -- For simplicity, not overwriting, even if @maxFps@ changed.
    void $ tryPutMVar ftimeout delta
  else
    -- Keys pending, so instantly show any future frame waiting for display.
    void $ tryPutMVar fshowNow ()
  fdisplay rf frontFrame

defaultMaxFps :: Int
defaultMaxFps = 30

microInSec :: Int
microInSec = 1000000

-- This thread is canceled forcefully, because the @threadDelay@
-- may be much longer than an acceptable shutdown time.
forkFrameTimeout :: MVar Int -> RawFrontend -> IO ()
forkFrameTimeout ftimeout RawFrontend{fshowNow} = do
  let loop = do
        timeout <- takeMVar ftimeout
        threadDelay timeout
        -- For simplicity, we don't care that occasionally a frame will be
        -- shown too early, when a keypress happens during the timeout,
        -- is handled and the next frame is ready before timeout expires.
        void $ tryPutMVar fshowNow ()
        loop
  loop

-- | The name of the chosen frontend.
frontendName :: String
frontendName = Chosen.frontendName

nullStartup :: IO RawFrontend
nullStartup = createRawFrontend (\_ -> return ()) (return ())

chanFrontend :: DebugModeCli -> IO ChanFrontend
chanFrontend sdebugCli = do
  let startup | sfrontendNull sdebugCli = nullStartup
              | sfrontendStd sdebugCli = Std.startup sdebugCli
              | otherwise = Chosen.startup sdebugCli
  rf <- startup
  fautoYesRef <- newIORef $ not $ sdisableAutoYes sdebugCli
  ftimeout <- newEmptyMVar
  fdelay <- newIORef False
  fasyncTimeout <- async $ forkFrameTimeout ftimeout rf
  -- Warning: not linking @fasyncTimeout@, so it'd better not crash.
  let fs = FSession{..}
  return $ fchanFrontend sdebugCli fs rf
