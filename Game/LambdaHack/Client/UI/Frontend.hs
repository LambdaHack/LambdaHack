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
import Game.LambdaHack.Client.UI.Animation
import qualified Game.LambdaHack.Client.UI.Frontend.Chosen as Chosen
import Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Frontend.Std as Std
import Game.LambdaHack.Common.ClientOptions

-- | The instructions sent by clients to the raw frontend.
data FrontReq :: * -> * where
  FrontFrame :: {frontFrame :: !SingleFrame} -> FrontReq ()
    -- ^ show a frame
  FrontDelay :: FrontReq ()
    -- ^ perform a single explicit delay
  FrontKey :: { frontKeyKeys  :: ![K.KM]
              , frontKeyFrame :: !SingleFrame } -> FrontReq K.KM
    -- ^ flush frames, display a frame and ask for a keypress
  FrontSync :: FrontReq ()
    -- ^ flush frames
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
  { fautoYesRef :: !(IORef Bool)
  , ftimeout    :: !(MVar Int)
  }

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: FSession -> RawFrontend -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey _ rf [] frame = fpromptGetKey rf frame
promptGetKey fs@FSession{fautoYesRef} rf keys frame = do
  autoYes <- readIORef fautoYesRef
  if autoYes && K.spaceKM `elem` keys then do
    fdisplay rf frame
    return K.spaceKM
  else do
    -- cancel delay (or extend delay?)
    km <- fpromptGetKey rf frame
    if km{K.pointer=Nothing} `elem` keys
    then return km
    else promptGetKey fs rf keys frame

-- | Read UI requests from the client and send them to the frontend,
fchanFrontend :: DebugModeCli -> FSession -> RawFrontend -> ChanFrontend
fchanFrontend DebugModeCli{smaxFps}
              fsess@FSession{..}
              rf@RawFrontend{fshowNow} =
  let maxFps = fromMaybe defaultMaxFps smaxFps
  in ChanFrontend $ \req -> case req of
    FrontFrame{..} -> do
      takeMVar fshowNow
      noKeysPending <- STM.atomically $ STM.isEmptyTQueue (fchanKey rf)
      if noKeysPending then
        -- For simplicity, not overwriting, even if @maxFps@ changed.
        void $ tryPutMVar ftimeout $ microInSec `div` maxFps
      else
        -- Keys pending, so instantly show any future frame waiting for display.
        void $ tryPutMVar fshowNow ()
      fdisplay rf frontFrame
    FrontDelay -> return ()
    FrontKey{..} -> promptGetKey fsess rf frontKeyKeys frontKeyFrame
    FrontSync ->
      void $ tryPutMVar fshowNow ()
    FrontPressed -> do
      noKeysPending <- STM.atomically $ STM.isEmptyTQueue (fchanKey rf)
      return $! not noKeysPending
    FrontAutoYes b -> writeIORef fautoYesRef b
    FrontShutdown -> fshutdown rf

defaultMaxFps :: Int
defaultMaxFps = 30

microInSec :: Int
microInSec = 1000000

forkFrameTimeout :: FSession -> RawFrontend -> IO ()
forkFrameTimeout FSession{ftimeout} RawFrontend{fshowNow} = do
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
nullStartup = do
  fchanKey <- STM.atomically STM.newTQueue
  fshowNow <- newMVar ()
  return $! RawFrontend
    { fdisplay = \_ -> return ()
    , fpromptGetKey = \_ -> return K.escKM
    , fshutdown = return ()
    , fshowNow
    , fchanKey
    }

chanFrontend :: DebugModeCli -> IO ChanFrontend
chanFrontend sdebugCli = do
  let startup | sfrontendNull sdebugCli = nullStartup
              | sfrontendStd sdebugCli = Std.startup sdebugCli
              | otherwise = Chosen.startup sdebugCli
  rf <- startup
  fautoYesRef <- newIORef $ not $ sdisableAutoYes sdebugCli
  ftimeout <- newEmptyMVar
  let fs = FSession{..}
  a <- async $ forkFrameTimeout fs rf
  link a
  return $ fchanFrontend sdebugCli fs rf
