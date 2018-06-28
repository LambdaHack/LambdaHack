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

import Game.LambdaHack.Common.Prelude

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import           Control.Monad.ST.Strict
import           Data.IORef
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Word

import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.UI.Frame
import qualified Game.LambdaHack.Client.UI.Frontend.Chosen as Chosen
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Frontend.Teletype as Teletype
import           Game.LambdaHack.Client.UI.Key (KMP (..))
import qualified Game.LambdaHack.Client.UI.Key as K
import qualified Game.LambdaHack.Common.Color as Color
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | The instructions sent by clients to the raw frontend, indexed
-- by the returned value.
data FrontReq :: * -> * where
  -- | Show a frame.
  FrontFrame :: {frontFrame :: FrameForall} -> FrontReq ()
  -- | Perform an explicit delay of the given length.
  FrontDelay :: Int -> FrontReq ()
  -- | Flush frames, display a frame and ask for a keypress.
  FrontKey :: { frontKeyKeys  :: [K.KM]
              , frontKeyFrame :: FrameForall } -> FrontReq KMP
  -- | Tell if a keypress is pending.
  FrontPressed :: FrontReq Bool
  -- | Discard a key in the queue, if any.
  FrontDiscard :: FrontReq ()
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
chanFrontendIO :: ClientOptions -> IO ChanFrontend
chanFrontendIO soptions = do
  let startup | sfrontendNull soptions = nullStartup
              | sfrontendLazy soptions = lazyStartup
              | sfrontendTeletype soptions = Teletype.startup soptions
              | otherwise = Chosen.startup soptions
      maxFps = fromMaybe defaultMaxFps $ smaxFps soptions
      delta = max 1 $ microInSec `div` maxFps
  rf <- startup
  fautoYesRef <- newIORef $ not $ sdisableAutoYes soptions
  fdelay <- newMVar 0
  fasyncTimeout <- async $ frameTimeoutThread delta fdelay rf
  -- Warning: not linking @fasyncTimeout@, so it'd better not crash.
  let fs = FrontSetup{..}
  return $ fchanFrontend soptions fs rf

-- Display a frame, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
getKey :: ClientOptions -> FrontSetup -> RawFrontend -> [K.KM] -> FrameForall
       -> IO KMP
getKey soptions fs rf@RawFrontend{fchanKey} keys frame = do
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
    else getKey soptions fs rf keys frame

-- Read UI requests from the client and send them to the frontend,
fchanFrontend :: ClientOptions -> FrontSetup -> RawFrontend -> ChanFrontend
fchanFrontend soptions fs@FrontSetup{..} rf =
  ChanFrontend $ \case
    FrontFrame{..} -> display rf frontFrame
    FrontDelay k -> modifyMVar_ fdelay $ return . (+ k)
    FrontKey{..} -> getKey soptions fs rf frontKeyKeys frontKeyFrame
    FrontPressed -> do
      noKeysPending <- STM.atomically $ STM.isEmptyTQueue (fchanKey rf)
      return $! not noKeysPending
    FrontDiscard ->
      void $ STM.atomically $ STM.tryReadTQueue (fchanKey rf)
    FrontAdd kmp -> STM.atomically $ STM.writeTQueue (fchanKey rf) kmp
    FrontAutoYes b -> writeIORef fautoYesRef b
    FrontShutdown -> do
      cancel fasyncTimeout
      -- In case the last frame display is pending:
      void $ tryTakeMVar $ fshowNow rf
      fshutdown rf
    FrontPrintScreen -> fprintScreen rf

display :: RawFrontend -> FrameForall -> IO ()
display rf@RawFrontend{fshowNow} frontFrame = do
  let lxsize = fst normalLevelBound + 1
      lysize = snd normalLevelBound + 1
      canvasLength = lysize + 3
      new :: forall s. ST s (G.Mutable U.Vector s Word32)
      new = do
        v <- VM.replicate (lxsize * canvasLength)
                          (Color.attrCharW32 Color.spaceAttrW32)
        unFrameForall frontFrame v
        return v
      singleFrame = PointArray.Array lxsize canvasLength (U.create new)
  putMVar fshowNow () -- 1. wait for permission to display; 3. ack
  fdisplay rf $ SingleFrame singleFrame

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
frontendName :: String
frontendName = Chosen.frontendName

lazyStartup :: IO RawFrontend
lazyStartup = createRawFrontend (\_ -> return ()) (return ())

nullStartup :: IO RawFrontend
nullStartup = createRawFrontend seqFrame (return ())

seqFrame :: SingleFrame -> IO ()
seqFrame SingleFrame{singleFrame} =
  let seqAttr () attr = Color.colorToRGB (Color.fgFromW32 attr)
                        `seq` Color.bgFromW32 attr
                        `seq` Color.charFromW32 attr == ' '
                        `seq` ()
  in return $! PointArray.foldlA' seqAttr () singleFrame
