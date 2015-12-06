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
  }

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: IORef Bool -> RawFrontend -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey _ rf [] frame = fpromptGetKey rf frame
promptGetKey fautoYesRef rf keys frame = do
  autoYes <- readIORef fautoYesRef
  if autoYes && K.spaceKM `elem` keys then do
    fdisplay rf frame
    return K.spaceKM
  else do
    km <- fpromptGetKey rf frame
    if km{K.pointer=Nothing} `elem` keys
    then return km
    else promptGetKey fautoYesRef rf keys frame

-- | Read UI requests from the client and send them to the frontend,
fchanFrontend :: FSession -> RawFrontend -> ChanFrontend
fchanFrontend FSession{..} rf = ChanFrontend $ \req -> case req of
  FrontFrame{..} -> fdisplay rf frontFrame
  FrontDelay -> return ()
  FrontKey{..} -> promptGetKey fautoYesRef rf frontKeyKeys frontKeyFrame
  FrontSync -> return ()  -- TODO
  FrontPressed -> do
    mUnit <- tryTakeMVar (fkeyPressed rf)
    return $! isJust mUnit
  FrontAutoYes b -> writeIORef fautoYesRef b
  FrontShutdown -> fshutdown rf

-- | The name of the chosen frontend.
frontendName :: String
frontendName = Chosen.frontendName

nullStartup :: IO RawFrontend
nullStartup = do
  fkeyPressed <- newEmptyMVar
  return $! RawFrontend
    { fdisplay = \_ -> return ()
    , fpromptGetKey = \_ -> return K.escKM
    , fshutdown = return ()
    , fkeyPressed
    }

chanFrontend :: DebugModeCli -> IO ChanFrontend
chanFrontend sdebugCli = do
  let startup | sfrontendNull sdebugCli = nullStartup
              | sfrontendStd sdebugCli = Std.startup sdebugCli
              | otherwise = Chosen.startup sdebugCli
  rf <- startup
  fautoYesRef <- newIORef $ not $ sdisableAutoYes sdebugCli
  let fs = FSession{..}
  return $ fchanFrontend fs rf
