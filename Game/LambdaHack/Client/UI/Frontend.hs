{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived operations.
module Game.LambdaHack.Client.UI.Frontend
  ( -- * Connection types
    FrontReq(..), RawFrontend(..), ChanFrontend
    -- * Re-exported part of the raw frontend
  , frontendName
    -- * Derived operations
  , startupF, chanFrontend
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import Data.IORef
import qualified Data.Text.IO as T
import System.IO

import Data.Maybe
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Frontend.Chosen
import Game.LambdaHack.Common.ClientOptions

-- | The instructions sent by clients to the raw frontend.
data FrontReq :: * -> * where
  FrontNormalFrame :: {frontFrame :: !SingleFrame} -> FrontReq ()
    -- ^ show a frame
  FrontDelay :: FrontReq ()
    -- ^ perform a single explicit delay
  FrontKey :: { frontKM :: ![K.KM]
              , frontFr :: !SingleFrame } -> FrontReq K.KM
    -- ^ flush frames, display a frame and ask for a keypress
  FrontSlides :: { frontClear   :: ![K.KM]
                 , frontSlides  :: ![SingleFrame]
                 , frontFromTop :: !(Maybe Bool) } -> FrontReq K.KM
    -- ^ flush frames and disply a whole slideshow
  FrontAutoYes :: !Bool -> FrontReq ()
    -- ^ set in the frontend that it should auto-answer prompts

-- | Connection channel between a frontend and a client. Frontend acts
-- as a server, serving keys, etc., when given frames to display.
type ChanFrontend = forall a. FrontReq a -> IO a

-- | Initialize the frontend and apply the given continuation to the results
-- of the initialization.
startupF :: DebugModeCli  -- ^ debug settings
         -> (RawFrontend -> IO ())  -- ^ continuation
         -> IO ()
startupF dbg cont =
  (if sfrontendNull dbg then nullStartup
   else if sfrontendStd dbg then stdStartup
        else chosenStartup) dbg $ \fs -> do
    cont fs
    let debugPrint t = when (sdbgMsgCli dbg) $ do
          T.hPutStrLn stderr t
          hFlush stderr
    debugPrint "Frontend shuts down"

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: RawFrontend -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey fs [] frame = fpromptGetKey fs frame
promptGetKey fs keys frame = do
  km <- fpromptGetKey fs frame
  if km{K.pointer=Nothing} `elem` keys
    then return km
    else promptGetKey fs keys frame

getConfirmGeneric :: RawFrontend -> [K.KM] -> SingleFrame -> IO K.KM
getConfirmGeneric fs clearKeys frame = do
  autoYes <- readIORef (fautoYesRef fs)
  if autoYes then do
    fdisplay fs (Just frame)
    return K.spaceKM
  else do
    let extraKeys = [K.spaceKM, K.escKM, K.pgupKM, K.pgdnKM]
    promptGetKey fs (clearKeys ++ extraKeys) frame

-- Read UI requests from the client and send them to the frontend,
chanFrontend :: RawFrontend -> ChanFrontend
chanFrontend fs req = case req of
  FrontNormalFrame{..} -> do
    fdisplay fs (Just frontFrame)
    return ()
  FrontDelay -> do
    fdisplay fs Nothing
    return ()
  FrontKey{..} -> do
    promptGetKey fs frontKM frontFr
  FrontSlides{frontSlides = []} -> do
    -- Hack.
    fsyncFrames fs
    return K.spaceKM
  FrontSlides{..} -> do
    let displayFrs frs srf = case frs of
          [] -> assert `failure` "null slides" `twith` frs
          [x] | isNothing frontFromTop -> do
            fdisplay fs (Just x)
            return K.spaceKM
          x : xs -> do
            K.KM{..} <- getConfirmGeneric fs frontClear x
            case key of
              K.Esc -> return K.escKM
              K.PgUp -> case srf of
                [] -> displayFrs frs srf
                y : ys -> displayFrs (y : frs) ys
              K.Space -> case xs of
                [] -> return K.escKM  -- hack
                _ -> displayFrs xs (x : srf)
              _ -> case xs of  -- K.PgDn and any other permitted key
                [] -> displayFrs frs srf
                _ -> displayFrs xs (x : srf)
    case (frontFromTop, reverse frontSlides) of
      (Just False, r : rs) -> displayFrs [r] rs
      _ -> displayFrs frontSlides []
  FrontAutoYes b ->
    writeIORef (fautoYesRef fs) b
