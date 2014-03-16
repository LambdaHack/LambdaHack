-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived operations.
module Game.LambdaHack.Client.UI.Frontend
  ( -- * Connection types.
    FrontReq(..), ChanFrontend(..)
    -- * Re-exported part of the raw frontend
  , frontendName
    -- * A derived operation
  , startupF
  ) where

import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.Text.IO as T
import System.IO

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Frontend.Chosen
import Game.LambdaHack.Common.Animation

data FrontReq =
    FrontNormalFrame {frontFrame :: !SingleFrame}
      -- ^ show a frame
  | FrontRunningFrame {frontFrame :: !SingleFrame}
      -- ^ show a frame in running mode (don't insert delay between frames)
  | FrontDelay
      -- ^ perform a single explicit delay
  | FrontKey {frontKM :: ![K.KM], frontFr :: !SingleFrame}
      -- ^ flush frames, possibly show fadeout/fadein and ask for a keypress
  | FrontSlides {frontClear :: ![K.KM], frontSlides :: ![SingleFrame]}
      -- ^ show a whole slideshow without interleaving with other clients
  | FrontFinish
      -- ^ exit frontend loop

-- | Connection channel between a frontend and a client. Frontend acts
-- as a server, serving keys, when given frames to display.
data ChanFrontend = ChanFrontend
  { responseF :: !(STM.TQueue K.KM)
  , requestF  :: !(STM.TQueue FrontReq)
  }

startupF :: DebugModeCli
         -> (Maybe (MVar ()) -> (ChanFrontend -> IO ()) -> IO ())
         -> IO ()
startupF dbg cont =
  (if sfrontendNull dbg then nullStartup
   else if sfrontendStd dbg then stdStartup
        else chosenStartup) dbg $ \fs -> do
    cont (fescMVar fs) (loopFrontend fs)
    let debugPrint t = when (sdbgMsgCli dbg) $ do
          T.hPutStrLn stderr t
          hFlush stderr
    debugPrint "Server shuts down"

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: RawFrontend -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey fs [] frame = fpromptGetKey fs frame
promptGetKey fs keys frame = do
  km <- fpromptGetKey fs frame
  if km `elem` keys
    then return km
    else promptGetKey fs keys frame

getConfirmGeneric :: RawFrontend -> [K.KM] -> SingleFrame -> IO Bool
getConfirmGeneric fs clearKeys frame = do
  let DebugModeCli{snoMore} = fdebugCli fs
  -- TODO: turn noMore off somehow when faction not under computer control;
  -- perhaps by adding a FrontReq request that turns it off/on?
  if snoMore then do
    fdisplay fs True (Just frame)
    return True
  else do
    let extraKeys = [K.spaceKey, K.escKey]
    km <- promptGetKey fs (clearKeys ++ extraKeys) frame
    return $! km /= K.escKey

-- Read UI requests from the client and send them to the frontend,
loopFrontend :: RawFrontend -> ChanFrontend -> IO ()
loopFrontend fs ChanFrontend{..} = loop
 where
  writeKM :: K.KM -> IO ()
  writeKM km = STM.atomically $ STM.writeTQueue responseF km

  loop :: IO ()
  loop = do
    efr <- STM.atomically $ STM.readTQueue requestF
    case efr of
      FrontNormalFrame{..} -> do
        fdisplay fs False (Just frontFrame)
        loop
      FrontRunningFrame{..} -> do
        fdisplay fs True (Just frontFrame)
        loop
      FrontDelay -> do
        fdisplay fs False Nothing
        loop
      FrontKey{..} -> do
        km <- promptGetKey fs frontKM frontFr
        writeKM km
        loop
      FrontSlides{frontSlides = []} -> do
        -- Hack.
        fsyncFrames fs
        writeKM K.spaceKey
        loop
      FrontSlides{..} -> do
        let displayFrs frs =
              case frs of
                [] -> assert `failure` "null slides" `twith` frs
                [x] -> do
                  fdisplay fs False (Just x)
                  writeKM K.spaceKey
                x : xs -> do
                  go <- getConfirmGeneric fs frontClear x
                  if go
                    then displayFrs xs
                    else writeKM K.escKey
        displayFrs frontSlides
        loop
      FrontFinish ->
        return ()
        -- Do not loop again.
