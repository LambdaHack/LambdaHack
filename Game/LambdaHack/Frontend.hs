-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived  operations.
module Game.LambdaHack.Frontend
  ( -- * Re-exported part of the raw frontend
    frontendName
    -- * Derived operation
  , startupF
    -- * Connection channels
  , ChanFrontend, FrontReq(..), ConnMulti(..), connMulti
  ) where

import Control.Concurrent
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, writeTQueue)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.Text.IO as T
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Frontend.Chosen
import Game.LambdaHack.Utils.Thread

type ChanFrontend = TQueue K.KM

-- | The first component is the number of UI players at game start.
type FromMulti = MVar (FactionId -> ChanFrontend)

type ToMulti = TQueue (FactionId, FrontReq)

data FrontReq =
    FrontFrame {frontAc :: !AcFrame}
      -- ^ show a frame, if the fid active, or save it to the client's queue
  | FrontKey {frontKM :: ![K.KM], frontFr :: !SingleFrame}
      -- ^ flush frames, possibly show fadeout/fadein and ask for a keypress
  | FrontSlides {frontClear :: ![K.KM], frontSlides :: ![SingleFrame]}
      -- ^ show a whole slideshow without interleaving with other clients
  | FrontFinish
      -- ^ exit frontend loop

-- | Multiplex connection channels, for the case of a frontend shared
-- among clients. This is transparent to the clients themselves.
data ConnMulti = ConnMulti
  { fromMulti :: !FromMulti
  , toMulti   :: !ToMulti
  }

startupF :: DebugModeCli -> (Maybe (MVar ()) -> IO ()) -> IO ()
startupF dbg cont =
  (if sfrontendNull dbg then nullStartup
   else if sfrontendStd dbg then stdStartup
        else chosenStartup) dbg $ \fs -> do
    let debugPrint t = when (sdbgMsgCli dbg) $ do
          T.hPutStrLn stderr t
          hFlush stderr
    children <- newMVar []
    void $ forkChild children $ loopFrontend fs connMulti
    cont (fescMVar fs)
    debugPrint "Server shuts down"
    let toF = toMulti connMulti
    -- TODO: instead of this, wait for clients to send FrontFinish or timeout
    atomically $ writeTQueue toF (toEnum 0 {-hack-}, FrontFinish)
    waitForChildren children
    debugPrint "Frontend shuts down"

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: Frontend -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey fs [] frame = fpromptGetKey fs frame
promptGetKey fs keys frame = do
  km <- fpromptGetKey fs frame
  if km `elem` keys
    then return km
    else promptGetKey fs keys frame

-- TODO: avoid unsafePerformIO; but server state is a wrong idea, too
connMulti :: ConnMulti
{-# NOINLINE connMulti #-}
connMulti = unsafePerformIO $ do
  fromMulti <- newMVar undefined
  toMulti <- newTQueueIO
  return $! ConnMulti{..}

getConfirmGeneric :: Frontend -> [K.KM] -> SingleFrame -> IO Bool
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

displayAc :: Frontend -> AcFrame -> IO ()
displayAc fs (AcConfirm fr) = void $ getConfirmGeneric fs [] fr
displayAc fs (AcRunning fr) = fdisplay fs True (Just fr)
displayAc fs (AcNormal fr) = fdisplay fs False (Just fr)
displayAc fs AcDelay = fdisplay fs False Nothing

-- Read UI requests from clients and send them to the frontend,
-- separated by fadeout/fadein frame sequences, if needed.
-- There may be many UI clients, but this function is only ever
-- executed on one thread, so the frontend receives requests
-- in a sequential way, without any random interleaving.
loopFrontend :: Frontend -> ConnMulti -> IO ()
loopFrontend fs ConnMulti{..} = loop
 where
  writeKM :: FactionId -> K.KM -> IO ()
  writeKM fid km = do
    fM <- takeMVar fromMulti
    let chanFrontend = fM fid
    atomically $ STM.writeTQueue chanFrontend km
    putMVar fromMulti fM

  loop :: IO ()
  loop = do
    (fid, efr) <- atomically $ STM.readTQueue toMulti
    case efr of
      FrontFrame{..} -> do
        displayAc fs frontAc
        loop
      FrontKey{..} -> do
        km <- promptGetKey fs frontKM frontFr
        writeKM fid km
        loop
      FrontSlides{frontSlides = []} -> do
        -- Hack.
        fsyncFrames fs
        writeKM fid K.spaceKey
        loop
      FrontSlides{..} -> do
        let displayFrs frs =
              case frs of
                [] -> assert `failure` "null slides" `twith` fid
                [x] -> do
                  fdisplay fs False (Just x)
                  writeKM fid K.spaceKey
                x : xs -> do
                  go <- getConfirmGeneric fs frontClear x
                  if go
                    then displayFrs xs
                    else writeKM fid K.escKey
        displayFrs frontSlides
        loop
      FrontFinish ->
        return ()
        -- Do not loop again.
