-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived  operations.
module Game.LambdaHack.Frontend
  ( -- * Re-exported part of the raw frontend
    frontendName
    -- * Derived operation
  , startupF
    -- * Connection channels
  , FrontReq(..), ConnMulti(..)
  ) where

import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.Text.IO as T
import System.IO

import Game.LambdaHack.Common.Animation
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Frontend.Chosen

type FromMulti = STM.TQueue K.KM

type ToMulti = STM.TQueue FrontReq

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

startupF :: DebugModeCli
         -> (Maybe (MVar ()) -> (ConnMulti -> IO ()) -> IO ())
         -> IO ()
startupF dbg cont =
  (if sfrontendNull dbg then nullStartup
   else if sfrontendStd dbg then stdStartup
        else chosenStartup) dbg $ \fs -> do
    let debugPrint t = when (sdbgMsgCli dbg) $ do
          T.hPutStrLn stderr t
          hFlush stderr
    cont (fescMVar fs) (loopFrontend fs)
    debugPrint "Server shuts down"

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: Frontend -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey fs [] frame = fpromptGetKey fs frame
promptGetKey fs keys frame = do
  km <- fpromptGetKey fs frame
  if km `elem` keys
    then return km
    else promptGetKey fs keys frame

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
  writeKM :: K.KM -> IO ()
  writeKM km = STM.atomically $ STM.writeTQueue fromMulti km

  loop :: IO ()
  loop = do
    efr <- STM.atomically $ STM.readTQueue toMulti
    case efr of
      FrontFrame{..} -> do
        displayAc fs frontAc
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
                [] -> assert `failure` "null slides" `twith` ()
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
