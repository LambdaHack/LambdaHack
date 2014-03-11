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
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Key as K
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Frontend.Chosen
import Game.LambdaHack.Utils.LQueue
import Game.LambdaHack.Utils.Thread

type ChanFrontend = TQueue K.KM

-- | The first component is the number of UI players at game start.
type FromMulti = MVar (Int, FactionId -> (ChanFrontend, Text))

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

type ReqMap = EM.EnumMap FactionId (LQueue AcFrame)

-- | Multiplex connection channels, for the case of a frontend shared
-- among clients. This is transparent to the clients themselves.
data ConnMulti = ConnMulti
  { fromMulti :: !FromMulti
  , toMulti   :: !ToMulti
  }

startupF :: DebugModeCli -> (Maybe (MVar ()) -> IO ()) -> IO ()
startupF dbg cont =
  (if sfrontendNo dbg then noStartup
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

flushFrames :: Frontend -> FactionId -> ReqMap -> IO ReqMap
flushFrames fs fid reqMap = do
  let queue = toListLQueue $ fromMaybe newLQueue $ EM.lookup fid reqMap
      reqMap2 = EM.delete fid reqMap
  mapM_ (displayAc fs) queue
  return $! reqMap2

displayAc :: Frontend -> AcFrame -> IO ()
displayAc fs (AcConfirm fr) = void $ getConfirmGeneric fs [] fr
displayAc fs (AcRunning fr) = fdisplay fs True (Just fr)
displayAc fs (AcNormal fr) = fdisplay fs False (Just fr)
displayAc fs AcDelay = fdisplay fs False Nothing

getSingleFrame :: AcFrame -> Maybe SingleFrame
getSingleFrame (AcConfirm fr) = Just fr
getSingleFrame (AcRunning fr) = Just fr
getSingleFrame (AcNormal fr) = Just fr
getSingleFrame AcDelay = Nothing

toSingles :: FactionId -> ReqMap -> [SingleFrame]
toSingles fid reqMap =
  let queue = toListLQueue $ fromMaybe newLQueue $ EM.lookup fid reqMap
  in mapMaybe getSingleFrame queue

fadeF :: Frontend -> Bool -> FactionId -> Text -> SingleFrame -> IO ()
fadeF fs out side pname frame = do
  let topRight = True
      lxsize = xsizeSingleFrame frame
      lysize = ysizeSingleFrame frame
      msg = "Player" <+> tshow (fromEnum side) <> ","
            <+> pname <> (if T.null pname then "" else ",")
            <+> "get ready!"
  animMap <- rndToIO $ fadeout out topRight lxsize lysize
  let sfTop = truncateToOverlay lxsize msg
      basicFrame = frame {sfTop}  -- overwrite the whole original overlay
      animFrs = renderAnim lxsize lysize basicFrame animMap
      frs | out = animFrs
            -- Empty frame to mark the fade-in end,
            -- to trim only to here if SPACE pressed.
          | otherwise = animFrs ++ [Nothing]
  mapM_ (fdisplay fs False) frs

insertFr :: FactionId -> AcFrame -> ReqMap -> ReqMap
insertFr fid fr reqMap =
  let queue = fromMaybe newLQueue $ EM.lookup fid reqMap
  in EM.insert fid (writeLQueue queue fr) reqMap

-- Read UI requests from clients and send them to the frontend,
-- separated by fadeout/fadein frame sequences, if needed.
-- There may be many UI clients, but this function is only ever
-- executed on one thread, so the frontend receives requests
-- in a sequential way, without any random interleaving.
loopFrontend :: Frontend -> ConnMulti -> IO ()
loopFrontend fs ConnMulti{..} = loop Nothing EM.empty
 where
  writeKM :: FactionId -> K.KM -> IO ()
  writeKM fid km = do
    fM <- takeMVar fromMulti
    let chanFrontend = fst $ snd fM fid
    atomically $ STM.writeTQueue chanFrontend km
    putMVar fromMulti fM

  flushFade :: SingleFrame -> Maybe (FactionId, SingleFrame)
            -> ReqMap -> FactionId
            -> IO ReqMap
  flushFade frontFr oldFidFrame reqMap fid =
    if Just fid == fmap fst oldFidFrame then
      return reqMap
    else do
      (nU, fCT) <- readMVar fromMulti
      let pname = snd $ fCT fid
      reqMap2 <- case oldFidFrame of
        Nothing -> return reqMap
        Just (oldFid, oldFrame) -> do
          reqMap2 <- flushFrames fs oldFid reqMap
          let singles = toSingles oldFid reqMap  -- not @reqMap2@!
              lastFrame = fromMaybe oldFrame $ listToMaybe $ reverse singles
          fadeF fs True fid pname lastFrame
          return $! reqMap2
      let singles = toSingles fid reqMap2
          firstFrame = fromMaybe frontFr $ listToMaybe singles
      -- TODO: @nU@ is unreliable, when some of UI players die;
      -- in the result a single players has unneeded fadeins.
      unless (isNothing oldFidFrame && nU < 2) $
        fadeF fs False fid pname firstFrame
      flushFrames fs fid reqMap2

  loop :: Maybe (FactionId, SingleFrame) -> ReqMap -> IO ()
  loop oldFidFrame reqMap = do
    (fid, efr) <- atomically $ STM.readTQueue toMulti
    case efr of
      FrontFrame{..} | Just (oldFid, oldFrame) <- oldFidFrame
                     , fid == oldFid -> do
        displayAc fs frontAc
        let frame = fromMaybe oldFrame $ getSingleFrame frontAc
        loop (Just (fid, frame)) reqMap
      FrontFrame{..} -> do
        let reqMap2 = insertFr fid frontAc reqMap
        loop oldFidFrame reqMap2
      FrontKey{..} -> do
        reqMap2 <- flushFade frontFr oldFidFrame reqMap fid
        km <- promptGetKey fs frontKM frontFr
        writeKM fid km
        loop (Just (fid, frontFr)) reqMap2
      FrontSlides{frontSlides = []} -> do
        -- Hack.
        fsyncFrames fs
        writeKM fid K.spaceKey
        loop oldFidFrame reqMap
      FrontSlides{frontSlides = frontSlides@(fr1 : _), ..} -> do
        reqMap2 <- flushFade fr1 oldFidFrame reqMap fid
        let displayFrs frs =
              case frs of
                [] -> assert `failure` "null slides" `twith` fid
                [x] -> do
                  fdisplay fs False (Just x)
                  writeKM fid K.spaceKey
                  return x
                x : xs -> do
                  go <- getConfirmGeneric fs frontClear x
                  if go then displayFrs xs
                  else do
                    writeKM fid K.escKey
                    return x
        frLast <- displayFrs frontSlides
        loop (Just (fid, frLast)) reqMap2
      FrontFinish ->
        return ()  -- TODO: apply modified flushFrames to fid
        -- Do not loop again.
