-- | Text frontend based on Gtk.
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Game.LambdaHack.Display.Gtk
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , display, nextEvent, promptGetKey
    -- * Frontend administration tools
  , frontendName, startup, shutdown
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception (finally)
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk hiding (Point)
import qualified Data.List as L
import Data.IORef
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import System.Time

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.LQueue
import qualified Game.LambdaHack.Key as K (Key(..), keyTranslate, Modifier(..))
import qualified Game.LambdaHack.Color as Color

data FrameState =
    FPushed  -- frames stored in a queue, to be drawn in equal time intervals
      { fpushed :: !(LQueue (Maybe GtkFrame))  -- ^ screen output channel
      , fshown  :: !GtkFrame                   -- ^ last full frame shown
      }
  | FSet  -- a single frame stored, to be drawn when a keypress is requested
      { fsetFrame :: !(Maybe GtkFrame)  -- ^ frame to draw at input key
      }
  | FNone  -- no frames stored

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { sview       :: !TextView                    -- ^ the widget to draw to
  , stags       :: !(M.Map Color.Attr TextTag)  -- ^ text color tags for fg/bg
  , schanKey    :: !(Chan (K.Key, K.Modifier))  -- ^ channel for keyboard input
  , sframeState :: !(MVar FrameState)           -- ^ state of the frame machine
  , slastFull   :: !(IORef (GtkFrame, Bool))
       -- ^ most recent full (not empty, not repeated) frame received
       -- and if any empty frame followed it
  }

data GtkFrame = GtkFrame
  { gfChar :: !BS.ByteString
  , gfAttr :: ![[TextTag]]
  }
  deriving Eq

dummyFrame :: GtkFrame
dummyFrame = GtkFrame BS.empty []

-- | Remove all but the last element of the frame queue.
-- The kept last element ensures that slastFull is not invalidated.
trimQueue :: FrontendSession -> IO ()
trimQueue FrontendSession{sframeState} = do
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} ->
      putMVar sframeState FPushed{fpushed = trimLQueue fpushed, ..}
    _ ->
      putMVar sframeState fs

-- | The name of the frontend.
frontendName :: String
frontendName = "gtk"

-- | Spawns the gtk input and output thread, which spawns all the other
-- required threads. We create a separate thread for gtk to minimize
-- communication with the heavy main thread. The other threads have to be
-- spawned after gtk is initialized, because they call @postGUIAsync@,
-- and need 'sview' and 'stags'.
startup :: String -> (FrontendSession -> IO ()) -> IO ()
startup configFont k = do
  mv <- newEmptyMVar
  -- Fork the gtk input and output thread.
  void $ forkIO (runGtk configFont k `finally` putMVar mv ())
  takeMVar mv

-- | Sets up and starts the main GTK loop providing input and output.
runGtk :: String ->  (FrontendSession -> IO ()) -> IO ()
runGtk configFont k = do
  -- Init GUI.
  unsafeInitGUIForThreadedRTS
  -- Text attributes.
  ttt <- textTagTableNew
  stags <- fmap M.fromList $
             mapM (\ ak -> do
                      tt <- textTagNew Nothing
                      textTagTableAdd ttt tt
                      doAttr tt ak
                      return (ak, tt))
               [ Color.Attr{fg, bg}
               | fg <- [minBound..maxBound], bg <- Color.legalBG ]
  -- Text buffer.
  tb <- textBufferNew (Just ttt)
  -- Create text view. TODO: use GtkLayout or DrawingArea instead of TextView?
  sview <- textViewNewWithBuffer tb
  textViewSetEditable sview False
  textViewSetCursorVisible sview False
  -- Set up the channel for keyboard input.
  schanKey <- newChan
  -- Set up the frame state.
  let frameState = FNone
  -- Create the session record.
  sframeState <- newMVar frameState
  slastFull <- newIORef (dummyFrame, False)
  let sess = FrontendSession{..}
  -- Fork the game logic thread.
  forkIO $ k sess
  -- Fork the thread that periodically draws a frame from a queue, if any.
  forkIO $ pollFrames sess Nothing
  -- Fill the keyboard channel.
  sview `on` keyPressEvent $ do
    n <- eventKeyName
    mods <- eventModifier
    let !key = K.keyTranslate n
        !modifier = modifierTranslate mods
    liftIO $ do
      unless (deadKey n) $ do
        -- Drop all the old frames. Some more may be arriving at the same time.
        trimQueue sess
        -- Store the key in the channel.
        writeChan schanKey (key, modifier)
      return True
  -- Set the font specified in config, if any.
  f <- fontDescriptionFromString configFont
  widgetModifyFont sview (Just f)
  -- Prepare font chooser dialog.
  currentfont <- newIORef f
  sview `on` buttonPressEvent $ do
    but <- eventButton
    liftIO $ case but of
      RightButton -> do
        fsd <- fontSelectionDialogNew "Choose font"
        cf  <- readIORef currentfont  -- TODO: "Terminus,Monospace" fails
        fds <- fontDescriptionToString cf
        fontSelectionDialogSetFontName fsd fds
        fontSelectionDialogSetPreviewText fsd "eee...@.##+##"
        resp <- dialogRun fsd
        when (resp == ResponseOk) $ do
          fn <- fontSelectionDialogGetFontName fsd
          case fn of
            Just fn' -> do
              fd <- fontDescriptionFromString fn'
              writeIORef currentfont fd
              widgetModifyFont sview (Just fd)
            Nothing  -> return ()
        widgetDestroy fsd
        return True
      _ -> return False
  -- Modify default colours.
  let black = Color minBound minBound minBound  -- Color.defBG == Color.Black
      white = Color 0xC500 0xBC00 0xB800        -- Color.defFG == Color.White
  widgetModifyBase sview StateNormal black
  widgetModifyText sview StateNormal white
  -- Set up the main window.
  w <- windowNew
  containerAdd w sview
  onDestroy w mainQuit
  widgetShowAll w
  -- Wait until the other thread draws something and show the window.
  yield
  mainGUI

-- | Shuts down the frontend cleanly.
shutdown :: FrontendSession -> IO ()
shutdown _ = mainQuit

-- | Output to the screen via the frontend.
output :: FrontendSession  -- ^ frontend session data
       -> GtkFrame         -- ^ the screen frame to draw
       -> IO ()
output FrontendSession{sview, stags} GtkFrame{..} = do  -- new frame
  tb <- textViewGetBuffer sview
  let attrs = L.zip [0..] gfAttr
      defaultAttr = stags M.! Color.defaultAttr
  textBufferSetByteString tb gfChar
  mapM_ (setTo tb defaultAttr 0) attrs

setTo :: TextBuffer -> TextTag -> Int -> (Int, [TextTag]) -> IO ()
setTo _  _   _  (_,  [])         = return ()
setTo tb defaultAttr lx (ly, attr:attrs) = do
  ib <- textBufferGetIterAtLineOffset tb (ly + 1) lx
  ie <- textIterCopy ib
  let setIter :: TextTag -> Int -> [TextTag] -> IO ()
      setIter previous repetitions [] = do
        textIterForwardChars ie repetitions
        when (previous /= defaultAttr) $
          textBufferApplyTag tb previous ib ie
      setIter previous repetitions (a:as)
        | a == previous =
            setIter a (repetitions + 1) as
        | otherwise = do
            textIterForwardChars ie repetitions
            when (previous /= defaultAttr) $
              textBufferApplyTag tb previous ib ie
            textIterForwardChars ib repetitions
            setIter a 1 as
  setIter attr 1 attrs

-- TODO: configure
-- | Maximal frames per second.
maxFps :: Int
maxFps = 15

-- | Maximal polls per second.
maxPolls :: Int
maxPolls = let maxP = 120
           in assert (maxP >= 2 * maxFps `blame` (maxP, maxFps)) $
              maxP

-- | Add a given number of microseconds to time.
addTime :: ClockTime -> Int -> ClockTime
addTime (TOD s p) ms = TOD s (p + fromIntegral (ms * 1000000))

-- | The difference between the first and the second time, in microseconds.
diffTime :: ClockTime -> ClockTime -> Int
diffTime (TOD s1 p1) (TOD s2 p2) =
  (fromIntegral $ s1 - s2) * 1000000 +
  (fromIntegral $ p1 - p2) `div` 1000000

-- | Poll the frame queue often and draw frames at fixed intervals.
pollFrames :: FrontendSession -> Maybe ClockTime -> IO ()
pollFrames sess (Just setTime) = do
  -- Check if the time is up.
  curTime <- getClockTime
  let diffT = diffTime setTime curTime
  if diffT > 1000000 `div` maxPolls
    then do
      -- Delay half of the time difference.
      threadDelay $ diffTime curTime setTime `div` 2
      pollFrames sess $ Just setTime
    else
      -- Don't delay, because time is up!
      pollFrames sess Nothing
pollFrames sess@FrontendSession{sframeState} Nothing = do
  -- Time time is up, check if we actually wait for anyting.
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} ->
      case tryReadLQueue fpushed of
        Just (Just frame, queue) -> do
          -- The frame has arrived so send it for drawing and update delay.
          putMVar sframeState FPushed{fpushed = queue, fshown = frame}
          postGUIAsync $ output sess frame
          curTime <- getClockTime
          threadDelay $ 1000000 `div` (maxFps * 2)
          pollFrames sess $ Just $ addTime curTime $ 1000000 `div` maxFps
        Just (Nothing, queue) -> do
          -- Delay requested via an empty frame.
          putMVar sframeState FPushed{fpushed = queue, ..}
          curTime <- getClockTime
          -- There is no problem if the delay is a bit delayed.
          threadDelay $ 1000000 `div` maxFps
          pollFrames sess $ Just $ addTime curTime $ 1000000 `div` maxFps
        Nothing -> do
          -- The queue is empty, the game logic thread lags.
          putMVar sframeState fs
          -- Time time is up, the game thread is going to send a frame,
          -- (otherwise it would change the state), so poll often.
          threadDelay $ 1000000 `div` maxPolls
          pollFrames sess Nothing
    _ -> do
      putMVar sframeState fs
      -- Not in the Push state, so poll lazily to catch the next state change.
      -- The slow polling also gives the game logic a head start
      -- in creating frames in case one of the further frames is slow
      -- to generate and would normally cause a jerky delay in drawing.
      threadDelay $ 1000000 `div` (maxFps * 2)
      pollFrames sess Nothing

-- | Add a frame to be drawn.
display :: FrontendSession -> Bool -> Bool -> Maybe Color.SingleFrame -> IO ()
display sess True noDelay rawFrame = pushFrame sess noDelay rawFrame
display sess False _ (Just rawFrame) = setFrame sess rawFrame
display _ _ _ _ = assert `failure` "display: empty frame to be set"

-- | Add a game screen frame to the frame drawing channel.
pushFrame :: FrontendSession -> Bool -> Maybe Color.SingleFrame -> IO ()
pushFrame sess@FrontendSession{sframeState, slastFull} noDelay rawFrame = do
  -- Full evaluation and comparison is done outside the mvar lock.
  (lastFrame, anyFollowed) <- readIORef slastFull
  let frame = maybe Nothing (Just . evalFrame sess) rawFrame
      nextFrame =
        if frame == Just lastFrame
        then Nothing  -- no sense repeating
        else frame
  -- Now we take the lock.
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} ->
      if (isNothing nextFrame && anyFollowed)
      then putMVar sframeState fs  -- old news
      else putMVar sframeState
             FPushed{fpushed = writeLQueue fpushed nextFrame, ..}
    FSet{} -> assert `failure` "pushFrame: FSet, expecting FPushed or FNone"
    FNone ->
      -- Never start playing with an empty frame.
      let fpushed = if isJust nextFrame
                    then writeLQueue newLQueue nextFrame
                    else newLQueue
          fshown = dummyFrame
      in putMVar sframeState FPushed{..}
  yield  -- drawing has priority
  case nextFrame of
    Nothing -> writeIORef slastFull (lastFrame, True)
    Just f  -> writeIORef slastFull (f, noDelay)

evalFrame :: FrontendSession -> Color.SingleFrame -> GtkFrame
evalFrame FrontendSession{stags} Color.SingleFrame{..} =
  let levelChar = L.map (L.map Color.acChar) sfLevel
      gfChar = BS.pack $ L.intercalate "\n" $ sfTop : levelChar ++ [sfBottom]
      -- Strict version of @L.map (L.map ((stags M.!) . fst)) sfLevel@.
      gfAttr  = L.reverse $ L.foldl' ff [] sfLevel
      ff ll l = (L.reverse $ L.foldl' f [] l) : ll
      f l ac  = let !tag = stags M.! Color.acAttr ac in tag : l
  in GtkFrame{..}

-- | Set the frame to be drawn at the next invocation of @nextEvent@.
-- Fail if there is already a frame pushed or set.
-- Don't show the frame if it's unchanged vs the previous.
setFrame :: FrontendSession -> Color.SingleFrame -> IO ()
setFrame sess@FrontendSession{slastFull, sframeState} rawFrame = do
  -- Full evaluation and comparison is done outside the mvar lock.
  (lastFrame, _) <- readIORef slastFull
  let frame = evalFrame sess rawFrame
      fsetFrame =
        if frame == lastFrame
        then Nothing  -- no sense repeating
        else Just frame
  -- Now we take the lock.
  fs <- takeMVar sframeState
  case fs of
    FPushed{} -> assert `failure` "setFrame: FPushed, expecting FNone"
    FSet{} -> assert `failure` "setFrame: FSet, expecting FNone"
    FNone -> do
      -- Update the last received frame with the processed frame.
      -- There is no race condition, because we are on the same thread
      -- as pushFrame.
      maybe (return ()) (\ fr -> writeIORef slastFull (fr, False)) fsetFrame
  -- Store the frame. Release the lock.
  putMVar sframeState FSet{..}

-- | Set key via the frontend. Fail if there is no frame to show
-- to the player as a prompt for the keypress.
nextEvent :: FrontendSession -> Maybe Bool -> IO (K.Key, K.Modifier)
nextEvent FrontendSession{schanKey, sframeState} Nothing = do
  -- Verify the state.
  -- Assumption: no other thread changes the main constructor in sframeState.
  fs <- readMVar sframeState
  case fs of
    FNone     -> return ()  -- old frame requested, as expected
    FPushed{} -> assert `failure` "nextEvent: FPushed, expecting FNone"
    FSet{}    -> assert `failure` "nextEvent: FSet, expecting FNone"
  -- Wait for a keypress.
  km <- readChan schanKey
  return km
nextEvent sess@FrontendSession{schanKey, sframeState} (Just False) = do
  -- Take the lock to display the set frame.
  fs <- takeMVar sframeState
  case fs of
    FSet{fsetFrame} -> do
      -- If the frame not repeated, draw it.
      maybe (return ()) (postGUIAsync . output sess) fsetFrame
    FPushed{} -> assert `failure` "nextEvent: FPushed, expecting FSet"
    FNone     -> assert `failure` "nextEvent: FNone, expecting FSet"
  -- Clear the stored frame. Release the lock.
  putMVar sframeState FNone
  -- Wait for a keypress.
  km <- readChan schanKey
  return km
nextEvent sess@FrontendSession{schanKey, sframeState} (Just True) = do
  -- Wait for a keypress.
  km <- readChan schanKey
  -- Trim the queue.
  trimQueue sess
  -- Take the lock to wipe out the frame queue, unless it's empty already.
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} -> do
      -- Draw the last frame ASAP.
      case tryReadLQueue fpushed of
        Just (Just frame, queue) -> assert (nullLQueue queue) $ do
          -- Comparison is done inside the mvar lock, this time, but it's OK.
          let lastFrame = fshown
              nextFrame =
                if frame == lastFrame
                then Nothing  -- no sense repeating
                else Just frame
          maybe (return ()) (postGUIAsync . output sess) nextFrame
        Just (Nothing, _) ->  assert `failure` "nextEvent: trimmed queue"
        Nothing -> return ()
    FSet{} -> assert `failure` "nextEvent: FSet, expecting FPushed"
    FNone  -> assert `failure` "nextEvent: FNone, expecting FPushed"
  -- Wipe out the frame queue. No more frames will arrive, because we are
  -- on the same thread as pushFrame. Release the lock.
  putMVar sframeState FNone
  return km

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
-- Starts in Push or None, stop in the None mode.
-- Spends most time waiting for a key, so not performance critical,
-- so does not need optimization.
promptGetKey :: FrontendSession -> [(K.Key, K.Modifier)] -> Color.SingleFrame
             -> IO (K.Key, K.Modifier)
promptGetKey sess@FrontendSession{sframeState} keys frame = do
  -- Assumption: no other thread changes the main constructor in sframeState.
  fs <- readMVar sframeState
  yield  -- drawing has priority
  let doPush = case fs of
        FPushed{} -> True
        FSet{}    ->
          assert `failure` "promptGetKey: FSet, expecting FPushed or FNone"
        FNone     -> False
  display sess doPush True $ Just frame
  km <- nextEvent sess (Just doPush)
  let loop km2 =
        if null keys || km2 `elem` keys
        then return km2
        else do
          km3 <- nextEvent sess Nothing
          loop km3
  loop km

-- | Tells a dead key.
deadKey :: String -> Bool
deadKey x = case x of
  "Shift_R"          -> True
  "Shift_L"          -> True
  "Control_L"        -> True
  "Control_R"        -> True
  "Super_L"          -> True
  "Super_R"          -> True
  "Menu"             -> True
  "Alt_L"            -> True
  "Alt_R"            -> True
  "ISO_Level2_Shift" -> True
  "ISO_Level3_Shift" -> True
  "ISO_Level2_Latch" -> True
  "ISO_Level3_Latch" -> True
  "Num_Lock"         -> True
  "Caps_Lock"        -> True
  _                  -> False

-- | Translates modifiers to our own encoding.
modifierTranslate :: [Modifier] -> K.Modifier
modifierTranslate mods =
  if Control `elem` mods then K.Control else K.NoModifier

doAttr :: TextTag -> Color.Attr -> IO ()
doAttr tt attr@Color.Attr{fg, bg}
  | attr == Color.defaultAttr = return ()
  | fg == Color.defFG = set tt [textTagBackground := Color.colorToRGB bg]
  | bg == Color.defBG = set tt [textTagForeground := Color.colorToRGB fg]
  | otherwise         = set tt [textTagForeground := Color.colorToRGB fg,
                                textTagBackground := Color.colorToRGB bg]
