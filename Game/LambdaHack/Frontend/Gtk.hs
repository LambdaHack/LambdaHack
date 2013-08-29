-- | Text frontend based on Gtk.
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Game.LambdaHack.Frontend.Gtk
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , display, promptGetAnyKey
    -- * Frontend administration tools
  , frontendName, startup
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Graphics.UI.Gtk hiding (Point)
import System.Time

import Game.LambdaHack.Common.Animation (SingleFrame (..))
import qualified Game.LambdaHack.Common.Key as K (KM (..), Modifier (..),
                                                  keyTranslate)
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.LQueue

data FrameState =
    FPushed  -- frames stored in a queue, to be drawn in equal time intervals
      { fpushed :: !(LQueue (Maybe GtkFrame))  -- ^ screen output channel
      , fshown  :: !GtkFrame                   -- ^ last full frame shown
      }
  | FNone  -- no frames stored

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { sview       :: !TextView                    -- ^ the widget to draw to
  , stags       :: !(M.Map Color.Attr TextTag)  -- ^ text color tags for fg/bg
  , schanKey    :: !(Chan K.KM)                 -- ^ channel for keyboard input
  , sframeState :: !(MVar FrameState)
      -- ^ state of the frame finite machine; this mvar is locked
      -- for a short time only, because it's needed, among others,
      -- to display frames, which is done by a single polling thread,
      -- in real time
  , slastFull   :: !(MVar (GtkFrame, Bool))
      -- ^ most recent full (not empty, not repeated) frame received
      -- and if any empty frame followed it; this mvar is locked
      -- for longer intervals to ensure that threads (possibly many)
      -- add frames in an orderly manner, which is not done in real time,
      -- though sometimes the frame display subsystem has to poll
      -- for a frame, in which case the locking interval becomes meaningful
  }

data GtkFrame = GtkFrame
  { gfChar :: !BS.ByteString
  , gfAttr :: ![[TextTag]]
  }
  deriving Eq

dummyFrame :: GtkFrame
dummyFrame = GtkFrame BS.empty []

-- | Perform an operation on the frame queue.
onQueue :: (LQueue (Maybe GtkFrame) -> LQueue (Maybe GtkFrame))
        -> FrontendSession -> IO ()
onQueue f FrontendSession{sframeState} = do
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} ->
      putMVar sframeState FPushed{fpushed = f fpushed, ..}
    _ ->
      putMVar sframeState fs

lengthQueue :: FrontendSession -> IO Int
lengthQueue FrontendSession{sframeState} = do
  fs <- readMVar sframeState
  case fs of
    FPushed{..} -> return $ lengthLQueue fpushed
    _ -> return 0

-- | The name of the frontend.
frontendName :: String
frontendName = "gtk"

-- | Starts GTK. The other threads have to be spawned
-- after gtk is initialized, because they call @postGUIAsync@,
-- and need @sview@ and @stags@. Because of Windows, GTK needs to be
-- on a bound thread, so we can't avoid the communication overhead
-- of bound threads, so there's no point spawning a separate thread for GTK.
startup :: String -> (FrontendSession -> IO ()) -> IO ()
startup configFont k = runGtk configFont k

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
  slastFull <- newMVar (dummyFrame, False)
  let sess = FrontendSession{..}
  -- Fork the game logic thread. When logic ends, game exits.
  -- TODO: is postGUIAsync needed here?
  forkIO $ k sess >> postGUIAsync mainQuit
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
        len <- lengthQueue sess
        if n == "space" && len > 1 then
          -- Only drop frames up to the first empty frame.
          -- Some new frames may be arriving at the same time
          -- or being displayed and removed, but we don't care.
          onQueue dropStartLQueue sess
        else
          -- Store the key in the channel. All but last frame will be dropped
          -- as soon as the channel is read. Use SPACE repeatedly to step
          -- through some intermediate frames of an animation --- other keys
          -- are not meant to be pressed many times before the engine
          -- is ready to recognize and process them.
          writeChan schanKey $ K.KM {key, modifier}
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

-- | Output to the screen via the frontend.
output :: FrontendSession  -- ^ frontend session data
       -> GtkFrame         -- ^ the screen frame to draw
       -> IO ()
output FrontendSession{sview, stags} GtkFrame{..} = do  -- new frame
  tb <- textViewGetBuffer sview
  let attrs = zip [0..] gfAttr
      defAttr = stags M.! Color.defAttr
  textBufferSetByteString tb gfChar
  mapM_ (setTo tb defAttr 0) attrs

setTo :: TextBuffer -> TextTag -> Int -> (Int, [TextTag]) -> IO ()
setTo _  _   _  (_,  [])         = return ()
setTo tb defAttr lx (ly, attr:attrs) = do
  ib <- textBufferGetIterAtLineOffset tb (ly + 1) lx
  ie <- textIterCopy ib
  let setIter :: TextTag -> Int -> [TextTag] -> IO ()
      setIter previous repetitions [] = do
        textIterForwardChars ie repetitions
        when (previous /= defAttr) $
          textBufferApplyTag tb previous ib ie
      setIter previous repetitions (a:as)
        | a == previous =
            setIter a (repetitions + 1) as
        | otherwise = do
            textIterForwardChars ie repetitions
            when (previous /= defAttr) $
              textBufferApplyTag tb previous ib ie
            textIterForwardChars ib repetitions
            setIter a 1 as
  setIter attr 1 attrs

-- TODO: configure
-- | Maximal frames per second.
-- This is better low and fixed, to avoid jerkiness and delays
-- that tell the player there are many intelligent enemies on the level.
-- That's better than scaling AI sofistication down based on the FPS setting
-- and machine speed.
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
  -- Time is up, check if we actually wait for anyting.
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
          -- Time is up, the game thread is going to send a frame,
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

-- | Add a game screen frame to the frame drawing channel, or show
-- it ASAP if @immediate@ display is requested and the channel is empty.
pushFrame :: FrontendSession -> Bool -> Bool -> Maybe SingleFrame -> IO ()
pushFrame sess noDelay immediate rawFrame = do
  let FrontendSession{sframeState, slastFull} = sess
  -- Full evaluation is done outside the mvar locks.
  let !frame = case rawFrame of
        Nothing -> Nothing
        Just fr -> Just $! evalFrame sess fr
  -- Lock frame addition.
  (lastFrame, anyFollowed) <- takeMVar slastFull
  -- Comparison of frames is done outside the frame queue mvar lock.
  let nextFrame = if frame == Just lastFrame
                  then Nothing  -- no sense repeating
                  else frame
  -- Lock frame queue.
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} ->
      if (isNothing nextFrame && anyFollowed)
      then putMVar sframeState fs  -- old news
      else putMVar sframeState
             FPushed{fpushed = writeLQueue fpushed nextFrame, ..}
    FNone | immediate -> do
      -- If the frame not repeated, draw it.
      maybe skip (postGUIAsync . output sess) nextFrame
      putMVar sframeState FNone
    FNone ->
      -- Never start playing with an empty frame.
      let fpushed = if isJust nextFrame
                    then writeLQueue newLQueue nextFrame
                    else newLQueue
          fshown = dummyFrame
      in putMVar sframeState FPushed{..}
  case nextFrame of
    Nothing -> putMVar slastFull (lastFrame, True)
    Just f  -> putMVar slastFull (f, noDelay)

evalFrame :: FrontendSession -> SingleFrame -> GtkFrame
evalFrame FrontendSession{stags} SingleFrame{..} =
  let levelChar = map (T.pack . map Color.acChar) sfLevel
      gfChar = encodeUtf8 $ T.intercalate (T.singleton '\n')
               $ sfTop : levelChar ++ [sfBottom]
      -- Strict version of @map (map ((stags M.!) . fst)) sfLevel@.
      gfAttr  = reverse $ foldl' ff [] sfLevel
      ff ll l = (reverse $ foldl' f [] l) : ll
      f l ac  = let !tag = stags M.! Color.acAttr ac in tag : l
  in GtkFrame{..}

-- | Trim current frame queue and display the most recent frame, if any.
trimFrameState :: FrontendSession -> IO ()
trimFrameState sess@FrontendSession{sframeState} = do
  -- Take the lock to wipe out the frame queue, unless it's empty already.
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} -> do
      -- Remove all but the last element of the frame queue.
      -- The kept (and displayed) last element ensures that
      -- @slastFull@ is not invalidated.
      case lastLQueue fpushed of
        Just frame -> do
          -- Comparison is done inside the mvar lock, this time, but it's OK,
          -- since we wipe out the queue anyway, not draw it concurrently.
          let lastFrame = fshown
              nextFrame = if frame == lastFrame
                          then Nothing  -- no sense repeating
                          else Just frame
          -- Draw the last frame ASAP.
          maybe skip (postGUIAsync . output sess) nextFrame
        Nothing -> return ()
    FNone -> return ()
  -- Wipe out the frame queue. Release the lock.
  putMVar sframeState FNone

-- | Add a frame to be drawn.
display :: FrontendSession -> Bool -> Maybe SingleFrame -> IO ()
display sess noDelay rawFrame = pushFrame sess noDelay False rawFrame

-- | Display a prompt, wait for any key.
-- Starts in Push or None mode, stop in None mode.
promptGetAnyKey :: FrontendSession -> SingleFrame -> IO K.KM
promptGetAnyKey sess@FrontendSession{schanKey} frame = do
  pushFrame sess True True $ Just frame
  km <- readChan schanKey
  trimFrameState sess
  return km

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
  | attr == Color.defAttr = return ()
  | fg == Color.defFG = set tt [textTagBackground := Color.colorToRGB bg]
  | bg == Color.defBG = set tt [textTagForeground := Color.colorToRGB fg]
  | otherwise         = set tt [textTagForeground := Color.colorToRGB fg,
                                textTagBackground := Color.colorToRGB bg]
