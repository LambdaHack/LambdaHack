{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Text frontend based on Gtk.
module Game.LambdaHack.Client.UI.Frontend.Gtk
  ( -- * Session data type for the frontend
    FrontendSession(sescMVar)
    -- * The output and input operations
  , fdisplay, fpromptGetKey, fsyncFrames
    -- * Frontend administration tools
  , frontendName, startup
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String (IsString (..))
import qualified Data.Text as T
import Graphics.UI.Gtk hiding (Point)
import System.Time

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.LQueue
import Game.LambdaHack.Common.Point

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
  , schanKey    :: !(STM.TQueue K.KM)           -- ^ channel for keyboard input
  , sframeState :: !(MVar FrameState)
      -- ^ State of the frame finite machine. This mvar is locked
      -- for a short time only, because it's needed, among others,
      -- to display frames, which is done by a single polling thread,
      -- in real time.
  , slastFull   :: !(MVar (GtkFrame, Bool))
      -- ^ Most recent full (not empty, not repeated) frame received
      -- and if any empty frame followed it. This mvar is locked
      -- for longer intervals to ensure that threads (possibly many)
      -- add frames in an orderly manner. This is not done in real time,
      -- though sometimes the frame display subsystem has to poll
      -- for a frame, in which case the locking interval becomes meaningful.
  , sescMVar    :: !(Maybe (MVar ()))
  , sdebugCli   :: !DebugModeCli  -- ^ client configuration
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
    FNone ->
      putMVar sframeState fs

-- | The name of the frontend.
frontendName :: String
frontendName = "gtk"

-- | Starts GTK. The other threads have to be spawned
-- after gtk is initialized, because they call @postGUIAsync@,
-- and need @sview@ and @stags@. Because of Windows, GTK needs to be
-- on a bound thread, so we can't avoid the communication overhead
-- of bound threads, so there's no point spawning a separate thread for GTK.
startup :: DebugModeCli -> (FrontendSession -> IO ()) -> IO ()
startup = runGtk

-- | Sets up and starts the main GTK loop providing input and output.
runGtk :: DebugModeCli -> (FrontendSession -> IO ()) -> IO ()
runGtk sdebugCli@DebugModeCli{sfont} cont = do
  -- Init GUI.
  unsafeInitGUIForThreadedRTS
  -- Text attributes.
  ttt <- textTagTableNew
  stags <- M.fromList <$>
             mapM (\ ak -> do
                      tt <- textTagNew Nothing
                      textTagTableAdd ttt tt
                      doAttr sdebugCli tt ak
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
  schanKey <- STM.atomically STM.newTQueue
  -- Set up the frame state.
  let frameState = FNone
  -- Create the session record.
  sframeState <- newMVar frameState
  slastFull <- newMVar (dummyFrame, False)
  escMVar <- newEmptyMVar
  let sess = FrontendSession{sescMVar = Just escMVar, ..}
  -- Fork the game logic thread. When logic ends, game exits.
  -- TODO: is postGUISync needed here?
  aCont <- async $ cont sess `Ex.finally` postGUISync mainQuit
  link aCont
  -- Fork the thread that periodically draws a frame from a queue, if any.
  aPoll <- async $ pollFramesAct sess `Ex.finally` postGUISync mainQuit
  link aPoll
  let flushChanKey = do
        res <- STM.atomically $ STM.tryReadTQueue schanKey
        when (isJust res) flushChanKey
  -- Fill the keyboard channel.
  sview `on` keyPressEvent $ do
    n <- eventKeyName
    mods <- eventModifier
#if MIN_VERSION_gtk(0,13,0)
    let !key = K.keyTranslate $ T.unpack n
#else
    let !key = K.keyTranslate n
#endif
        !modifier = let md = modifierTranslate mods
                    in if md == K.Shift then K.NoModifier else md
        !pointer = dummyPoint
    liftIO $ do
      unless (deadKey n) $ do
        -- If ESC, also mark it specially and reset the key channel.
        when (key == K.Esc) $ do
          void $ tryPutMVar escMVar ()
          flushChanKey
        -- Store the key in the channel.
        STM.atomically $ STM.writeTQueue schanKey K.KM{..}
      return True
  -- Set the font specified in config, if any.
  f <- fontDescriptionFromString $ fromMaybe "" sfont
  widgetModifyFont sview (Just f)
  liftIO $ do
    textViewSetLeftMargin sview 3
    textViewSetRightMargin sview 3
  -- Prepare font chooser dialog.
  currentfont <- newIORef f
  Just display <- displayGetDefault
  -- TODO: change cursor depending on targeting mode, etc.; hard
  cursor <- cursorNewForDisplay display Tcross  -- Target Crosshair Arrow
  sview `on` buttonPressEvent $ do
    liftIO flushChanKey
    but <- eventButton
    (wx, wy) <- eventCoordinates
    mods <- eventModifier
    let !modifier = modifierTranslate mods  -- Shift included
    liftIO $ do
      when (but == RightButton && modifier == K.Control) $ do
        fsd <- fontSelectionDialogNew ("Choose font" :: String)
        cf  <- readIORef currentfont
        fds <- fontDescriptionToString cf
        fontSelectionDialogSetFontName fsd (fds :: String)
        fontSelectionDialogSetPreviewText fsd ("eee...@.##+##" :: String)
        resp <- dialogRun fsd
        when (resp == ResponseOk) $ do
          fn <- fontSelectionDialogGetFontName fsd
          case fn :: Maybe String of
            Just fn' -> do
              fd <- fontDescriptionFromString fn'
              writeIORef currentfont fd
              widgetModifyFont sview (Just fd)
            Nothing  -> return ()
        widgetDestroy fsd
      -- We shouldn't pass on the click if the user has selected something.
      hasSelection <- textBufferHasSelection tb
      unless hasSelection $ do
        mdrawWin <- displayGetWindowAtPointer display
        let setCursor (drawWin, _, _) =
              drawWindowSetCursor drawWin (Just cursor)
        maybe (return ()) setCursor mdrawWin
        (bx, by) <-
          textViewWindowToBufferCoords sview TextWindowText
                                       (round wx, round wy)
        (iter, _) <- textViewGetIterAtPosition sview bx by
        cx <- textIterGetLineOffset iter
        cy <- textIterGetLine iter
        let !key = case but of
              LeftButton -> K.LeftButtonPress
              MiddleButton -> K.MiddleButtonPress
              RightButton -> K.RightButtonPress
              _ -> K.LeftButtonPress
            !pointer = Point cx (cy - 1)
        -- Store the mouse even coords in the keypress channel.
        STM.atomically $ STM.writeTQueue schanKey K.KM{..}
    return $! but == RightButton  -- not to disable selection
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
setTo _ _ _ (_,  []) = return ()
setTo tb defAttr lx (ly, attr:attrs) = do
  ib <- textBufferGetIterAtLineOffset tb ly lx
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

-- | Maximal polls per second.
maxPolls :: Int -> Int
maxPolls maxFps = max 120 (2 * maxFps)

picoInMicro :: Int
picoInMicro = 1000000

-- | Add a given number of microseconds to time.
addTime :: ClockTime -> Int -> ClockTime
addTime (TOD s p) mus = TOD s (p + fromIntegral (mus * picoInMicro))

-- | The difference between the first and the second time, in microseconds.
diffTime :: ClockTime -> ClockTime -> Int
diffTime (TOD s1 p1) (TOD s2 p2) =
  fromIntegral (s1 - s2) * picoInMicro +
  fromIntegral (p1 - p2) `div` picoInMicro

microInSec :: Int
microInSec = 1000000

defaultMaxFps :: Int
defaultMaxFps = 30

-- | Poll the frame queue often and draw frames at fixed intervals.
pollFramesWait :: FrontendSession -> ClockTime -> IO ()
pollFramesWait sess@FrontendSession{sdebugCli=DebugModeCli{smaxFps}}
               setTime = do
  -- Check if the time is up.
  let maxFps = fromMaybe defaultMaxFps smaxFps
  curTime <- getClockTime
  let diffSetCur = diffTime setTime curTime
  if diffSetCur > microInSec `div` maxPolls maxFps
    then do
      -- Delay half of the time difference.
      threadDelay $ diffTime curTime setTime `div` 2
      pollFramesWait sess setTime
    else
      -- Don't delay, because time is up!
      pollFramesAct sess

-- | Poll the frame queue often and draw frames at fixed intervals.
pollFramesAct :: FrontendSession -> IO ()
pollFramesAct sess@FrontendSession{sframeState, sdebugCli=DebugModeCli{..}} = do
  -- Time is up, check if we actually wait for anyting.
  let maxFps = fromMaybe defaultMaxFps smaxFps
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} ->
      case tryReadLQueue fpushed of
        Just (Just frame, queue) -> do
          -- The frame has arrived so send it for drawing and update delay.
          putMVar sframeState FPushed{fpushed = queue, fshown = frame}
          -- Count the time spent outputting towards the total frame time.
          curTime <- getClockTime
          -- Wait until the frame is drawn.
          postGUISync $ output sess frame
          -- Regardless of how much time drawing took, wait at least
          -- half of the normal delay time. This can distort the large-scale
          -- frame rhythm, but makes sure this frame can at all be seen.
          -- If the main GTK thread doesn't lag, large-scale rhythm will be OK.
          -- TODO: anyway, it's GC that causes visible snags, most probably.
          threadDelay $ microInSec `div` (maxFps * 2)
          pollFramesWait sess $ addTime curTime $ microInSec `div` maxFps
        Just (Nothing, queue) -> do
          -- Delay requested via an empty frame.
          putMVar sframeState FPushed{fpushed = queue, ..}
          unless snoDelay $
            -- There is no problem if the delay is a bit delayed.
            threadDelay $ microInSec `div` maxFps
          pollFramesAct sess
        Nothing -> do
          -- The queue is empty, the game logic thread lags.
          putMVar sframeState fs
          -- Time is up, the game thread is going to send a frame,
          -- (otherwise it would change the state), so poll often.
          threadDelay $ microInSec `div` maxPolls maxFps
          pollFramesAct sess
    FNone -> do
      putMVar sframeState fs
      -- Not in the Push state, so poll lazily to catch the next state change.
      -- The slow polling also gives the game logic a head start
      -- in creating frames in case one of the further frames is slow
      -- to generate and would normally cause a jerky delay in drawing.
      threadDelay $ microInSec `div` (maxFps * 2)
      pollFramesAct sess

-- | Add a game screen frame to the frame drawing channel, or show
-- it ASAP if @immediate@ display is requested and the channel is empty.
pushFrame :: FrontendSession -> Bool -> Maybe SingleFrame -> IO ()
pushFrame sess immediate rawFrame = do
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
      putMVar sframeState
      $ if isNothing nextFrame && anyFollowed && isJust rawFrame
        then fs  -- old news
        else FPushed{fpushed = writeLQueue fpushed nextFrame, ..}
    FNone | immediate -> do
      -- If the frame not repeated, draw it.
      maybe (return ()) (postGUIAsync . output sess) nextFrame
      -- Frame sent, we may now safely release the queue lock.
      putMVar sframeState FNone
    FNone ->
      putMVar sframeState
      $ if isNothing nextFrame && anyFollowed && isJust rawFrame
        then fs  -- old news
        else FPushed{ fpushed = writeLQueue newLQueue nextFrame
                    , fshown = dummyFrame }
  case nextFrame of
    Nothing -> putMVar slastFull (lastFrame, not (case fs of
                                                    FNone -> True
                                                    FPushed{} -> False
                                                  && immediate
                                                  && not anyFollowed))
    Just f  -> putMVar slastFull (f, False)

evalFrame :: FrontendSession -> SingleFrame -> GtkFrame
evalFrame FrontendSession{stags} rawSF =
  let SingleFrame{sfLevel} = overlayOverlay rawSF
      sfLevelDecoded = map decodeLine sfLevel
      levelChar = unlines $ map (map Color.acChar) sfLevelDecoded
      gfChar = BS.pack $ init levelChar
      -- Strict version of @map (map ((stags M.!) . fst)) sfLevelDecoded@.
      gfAttr  = reverse $ foldl' ff [] sfLevelDecoded
      ff ll l = reverse (foldl' f [] l) : ll
      f l ac  = let !tag = stags M.! Color.acAttr ac in tag : l
  in GtkFrame{..}

-- | Trim current frame queue and display the most recent frame, if any.
trimFrameState :: FrontendSession -> IO ()
trimFrameState sess@FrontendSession{sframeState} = do
  -- Take the lock to wipe out the frame queue, unless it's empty already.
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} ->
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
          maybe (return ()) (postGUIAsync . output sess) nextFrame
        Nothing -> return ()
    FNone -> return ()
  -- Wipe out the frame queue. Release the lock.
  putMVar sframeState FNone

-- | Add a frame to be drawn.
fdisplay :: FrontendSession    -- ^ frontend session data
         -> Maybe SingleFrame  -- ^ the screen frame to draw
         -> IO ()
fdisplay sess = pushFrame sess False

-- Display all queued frames, synchronously.
displayAllFramesSync :: FrontendSession -> FrameState -> IO ()
displayAllFramesSync sess@FrontendSession{sdebugCli=DebugModeCli{..}, sescMVar}
                     fs = do
  escPressed <- case sescMVar of
    Nothing -> return False
    Just escMVar -> not <$> isEmptyMVar escMVar
  let maxFps = fromMaybe defaultMaxFps smaxFps
  case fs of
    _ | escPressed -> return ()
    FPushed{..} ->
      case tryReadLQueue fpushed of
        Just (Just frame, queue) -> do
          -- Display synchronously.
          postGUISync $ output sess frame
          threadDelay $ microInSec `div` maxFps
          displayAllFramesSync sess FPushed{fpushed = queue, fshown = frame}
        Just (Nothing, queue) -> do
          -- Delay requested via an empty frame.
          unless snoDelay $
            threadDelay $ microInSec `div` maxFps
          displayAllFramesSync sess FPushed{fpushed = queue, ..}
        Nothing ->
          -- The queue is empty.
          return ()
    FNone ->
      -- Not in Push state to start with.
      return ()

fsyncFrames :: FrontendSession -> IO ()
fsyncFrames sess@FrontendSession{sframeState} = do
  fs <- takeMVar sframeState
  displayAllFramesSync sess fs
  putMVar sframeState FNone

-- | Display a prompt, wait for any key.
-- Starts in Push mode, ends in Push or None mode.
-- Syncs with the drawing threads by showing the last or all queued frames.
fpromptGetKey :: FrontendSession -> SingleFrame -> IO K.KM
fpromptGetKey sess@FrontendSession{..}
              frame = do
  pushFrame sess True $ Just frame
  km <- STM.atomically $ STM.readTQueue schanKey
  case km of
    K.KM{key=K.Space} ->
      -- Drop frames up to the first empty frame.
      -- Keep the last non-empty frame, if any.
      -- Pressing SPACE repeatedly can be used to step
      -- through intermediate stages of an animation,
      -- whereas any other key skips the whole animation outright.
      onQueue dropStartLQueue sess
    _ ->
      -- Show the last non-empty frame and empty the queue.
      trimFrameState sess
  return km

-- | Tells a dead key.
deadKey :: (Eq t, IsString t) => t -> Bool
deadKey x = case x of
  "Shift_L"          -> True
  "Shift_R"          -> True
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
modifierTranslate mods
  | Control `elem` mods = K.Control
  | any (`elem` mods) [Meta, Super, Alt, Alt2, Alt3, Alt4, Alt5] = K.Alt
  | Shift `elem` mods = K.Shift
  | otherwise = K.NoModifier

doAttr :: DebugModeCli -> TextTag -> Color.Attr -> IO ()
doAttr sdebugCli tt attr@Color.Attr{fg, bg}
  | attr == Color.defAttr = return ()
  | fg == Color.defFG =
    set tt $ extraAttr sdebugCli
             ++ [textTagBackground := Color.colorToRGB bg]
  | bg == Color.defBG =
    set tt $ extraAttr sdebugCli
             ++ [textTagForeground := Color.colorToRGB fg]
  | otherwise =
    set tt $ extraAttr sdebugCli
             ++ [ textTagForeground := Color.colorToRGB fg
                , textTagBackground := Color.colorToRGB bg ]

extraAttr :: DebugModeCli -> [AttrOp TextTag]
extraAttr DebugModeCli{scolorIsBold} =
  [textTagWeight := fromEnum WeightBold | scolorIsBold == Just True]
--     , textTagStretch := StretchUltraExpanded
