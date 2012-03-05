-- | Text frontend based on Gtk.
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Game.LambdaHack.Display.Gtk
  ( -- * Session data type for the frontend
    FrontendSession
    -- * The output and input operations
  , display, nextEvent, promptGetKey, displayNoKey
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
   -- Install a function that periodically draws a frame from a queue, if any.
  timeoutAdd (drawFrame sess) (1000 `div` maxFps)
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
  -- Show the window.
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

-- | Draw a frame from a channel. Don't block if not frame.
drawFrame :: FrontendSession -> IO Bool
drawFrame sess@FrontendSession{sframeState} = do
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} ->
      case tryReadLQueue fpushed of
        Just (Just frame, queue) -> do
          putMVar sframeState FPushed{fpushed = queue, fshown = frame}
          output sess frame
        Just (Nothing, queue) ->  -- timeout requested via an empty frame
          putMVar sframeState FPushed{fpushed = queue, ..}
        Nothing ->  -- the queue is empty
          putMVar sframeState fs
    _ ->
      putMVar sframeState fs
  return True

-- | Add a frame to be drawn.
display :: FrontendSession -> Bool -> Bool -> Maybe Color.SingleFrame -> IO ()
display sess True noTimeout rawFrame = pushFrame sess noTimeout rawFrame
display sess False _ (Just rawFrame) = setFrame sess rawFrame
display _ _ _ _ = assert `failure` "display: empty frame to be set"

-- | Add a game screen frame to the frame drawing channel.
pushFrame :: FrontendSession -> Bool -> Maybe Color.SingleFrame -> IO ()
pushFrame sess@FrontendSession{sframeState, slastFull} noTimeout rawFrame = do
  -- Full evaluation and comparison is done outside the mvar lock.
  (lastFrame, anyFollowed) <- readIORef slastFull
  let frame = maybe Nothing (Just . evalFrame sess) rawFrame
      nextFrame =
        if frame == Just lastFrame
        then Nothing  -- no sense repeating
        else frame
  fs <- takeMVar sframeState
  case fs of
    FPushed{..} ->
      if (isNothing nextFrame && anyFollowed)
      then putMVar sframeState fs  -- old news
      else putMVar sframeState
             FPushed{fpushed = writeLQueue fpushed nextFrame, ..}
--    FSet{} -> assert `failure` "pushFrame: FSet state"
    -- TODO: A hack until we ensure correctness via types in game logic.
    -- The Action type should be parameterized by a phantom type representing
    -- the frame state or, later, done in a way that can't be subverted
    -- as easily.
    -- The rawFrame is ignored, but next frames won't be.
    FSet{fsetFrame} ->
      putMVar sframeState
             FPushed{ fpushed = writeLQueue newLQueue fsetFrame
                    , fshown = dummyFrame
                    }
    FNone ->
      -- Never start playing with an empty frame.
      let fpushed = if isJust nextFrame
                    then writeLQueue newLQueue nextFrame
                    else newLQueue
          fshown = dummyFrame
      in putMVar sframeState FPushed{..}
  case nextFrame of
    Nothing -> writeIORef slastFull (lastFrame, True)
    Just f  -> writeIORef slastFull (f, noTimeout)

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
-- Dont show if the frame unchanged from the previous.
setFrame :: FrontendSession -> Color.SingleFrame -> IO ()
setFrame sess@FrontendSession{slastFull, sframeState} rawFrame = do
  -- Full evaluation and comparison is done outside the mvar lock.
  (lastFrame, _) <- readIORef slastFull
  let frame = evalFrame sess rawFrame
      fsetFrame =
        if frame == lastFrame
        then Nothing  -- no sense repeating
        else Just frame
  fs <- takeMVar sframeState
  case fs of
    FPushed{} -> assert `failure` "setFrame: FPushed state"
    FSet{} -> assert `failure` "setFrame: FSet state"
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
  -- Take the lock to verify the state.
  fs <- takeMVar sframeState
  case fs of
    FNone -> putMVar sframeState fs  -- old frame requested, as expected
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
      -- Clear the stored frame. Release the lock.
      putMVar sframeState FNone
    FPushed{} -> assert `failure` "nextEvent: FPushed, expecting FSet"
    FNone     -> assert `failure` "nextEvent: FNone, expecting FSet"
  -- Wait for a keypress.
  km <- readChan schanKey
  return km
nextEvent FrontendSession{schanKey, slastFull, sframeState} (Just True) = do
  -- Wait for a keypress.
  km <- readChan schanKey
  -- Take the lock to clear the frame queue.
  fs <- takeMVar sframeState
  case fs of
    FPushed{fshown} -> do
      -- Frame is already drawn or being drawn from the queue.
      -- Update the last received frame with the last shown,
      -- because we clear the queue and so invalidate the old value.
      -- There is no race condition, because we are on the same thread
      -- as pushFrame and the lock synchronizes us with drawFrame.
      writeIORef slastFull (fshown, False)
      -- Clear the frame queue. No more frames will arrive, because we are
      -- on the same thread as pushFrame. Release the lock.
      putMVar sframeState FNone
    FSet{} -> assert `failure` "nextEvent: FSet, expecting FPushed"
    FNone  -> assert `failure` "nextEvent: FNone, expecting FPushed"
  return km

-- TODO: simplify
-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: FrontendSession -> [(K.Key, K.Modifier)] -> Color.SingleFrame
             -> IO (K.Key, K.Modifier)
promptGetKey sess keys frame = do
  display sess False False $ Just frame
  km <- nextEvent sess (Just False)
  let loop km2 =
        if null keys || km2 `elem` keys
        then return km2
        else do
          km3 <- nextEvent sess Nothing
          loop km3
  loop km

-- TODO: simplify
displayNoKey :: FrontendSession -> Color.SingleFrame -> IO ()
displayNoKey sess@FrontendSession{sframeState} frame = do
  display sess False False $ Just frame
  -- Take the lock to display the frame.
  fs <- takeMVar sframeState
  case fs of
    FSet{fsetFrame} -> do
      -- If the frame not repeated, draw it.
      maybe (return ()) (postGUIAsync . output sess) fsetFrame
      -- Clear the stored frame. Release the lock.
      putMVar sframeState FNone
    FPushed{} -> assert `failure` "promptGetKey: FPushed, expecting FSet"
    FNone     -> assert `failure` "promptGetKey: FNone, expecting FSet"
  return ()

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
