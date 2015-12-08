{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Text frontend based on Gtk.
module Game.LambdaHack.Client.UI.Frontend.Gtk
  ( startup, frontendName
  ) where

import Prelude ()
import Prelude.Compat

import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Monad (unless, void, when)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String (IsString (..))
import qualified Data.Text as T
import Graphics.UI.Gtk hiding (Point)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Point

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { sview :: !TextView                    -- ^ the widget to draw to
  , stags :: !(M.Map Color.Attr TextTag)  -- ^ text color tags for fg/bg
  }

data GtkFrame = GtkFrame
  { gfChar :: !BS.ByteString
  , gfAttr :: ![[TextTag]]
  }
  deriving Eq

-- | The name of the frontend.
frontendName :: String
frontendName = "gtk"

-- | Set up and start the main GTK loop providing input and output.
--
-- Because of Windows, GTK needs to be on a bound thread,
-- so we can't avoid the communication overhead of bound threads.
startup :: DebugModeCli -> IO RawFrontend
startup sdebugCli@DebugModeCli{sfont} = startupBound $ \rfMVar -> do
  -- Init GUI.
  unsafeInitGUIForThreadedRTS
  -- Text attributes.
  ttt <- textTagTableNew
  stags <- M.fromList <$>
             mapM (\ak -> do
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
  fchanKey <- STM.atomically STM.newTQueue
  -- Create the session record.
  fshowNow <- newMVar ()
  let sess = FrontendSession{..}
      promptGetKey :: SingleFrame -> IO K.KM
      promptGetKey frame = do
        display sess frame
        STM.atomically $ STM.readTQueue fchanKey
      rf = RawFrontend
        { fdisplay = display sess
        , fpromptGetKey = promptGetKey
        , fshutdown = shutdown
        , fshowNow
        , fchanKey
        }
  putMVar rfMVar rf
  let modTranslate mods = modifierTranslate
        (Control `elem` mods)
        (Shift `elem` mods)
        (any (`elem` mods) [Alt, Alt2, Alt3, Alt4, Alt5])
        (any (`elem` mods) [Meta, Super])
  sview `on` keyPressEvent $ do
    n <- eventKeyName
    mods <- eventModifier
    let !key = K.keyTranslate $ T.unpack n
        !modifier =
          let md = modTranslate mods
          in if md == K.Shift then K.NoModifier else md
        !pointer = Nothing
    liftIO $ do
      unless (deadKey n) $ do
        -- Store the key in the channel.
        STM.atomically $ STM.writeTQueue fchanKey K.KM{..}
        -- Instantly show any frame waiting for display.
        void $ tryPutMVar fshowNow ()
      return True
  -- Set the font specified in config, if any.
  f <- fontDescriptionFromString $ fromMaybe "" sfont
  widgetModifyFont sview (Just f)
  liftIO $ do
    textViewSetLeftMargin sview 3
    textViewSetRightMargin sview 3
  -- Take care of the mouse events.
  currentfont <- newIORef f
  Just defDisplay <- displayGetDefault
  -- TODO: change cursor depending on targeting mode, etc.; hard
  cursor <- cursorNewForDisplay defDisplay Tcross  -- Target Crosshair Arrow
  sview `on` buttonPressEvent $ do
    liftIO $ resetChanKey fchanKey
    but <- eventButton
    (wx, wy) <- eventCoordinates
    mods <- eventModifier
    let !modifier = modTranslate mods  -- Shift included
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
        mdrawWin <- displayGetWindowAtPointer defDisplay
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
            !pointer = Just $! Point cx cy
        -- Store the mouse event coords in the keypress channel.
        STM.atomically $ STM.writeTQueue fchanKey K.KM{..}
    return $! but == RightButton  -- not to disable selection
  -- Modify default colours.
  let black = Color minBound minBound minBound  -- Color.defBG == Color.Black
      white = Color 0xC500 0xBC00 0xB800        -- Color.defFG == Color.White
  widgetModifyBase sview StateNormal black
  widgetModifyText sview StateNormal white
  -- Set up the main window.
  w <- windowNew
  containerAdd w sview
  onDestroy w (error "Window killed")
  widgetShowAll w
  mainGUI

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

-- TODO: is postGUISync required?
shutdown :: IO ()
shutdown = postGUISync mainQuit

-- | Add a frame to be drawn.
display :: FrontendSession    -- ^ frontend session data
        -> SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display sess@FrontendSession{sview, stags} frame = postGUISync $ do
  let GtkFrame{..} = evalFrame sess frame
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
