{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- | Text frontend based on Gtk.
module Game.LambdaHack.Client.UI.Frontend.Gtk
  (
#ifdef USE_GTK
-- to molify doctest, but don't break stylish-haskell parsing
   startup, frontendName
#endif
  ) where

#ifdef USE_GTK
import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import           Data.Bits (unsafeShiftL)
import           Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import           Data.Word (Word32)
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Graphics.UI.Gtk
import           System.Exit (exitFailure)

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { sview :: TextView           -- ^ the widget to draw to
  , stags :: IM.IntMap TextTag  -- ^ text color tags for fg/bg
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "gtk"

-- | Set up and start the main GTK loop providing input and output.
--
-- Because of Windows, GTK needs to be on a bound thread,
-- so we can't avoid the communication overhead of bound threads.
startup :: ScreenContent -> ClientOptions -> IO RawFrontend
startup coscreen soptions = startupBound $ startupFun coscreen soptions

startupFun :: ScreenContent -> ClientOptions -> MVar RawFrontend -> IO ()
startupFun coscreen _ rfMVar = do
  -- Init GUI.
  unsafeInitGUIForThreadedRTS
  -- Text attributes.
  let emulateBox Color.Attr{..} = case bg of
        Color.HighlightNone -> (fg, Color.Black)
        Color.HighlightGreen ->
          if fg /= Color.Green
          then (fg, Color.Green)
          else (fg, Color.BrBlack)
        Color.HighlightBlue ->
          if fg /= Color.Blue
          then (fg, Color.Blue)
          else (fg, Color.BrBlack)
        Color.HighlightBrown ->
          if fg /= Color.Blue
          then (fg, Color.Brown)
          else (fg, Color.BrBlack)
        Color.HighlightCyan ->
          if fg /= Color.Blue
          then (fg, Color.Cyan)
          else (fg, Color.BrBlack)
        Color.HighlightGrey ->
          if fg /= Color.BrBlack
          then (fg, Color.BrBlack)
          else (fg, Color.defFG)
        Color.HighlightWhite -> (fg, Color.Black)
        Color.HighlightMagenta -> (fg, Color.Black)
        Color.HighlightRed ->
          if fg /= Color.Red
          then (fg, Color.Red)
          else (fg, Color.defFG)
        Color.HighlightYellow -> (Color.Black, Color.defFG)  -- no cursor
        Color.HighlightYellowAim -> (Color.Black, Color.defFG)
        Color.HighlightRedAim ->
          if fg /= Color.Red
          then (fg, Color.Red)
          else (fg, Color.defFG)
        Color.HighlightNoneCursor -> (fg, Color.Black)
        Color.HighlightBackground -> (fg, Color.BrBlack)
  ttt <- textTagTableNew
  stags <- IM.fromDistinctAscList <$>
             mapM (\ak -> do
                      tt <- textTagNew Nothing
                      textTagTableAdd ttt tt
                      doAttr tt (emulateBox ak)
                      return (fromAttr ak, tt))
               [ Color.Attr{fg, bg}
               | fg <- Color.legalFgCol
               , bg <- [minBound..maxBound] ]
  -- Text buffer.
  tb <- textBufferNew (Just ttt)
  -- Create text view.
  sview <- textViewNewWithBuffer tb
  textViewSetEditable sview False
  textViewSetCursorVisible sview False
  widgetDelEvents sview [SmoothScrollMask, TouchMask]
  widgetAddEvents sview [ScrollMask]
  let sess = FrontendSession{..}
  rf <- createRawFrontend coscreen (display coscreen sess) shutdown
  putMVar rfMVar rf
  let modTranslate mods = modifierTranslate
        (Control `elem` mods)
        (Shift `elem` mods)
        (any (`elem` mods) [Alt, Alt2, Alt3, Alt4, Alt5])
        (any (`elem` mods) [Meta, Super])
  sview `on` keyPressEvent $ do
    n <- eventKeyName
    mods <- eventModifier
    let key = K.keyTranslate $ T.unpack n
        modifier = modTranslate mods  -- Shift included
        modifierNoShift = case modifier of  -- to prevent S-!, etc.
          K.Shift -> K.NoModifier
          K.ControlShift -> K.Control
          K.AltShift -> K.Alt
          _ -> modifier
    when (key == K.Esc) $ IO.liftIO $ resetChanKey (fchanKey rf)
    IO.liftIO $ saveKMP rf modifierNoShift key (PointUI 0 0)
    return True
  -- Set the font specified in config, if any.
  -- The list are monospace fonts that have fixed size regardless
  -- of boldness (on some OSes at least).
  f <- fontDescriptionFromString ("DejaVu Sans Mono,Consolas,Courier New,Liberation Mono,Courier,FreeMono,Monospace 16px" :: String)
  widgetModifyFont sview (Just f)
  IO.liftIO $ do
    textViewSetLeftMargin sview 3
    textViewSetRightMargin sview 3
  -- Take care of the mouse events.
  sview `on` scrollEvent $ do
    IO.liftIO $ resetChanKey (fchanKey rf)
    scrollDir <- eventScrollDirection
    (wx, wy) <- eventCoordinates
    mods <- eventModifier
    let modifier = modTranslate mods  -- Shift included
    IO.liftIO $ do
      (bx, by) <-
        textViewWindowToBufferCoords sview TextWindowText
                                     (round wx, round wy)
      (iter, _) <- textViewGetIterAtPosition sview bx by
      cx <- textIterGetLineOffset iter
      cy <- textIterGetLine iter
      let pointer = PointUI cx cy
          -- Store the mouse event coords in the keypress channel.
          storeK key = saveKMP rf modifier key pointer
      case scrollDir of
        ScrollUp -> storeK K.WheelNorth
        ScrollDown -> storeK K.WheelSouth
        _ -> return ()  -- ignore any fancy new gizmos
    return True  -- disable selection
  currentfont <- newIORef f
  Just defDisplay <- displayGetDefault
  cursor <- cursorNewForDisplay defDisplay Tcross  -- Target Crosshair Arrow
  sview `on` buttonPressEvent $ return True  -- disable selection
  sview `on` buttonReleaseEvent $ do
    IO.liftIO $ resetChanKey (fchanKey rf)
    but <- eventButton
    (wx, wy) <- eventCoordinates
    mods <- eventModifier
    let modifier = modTranslate mods  -- Shift included
    IO.liftIO $ do
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
      let mkey = case but of
            LeftButton -> Just K.LeftButtonRelease
            MiddleButton -> Just K.MiddleButtonRelease
            RightButton -> Just K.RightButtonRelease
            _ -> Nothing  -- probably a glitch
          pointer = PointUI cx cy
      -- Store the mouse event coords in the keypress channel.
      maybe (return ())
            (\key -> IO.liftIO $ saveKMP rf modifier key pointer) mkey
    return True
  -- Modify default colours.
  let black = Color minBound minBound minBound  -- Color.defBG == Color.Black
      white = Color 0xB800 0xBF00 0xCB00        -- Color.defFG == Color.White
  widgetModifyBg sview StateNormal black
  widgetModifyFg sview StateNormal white
  -- Set up the main window.
  w <- windowNew
  containerAdd w sview
  -- We assume it's intentional window kill by the player,
  -- so game is not saved, unlike with assertion failure, etc.
  w `on` deleteEvent $ IO.liftIO $ do
    putStrLn "Window killed"
    mainQuit
    exitFailure
  widgetShowAll w
  mainGUI

shutdown :: IO ()
shutdown = postGUISync mainQuit

fromAttr :: Color.Attr -> Int
fromAttr Color.Attr{..} = unsafeShiftL (fromEnum fg) 8 + fromEnum bg

doAttr :: TextTag -> (Color.Color, Color.Color) -> IO ()
doAttr tt (fg, bg)
  | fg == Color.defFG && bg == Color.Black = return ()
  | fg == Color.defFG =
    set tt [textTagBackground := Color.colorToRGB bg]
  | bg == Color.Black =
    set tt $ extraAttr
             ++ [textTagForeground := Color.colorToRGB fg]
  | otherwise =
    set tt $ extraAttr
             ++ [ textTagForeground := Color.colorToRGB fg
                , textTagBackground := Color.colorToRGB bg ]

extraAttr :: [AttrOp TextTag]
extraAttr = [textTagWeight := fromEnum WeightBold]
--          , textTagStretch := StretchUltraExpanded

-- | Add a frame to be drawn.
display :: ScreenContent
        -> FrontendSession
        -> SingleFrame
        -> IO ()
display coscreen FrontendSession{..} SingleFrame{singleArray} = do
  let f !w (!n, !l) = if n == -1
                      then (rwidth coscreen - 2, Color.charFromW32 w : '\n' : l)
                      else (n - 1, Color.charFromW32 w : l)
      (_, levelChar) =
        PointArray.foldrA' f (rwidth coscreen - 1, []) singleArray
      !gfChar = T.pack levelChar
  postGUISync $ do
    tb <- textViewGetBuffer sview
    textBufferSetText tb gfChar
    ib <- textBufferGetStartIter tb
    ie <- textIterCopy ib
    let defEnum = toEnum $ fromAttr Color.defAttr
        setTo :: (X, Word32) -> Color.AttrCharW32 -> IO (X, Word32)
        setTo (!lx, !previous) !w
          | (lx + 1) `mod` (rwidth coscreen + 1) /= 0 = do
            let current :: Word32
                current = Color.attrCharW32 w
            if current == previous
            then return (lx + 1, previous)
            else do
              textIterSetOffset ie lx
              when (previous /= defEnum) $
                textBufferApplyTag tb (stags IM.! fromEnum previous) ib ie
              textIterSetOffset ib lx
              return (lx + 1, current)
        setTo (lx, previous) w = setTo (lx + 1, previous) w
    (lx, previous) <- PointArray.foldMA' setTo (-1, defEnum) singleArray
    textIterSetOffset ie lx
    when (previous /= defEnum) $
      textBufferApplyTag tb (stags IM.! fromEnum previous) ib ie
#endif
