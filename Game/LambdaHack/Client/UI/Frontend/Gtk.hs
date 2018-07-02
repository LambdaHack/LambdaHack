{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- | Text frontend based on Gtk.
module Game.LambdaHack.Client.UI.Frontend.Gtk
  ( startup, frontendName
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude hiding (Alt)

import           Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import qualified Data.IntMap.Strict as IM
import           Data.IORef
import qualified Data.Text as T
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Graphics.UI.Gtk hiding (Point)
import           System.Exit (exitFailure)

import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import qualified Game.LambdaHack.Common.Color as Color
import           Game.LambdaHack.Common.Point

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
startupFun coscreen soptions@ClientOptions{..} rfMVar = do
  -- Init GUI.
  unsafeInitGUIForThreadedRTS
  -- Text attributes.
  let emulateBox attr = case attr of
        Color.Attr{bg=Color.HighlightNone,fg} ->
          (fg, Color.Black)
        Color.Attr{bg=Color.HighlightRed} ->
          (Color.Black, Color.defFG)
        Color.Attr{bg=Color.HighlightBlue,fg} ->
          if fg /= Color.Blue
          then (fg, Color.Blue)
          else (fg, Color.BrBlack)
        Color.Attr{bg=Color.HighlightYellow,fg} ->
          if fg /= Color.Brown
          then (fg, Color.Brown)
          else (fg, Color.defFG)
        Color.Attr{bg=Color.HighlightGrey,fg} ->
          if fg /= Color.BrBlack
          then (fg, Color.BrBlack)
          else (fg, Color.defFG)
        Color.Attr{fg} ->
          (fg, Color.Black)
  ttt <- textTagTableNew
  stags <- IM.fromDistinctAscList <$>
             mapM (\ak -> do
                      tt <- textTagNew Nothing
                      textTagTableAdd ttt tt
                      doAttr soptions tt (emulateBox ak)
                      return (fromEnum ak, tt))
               [ Color.Attr{fg, bg}
               | fg <- [minBound..maxBound], bg <- [minBound..maxBound] ]
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
        modifier =
          let md = modTranslate mods
          in if md == K.Shift then K.NoModifier else md
        pointer = originPoint
    when (key == K.Esc) $ IO.liftIO $ resetChanKey (fchanKey rf)
    IO.liftIO $ saveKMP rf modifier key pointer
    return True
  -- Set the font specified in config, if any.
  f <- fontDescriptionFromString
       $ fromMaybe "Monospace" sgtkFontFamily
         <+> maybe "16" tshow sfontSize <> "px"
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
      let pointer = Point cx cy
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
          pointer = Point cx cy
      -- Store the mouse event coords in the keypress channel.
      maybe (return ())
            (\key -> IO.liftIO $ saveKMP rf modifier key pointer) mkey
    return True
  -- Modify default colours.
  let black = Color minBound minBound minBound  -- Color.defBG == Color.Black
      white = Color 0xC500 0xBC00 0xB800        -- Color.defFG == Color.White
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

doAttr :: ClientOptions -> TextTag -> (Color.Color, Color.Color) -> IO ()
doAttr soptions tt (fg, bg)
  | fg == Color.defFG && bg == Color.Black = return ()
  | fg == Color.defFG =
    set tt [textTagBackground := Color.colorToRGB bg]
  | bg == Color.Black =
    set tt $ extraAttr soptions
             ++ [textTagForeground := Color.colorToRGB fg]
  | otherwise =
    set tt $ extraAttr soptions
             ++ [ textTagForeground := Color.colorToRGB fg
                , textTagBackground := Color.colorToRGB bg ]

extraAttr :: ClientOptions -> [AttrOp TextTag]
extraAttr ClientOptions{scolorIsBold} =
  [textTagWeight := fromEnum WeightBold | scolorIsBold == Just True]
--     , textTagStretch := StretchUltraExpanded

-- | Add a frame to be drawn.
display :: ScreenContent
        -> FrontendSession
        -> SingleFrame
        -> IO ()
display coscreen FrontendSession{..} SingleFrame{singleFrame} = do
  let f !w (!n, !l) = if n == -1
                      then (rwidth coscreen - 2, Color.charFromW32 w : '\n' : l)
                      else (n - 1, Color.charFromW32 w : l)
      (_, levelChar) =
        PointArray.foldrA' f (rwidth coscreen - 1, []) singleFrame
      !gfChar = T.pack levelChar
  postGUISync $ do
    tb <- textViewGetBuffer sview
    textBufferSetText tb gfChar
    ib <- textBufferGetStartIter tb
    ie <- textIterCopy ib
    let defEnum = fromEnum Color.defAttr
        setTo :: (X, Int) -> Color.AttrCharW32 -> IO (X, Int)
        setTo (!lx, !previous) !w
          | (lx + 1) `mod` (rwidth coscreen + 1) /= 0 = do
            let current :: Int
                current = Color.attrEnumFromW32 w
            if current == previous
            then return (lx + 1, previous)
            else do
              textIterSetOffset ie lx
              when (previous /= defEnum) $
                textBufferApplyTag tb (stags IM.! previous) ib ie
              textIterSetOffset ib lx
              return (lx + 1, current)
        setTo (lx, previous) w = setTo (lx + 1, previous) w
    (lx, previous) <- PointArray.foldMA' setTo (-1, defEnum) singleFrame
    textIterSetOffset ie lx
    when (previous /= defEnum) $
      textBufferApplyTag tb (stags IM.! previous) ib ie
