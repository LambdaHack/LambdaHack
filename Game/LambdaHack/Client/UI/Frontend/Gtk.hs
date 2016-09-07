{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Text frontend based on Gtk.
module Game.LambdaHack.Client.UI.Frontend.Gtk
  ( startup, frontendName
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , startupFun, shutdown, doAttr, extraAttr, display, setTo, evalFrame
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude hiding (Alt)

import Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.EnumMap.Strict as EM
import Data.IORef
import qualified Data.Text as T
import Graphics.UI.Gtk hiding (Point)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Point

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { sview :: !TextView                    -- ^ the widget to draw to
  , stags :: !(EM.EnumMap Color.Attr TextTag)  -- ^ text color tags for fg/bg
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
startup sdebugCli = startupBound $ startupFun sdebugCli

startupFun :: DebugModeCli -> MVar RawFrontend -> IO ()
startupFun sdebugCli@DebugModeCli{..} rfMVar = do
  -- Init GUI.
  unsafeInitGUIForThreadedRTS
  -- Text attributes.
  ttt <- textTagTableNew
  stags <- EM.fromDistinctAscList <$>
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
  let sess = FrontendSession{..}
  rf <- createRawFrontend (display sess) shutdown
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
        !pointer = Point 0 0  -- FIXME: workaround for GHC 8.0.1 -- originPoint
    IO.liftIO $ saveKMP rf modifier key pointer
    return True
  -- Set the font specified in config, if any.
  f <- fontDescriptionFromString
       $ fromMaybe "Monospace" sfontFamily <+> fromMaybe "18px" sfontSize
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
    let !modifier = modTranslate mods  -- Shift included
    IO.liftIO $ do
      (bx, by) <-
        textViewWindowToBufferCoords sview TextWindowText
                                     (round wx, round wy)
      (iter, _) <- textViewGetIterAtPosition sview bx by
      cx <- textIterGetLineOffset iter
      cy <- textIterGetLine iter
      let !key = case scrollDir of
            ScrollUp -> K.WheelNorth
            ScrollDown -> K.WheelSouth
            _ -> K.Esc  -- probably a glitch
          !pointer = Point cx cy
      -- Store the mouse event coords in the keypress channel.
      saveKMP rf modifier key pointer
    return True  -- disable selection
  currentfont <- newIORef f
  Just defDisplay <- displayGetDefault
  -- TODO: change cursor depending on aiming mode, etc.; hard
  cursor <- cursorNewForDisplay defDisplay Tcross  -- Target Crosshair Arrow
  sview `on` buttonPressEvent $ return True  -- disable selection
  sview `on` buttonReleaseEvent $ do
    IO.liftIO $ resetChanKey (fchanKey rf)
    but <- eventButton
    (wx, wy) <- eventCoordinates
    mods <- eventModifier
    let !modifier = modTranslate mods  -- Shift included
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
  widgetModifyBase sview StateNormal black
  widgetModifyText sview StateNormal white
  -- Set up the main window.
  w <- windowNew
  containerAdd w sview
  onDestroy w (error "Window killed")
  widgetShowAll w
  mainGUI

-- TODO: is postGUISync required?
shutdown :: IO ()
shutdown = postGUISync mainQuit

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

-- | Add a frame to be drawn.
display :: FrontendSession  -- ^ frontend session data
        -> SingleFrame      -- ^ the screen frame to draw
        -> IO ()
display sess@FrontendSession{..} frame = do
  let !GtkFrame{..} = evalFrame sess frame
      !defAttr = stags EM.! Color.defAttr
      attrs = zip [0..] gfAttr
  postGUISync $ do
    tb <- textViewGetBuffer sview
    textBufferSetByteString tb gfChar
    mapM_ (setTo tb defAttr) attrs

setTo :: TextBuffer -> TextTag -> (Int, [TextTag]) -> IO ()
setTo _ _ (_,  []) = return ()
setTo tb defAttr (ly, attr:attrs) = do
  ib <- textBufferGetIterAtLineOffset tb ly 0
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
evalFrame FrontendSession{stags} SingleFrame{singleFrame} =
  let f Color.AttrChar{acAttr=acAttr@Color.Attr{..}} =
        let acAttr1 = case bg of
              Color.BrRed ->
                Color.Attr Color.defBG Color.defFG  -- highlighted tile
              Color.BrBlue ->  -- blue highlighted tile
                if fg /= Color.Blue
                then Color.Attr fg Color.Blue
                else Color.Attr fg Color.BrBlack
              Color.BrYellow ->  -- yellow highlighted tile
                if fg /= Color.BrBlack
                then Color.Attr fg Color.BrBlack
                else Color.Attr fg Color.defFG
              _ -> acAttr
        in stags EM.! acAttr1
      gfAttr = map (map f) singleFrame
      levelChar =
        unlines $ map (map (\Color.AttrChar{acChar} -> acChar)) singleFrame
      gfChar = BS.pack $ init levelChar
  in GtkFrame{..}
