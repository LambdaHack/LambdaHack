{-# LANGUAGE CPP #-}
-- | Text frontend running in Browser or in Webkit.
module Game.LambdaHack.Client.UI.Frontend.Dom
  ( -- * Session data type for the frontend
    FrontendSession(sescMVar)
    -- * The output and input operations
  , fdisplay, fpromptGetKey, fsyncFrames
    -- * Frontend administration tools
  , frontendName, startup
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Ex hiding (handle)
import Control.Monad
import Control.Monad.Reader (ask, liftIO)
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.Maybe
import Data.String (IsString (..))
import GHCJS.DOM (WebView, enableInspector, postGUISync, runWebGUI,
                  webViewGetDomDocument)
import GHCJS.DOM.CSSStyleDeclaration (setProperty)
import GHCJS.DOM.Document (click, createElement, getBody, keyPress)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Element (getStyle)
import GHCJS.DOM.EventM (mouseAltKey, mouseButton, mouseClientXY, mouseCtrlKey,
                         mouseMetaKey, mouseShiftKey, on)
import GHCJS.DOM.HTMLElement (setInnerText)
import GHCJS.DOM.HTMLParagraphElement (HTMLParagraphElement,
                                       castToHTMLParagraphElement)
import GHCJS.DOM.KeyboardEvent (getAltGraphKey, getAltKey, getCtrlKey,
                                getKeyIdentifier, getKeyLocation, getMetaKey,
                                getShiftKey)
import GHCJS.DOM.Node (appendChild)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Point

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { swebView   :: !WebView
  , sparagraph :: !HTMLParagraphElement
  , schanKey   :: !(STM.TQueue K.KM)  -- ^ channel for keyboard input
  , sescMVar   :: !(Maybe (MVar ()))
  , sdebugCli  :: !DebugModeCli  -- ^ client configuration
  }

-- | The name of the frontend.
frontendName :: String
#ifdef USE_BROWSER
frontendName = "browser"
#elif USE_WEBKIT
frontendName = "webkit"
#else
terrible error
#endif

-- | Starts the main program loop using the frontend input and output.
startup :: DebugModeCli -> (FrontendSession -> IO ()) -> IO ()
startup sdebugCli k = runWebGUI $ runWeb sdebugCli k

runWeb :: DebugModeCli -> (FrontendSession -> IO ()) -> WebView -> IO ()
runWeb sdebugCli@DebugModeCli{sfont} k swebView = do
  -- Init the document.
  enableInspector swebView  -- enables Inspector in Webkit
  Just doc <- webViewGetDomDocument swebView
  Just body <- getBody doc
  -- Set up the HTML.
  setInnerHTML body (Just ("<h1>LambdaHack</h1>" :: String))
  Just sparagraph <- fmap castToHTMLParagraphElement <$> createElement doc (Just ("p" :: String))
  setInnerText sparagraph $ Just $ ("Please wait..." :: String)
  Just style <- getStyle sparagraph
  let setProp :: String -> String -> IO ()
      setProp propRef propValue =
        setProperty style propRef (Just propValue) ("" :: String)
  -- Set the font specified in config, if any.
  let font = "Monospace normal normal normal normal 14" -- fromMaybe "" sfont
  -- setProp "font" font
      {-
font-family: 'Times New Roman';
font-kerning: auto;
font-size: 16px;
font-style: normal;
font-variant: normal;
font-variant-ligatures: normal;
font-weight: normal;
      -}
  setProp "font-family" "Monospace"
  -- Modify default colours.
  setProp "background-color" (Color.colorToRGB Color.Black)
  setProp "color" (Color.colorToRGB Color.White)
  void $ appendChild body (Just sparagraph)
  -- Create the session record.
  schanKey <- STM.atomically STM.newTQueue
  escMVar <- newEmptyMVar
  let sess = FrontendSession{sescMVar = Just escMVar, ..}
  -- Fork the game logic thread. When logic ends, game exits.
  aCont <- async $ k sess `Ex.finally` return ()  --- TODO: webkit cleanup?
  link aCont
  -- Fill the keyboard channel.
  let flushChanKey = do
        res <- STM.atomically $ STM.tryReadTQueue schanKey
        when (isJust res) flushChanKey
  void $ doc `on` keyPress $ do
    -- https://hackage.haskell.org/package/webkitgtk3-0.14.1.0/docs/Graphics-UI-Gtk-WebKit-DOM-KeyboardEvent.html
    n <- ask >>= getKeyIdentifier
    _keyLoc <- ask >>= getKeyLocation  -- TODO: KEY_LOCATION_NUMPAD
    modCtrl <- ask >>= getCtrlKey
    modShift <- ask >>= getShiftKey
    modAlt <- ask >>= getAltKey
    modMeta <- ask >>= getMetaKey
    modAltG <- ask >>= getAltGraphKey
    let !key = K.keyTranslate n
        !modifier = let md = modifierTranslate
                               modCtrl modShift (modAlt || modAltG) modMeta
                    in if md == K.Shift then K.NoModifier else md
        !pointer = Nothing
    liftIO $ do
      unless (deadKey n) $ do
        -- If ESC, also mark it specially and reset the key channel.
        when (key == K.Esc) $ do
          void $ tryPutMVar escMVar ()
          flushChanKey
        -- Store the key in the channel.
        STM.atomically $ STM.writeTQueue schanKey K.KM{..}
  -- Take care of the mouse events.
  void $ doc `on` click $ do
    -- https://hackage.haskell.org/package/ghcjs-dom-0.2.1.0/docs/GHCJS-DOM-EventM.html
    liftIO flushChanKey
    but <- mouseButton
    (wx, wy) <- mouseClientXY
    modCtrl <- mouseCtrlKey
    modShift <- mouseShiftKey
    modAlt <- mouseAltKey
    modMeta <- mouseMetaKey
    let !modifier = modifierTranslate modCtrl modShift modAlt modMeta
    liftIO $ do
      -- TODO: Graphics.UI.Gtk.WebKit.DOM.Selection? ClipboardEvent?
      -- hasSelection <- textBufferHasSelection tb
      -- unless hasSelection $ do
      -- TODO: mdrawWin <- displayGetWindowAtPointer display
      -- let setCursor (drawWin, _, _) =
      --       drawWindowSetCursor drawWin (Just cursor)
      -- maybe (return ()) setCursor mdrawWin
      let (cx, cy) = windowToTextCoords (wx, wy)
          !key = case but of
            0 -> K.LeftButtonPress
            1 -> K.MiddleButtonPress
            2 -> K.RightButtonPress
            _ -> K.LeftButtonPress
          !pointer = Just $! Point cx (cy - 1)
      -- Store the mouse even coords in the keypress channel.
      STM.atomically $ STM.writeTQueue schanKey K.KM{..}
  return ()  -- nothing to clean up

windowToTextCoords :: (Int, Int) -> (Int, Int)
windowToTextCoords (x, y) = (x, y)  -- TODO

-- | Output to the screen via the frontend.
fdisplay :: FrontendSession    -- ^ frontend session data
         -> Maybe SingleFrame  -- ^ the screen frame to draw
         -> IO ()
fdisplay _ Nothing = return ()
fdisplay FrontendSession{sparagraph} (Just rawSF) = postGUISync $ do
  let SingleFrame{sfLevel} = overlayOverlay rawSF
      ls = map (map Color.acChar . decodeLine) sfLevel ++ []
      s = intercalate ['\n'] ls
  setInnerText sparagraph $ Just s
  return ()

fsyncFrames :: FrontendSession -> IO ()
fsyncFrames _ = return ()

-- | Display a prompt, wait for any key.
fpromptGetKey :: FrontendSession -> SingleFrame -> IO K.KM
fpromptGetKey sess@FrontendSession{schanKey} frame = do
  fdisplay sess $ Just frame
  STM.atomically $ STM.readTQueue schanKey

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
modifierTranslate :: Bool -> Bool -> Bool -> Bool -> K.Modifier
modifierTranslate modCtrl modShift modAlt modMeta
  | modCtrl = K.Control
  | modAlt || modMeta = K.Alt
  | modShift = K.Shift
  | otherwise = K.NoModifier

keyTranslate :: Char -> K.KM
keyTranslate e = (\(key, modifier) -> K.toKM modifier key) $
  case e of
    '\ESC' -> (K.Esc,     K.NoModifier)
    '\n'   -> (K.Return,  K.NoModifier)
    '\r'   -> (K.Return,  K.NoModifier)
    ' '    -> (K.Space,   K.NoModifier)
    '\t'   -> (K.Tab,     K.NoModifier)
    c | ord '\^A' <= ord c && ord c <= ord '\^Z' ->
        -- Alas, only lower-case letters.
        (K.Char $ chr $ ord c - ord '\^A' + ord 'a', K.Control)
        -- Movement keys are more important than leader picking,
        -- so disabling the latter and interpreting the keypad numbers
        -- as movement:
      | c `elem` ['1'..'9'] -> (K.KP c,              K.NoModifier)
      | otherwise           -> (K.Char c,            K.NoModifier)
