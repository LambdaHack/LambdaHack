{-# LANGUAGE CPP #-}
-- | Text frontend running in Browser or in Webkit.
module Game.LambdaHack.Client.UI.Frontend.Dom
  ( startup, frontendName
  ) where

import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Monad
import Control.Monad.Reader (ask, liftIO)
import Data.Bits ((.|.))
import Data.Char (chr, isUpper, toLower)
import Data.Maybe
import Data.String (IsString (..))
import GHCJS.DOM (WebView, enableInspector, postGUISync, runWebGUI,
                  webViewGetDomDocument)
import GHCJS.DOM.CSSStyleDeclaration (getPropertyValue, setProperty)
import GHCJS.DOM.Document (createElement, getBody, keyDown)
import GHCJS.DOM.Element (getStyle, setInnerHTML)
import GHCJS.DOM.EventM (mouseAltKey, mouseButton, mouseCtrlKey, mouseMetaKey,
                         mouseShiftKey, on)
import GHCJS.DOM.EventTargetClosures (EventName (EventName))
import GHCJS.DOM.HTMLCollection (item)
import GHCJS.DOM.HTMLElement (setInnerText)
import GHCJS.DOM.HTMLTableCellElement (HTMLTableCellElement,
                                       castToHTMLTableCellElement)
import GHCJS.DOM.HTMLTableElement (HTMLTableElement, castToHTMLTableElement,
                                   getRows, setCellPadding, setCellSpacing)
import GHCJS.DOM.HTMLTableRowElement (HTMLTableRowElement,
                                      castToHTMLTableRowElement, getCells)
import GHCJS.DOM.KeyboardEvent (getAltGraphKey, getAltKey, getCtrlKey,
                                getKeyIdentifier, getKeyLocation, getMetaKey,
                                getShiftKey)
import GHCJS.DOM.Node (appendChild, cloneNode)
import GHCJS.DOM.Types (CSSStyleDeclaration, MouseEvent)
import GHCJS.DOM.UIEvent (getKeyCode, getWhich)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { swebView    :: !WebView
  , scharStyle  :: !CSSStyleDeclaration
  , scharCells  :: ![HTMLTableCellElement]
  , scharStyle2 :: !CSSStyleDeclaration
  , scharCells2 :: ![HTMLTableCellElement]
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

-- TODO: test if runWebGUI already starts the frontend in a bound thread
-- | Starts the main program loop using the frontend input and output.
startup :: DebugModeCli -> IO RawFrontend
startup sdebugCli = do
#ifdef USE_BROWSER
  startupAsync $ \rfMVar -> runWebGUI $ runWeb sdebugCli rfMVar
#elif USE_WEBKIT
  startupBound $ \rfMVar -> runWebGUI $ runWeb sdebugCli rfMVar
#else
terrible error
#endif

runWeb :: DebugModeCli -> MVar RawFrontend -> WebView -> IO ()
runWeb sdebugCli@DebugModeCli{sfont} rfMVar swebView = do
  -- Init the document.
  enableInspector swebView  -- enables Inspector in Webkit
  Just doc <- webViewGetDomDocument swebView
  Just body <- getBody doc
  -- Set up the HTML.
  setInnerHTML body (Just ("<h1>LambdaHack</h1>" :: String))
  let lxsize = fst normalLevelBound + 1  -- TODO
      lysize = snd normalLevelBound + 4
      cell = "<td>" ++ [chr 160]
      row = "<tr>" ++ concat (replicate lxsize cell)
      rows = concat (replicate lysize row)
  Just tableElem <- fmap castToHTMLTableElement
                     <$> createElement doc (Just ("table" :: String))
  setInnerHTML tableElem (Just (rows :: String))
  Just scharStyle <- getStyle tableElem
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
  setProp scharStyle "font-family" "Monospace"
  -- Get rid of table spacing. Tons of spurious hacks just in case.
  setCellPadding tableElem ("0" :: String)
  setCellSpacing tableElem ("0" :: String)
  setProp scharStyle "border-collapse" "collapse"
  setProp scharStyle "border-spacing" "0"
    -- supposedly no effect with 'collapse'
  setProp scharStyle "border-width" "0"
  setProp scharStyle "margin" "0 0 0 0"
  setProp scharStyle "padding" "0 0 0 0"
  -- TODO: for icons, in <td>
  -- setProp "display" "block"
  -- setProp "vertical-align" "bottom"
  -- Create the session record.
  scharCells <- flattenTable tableElem
  Just tableElem2 <- fmap castToHTMLTableElement <$> cloneNode tableElem True
  scharCells2 <- flattenTable tableElem2
  Just scharStyle2 <- getStyle tableElem2
  let sess = FrontendSession{..}
  rf <- createRawFrontend (display sess) shutdown
  putMVar rfMVar rf  -- send to client ASAP, while wepage is being redrawn
  -- Handle keypresses.
  -- A bunch of fauity hacks; @keyPress@ doesn't handle non-character keys and
  -- @getKeyCode@ then returns wrong characters anyway.
  -- Regardless, it doesn't work: https://bugs.webkit.org/show_bug.cgi?id=20027
  void $ doc `on` keyDown $ do
    -- https://hackage.haskell.org/package/webkitgtk3-0.14.1.0/docs/Graphics-UI-Gtk-WebKit-DOM-KeyboardEvent.html
    -- though: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyIdentifier
    keyId <- ask >>= getKeyIdentifier
    _keyLoc <- ask >>= getKeyLocation
    modCtrl <- ask >>= getCtrlKey
    modShift <- ask >>= getShiftKey
    modAlt <- ask >>= getAltKey
    modMeta <- ask >>= getMetaKey
    modAltG <- ask >>= getAltGraphKey
    which <- ask >>= getWhich
    keyCode <- ask >>= getKeyCode
    let keyIdBogus = keyId `elem` ["", "Unidentified"]
                     || take 2 keyId == "U+"
        -- Handle browser quirks and webkit non-conformance to standards,
        -- especially for ESC, etc. This is still not nearly enough.
        -- Webkit DOM is just too old.
        -- http://www.w3schools.com/jsref/event_key_keycode.asp
        quirksN | not keyIdBogus = keyId
                | otherwise = let c = chr $ which .|. keyCode
                              in [if isUpper c && not modShift
                                  then toLower c
                                  else c]
        !key = K.keyTranslateWeb quirksN
        !modifier = let md = modifierTranslate
                               modCtrl modShift (modAlt || modAltG) modMeta
                    in if md == K.Shift then K.NoModifier else md
        !pointer = Nothing
    liftIO $ do
      {-
      putStrLn keyId
      putStrLn quirksN
      putStrLn $ T.unpack $ K.showKey key
      putStrLn $ show which
      putStrLn $ show keyCode
      -}
      unless (deadKey keyId) $ do
        -- Store the key in the channel.
        STM.atomically $ STM.writeTQueue (fchanKey rf) K.KM{..}
        -- Instantly show any frame waiting for display.
        void $ tryPutMVar (fshowNow rf) ()
  -- Handle mouseclicks, per-cell.
  let xs = [0..lxsize - 1]
      ys = [0..lysize - 1]
      xys = concat $ map (\y -> zip xs (repeat y)) ys
  -- This can't be cloned, so I has to be done for both cell sets.
  mapM_ (handleMouse (fchanKey rf)) $ zip scharCells xys
  mapM_ (handleMouse (fchanKey rf)) $ zip scharCells2 xys
  -- Display at the end to avoid redraw
  void $ appendChild body (Just tableElem)
  setProp scharStyle "display" "none"
  void $ appendChild body (Just tableElem2)
  setProp scharStyle2 "display" "block"
  return ()

setProp :: CSSStyleDeclaration -> String -> String -> IO ()
setProp style propRef propValue =
  setProperty style propRef (Just propValue) ("" :: String)

click :: EventName HTMLTableCellElement MouseEvent
click = EventName "click"

-- | Let each table cell handle mouse events inside.
handleMouse :: STM.TQueue K.KM -> (HTMLTableCellElement, (Int, Int)) -> IO ()
handleMouse fchanKey (cell, (cx, cy)) = do
  void $ cell `on` click $ do
    -- https://hackage.haskell.org/package/ghcjs-dom-0.2.1.0/docs/GHCJS-DOM-EventM.html
    liftIO $ resetChanKey fchanKey
    but <- mouseButton
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
      let !key = case but of
            0 -> K.LeftButtonPress
            1 -> K.MiddleButtonPress
            2 -> K.RightButtonPress
            _ -> K.LeftButtonPress
          !pointer = Just $! Point cx cy
      -- Store the mouse event coords in the keypress channel.
      STM.atomically $ STM.writeTQueue fchanKey K.KM{..}

-- | Get the list of all cells of an HTML table.
flattenTable :: HTMLTableElement -> IO [HTMLTableCellElement]
flattenTable table = do
  let lxsize = fromIntegral $ fst normalLevelBound + 1  -- TODO
      lysize = fromIntegral $ snd normalLevelBound + 4
  Just rows <- getRows table
  lmrow <- mapM (item rows) [0..lysize-1]
  let lrow = map (castToHTMLTableRowElement . fromJust) lmrow
      getC :: HTMLTableRowElement -> IO [HTMLTableCellElement]
      getC row = do
        Just cells <- getCells row
        lmcell <- mapM (item cells) [0..lxsize-1]
        return $! map (castToHTMLTableCellElement . fromJust) lmcell
  lrc <- mapM getC lrow
  return $! concat lrc

shutdown :: IO ()
shutdown = return () -- nothing to clean up

-- | Output to the screen via the frontend.
display :: FrontendSession    -- ^ frontend session data
        -> SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display FrontendSession{..} rawSF = postGUISync $ do
  let setChar :: (HTMLTableCellElement, Color.AttrChar) -> IO ()
      setChar (cell, Color.AttrChar{..}) = do
        let s = if acChar == ' ' then [chr 160] else [acChar]
        setInnerText cell $ Just s
        Just style <- getStyle cell
        setProp style "background-color" (Color.colorToRGB $ Color.bg acAttr)
        setProp style "color" (Color.colorToRGB $ Color.fg acAttr)
      SingleFrame{sfLevel} = overlayOverlay rawSF
      acs = concat $ map decodeLine sfLevel
  -- Double buffering, to avoid redraw after each cell update.
  -- Another possibility (probably less efficient) is visibility:hidden/visible
  -- and yet another is replaceChild.
  Just disp <- getPropertyValue scharStyle ("display" :: String)
  if disp == ("block" :: String)
    then do
      mapM_ setChar $ zip scharCells2 acs
      setProp scharStyle "display" "none"
      setProp scharStyle2 "display" "block"
    else do
      mapM_ setChar $ zip scharCells acs
      setProp scharStyle2 "display" "none"
      setProp scharStyle "display" "block"

-- | Tells a dead key.
deadKey :: (Eq t, IsString t) => t -> Bool
deadKey x = case x of   -- ??? x == "Dead"
  "Dead"        -> True
  "Shift"       -> True
  "Control"     -> True
  "Meta"        -> True
  "Menu"        -> True
  "ContextMenu" -> True
  "Alt"         -> True
  "AltGraph"    -> True
  "Num_Lock"    -> True
  "CapsLock"    -> True
  _             -> False
