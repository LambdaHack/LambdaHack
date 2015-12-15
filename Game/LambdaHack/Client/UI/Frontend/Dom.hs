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
import Data.Text (Text)
import GHCJS.DOM (WebView, enableInspector, postGUISync, runWebGUI,
                  webViewGetDomDocument)
import GHCJS.DOM.CSSStyleDeclaration (removeProperty, setProperty)
import GHCJS.DOM.Document (createElement, getBody, keyDown)
import GHCJS.DOM.Element (getStyle, setInnerHTML)
import GHCJS.DOM.EventM (mouseAltKey, mouseButton, mouseCtrlKey, mouseMetaKey,
                         mouseShiftKey, on)
import GHCJS.DOM.EventTargetClosures (EventName (EventName))
import GHCJS.DOM.HTMLCollection (item)
import GHCJS.DOM.HTMLTableCellElement (HTMLTableCellElement,
                                       castToHTMLTableCellElement)
import GHCJS.DOM.HTMLTableElement (HTMLTableElement, castToHTMLTableElement,
                                   getRows, setCellPadding, setCellSpacing)
import GHCJS.DOM.HTMLTableRowElement (HTMLTableRowElement,
                                      castToHTMLTableRowElement, getCells)
import GHCJS.DOM.JSFFI.Generated.RequestAnimationFrameCallback
import GHCJS.DOM.JSFFI.Generated.Window (requestAnimationFrame)
import GHCJS.DOM.KeyboardEvent (getAltGraphKey, getAltKey, getCtrlKey,
                                getKeyIdentifier, getKeyLocation, getMetaKey,
                                getShiftKey)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.Types (CSSStyleDeclaration, MouseEvent, castToHTMLDivElement)
import GHCJS.DOM.UIEvent (getKeyCode, getWhich)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { swebView   :: !WebView
  , scharStyle :: !CSSStyleDeclaration
  , scharCells :: ![HTMLTableCellElement]
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
runWeb sdebugCli@DebugModeCli{..} rfMVar swebView = do
  -- Init the document.
  enableInspector swebView  -- enables Inspector in Webkit
  Just doc <- webViewGetDomDocument swebView
  Just body <- getBody doc
  Just pageStyle <- getStyle body
  setProp pageStyle "background-color" (Color.colorToRGB Color.Black)
  setProp pageStyle "color" (Color.colorToRGB Color.White)
  Just divBlock <- fmap castToHTMLDivElement
                   <$> createElement doc (Just ("div" :: Text))
  Just divStyle <- getStyle divBlock
  setProp divStyle "text-align" "center"
  case (saddress, stitle) of
    (Just address, Just title) -> do
      let headerText = "<h1><a href=\"" <> address <> "\">"
                       <> title <> "</a></h1>"
      setInnerHTML divBlock (Just headerText)
    _ -> return ()
  let lxsize = fst normalLevelBound + 1  -- TODO
      lysize = snd normalLevelBound + 4
      cell = "<td>" ++ [chr 160]
      row = "<tr>" ++ concat (replicate lxsize cell)
      rows = concat (replicate lysize row)
  Just tableElem <- fmap castToHTMLTableElement
                     <$> createElement doc (Just ("table" :: Text))
  void $ appendChild divBlock (Just tableElem)
  setInnerHTML tableElem $ Just rows
  Just scharStyle <- getStyle tableElem
  -- Speed: http://www.w3.org/TR/CSS21/tables.html#fixed-table-layout
  setProp scharStyle "table-layout" "fixed"
  -- Set the font specified in config, if any.
  let font = "Monospace normal normal normal normal 18" :: Text  -- fromMaybe "" sfont
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
  setCellPadding tableElem ("0" :: Text)
  setCellSpacing tableElem ("0" :: Text)
  setProp scharStyle "padding" "0 0 0 0"
  setProp scharStyle "border" "1px solid grey"
  setProp scharStyle "margin-left" "auto"
  setProp scharStyle "margin-right" "auto"
  -- TODO: for icons, in <td>
  -- setProp "display" "block"
  -- setProp "vertical-align" "bottom"
  -- Create the session record.
  scharCells <- flattenTable tableElem
  let sess = FrontendSession{..}
  rf <- createRawFrontend (display sdebugCli sess) shutdown
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
    {-
    putStrLn keyId
    putStrLn quirksN
    putStrLn $ T.unpack $ K.showKey key
    putStrLn $ show which
    putStrLn $ show keyCode
    -}
    let !km = K.KM{..}
    liftIO $ saveKM rf km
  -- Handle mouseclicks, per-cell.
  let xs = [0..lxsize - 1]
      ys = [0..lysize - 1]
      xys = concat $ map (\y -> zip xs (repeat y)) ys
  mapM_ (handleMouse (fchanKey rf)) $ zip scharCells xys
  -- Display at the end to avoid redraw
  void $ appendChild body (Just divBlock)
  putMVar rfMVar rf  -- send to client only after the whole wegage is set up
                     -- because there is no @mainGUI@ to start accepting

shutdown :: IO ()
shutdown = return () -- nothing to clean up

setProp :: CSSStyleDeclaration -> Text -> Text -> IO ()
setProp style propRef propValue =
  setProperty style propRef (Just propValue) ("" :: Text)

removeProp :: CSSStyleDeclaration -> Text -> IO ()
removeProp style propRef = do
  (_t :: Maybe Text) <- removeProperty style propRef
  return ()

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

-- | Output to the screen via the frontend.
display :: DebugModeCli
        -> FrontendSession  -- ^ frontend session data
        -> SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display DebugModeCli{scolorIsBold} FrontendSession{..} rawSF = postGUISync $ do
  let setChar :: (HTMLTableCellElement, Color.AttrChar) -> IO ()
      setChar (cell, Color.AttrChar{acAttr=acAttr@Color.Attr{..}, acChar}) = do
        let s = if acChar == ' ' then [chr 160] else [acChar]
        setInnerHTML cell $ Just s
        Just style <- getStyle cell
        if acAttr == Color.defAttr then do
          removeProp style "background-color"
          removeProp style "color"
          removeProp style "font-weight"
        else do
          setProp style "background-color" (Color.colorToRGB bg)
          setProp style "color" (Color.colorToRGB fg)
          when (scolorIsBold == Just True) $
            setProp style "font-weight" "bold"
      SingleFrame{sfLevel} = overlayOverlay rawSF
      acs = concat $ map decodeLine sfLevel
  -- TODO: Sync or Async?
  callback <- newRequestAnimationFrameCallbackSync $ \_ -> do
    mapM_ setChar $ zip scharCells acs
  -- This ensure no frame redraws while callback executes.
  void $ requestAnimationFrame swebView (Just callback)
  -- This delay is not enough to always induce UI refresh.
  threadDelay $ 1000000 `div` (4 * 30)
