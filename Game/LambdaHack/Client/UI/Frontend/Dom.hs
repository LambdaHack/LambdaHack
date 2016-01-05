{-# LANGUAGE CPP #-}
-- | Text frontend running in Browser or in Webkit.
module Game.LambdaHack.Client.UI.Frontend.Dom
  ( startup, frontendName
  ) where

import Prelude ()
import Prelude.Compat

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader (ask, liftIO)
import Data.Char (chr)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM (WebView, enableInspector, postGUISync, runWebGUI,
                  webViewGetDomDocument)
import GHCJS.DOM.CSSStyleDeclaration (removeProperty, setProperty)
import GHCJS.DOM.Document (createElement, getBody, keyDown, keyPress)
import GHCJS.DOM.Element (click, contextMenu, getStyle, setInnerHTML)
import GHCJS.DOM.EventM (mouseAltKey, mouseButton, mouseCtrlKey, mouseMetaKey,
                         mouseShiftKey, on, preventDefault)
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
import GHCJS.DOM.Types (CSSStyleDeclaration, castToHTMLDivElement)
import GHCJS.DOM.UIEvent (getCharCode, getKeyCode, getWhich)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
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
  let lxsize = fst normalLevelBound + 1
      lysize = snd normalLevelBound + 4
      cell = "<td>" ++ [chr 160]
      row = "<tr>" ++ concat (replicate lxsize cell)
      rows = concat (replicate lysize row)
  Just tableElem <- fmap castToHTMLTableElement
                     <$> createElement doc (Just ("table" :: Text))
  void $ appendChild divBlock (Just tableElem)
  Just scharStyle <- getStyle tableElem
  -- Speed: http://www.w3.org/TR/CSS21/tables.html#fixed-table-layout
  setProp scharStyle "table-layout" "fixed"
  -- Set the font specified in config, if any.
  setProp scharStyle "font-family" $ fromMaybe "Monospace" sfontFamily
  setProp scharStyle "font-size" $ fromMaybe "18px" sfontSize
  -- Get rid of table spacing. Tons of spurious hacks just in case.
  setCellPadding tableElem ("0" :: Text)
  setCellSpacing tableElem ("0" :: Text)
  setProp scharStyle "outline" "1px solid grey"
  setProp scharStyle "padding" "0 0 0 0"
  setProp scharStyle "border-collapse" "collapse"
  setProp scharStyle "margin-left" "auto"
  setProp scharStyle "margin-right" "auto"
  -- TODO: for icons, in <td>
  -- setProp "display" "block"
  -- setProp "vertical-align" "bottom"
  -- Create the session record.
  setInnerHTML tableElem $ Just rows
  scharCells <- flattenTable tableElem
  let sess = FrontendSession{..}
  rf <- createRawFrontend (display sdebugCli sess) shutdown
  -- Handle keypresses. http://unixpapa.com/js/key.html
  -- A bunch of fauity hacks; @keyPress@ doesn't handle non-character keys
  -- while with @keeDown@ all of getKeyIdentifier, getWhich and getKeyCode
  -- return absurd codes for, e.g., semicolon in Chromium.
  -- The new standard might help on newer browsers
  -- http://www.w3schools.com/jsref/event_key_keycode.asp
  -- but webkit doesn't give access to it yet.
  let readMod = do
        modCtrl <- ask >>= getCtrlKey
        modShift <- ask >>= getShiftKey
        modAlt <- ask >>= getAltKey
        modMeta <- ask >>= getMetaKey
        modAltG <- ask >>= getAltGraphKey
        return $! modifierTranslate
                    modCtrl modShift (modAlt || modAltG) modMeta
  void $ doc `on` keyDown $ do
    keyId <- ask >>= getKeyIdentifier
    which <- ask >>= getWhich
    keyCode <- ask >>= getKeyCode
    charCode <- ask >>= getCharCode
    modifier <- readMod
    let keyIdBogus = keyId `elem` ["", "Unidentified"]
                     || take 2 keyId == "U+"
        -- Handle browser quirks and webkit non-conformance to standards,
        -- especially for ESC, etc.
        quirksN | which == 0 = ""
                | charCode /= 0 = ""
                | not keyIdBogus = keyId
                | keyCode == 8 = "Backspace"
                | keyCode == 9 =
                  if modifier == K.Shift then "BackTab" else "Tab"
                | keyCode == 27 = "Escape"
                | keyCode == 46 = "Delete"
                | otherwise = ""  -- TODO: translate from keyCode in FF and IE
                                  -- until @key@ available in webkit DOM
    when (not $ null quirksN) $ do
      let !key = K.keyTranslateWeb quirksN False
          !pointer = Nothing
          !km = K.KM{..}
          _ks = T.unpack (K.showKey key)
      -- liftIO $ do
      --   putStrLn $ "keyId: " ++ keyId
      --   putStrLn $ "quirksN: " ++ quirksN
      --   putStrLn $ "key: " ++ _ks
      --   putStrLn $ "which: " ++ show which
      --   putStrLn $ "keyCode: " ++ show keyCode
      --   putStrLn $ "modifier: " ++ show modifier
      liftIO $ saveKM rf km
      -- Pass through Ctrl-+ and others, disable Tab.
      when (modifier `elem` [K.NoModifier, K.Shift]) preventDefault
  void $ doc `on` keyPress $ do
    keyLoc <- ask >>= getKeyLocation
    which <- ask >>= getWhich
    keyCode <- ask >>= getKeyCode
    charCode <- ask >>= getCharCode
    let quirksN | which == 0 = [chr keyCode]  -- old IE
                | charCode /= 0 = [chr which]  -- all others
                | otherwise = ""
    when (not $ null quirksN) $ do
      modifier <- readMod
      let !onKeyPad = case keyLoc of
            3 {-KEY_LOCATION_NUMPAD-} -> True
            _ -> False
          !key = K.keyTranslateWeb quirksN onKeyPad
          !pointer = Nothing
          !modifierNoShift =  -- to prevent Shift-!, etc.
            if modifier == K.Shift then K.NoModifier else modifier
          !km = K.KM{modifier=modifierNoShift, ..}
          _ks = T.unpack (K.showKey key)
      -- liftIO $ do
      --   putStrLn $ "charCode: " ++ show charCode
      --   putStrLn $ "quirksN: " ++ quirksN
      --   putStrLn $ "key: " ++ _ks
      --   putStrLn $ "which: " ++ show which
      --   putStrLn $ "keyCode: " ++ show keyCode
      --   putStrLn $ "modifier: " ++ show modifier
      liftIO $ saveKM rf km
      -- Pass through Ctrl-+ and others, disable Tab.
      when (modifier `elem` [K.NoModifier, K.Shift]) preventDefault
  -- Handle mouseclicks, per-cell.
  let xs = [0..lxsize - 1]
      ys = [0..lysize - 1]
      xys = concat $ map (\y -> zip xs (repeat y)) ys
  mapM_ (handleMouse rf) $ zip scharCells xys
  let setBorder tcell = do
        Just style <- getStyle tcell
        setProp style "border" "1px solid transparent"
  mapM_ setBorder scharCells
  -- Display at the end to avoid redraw
  void $ appendChild body (Just divBlock)
  putMVar rfMVar rf  -- send to client only after the whole webpage is set up
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

-- | Let each table cell handle mouse events inside.
handleMouse :: RawFrontend -> (HTMLTableCellElement, (Int, Int)) -> IO ()
handleMouse rf (cell, (cx, cy)) = do
  let saveMouse = do
        -- https://hackage.haskell.org/package/ghcjs-dom-0.2.1.0/docs/GHCJS-DOM-EventM.html
        but <- mouseButton
        modCtrl <- mouseCtrlKey
        modShift <- mouseShiftKey
        modAlt <- mouseAltKey
        modMeta <- mouseMetaKey
        let !modifier = modifierTranslate modCtrl modShift modAlt modMeta
        liftIO $ do
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
              !km = K.KM{..}
              _ks = T.unpack (K.showKey key)
          -- liftIO $ putStrLn $ "m: " ++ _ks ++ show modifier ++ show pointer
          -- Store the mouse event coords in the keypress channel.
          liftIO $ saveKM rf km
  void $ cell `on` contextMenu $ do
    saveMouse
    preventDefault
  void $ cell `on` click $
    saveMouse

-- | Get the list of all cells of an HTML table.
flattenTable :: HTMLTableElement -> IO [HTMLTableCellElement]
flattenTable table = do
  let lxsize = fromIntegral $ fst normalLevelBound + 1
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
display DebugModeCli{scolorIsBold}
        FrontendSession{..}
        SingleFrame{sfLevel} = postGUISync $ do
  let setChar :: (HTMLTableCellElement, Color.AttrChar) -> IO ()
      setChar (cell, Color.AttrChar{acAttr=acAttr@Color.Attr{..}, acChar}) = do
        let s = if acChar == ' ' then [chr 160] else [acChar]
        setInnerHTML cell $ Just s
        Just style <- getStyle cell
        if acAttr == Color.defAttr then do
          removeProp style "background-color"
          removeProp style "color"
          removeProp style "font-weight"
          setProp style "border-color" "transparent"
        else do
          when (scolorIsBold == Just True) $
            setProp style "font-weight" "bold"
          if bg `elem` [Color.BrRed, Color.BrBlue, Color.BrYellow, Color.defBG]
          then removeProp style "background-color"
          else setProp style "background-color" (Color.colorToRGB bg)
          setProp style "color" (Color.colorToRGB fg)
          case bg of
            Color.BrRed ->  -- highlighted tile
              let ourColor = Color.colorToRGB Color.Red
              in setProp style "border-color" ourColor
            Color.BrBlue ->  -- blue highlighted tile
              let ourColor = Color.colorToRGB Color.Blue
              in setProp style "border-color" ourColor
            Color.BrYellow ->  -- yellow highlighted tile
              let ourColor = Color.colorToRGB Color.BrYellow
              in setProp style "border-color" ourColor
            _ -> setProp style "border-color" "transparent"
      acs = concat $ overlay sfLevel
  -- TODO: Sync or Async?
  callback <- newRequestAnimationFrameCallback $ \_ -> do
    mapM_ setChar $ zip scharCells acs
  -- This ensure no frame redraws while callback executes.
  void $ requestAnimationFrame swebView (Just callback)
