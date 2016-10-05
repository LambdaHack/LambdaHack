-- | Text frontend running in Browser or in Webkit.
module Game.LambdaHack.Client.UI.Frontend.Dom
  ( startup, frontendName
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

#if USE_WEBKIT
import GHCJS.DOM (enableInspector, runWebGUI)
#endif

import Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.Reader (ask)
import Data.Char (chr)
import GHCJS.DOM (currentDocument, currentWindow, postGUISync)
import GHCJS.DOM.CSSStyleDeclaration (removeProperty, setProperty)
import GHCJS.DOM.Document (createElement, getBody, keyDown, keyPress)
import GHCJS.DOM.Element (contextMenu, getStyle, mouseUp, setInnerHTML, wheel)
import GHCJS.DOM.EventM (EventM, mouseAltKey, mouseButton, mouseCtrlKey,
                         mouseMetaKey, mouseShiftKey, on, preventDefault)
import GHCJS.DOM.HTMLCollection (item)
import GHCJS.DOM.HTMLTableCellElement (HTMLTableCellElement,
                                       castToHTMLTableCellElement)
import GHCJS.DOM.HTMLTableElement (HTMLTableElement, castToHTMLTableElement,
                                   getRows, setCellPadding, setCellSpacing)
import GHCJS.DOM.HTMLTableRowElement (HTMLTableRowElement,
                                      castToHTMLTableRowElement, getCells)
import GHCJS.DOM.KeyboardEvent (getAltGraphKey, getAltKey, getCtrlKey,
                                getKeyIdentifier, getKeyLocation, getMetaKey,
                                getShiftKey)
import GHCJS.DOM.Node (appendChild, setTextContent)
import GHCJS.DOM.RequestAnimationFrameCallback
import GHCJS.DOM.Types (CSSStyleDeclaration, DOM, DOMContext, IsMouseEvent,
                        askDOM, castToHTMLDivElement, runDOM)
import GHCJS.DOM.UIEvent (getCharCode, getKeyCode, getWhich)
import GHCJS.DOM.WheelEvent (getDeltaY)
import GHCJS.DOM.Window (requestAnimationFrame)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.JSFile (domContextUnsafe)
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { sdomContext :: !DOMContext
  , scharStyle  :: !CSSStyleDeclaration
  , scharCells  :: ![HTMLTableCellElement]
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
  rfMVar <- newEmptyMVar
  flip runDOM domContextUnsafe $ postGUISync $ runWeb sdebugCli rfMVar
  takeMVar rfMVar
#elif USE_WEBKIT
  startupBound $ \rfMVar -> runWebGUI $ \webView -> do
    enableInspector webView  -- enables Inspector in Webkit
    runWeb sdebugCli rfMVar
#else
terrible error
#endif

runWeb :: DebugModeCli -> MVar RawFrontend -> DOM ()
runWeb sdebugCli@DebugModeCli{..} rfMVar = do
  -- Init the document.
  sdomContext <- askDOM
  Just doc <- currentDocument
  Just body <- getBody doc
  Just pageStyle <- getStyle body
  setProp pageStyle "background-color" (Color.colorToRGB Color.Black)
  setProp pageStyle "color" (Color.colorToRGB Color.White)
  Just divBlockRaw <- createElement doc (Just ("div" :: Text))
  divBlock <- castToHTMLDivElement divBlockRaw
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
  Just tableElemRaw <- createElement doc (Just ("table" :: Text))
  tableElem <- castToHTMLTableElement tableElemRaw
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
  rf <- IO.liftIO $ createRawFrontend (display sdebugCli sess) shutdown
  -- Handle keypresses. http://unixpapa.com/js/key.html
  -- A bunch of fauity hacks; @keyPress@ doesn't handle non-character keys
  -- while with @keyDown@ all of getKeyIdentifier, getWhich and getKeyCode
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
          _ks = K.showKey key
      -- IO.liftIO $ do
      --   putStrLn $ "keyId: " ++ keyId
      --   putStrLn $ "quirksN: " ++ quirksN
      --   putStrLn $ "key: " ++ _ks
      --   putStrLn $ "which: " ++ show which
      --   putStrLn $ "keyCode: " ++ show keyCode
      --   putStrLn $ "modifier: " ++ show modifier
      IO.liftIO $ saveKMP rf modifier key originPoint
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
          !modifierNoShift =  -- to prevent Shift-!, etc.
            if modifier == K.Shift then K.NoModifier else modifier
          _ks = K.showKey key
      -- IO.liftIO $ do
      --   putStrLn $ "charCode: " ++ show charCode
      --   putStrLn $ "quirksN: " ++ quirksN
      --   putStrLn $ "key: " ++ _ks
      --   putStrLn $ "which: " ++ show which
      --   putStrLn $ "keyCode: " ++ show keyCode
      --   putStrLn $ "modifier: " ++ show modifier
      IO.liftIO $ saveKMP rf modifierNoShift key originPoint
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
  IO.liftIO $ putMVar rfMVar rf
    -- send to client only after the whole webpage is set up
    -- because there is no @mainGUI@ to start accepting

shutdown :: IO ()
shutdown = return () -- nothing to clean up

setProp :: CSSStyleDeclaration -> Text -> Text -> DOM ()
setProp style propRef propValue =
  setProperty style propRef (Just propValue) ("" :: Text)

removeProp :: CSSStyleDeclaration -> Text -> DOM ()
removeProp style propRef = do
  (_t :: Maybe Text) <- removeProperty style propRef
  return ()

-- | Let each table cell handle mouse events inside.
handleMouse :: RawFrontend -> (HTMLTableCellElement, (Int, Int)) -> DOM ()
handleMouse rf (cell, (cx, cy)) = do
  let readMod :: IsMouseEvent e => EventM HTMLTableCellElement e K.Modifier
      readMod = do
        modCtrl <- mouseCtrlKey
        modShift <- mouseShiftKey
        modAlt <- mouseAltKey
        modMeta <- mouseMetaKey
        return $! modifierTranslate modCtrl modShift modAlt modMeta
      saveWheel = do
        wheelY <- ask >>= getDeltaY
        modifier <- readMod
        let mkey = if | wheelY < -0.01 -> Just K.WheelNorth
                      | wheelY > 0.01 -> Just K.WheelSouth
                      | otherwise -> Nothing  -- probably a glitch
            pointer = Point cx cy
        maybe (return ())
              (\key -> IO.liftIO $ saveKMP rf modifier key pointer) mkey
      saveMouse = do
        -- https://hackage.haskell.org/package/ghcjs-dom-0.2.1.0/docs/GHCJS-DOM-EventM.html
        but <- mouseButton
        modifier <- readMod
      -- TODO: mdrawWin <- displayGetWindowAtPointer display
      -- let setCursor (drawWin, _, _) =
      --       drawWindowSetCursor drawWin (Just cursor)
      -- maybe (return ()) setCursor mdrawWin
        let mkey = case but of
              0 -> Just K.LeftButtonRelease
              1 -> Just K.MiddleButtonRelease
              2 -> Just K.RightButtonRelease  -- not handled in contextMenu
              _ -> Nothing  -- probably a glitch
            pointer = Point cx cy
          -- _ks = T.unpack (K.showKey key)
          -- IO.liftIO $ putStrLn $ "m: " ++ _ks ++ show modifier ++ show pointer
        maybe (return ())
              (\key -> IO.liftIO $ saveKMP rf modifier key pointer) mkey
  void $ cell `on` wheel $ do
    saveWheel
    preventDefault
  void $ cell `on` contextMenu $
    preventDefault
  void $ cell `on` mouseUp $ do
    saveMouse
    preventDefault

-- | Get the list of all cells of an HTML table.
flattenTable :: HTMLTableElement -> DOM [HTMLTableCellElement]
flattenTable table = do
  let lxsize = fromIntegral $ fst normalLevelBound + 1
      lysize = fromIntegral $ snd normalLevelBound + 4
  Just rows <- getRows table
  let f y = do
        Just rowsItem <- item rows y
        castToHTMLTableRowElement rowsItem
  lrow <- mapM f [0..lysize-1]
  let getC :: HTMLTableRowElement -> DOM [HTMLTableCellElement]
      getC row = do
        Just cells <- getCells row
        let g x = do
              Just cellsItem <- item cells x
              castToHTMLTableCellElement cellsItem
        mapM g [0..lxsize-1]
  lrc <- mapM getC lrow
  return $! concat lrc

-- | Output to the screen via the frontend.
display :: DebugModeCli
        -> FrontendSession  -- ^ frontend session data
        -> SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display DebugModeCli{scolorIsBold}
        FrontendSession{..}
        SingleFrame{singleFrame} = flip runDOM sdomContext $ postGUISync $ do
  let setChar (cell, Color.AttrChar{acAttr=Color.Attr{..}, acChar}) = do
        case acChar of
          ' ' -> setTextContent cell $ Just [chr 160]
          ch -> setTextContent cell $ Just [ch]
        Just style <- getStyle cell
        case fg of
          Color.White -> do
            removeProp style "color"
            when (scolorIsBold == Just True) $
              removeProp style "font-weight"
          c -> do
            setProp style "color" $ Color.colorToRGB c
            when (scolorIsBold == Just True) $
              setProp style "font-weight" "bold"
        case bg of
          Color.Black -> do
            setProp style "border-color" "transparent"
            removeProp style "background-color"
          Color.BrRed -> do  -- highlighted tile
            setProp style "border-color" $ Color.colorToRGB Color.Red
            removeProp style "background-color"
          Color.BrBlue -> do  -- blue highlighted tile
            setProp style "border-color" $ Color.colorToRGB Color.Blue
            removeProp style "background-color"
          Color.BrYellow -> do  -- yellow highlighted tile
            setProp style "border-color" $ Color.colorToRGB Color.BrYellow
            removeProp style "background-color"
          c -> do
            setProp style "border-color" "transparent"
            setProp style "background-color" $ Color.colorToRGB c
      acs = PointArray.foldrA (\w l ->
              Color.attrCharFromW32 w : l) [] singleFrame
  -- Sync, no point mutitasking threads in the single-threaded JS.
  callback <- newRequestAnimationFrameCallback $ \_ -> do
    mapM_ setChar $ zip scharCells acs
  -- This ensure no frame redraws while callback executes.
  Just win <- currentWindow
  void $ requestAnimationFrame win (Just callback)
