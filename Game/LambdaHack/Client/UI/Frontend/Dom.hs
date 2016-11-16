-- | Text frontend running in Browser.
module Game.LambdaHack.Client.UI.Frontend.Dom
  ( startup, frontendName
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.Reader (ask)
import Data.Char (chr)
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import GHCJS.DOM (currentDocument, currentWindow)
import GHCJS.DOM.CSSStyleDeclaration (setProperty)
import GHCJS.DOM.Document (createElementUnchecked, getBodyUnchecked, keyDown,
                           keyPress)
import GHCJS.DOM.Element (contextMenu, getStyleUnchecked, mouseUp, setInnerHTML,
                          wheel)
import GHCJS.DOM.EventM (EventM, mouseAltKey, mouseButton, mouseCtrlKey,
                         mouseMetaKey, mouseShiftKey, on, on, preventDefault)
import GHCJS.DOM.HTMLCollection (itemUnchecked)
import GHCJS.DOM.HTMLTableCellElement (HTMLTableCellElement,
                                       castToHTMLTableCellElement)
import GHCJS.DOM.HTMLTableElement (HTMLTableElement, castToHTMLTableElement,
                                   getRowsUnchecked, setCellPadding,
                                   setCellSpacing)
import GHCJS.DOM.HTMLTableRowElement (HTMLTableRowElement,
                                      castToHTMLTableRowElement,
                                      getCellsUnchecked)
import GHCJS.DOM.KeyboardEvent (getAltGraphKey, getAltKey, getCtrlKey,
                                getKeyIdentifier, getKeyLocation, getMetaKey,
                                getShiftKey)
import GHCJS.DOM.Node (appendChild_, setTextContent)
import GHCJS.DOM.RequestAnimationFrameCallback
import GHCJS.DOM.Types (CSSStyleDeclaration, DOM, IsMouseEvent, Window,
                        castToHTMLDivElement, runDOM)
import GHCJS.DOM.UIEvent (getCharCode, getKeyCode, getWhich)
import GHCJS.DOM.WheelEvent (getDeltaY)
import GHCJS.DOM.Window (requestAnimationFrame_)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { scurrentWindow :: !Window
  , scharCells     :: !(V.Vector (HTMLTableCellElement, CSSStyleDeclaration))
  , previousFrame  :: !(IORef SingleFrame)
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "browser"

-- | Starts the main program loop using the frontend input and output.
startup :: DebugModeCli -> IO RawFrontend
startup sdebugCli = do
  rfMVar <- newEmptyMVar
  flip runDOM undefined $ runWeb sdebugCli rfMVar
  takeMVar rfMVar

runWeb :: DebugModeCli -> MVar RawFrontend -> DOM ()
runWeb sdebugCli@DebugModeCli{..} rfMVar = do
  -- Init the document.
  Just doc <- currentDocument
  Just scurrentWindow <- currentWindow
  body <- getBodyUnchecked doc
  pageStyle <- getStyleUnchecked body
  setProp pageStyle "background-color" (Color.colorToRGB Color.Black)
  setProp pageStyle "color" (Color.colorToRGB Color.White)
  divBlockRaw <- createElementUnchecked doc (Just ("div" :: Text))
  divBlock <- castToHTMLDivElement divBlockRaw
  divStyle <- getStyleUnchecked divBlock
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
  tableElemRaw <- createElementUnchecked doc (Just ("table" :: Text))
  tableElem <- castToHTMLTableElement tableElemRaw
  appendChild_ divBlock (Just tableElem)
  scharStyle <- getStyleUnchecked tableElem
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
  previousFrame <- newIORef blankSingleFrame
  let sess = FrontendSession{..}
  rf <- IO.liftIO $ createRawFrontend (display sdebugCli sess) shutdown
  -- Handle keypresses. http://unixpapa.com/js/key.html
  -- A bunch of fauity hacks; @keyPress@ doesn't handle non-character keys
  -- while with @keyDown@ all of getKeyIdentifier, getWhich and getKeyCode
  -- return absurd codes for, e.g., semicolon in Chromium.
  -- TODO: switch to the new standard
  -- http://www.w3schools.com/jsref/event_key_keycode.asp
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
        -- Handle browser quirks, especially for ESC, etc.
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
      let key = K.keyTranslateWeb quirksN False
      -- IO.liftIO $ do
      --   putStrLn $ "keyId: " ++ keyId
      --   putStrLn $ "quirksN: " ++ quirksN
      --   putStrLn $ "key: " ++ K.showKey key
      --   putStrLn $ "which: " ++ show which
      --   putStrLn $ "keyCode: " ++ show keyCode
      --   putStrLn $ "modifier: " ++ show modifier
      IO.liftIO $ saveKMP rf modifier key originPoint
      -- Pass through Ctrl-+ and others, disable Tab.
      when (modifier `elem` [K.NoModifier, K.Shift, K.Control]) preventDefault
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
      let onKeyPad = case keyLoc of
            3 {-KEY_LOCATION_NUMPAD-} -> True
            _ -> False
          key = K.keyTranslateWeb quirksN onKeyPad
          modifierNoShift =  -- to prevent Shift-!, etc.
            if modifier == K.Shift then K.NoModifier else modifier
      -- IO.liftIO $ do
      --   putStrLn $ "charCode: " ++ show charCode
      --   putStrLn $ "quirksN: " ++ quirksN
      --   putStrLn $ "key: " ++ K.showKey key
      --   putStrLn $ "which: " ++ show which
      --   putStrLn $ "keyCode: " ++ show keyCode
      --   putStrLn $ "modifier: " ++ show modifier
      IO.liftIO $ saveKMP rf modifierNoShift key originPoint
      -- Pass through Ctrl-+ and others, disable Tab.
      when (modifier `elem` [K.NoModifier, K.Shift, K.Control]) preventDefault
  -- Handle mouseclicks, per-cell.
  let setupMouse i a = let Point x y = PointArray.punindex lxsize i
                       in handleMouse rf a x y
  V.imapM_ setupMouse scharCells
  let setBorder (_, style) = setProp style "border" "1px solid transparent"
  V.mapM_ setBorder scharCells
  -- Display at the end to avoid redraw
  appendChild_ body (Just divBlock)
  IO.liftIO $ putMVar rfMVar rf
    -- send to client only after the whole webpage is set up
    -- because there is no @mainGUI@ to start accepting

shutdown :: IO ()
shutdown = return () -- nothing to clean up

setProp :: CSSStyleDeclaration -> Text -> Text -> DOM ()
setProp style propRef propValue =
  setProperty style propRef (Just propValue) ("" :: Text)

-- | Let each table cell handle mouse events inside.
handleMouse :: RawFrontend
            -> (HTMLTableCellElement, CSSStyleDeclaration) -> Int -> Int
            -> DOM ()
handleMouse rf (cell, _) cx cy = do
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
        -- IO.liftIO $ putStrLn $
        --   "m: " ++ show but ++ show modifier ++ show pointer
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
flattenTable :: HTMLTableElement
             -> DOM (V.Vector (HTMLTableCellElement, CSSStyleDeclaration))
flattenTable table = do
  let lxsize = fst normalLevelBound + 1
      lysize = snd normalLevelBound + 4
  rows <- getRowsUnchecked table
  let f y = do
        rowsItem <- itemUnchecked rows y
        castToHTMLTableRowElement rowsItem
  lrow <- mapM f [0 .. toEnum (lysize-1)]
  let getC :: HTMLTableRowElement
           -> DOM [(HTMLTableCellElement, CSSStyleDeclaration)]
      getC row = do
        cells <- getCellsUnchecked row
        let g x = do
              cellsItem <- itemUnchecked cells x
              cell <- castToHTMLTableCellElement cellsItem
              style <- getStyleUnchecked cell
              return (cell, style)
        mapM g [0 .. toEnum (lxsize-1)]
  lrc <- mapM getC lrow
  return $! V.fromListN (lxsize * lysize) $ concat lrc

-- | Output to the screen via the frontend.
display :: DebugModeCli
        -> FrontendSession  -- ^ frontend session data
        -> SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display DebugModeCli{scolorIsBold}
        FrontendSession{..}
        curFrame = flip runDOM undefined $ do
  let setChar :: Int -> Word32 -> Word32 -> DOM ()
      setChar i w wPrev = unless (w == wPrev) $ do
        let Color.AttrChar{acAttr=Color.Attr{..}, acChar} =
              Color.attrCharFromW32 $ Color.AttrCharW32 w
            (cell, style) = scharCells V.! i
        case acChar of
          ' ' -> setTextContent cell $ Just [chr 160]
          ch -> setTextContent cell $ Just [ch]
        setProp style "color" $ Color.colorToRGB fg
        when (scolorIsBold == Just True) $
          case fg of
            Color.White -> setProp style "font-weight" "normal"
            _ -> setProp style "font-weight" "bold"
        case bg of
          Color.Black -> do
            setProp style "border-color" "transparent"
            setProp style "background-color" $ Color.colorToRGB Color.Black
          Color.BrRed -> do  -- highlighted tile
            setProp style "border-color" $ Color.colorToRGB Color.Red
            setProp style "background-color" $ Color.colorToRGB Color.Black
          Color.BrBlue -> do  -- blue highlighted tile
            setProp style "border-color" $ Color.colorToRGB Color.Blue
            setProp style "background-color" $ Color.colorToRGB Color.Black
          Color.BrYellow -> do  -- yellow highlighted tile
            setProp style "border-color" $ Color.colorToRGB Color.BrYellow
            setProp style "background-color" $ Color.colorToRGB Color.Black
          _ -> do
            setProp style "border-color" "transparent"
            setProp style "background-color" $ Color.colorToRGB bg
  prevFrame <- readIORef previousFrame
  writeIORef previousFrame curFrame
  -- Sync, no point mutitasking threads in the single-threaded JS.
  callback <- newRequestAnimationFrameCallback $ \_ ->
    U.izipWithM_ setChar (PointArray.avector $ singleFrame curFrame)
                         (PointArray.avector $ singleFrame prevFrame)
  -- This ensures no frame redraws while callback executes.
  requestAnimationFrame_ scurrentWindow (Just callback)
