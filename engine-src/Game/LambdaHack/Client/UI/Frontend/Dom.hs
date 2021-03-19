-- | Text frontend running in a browser.
module Game.LambdaHack.Client.UI.Frontend.Dom
  ( startup, frontendName
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import           Control.Monad.Trans.Reader (ask)
import           Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word32)

import GHCJS.DOM (currentDocument, currentWindow)
import GHCJS.DOM.CSSStyleDeclaration (setProperty)
import GHCJS.DOM.Document (createElement, getBodyUnchecked)
import GHCJS.DOM.Element (Element (Element), setInnerHTML)
import GHCJS.DOM.ElementCSSInlineStyle (getStyle)
import GHCJS.DOM.EventM
  ( EventM
  , mouseAltKey
  , mouseButton
  , mouseCtrlKey
  , mouseMetaKey
  , mouseShiftKey
  , on
  , preventDefault
  , stopPropagation
  )
import GHCJS.DOM.GlobalEventHandlers (contextMenu, keyDown, mouseUp, wheel)
import GHCJS.DOM.HTMLCollection (itemUnsafe)
import GHCJS.DOM.HTMLElement (focus)
import GHCJS.DOM.HTMLTableElement
  ( HTMLTableElement (HTMLTableElement)
  , getRows
  , setCellPadding
  , setCellSpacing
  )
import GHCJS.DOM.HTMLTableRowElement
  (HTMLTableRowElement (HTMLTableRowElement), getCells)
import GHCJS.DOM.KeyboardEvent
  (getAltGraphKey, getAltKey, getCtrlKey, getKey, getMetaKey, getShiftKey)
import GHCJS.DOM.Node (appendChild_, replaceChild_, setTextContent)
import GHCJS.DOM.NonElementParentNode (getElementByIdUnsafe)
import GHCJS.DOM.RequestAnimationFrameCallback
import GHCJS.DOM.Types
  ( CSSStyleDeclaration
  , DOM
  , HTMLDivElement (HTMLDivElement)
  , HTMLTableCellElement (HTMLTableCellElement)
  , IsMouseEvent
  , JSString
  , Window
  , runDOM
  , unsafeCastTo
  )
import GHCJS.DOM.WheelEvent (getDeltaY)
import GHCJS.DOM.Window (requestAnimationFrame_)

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Content.TileKind (floorSymbol)
import qualified Game.LambdaHack.Definition.Color as Color

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { scurrentWindow :: Window
  , scharCells     :: V.Vector (HTMLTableCellElement, CSSStyleDeclaration)
  , spreviousFrame :: IORef SingleFrame
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "browser"

-- | Starts the main program loop using the frontend input and output.
startup :: ScreenContent -> ClientOptions -> IO RawFrontend
startup coscreen soptions = do
  rfMVar <- newEmptyMVar
  flip runDOM undefined $ runWeb coscreen soptions rfMVar
  takeMVar rfMVar

runWeb :: ScreenContent -> ClientOptions -> MVar RawFrontend -> DOM ()
runWeb coscreen ClientOptions{..} rfMVar = do
  -- Init the document.
  Just doc <- currentDocument
  Just scurrentWindow <- currentWindow
  body <- getBodyUnchecked doc
  pageStyle <- getStyle body
  setProp pageStyle "background-color" (Color.colorToRGB Color.Black)
  setProp pageStyle "color" (Color.colorToRGB Color.AltWhite)
  -- Create the session record.
  divBlockRaw <- createElement doc ("div" :: Text)
  divBlock <- unsafeCastTo HTMLDivElement divBlockRaw
  let cell = "<td>\x00a0"
      row = "<tr>" ++ concat (replicate (rwidth coscreen) cell)
      rows = concat (replicate (rheight coscreen) row)
  tableElemRaw <- createElement doc ("table" :: Text)
  tableElem <- unsafeCastTo HTMLTableElement tableElemRaw
  -- Get rid of table spacing. Spurious hacks just in case.
  setCellPadding tableElem ("0" :: Text)
  setCellSpacing tableElem ("0" :: Text)
  appendChild_ divBlock tableElem
  setInnerHTML tableElem rows
  scharCells <- flattenTable coscreen tableElem
  spreviousFrame <- newIORef $ blankSingleFrame coscreen
  let sess = FrontendSession{..}
  rf <- IO.liftIO $ createRawFrontend coscreen (display sess) shutdown
  let readMod = do
        modCtrl <- ask >>= getCtrlKey
        modShift <- ask >>= getShiftKey
        modAlt <- ask >>= getAltKey
        modMeta <- ask >>= getMetaKey
        modAltG <- ask >>= getAltGraphKey
        return $! modifierTranslate modCtrl modShift (modAlt || modAltG) modMeta
  gameMap <- getElementByIdUnsafe doc ("gameMap" :: Text)
  divMap <- unsafeCastTo HTMLDivElement gameMap
  focus divMap
  void $ divMap `on` keyDown $ do
    keyId <- ask >>= getKey
    modifier <- readMod
--  This is currently broken at least for Shift-F1, etc., so won't be used:
--    keyLoc <- ask >>= getKeyLocation
--    let onKeyPad = case keyLoc of
--          3 {-KEY_LOCATION_NUMPAD-} -> True
--          _ -> False
    let key = K.keyTranslateWeb keyId (modifier == K.Shift)
        modifierNoShift = case modifier of  -- to prevent S-!, etc.
          K.Shift -> K.NoModifier
          K.ControlShift -> K.Control
          _ -> modifier
    -- IO.liftIO $ do
    --   putStrLn $ "keyId: " ++ keyId
    --   putStrLn $ "key: " ++ K.showKey key
    --   putStrLn $ "modifier: " ++ show modifier
    when (key == K.Esc) $ IO.liftIO $ resetChanKey (fchanKey rf)
    IO.liftIO $ saveKMP rf modifierNoShift key (PointUI 0 0)
    -- Pass through C-+ and others, but disable special behaviour on Tab, etc.
    let browserKeys = "+-0tTnNdxcv"
    unless (modifier == K.Alt
            || modifier == K.Control && key `elem` map K.Char browserKeys
            || key == K.DeadKey) $ do  -- NumLock in particular
      preventDefault
      stopPropagation
  -- Handle mouseclicks, per-cell.
  let setupMouse i a =
        let Point{..} = punindex (rwidth coscreen) i
              -- abuse of convention in that @Point@ used for screen, not map
            pUI = squareToUI $ PointSquare px py
        in handleMouse rf a pUI
  V.imapM_ setupMouse scharCells
  -- Display at the end to avoid redraw. Replace "Please wait".
  pleaseWait <- getElementByIdUnsafe doc ("pleaseWait" :: Text)
  replaceChild_ gameMap divBlock pleaseWait
  IO.liftIO $ putMVar rfMVar rf
    -- send to client only after the whole webpage is set up
    -- because there is no @mainGUI@ to start accepting

shutdown :: IO ()
shutdown = return () -- nothing to clean up

setProp :: CSSStyleDeclaration -> JSString -> Text -> DOM ()
setProp style propRef propValue =
  setProperty style propRef propValue (Nothing :: Maybe JSString)

-- | Let each table cell handle mouse events inside.
handleMouse :: RawFrontend
            -> (HTMLTableCellElement, CSSStyleDeclaration) -> PointUI
            -> DOM ()
handleMouse rf (cell, _) pUI = do
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
        maybe (return ())
              (\key -> IO.liftIO $ saveKMP rf modifier key pUI) mkey
      saveMouse = do
        -- <https://hackage.haskell.org/package/ghcjs-dom-0.2.1.0/docs/GHCJS-DOM-EventM.html>
        but <- mouseButton
        modifier <- readMod
        let key = case but of
              0 -> K.LeftButtonRelease
              1 -> K.MiddleButtonRelease
              2 -> K.RightButtonRelease  -- not handled in contextMenu
              _ -> K.LeftButtonRelease  -- any other is alternate left
        -- IO.liftIO $ putStrLn $
        --   "m: " ++ show but ++ show modifier ++ show pUI
        IO.liftIO $ saveKMP rf modifier key pUI
  void $ cell `on` wheel $ do
    saveWheel
    preventDefault
    stopPropagation
  void $ cell `on` contextMenu $ do
    preventDefault
    stopPropagation
  void $ cell `on` mouseUp $ do
    saveMouse
    preventDefault
    stopPropagation

-- | Get the list of all cells of an HTML table.
flattenTable :: ScreenContent
             -> HTMLTableElement
             -> DOM (V.Vector (HTMLTableCellElement, CSSStyleDeclaration))
flattenTable coscreen table = do
  rows <- getRows table
  let f y = do
        rowsItem <- itemUnsafe rows y
        unsafeCastTo HTMLTableRowElement rowsItem
  lrow <- mapM f [0 .. toEnum (rheight coscreen - 1)]
  let getC :: HTMLTableRowElement
           -> DOM [(HTMLTableCellElement, CSSStyleDeclaration)]
      getC row = do
        cells <- getCells row
        let g x = do
              cellsItem <- itemUnsafe cells x
              cell <- unsafeCastTo HTMLTableCellElement cellsItem
              style <- getStyle cell
              return (cell, style)
        mapM g [0 .. toEnum (rwidth coscreen - 1)]
  lrc <- mapM getC lrow
  return $! V.fromListN (rwidth coscreen * rheight coscreen) $ concat lrc

-- | Output to the screen via the frontend.
display :: FrontendSession  -- ^ frontend session data
        -> SingleFrame  -- ^ the screen frame to draw
        -> IO ()
display FrontendSession{..} !curFrame = flip runDOM undefined $ do
  let setChar :: Int -> (Word32, Word32) -> DOM Int
      setChar !i (!w, !wPrev) | w == wPrev = return $! i + 1
      setChar i (w, wPrev) = do
        let Point{..} = toEnum i
            Color.AttrChar{acAttr=Color.Attr{fg=fgRaw,bg}, acChar} =
              Color.attrCharFromW32 $ Color.AttrCharW32 w
            fg | py `mod` 2 == 0 && fgRaw == Color.White = Color.AltWhite
               | otherwise = fgRaw
            (!cell, !style) = scharCells V.! i
        if | acChar == ' ' -> setTextContent cell $ Just ("\x00a0" :: JSString)
           | acChar == floorSymbol && not (Color.isBright fg) ->
             setTextContent cell $ Just ("\x22C5" :: JSString)
           | otherwise -> setTextContent cell $ Just [acChar]
        setProp style "color" $ Color.colorToRGB fg
        let bgPrev = Color.bgFromW32 $ Color.AttrCharW32 wPrev
        when (bg /= bgPrev) $
          setProp style "border-color"
                        (Color.colorToRGB $ Color.highlightToColor bg)
        return $! i + 1
  !prevFrame <- readIORef spreviousFrame
  writeIORef spreviousFrame curFrame
  -- This continues asynchronously, if can't otherwise.
  callback <- newRequestAnimationFrameCallbackSync $ \_ ->
    U.foldM'_ setChar 0 $ U.zip (PointArray.avector $ singleArray curFrame)
                                (PointArray.avector $ singleArray prevFrame)
  -- This attempts to ensure no redraws while callback executes
  -- and a single redraw when it completes.
  requestAnimationFrame_ scurrentWindow callback
