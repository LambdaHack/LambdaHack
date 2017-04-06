-- | Text frontend running in Browser.
module Game.LambdaHack.Client.UI.Frontend.Dom
  ( startup, frontendName
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent
import qualified Control.Monad.IO.Class as IO
import Control.Monad.Trans.Reader (ask)
import qualified Data.Char as Char
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)

import GHCJS.DOM (currentDocument, currentWindow)
import GHCJS.DOM.CSSStyleDeclaration (setProperty)
import GHCJS.DOM.Document (createElement, getBodyUnchecked)
import GHCJS.DOM.Element (Element (Element), getStyle, setInnerHTML)
import GHCJS.DOM.EventM (EventM, mouseAltKey, mouseButton, mouseCtrlKey,
                         mouseMetaKey, mouseShiftKey, on, on, preventDefault,
                         stopPropagation)
import GHCJS.DOM.GlobalEventHandlers (contextMenu, keyDown, mouseUp, wheel)
import GHCJS.DOM.HTMLCollection (itemUnsafe)
import GHCJS.DOM.HTMLTableElement (HTMLTableElement (HTMLTableElement), getRows,
                                   setCellPadding, setCellSpacing)
import GHCJS.DOM.HTMLTableRowElement (HTMLTableRowElement (HTMLTableRowElement),
                                      getCells)
import GHCJS.DOM.KeyboardEvent (getAltGraphKey, getAltKey, getCtrlKey, getKey,
                                getMetaKey, getShiftKey)
import GHCJS.DOM.Node (appendChild_, setTextContent)
import GHCJS.DOM.RequestAnimationFrameCallback
import GHCJS.DOM.Types (CSSStyleDeclaration, DOM,
                        HTMLDivElement (HTMLDivElement),
                        HTMLTableCellElement (HTMLTableCellElement),
                        IsMouseEvent, Window, runDOM, unsafeCastTo)
import GHCJS.DOM.WheelEvent (getDeltaY)
import GHCJS.DOM.Window (requestAnimationFrame_)

import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Frontend.Common
import qualified Game.LambdaHack.Client.UI.Key as K
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { scurrentWindow :: !Window
  , scharCells     :: !(V.Vector (HTMLTableCellElement, CSSStyleDeclaration))
  , spreviousFrame :: !(IORef SingleFrame)
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
  pageStyle <- getStyle body
  setProp pageStyle "background-color" (Color.colorToRGB Color.Black)
  setProp pageStyle "color" (Color.colorToRGB Color.White)
  divBlockRaw <- createElement doc ("div" :: Text)
  divBlock <- unsafeCastTo HTMLDivElement divBlockRaw
  divStyle <- getStyle divBlock
  setProp divStyle "text-align" "center"
  case (saddress, stitle) of
    (Just address, Just title) -> do
      let headerText = "<h1><a href=\"" <> address <> "\">"
                       <> title <> "</a></h1>"
      setInnerHTML divBlock (Just headerText)
    _ -> return ()
  let lxsize = fst normalLevelBound + 1
      lysize = snd normalLevelBound + 4
      cell = "<td>" ++ [Char.chr 160]
      row = "<tr>" ++ concat (replicate lxsize cell)
      rows = concat (replicate lysize row)
  tableElemRaw <- createElement doc ("table" :: Text)
  tableElem <- unsafeCastTo HTMLTableElement tableElemRaw
  appendChild_ divBlock tableElem
  scharStyle <- getStyle tableElem
  -- Speed: http://www.w3.org/TR/CSS21/tables.html#fixed-table-layout
  setProp scharStyle "table-layout" "fixed"
  setProp scharStyle "font-family" "lambdaHackFont"
  setProp scharStyle "font-size" $ tshow (fromJust sfontSize) <> "px"
  setProp scharStyle "font-weight" "bold"
  -- Get rid of table spacing. Tons of spurious hacks just in case.
  setCellPadding tableElem (Just ("0" :: Text))
  setCellSpacing tableElem (Just ("0" :: Text))
  setProp scharStyle "outline" "1px solid grey"
  setProp scharStyle "padding" "0 0 0 0"
  setProp scharStyle "border-collapse" "collapse"
  setProp scharStyle "margin-left" "auto"
  setProp scharStyle "margin-right" "auto"
  -- Create the session record.
  setInnerHTML tableElem $ Just rows
  scharCells <- flattenTable tableElem
  spreviousFrame <- newIORef blankSingleFrame
  let sess = FrontendSession{..}
  rf <- IO.liftIO $ createRawFrontend (display sdebugCli sess) shutdown
  let readMod = do
        modCtrl <- ask >>= getCtrlKey
        modShift <- ask >>= getShiftKey
        modAlt <- ask >>= getAltKey
        modMeta <- ask >>= getMetaKey
        modAltG <- ask >>= getAltGraphKey
        return $! modifierTranslate
                    modCtrl modShift (modAlt || modAltG) modMeta
  void $ doc `on` keyDown $ do
    keyId <- ask >>= getKey
    modifier <- readMod
--  This is currently broken at least for Shift-F1, etc., so won't be used:
--    keyLoc <- ask >>= getKeyLocation
--    let onKeyPad = case keyLoc of
--          3 {-KEY_LOCATION_NUMPAD-} -> True
--          _ -> False
    let key = K.keyTranslateWeb keyId (modifier == K.Shift)
        modifierNoShift =  -- to prevent S-!, etc.
          if modifier == K.Shift then K.NoModifier else modifier
    -- IO.liftIO $ do
    --   putStrLn $ "keyId: " ++ keyId
    --   putStrLn $ "key: " ++ K.showKey key
    --   putStrLn $ "modifier: " ++ show modifier
    when (key == K.Esc) $ IO.liftIO $ resetChanKey (fchanKey rf)
    IO.liftIO $ saveKMP rf modifierNoShift key originPoint
    -- Pass through C-+ and others, disable special behaviour on Tab.
    when (modifier `elem` [K.NoModifier, K.Shift, K.Control]) $ do
      preventDefault
      stopPropagation
  -- Handle mouseclicks, per-cell.
  let setupMouse i a = let Point x y = PointArray.punindex lxsize i
                       in handleMouse rf a x y
  V.imapM_ setupMouse scharCells
  let setBorder (_, style) = setProp style "border" "1px solid transparent"
  V.mapM_ setBorder scharCells
  -- Display at the end to avoid redraw
  appendChild_ body divBlock
  IO.liftIO $ putMVar rfMVar rf
    -- send to client only after the whole webpage is set up
    -- because there is no @mainGUI@ to start accepting

shutdown :: IO ()
shutdown = return () -- nothing to clean up

setProp :: CSSStyleDeclaration -> Text -> Text -> DOM ()
setProp style propRef propValue =
  setProperty style propRef (Just propValue) (Nothing :: Maybe Text)

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
        let key = case but of
              0 -> K.LeftButtonRelease
              1 -> K.MiddleButtonRelease
              2 -> K.RightButtonRelease  -- not handled in contextMenu
              _ -> K.LeftButtonRelease  -- any other is alternate left
            pointer = Point cx cy
        -- IO.liftIO $ putStrLn $
        --   "m: " ++ show but ++ show modifier ++ show pointer
        IO.liftIO $ saveKMP rf modifier key pointer
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
flattenTable :: HTMLTableElement
             -> DOM (V.Vector (HTMLTableCellElement, CSSStyleDeclaration))
flattenTable table = do
  let lxsize = fst normalLevelBound + 1
      lysize = snd normalLevelBound + 4
  rows <- getRows table
  let f y = do
        rowsItem <- itemUnsafe rows y
        unsafeCastTo HTMLTableRowElement rowsItem
  lrow <- mapM f [0 .. toEnum (lysize-1)]
  let getC :: HTMLTableRowElement
           -> DOM [(HTMLTableCellElement, CSSStyleDeclaration)]
      getC row = do
        cells <- getCells row
        let g x = do
              cellsItem <- itemUnsafe cells x
              cell <- unsafeCastTo HTMLTableCellElement cellsItem
              style <- getStyle cell
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
        case Char.ord acChar of
          32 -> setTextContent cell $ Just [Char.chr 160]
          183 | fg <= Color.BrBlack && scolorIsBold == Just True ->
            setTextContent cell $ Just [Char.chr 8901]
          _  -> setTextContent cell $ Just [acChar]
        setProp style "color" $ Color.colorToRGB fg
        case bg of
          Color.HighlightNone ->
            setProp style "border-color" "transparent"
          Color.HighlightRed ->
            setProp style "border-color" $ Color.colorToRGB Color.Red
          Color.HighlightBlue ->
            setProp style "border-color" $ Color.colorToRGB Color.Blue
          Color.HighlightYellow ->
            setProp style "border-color" $ Color.colorToRGB Color.BrYellow
          Color.HighlightGrey ->
            setProp style "border-color" $ Color.colorToRGB Color.BrBlack
  prevFrame <- readIORef spreviousFrame
  writeIORef spreviousFrame curFrame
  -- Sync, no point mutitasking threads in the single-threaded JS.
  callback <- newRequestAnimationFrameCallback $ \_ ->
    U.izipWithM_ setChar (PointArray.avector $ singleFrame curFrame)
                         (PointArray.avector $ singleFrame prevFrame)
  -- This ensures no frame redraws while callback executes.
  requestAnimationFrame_ scurrentWindow callback
