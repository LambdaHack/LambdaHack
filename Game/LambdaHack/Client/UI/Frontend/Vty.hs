-- | Text frontend based on Vty.
module Game.LambdaHack.Client.UI.Frontend.Vty
  ( -- * Session data type for the frontend
    FrontendSession(sescMVar)
    -- * The output and input operations
  , fdisplay, fpromptGetKey, fsyncFrames
    -- * Frontend administration tools
  , frontendName, startup
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as Ex hiding (handle)
import Data.Default
import Graphics.Vty
import qualified Graphics.Vty as Vty
import qualified Control.Concurrent.STM as STM
import Control.Monad
import Data.Maybe

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Msg

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { svty      :: !Vty  -- internal vty session
  , schanKey  :: !(STM.TQueue K.KM)  -- ^ channel for keyboard input
  , sescMVar  :: !(Maybe (MVar ()))
  , sdebugCli :: !DebugModeCli  -- ^ client configuration
      -- ^ Configuration of the frontend session.
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "vty"

-- | Starts the main program loop using the frontend input and output.
startup :: DebugModeCli -> (FrontendSession -> IO ()) -> IO ()
startup sdebugCli k = do
  svty <- mkVty def
  schanKey <- STM.atomically $ STM.newTQueue
  escMVar <- newEmptyMVar
  let sess = FrontendSession{sescMVar = Just escMVar, ..}
  void $ async $ storeKeys sess
  a <- async $ k sess `Ex.finally` Vty.shutdown svty
  wait a

storeKeys :: FrontendSession -> IO ()
storeKeys sess@FrontendSession{..} = do
  e <- nextEvent svty  -- blocks here, so no polling
  case e of
    EvKey n mods -> do
      let !key = keyTranslate n
          !modifier = modifierTranslate mods
          readAll = do
            res <- STM.atomically $ STM.tryReadTQueue schanKey
            when (isJust res) $ readAll
      -- If ESC, also mark it specially and reset the key channel.
      case sescMVar of
        Just escMVar -> do
          when (key == K.Esc) $ do
            void $ tryPutMVar escMVar ()
            readAll
        Nothing -> return ()
      -- Store the key in the channel.
      STM.atomically $ STM.writeTQueue schanKey K.KM{key, modifier}
    _ -> return ()
  storeKeys sess

-- | Output to the screen via the frontend.
fdisplay :: FrontendSession    -- ^ frontend session data
         -> Bool
         -> Maybe SingleFrame  -- ^ the screen frame to draw
         -> IO ()
fdisplay _ _ Nothing = return ()
fdisplay FrontendSession{svty} _ (Just rawSF) =
  let SingleFrame{sfLevel} = overlayOverlay rawSF
      img = (foldr (<->) emptyImage
             . map (foldr (<|>) emptyImage
                      . map (\ Color.AttrChar{..} ->
                                char (setAttr acAttr) acChar)))
            $ map decodeLine sfLevel
      pic = picForImage img
  in update svty pic

-- | Input key via the frontend.
nextKeyEvent :: FrontendSession -> IO K.KM
nextKeyEvent FrontendSession{..} = do
  km <- STM.atomically $ STM.readTQueue schanKey
  case km of
    K.KM{key=K.Space} ->
      -- Drop frames up to the first empty frame.
      -- Keep the last non-empty frame, if any.
      -- Pressing SPACE repeatedly can be used to step
      -- through intermediate stages of an animation,
      -- whereas any other key skips the whole animation outright.
--      onQueue dropStartLQueue sess
      return ()
    _ ->
      -- Show the last non-empty frame and empty the queue.
--      trimFrameState sess
      return ()
  return km

fsyncFrames :: FrontendSession -> IO ()
fsyncFrames _ = return ()

-- | Display a prompt, wait for any key.
fpromptGetKey :: FrontendSession -> SingleFrame -> IO K.KM
fpromptGetKey sess frame = do
  fdisplay sess True $ Just frame
  nextKeyEvent sess

-- TODO: Ctrl-Home and Ctrl-End are the same as Home and End on some terminals
-- so we should probably go back to using 0-9 (and Shift) for movement
-- but let's wait until vty 5.0 is out and see if it helps.
keyTranslate :: Key -> K.Key
keyTranslate n =
  case n of
    KEsc          -> K.Esc
    KEnter        -> K.Return
    (KChar ' ')   -> K.Space
    (KChar '\t')  -> K.Tab
    KBackTab      -> K.BackTab
    KBS           -> K.BackSpace
    KUp           -> K.Up
    KDown         -> K.Down
    KLeft         -> K.Left
    KRight        -> K.Right
    KHome         -> K.Home
    KEnd          -> K.End
    KPageUp       -> K.PgUp
    KPageDown     -> K.PgDn
    KBegin        -> K.Begin
    KCenter       -> K.Begin
    KIns          -> K.Insert
    (KChar c)     -> K.Char c
    _             -> K.Unknown (tshow n)

-- | Translates modifiers to our own encoding.
modifierTranslate :: [Modifier] -> K.Modifier
modifierTranslate mods =
  if MCtrl `elem` mods then K.Control else K.NoModifier

-- TODO: with vty 5.0 check if bold is still needed.
-- A hack to get bright colors via the bold attribute. Depending on terminal
-- settings this is needed or not and the characters really get bold or not.
-- HSCurses does this by default, but in Vty you have to request the hack.
hack :: Color.Color -> Attr -> Attr
hack c a = if Color.isBright c then withStyle a bold else a

setAttr :: Color.Attr -> Attr
setAttr Color.Attr{fg, bg} =
-- This optimization breaks display for white background terminals:
--  if (fg, bg) == Color.defAttr
--  then def_attr
--  else
  hack fg $ hack bg $
    defAttr { attrForeColor = SetTo (aToc fg)
            , attrBackColor = SetTo (aToc bg) }

aToc :: Color.Color -> Color
aToc Color.Black     = black
aToc Color.Red       = red
aToc Color.Green     = green
aToc Color.Brown     = yellow
aToc Color.Blue      = blue
aToc Color.Magenta   = magenta
aToc Color.Cyan      = cyan
aToc Color.White     = white
aToc Color.BrBlack   = brightBlack
aToc Color.BrRed     = brightRed
aToc Color.BrGreen   = brightGreen
aToc Color.BrYellow  = brightYellow
aToc Color.BrBlue    = brightBlue
aToc Color.BrMagenta = brightMagenta
aToc Color.BrCyan    = brightCyan
aToc Color.BrWhite   = brightWhite
