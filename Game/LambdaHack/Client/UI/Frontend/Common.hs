-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Frontend.Common
  ( RawFrontend(..), startupAsync, startupBound, resetChanKey, modifierTranslate
  ) where

import Prelude ()
import Prelude.Compat

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Monad (when)
import Data.IORef
import Data.Maybe

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation

data RawFrontend = RawFrontend
  { fdisplay      :: SingleFrame -> IO ()
  , fpromptGetKey :: SingleFrame -> IO K.KM
  , fshutdown     :: IO ()
  , fescPressed   :: !(IORef Bool)
  }

startupAsync :: (MVar RawFrontend -> IO ()) -> IO RawFrontend
startupAsync k = do
  rfMVar <- newEmptyMVar
  a <- async $ k rfMVar
  link a
  takeMVar rfMVar

startupBound :: (MVar RawFrontend -> IO ()) -> IO RawFrontend
startupBound k = do
  rfMVar <- newEmptyMVar
  a <- asyncBound $ k rfMVar
  link a
  takeMVar rfMVar

-- | Empty the keyboard channel.
resetChanKey :: STM.TQueue K.KM -> IO ()
resetChanKey schanKey = do
  res <- STM.atomically $ STM.tryReadTQueue schanKey
  when (isJust res) $ resetChanKey schanKey

-- | Translates modifiers to our own encoding.
modifierTranslate :: Bool -> Bool -> Bool -> Bool -> K.Modifier
modifierTranslate modCtrl modShift modAlt modMeta
  | modCtrl = K.Control
  | modAlt || modMeta = K.Alt
  | modShift = K.Shift
  | otherwise = K.NoModifier
