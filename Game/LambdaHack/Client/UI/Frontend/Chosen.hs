{-# LANGUAGE CPP #-}
-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.Frontend.Chosen
  ( RawFrontend(..), chosenStartup, stdStartup, nullStartup
  , frontendName
  ) where

import Control.Concurrent
import Data.IORef
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation (SingleFrame (..))
import Game.LambdaHack.Common.ClientOptions

#ifdef USE_CURSES
import qualified Game.LambdaHack.Client.UI.Frontend.Curses as Chosen
#elif USE_VTY
import qualified Game.LambdaHack.Client.UI.Frontend.Vty as Chosen
#elif (USE_BROWSER || USE_WEBKIT)
import qualified Game.LambdaHack.Client.UI.Frontend.Dom as Chosen
#else
import qualified Game.LambdaHack.Client.UI.Frontend.Gtk as Chosen
#endif

import qualified Game.LambdaHack.Client.UI.Frontend.Std as Std

-- | The name of the chosen frontend.
frontendName :: String
frontendName = Chosen.frontendName

data RawFrontend = RawFrontend
  { fdisplay      :: Maybe SingleFrame -> IO ()
  , fpromptGetKey :: SingleFrame -> IO K.KM
  , fsyncFrames   :: IO ()
  , fescMVar      :: !(Maybe (MVar ()))
  , fautoYesRef   :: !(IORef Bool)
  }

chosenStartup :: DebugModeCli -> (RawFrontend -> IO ()) -> IO ()
chosenStartup fdebugCli cont =
  Chosen.startup fdebugCli $ \fs -> do
    fautoYesRef <- newIORef $ not $ sdisableAutoYes fdebugCli
    cont RawFrontend
      { fdisplay = Chosen.fdisplay fs
      , fpromptGetKey = Chosen.fpromptGetKey fs
      , fsyncFrames = Chosen.fsyncFrames fs
      , fescMVar = Chosen.sescMVar fs
      , fautoYesRef
      }

stdStartup :: DebugModeCli -> (RawFrontend -> IO ()) -> IO ()
stdStartup fdebugCli cont =
  Std.startup fdebugCli $ \fs -> do
    fautoYesRef <- newIORef $ not $ sdisableAutoYes fdebugCli
    cont RawFrontend
      { fdisplay = Std.fdisplay fs
      , fpromptGetKey = Std.fpromptGetKey fs
      , fsyncFrames = Std.fsyncFrames fs
      , fescMVar = Std.sescMVar fs
      , fautoYesRef
      }

nullStartup :: DebugModeCli -> (RawFrontend -> IO ()) -> IO ()
nullStartup fdebugCli cont =
  -- Std used to fork (async) the server thread, to avoid bound thread overhead.
  Std.startup fdebugCli $ \_ -> do
    fautoYesRef <- newIORef $ not $ sdisableAutoYes fdebugCli
    cont RawFrontend
      { fdisplay = \_ -> return ()
      , fpromptGetKey = \_ -> return K.escKM
      , fsyncFrames = return ()
      , fescMVar = Nothing
      , fautoYesRef
      }
