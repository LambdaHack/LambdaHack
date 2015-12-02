{-# LANGUAGE CPP #-}
-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.Frontend.Chosen
  ( chosenStartup, stdStartup, nullStartup
  , frontendName
  ) where

import Data.IORef
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
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

chosenStartup :: DebugModeCli -> (RawFrontend -> IO ()) -> IO ()
chosenStartup = Chosen.startup

stdStartup :: DebugModeCli -> (RawFrontend -> IO ()) -> IO ()
stdStartup = Std.startup

nullStartup :: DebugModeCli -> (RawFrontend -> IO ()) -> IO ()
nullStartup sdebugCli cont = do
  fautoYesRef <- newIORef $ not $ sdisableAutoYes sdebugCli
  fescPressed <- newIORef False
  cont RawFrontend
    { fdisplay = \_ -> return ()
    , fpromptGetKey = \_ -> return K.escKM
    , fsyncFrames = return ()
    , fescPressed
    , fautoYesRef
    }
