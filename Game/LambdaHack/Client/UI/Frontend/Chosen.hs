{-# LANGUAGE CPP #-}
-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.Frontend.Chosen
  ( startupF, frontendName
  ) where

import Control.Concurrent
import Control.Concurrent.Async
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

nullStartup :: DebugModeCli -> MVar RawFrontend -> IO ()
nullStartup sdebugCli rfMVar = do
  fautoYesRef <- newIORef $ not $ sdisableAutoYes sdebugCli
  fescPressed <- newIORef False
  putMVar rfMVar RawFrontend
    { fdisplay = \_ -> return ()
    , fpromptGetKey = \_ -> return K.escKM
    , fshutdown = return ()
    , fescPressed
    , fautoYesRef
    }

-- | Initialize the frontend and apply the given continuation to the results
-- of the initialization.
startupF :: DebugModeCli -> IO RawFrontend
startupF dbg = do
  let startup | sfrontendNull dbg = nullStartup
              | sfrontendStd dbg = Std.startup
              | otherwise = Chosen.startup
  rfMVar <- newEmptyMVar
  a <- async $ startup dbg rfMVar
  link a
  takeMVar rfMVar
