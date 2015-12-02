{-# LANGUAGE CPP #-}
-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.Frontend.Chosen
  ( startupF, frontendName
  ) where

import Control.Monad
import Data.IORef
import qualified Data.Text.IO as T
import System.IO

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

-- | Initialize the frontend and apply the given continuation to the results
-- of the initialization.
startupF :: DebugModeCli  -- ^ debug settings
         -> (RawFrontend -> IO ())  -- ^ continuation
         -> IO ()
startupF dbg cont = do
  let startup | sfrontendNull dbg = nullStartup
              | sfrontendStd dbg = Std.startup
              | otherwise = Chosen.startup
  startup dbg $ \fs -> do
    cont fs
    let debugPrint t = when (sdbgMsgCli dbg) $ do
          T.hPutStrLn stderr t
          hFlush stderr
    debugPrint "Frontend shuts down"
