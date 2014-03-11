{-# LANGUAGE CPP #-}
-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Frontend.Chosen
  ( Frontend(..), chosenStartup, stdStartup, noStartup
  , frontendName
  ) where

import Control.Concurrent
import Game.LambdaHack.Common.Animation (DebugModeCli (..), SingleFrame (..))
import qualified Game.LambdaHack.Common.Key as K

#ifdef VTY
import qualified Game.LambdaHack.Frontend.Vty as Chosen
#elif CURSES
import qualified Game.LambdaHack.Frontend.Curses as Chosen
#else
import qualified Game.LambdaHack.Frontend.Gtk as Chosen
#endif

import qualified Game.LambdaHack.Frontend.Std as Std

-- | The name of the chosen frontend.
frontendName :: String
frontendName = Chosen.frontendName

data Frontend = Frontend
  { fdisplay      :: Bool -> Maybe SingleFrame -> IO ()
  , fpromptGetKey :: SingleFrame -> IO K.KM
  , fsyncFrames   :: IO ()
  , fescMVar      :: !(Maybe (MVar ()))
  , fdebugCli     :: !DebugModeCli
  }

chosenStartup :: DebugModeCli -> (Frontend -> IO ()) -> IO ()
chosenStartup fdebugCli cont =
  Chosen.startup fdebugCli $ \fs ->
    cont $ Frontend
      { fdisplay = Chosen.fdisplay fs
      , fpromptGetKey = Chosen.fpromptGetKey fs
      , fsyncFrames = Chosen.fsyncFrames fs
      , fescMVar = Chosen.sescMVar fs
      , fdebugCli
      }

stdStartup :: DebugModeCli -> (Frontend -> IO ()) -> IO ()
stdStartup fdebugCli cont =
  Std.startup fdebugCli $ \fs ->
    cont $ Frontend
      { fdisplay = Std.fdisplay fs
      , fpromptGetKey = Std.fpromptGetKey fs
      , fsyncFrames = Std.fsyncFrames fs
      , fescMVar = Std.sescMVar fs
      , fdebugCli
      }

noStartup :: DebugModeCli -> (Frontend -> IO ()) -> IO ()
noStartup fdebugCli cont =
    cont $ Frontend
      { fdisplay = \_ _ -> return ()
      , fpromptGetKey = \_ -> return K.escKey
      , fsyncFrames = return ()
      , fescMVar = Nothing
      , fdebugCli
      }
