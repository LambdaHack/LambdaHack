{-# LANGUAGE CPP #-}
-- | Re-export the operations of the chosen raw frontend
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Frontend.Chosen
  ( Frontend(..), chosenStartup, stdStartup
  , frontendName
  ) where

import Game.LambdaHack.Common.Animation (DebugModeCli (..), SingleFrame (..))
import qualified Game.LambdaHack.Common.Key as K

#ifdef GTK
import qualified Game.LambdaHack.Frontend.Gtk as Chosen
#elif VTY
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
  , fdebugCli     :: DebugModeCli
  }

chosenStartup :: DebugModeCli -> (Frontend -> IO ()) -> IO ()
chosenStartup fdebugCli cont =
  Chosen.startup fdebugCli $ \fs ->
    cont $ Frontend
      { fdisplay = Chosen.fdisplay fs
      , fpromptGetKey = Chosen.fpromptGetKey fs
      , fdebugCli
      }

stdStartup :: DebugModeCli -> (Frontend -> IO ()) -> IO ()
stdStartup fdebugCli cont =
  Std.startup fdebugCli $ \fs ->
    cont $ Frontend
      { fdisplay = Std.fdisplay fs
      , fpromptGetKey = Std.fpromptGetKey fs
      , fdebugCli
      }
