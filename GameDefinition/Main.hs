-- | The main source code file of LambdaHack the game.
-- Module "TieKnot" is separated to make it usable in tests.
module Main
  ( main
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.Concurrent.Async
import qualified Control.Exception as Ex
import qualified Options.Applicative as OA
import           System.Exit

#ifndef USE_JSFILE
import qualified GHC.IO.Handle
import           System.FilePath
import qualified System.IO as SIO

import Game.LambdaHack.Common.File (tryCreateDir)
import Game.LambdaHack.Common.Misc
#endif

import Game.LambdaHack.Server (serverOptionsPI)

import Implementation.TieKnot

-- | Parse commandline options, tie the engine, content and clients knot,
-- run the game and handle exit.
main :: IO ()
main = do
#ifndef USE_JSFILE
  -- For the case when the game is started not on a console.
  -- This is broken with JS and also bloats the outcome file.
  isTerminal <- SIO.hIsTerminalDevice SIO.stdout
  unless isTerminal $ do
    dataDir <- appDataDir
    tryCreateDir dataDir
    fstdout <- SIO.openFile (dataDir </> "stdout.txt") SIO.WriteMode
    fstderr <- SIO.openFile (dataDir </> "stderr.txt") SIO.WriteMode
    GHC.IO.Handle.hDuplicateTo fstdout SIO.stdout
    GHC.IO.Handle.hDuplicateTo fstderr SIO.stderr
#endif
  -- Fail here, not inside server code, so that savefiles are not removed,
  -- because they are not the source of the failure.
  !serverOptions <- OA.execParser serverOptionsPI
  -- Avoid the bound thread that would slow down the communication.
  a <- async $ tieKnot serverOptions
  resOrEx <- waitCatch a
  let unwrapEx e = case Ex.fromException e of
        Just (ExceptionInLinkedThread _ ex) -> unwrapEx ex
        _ -> e
  case resOrEx of
    Right () -> return ()
    Left e -> case Ex.fromException $ unwrapEx e of
      Just ExitSuccess ->
        exitSuccess  -- we are in the main thread, so here it really exits
      _ -> Ex.throwIO e
