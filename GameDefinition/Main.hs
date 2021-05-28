-- | The main source code file of LambdaHack the game.
-- Module "TieKnot" is separated to make it usable in tests.
module Main
  ( main
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent.Async
import qualified Control.Exception as Ex
import qualified Options.Applicative as OA
import           System.Exit
import qualified System.IO as SIO

#ifndef USE_JSFILE
import qualified GHC.IO.Handle
import           System.FilePath

import Game.LambdaHack.Common.File (tryCreateDir)
import Game.LambdaHack.Common.Misc
#endif

import Game.LambdaHack.Server (serverOptionsPI)

import TieKnot

-- | Parse commandline options, tie the engine, content and clients knot,
-- run the game and handle exit.
main :: IO ()
main = do
  -- This test is faulty with JS, because it reports the browser console
  -- is not a terminal, but then we can't open files to contain the logs.
  -- Also it bloats the outcome JS file, so disabled.
#ifndef USE_JSFILE
  -- Special case hack, when the game is started not on a console.
  -- Without this, any attempt to output on stdout crashes a Windows exe
  -- (at least on Windows Vista) launched from the desktop or start menu.
  -- This is very crude and results in the inability to, e.g., process
  -- the output of @--help@ through a unix pipe. However, this should be
  -- effective on all Windows version, without the need to test all.
  isTerminal <- SIO.hIsTerminalDevice SIO.stdout
  unless isTerminal $ do
    dataDir <- appDataDir
    tryCreateDir dataDir
    fstdout <- SIO.openFile (dataDir </> "stdout.txt") SIO.WriteMode
    fstderr <- SIO.openFile (dataDir </> "stderr.txt") SIO.WriteMode
    GHC.IO.Handle.hDuplicateTo fstdout SIO.stdout
    GHC.IO.Handle.hDuplicateTo fstderr SIO.stderr
#else
  -- Work around display of one character per line.
  SIO.hSetBuffering SIO.stderr SIO.LineBuffering
#endif
  -- Fail here, not inside server code, so that savefiles are not removed,
  -- because they are not the source of the failure.
  !serverOptions <- OA.execParser serverOptionsPI
  resOrEx :: Either Ex.SomeException () <- Ex.try $ tieKnot serverOptions
  let unwrapEx e = case Ex.fromException e of
        Just (ExceptionInLinkedThread _ ex) -> unwrapEx ex
        _ -> e
  case resOrEx of
    Right () -> return ()
    Left e -> case Ex.fromException $ unwrapEx e of
      Just ExitSuccess ->
        exitSuccess  -- we are in the main thread, so here it really exits
      _ -> Ex.throwIO $ unwrapEx e
