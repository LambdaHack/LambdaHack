import Prelude ()

import Options.Applicative

import Game.LambdaHack.Common.Prelude
import Game.LambdaHack.Server

import Implementation.TieKnot

main :: IO ()
main = do
  let args = words "--dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 100 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 42 --setMainRng 42"
  serverOptions <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
  tieKnot serverOptions
  -- tieKnot $ words "--dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 100 --automateAll --keepAutomated --gameMode battle --setDungeonRng 42 --setMainRng 42"
