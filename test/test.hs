import TieKnot

main :: IO ()
main =
  tieKnot "--dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 100 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 42 --setMainRng 42"
  -- tieKnot "--dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 100 --automateAll --keepAutomated --gameMode battle --setDungeonRng 42 --setMainRng 42"
