import TieKnot

main :: IO ()
main = do
  tieKnot $ tail $ words "dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 6 --automateAll --gameMode campaign --difficulty 1 --setDungeonRng 42 --setMainRng 42"
  -- tieKnot $ tail $ words "dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 6 --automateAll --gameMode battle --difficulty 1 --setDungeonRng 42 --setMainRng 42"
