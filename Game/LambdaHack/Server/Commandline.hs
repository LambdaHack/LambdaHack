-- | Parsing of commandline arguments.
module Game.LambdaHack.Server.Commandline
  ( debugArgs
  ) where

import qualified Data.Text as T
import System.Environment (getArgs)

import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Server.State

-- TODO: make more maintainable

debugArgs :: IO DebugModeSer
debugArgs = do
  args <- getArgs
  let usage =
        [ "Configure debug options here, gameplay options in config.rules.ini."
        , "  --knowMap  reveal map for all clients in the next game"
        , "  --knowEvents  show all events in the next game (needs --knowMap)"
        , "  --sniffIn  display all incoming commands on console "
        , "  --sniffOut  display all outgoing commands on console "
        , "  --allClear  let all map tiles be translucent"
        , "  --gameMode m  start next game in the given mode"
        , "  --automateAll  give control of all UI teams to computer"
        , "  --newGame  start a new game, overwriting the save file"
        , "  --difficulty n  set difficulty for all UI players to n"
        , "  --stopAfter n  exit this game session after around n seconds"
        , "  --benchmark  print stats, limit saving and other file operations"
        , "  --setDungeonRng s  set dungeon generation RNG seed to string s"
        , "  --setMainRng s  set the main game RNG seed to string s"
        , "  --dumpInitRngs  dump RNG states from the start of the game"
        , "  --dbgMsgSer  let the server emit its internal debug messages"
        , "  --font fn  use the given font for the main game window"
        , "  --maxFps n  display at most n frames per second"
        , "  --noDelay  don't maintain any requested delays between frames"
        , "  --noMore  auto-answer all prompts"
        , "  --noAnim  don't show any animations"
        , "  --savePrefix  prepend the text to all savefile names"
        , "  --frontendStd  use the simple stdout/stdin frontend"
        , "  --frontendNull  use no frontend at all (for AIvsAI benchmarks)"
        , "  --dbgMsgCli  let clients emit their internal debug messages"
        , "  --fovMode m  set a Field of View mode, where m can be"
        , "    Digital"
        , "    Permissive"
        , "    Shadow"
        , "    Blind"
        ]
      parseArgs [] = defDebugModeSer
      parseArgs ("--knowMap" : rest) =
        (parseArgs rest) {sknowMap = True}
      parseArgs ("--knowEvents" : rest) =
        (parseArgs rest) {sknowEvents = True}
      parseArgs ("--sniffIn" : rest) =
        (parseArgs rest) {sniffIn = True}
      parseArgs ("--sniffOut" : rest) =
        (parseArgs rest) {sniffOut = True}
      parseArgs ("--allClear" : rest) =
        (parseArgs rest) {sallClear = True}
      parseArgs ("--gameMode" : s : rest) =
        (parseArgs rest) {sgameMode = T.pack s}
      parseArgs ("--automateAll" : rest) =
        (parseArgs rest) {sautomateAll = True}
      parseArgs ("--newGame" : rest) =
        let debugSer = parseArgs rest
        in debugSer { snewGameSer = True
                    , sdebugCli = (sdebugCli debugSer) {snewGameCli = True}}
      parseArgs ("--difficulty" : s : rest) =
        let debugSer = parseArgs rest
            diff = read s
        in debugSer { sdifficultySer = diff
                    , sdebugCli = (sdebugCli debugSer) {sdifficultyCli = diff}}
      parseArgs ("--stopAfter" : s : rest) =
        (parseArgs rest) {sstopAfter = Just $ read s}
      parseArgs ("--benchmark" : rest) =
        (parseArgs rest) {sbenchmark = True}
      parseArgs ("--setDungeonRng" : s : rest) =
        (parseArgs rest) {sdungeonRng = Just $ read s}
      parseArgs ("--setMainRng" : s : rest) =
        (parseArgs rest) {smainRng = Just $ read s}
      parseArgs ("--dumpInitRngs" : rest) =
        (parseArgs rest) {sdumpInitRngs = True}
      parseArgs ("--fovMode" : mode : rest) =
        (parseArgs rest) {sfovMode = Just $ read mode}
      parseArgs ("--dbgMsgSer" : rest) =
        (parseArgs rest) {sdbgMsgSer = True}
      parseArgs ("--font" : s : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {sfont = Just s}}
      parseArgs ("--maxFps" : n : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli =
                       (sdebugCli debugSer) {smaxFps = Just $ read n}}
      parseArgs ("--noDelay" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {snoDelay = True}}
      parseArgs ("--noMore" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {snoMore = True}}
      parseArgs ("--noAnim" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {snoAnim = Just True}}
      parseArgs ("--savePrefix" : s : rest) =
        let debugSer = parseArgs rest
        in debugSer { ssavePrefixSer = Just s
                    , sdebugCli =
                        (sdebugCli debugSer) {ssavePrefixCli = Just s}}
      parseArgs ("--frontendStd" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {sfrontendStd = True}}
      parseArgs ("--frontendNull" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {sfrontendNull = True}}
      parseArgs ("--dbgMsgCli" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {sdbgMsgCli = True}}
      parseArgs (wrong : _rest) =
        error $ "Unrecognized: " ++ wrong ++ "\n" ++ unlines usage
  return $! parseArgs args
