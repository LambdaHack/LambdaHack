-- | Parsing of commandline arguments.
module Game.LambdaHack.Server.Commandline
  ( debugArgs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Server.State

-- TODO: make more maintainable

-- | Parse server debug parameters from commandline arguments.
debugArgs :: [String] -> IO DebugModeSer
debugArgs args = do
  let usage =
        [ "Configure debug options here, gameplay options in config.rules.ini."
        , "  --knowMap  reveal map for all clients in the next game"
        , "  --knowEvents  show all events in the next game (needs --knowMap)"
        , "  --sniffIn  display all incoming commands on console "
        , "  --sniffOut  display all outgoing commands on console "
        , "  --allClear  let all map tiles be translucent"
        , "  --gameMode m  start next game in the given mode"
        , "  --automateAll  give control of all UI teams to computer"
        , "  --keepAutomated  keep factions automated after game over"
        , "  --newGame n  start a new game, overwriting the save file,"
        , "               with difficulty for all UI players set to n"
        , "  --stopAfter n  exit this game session after around n seconds"
        , "  --benchmark  print stats, limit saving and other file operations"
        , "  --setDungeonRng s  set dungeon generation RNG seed to string s"
        , "  --setMainRng s  set the main game RNG seed to string s"
        , "  --dumpInitRngs  dump RNG states from the start of the game"
        , "  --dbgMsgSer  let the server emit its internal debug messages"
        , "  --fontFamily s  use the given font family for the main game window"
        , "  --fontSize s  use the given font size for the main game window"
        , "  --noColorIsBold  don't use bold attribute for colorful characters"
        , "  --maxFps n  display at most n frames per second"
        , "  --disableAutoYes  never auto-answer all prompts"
        , "  --noAnim  don't show any animations"
        , "  --savePrefix  prepend the text to all savefile names"
        , "  --frontendStd  use the simple stdout/stdin frontend"
        , "  --frontendNull  use no frontend at all (for AIvsAI benchmarks)"
        , "  --dbgMsgCli  let clients emit their internal debug messages"
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
        (parseArgs rest) {sgameMode = Just $ toGroupName (T.pack s)}
      parseArgs ("--automateAll" : rest) =
        (parseArgs rest) {sautomateAll = True}
      parseArgs ("--keepAutomated" : rest) =
        (parseArgs rest) {skeepAutomated = True}
      parseArgs ("--newGame" : s : rest) =
        let debugSer = parseArgs rest
            scurDiffSer = read s
        in debugSer { scurDiffSer
                    , snewGameSer = True
                    , sdebugCli = (sdebugCli debugSer) {snewGameCli = True}}
      parseArgs ("--stopAfterSeconds" : s : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli =
             (sdebugCli debugSer) {sstopAfterSeconds = Just $ read s}}
      parseArgs ("--stopAfterFrames" : s : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli =
             (sdebugCli debugSer) {sstopAfterFrames = Just $ read s}}
      parseArgs ("--benchmark" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {sbenchmark = True}}
      parseArgs ("--setDungeonRng" : s : rest) =
        (parseArgs rest) {sdungeonRng = Just $ read s}
      parseArgs ("--setMainRng" : s : rest) =
        (parseArgs rest) {smainRng = Just $ read s}
      parseArgs ("--dumpInitRngs" : rest) =
        (parseArgs rest) {sdumpInitRngs = True}
      parseArgs ("--dbgMsgSer" : rest) =
        (parseArgs rest) {sdbgMsgSer = True}
      parseArgs ("--fontFamily" : s : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {sfontFamily = Just $ T.pack s}}
      parseArgs ("--fontSize" : s : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {sfontSize = Just $ T.pack s}}
      parseArgs ("--noColorIsBold" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli =
                       (sdebugCli debugSer) {scolorIsBold = Just False}}
      parseArgs ("--maxFps" : n : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli =
                       (sdebugCli debugSer) {smaxFps = Just $ max 1 $ read n}}
      parseArgs ("--disableAutoYes" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {sdisableAutoYes = True}}
      parseArgs ("--noAnim" : rest) =
        let debugSer = parseArgs rest
        in debugSer {sdebugCli = (sdebugCli debugSer) {snoAnim = Just True}}
      parseArgs ("--savePrefix" : s : rest) =
        let debugSer = parseArgs rest
        in debugSer { ssavePrefixSer = s
                    , sdebugCli =
                        (sdebugCli debugSer) {ssavePrefixCli = s}}
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
