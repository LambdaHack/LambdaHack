-- | Semantics of server commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Server
  ( mainSer
  ) where

import System.Environment (getArgs)

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.ClientCmd
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Frontend
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.Fov
import Game.LambdaHack.Server.LoopAction
import Game.LambdaHack.Server.ServerSem
import Game.LambdaHack.Server.State

-- | The semantics of server commands. The resulting boolean value
-- indicates if the command took some time.
cmdSerSem :: (MonadAtomic m, MonadServer m) => CmdSer -> m Bool
cmdSerSem cmd = case cmd of
  MoveSer aid dir -> moveSer aid dir False
  ExploreSer aid dir -> moveSer aid dir True
  RunSer aid dir -> runSer aid dir
  WaitSer aid -> waitSer aid >> return True
  PickupSer aid i k l -> pickupSer aid i k l >> return True
  DropSer aid iid -> dropSer aid iid >> return True
  ProjectSer aid p eps iid container -> projectSer aid p eps iid container
  ApplySer aid iid container -> applySer aid iid container >> return True
  TriggerSer aid p -> triggerSer aid p
  SetPathSer aid path -> setPathSer aid path >> return (not $ null path)
  GameRestartSer aid t -> gameRestartSer aid t >> return False
  GameExitSer aid -> gameExitSer aid >> return False
  GameSaveSer _ -> gameSaveSer >> return False
  CfgDumpSer aid -> cfgDumpSer aid >> return False

debugArgs :: IO DebugModeSer
debugArgs = do
  args <- getArgs
  let usage =
        [ "Configure server debug options here, gameplay in config.rules.ini."
        , "  --knowMap reveal map for all clients in the next game"
        , "  --knowEvents show all events in the next game (needs --knowMap)"
        , "  --sniffIn display all incoming commands on console "
        , "  --sniffOut display all outgoing commands on console "
        , "  --allClear let all map tiles be translucent"
        , "  --debugCli let clients emit their debug messages"
        , "  --tryFov m set a Field of View mode, where m can be"
        , "    Digital r, r > 0"
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
      parseArgs ("--tryFov" : "Digital" : r : rest) | (read r :: Int) > 0 =
        (parseArgs rest) {stryFov = Just $ Digital $ read r}
      parseArgs ("--tryFov" : mode : rest) =
        (parseArgs rest) {stryFov = Just $ read mode}
      parseArgs ("--debugCli" : rest) =
        (parseArgs rest) {sdebugCli = True}
      parseArgs _ = error $ unlines usage
  return $! parseArgs args

-- | Fire up the frontend with the engine fueled by content.
-- The action monad types to be used are determined by the 'exeSer'
-- and 'executorCli' calls. If other functions are used in their place
-- the types are different and so the whole pattern of computation
-- is different. Which of the frontends is run depends on the flags supplied
-- when compiling the engine library.
mainSer :: (MonadAtomic m, MonadConnServer m)
        => Kind.COps
        -> (m () -> IO ())
        -> (Kind.COps
            -> ((FactionId -> ChanFrontend -> ChanServer CmdClientUI -> IO ())
                -> (FactionId -> ChanServer CmdClientAI -> IO ())
                -> IO ())
            -> IO ())
        -> IO ()
mainSer copsSlow exeSer exeFront = do
  sdebugNxt <- debugArgs
  let cops = speedupCOps False copsSlow
      loopServer = loopSer sdebugNxt cmdSerSem
      exeServer executorUI executorAI = do
        exeSer (loopServer executorUI executorAI cops)
        waitForChildren
  exeFront cops exeServer
