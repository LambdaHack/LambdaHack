{-# LANGUAGE ApplicativeDo #-}
-- | Parsing of commandline arguments.
module Game.LambdaHack.Server.Commandline
  ( serverOptionsPI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , serverOptionsP
      -- other internal operations too numerous and changing, so not listed
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude
-- Cabal
import qualified Paths_LambdaHack as Self (version)

import qualified Data.Text as T
import           Data.Version
import           Options.Applicative
import qualified System.Random.SplitMix32 as SM

-- Dependence on ClientOptions is an anomaly. Instead, probably the raw
-- remaining commandline should be passed and parsed by the client to extract
-- client and ui options from and singnal an error if anything was left.

import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Definition.Defs
import qualified Game.LambdaHack.Definition.DefsInternal as DefsInternal
import           Game.LambdaHack.Server.ServerOptions

-- | Parser for server options from commandline arguments.
serverOptionsPI :: ParserInfo ServerOptions
serverOptionsPI = info (serverOptionsP <**> helper <**> version)
                  $ fullDesc
                    <> progDesc "Configure debug options here, gameplay options in configuration file."

version :: Parser (a -> a)
version = infoOption (showVersion Self.version)
  (long "version"
   <> help "Print engine version information")

serverOptionsP :: Parser ServerOptions
serverOptionsP = do
  ~(snewGameSer, scurChalSer)
                    <- serToChallenge <$> newGameP
  sfullscreenMode   <- fullscreenModeP
  knowMap           <- knowMapP
  knowEvents        <- knowEventsP
  knowItems         <- knowItemsP
  showItemSamples   <- showItemSamplesP
  sexposePlaces     <- exposePlacesP
  sexposeItems      <- exposeItemsP
  sexposeActors     <- exposeActorsP
  sniff             <- sniffP
  sallClear         <- allClearP
  sboostRandomItem  <- boostRandItemP
  sgameMode         <- gameModeP
  sautomateAll      <- automateAllP
  skeepAutomated    <- keepAutomatedP
  sstopAfterSeconds <- stopAfterSecsP
  sstopAfterFrames  <- stopAfterFramesP
  sstopAfterGameOver <- stopAfterGameOverP
  sprintEachScreen  <- printEachScreenP
  sbenchmark        <- benchmarkP
  sbenchMessages    <- benchMessagesP
  sdungeonRng       <- setDungeonRngP
  smainRng          <- setMainRngP
  sdumpInitRngs     <- dumpInitRngsP
  sdbgMsgSer        <- dbgMsgSerP
  sassertExplored   <- assertExploredP
  schosenFontset    <- chosenFontsetP
  sallFontsScale    <- allFontsScaleP
  slogPriority      <- logPriorityP
  smaxFps           <- maxFpsP
  sdisableAutoYes   <- disableAutoYesP
  snoAnim           <- noAnimP
  ssavePrefixSer    <- savePrefixP
  sfrontendANSI     <- frontendANSIP
  sfrontendTeletype <- frontendTeletypeP
  sfrontendNull     <- frontendNullP
  sfrontendLazy     <- frontendLazyP
  sdbgMsgCli        <- dbgMsgCliP

  pure ServerOptions
    {
      sclientOptions = ClientOptions
        { sfonts         = []  -- comes only from config file
        , sfontsets      = []  -- comes only from config file
        , stitle         = Nothing
        , snewGameCli    = snewGameSer
        , ssavePrefixCli = ssavePrefixSer
        , ..
        }
    , sknowMap = knowMap || knowEvents || knowItems
    , sknowEvents = knowEvents || knowItems
    , sknowItems = knowItems
    , sshowItemSamples = not (knowEvents || knowItems) && showItemSamples
    , ..
    }
 where
   serToChallenge :: Maybe Int -> (Bool, Challenge)
   serToChallenge Nothing      = (False, defaultChallenge)
   serToChallenge (Just cdiff) = (True, defaultChallenge {cdiff})

knowMapP :: Parser Bool
knowMapP =
  switch (  long "knowMap"
         <> help "Reveal map for all clients in the next game" )

knowEventsP :: Parser Bool
knowEventsP =
  switch (  long "knowEvents"
         <> help "Show all events in the next game (implies --knowMap)" )

knowItemsP :: Parser Bool
knowItemsP =
  switch (  long "knowItems"
         <> help "Auto-identify all items in the next game (implies --knowEvents)" )

exposePlacesP :: Parser Bool
exposePlacesP =
  switch (  long "exposePlaces"
         <> help "Expose all possible places in the next game" )

exposeItemsP :: Parser Bool
exposeItemsP =
  switch (  long "exposeItems"
         <> help "Expose all possible items in the next game" )

exposeActorsP :: Parser Bool
exposeActorsP =
  switch (  long "exposeActors"
         <> help "Expose all killable actors in the next game" )

showItemSamplesP :: Parser Bool
showItemSamplesP =
  switch (  long "showItemSamples"
         <> help "At game over show samples of all items (--knowEvents disables this)" )

sniffP :: Parser Bool
sniffP =
  switch (  long "sniff"
         <> help "Monitor all trafic between server and clients" )

allClearP :: Parser Bool
allClearP =
  switch (  long "allClear"
         <> help "Let all map tiles be translucent" )

boostRandItemP :: Parser Bool
boostRandItemP =
  switch (  long "boostRandomItem"
         <> help "Pick a random item and make it very common" )

gameModeP :: Parser (Maybe (GroupName ModeKind))
gameModeP = optional $ toGameMode <$>
  option nonEmptyStr
         (  long "gameMode"
            <> metavar "MODE"
            <> help "Start next game in the scenario indicated by MODE" )
 where
  -- This ignores all but the first word of a game mode name
  -- and assumes the fist word is present among its frequencies.
  toGameMode :: String -> GroupName ModeKind
  toGameMode = DefsInternal.GroupName . head . T.words . T.pack
  nonEmptyStr :: ReadM String
  nonEmptyStr = eitherReader $ \case
    "" -> Left "name of game mode cannot be empty"
    ns -> Right ns

automateAllP :: Parser Bool
automateAllP =
  switch (  long "automateAll"
         <> help "Give control of all UI teams to computer" )

keepAutomatedP :: Parser Bool
keepAutomatedP =
  switch (  long "keepAutomated"
         <> help "Keep factions automated after game over" )

newGameP :: Parser (Maybe Int)
newGameP = optional $ max 1 <$> min difficultyBound <$>
  option auto (  long "newGame"
              <> help "Start a new game, overwriting the save file, with difficulty for all UI players set to N"
              <> metavar "N" )

fullscreenModeP :: Parser (Maybe FullscreenMode)
fullscreenModeP = optional $
  option auto (  long "fullscreenMode"
              <> short 'f'
              <> metavar "MODE"
              <> help "Display in MODE, one of NotFullscreen (default), BigBorderlessWindow (preferred), ModeChange" )

stopAfterSecsP :: Parser (Maybe Int)
stopAfterSecsP = optional $ max 0 <$>
  option auto (  long "stopAfterSeconds"
              <> help "Exit game session after around N seconds"
              <> metavar "N" )

stopAfterFramesP :: Parser (Maybe Int)
stopAfterFramesP = optional $ max 0 <$>
  option auto (  long "stopAfterFrames"
              <> help "Exit game session after around N frames"
              <> metavar "N" )

stopAfterGameOverP :: Parser Bool
stopAfterGameOverP =
  switch (  long "stopAfterGameOver"
         <> help "Exit the application after game over" )

printEachScreenP :: Parser Bool
printEachScreenP =
  switch (  long "printEachScreen"
         <> help "Take a screenshot of each rendered distinct frame (SDL only)" )

benchmarkP :: Parser Bool
benchmarkP =
  switch (  long "benchmark"
         <> help "Restrict file IO, print timing stats" )

benchMessagesP :: Parser Bool
benchMessagesP =
  switch (  long "benchMessages"
         <> help "Display messages in realistic was under AI control (for benchmarks)" )

setDungeonRngP :: Parser (Maybe SM.SMGen)
setDungeonRngP = optional $
  option auto (  long "setDungeonRng"
              <> metavar "RNG_SEED"
              <> help "Set dungeon generation RNG seed to string RNG_SEED" )

setMainRngP :: Parser (Maybe SM.SMGen)
setMainRngP = optional $
  option auto (  long "setMainRng"
              <> metavar "RNG_SEED"
              <> help "Set the main game RNG seed to string RNG_SEED" )

dumpInitRngsP :: Parser Bool
dumpInitRngsP =
  switch (  long "dumpInitRngs"
         <> help "Dump the RNG seeds used to initialize the game" )

dbgMsgSerP :: Parser Bool
dbgMsgSerP =
  switch (  long "dbgMsgSer"
         <> help "Emit extra internal server debug messages" )

assertExploredP :: Parser (Maybe Int)
assertExploredP = optional $ max 1 <$>
  option auto (  long "assertExplored"
              <> help "Check that when the session ends, the indicated level has been explored"
              <> metavar "N" )

chosenFontsetP :: Parser (Maybe Text)
chosenFontsetP = optional $ T.pack <$>
  strOption (  long "fontset"
            <> metavar "FONTSET_ID"
            <> help "Render UI using the given fontset from config file" )

allFontsScaleP :: Parser (Maybe Double)
allFontsScaleP = optional $ max 0 <$>
  option auto (  long "allFontsScale"
              <> metavar "D"
              <> help "Scale all fonts by D, resizing the whole UI" )

maxFpsP :: Parser (Maybe Double)
maxFpsP = optional $ max 0 <$>
  option auto (  long "maxFps"
              <> metavar "D"
              <> help "Display at most D frames per second" )

logPriorityP :: Parser (Maybe Int)
logPriorityP = optional $
  option (auto >>= verifyLogPriority) $
       long "logPriority"
    <> showDefault
    <> value 5
    <> metavar "N"
    <> help ( "Log only messages of priority at least N, where 1 (all) is "
           ++ "the lowest and 5 logs errors only; use value 0 for testing on "
           ++ "CIs without graphics access; setting priority to 0 causes "
           ++ "SDL frontend to init and quit at once" )
  where
    verifyLogPriority n =
      if n >= 0 && n <= 5
      then return n
      else readerError "N has to be 0 or a positive integer not larger than 5"

disableAutoYesP :: Parser Bool
disableAutoYesP =
  switch (  long "disableAutoYes"
         <> help "Never auto-answer prompts, not even when UI faction is automated" )

noAnimP :: Parser (Maybe Bool)
noAnimP =
  flag Nothing (Just True)
       (  long "noAnim"
       <> help "Don't show any animations" )

savePrefixP :: Parser String
savePrefixP =
  strOption (  long "savePrefix"
            <> metavar "PREFIX"
            <> showDefault
            <> value ""
            <> help "Prepend PREFIX to all savefile names" )

frontendANSIP :: Parser Bool
frontendANSIP =
  switch (  long "frontendANSI"
         <> help "Use the ANSI terminal frontend (best for screen readers)" )

frontendTeletypeP :: Parser Bool
frontendTeletypeP =
  switch (  long "frontendTeletype"
         <> help "Use the line terminal frontend (for tests)" )

frontendNullP :: Parser Bool
frontendNullP =
  switch (  long "frontendNull"
         <> help "Use frontend with no display (for benchmarks)" )

frontendLazyP :: Parser Bool
frontendLazyP =
  switch (  long "frontendLazy"
         <> help "Use frontend that not even computes frames (for benchmarks)" )

dbgMsgCliP :: Parser Bool
dbgMsgCliP =
  switch (  long "dbgMsgCli"
         <> help "Emit extra internal client debug messages" )
