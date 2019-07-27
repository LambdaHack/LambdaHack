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
import qualified System.Random as R

-- Dependence on ClientOptions is an anomaly. Instead, probably the raw
-- remaining commandline should be passed and parsed by the client to extract
-- client and ui options from and singnal an error if anything was left.

import Game.LambdaHack.Client (ClientOptions (..))
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Server.ServerOptions

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
  sprintEachScreen  <- printEachScreenP
  sbenchmark        <- benchmarkP
  sdungeonRng       <- setDungeonRngP
  smainRng          <- setMainRngP
  sdumpInitRngs     <- dumpInitRngsP
  sdbgMsgSer        <- dbgMsgSerP
  sgtkFontFamily    <- gtkFontFamilyP
  sdlSquareFontFile <- sdlSquareFontFileP
  sdlPropFontSize   <- sdlPropFontSizeP
  sdlPropFontFile   <- sdlPropFontFileP
  sdlMonoFontSize   <- sdlMonoFontSizeP
  sdlMonoFontFile   <- sdlMonoFontFileP
  sdlScalableSizeAdd <- sdlScalableSizeAddP
  sdlBitmapSizeAdd  <- sdlBitmapSizeAddP
  sscalableFontSize <- scalableFontSizeP
  sfontDir          <- fontDirP
  slogPriority      <- logPriorityP
  smaxFps           <- maxFpsP
  sdisableAutoYes   <- disableAutoYesP
  snoAnim           <- noAnimP
  ssavePrefixSer    <- savePrefixP
  sfrontendTeletype <- frontendTeletypeP
  sfrontendNull     <- frontendNullP
  sfrontendLazy     <- frontendLazyP
  sdbgMsgCli        <- dbgMsgCliP

  pure ServerOptions
    {
      sclientOptions = ClientOptions
        { stitle         = Nothing
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
         <> help "At game over show samples of all items (--sknowEvents disables this)" )

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
  strOption (  long "gameMode"
            <> metavar "MODE"
            <> help "Start next game in the scenario indicated by MODE" )
 where
  toGameMode :: String -> GroupName ModeKind
  toGameMode = toGroupName . T.pack

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
              <> showDefault
              <> value difficultyDefault
              <> metavar "N" )

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

printEachScreenP :: Parser Bool
printEachScreenP =
  switch (  long "printEachScreen"
         <> help "Take a screenshot of each rendered distinct frame (SDL only)" )

benchmarkP :: Parser Bool
benchmarkP =
  switch (  long "benchmark"
         <> help "Restrict file IO, print timing stats" )

setDungeonRngP :: Parser (Maybe R.StdGen)
setDungeonRngP = optional $
  option auto (  long "setDungeonRng"
              <> metavar "RNG_SEED"
              <> help "Set dungeon generation RNG seed to string RNG_SEED" )

setMainRngP :: Parser (Maybe R.StdGen)
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

gtkFontFamilyP :: Parser (Maybe Text)
gtkFontFamilyP = optional $ T.pack <$>
  strOption (  long "gtkFontFamily"
            <> metavar "FONT_FAMILY"
            <> help "Use FONT_FAMILY for the main game window in GTK frontend" )

sdlSquareFontFileP :: Parser (Maybe Text)
sdlSquareFontFileP = optional $ T.pack <$>
  strOption (  long "sdlFontFile"
            <> metavar "FONT_FILE"
            <> help "Use FONT_FILE for the main game window in SDL2 frontend" )

sdlPropFontSizeP :: Parser (Maybe Int)
sdlPropFontSizeP = optional $ max 0 <$>
  option auto (  long "sdlPropFontSize"
              <> metavar "N"
              <> help "Use font size of N pixels for the message overlay in SDL2 frontend" )

sdlPropFontFileP :: Parser (Maybe Text)
sdlPropFontFileP = optional $ T.pack <$>
  strOption (  long "sdlPropFontFile"
            <> metavar "FONT_FILE"
            <> help "Use FONT_FILE for the message overlay in SDL2 frontend" )

sdlMonoFontSizeP :: Parser (Maybe Int)
sdlMonoFontSizeP = optional $ max 0 <$>
  option auto (  long "sdlMonoFontSize"
              <> metavar "N"
              <> help "Use font size of N pixels for the monospaced rectangular font in SDL2 frontend" )

sdlMonoFontFileP :: Parser (Maybe Text)
sdlMonoFontFileP = optional $ T.pack <$>
  strOption (  long "sdlMonoFontFile"
            <> metavar "FONT_FILE"
            <> help "Use FONT_FILE for the monospaced rectangular font in SDL2 frontend" )

sdlScalableSizeAddP :: Parser (Maybe Int)
sdlScalableSizeAddP = optional $
  option auto (  long "sdlScalableSizeAdd"
              <> metavar "N"
              <> help "Enlarge map cells by N over scalable font max height in SDL2 frontend (N may be negative)" )

sdlBitmapSizeAddP :: Parser (Maybe Int)
sdlBitmapSizeAddP = optional $
  option auto (  long "sdlBitmapSizeAdd"
              <> metavar "N"
              <> help "Enlarge map cells by N on top of bitmap font max height in SDL2 frontend (N may be negative)" )

scalableFontSizeP :: Parser (Maybe Int)
scalableFontSizeP = optional $
  option auto (  long "scalableFontSize"
              <> metavar "N"
              <> help "Use font size of N pixels for the main game window (interpreted differently by different graphical frontends; ignored for bitmap fonts)" )

fontDirP :: Parser (Maybe FilePath)
fontDirP = optional $
  strOption (  long "fontDir"
            <> metavar "FILEPATH"
            <> help "Take font files for the SDL2 frontend from FILEPATH" )

maxFpsP :: Parser (Maybe Int)
maxFpsP = optional $ max 1 <$>
  option auto (  long "maxFps"
              <> metavar "N"
              <> help "Display at most N frames per second" )

logPriorityP :: Parser (Maybe Int)
logPriorityP = optional $
  option (auto >>= verifyLogPriority) $
       long "logPriority"
    <> metavar "N"
    <> help "Log only messages of priority at least N, where 1 (all) is the lowest and 5 (errors only) is the default."
  where
    verifyLogPriority n = do
      if (n >= 1 && n <= 5)
      then return n
      else readerError $ "N has to be a positive integer not larger than 5"

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
            <> value ""
            <> help "Prepend PREFIX to all savefile names" )

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
