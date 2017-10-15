{-# LANGUAGE ApplicativeDo #-}

-- | Parsing of commandline arguments.

module Game.LambdaHack.Server.Commandline
  ( debugModeSerPI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T
import Options.Applicative
import qualified System.Random as R

import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.State

-- | Parser for server debug parameters from commandline arguments.
debugModeSerPI :: ParserInfo DebugModeSer
debugModeSerPI =  info ( helper <*> debugModeSerP )
               $  fullDesc
               <> progDesc "Configure debug options here, gameplay options in config.rules.ini."

debugModeSerP :: Parser DebugModeSer
debugModeSerP = do
  ~(snewGameSer, scurChalSer)
                    <- serToChallenge <$> newGameP
  sknowMap          <- knowMapP
  sknowEvents       <- knowEventsP
  sknowItems        <- knowItemsP
  sniffIn           <- sniffInP
  sniffOut          <- sniffOutP
  sallClear         <- allClearP
  sboostRandomItem  <- boostRandItemP
  sgameMode         <- gameModeP
  sautomateAll      <- automateAllP
  skeepAutomated    <- keepAutomatedP
  sstopAfterSeconds <- stopAfterSecsP
  sstopAfterFrames  <- stopAfterFramesP
  sbenchmark        <- benchmarkP
  sdungeonRng       <- setDungeonRngP
  smainRng          <- setMainRngP
  sdumpInitRngs     <- dumpInitRngsP
  sdbgMsgSer        <- dbgMsgSerP
  sgtkFontFamily    <- gtkFontFamilyP
  sdlFontFile       <- sdlFontFileP
  sdlTtfSizeAdd     <- sdlTtfSizeAddP
  sdlFonSizeAdd     <- sdlFonSizeAddP
  sfontSize         <- fontSizeP
  sfontDir          <- fontDirP
  scolorIsBold      <- noColorIsBoldP
  smaxFps           <- maxFpsP
  sdisableAutoYes   <- disableAutoYesP
  snoAnim           <- noAnimP
  ssavePrefixSer    <- savePrefixP
  sfrontendTeletype <- frontendTeletypeP
  sfrontendNull     <- frontendNullP
  sfrontendLazy     <- frontendLazyP
  sdbgMsgCli        <- dbgMsgCliP

  pure DebugModeSer
    {
      sdebugCli = DebugModeCli
        { stitle         = Nothing
        , snewGameCli    = snewGameSer
        , ssavePrefixCli = ssavePrefixSer
        , ..
        }
    , ..
    }
 where
   serToChallenge :: Maybe Int -> (Bool, Challenge)
   serToChallenge Nothing      = (False, defaultChallenge)
   serToChallenge (Just cdiff) = (True,  defaultChallenge {cdiff})

knowMapP :: Parser Bool
knowMapP =
  switch (  long "knowMap"
         <> help "Reveal map for all clients in the next game" )

knowEventsP :: Parser Bool
knowEventsP =
  switch (  long "knowEvents"
         <> help "Show all events in the next game (needs --knowMap)" )

knowItemsP :: Parser Bool
knowItemsP =
  switch (  long "knowItems"
         <> help "Auto-identify all items in the next game (needs --knowEvents)" )

sniffInP :: Parser Bool
sniffInP =
  switch (  long "sniffIn"
         <> help "Display all incoming commands on console" )

sniffOutP :: Parser Bool
sniffOutP =
  switch (  long "sniffOut"
         <> help "Display all outgoing commands on console" )

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
            <> help "Start next game in the given mode" )
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
newGameP = optional $
  option auto (  long "newGame"
              <> help "Start a new game, overwriting the save file, with difficulty for all UI players set to N"
              <> metavar "N" )

stopAfterSecsP :: Parser (Maybe Int)
stopAfterSecsP = optional $
  option auto (  long "stopAfterSeconds"
              <> help "Exit game session after around N seconds"
              <> metavar "N" )

stopAfterFramesP :: Parser (Maybe Int)
stopAfterFramesP = optional $
  option auto (  long "stopAfterFrames"
              <> help "Exit game session after around N frames"
              <> metavar "N" )

benchmarkP :: Parser Bool
benchmarkP =
  switch (  long "benchmark"
         <> help "Restrict file IO, print stats" )

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
         <> help "Dump RNG states from the start of the game" )

dbgMsgSerP :: Parser Bool
dbgMsgSerP =
  switch (  long "dbgMsgSer"
         <> help "Let the server emit its internal debug messages" )

gtkFontFamilyP :: Parser (Maybe Text)
gtkFontFamilyP = optional $ T.pack <$>
  strOption (  long "gtkFontFamily"
            <> metavar "FONT_FAMILY"
            <> help "Use the given font family for the main game window in GTK" )

sdlFontFileP :: Parser (Maybe Text)
sdlFontFileP = optional $ T.pack <$>
  strOption (  long "gtkFontFamily"
            <> metavar "FONT_FILE"
            <> help "Use the given font file for the main game window in SDL2" )

sdlTtfSizeAddP :: Parser (Maybe Int)
sdlTtfSizeAddP = optional $
  option auto (  long "sdlTtfSizeAdd"
              <> metavar "N"
              <> help "Enlarge map cells over scalable font max height in SDL2" )

sdlFonSizeAddP :: Parser (Maybe Int)
sdlFonSizeAddP = optional $
  option auto (  long "sdlFonSizeAdd"
              <> metavar "N"
              <> help "Enlarge map cells on top of .fon font max height in SDL2" )

fontSizeP :: Parser (Maybe Int)
fontSizeP = optional $
  option auto (  long "fontSize"
              <> metavar "N"
              <> help "Use the given font size for the main game window" )

fontDirP :: Parser (Maybe FilePath)
fontDirP = optional $
  option auto (  long "fontDir"
              <> metavar "FILEPATH"
              <> help "Use the given font path" )

noColorIsBoldP :: Parser (Maybe Bool)
noColorIsBoldP =
  flag Nothing (Just False)
       (  long "noColorIsBold"
       <> help "Refrain from making some bright color characters bolder" )

maxFpsP :: Parser (Maybe Int)
maxFpsP = optional $ max 1 <$>
  option auto (  long "maxFps"
              <> metavar "N"
              <> help "Display at most N frames per second" )

disableAutoYesP :: Parser Bool
disableAutoYesP =
  switch (  long "disableAutoYes"
         <> help "Never auto-answer all prompts" )

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
            <> help "Prepend the text to all savefile names" )

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
         <> help "Let clients emit their internal debug messages" )
