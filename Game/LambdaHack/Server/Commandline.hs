{-# LANGUAGE ApplicativeDo #-}

-- | Parsing of commandline arguments.

module Game.LambdaHack.Server.Commandline
  ( debugModeSerPI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import qualified System.Random as R

import Options.Applicative

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
  knowMap              <- knowMapP
  knowEvents           <- knowEventsP
  knowItems            <- knowItemsP
  sniffIn              <- sniffInP
  sniffOut             <- sniffOutP
  allClear             <- allClearP
  boostRandomItem      <- boostRandItemP
  gameMode             <- gameModeP
  automateAll          <- automateAllP
  keepAutomated        <- keepAutomatedP
  ~(newGame,challenge) <- serToChallenge <$> newGameP
  stopAfterSeconds     <- stopAfterSecsP
  stopAfterFrames      <- stopAfterFramesP
  benchmark            <- benchmarkP
  dungeonRng           <- setDungeonRngP
  mainRng              <- setMainRngP
  dumpInitRngs         <- dumpInitRngsP
  dbgMsgSer            <- dbgMsgSerP
  gtkFontFamily        <- gtkFontFamilyP
  sdlFontFile          <- sdlFontFileP
  sdlTtfSizeAdd        <- sdlTtfSizeAddP
  sdlFonSizeAdd        <- sdlFonSizeAddP
  fontSize             <- fontSizeP
  noColorIsBold        <- noColorIsBoldP
  maxFps               <- maxFpsP
  disableAutoYes       <- disableAutoYesP
  noAnim               <- noAnimP
  savePrefix           <- savePrefixP
  frontendTeletype     <- frontendTeletypeP
  frontendNull         <- frontendNullP
  frontendLazy         <- frontendLazyP
  dbgMsgCli            <- dbgMsgCliP

  pure DebugModeSer
    { sknowMap         = knowMap
    , sknowEvents      = knowEvents
    , sknowItems       = knowItems
    , sniffIn          = sniffIn
    , sniffOut         = sniffOut
    , sallClear        = allClear
    , sboostRandomItem = boostRandomItem
    , sgameMode        = gameMode
    , sautomateAll     = automateAll
    , skeepAutomated   = keepAutomated
    , sdungeonRng      = dungeonRng
    , smainRng         = mainRng
    , snewGameSer      = newGame
    , scurChalSer      = challenge
    , sdumpInitRngs    = dumpInitRngs
    , ssavePrefixSer   = savePrefix
    , sdbgMsgSer       = dbgMsgSer
    , sdebugCli        = defDebugModeCli
        { sgtkFontFamily    = gtkFontFamily
        , sdlFontFile       = sdlFontFile
        , sdlTtfSizeAdd     = sdlTtfSizeAdd
        , sdlFonSizeAdd     = sdlFonSizeAdd
        , sfontSize         = fontSize
        , scolorIsBold      = noColorIsBold
        , smaxFps           = maxFps
        , sdisableAutoYes   = disableAutoYes
        , snoAnim           = noAnim
        , snewGameCli       = newGame
        , sbenchmark        = benchmark
        , ssavePrefixCli    = savePrefix
        , sfrontendTeletype = frontendTeletype
        , sfrontendNull     = frontendNull
        , sfrontendLazy     = frontendLazy
        , sdbgMsgCli        = dbgMsgCli
        , sstopAfterSeconds = stopAfterSeconds
        , sstopAfterFrames  = stopAfterFrames
        }
    }
 where
   serToChallenge :: Maybe Int -> (Bool, Challenge)
   serToChallenge Nothing  = (False, defaultChallenge)
   serToChallenge (Just n) = (True, Challenge n False False)

knowMapP :: Parser Bool
knowMapP =
  switch (  long "knowMap"
         <> help "reveal map for all clients in the next game" )

knowEventsP :: Parser Bool
knowEventsP =
  switch (  long "knowEvents"
         <> help "show all events in the next game (needs --knowMap)" )

knowItemsP :: Parser Bool
knowItemsP =
  switch (  long "knowItems"
         <> help "auto-identify all items in the next game (needs --knowEvents)" )

sniffInP :: Parser Bool
sniffInP =
  switch (  long "sniffIn"
         <> help "display all incoming commands on console" )

sniffOutP :: Parser Bool
sniffOutP =
  switch (  long "sniffOut"
         <> help "display all outgoing commands on console" )

allClearP :: Parser Bool
allClearP =
  switch (  long "allClear"
         <> help "let all map tiles be translucent" )

boostRandItemP :: Parser Bool
boostRandItemP =
  switch (  long "boostRandomItem"
         <> help "pick a random item and make it very common" )

gameModeP :: Parser (Maybe (GroupName ModeKind))
gameModeP = optional $ toGameMode <$>
  strOption (  long "gameMode"
            <> metavar "MODE"
            <> help "start next game in the given mode" )
 where
  toGameMode :: String -> GroupName ModeKind
  toGameMode s  = toGroupName (T.pack s)

automateAllP :: Parser Bool
automateAllP =
  switch (  long "automateAll"
         <> help "give control of all UI teams to computer" )

keepAutomatedP :: Parser Bool
keepAutomatedP =
  switch (  long "keepAutomated"
         <> help "keep factions automated after game over" )

newGameP :: Parser (Maybe Int)
newGameP =
  optional $ option auto
    ( long "newGame"
   <> help "start a new game, overwriting the save file, with difficulty for all UI players set to this value"
   <> metavar "INT" )

stopAfterSecsP :: Parser (Maybe Int)
stopAfterSecsP =
  optional $ option auto
    ( long "stopAfterSeconds"
   <> help "exit game session after around n seconds"
   <> metavar "INT" )

stopAfterFramesP :: Parser (Maybe Int)
stopAfterFramesP =
  optional $ option auto
    ( long "stopAfterFrames"
   <> help "exit game session after around n frames"
   <> metavar "INT" )

benchmarkP :: Parser Bool
benchmarkP =
  switch (  long "benchmark"
         <> help "restrict file IO, print stats" )

setDungeonRngP :: Parser (Maybe R.StdGen)
setDungeonRngP = optional $
  option auto (  long "setDungeonRng"
              <> metavar "DUNGEON_RNG"
              <> help "set dungeon generation RNG seed to string s" )

setMainRngP :: Parser (Maybe R.StdGen)
setMainRngP = optional $
  option auto (  long "setMainRng"
              <> metavar "MAIN_RNG"
              <> help "set the main game RNG seed to string s" )

dumpInitRngsP :: Parser Bool
dumpInitRngsP =
  switch (  long "dumpInitRngs"
         <> help "dump RNG states from the start of the game" )

dbgMsgSerP :: Parser Bool
dbgMsgSerP =
  switch (  long "dbgMsgSer"
         <> help "let the server emit its internal debug messages" )

gtkFontFamilyP :: Parser (Maybe Text)
gtkFontFamilyP = optional $ T.pack <$>
  strOption (  long "gtkFontFamily"
            <> metavar "FONT_FAMILY"
            <> help "use the given font family for the main game window in GTK" )

sdlFontFileP :: Parser (Maybe Text)
sdlFontFileP = optional $ T.pack <$>
  strOption (  long "gtkFontFamily"
            <> metavar "FONT_FILE"
            <> help "use the given font file for the main game window in SDL2" )

sdlTtfSizeAddP :: Parser (Maybe Int)
sdlTtfSizeAddP = optional $
  option auto (  long "sdlTtfSizeAdd"
              <> metavar "INT"
              <> help "enlarge map cells over scalable font max height in SDL2" )

sdlFonSizeAddP :: Parser (Maybe Int)
sdlFonSizeAddP = optional $
  option auto (  long "sdlFonSizeAdd"
              <> metavar "INT"
              <> help "enlarge map cells on top of .fon font max height in SDL2" )

fontSizeP :: Parser (Maybe Int)
fontSizeP = optional $
  option auto (  long "fontSize"
              <> metavar "INT"
              <> help "use the given font size for the main game window" )

noColorIsBoldP :: Parser (Maybe Bool)
noColorIsBoldP = flag Nothing (Just False)
                  (  long "noColorIsBold"
                  <> help "refrain from making some bright color characters bolder" )

maxFpsP :: Parser (Maybe Int)
maxFpsP = optional $ max 1 <$>
  option auto (  long "maxFps"
              <> metavar "INT"
              <> help "display at most n frames per second" )

disableAutoYesP :: Parser Bool
disableAutoYesP =
  switch (  long "disableAutoYes"
         <> help "never auto-answer all prompts" )

noAnimP :: Parser (Maybe Bool)
noAnimP = flag Nothing (Just True)
                  (  long "noAnim"
                  <> help "don't show any animations" )

savePrefixP :: Parser String
savePrefixP =
  strOption (  long "savePrefix"
            <> metavar "PREFIX"
            <> value ""
            <> help "prepend the text to all savefile names" )

frontendTeletypeP :: Parser Bool
frontendTeletypeP =
  switch (  long "frontendTeletype"
         <> help "use the line terminal frontend (for tests)" )

frontendNullP :: Parser Bool
frontendNullP =
  switch (  long "frontendNull"
         <> help "use frontend with no display (for benchmarks)" )

frontendLazyP :: Parser Bool
frontendLazyP =
  switch (  long "frontendLazy"
         <> help "use frontend that not even computes frames (for benchmarks)" )

dbgMsgCliP :: Parser Bool
dbgMsgCliP =
  switch (  long "dbgMsgCli"
         <> help "let clients emit their internal debug messages" )
