-- | Personal game configuration file type definitions.
module Game.LambdaHack.Client.UI.Config
  ( Config(..), mkConfig, applyConfigToDebug
  ) where

import Control.DeepSeq
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.Ini as Ini
import qualified Data.Ini.Reader as Ini
import qualified Data.Ini.Types as Ini
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Game.LambdaHack.Common.ClientOptions
import System.Directory
import System.FilePath
import Text.Read

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Common.File
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.RuleKind

-- | Fully typed contents of the UI config file. This config
-- is a part of a game client.
data Config = Config
  { -- commands
    configCommands    :: ![(K.KM, (CmdCategory, HumanCmd))]
    -- hero names
  , configHeroNames   :: ![(Int, Text)]
    -- ui
  , configVi          :: !Bool  -- ^ the option for Vi keys takes precendence
  , configLaptop      :: !Bool  -- ^ because the laptop keys are the default
  , configFont        :: !String
  , configHistoryMax  :: !Int
  , configMaxFps      :: !Int
  , configNoAnim      :: !Bool
  , configRunStopMsgs :: !Bool
  }
  deriving Show

instance NFData Config

parseConfig :: Ini.Config -> Config
parseConfig cfg =
  let configCommands =
        let mkCommand (ident, keydef) =
              case stripPrefix "Macro_" ident of
                Just _ ->
                  let (key, def) = read keydef
                  in (K.mkKM key, def :: (CmdCategory, HumanCmd))
                Nothing -> assert `failure` "wrong macro id" `twith` ident
            section = Ini.allItems "extra_commands" cfg
        in map mkCommand section
      configHeroNames =
        let toNumber (ident, name) =
              case stripPrefix "HeroName_" ident of
                Just n -> (read n, T.pack name)
                Nothing -> assert `failure` "wrong hero name id" `twith` ident
            section = Ini.allItems "hero_names" cfg
        in map toNumber section
      getOption :: forall a. Read a => String -> a
      getOption optionName =
        let lookupFail :: forall b. String -> b
            lookupFail err =
              assert `failure` ("config file access failed:" <+> T.pack err)
                     `twith` (optionName, cfg)
            s = fromMaybe (lookupFail "") $ Ini.getOption "ui" optionName cfg
        in either lookupFail id $ readEither s
      configVi = getOption "movementViKeys_hjklyubn"
      -- The option for Vi keys takes precendence,
      -- because the laptop keys are the default.
      configLaptop = not configVi && getOption "movementLaptopKeys_uk8o79jl"
      configFont = getOption "font"
      configHistoryMax = getOption "historyMax"
      configMaxFps = max 1 $ getOption "maxFps"
      configNoAnim = getOption "noAnim"
      configRunStopMsgs = getOption "runStopMsgs"
  in Config{..}

-- | Read and parse UI config file.
mkConfig :: Kind.Ops RuleKind -> IO Config
mkConfig corule = do
  let stdRuleset = Kind.stdRuleset corule
      cfgUIName = rcfgUIName stdRuleset
      commentsUIDefault = init $ map (drop 2) $ lines $ rcfgUIDefault stdRuleset  -- TODO: init is a hack until Ini accepts empty files
      sUIDefault = unlines commentsUIDefault
      cfgUIDefault = either (assert `failure`) id $ Ini.parse sUIDefault
  dataDir <- appDataDir
  let userPath = dataDir </> cfgUIName <.> "ini"
  cfgUser <- do
    cpExists <- doesFileExist userPath
    if not cpExists
      then return Ini.emptyConfig
      else do
        sUser <- readFile userPath
        return $! either (assert `failure`) id $ Ini.parse sUser
  let cfgUI = M.unionWith M.union cfgUser cfgUIDefault  -- user cfg preferred
      conf = parseConfig cfgUI
  -- Catch syntax errors in complex expressions ASAP,
  return $! deepseq conf conf

applyConfigToDebug :: Config -> DebugModeCli -> Kind.Ops RuleKind
                   -> DebugModeCli
applyConfigToDebug sconfig sdebugCli corule =
  let stdRuleset = Kind.stdRuleset corule
  in (\dbg -> dbg {sfont =
        sfont dbg `mplus` Just (configFont sconfig)}) .
     (\dbg -> dbg {smaxFps =
        smaxFps dbg `mplus` Just (configMaxFps sconfig)}) .
     (\dbg -> dbg {snoAnim =
        snoAnim dbg `mplus` Just (configNoAnim sconfig)}) .
     (\dbg -> dbg {ssavePrefixCli =
        ssavePrefixCli dbg `mplus` Just (rsavePrefix stdRuleset)})
     $ sdebugCli
