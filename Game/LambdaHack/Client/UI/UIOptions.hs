{-# LANGUAGE DeriveGeneric #-}
-- | UI client options.
module Game.LambdaHack.Client.UI.UIOptions
  ( UIOptions(..), mkUIOptions, applyUIOptions
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , configError, readError, parseConfig
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.DeepSeq
import           Data.Binary
import qualified Data.Ini as Ini
import qualified Data.Ini.Reader as Ini
import qualified Data.Ini.Types as Ini
import qualified Data.Map.Strict as M
import           GHC.Generics (Generic)
import           System.FilePath
import           Text.Read

import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Common.File
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Content.RuleKind

-- | Options that affect the UI of the client.
data UIOptions = UIOptions
  { -- commands
    uCommands           :: [(K.KM, CmdTriple)]
    -- hero names
  , uHeroNames          :: [(Int, (Text, Text))]
    -- ui
  , uVi                 :: Bool  -- ^ the option for Vi keys takes precendence
  , uLaptop             :: Bool  -- ^ because the laptop keys are the default
  , uGtkFontFamily      :: Text
  , uSdlFontFile        :: Text
  , uSdlScalableSizeAdd :: Int
  , uSdlBitmapSizeAdd   :: Int
  , uScalableFontSize   :: Int
  , uHistoryMax         :: Int
  , uMaxFps             :: Int
  , uNoAnim             :: Bool
  , uRunStopMsgs        :: Bool
  , uhpWarningPercent   :: Int
      -- ^ HP percent at which warning is emitted.
  , uCmdline            :: [String]
      -- ^ Hardwired commandline arguments to process.
  }
  deriving (Show, Generic)

instance NFData UIOptions

instance Binary UIOptions

configError :: String -> a
configError err = error $ "Error when parsing configuration file. Please fix config.ui.ini or remove it altogether. The details:\n" ++ err

readError :: Read a => String -> a
readError = either (configError . ("when reading a value" `showFailure`)) id
            . readEither

parseConfig :: Ini.Config -> UIOptions
parseConfig cfg =
  let uCommands =
        let mkCommand (ident, keydef) =
              case stripPrefix "Cmd_" ident of
                Just _ ->
                  let (key, def) = readError keydef
                  in (K.mkKM key, def :: CmdTriple)
                Nothing -> configError $ "wrong macro id" `showFailure` ident
            section = Ini.allItems "extra_commands" cfg
        in map mkCommand section
      uHeroNames =
        let toNumber (ident, nameAndPronoun) =
              case stripPrefix "HeroName_" ident of
                Just n -> (readError n, readError nameAndPronoun)
                Nothing -> configError
                           $ "wrong hero name id" `showFailure` ident
            section = Ini.allItems "hero_names" cfg
        in map toNumber section
      getOption :: forall a. Read a => String -> a
      getOption optionName =
        let lookupFail :: forall b. String -> b
            lookupFail err =
              configError $ "config file access failed"
                            `showFailure` (err, optionName, cfg)
            s = fromMaybe (lookupFail "") $ Ini.getOption "ui" optionName cfg
        in either lookupFail id $ readEither s
      uVi = getOption "movementViKeys_hjklyubn"
      -- The option for Vi keys takes precendence,
      -- because the laptop keys are the default.
      uLaptop = not uVi && getOption "movementLaptopKeys_uk8o79jl"
      uGtkFontFamily = getOption "gtkFontFamily"
      uSdlFontFile = getOption "sdlFontFile"
      uSdlScalableSizeAdd = getOption "sdlScalableSizeAdd"
      uSdlBitmapSizeAdd = getOption "sdlBitmapSizeAdd"
      uScalableFontSize = getOption "scalableFontSize"
      uHistoryMax = getOption "historyMax"
      uMaxFps = max 1 $ getOption "maxFps"
      uNoAnim = getOption "noAnim"
      uRunStopMsgs = getOption "runStopMsgs"
      uhpWarningPercent = getOption "hpWarningPercent"
      uCmdline = words $ getOption "overrideCmdline"
  in UIOptions{..}

-- | Read and parse UI config file.
mkUIOptions :: COps -> Bool -> IO UIOptions
mkUIOptions COps{corule} benchmark = do
  let cfgUIName = rcfgUIName corule
      sUIDefault = rcfgUIDefault corule
      cfgUIDefault =
        either (configError . ("Ini.parse sUIDefault" `showFailure`)) id
        $ Ini.parse sUIDefault
  dataDir <- appDataDir
  let userPath = dataDir </> cfgUIName
  cfgUser <- if benchmark then return Ini.emptyConfig else do
    cpExists <- doesFileExist userPath
    if not cpExists
      then return Ini.emptyConfig
      else do
        sUser <- readFile userPath
        return $! either (configError . ("Ini.parse sUser" `showFailure`)) id
                  $ Ini.parse sUser
  let cfgUI = M.unionWith M.union cfgUser cfgUIDefault  -- user cfg preferred
      conf = parseConfig cfgUI
  -- Catch syntax errors in complex expressions ASAP.
  return $! deepseq conf conf

-- | Modify client options with UI options.
applyUIOptions :: COps -> UIOptions -> ClientOptions -> ClientOptions
applyUIOptions COps{corule} uioptions soptions =
     (\opts -> opts {sgtkFontFamily =
        sgtkFontFamily opts `mplus` Just (uGtkFontFamily uioptions)}) .
     (\opts -> opts {sdlFontFile =
        sdlFontFile opts `mplus` Just (uSdlFontFile uioptions)}) .
     (\opts -> opts {sdlScalableSizeAdd =
        sdlScalableSizeAdd opts `mplus` Just (uSdlScalableSizeAdd uioptions)}) .
     (\opts -> opts {sdlBitmapSizeAdd =
        sdlBitmapSizeAdd opts `mplus` Just (uSdlBitmapSizeAdd uioptions)}) .
     (\opts -> opts {sscalableFontSize =
        sscalableFontSize opts `mplus` Just (uScalableFontSize uioptions)}) .
     (\opts -> opts {smaxFps =
        smaxFps opts `mplus` Just (uMaxFps uioptions)}) .
     (\opts -> opts {snoAnim =
        snoAnim opts `mplus` Just (uNoAnim uioptions)}) .
     (\opts -> opts {stitle =
        stitle opts `mplus` Just (rtitle corule)}) .
     (\opts -> opts {sfontDir =
        sfontDir opts `mplus` Just (rfontDir corule)})
     $ soptions
