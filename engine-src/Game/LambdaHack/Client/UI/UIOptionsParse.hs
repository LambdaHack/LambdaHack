-- | UI client options.
module Game.LambdaHack.Client.UI.UIOptionsParse
  ( mkUIOptions, applyUIOptions
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , configError, readError, parseConfig
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.DeepSeq
import qualified Data.Ini as Ini
import qualified Data.Ini.Reader as Ini
import qualified Data.Ini.Types as Ini
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Version
import           System.FilePath
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Read

import           Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.File
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Save (compatibleVersion, delayPrint)
import qualified Game.LambdaHack.Common.Save as Save
import           Game.LambdaHack.Content.RuleKind

configError :: String -> a
configError err = error $ "Error when parsing configuration file. Please fix config.ui.ini or remove it altogether. The details:\n" ++ err

readError :: Read a => String -> a
readError s = either (configError . ("when reading:\n" ++ s `showFailure`)) id
              $ readEither s

parseConfig :: Ini.Config -> UIOptions
parseConfig cfg =
  let uCommands =
        let mkCommand (ident, keydef) =
              case stripPrefix "Cmd_" ident of
                Just _ ->
                  let (key, def) = readError keydef
                  in (K.mkKM key, def :: CmdTriple)
                Nothing -> configError $ "wrong macro id" `showFailure` ident
            section = Ini.allItems "additional_commands" cfg
        in map mkCommand section
      uHeroNames =
        let toNumber (ident, nameAndPronoun) =
              case stripPrefix "HeroName_" ident of
                Just n -> (readError n, readError nameAndPronoun)
                Nothing -> configError
                           $ "wrong hero name id" `showFailure` ident
            section = Ini.allItems "hero_names" cfg
        in map toNumber section
      lookupFail :: forall b. String -> String -> b
      lookupFail optionName err =
        configError $ "config file access failed"
                      `showFailure` (err, optionName, cfg)
      _getOptionMaybe :: forall a. Read a => String -> Maybe a
      _getOptionMaybe optionName =
        let ms = Ini.getOption "ui" optionName cfg
        in either (lookupFail optionName) id . readEither <$> ms
      getOption :: forall a. Read a => String -> a
      getOption optionName =
        let s = fromMaybe (lookupFail optionName "")
                $ Ini.getOption "ui" optionName cfg
        in either (lookupFail optionName) id $ readEither s
      uVi = getOption "movementViKeys_hjklyubn"
      uLeftHand = getOption "movementLeftHandKeys_axwdqezc"
      uChosenFontset = getOption "chosenFontset"
      uAllFontsScale = getOption "allFontsScale"
      uScreen1PerLine = getOption "screenOneMessagePerLine"
      uHistory1PerLine = getOption "historyOneMessagePerLine"
      uHistoryMax = getOption "historyMax"
      uMaxFps = max 1 $ getOption "maxFps"
      uNoAnim = getOption "noAnim"
      uhpWarningPercent = getOption "hpWarningPercent"
      uMessageColors =
        map (second readError) $ Ini.allItems "message_colors" cfg
      uCmdline = glueSeed $ words $ getOption "overrideCmdline"
      uFonts =
        let toFont (ident, fontString) = (T.pack ident, readError fontString)
            section = Ini.allItems "fonts" cfg
        in map toFont section
      uFontsets =
        let toFontSet (ident, fontSetString) =
              (T.pack ident, readError fontSetString)
            section = Ini.allItems "fontsets" cfg
        in map toFontSet section
      uFullscreenMode = getOption "fullscreenMode"
  in UIOptions{..}

glueSeed :: [String] -> [String]
glueSeed [] = []
glueSeed ("SMGen" : s1 : s2 : rest) =
  ("SMGen" ++ " " ++ s1 ++ " " ++ s2) : glueSeed rest
glueSeed (s : rest) = s : glueSeed rest

-- | Read and parse UI config file.
mkUIOptions :: RuleContent -> Bool -> IO UIOptions
mkUIOptions corule benchmark = do
  let cfgUIName = rcfgUIName corule
      (configString, cfgUIDefault) = rcfgUIDefault corule
  dataDir <- appDataDir
  let path bkp = dataDir </> bkp <> cfgUIName
  cfgUser <- if benchmark then return Ini.emptyConfig else do
    cpExists <- doesFileExist (path "")
    if not cpExists
      then return Ini.emptyConfig
      else do
        sUser <- readFile (path "")
        return $! either (configError . ("Ini.parse sUser" `showFailure`)) id
                  $ Ini.parse sUser
  let cfgUI = M.unionWith M.union cfgUser cfgUIDefault  -- user cfg preferred
      vExe1 = rexeVersion corule
      vExe2 =
        let optionName = "version"
            -- Lenient to parse, and reject, old config files:
            s = fromMaybe "" $ Ini.getOption "version" optionName cfgUser
            dummyVersion = makeVersion []
        in case find ((== "") . snd) $ readP_to_S parseVersion s of
          Just (ver, "") -> ver
          _ -> dummyVersion
  if benchmark || compatibleVersion vExe1 vExe2 then do
    let conf = parseConfig cfgUI
    -- Catch syntax errors in complex expressions ASAP.
    return $! deepseq conf conf
  else do
    cpExists <- doesFileExist (path "")
    let bkpOneSave name = do
          let pathSave bkp = dataDir </> "saves" </> bkp <> name
          b <- doesFileExist (pathSave "")
          when b $ renameFile (pathSave "") (pathSave "bkp.")
        bkpAllSaves = do
          bkpOneSave $ Save.saveNameSer corule
          forM_ [-199..199] $ \n ->
            bkpOneSave $ Save.saveNameCli corule (toEnum n)
    when cpExists $ do
      renameFile (path "") (path "bkp.")
      bkpAllSaves
      let msg = "Config file" <+> T.pack (path "")
                <+> "from an incompatible version '"
                <> T.pack (showVersion vExe2)
                <> "' detected while starting"
                <+> T.pack (showVersion vExe1)
                <+> "game. The config file and savefiles have been moved aside."
      delayPrint msg
    tryWriteFile (path "") configString
    let confDefault = parseConfig cfgUIDefault
    return confDefault

-- | Modify client options with UI options.
applyUIOptions :: COps -> UIOptions -> ClientOptions -> ClientOptions
applyUIOptions COps{corule} uioptions =
     (\opts -> opts {schosenFontset =
        schosenFontset opts `mplus` Just (uChosenFontset uioptions)}) .
     (\opts -> opts {sallFontsScale =
        sallFontsScale opts `mplus` Just (uAllFontsScale uioptions)}) .
     (\opts -> opts {sfonts = uFonts uioptions}) .
     (\opts -> opts {sfontsets = uFontsets uioptions}) .
     (\opts -> opts {sfullscreenMode =
        sfullscreenMode opts `mplus` Just (uFullscreenMode uioptions)}) .
     (\opts -> opts {smaxFps =
        smaxFps opts `mplus` Just (uMaxFps uioptions)}) .
     (\opts -> opts {snoAnim =
        snoAnim opts `mplus` Just (uNoAnim uioptions)}) .
     (\opts -> opts {stitle =
        stitle opts `mplus` Just (rtitle corule)})
