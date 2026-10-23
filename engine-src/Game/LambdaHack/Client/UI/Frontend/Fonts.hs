-- | The chosen fontset's fonts, resolved from the client options in one place
-- for every frontend that draws with fonts: SDL loads them, the browser is
-- handed them, and the UI asks whether there are auxiliary fonts to lay its
-- texts out in.
module Game.LambdaHack.Client.UI.Frontend.Fonts
  ( ChosenFont(..), ChosenFonts(..), resolveFonts
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Misc

-- | A font as a frontend loads it: its definition in the config file, its
-- file, and its size in pixels with @allFontsScale@ applied, 0 for
-- a bitmap font, whose size its file fixes.
data ChosenFont = ChosenFont
  { cfDefinition :: FontDefinition
  , cfFile       :: Text
  , cfSize       :: Int
  }
  deriving (Show, Eq)

-- | The map font, whose size sizes the cell, and the three auxiliary
-- fonts, proportional regular, proportional bold and monospace, all three
-- or none.
data ChosenFonts = ChosenFonts
  { cfMap       :: ChosenFont
  , cfAuxiliary :: Maybe (ChosenFont, ChosenFont, ChosenFont)
  }
  deriving (Show, Eq)

-- | Resolve the chosen fontset, or say why it cannot be. At scale 1 the
-- map font is the bitmap one where the fontset names one, else the
-- scalable one; at any other scale it is the scalable one.
resolveFonts :: ClientOptions -> Either String ChosenFonts
resolveFonts ClientOptions{schosenFontset, sfontsets, sfonts, sallFontsScale} =
 do
  fontsetID <- maybe (Left "No fontset chosen") Right schosenFontset
  fontset <- maybe (Left $ "Fontset not defined in config file"
                           `showFailure` fontsetID)
                   Right
                   (lookup fontsetID sfontsets)
  scale <- maybe (Left "No allFontsScale set") Right sallFontsScale
  let lookupFont t
        | T.null t = Right Nothing
        | otherwise = case lookup t sfonts of
            Nothing -> Left $ "Font not defined in config file"
                              `showFailure` t
            Just def -> Right $ Just $ chosen def
      chosen def =
        let (file, size) = case def of
              FontProportional fname fsize _ -> (fname, fsize)
              FontMonospace fname fsize _ -> (fname, fsize)
              FontMapScalable fname fsize _ _ -> (fname, fsize)
              FontMapBitmap fname _ -> (fname, 0)
        in ChosenFont def file (round $ scale * intToDouble size)
      scalable = lookupFont (fontMapScalable fontset)
  cfMap <-
    if scale == 1.0
    then lookupFont (fontMapBitmap fontset) >>= \case
      Just font -> Right font
      Nothing -> scalable >>= maybe
        (Left "Neither bitmap nor scalable map font defined") Right
    else scalable >>= maybe
      (Left "Scaling requested but scalable map font not defined") Right
  auxiliary <- (,,) <$> lookupFont (fontPropRegular fontset)
                    <*> lookupFont (fontPropBold fontset)
                    <*> lookupFont (fontMono fontset)
  cfAuxiliary <- case auxiliary of
    (Just prop, Just bold, Just mono) -> Right $ Just (prop, bold, mono)
    (Nothing, Nothing, Nothing) -> Right Nothing
    _ -> Left $ "Either all auxiliary fonts should be defined or none"
                `showFailure` fontset
  return ChosenFonts{..}
