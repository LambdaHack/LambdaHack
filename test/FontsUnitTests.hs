-- | Tests of 'resolveFonts', the fonts every frontend that draws with fonts
-- takes from the client options.
module FontsUnitTests (fontsUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Either (isLeft)
import Test.Tasty
import Test.Tasty.HUnit

import Game.LambdaHack.Client.UI.Frontend
  (ChosenFont (..), ChosenFonts (..), resolveFonts)
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Misc

fontsUnitTests :: TestTree
fontsUnitTests = testGroup "fontsUnitTests"
  [ testCase "at scale 1 the map font is the bitmap one, at its own size" $
      mapFont options @?= Right ("b.fon", 0)
  , testCase "at scale 1 without a bitmap font it is the scalable one" $
      mapFont (withFontset fontset {fontMapBitmap = ""} options)
        @?= Right ("s.ttf", 16)
  , testCase "at another scale it is the scalable one, scaled" $
      mapFont options {sallFontsScale = Just 1.5} @?= Right ("s.ttf", 24)
  , testCase "the auxiliary fonts are scaled and rounded as SDL loads them" $
      auxiliary options {sallFontsScale = Just 1.5}
        @?= Right (Just [("p.ttf", 21), ("pb.ttf", 21), ("m.ttf", 16)])
  , testCase "a fontset naming no auxiliary font has none" $
      auxiliary (withFontset fontset { fontPropRegular = ""
                                     , fontPropBold = ""
                                     , fontMono = "" } options)
        @?= Right Nothing
  , testCase "a fontset naming some auxiliary fonts and not all is refused" $
      isLeft (resolveFonts $ withFontset fontset {fontMono = ""} options)
        @? "resolved"
  , testCase "a fontset the config does not define is refused" $
      isLeft (resolveFonts options {schosenFontset = Just "absent"})
        @? "resolved"
  , testCase "a font the config does not define is refused" $
      isLeft (resolveFonts options {sfonts = filter ((/= "mono") . fst)
                                                    (sfonts options)})
        @? "resolved"
  , testCase "no fontset chosen is refused" $
      isLeft (resolveFonts defClientOptions) @? "resolved"
  ]
 where
  fileAndSize ChosenFont{..} = (cfFile, cfSize)
  mapFont = fmap (fileAndSize . cfMap) . resolveFonts
  auxiliary = fmap (fmap (\(p, b, m) -> map fileAndSize [p, b, m])
                    . cfAuxiliary) . resolveFonts
  withFontset fs o = o {sfontsets = [("set", fs)]}

fontset :: FontSet
fontset = FontSet { fontMapScalable = "scalable"
                  , fontMapBitmap = "bitmap"
                  , fontPropRegular = "prop"
                  , fontPropBold = "bold"
                  , fontMono = "mono" }

options :: ClientOptions
options = defClientOptions
  { schosenFontset = Just "set"
  , sallFontsScale = Just 1.0
  , sfontsets = [("set", fontset)]
  , sfonts = [ ("scalable", FontMapScalable "s.ttf" 16 HintingHeavy 2)
             , ("bitmap", FontMapBitmap "b.fon" 1)
             , ("prop", FontProportional "p.ttf" 14 HintingLight)
             , ("bold", FontProportional "pb.ttf" 14 HintingLight)
             , ("mono", FontMonospace "m.ttf" 11 HintingHeavy) ]
  }
