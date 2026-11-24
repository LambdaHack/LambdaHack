-- | Tests of 'validateSingle', the check of a single tile kind.
module TileKindUnitTests (tileKindUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Definition.Color (defFG)

tileKindUnitTests :: TestTree
tileKindUnitTests = testGroup "tileKindUnitTests"
  [ testCase "OftenItem and NoItem on a tile are refused once" $
      validateSingle (tileWith [OftenItem, NoItem])
      @?= ["OftenItem and NoItem on a tile"]
  , testCase "OftenActor and NoActor on a tile are refused" $
      validateSingle (tileWith [OftenActor, NoActor])
      @?= ["OftenActor and NoActor on a tile"]
  ]
 where
  tileWith tfeature = TileKind { tsymbol = '.'
                               , tname = "test tile"
                               , tfreq = []
                               , tcolor = defFG
                               , tcolor2 = defFG
                               , talter = 0
                               , tfeature }
