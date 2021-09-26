module ItemRevUnitTests (itemRevUnitTests) where

import Prelude ()


import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Vector.Unboxed as U

import Test.Tasty
import Test.Tasty.HUnit

-- import           Game.LambdaHack.Client.UI.ItemDescription
-- import           Game.LambdaHack.Common.ItemAspect

import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Content.ItemKind
import           Game.LambdaHack.Core.Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color
import           Game.LambdaHack.Definition.Flavour
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Server.ItemRev

itemRevUnitTests :: TestTree
itemRevUnitTests = testGroup "itemRevUnitTests" $
  [
  let testItemKind = ItemKind
        { isymbol  = 'x'
        , iname    = "12345678901234567890123"
        , ifreq    = [ (UNREPORTED_INVENTORY, 1) ]
        , iflavour = zipPlain [Green]
        , icount   = 1 + 1 `d` 2
        , irarity  = [(1, 50), (10, 1)]
        , iverbHit = "hit"
        , iweight  = 300
        , idamage  = 1 `d` 1
        , iaspects = [ AddSkill Ability.SkHurtMelee $ -16 * 5
                     , SetFlag Ability.Fragile
                     , toVelocity 70 ]
        , ieffects = []
        , idesc    = "A really cool test item."
        , ikit     = []
        }
      emptyIdToFlavourSymbolToFlavourSetPair = ( EM.empty, EM.empty )
  in testCase "rollFlavourMap_xxx_xxx" $
    do 
      let rndMapPair0 = return emptyIdToFlavourSymbolToFlavourSetPair
      (idToFlavourMap, symbolToFlavourSetMap) <- rollFlavourMap U.empty rndMapPair0 0 testItemKind 
      idToFlavourMap @?= EM.empty
      symbolToFlavourSetMap @?= EM.empty
    --  testCase "dungeonFlavourMap_emptyFlavourMap_isEmpty" $
    --   dungeonFlavourMap emptyCOps emptyFlavourMap
    --   @?= Rnd emptyFlavourMap 
  ]
