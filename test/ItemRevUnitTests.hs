module ItemRevUnitTests (itemRevUnitTests) where

import Prelude ()


import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Vector.Unboxed as U
import qualified System.Random.SplitMix32 as SM

import Test.Tasty
import Test.Tasty.HUnit

-- import           Game.LambdaHack.Client.UI.ItemDescription
-- import           Game.LambdaHack.Common.ItemAspect

import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Content.ItemKind
import           Game.LambdaHack.Core.Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color
import           Game.LambdaHack.Definition.DefsInternal
import           Game.LambdaHack.Definition.Flavour
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Server.ItemRev

itemRevUnitTests :: TestTree
itemRevUnitTests = testGroup "itemRevUnitTests" $
  let testItemKind = ItemKind
        { isymbol  = 'x'
        , iname    = "12345678901234567890123"
        , ifreq    = [ (UNREPORTED_INVENTORY, 1) ]
        , iflavour = zipStory [Black]
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
      singletonIdToFlavourSymbolToFlavourSetPair = ( EM.singleton (toContentId 0) dummyFlavour, EM.singleton 'x' (ES.singleton dummyFlavour) )
  in
  [ testCase "empty & default initializers -> first is single dummy result" $
      let rndMapPair0 = return emptyIdToFlavourSymbolToFlavourSetPair
          (mapPair1, _) = St.runState (rollFlavourMap U.empty rndMapPair0 (toContentId 0) testItemKind) $ SM.mkSMGen 1
        in fst mapPair1 @?= EM.singleton (toContentId 0) dummyFlavour
  , testCase "empty & default initializers -> second is empty" $
      let rndMapPair0 = return emptyIdToFlavourSymbolToFlavourSetPair
          (mapPair1, _) = St.runState (rollFlavourMap U.empty rndMapPair0 (toContentId 0) testItemKind) $ SM.mkSMGen 1
        in snd mapPair1 @?= EM.empty
  , testCase "singleton initializers -> first is single dummy result" $
      let rndMapPair0 = return singletonIdToFlavourSymbolToFlavourSetPair
          (mapPair1, _) = St.runState (rollFlavourMap U.empty rndMapPair0 (toContentId 0) testItemKind) $ SM.mkSMGen 1
        in fst mapPair1 @?= EM.singleton (toContentId 0) dummyFlavour
  , testCase "singleton initializers -> second is single dummy result" $
      let rndMapPair0 = return singletonIdToFlavourSymbolToFlavourSetPair
          (mapPair1, _) = St.runState (rollFlavourMap U.empty rndMapPair0 (toContentId 0) testItemKind) $ SM.mkSMGen 1
        in snd mapPair1 @?= EM.singleton 'x' (ES.singleton dummyFlavour)
      
    --  testCase "dungeonFlavourMap_emptyFlavourMap_isEmpty" $
    --   dungeonFlavourMap emptyCOps emptyFlavourMap
    --   @?= Rnd emptyFlavourMap 
  ]
