module ItemRevUnitTests (itemRevUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Vector.Unboxed as U
import qualified System.Random.SplitMix32 as SM
import           Test.Tasty
import           Test.Tasty.HUnit

import Game.LambdaHack.Common.Kind (emptyMultiGroupItem)
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Random
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.DefsInternal
import Game.LambdaHack.Definition.Flavour
import Game.LambdaHack.Server.ItemRev

itemRevUnitTests :: TestTree
itemRevUnitTests = testGroup "itemRevUnitTests" $
  let testItemKind2Flavours = emptyMultiGroupItem
        { iflavour = zipStory [Black,Green] }
      emptyIdToFlavourSymbolToFlavourSetPair = ( EM.empty, EM.empty )
      singletonIdToFlavourSymbolToFlavourSetPair =
        ( EM.singleton (toContentId 0) dummyFlavour
        , EM.singleton 'x' (ES.singleton dummyFlavour) )
      flavourBlack = head $ zipStory [Black]
      flavourGreen = head $ zipStory [Green]
  in
  [ testCase "empty & default initializers -> first is single dummy result" $
      let rndMapPair0 = return emptyIdToFlavourSymbolToFlavourSetPair
          mapPair1 = St.evalState (rollFlavourMap U.empty rndMapPair0 (toContentId 0) emptyMultiGroupItem) $ SM.mkSMGen 1
        in fst mapPair1 @?= EM.singleton (toContentId 0) dummyFlavour
  , testCase "empty & default initializers -> second is empty" $
      let rndMapPair0 = return emptyIdToFlavourSymbolToFlavourSetPair
          (mapPair1, _) = St.runState (rollFlavourMap U.empty rndMapPair0 (toContentId 0) emptyMultiGroupItem) $ SM.mkSMGen 1
        in snd mapPair1 @?= EM.empty
  , testCase "singleton initializers -> first is single dummy result" $
      let rndMapPair0 = return singletonIdToFlavourSymbolToFlavourSetPair
          (mapPair1, _) = St.runState (rollFlavourMap U.empty rndMapPair0 (toContentId 0) emptyMultiGroupItem) $ SM.mkSMGen 1
        in fst mapPair1 @?= EM.singleton (toContentId 0) dummyFlavour
  , testCase "singleton initializers -> second is single dummy result" $
      let rndMapPair0 = return singletonIdToFlavourSymbolToFlavourSetPair
          (mapPair1, _) = St.runState (rollFlavourMap U.empty rndMapPair0 (toContentId 0) emptyMultiGroupItem) $ SM.mkSMGen 1
        in snd mapPair1 @?= EM.singleton 'x' (ES.singleton dummyFlavour)
  , testCase "rollFlavourMap on two flavours -> first flavour can be rolled" $  -- relies on us not messing with RNG
      let rndMapPair0 = return singletonIdToFlavourSymbolToFlavourSetPair
          (mapPair1, _) = St.runState (rollFlavourMap (U.singleton invalidInformationCode) rndMapPair0 (toContentId 0) testItemKind2Flavours) $ SM.mkSMGen 1
        in fst mapPair1 @?= EM.singleton (toContentId 0) flavourBlack
  , testCase "rollFlavourMap on two flavours -> second flavour can be rolled" $  -- relies on us not messing with RNG
      let rndMapPair0 = return singletonIdToFlavourSymbolToFlavourSetPair
          (mapPair1, _) = St.runState (rollFlavourMap (U.singleton invalidInformationCode) rndMapPair0 (toContentId 0) testItemKind2Flavours) $ SM.mkSMGen 2
        in fst mapPair1 @?= EM.singleton (toContentId 0) flavourGreen
  ]
