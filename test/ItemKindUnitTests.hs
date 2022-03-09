module ItemKindUnitTests (itemKindUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Content.RuleKind
import           Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Content.RuleKind as RK
import qualified Game.LambdaHack.Definition.Ability as Ability
import Game.LambdaHack.Common.Kind (emptyMultiGroupItem)

itemKindUnitTests :: TestTree
itemKindUnitTests = testGroup "itemKindUnitTests" $
  let standardSymbols = RK.ritemSymbols Content.RuleKind.standardRules
      customSymbols = emptyItemSymbolsUsedInEngine {rsymbolNecklace = '*'}

  in
  [ testCase "overlonginame_validateSingle_errs" $
      validateSingle standardSymbols
                     emptyMultiGroupItem { iname = "123456789012345678901234" }
      @?= ["iname longer than 23"]
  , testCase "shortEnoughiname_validateSingle_noErr" $
      validateSingle standardSymbols
                     emptyMultiGroupItem
      @?= []
  , testCase "equipableNoSlotxSymbol_validateSingle_errs" $
      validateSingle standardSymbols
                     emptyMultiGroupItem { iaspects = [ SetFlag Ability.Equipable ] }
      @?= ["EqpSlot not specified but Equipable or Meleeable and not a likely organ or necklace or template"]
  , testCase "equipableNoSlot,Symbol_validateSingle_noErr" $
      validateSingle standardSymbols
                     emptyMultiGroupItem { isymbol = ','
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= []
  , testCase "equipableNoSlot\"Symbol_validateSingle_noErr" $
      validateSingle standardSymbols
                     emptyMultiGroupItem { isymbol = '"'
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= []
  , testCase "equipableNoSlot/Symbol_validateSingle_noErr" $
      validateSingle standardSymbols
                     emptyMultiGroupItem { isymbol = '/'
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= []
  , testCase "equipableNoSlot*CustomRules_validateSingle_noErr" $
      validateSingle customSymbols
                     emptyMultiGroupItem { isymbol = '*'
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= []
  , testCase "equipableNoSlot\"CustomRules_validateSingle_errs" $
      validateSingle customSymbols
                     emptyMultiGroupItem { isymbol = '"'
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= ["EqpSlot not specified but Equipable or Meleeable and not a likely organ or necklace or template"]
  ]
