module ItemKindUnitTests (itemKindUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Content.RuleKind as RK
import           Game.LambdaHack.Core.Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color
import           Game.LambdaHack.Definition.Flavour

import qualified Content.RuleKind

itemKindUnitTests :: TestTree
itemKindUnitTests = testGroup "itemKindUnitTests" $
  let standardSymbols = RK.ritemSymbols Content.RuleKind.standardRules
      customSymbols = emptyItemSymbolsUsedInEngine {rsymbolNecklace = '*'}
      testItemKind = ItemKind
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
  in
  [ testCase "overlonginame_validateSingle_errs" $
      validateSingle standardSymbols
                     testItemKind { iname = "123456789012345678901234" }
      @?= ["iname longer than 23"]
  , testCase "shortEnoughiname_validateSingle_noErr" $
      validateSingle standardSymbols
                     testItemKind
      @?= []
  , testCase "equipableNoSlotxSymbol_validateSingle_errs" $
      validateSingle standardSymbols
                     testItemKind { iaspects = [ SetFlag Ability.Equipable ] }
      @?= ["EqpSlot not specified but Equipable or Meleeable and not a likely organ or necklace or template"]
  , testCase "equipableNoSlot,Symbol_validateSingle_noErr" $
      validateSingle standardSymbols
                     testItemKind { isymbol = ','
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= []
  , testCase "equipableNoSlot\"Symbol_validateSingle_noErr" $
      validateSingle standardSymbols
                     testItemKind { isymbol = '"'
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= []
  , testCase "equipableNoSlot/Symbol_validateSingle_noErr" $
      validateSingle standardSymbols
                     testItemKind { isymbol = '/'
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= []
  , testCase "equipableNoSlot*CustomRules_validateSingle_noErr" $
      validateSingle customSymbols
                     testItemKind { isymbol = '*'
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= []
  , testCase "equipableNoSlot\"CustomRules_validateSingle_errs" $
      validateSingle customSymbols
                     testItemKind { isymbol = '"'
                                  , iaspects = [ SetFlag Ability.Equipable ] }
      @?= ["EqpSlot not specified but Equipable or Meleeable and not a likely organ or necklace or template"]
  ]
