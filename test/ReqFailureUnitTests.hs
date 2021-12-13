module ReqFailureUnitTests (reqFailureUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemAspect
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.ItemKind
import           Game.LambdaHack.Core.Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

import qualified Content.RuleKind

reqFailureUnitTests :: TestTree
reqFailureUnitTests = testGroup "reqFailureUnitTests" $
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
      testItemFull = ItemFull
        { itemBase = Item
            { jkind = IdentityObvious (toEnum 667)
            , jfid = Nothing
            , jflavour = dummyFlavour
            }
        , itemKindId = toEnum 667
        , itemKind = testItemKind
        , itemDisco = ItemDiscoFull emptyAspectRecord
        , itemSuspect = True
        }
      standardRules = Content.RuleKind.standardRules
  in
  [ testCase "permittedApply: One Skill and x symbol -> FailureApplyFood" $
      permittedApply standardRules timeZero 1 True Nothing
                     testItemFull quantSingle
      @?= Left ApplyFood
  , testCase "permittedApply: One Skill and , symbol And CGround -> True" $
      permittedApply standardRules timeZero 1 True (Just CGround)
                     testItemFull {itemKind = testItemKind{isymbol = ','}}
                     quantSingle
      @?= Right True
  , testCase "permittedApply: One Skill and \" symbol -> True" $
      permittedApply standardRules timeZero 1 True Nothing
                     testItemFull {itemKind = testItemKind{isymbol = '"'}}
                     quantSingle
      @?= Right True
  , testCase "permittedApply: Two Skill and ? symbol -> FailureApplyRead" $
      permittedApply standardRules timeZero 2 True Nothing
                     testItemFull {itemKind = testItemKind{isymbol = '?'}}
                     quantSingle
      @?= Left ApplyRead
  , testCase "permittedApply: Two Skill and , symbol -> True" $
      permittedApply standardRules timeZero 2 True Nothing
                     testItemFull {itemKind = testItemKind{isymbol = ','}}
                     quantSingle
      @?= Right True
  ]
