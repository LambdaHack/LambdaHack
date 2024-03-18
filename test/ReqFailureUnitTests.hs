module ReqFailureUnitTests (reqFailureUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Content.RuleKind
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemAspect
import           Game.LambdaHack.Common.Kind (emptyMultiGroupItem)
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.ItemKind
import           Game.LambdaHack.Definition.Defs
import           UnitTestHelpers (stubItem)

reqFailureUnitTests :: TestTree
reqFailureUnitTests = testGroup "reqFailureUnitTests" $
  let testItemFull = ItemFull
        { itemBase = stubItem -- Item { jkind = IdentityObvious (toEnum 667) , jfid = Nothing , jflavour = dummyFlavour}
        , itemKindId = toEnum 667
        , itemKind = emptyMultiGroupItem
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
                     testItemFull {itemKind = emptyMultiGroupItem{isymbol = ','}}
                     quantSingle
      @?= Right True
  , testCase "permittedApply: One Skill and \" symbol -> True" $
      permittedApply standardRules timeZero 1 True Nothing
                     testItemFull {itemKind = emptyMultiGroupItem{isymbol = '"'}}
                     quantSingle
      @?= Right True
  , testCase "permittedApply: Two Skill and ? symbol -> FailureApplyRead" $
      permittedApply standardRules timeZero 2 True Nothing
                     testItemFull {itemKind = emptyMultiGroupItem{isymbol = '?'}}
                     quantSingle
      @?= Left ApplyRead
  , testCase "permittedApply: Two Skill and , symbol -> True" $
      permittedApply standardRules timeZero 2 True Nothing
                     testItemFull {itemKind = emptyMultiGroupItem{isymbol = ','}}
                     quantSingle
      @?= Right True
  ]
