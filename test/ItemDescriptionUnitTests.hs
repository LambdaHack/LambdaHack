module ItemDescriptionUnitTests (itemDescriptionUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Test.Tasty
import           Test.Tasty.HUnit

import Game.LambdaHack.Client.UI.ItemDescription
  (viewItem, viewItemBenefitColored)
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemAspect (emptyAspectRecord)
import Game.LambdaHack.Common.Kind (emptyMultiGroupItem)
import Game.LambdaHack.Content.ItemKind (ItemKind (isymbol))
import Game.LambdaHack.Definition.Color
  (Color (BrGreen, BrRed, Green), attrChar2ToW32)
import Game.LambdaHack.Definition.Flavour (zipPlain)
import UnitTestHelpers (stubItem)

itemDescriptionUnitTests :: TestTree
itemDescriptionUnitTests = testGroup "itemDescriptionUnitTests" $
  let testItemFull = ItemFull
        { itemBase = stubItem
          {
            jkind = IdentityObvious (toEnum 667)
          , jflavour = head $ zipPlain [Green]
          }
        , itemKindId = toEnum 667
        , itemKind = emptyMultiGroupItem
        , itemDisco = ItemDiscoFull emptyAspectRecord
        , itemSuspect = True
        }
  in
  [ testCase "testItem_viewItem_Blackx" $
      viewItem testItemFull {itemKind = emptyMultiGroupItem {isymbol = 'x'}}
      @?= attrChar2ToW32 Green 'x'
  , testCase "testItem_viewItem_Black!" $
      viewItem testItemFull {itemKind = emptyMultiGroupItem {isymbol = '!'}}
      @?= attrChar2ToW32 Green '!'
  , testCase "testItem_viewItemBenefitColored_isEquip_Greenx" $
      viewItemBenefitColored (EM.singleton (toEnum 42) (Benefit True 0 0 0 0)) (toEnum 42) testItemFull {itemKind = emptyMultiGroupItem {isymbol = 'x'}}
      @?= attrChar2ToW32 BrGreen 'x'
  , testCase "testItem_viewItemBenefitColored_isNotEquip_Redx" $
      viewItemBenefitColored (EM.singleton (toEnum 42) (Benefit False 0 0 0 0)) (toEnum 42) testItemFull {itemKind = emptyMultiGroupItem {isymbol = 'x'}}
      @?= attrChar2ToW32 BrRed 'x'
  ]
