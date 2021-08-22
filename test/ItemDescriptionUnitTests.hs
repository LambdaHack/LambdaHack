module ItemDescriptionUnitTests (itemDescriptionUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemAspect
-- import           Game.LambdaHack.Common.ReqFailure
-- import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Content.ItemKind
import           Game.LambdaHack.Core.Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color
-- import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

itemDescriptionUnitTests :: TestTree
itemDescriptionUnitTests = testGroup "itemDescriptionUnitTests" $
  let testItemBase = Item { jkind = IdentityObvious (toEnum 667)
            , jfid = Nothing
            , jflavour = dummyFlavour
            }
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
      testItemFull = ItemFull
        { itemBase = testItemBase
        , itemKindId = toEnum 667
        , itemKind = testItemKind
        , itemDisco = ItemDiscoFull emptyAspectRecord
        , itemSuspect = True
        }
  in
  [ testCase "testItem_viewItem_BlackX" $
      viewItem testItemFull @?= (attrChar2ToW32 Black 'x')
  , testCase "testItem!_viewItem_Black!" $
      viewItem testItemFull { itemKind = testItemKind { isymbol = '!' }}
      @?= (attrChar2ToW32 Black '!')
  ]
