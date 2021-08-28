module HandleHelperMUnitTests (handleHelperMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemAspect
import           Game.LambdaHack.Content.ItemKind
import           Game.LambdaHack.Core.Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color
import           Game.LambdaHack.Definition.Flavour

handleHelperMUnitTests :: TestTree
handleHelperMUnitTests = testGroup "handleHelperMUnitTests" $
  let testItemBag = singleton 1 1  -- I think the compiler is looking at the wrong singleton, not the one for Map here
      testSingleItemSlots = singleton 'a' 1
      --testMonadClientUI = -- umm...do I need something like this?
      expectedOKX = (empty, []) -- I don't know what to expect yet
  in
  [ testCase "itemOverlay" $
      okxval @?= expectedOKX
      --where (m okxval) = itemOverlay testSingleItemSlots (LevelId 2) testItemBag false  -- do I need to pass a MonadClientUI in here somehow?
  ]
