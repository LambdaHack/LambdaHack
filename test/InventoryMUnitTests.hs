-- TODO: at some point we'll want our unit test hierarchy to match the
-- main codebase file hierarchy
module InventoryMUnitTests (inventoryMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Exception (SomeException, evaluate, try)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI
  (SessionUI (..), modifySession, updateClientLeader)
import           Game.LambdaHack.Client.UI.InventoryM
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.SessionUI (ItemRoles (..))
import           Game.LambdaHack.Common.Actor (Actor (..))
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Definition.Defs

import UnitTestHelpers

inventoryMUnitTests :: TestTree
inventoryMUnitTests = testGroup "inventoryMUnitTests"
  [ testCase "getFull no stores " $ do
      let testFn = getFull testActorId
                           (return SuitsEverything)  -- :: m Suitability
                           (\_ _ _ _ _ -> "specific prompt")
                           (\_ _ _ _ _ -> "generic prompt")
                           []  -- :: [CStore]
                           False
                           False
      result <- executorCli testFn stubCliState
      fst result @?= Left "no items"
  , testCase "getFull no item in eqp store" $ do
      let testFn = getFull testActorId
                           (return SuitsEverything)
                           (\_ _ _ _ _ -> "specific prompt")
                           (\_ _ _ _ _ -> "generic prompt")
                           [CEqp]
                           False
                           False
      result <- executorCli testFn stubCliState
      fst result @?= Left "no items in equipment outfit"
  , testCase "getFull an item in eqp store" $ do
      let testFn = getFull testActorId
                           (return SuitsEverything)
                           (\_ _ _ _ _ -> "specific prompt")
                           (\_ _ _ _ _ -> "generic prompt")
                           [CEqp]
                           False
                           False
      result <- executorCli testFn testCliStateWithItem
      fst result @?= Right (CEqp, [(testItemId, (1, []))])

  , -- [LR-flip] The bag @getFull@ looks the chosen item up in is closed
    -- over the actor the dialog *opened* on (InventoryM.hs:264-265), while
    -- the dialog itself follows the pointman: a scripted Tab switches to C
    -- mid-dialog (@maySwitchLeader MStore = True@) and recCall re-enters
    -- @transition@ for C, so RET picks an item from C's equipment -- which
    -- A does not have. The identity of that item comes back intact, but
    -- its quantity is @bagAll EM.! iid@ against A's bag
    -- (InventoryM.hs:290-292), so forcing it dies with a missing key
    -- rather than merely describing the wrong actor's item.
    -- This is not the identity going stale but a value *derived* from it
    -- before the wait, which live-read fixes only if the derivation moves
    -- down with the read: see docs/leader-desync-bug.md §10.3 and the
    -- placement table in docs/leader-desync-migration.md §03.
    -- Why no player has hit it: both single-item callers drop the quantity
    -- unforced -- @getGroupItem@ matches @[(iid, _)]@ -- and the move
    -- family's single-item path guards the same case with @EM.lookup@ and
    -- a comment naming it ("selection from another actor",
    -- HandleHumanGlobalM.hs:775-777); @moveItems@, reached with several
    -- items or from the ground, is what forces the thunk.
    -- After live-read this returns @Right (CEqp, [(testItemId2, (1, []))])@
    -- whole: C's item, C's quantity, nothing to catch.
    -- Proved non-vacuous in both halves (2026-07-30): give C the same item
    -- as A and the identity assertion fails, @ItemId 113@ coming back, so
    -- the scripted switch is what puts C's item there; give A both items
    -- and the forcing assertion fails with "got quantity 2", so the
    -- exception caught below is this lookup and not an incidental one.
    testCase "LR-flip getFull looks the chosen item up in the entry actor's bag"
      $ do
      let giveA b = b {beqp = EM.singleton testItemId (1, [])}
          giveC b = b {beqp = EM.singleton testItemId2 (1, [])}
          seeItems sess =
            let ItemRoles roles = sroles sess
            in sess {sroles = ItemRoles
                     $ EM.adjust (ES.insert testItemId . ES.insert testItemId2)
                                 SItem roles}
            -- the dialog lists only items given a role, however full a bag
      cliS0 <- scriptedCliState partyCliStateWalkable
                                [K.mkKM "Tab", K.returnKM]
      let cliS = cliS0
            { cliState = updateItemD (EM.insert testItemId stubItem
                                      . EM.insert testItemId2 stubItem)
                         $ updateActorD (EM.adjust giveA testActorId
                                         . EM.adjust giveC testActorId2)
                         $ cliState cliS0
            , cliSession = seeItems <$> cliSession cliS0 }
          testFn = do
            modifySession enlargeScreenForItems
            initBfsTabs
            updateClientLeader testActorId
            getFull testActorId
                    (return SuitsEverything)
                    (\_ _ _ _ _ -> "specific prompt")
                    (\_ _ _ _ _ -> "generic prompt")
                    [CEqp]
                    True   -- @askWhenLone@: ask even for a lone item, so
                    False  -- the dialog opens at all -- and these are the
                           -- flags @getGroupItem@ itself passes
                           -- (InventoryM.hs:117), not a pair chosen here
      (result, _) <- executorCli testFn cliS
      -- The identity is C's, and reading it forces nothing:
      case result of
        Right (store, [(iid, _)]) -> (store, iid) @?= (CEqp, testItemId2)
        _ -> assertFailure "the dialog did not return a single chosen item"
      -- ... but the quantity beside it was looked up in A's bag:
      forced <- try $ evaluate $ case result of
        Right (_, [(_, (k, _))]) -> k
        _ -> 0
      case (forced :: Either SomeException Int) of
        Left e -> assertBool ("expected the missing-key failure, got: "
                              ++ show e)
                             ("is not an element of the map" `isInfixOf` show e)
          -- the message is @IntMap.!: key 117 is not an element of the map@,
          -- 117 being C's item; asserting on it rather than on "something
          -- threw" is what keeps this test from passing for a wrong reason
        Right k -> assertFailure $
          "expected A's bag to miss C's item; got quantity " ++ show k
  ]
