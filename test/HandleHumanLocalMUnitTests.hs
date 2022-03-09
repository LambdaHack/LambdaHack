module HandleHumanLocalMUnitTests (handleHumanLocalMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Test.Tasty
import           Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemAspect
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Content.TileKind
import           Game.LambdaHack.Definition.DefsInternal
  (toContentId, toContentSymbol)

import Game.LambdaHack.Common.Kind (emptyMultiGroupItem)
import UnitTestHelpers

testItemFull :: ItemFull
testItemFull = ItemFull { itemBase = stubItem, itemKindId = toContentId 0, itemKind = emptyMultiGroupItem, itemDisco = ItemDiscoFull emptyAspectRecord, itemSuspect = False }

handleHumanLocalMUnitTests :: TestTree
handleHumanLocalMUnitTests = testGroup "handleHumanLocalMUnitTests"
  [ testCase "verify stubLevel has tile element" $
      case EM.lookup testLevelId (sdungeon stubState) of
        Nothing -> assertFailure "stubLevel lost in dungeon"
        Just level -> ltile level ! Point 0 0 @?= unknownId
  , testCase "verify stubCliState has actor" $
      getActorBody testActorId (cliState stubCliState) @?= testActor
  , testCase "permittedProjectClient stubCliState returns ProjectUnskilled" $ do
      let testFn = permittedProjectClient testActorId
      permittedProjectClientResultFnInMonad <- executorCli testFn stubCliState
      let ultimateResult =
            fst permittedProjectClientResultFnInMonad testItemFull
      ultimateResult @?= Left ProjectUnskilled
  , testCase "chooseItemProjectHuman" $ do
      let testFn = let triggerItems =
                         [ HumanCmd.TriggerItem {tiverb = "verb", tiobject = "object", tisymbols = [toContentSymbol 'a', toContentSymbol 'b']}
                         , HumanCmd.TriggerItem {tiverb = "verb2", tiobject = "object2", tisymbols = [toContentSymbol 'c']}
                         ]
                   in chooseItemProjectHuman testActorId triggerItems
      result <- executorCli testFn testCliStateWithItem
      showFailError (fromJust (fst result)) @?= "*aiming obstructed by terrain*"
  , testCase "psuitReq" $  do
      let testFn = psuitReq testActorId
      mpsuitReqMonad <- executorCli testFn testCliStateWithItem
      let mpsuitReq = fst mpsuitReqMonad
      case mpsuitReq of
        Left err -> do
          err @?= "aiming obstructed by terrain"
            -- TODO: I'd split the test into three tests, each taking a different branch and fail in the remaining two branches that the particular branch doesn't take. Here it takes the first branch, because unknown tiles are not walkable (regardless what I claimed previously) and so the player is surrounded by walls, basically, so aiming fails, because the projectiles wouldn't even leave the position of the actor. I think.
        Right psuitReqFun ->
          case psuitReqFun testItemFull of
            Left reqFail -> do
              reqFail @?= ProjectUnskilled
            Right (pos, _) -> do
              pos @?= Point 0 0
  , testCase "xhairLegalEps" $ do
      let testFn = xhairLegalEps testActorId
      result <- executorCli testFn testCliStateWithItem
      fst result @?= Right 114  -- not a coincidence this matches testFactionId,
                                -- because @eps@ is initialized that way,
                                -- for "randomness"
  ]
