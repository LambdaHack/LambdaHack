-- | Tests for local (client-side-only) human-command handlers, mostly
-- around the item-projection pipeline, plus the fling-dialog sibling of
-- the leader-desync family (see docs/leader-desync-bug.md, §09,
-- sibling bug (a)).
module HandleHumanLocalMUnitTests (handleHumanLocalMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Either (fromLeft)
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.MonadClient (getsClient)
import           Game.LambdaHack.Client.State (TGoal (..), Target (..), sleader)
import           Game.LambdaHack.Client.UI
  (SessionUI (..), modifySession, updateClientLeader)
import           Game.LambdaHack.Client.UI.Content.Screen (ScreenContent (..))
import           Game.LambdaHack.Client.UI.ContentClientUI (CCUI (..))
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.TutorialHints
import           Game.LambdaHack.Common.Actor (Actor (..))
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemAspect
import           Game.LambdaHack.Common.Kind (emptyMultiGroupItem)
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
  (CStore (..), ItemDialogMode (..))
import           Game.LambdaHack.Definition.DefsInternal
  (toContentId, toContentSymbol)

import UnitTestHelpers

-- Dialog prompts wrap via indentSplitAttrString, which asserts a screen
-- wider than 4, so tests that open real dialogs enlarge the stub screen
-- (the level can stay 3x3).
enlargeScreen :: SessionUI -> SessionUI
enlargeScreen sess = sess {sccui = (sccui sess)
  {coscreen = (coscreen (sccui sess)) {rwidth = 24, rheight = 12}}}

testItemFull :: ItemFull
testItemFull = ItemFull
  { itemBase = stubItem
  , itemKindId = toContentId 0
  , itemKind = emptyMultiGroupItem
  , itemDisco = ItemDiscoFull emptyAspectRecord
  , itemSuspect = False }

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
      let testFn =
            let triggerItems =
                  [ HumanCmd.TriggerItem
                      { tiverb = "verb"
                      , tiobject = "object"
                      , tisymbols = [toContentSymbol 'a', toContentSymbol 'b'] }
                  , HumanCmd.TriggerItem
                      { tiverb = "verb2"
                      , tiobject = "object2"
                      , tisymbols = [toContentSymbol 'c'] } ]
            in chooseItemProjectHuman testActorId triggerItems
      result <- executorCli testFn testCliStateWithItem
      showFailError (fromJust (fst result)) @?= "*aiming obstructed by terrain*"
  , testCase "tutorialHints-msg-in-history-report" $ do
      let testFn = do
            modifySession (\sess -> sess {scurTutorial = True})
              -- permit the client not to ignore tutorial hints
            tutorialHintMsgAdd CannotHarmYouInMelee
      result <- executorCli testFn testCliStateWithItem
      let maybeHistory = shistory <$> (cliSession . snd) result
      case maybeHistory of
        Nothing -> assertFailure "History is empty"
        Just history -> assertBool testFailureMsg isHintThere
         where
          renderedNewReports = reportToTexts . newReport $ history
          renderedHint = renderTutorialHints CannotHarmYouInMelee
          isHintThere = renderedHint `elem` renderedNewReports
          testFailureMsg = "Expected to find tutorial hint '"
            <> (T.unpack . renderTutorialHints $ CannotHarmYouInMelee)
            <> "' in SessionUI.shistory.newReport '"
            <> T.unpack (T.unlines renderedNewReports)
            <> "'"
  , testCase "psuitReq" $  do
      let testFn = psuitReq testActorId
      mpsuitReqMonad <- executorCli testFn testCliStateWithItem
      let mpsuitReq = fst mpsuitReqMonad
      case mpsuitReq of
        Left err -> do
          err @?= "aiming obstructed by terrain"
            -- TODO: I'd split the test into three tests, each taking
            -- a different branch and fail in the remaining two branches
            -- that the particular branch doesn't take. Here it takes
            -- the first branch, because unknown tiles are not walkable
            -- (regardless what I claimed previously) and so the player
            -- is surrounded by walls, basically, so aiming fails,
            -- because the projectiles wouldn't even leave the position
            -- of the actor. I think.
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
  , -- [LR-flip] Sibling bug (a) of the leader desync
    -- (docs/leader-desync-bug.md, §09): @chooseItemProjectHuman@ computes
    -- @psuitReq leader@ ONCE, at dialog entry, and bakes the resulting
    -- closure into the dialog's @psuit@; the fling dialog then permits
    -- switching the pointman (@maySwitchLeader@ is 'True' for stores), but
    -- the captured closure keeps judging items for the ENTRY actor. This
    -- pins the testable ingredient: the closure's verdict is actor-dependent
    -- (here via the projecting skill; position and calm differ the same
    -- way), so reusing the entry actor's closure after a switch reports
    -- wrong suitability/range for the new pointman. After the live-read design
    -- lands, the closure reads the live pointman at each evaluation
    -- (@psuitReq@ loses its ActorId argument), making the stale capture
    -- unrepresentable.
    testCase "fling suitability closure differs per actor (desync sibling)"
      $ do
      let skills = EM.fromList
            [ ( testActorId
              , Ability.addSk Ability.SkProject 1 Ability.zeroSkills )
            , (testActorId2, Ability.zeroSkills) ]
          cliS = partyCliState {cliState =
            updateActorMaxSkills (const skills) (cliState partyCliState)}
          testFn = do
            funA <- permittedProjectClient testActorId
            funC <- permittedProjectClient testActorId2
            return (funA testItemFull, funC testItemFull)
      (result, _) <- executorCli testFn cliS
      result @?= (Right True, Left ProjectUnskilled)

  , -- [LR-flip] Sibling bug (a) pinned at the exact captured value:
    -- @psuitReq@ -- what @chooseItemProjectHuman@ bakes into the dialog's
    -- @psuit@ -- gives a different verdict per actor through the real
    -- aiming pipeline, with no walkable tiles needed, because both
    -- verdicts are failures: with the xhair on C's own position, A's line
    -- to it is blocked by the (unwalkable, unknown) terrain, while C's
    -- line to itself is degenerate. The heroes are moved to row 0 first:
    -- the aiming pipeline indexes ltile via Point's Enum width hack
    -- (speedupHackXSize, still at its default 80 in the test binary), so
    -- on the 3x3 stub board only row-0 positions stay in bounds.
    -- After the live-read design lands, @psuitReq@ loses its ActorId
    -- argument and reads the live pointman, so the two calls below become
    -- one call before and one after a pointman switch.
    testCase "psuitReq verdict differs per actor (desync sibling, captured)"
      $ do
      let moveTo pos b = b {bpos = pos}
          cliS = partyCliState {cliState =
            updateActorD (EM.adjust (moveTo (Point 0 0)) testActorId
                          . EM.adjust (moveTo (Point 2 0)) testActorId2)
                         (cliState partyCliState)}
          testFn = do
            updateClientLeader testActorId
            modifySession $ \sess ->
              sess {sxhair = Just $ TPoint TUnknown testLevelId (Point 2 0)}
            resA <- psuitReq testActorId
            resC <- psuitReq testActorId2
            let verdict = fromLeft "suitability computed"
            return (verdict resA, verdict resC)
      (result, _) <- executorCli testFn cliS
      result @?= ( "aiming obstructed by terrain"
                 , "aiming blocked at the first step" )

  , -- [contract] The first test through the real dialog machinery:
    -- chooseItemHuman opens the equipment store dialog
    -- (chooseItemDialogMode -> getStoreItem -> displayChoiceScreen),
    -- which renders its frames and
    -- reads keys via promptGetKey -- answered ESC by the stub frontend --
    -- so the dialog exits with "never mind". Also pins promptGetKey's
    -- second engine call site (SlideshowM.hs:421). The screen (not the
    -- level) is enlarged first: dialog prompts wrap via
    -- indentSplitAttrString, which asserts a screen wider than 4.
    testCase "chooseItemHuman: ESC exits the real store dialog" $ do
      let giveItem b = b {beqp = EM.singleton testItemId (1, [])}
          cliS = partyCliState {cliState =
              updateItemD (EM.insert testItemId stubItem)
            $ updateActorD (EM.adjust giveItem testActorId)
            $ cliState partyCliState}
          testFn = do
            modifySession enlargeScreen
            updateClientLeader testActorId
            chooseItemHuman testActorId (MStore CEqp)
      (result, _) <- executorCli testFn cliS
      fmap showFailError result @?= Just "*never mind*"

  , -- [contract] The mid-dialog pointman switch through the REAL dialog
    -- machinery: a scripted Tab inside A's equipment-store dialog runs the
    -- dialog's own cycling handler (InventoryM's cycleKeyDef ->
    -- pointmanCycle, InventoryM.hs:431) and recCall re-enters the dialog
    -- for the new pointman C -- the §02 re-sync introduced by commit
    -- 8608d6f9c, pinned here for the first time. recCall re-enters
    -- transition (the key loop), not getFull, so C's store contents don't
    -- gate the re-entry: the script is now dry, the re-entered dialog reads
    -- ESC and exits with "never mind", and the switch to C sticks.
    -- Contract, not flip: the held leader is in sync when Tab fires, so the
    -- outcome survives the live-read design unchanged.
    testCase "chooseItemHuman: scripted Tab switches pointman mid-dialog"
      $ do
      cliS0 <- partyCliStateScripted [K.mkKM "Tab"]
      let giveItem b = b {beqp = EM.singleton testItemId (1, [])}
          cliS = cliS0 {cliState =
              updateItemD (EM.insert testItemId stubItem)
            $ updateActorD (EM.adjust giveItem testActorId)
            $ cliState cliS0}
          testFn = do
            modifySession enlargeScreen
            updateClientLeader testActorId
            merr <- chooseItemHuman testActorId (MStore CEqp)
            leaderAfter <- getsClient sleader
            return (fmap showFailError merr, leaderAfter)
      (result, _) <- executorCli testFn cliS
      result @?= (Just "*never mind*", Just testActorId2)
  ]
