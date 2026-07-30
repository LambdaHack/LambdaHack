-- | Tests for local (client-side-only) human-command handlers, mostly
-- around the item-projection pipeline, plus the fling-dialog sibling of
-- the leader-desync family (see docs/leader-desync-bug.md, §09,
-- sibling bug (a)).
module HandleHumanLocalMUnitTests (handleHumanLocalMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Either (fromLeft)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.MonadClient (getsClient)
import           Game.LambdaHack.Client.State (TGoal (..), Target (..), sleader)
import           Game.LambdaHack.Client.UI
  ( MonadClientUI
  , SessionUI (..)
  , getsSession
  , modifySession
  , updateClientLeader
  )
import           Game.LambdaHack.Client.UI.EffectDescription
  (defaultDetailLevel)
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.SessionUI
  (AimMode (..), ItemRoles (..))
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
  (CStore (..), ItemDialogMode (..), SLore (..))
import           Game.LambdaHack.Definition.DefsInternal
  (toContentId, toContentSymbol)

import UnitTestHelpers

testItemFull :: ItemFull
testItemFull = ItemFull
  { itemBase = stubItem
  , itemKindId = toContentId 0
  , itemKind = emptyMultiGroupItem
  , itemDisco = ItemDiscoFull emptyAspectRecord
  , itemSuspect = False }

-- The walkable-board party with one skill set shared by both heroes, for
-- the aiming branches the unknown board cannot reach (see 'walkableLevel').
walkableParty :: Ability.Skills -> CliState
walkableParty skills = partyCliStateWalkable {cliState =
  updateActorMaxSkills
    (const $ EM.fromList [(testActorId, skills), (testActorId2, skills)])
    (cliState partyCliStateWalkable)}

-- Aim at C's position, one walkable step from A.
aimAtHeroC :: MonadClientUI m => m ()
aimAtHeroC = modifySession $ \sess ->
  sess {sxhair = Just $ TPoint TUnknown testLevelId (bpos heroC)}

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
  , -- psuitReq's three outcomes, one test each, each failing if any other
    -- outcome is taken. Until the walkable board existed, only the first
    -- was reachable: the stub board's unknown tiles are not walkable, so
    -- the actor is walled in and a projectile wouldn't leave its position.
    testCase "psuitReq: unwalkable board obstructs aiming" $ do
      let testFn = psuitReq testActorId
      (result, _) <- executorCli testFn testCliStateWithItem
      case result of
        Left err -> err @?= "aiming obstructed by terrain"
        Right psuitReqFun -> assertFailure $ "expected a failed aim, got: "
          ++ show (psuitReqFun testItemFull)
  , testCase "psuitReq: walkable board, unskilled actor" $ do
      let testFn = aimAtHeroC >> psuitReq testActorId
      (result, _) <- executorCli testFn (walkableParty Ability.zeroSkills)
      case result of
        Left err -> assertFailure $ "expected a suitability function, got: "
          ++ T.unpack err
        Right psuitReqFun -> case psuitReqFun testItemFull of
          Left reqFail -> reqFail @?= ProjectUnskilled
          Right posRange -> assertFailure $ "expected an unskilled actor, got: "
            ++ show posRange
  , testCase "psuitReq: walkable board, skilled actor" $ do
      let projSk = Ability.addSk Ability.SkProject 1 Ability.zeroSkills
          testFn = aimAtHeroC >> psuitReq testActorId
      (result, _) <- executorCli testFn (walkableParty projSk)
      case result of
        Left err -> assertFailure $ "expected a suitability function, got: "
          ++ T.unpack err
        Right psuitReqFun -> case psuitReqFun testItemFull of
          Left reqFail -> assertFailure $ "expected a permitted fling, got: "
            ++ show reqFail
          Right posRange -> posRange @?= (bpos heroC, True)
            -- the xhair position, and the range verdict: the stub item is
            -- weightless, so the range its throw modifier yields covers
            -- the step to C (the range is computed from weight and
            -- velocity, not from the aiming path)
  , -- The fourth outcome, and the only one that never reaches the aiming
    -- pipeline: psuitReq returns before it whenever the viewed level is
    -- not the actor's (which is also what keeps xhairLegalEps's
    -- @lidV == blid b@ assertion out of reach here).
    testCase "psuitReq: xhair on a remote level" $ do
      let testFn = do
            modifySession $ \sess ->
              sess {saimMode = Just AimMode { aimLevelId = toEnum 222
                                            , detailLevel = defaultDetailLevel }}
            psuitReq testActorId
      (result, _) <- executorCli testFn testCliStateWithItem
      case result of
        Left err -> err @?= "can't fling on remote level"
        Right psuitReqFun -> assertFailure $ "expected a failed aim, got: "
          ++ show (psuitReqFun testItemFull)
  , testCase "xhairLegalEps" $ do
      let testFn = xhairLegalEps testActorId
      result <- executorCli testFn testCliStateWithItem
      fst result @?= Right 114  -- not a coincidence this matches testFactionId,
                                -- because @eps@ is initialized that way,
                                -- for "randomness"
  , -- [contract] The premise sibling bug (a) rests on
    -- (docs/leader-desync-bug.md, §09), and the ruling that keeps
    -- @permittedProjectClient@ a "some actor" function: its verdict is
    -- about the actor it is *given*, never about the pointman. The
    -- verdicts are actor-dependent -- here through the projecting skill;
    -- position and calm differ the same way -- which is what makes
    -- reusing an entry actor's captured closure after a switch report the
    -- wrong suitability, and what the two rows after this one pin.
    -- The pointman is switched between the two rounds of calls below and
    -- neither verdict moves. That is the contract, not a flip: §03 of
    -- docs/leader-desync-migration.md keeps this function's @ActorId@, so
    -- live-read must leave both answers exactly as they are; a conversion
    -- that made it read @sleader@ would collapse the second round to two
    -- copies of C's verdict and fail here. It carried an [LR-flip] tag
    -- until 2026-07-30, when a review found it had nothing to flip: what
    -- live-read makes unrepresentable is the dialog's capture, not this.
    testCase
      "contract permittedProjectClient judges its argument, not the pointman"
      $ do
      let skills = EM.fromList
            [ ( testActorId
              , Ability.addSk Ability.SkProject 1 Ability.zeroSkills )
            , (testActorId2, Ability.zeroSkills) ]
          cliS = partyCliState {cliState =
            updateActorMaxSkills (const skills) (cliState partyCliState)}
          testFn = do
            updateClientLeader testActorId
            funA1 <- permittedProjectClient testActorId
            funC1 <- permittedProjectClient testActorId2
            updateClientLeader testActorId2  -- the pointman switches ...
            funA2 <- permittedProjectClient testActorId
            funC2 <- permittedProjectClient testActorId2
            return ( (funA1 testItemFull, funC1 testItemFull)
                   , (funA2 testItemFull, funC2 testItemFull) )
      (result, _) <- executorCli testFn cliS
      let verdicts = (Right True, Left ProjectUnskilled)
      result @?= (verdicts, verdicts)  -- ... and nothing follows it

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
    testCase
      "LR-flip psuitReq verdict differs per actor (desync sibling, captured)"
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

  , -- [LR-flip] Sibling bug (a) end to end, through the real fling dialog:
    -- chooseItemProjectHuman computes @psuitReq A@ ONCE and bakes the
    -- resulting closure into the dialog's @psuit@; a scripted C-Tab then
    -- switches the pointman to C mid-dialog (the dialog permits it:
    -- @maySwitchLeader MStore = True@) and recCall re-enters the dialog
    -- for C, whose bag is judged by A's captured closure. C is unskilled,
    -- so the item is not flingable for the live pointman, yet Return
    -- selects it and @sitemSel@ is set. The two tests above pin the
    -- per-actor difference of the captured value; this one shows it
    -- surviving a real switch inside the real dialog.
    -- After the live-read design lands, @psuitReq@ loses its ActorId
    -- argument and the closure judges for the live pointman, so after the
    -- switch no item is suitable and the dialog runs out of keys and
    -- exits: flip the expectation to
    -- @(Just "*never mind*", Nothing, Just testActorId2)@ -- verified by
    -- temporarily making this dialog's @psuit@ re-read the pointman.
    testCase "LR-flip fling dialog: a mid-dialog switch keeps A's closure"
      $ do
      let projSk = Ability.addSk Ability.SkProject 1 Ability.zeroSkills
          skills = EM.fromList [ (testActorId, projSk)
                               , (testActorId2, Ability.zeroSkills) ]
          giveItem b = b {beqp = EM.singleton testItemId (1, [])}
          seeItem sess = let ItemRoles roles = sroles sess
                         in sess {sroles = ItemRoles
                                  $ EM.adjust (ES.insert testItemId) SItem
                                              roles}
            -- the dialog lists only items given a role; without this the
            -- store reads as empty however full the bag is
      cliS0 <- scriptedCliState partyCliStateWalkable
                                [K.mkKM "C-Tab", K.returnKM]
      let cliS = cliS0
            { cliState = updateItemD (EM.insert testItemId stubItem)
                         $ updateActorD (EM.adjust giveItem testActorId
                                         . EM.adjust giveItem testActorId2)
                         $ updateActorMaxSkills (const skills)
                         $ cliState cliS0
            , cliSession = seeItem <$> cliSession cliS0 }
          testFn = do
            modifySession enlargeScreenForItems
            initBfsTabs
            updateClientLeader testActorId
            aimAtHeroC
            merr <- chooseItemProjectHuman testActorId []
            itemSel <- getsSession sitemSel
            leaderAfter <- getsClient sleader
            return (fmap showFailError merr, itemSel, leaderAfter)
      (result, _) <- executorCli testFn cliS
      result @?= ( Nothing  -- the fling selection succeeded ...
                 , Just (testItemId, CEqp, False)  -- ... for this item ...
                 , Just testActorId2 )  -- ... which C, the pointman, can't
                                        -- fling at all

  , -- [contract] The first test through the real dialog machinery:
    -- chooseItemHuman opens the equipment store dialog
    -- (chooseItemDialogMode -> getStoreItem -> displayChoiceScreen),
    -- which renders its frames and
    -- reads keys via promptGetKey -- answered ESC by the stub frontend --
    -- so the dialog exits with "never mind". Also pins promptGetKey's
    -- second engine call site (SlideshowM.hs:421). The screen (not the
    -- level) is enlarged first: dialog prompts wrap via
    -- indentSplitAttrString, which asserts a screen wider than 4.
    testCase "contract chooseItemHuman: ESC exits the real store dialog" $ do
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
    testCase
      "contract chooseItemHuman: scripted Tab switches pointman mid-dialog"
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
