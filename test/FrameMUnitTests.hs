-- | Tests for FrameM's macro-playback machinery, i.e. the abort-split
-- series plus the bridge tests touching both designs (see
-- docs/promptgetkey-hygiene.md; the §04 crash window they guard is in
-- docs/leader-desync-bug.md).
--
-- The abort-split design (extracting a pure @macroStep@ decision and a
-- named @abortMacroPlayback@ out of 'promptGetKey') is planned strictly
-- AFTER the live-read design and assumes it. Accordingly, the abort-split
-- series below are [contract] tests: they pin 'promptGetKey''s observable
-- branch behaviour through its unchanged type, and must pass before the
-- live-read design, after it, and after the abort-split -- they are the
-- safety net for the abort-split refactor, not characterizations to flip.
-- Only the bridge test X1 carries an [LR-flip] expectation, in its final
-- (cycling) step.
module FrameMUnitTests (frameMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import           Test.Tasty
import           Test.Tasty.HUnit

import           Game.LambdaHack.Client.MonadClient (getsClient)
import           Game.LambdaHack.Client.State (sleader)
import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import           Game.LambdaHack.Client.UI.ContentClientUI (coinput)
import           Game.LambdaHack.Client.UI.Frame (ColorMode (..))
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.HandleHelperM (pointmanCycleLevel)
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI (MonadClientUI (..))
import           Game.LambdaHack.Client.UI.SessionUI
  ( KeyMacro (..)
  , KeyMacroFrame (..)
  , RunParams (..)
  , SessionUI (..)
  , emptyMacroFrame
  )
import           Game.LambdaHack.Definition.Defs (Direction (..))

import UnitTestHelpers

kmX :: K.KM
kmX = K.mkKM "x"

frameMUnitTests :: TestTree
frameMUnitTests = testGroup "frameMUnitTests" $
  pureMacroTests ++ promptGetKeyContractTests ++ restoreLeaderGuardTests
  ++ bridgeTests

-- * Pure macro helpers (already extractable; abort-split moves the rest)

pureMacroTests :: [TestTree]
pureMacroTests =
  [ -- [contract] Recording appends only keys bound to commands, and never
    -- the Record command itself; an idle buffer (Right) is left alone.
    testCase "AS1: addToMacro records bound keys, skips Record, noop when idle"
      $ do
      let kmWait = K.mkKM "z"
          kmRec = K.mkKM "r"
          bcmdMap = M.fromList
            [ (kmWait, ([], "", HumanCmd.Wait))
            , (kmRec, ([], "", HumanCmd.Record)) ]
          recording = emptyMacroFrame {keyMacroBuffer = Left []}
          recorded = addToMacro bcmdMap kmWait recording
          notCmd = addToMacro bcmdMap kmX recording
          recCmd = addToMacro bcmdMap kmRec recording
          idle = addToMacro bcmdMap kmWait emptyMacroFrame
      keyMacroBuffer recorded @?= Left [kmWait]
      keyMacroBuffer notCmd @?= Left []
      keyMacroBuffer recCmd @?= Left []
      keyMacroBuffer idle @?= Right mempty

  , -- [contract] Empty macro frames are dropped from the stack, but the
    -- last frame always survives (it holds the user's in-game macro state).
    testCase "AS2: dropEmptyMacroFrames GCs empties, keeps the last frame" $ do
      let pending = emptyMacroFrame {keyPending = KeyMacro [kmX]}
          (mf1, stack1) = dropEmptyMacroFrames emptyMacroFrame [pending]
          (mf2, stack2) = dropEmptyMacroFrames emptyMacroFrame [emptyMacroFrame]
      keyPending mf1 @?= KeyMacro [kmX]
      null stack1 @? "non-empty frame must become the head"
      keyPending mf2 @?= KeyMacro []
      null stack2 @? "all-empty stack must collapse to the last frame"
  ]

-- * promptGetKey contract tests (the abort-split design's safety net)

promptGetKeyContractTests :: [TestTree]
promptGetKeyContractTests =
  [ -- [contract] Voicing: a pending macro key that is legal and
    -- uninterrupted is consumed and returned; crucially, the run survives
    -- (srunning stays Just) and the pointman is untouched -- the enabler
    -- of the §04 crash window that both designs must keep in mind.
    testCase "AS3: promptGetKey voices a macro key; run and pointman survive"
      $ do
      let testFn = do
            updateClientLeader testActorId
            updateClientLeader testActorId2  -- run rotated pointman to C
            modifySession $ \sess ->
              sess { sreqQueried = True
                   , srunning = Just runParamsA
                   , smacroFrame =
                       emptyMacroFrame {keyPending = KeyMacro [kmX]} }
            km <- promptGetKey ColorFull EM.empty True []
            pending <- getsSession (keyPending . smacroFrame)
            running <- getsSession (isJust . srunning)
            leaderAfter <- getsClient sleader
            return (km, pending, running, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (kmX, KeyMacro [], True, Just testActorId2)

  , -- [contract] Natural end: no macro pending -> a real key is read,
    -- the run is cancelled, but the pointman is NOT restored (stays C).
    -- This is the branch-exactness of promptgetkey-hygiene.md §01:
    -- natural run end /= macro abort.
    testCase "AS4: promptGetKey with no macro clears the run, keeps pointman"
      $ do
      let testFn = do
            updateClientLeader testActorId
            updateClientLeader testActorId2  -- run rotated pointman to C
            modifySession $ \sess ->
              sess { sreqQueried = True
                   , srunning = Just runParamsA }
            km <- promptGetKey ColorFull EM.empty True []
            running <- getsSession (isJust . srunning)
            leaderAfter <- getsClient sleader
            return (km, running, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (K.escKM, False, Just testActorId2)
        -- escKM is what the stub frontend feeds as the "real" key

  , -- [contract] Abort via an illegal macro key ("a faulty key in a macro
    -- is a good reason to interrupt it"): macro wiped, run cancelled, and
    -- the pointman RESTORED to the run leader A. This is the hidden write
    -- of leader-desync-bug.md §03 -- the effect the abort-split design
    -- names abortMacroPlayback (promptgetkey-hygiene.md §01) and the
    -- live-read design makes safe. Its observable outcome must never
    -- change.
    testCase "AS5: promptGetKey aborts on illegal macro key, restores pointman"
      $ do
      let testFn = do
            updateClientLeader testActorId
            updateClientLeader testActorId2  -- run rotated pointman to C
            modifySession $ \sess ->
              sess { sreqQueried = True
                   , srunning = Just runParamsA
                   , smacroFrame =
                       emptyMacroFrame {keyPending = KeyMacro [kmX]} }
            _km <- promptGetKey ColorFull EM.empty True [K.spaceKM]
              -- kmX is not among the legal keys -> abort branch
            pending <- getsSession (keyPending . smacroFrame)
            running <- getsSession (isJust . srunning)
            leaderAfter <- getsClient sleader
            return (pending, running, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (KeyMacro [], False, Just testActorId)

  , -- [contract] Abort via "not queried" (special-event key request):
    -- same outcome as AS5, through the other input of the interrupt
    -- decision that the abort-split's pure macroStep must reproduce.
    testCase "AS6: promptGetKey aborts when not queried, restores pointman" $ do
      let testFn = do
            updateClientLeader testActorId
            updateClientLeader testActorId2  -- run rotated pointman to C
            modifySession $ \sess ->
              sess { sreqQueried = False
                   , srunning = Just runParamsA
                   , smacroFrame =
                       emptyMacroFrame {keyPending = KeyMacro [kmX]} }
            _km <- promptGetKey ColorFull EM.empty True []
            pending <- getsSession (keyPending . smacroFrame)
            running <- getsSession (isJust . srunning)
            leaderAfter <- getsClient sleader
            return (pending, running, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (KeyMacro [], False, Just testActorId)

  , -- [contract] Rendered (non-blank) frames work under the mock too:
    -- promptGetKey with onBlank = False draws the full HUD frame
    -- (drawHudFrame) over the stub board before reading the key. Pinned
    -- because the full-dialog coverage plan (leader-desync-bug.md §13)
    -- depends on dialogs rendering on the tiny fixture.
    testCase "AS7: promptGetKey renders a HUD frame on the stub board" $ do
      let testFn = do
            updateClientLeader testActorId
            promptGetKey ColorFull EM.empty False []
      (km, _) <- executorCli testFn partyCliState
      km @?= K.escKM

  , -- [contract] Keys voiced from a macro are recorded into an in-game
    -- macro being defined -- "keys coming from macros are recorded as
    -- well and this is well defined and essential" (the comment at the
    -- recording site in promptGetKey, which the abort-split's audit step
    -- keeps inside the primitive). Tab is used because it is bound in the
    -- fixture CCUI (unbound keys are not recorded).
    testCase "AS8: promptGetKey records voiced keys into the open macro" $ do
      let kmTab = K.mkKM "Tab"
          testFn = do
            updateClientLeader testActorId
            modifySession $ \sess ->
              sess { sreqQueried = True
                   , smacroFrame = emptyMacroFrame
                       { keyMacroBuffer = Left []
                       , keyPending = KeyMacro [kmTab] } }
            km <- promptGetKey ColorFull EM.empty True []
            buffer <- getsSession (keyMacroBuffer . smacroFrame)
            return (km, buffer)
      (result, _) <- executorCli testFn partyCliState
      result @?= (kmTab, Left [kmTab])

  , -- [contract] Abort via a disturbing report -- the third interrupt
    -- input: the real stopPlayBack (a watch-event handler) adds a
    -- MsgStopPlayback message, which disturbs resting and so interrupts
    -- pending playback. Same outcome as AS5/AS6: macro wiped, run
    -- cancelled, pointman restored to the run leader.
    testCase "AS9: promptGetKey aborts on a disturbing report" $ do
      let testFn = do
            updateClientLeader testActorId
            updateClientLeader testActorId2  -- run rotated pointman to C
            modifySession $ \sess ->
              sess { sreqQueried = True
                   , srunning = Just runParamsA
                   , smacroFrame =
                       emptyMacroFrame {keyPending = KeyMacro [kmX]} }
            stopPlayBack
            _km <- promptGetKey ColorFull EM.empty True []
            pending <- getsSession (keyPending . smacroFrame)
            running <- getsSession (isJust . srunning)
            leaderAfter <- getsClient sleader
            return (pending, running, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (KeyMacro [], False, Just testActorId)

  , -- [contract] ...but a help-displaying macro survives the same report:
    -- the F1 exemption, part of the interrupt decision that moves into
    -- the pure macroStep. The key is voiced, the run survives and the
    -- pointman is untouched.
    testCase "AS10: the F1 help macro survives a disturbing report" $ do
      let kmF1 = K.mkKM "F1"
          testFn = do
            updateClientLeader testActorId
            updateClientLeader testActorId2  -- run rotated pointman to C
            modifySession $ \sess ->
              sess { sreqQueried = True
                   , srunning = Just runParamsA
                   , smacroFrame =
                       emptyMacroFrame {keyPending = KeyMacro [kmF1]} }
            stopPlayBack
            km <- promptGetKey ColorFull EM.empty True []
            running <- getsSession (isJust . srunning)
            leaderAfter <- getsClient sleader
            return (km, running, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (kmF1, True, Just testActorId2)
  ]

-- * restoreLeaderFromRun guards (relocated verbatim into abortMacroPlayback)

restoreLeaderGuardTests :: [TestTree]
restoreLeaderGuardTests =
  [ -- [contract] Guard: without a run in progress the restore is a no-op
    -- -- the pointman stays put.
    testCase "AS11: restoreLeaderFromRun no-ops without a run" $ do
      let testFn = do
            updateClientLeader testActorId2  -- pointman C, no run
            restoreLeaderFromRun
            getsClient sleader
      (result, _) <- executorCli testFn partyCliState
      result @?= Just testActorId2

  , -- [contract] Guard: a faction that never runs with multiple actors
    -- (here banned via fspawnsFast) keeps whatever pointman is current,
    -- even with a run recorded.
    testCase "AS12: restoreLeaderFromRun no-ops for a noRunWithMulti faction"
      $ do
      let testFn = do
            updateClientLeader testActorId2  -- pointman C
            modifySession $ \sess -> sess {srunning = Just runParamsA}
            restoreLeaderFromRun
            getsClient sleader
      (result, _) <- executorCli testFn partyCliStateBanned
      result @?= Just testActorId2

  , -- [contract] Guard: a run leader no longer on the level (e.g. dead)
    -- is silently tolerated -- no restore, no crash.
    testCase "AS13: restoreLeaderFromRun no-ops on a gone run leader" $ do
      let testFn = do
            updateClientLeader testActorId2  -- pointman C
            modifySession $ \sess ->
              sess {srunning = Just runParamsA {runLeader = toEnum 999}}
            restoreLeaderFromRun
            getsClient sleader
      (result, _) <- executorCli testFn partyCliState
      result @?= Just testActorId2
  ]

-- * Bridge tests: the §04 window through the real promptGetKey

bridgeTests :: [TestTree]
bridgeTests =
  [ -- The full crash window of §04, with the leader restore performed by
    -- the REAL promptGetKey (unlike HandleHelperMUnitTests.LR3, which calls
    -- restoreLeaderFromRun directly): run rotates pointman to C; dialog
    -- captures C; macro dies inside the dialog; promptGetKey restores
    -- pointman to A; cycling with the stale captured C is then a silent
    -- no-op.
    --
    -- The promptGetKey part (restore to A) is [contract] and never flips.
    -- The final component is [LR-flip]: after the live-read design lands,
    -- cycling reads the pointman live and advances, so flip it to
    -- 'Just testActorId2'.
    testCase "X1: §04 window end-to-end; stale cycling no-ops after restore"
      $ do
      let testFn = do
            updateClientLeader testActorId   -- run leader A
            updateClientLeader testActorId2  -- run rotates pointman to C
            leaderCapturedByDialog <-
              fromMaybe testActorId2 <$> getsClient sleader
            modifySession $ \sess ->
              sess { sreqQueried = True
                   , srunning = Just runParamsA
                   , smacroFrame =
                       emptyMacroFrame {keyPending = KeyMacro [kmX]} }
            _km <- promptGetKey ColorFull EM.empty True [K.spaceKM]
              -- macro dies inside the dialog; pointman restored to A
            leaderRestored <- getsClient sleader
            _merr <- pointmanCycleLevel leaderCapturedByDialog False Forward
            leaderAfter <- getsClient sleader
            return (leaderCapturedByDialog, leaderRestored, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (testActorId2, Just testActorId, Just testActorId)

  , -- The §04 window again, with the post-abort keypress arriving as a
    -- REAL key from the (scripted) frontend: after promptGetKey aborts
    -- the macro (via sreqQueried = False, as in AS6) and restores the
    -- pointman, its real-key read returns the scripted C-Tab; the test
    -- resolves it through the fixture CCUI's real bindings and runs the
    -- dialog's cycling call with the stale captured leader, as
    -- InventoryM's cycleLevelKeyDef would. The promptGetKey observations
    -- are [contract]; the final component is [LR-flip], as in X1: flip it
    -- to 'Just testActorId2' when the live-read design lands.
    testCase "X2: §04 window ending in a literal scripted C-Tab" $ do
      cliS <- partyCliStateScripted [K.mkKM "C-Tab"]
      let testFn = do
            updateClientLeader testActorId   -- run leader A
            updateClientLeader testActorId2  -- run rotated pointman to C
            leaderCapturedByDialog <-
              fromMaybe testActorId2 <$> getsClient sleader
            modifySession $ \sess ->
              sess { sreqQueried = False
                   , srunning = Just runParamsA
                   , smacroFrame =
                       emptyMacroFrame {keyPending = KeyMacro [kmX]} }
            km <- promptGetKey ColorFull EM.empty True []
              -- macro dies (not queried); pointman restored to A;
              -- the real-key read pops the scripted C-Tab
            leaderRestored <- getsClient sleader
            coinputContent <- getsSession $ coinput . sccui
            case M.lookup km (IC.bcmdMap coinputContent) of
              Just (_, _, HumanCmd.PointmanCycleLevel direction) -> do
                _merr <- pointmanCycleLevel leaderCapturedByDialog False
                                            direction
                leaderAfter <- getsClient sleader
                return (km, leaderRestored, leaderAfter)
              _ -> error $ "scripted key not bound as expected"
                           `showFailure` km
      (result, _) <- executorCli testFn cliS
      result @?= (K.mkKM "C-Tab", Just testActorId, Just testActorId)
  ]

