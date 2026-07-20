-- | Tests for the pointman-cycling helpers, including the live-read series
-- for the leader-desync family (see docs/leader-desync-bug.md,
-- sections 09-13).
--
-- Test classification, reflecting that the abort-split design
-- (docs/promptgetkey-hygiene.md) is planned strictly after the live-read
-- design and assumes it:
--
-- * [LR-flip]  -- characterization of CURRENT (desync-prone) behaviour;
--               the expected value flips as indicated when the live-read
--               design lands.
-- * [contract] -- behaviour that must survive the live-read AND the
--               abort-split design unchanged; these never flip.
module HandleHelperMUnitTests (handleHelperMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Exception as Ex
import qualified Data.Map.Strict as M
import           Test.Tasty
import           Test.Tasty.HUnit

import           Game.LambdaHack.Client.MonadClient (MonadClient, getsClient)
import           Game.LambdaHack.Client.State (sleader)
import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import           Game.LambdaHack.Client.UI.ContentClientUI (coinput)
import           Game.LambdaHack.Client.UI.FrameM (restoreLeaderFromRun)
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanM (cmdSemInCxtOfKM)
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import           Game.LambdaHack.Client.UI.MonadClientUI (MonadClientUI (..))
import           Game.LambdaHack.Client.UI.SessionUI (SessionUI (..))
import           Game.LambdaHack.Definition.Defs (Direction (..))

import UnitTestHelpers

-- Dispatch a command through the real key-loop entry point
-- (cmdSemInCxtOfKM), with the key looked up in the fixture CCUI's real
-- bindings (the sample game's) -- exactly what the key loop does with a
-- keypress (Client/UI.hs:198-203). In particular the pointman handed to
-- the command's handler is read from sleader at dispatch time, so it is
-- in sync by construction, as for any top-level keystroke.
-- The PointmanCycle* commands are client-local, so no request can result.
dispatchCmd :: (MonadClient m, MonadClientUI m)
            => HumanCmd.HumanCmd -> m MError
dispatchCmd cmd = do
  coinputContent <- getsSession $ coinput . sccui
  let km = case M.lookup cmd (IC.brevMap coinputContent) of
        Just (km1 : _) -> km1
        _ -> error $ "no sample game key binding" `showFailure` cmd
  either id (\_req -> error $ "local command sent a request"
                              `showFailure` cmd)
    <$> cmdSemInCxtOfKM km cmd

handleHelperMUnitTests :: TestTree
handleHelperMUnitTests = testGroup "handleHelperMUnitTests" $
  partyAfterLeaderTest : liveReadSeries
 where
  partyAfterLeaderTest = testCase "partyAfterLeader" $ do
    -- You've got to fight for your right to party!
    let testFunc = partyAfterLeader testActorId
    partyInMonad <- executorCli testFunc testCliStateWithItem
    let party = fst partyInMonad
    party @?= []

-- * The live-read series: the pointman-desync family

liveReadSeries :: [TestTree]
liveReadSeries =
  [ -- [contract] The target invariant, driven through the real key-loop
    -- dispatch (dispatchCmd above), which reads the pointman from 'sleader'
    -- and so is in sync by construction: cycling advances to the next party
    -- member on the level. After the live-read design lands, this is the
    -- only reachable behaviour.
    testCase "LR1: in-sync pointmanCycleLevel advances A -> C" $ do
      let testFn = do
            updateClientLeader testActorId
            merr <- dispatchCmd $ HumanCmd.PointmanCycleLevel Forward
            leaderAfter <- getsClient sleader
            return (merr, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (Nothing, Just testActorId2)

  , -- [contract] Backward cycling picks the previous member (wrapping):
    -- party order is [A, B, C], so Backward from A lands on C.
    testCase "LR2: in-sync pointmanCycleLevel Backward picks C from A" $ do
      let testFn = do
            updateClientLeader testActorId
            merr <- dispatchCmd $ HumanCmd.PointmanCycleLevel Backward
            leaderAfter <- getsClient sleader
            return (merr, leaderAfter)
      (result, _) <- executorCli testFn partyCliState3
      result @?= (Nothing, Just testActorId2)

  , -- [LR-flip] The reproducer for commit 4a6eca154 (kept from the original
    -- §07 test): a stale captured leader makes cycling a silent no-op
    -- (with the disabled assertion restored, a crash). Every step is the
    -- genuine engine operation: step 2 is the run rotation's write pair
    -- (RunM.hs:90-91), step 4 the restoreLeaderFromRun that promptGetKey
    -- runs on interrupted macro playback (see FrameMUnitTests.X1 for the
    -- same window driven through the real promptGetKey) and step 5 the
    -- item dialogs' cycling call (InventoryM.hs:398).
    -- After the live-read design lands: flip the last component to
    -- 'Just testActorId2'.
    testCase "LR3: stale leader makes pointmanCycleLevel a no-op" $ do
      let testFn = do
            updateClientLeader testActorId   -- 1. run leader A is the pointman
            -- 2. run rotates pointman to C (the RunM.hs:90-91 write pair):
            updateClientLeader testActorId2
            modifySession $ \sess -> sess {srunning = Just runParamsA}
            leaderCapturedByDialog <-        -- 3. dialog binds C
              fromMaybe testActorId2 <$> getsClient sleader
            restoreLeaderFromRun             -- 4. macro dies: restore to A
            leaderBefore <- getsClient sleader
            -- 5. C-Tab inside the dialog, with the stale captured leader:
            _merr <- pointmanCycleLevel leaderCapturedByDialog False Forward
            leaderAfter <- getsClient sleader
            return (leaderCapturedByDialog, leaderBefore, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (testActorId2, Just testActorId, Just testActorId)

  , -- [LR-flip] With three members the same desync picks the WRONG member
    -- instead of no member: pointman restored to A, dialog still holds B;
    -- "next after B" is C, but the correct answer (next after A) is B.
    -- After the live-read design lands: flip the expectation to
    -- 'Just testActorId3' (B).
    testCase "LR4: stale leader makes pointmanCycleLevel pick the wrong member"
      $ do
      let testFn = do
            updateClientLeader testActorId3  -- pointman B (captured by dialog)
            modifySession $ \sess -> sess {srunning = Just runParamsA}
            leaderCaptured <-
              fromMaybe testActorId3 <$> getsClient sleader
            restoreLeaderFromRun             -- hidden restore to A
            _merr <- pointmanCycleLevel leaderCaptured False Forward
            getsClient sleader
      (result, _) <- executorCli testFn partyCliState3
      result @?= Just testActorId2  -- C; correct would be B (testActorId3)

  , -- [LR-flip] The dungeon-wide twin pointmanCycle kept its "same leader"
    -- assertion (only pointmanCycleLevel's was disabled by 4a6eca154), so
    -- the same desync CRASHES it -- this is the changelog crash, pinned.
    -- After the live-read design lands: no exception; expect
    -- advancement to C instead.
    testCase "LR5: stale leader crashes pointmanCycle ('same leader' assert)"
      $ do
      let testFn = do
            updateClientLeader testActorId2  -- pointman C (captured)
            modifySession $ \sess -> sess {srunning = Just runParamsA}
            leaderCaptured <-
              fromMaybe testActorId2 <$> getsClient sleader
            restoreLeaderFromRun             -- hidden restore to A
            pointmanCycle leaderCaptured False Forward
      result <- Ex.try (executorCli testFn partyCliState)
                :: IO (Either Ex.SomeException (MError, CliState))
      case result of
        Left _ -> return ()  -- crashed, as documented
        Right _ -> assertFailure
          "expected the 'same leader' assertion to fire on a stale leader"

  , -- [LR-flip] A dangling stale ActorId (actor absent from the party, e.g.
    -- conceptually dead) is silently tolerated: partyAfterLeader can't find
    -- the pivot and returns the WHOLE party, so cycling picks the first
    -- member -- an arbitrary result, not an error. After the live-read design
    -- lands, the argument does not exist, so this case is unrepresentable.
    testCase "LR6: dangling stale ActorId yields an arbitrary pick" $ do
      let testFn = do
            updateClientLeader testActorId2  -- pointman C
            _merr <- pointmanCycleLevel (toEnum 999) False Forward
            getsClient sleader
      (result, _) <- executorCli testFn partyCliState
      result @?= Just testActorId  -- first of the whole party, by accident

  , -- [contract] partyAfterLeader keeps its pivot parameter under the
    -- live-read design (it means "some actor", not "the pointman now"):
    -- rotation semantics.
    testCase "LR7: partyAfterLeader rotates around its pivot" $ do
      let testFn = do
            updateClientLeader testActorId
            afterA <- map (\(aid, _, _) -> aid) <$> partyAfterLeader testActorId
            afterB <- map (\(aid, _, _) -> aid)
                      <$> partyAfterLeader testActorId3
            return (afterA, afterB)
      (result, _) <- executorCli testFn partyCliState3
      result @?= ( [testActorId3, testActorId2]    -- after A: [B, C]
                 , [testActorId2, testActorId] )   -- after B: [C, A]

  , -- [contract] The edge that lets np == sleader: an unknown pivot makes
    -- partyAfterLeader return the whole party, current pointman included.
    testCase "LR8: partyAfterLeader with unknown pivot returns whole party" $ do
      let testFn = map (\(aid, _, _) -> aid) <$> partyAfterLeader (toEnum 999)
      (result, _) <- executorCli testFn partyCliState3
      result @?= [testActorId, testActorId3, testActorId2]  -- [A, B, C]

  , -- [contract] The pickLeader primitive both designs build on:
    -- no-op (False) when the target already is the pointman,
    -- a real switch (True) otherwise.
    testCase "LR9: pickLeader no-ops on current pointman, switches otherwise"
      $ do
      let testFn = do
            updateClientLeader testActorId
            noop <- pickLeader False testActorId
            switch <- pickLeader False testActorId2
            leaderAfter <- getsClient sleader
            return (noop, switch, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (False, True, Just testActorId2)

  , -- [contract] Banned factions (fspawnsFast): dungeon-wide cycling is
    -- refused outright...
    testCase "LR10: banned faction refuses pointmanCycle" $ do
      let testFn = do
            updateClientLeader testActorId
            merr <- dispatchCmd $ HumanCmd.PointmanCycle Forward
            leaderAfter <- getsClient sleader
            return (isJust merr, leaderAfter)
      (result, _) <- executorCli testFn partyCliStateBanned
      result @?= (True, Just testActorId)

  , -- [contract] ...but same-level cycling still works for banned factions
    -- (the ban only guards the cross-level case) -- the §10 partition
    -- subtlety that a live-read rewrite must not change.
    testCase "LR11: banned faction still allows pointmanCycleLevel on level"
      $ do
      let testFn = do
            updateClientLeader testActorId
            merr <- dispatchCmd $ HumanCmd.PointmanCycleLevel Forward
            leaderAfter <- getsClient sleader
            return (merr, leaderAfter)
      (result, _) <- executorCli testFn partyCliStateBanned
      result @?= (Nothing, Just testActorId2)

  , -- [contract] The dungeon-wide twin also advances when in sync, driven
    -- through the real key-loop dispatch (Tab): the non-banned success
    -- path of the same pointmanCycle whose live assertion the desync
    -- crashes (LR5).
    testCase "LR12: in-sync pointmanCycle advances A -> C" $ do
      let testFn = do
            updateClientLeader testActorId
            merr <- dispatchCmd $ HumanCmd.PointmanCycle Forward
            leaderAfter <- getsClient sleader
            return (merr, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= (Nothing, Just testActorId2)

  , -- [contract] The CmdLeader boundary: with no pointman designated, the
    -- dispatch refuses with a friendly failure instead of reaching the
    -- handler -- the one place that turns 'Maybe ActorId' into an MError,
    -- kept by the live-read design (docs/leader-desync-bug.md, §10).
    testCase "LR13: dispatch refuses when no pointman designated" $ do
      let testFn = do
            merr <- dispatchCmd $ HumanCmd.PointmanCycleLevel Forward
            leaderAfter <- getsClient sleader
            return (fmap showFailError merr, leaderAfter)
      (result, _) <- executorCli testFn partyCliState
      result @?= ( Just $ "*command disabled when no pointman designated,"
                          <> " choose another command*"
                 , Nothing )
  ]
