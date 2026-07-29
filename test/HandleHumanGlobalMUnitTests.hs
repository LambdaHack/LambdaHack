-- | Tests for global human-command handlers, currently pinning the
-- selection/execution sibling of the leader-desync family
-- (see docs/leader-desync-bug.md, §09, sibling bug (b)).
module HandleHumanGlobalMUnitTests (handleHumanGlobalMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Test.Tasty
import           Test.Tasty.HUnit

import           Game.LambdaHack.Client.MonadClient (getsClient)
import           Game.LambdaHack.Client.State (sleader)
import           Game.LambdaHack.Client.UI
  (SessionUI (..), modifySession, updateClientLeader)
import           Game.LambdaHack.Client.UI.HandleHelperM (showFailError)
import           Game.LambdaHack.Client.UI.HandleHumanGlobalM
  (alterDirHuman, projectHuman)
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.SessionUI
  (KeyMacro (..), KeyMacroFrame (..), emptyMacroFrame)
import           Game.LambdaHack.Common.Actor (Actor (..))
import           Game.LambdaHack.Common.State
  (updateActorD, updateActorMaxSkills, updateItemD)
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs (CStore (..))

import UnitTestHelpers

handleHumanGlobalMUnitTests :: TestTree
handleHumanGlobalMUnitTests = testGroup "handleHumanGlobalMUnitTests"
  [ -- [contract] Sibling bug (b) of the leader desync
    -- (docs/leader-desync-bug.md, §09): the fling key is
    -- @ComposeUnlessError (ChooseItemProject ts) Project@ -- two separate
    -- boundary dispatches, each reading the pointman afresh. If the player
    -- switches the pointman INSIDE the choose dialog, the selection
    -- (@sitemSel@) was validated for actor A but @Project@ then runs
    -- @projectHuman@ for actor C and looks the remembered item up in C's
    -- store -- where it isn't. This pins both halves of the seam:
    --
    -- * control: executed by A, the actor the selection was validated for,
    --   the store lookup succeeds and the command proceeds past it (here it
    --   then fails, deterministically, on the calm check -- the stub actors
    --   have no Calm -- but crucially NOT with "no item to fling");
    -- * bug: executed by C, it fails with "no item to fling" for the item
    --   the dialog just approved.
    --
    -- Classified [contract]: the execute-half behaviour pinned here is
    -- correct in isolation and survives the live-read design unchanged --
    -- what live-read fixes is the CHOOSE half (its live re-reads make the
    -- dialog re-validate for C before the selection is confirmed), closing
    -- the seam where the incoherent approval arises.
    testCase
      "contract Project executed by a different actor than the item selection"
      $ do
      let projSk = Ability.addSk Ability.SkProject 1 Ability.zeroSkills
          skills = EM.fromList
            [(testActorId, projSk), (testActorId2, projSk)]
          giveItem b = b {beqp = EM.singleton testItemId (1, [])}
          cliS = partyCliState {cliState =
              updateItemD (EM.insert testItemId stubItem)
            $ updateActorD (EM.adjust giveItem testActorId)
            $ updateActorMaxSkills (const skills)
            $ cliState partyCliState}
          -- What A's ChooseItemProject dialog leaves behind on success:
          select = modifySession $ \sess ->
            sess {sitemSel = Just (testItemId, CEqp, False)}
      (controlResult, _) <-
        executorCli (select >> projectHuman testActorId) cliS
      (bugResult, _) <-
        executorCli (select >> projectHuman testActorId2) cliS
      case bugResult of
        Left err -> showFailError err @?= "*no item to fling*"
        Right req -> assertFailure $ "unexpected request: " ++ show req
      case controlResult of
        Left err -> assertBool
          ("control must get past the store lookup, got: "
           ++ show (showFailError err))
          (showFailError err /= "*no item to fling*")
        Right _ -> return ()  -- even better: the request itself

  , -- [LR-flip] The remaining §09 site: @alterDirHuman@ asks for a
    -- direction through @pickPoint@, which spans an interactive wait and
    -- only then shifts the HELD leader's position to get the square to
    -- modify. Here the wait is the one from the post-mortem's §04 window:
    -- a macro dies inside @promptGetKey@, which restores the pointman to
    -- the run leader A -- and the command carries on with the leader it
    -- was handed. Both runs press the same key and differ only in that
    -- held value, so the square each modifies is the held actor's
    -- neighbour: A's is C's floor, C's is the wall past it, and the two
    -- failures name the two tiles. (The stub board is fenced with
    -- unalterable walls, so neither attempt can succeed; what is pinned
    -- is which square was chosen.)
    -- After the live-read design lands, @pickPoint@ reads the pointman
    -- after the wait rather than taking it as an argument, so both runs
    -- modify from the restored A: flip the second component to the floor
    -- message -- verified by temporarily making @pickPoint@ re-read the
    -- pointman after the key.
    testCase "LR-flip alterDir: the held leader picks the square to modify"
      $ do
      let dirRight = K.mkKM "Right"  -- keypad Right is the (1, 0) move
          testFn leaderHeld = do
            modifySession enlargeScreen
            initBfsTabs
            updateClientLeader testActorId   -- run leader A
            updateClientLeader testActorId2  -- run rotated pointman to C
            modifySession $ \sess ->
              sess { sreqQueried = False
                   , srunning = Just runParamsA
                   , smacroFrame =
                       emptyMacroFrame {keyPending = KeyMacro [K.mkKM "x"]} }
            merr <- alterDirHuman leaderHeld
              -- the macro dies in promptGetKey inside pickPoint: the
              -- pointman is restored to A and the scripted key is read
            leaderAfter <- getsClient sleader
            return (either showFailError (const "a request") merr, leaderAfter)
      cliSA <- scriptedCliState partyCliStateWalkable [dirRight]
      (resultHeldA, _) <- executorCli (testFn testActorId) cliSA
      cliSC <- scriptedCliState partyCliStateWalkable [dirRight]
      (resultHeldC, _) <- executorCli (testFn testActorId2) cliSC
      (resultHeldA, resultHeldC)
        @?= ( ("*there is no way to activate or modify a floor*"
              , Just testActorId)
            , ("*there is no way to activate or modify a wall*"
              , Just testActorId) )
  ]
