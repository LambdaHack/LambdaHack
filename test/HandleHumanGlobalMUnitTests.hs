-- | Tests for global human-command handlers, currently pinning the
-- selection/execution sibling of the leader-desync family
-- (see docs/leader-desync-bug.md, §09, sibling bug (b)).
module HandleHumanGlobalMUnitTests (handleHumanGlobalMUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Test.Tasty
import           Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI (SessionUI (..), modifySession)
import           Game.LambdaHack.Client.UI.HandleHelperM (showFailError)
import           Game.LambdaHack.Client.UI.HandleHumanGlobalM (projectHuman)
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
    testCase "Project executed by a different actor than the item selection"
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
  ]
