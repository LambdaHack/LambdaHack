{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of most 'CmdCli' client commands.
module Game.LambdaHack.Client.CmdCliSem where

import Control.Monad.Writer.Strict (WriterT, runWriterT)
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.CmdHuman
import Game.LambdaHack.Client.CmdHumanSem
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.LocalAction
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.Strategy
import Game.LambdaHack.Client.StrategyAction
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Content.StrategyKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

handleAI :: MonadClient m => ActorId -> m [CmdSer]
handleAI actor = do
  body <- getsState $ getActorBody actor
  side <- getsClient sside
  assert (bfaction body == side `blame` (actor, bfaction body, side)) $ do
    Kind.COps{costrat=Kind.Ops{okind}} <- getsState scops
    leader <- getsClient _sleader
    fact <- getsState $ (EM.! bfaction body) . sfaction
    let factionAI | Just actor /= leader = gAiMember fact
                  | otherwise = fromJust $ gAiLeader fact
        factionAbilities = sabilities (okind factionAI)
    stratTarget <- targetStrategy actor factionAbilities
    -- Choose a target from those proposed by AI for the actor.
    btarget <- rndToAction $ frequency $ bestVariant stratTarget
    modifyClient $ updateTarget actor (const btarget)
    stratAction <- actionStrategy actor factionAbilities
    let _debug = T.unpack
          $ "HandleAI abilities:" <+> showT factionAbilities
          <>          ", symbol:" <+> showT (bsymbol body)
          <>          ", loc:"    <+> showT (bpos body)
          <> "\nHandleAI target:" <+> showT stratTarget
          <> "\nHandleAI move:"   <+> showT stratAction
    -- trace _debug skip
    -- Run the AI: chose an action from those given by the AI strategy.
    rndToAction $ frequency $ bestVariant $ stratAction

-- | Handle the move of the hero.
handleHuman :: (MonadActionAbort m, MonadClientUI m) => ActorId -> m CmdSer
handleHuman aid = do
  -- When running, stop if aborted by a disturbance. Otherwise let
  -- the human player issue commands, until any of them takes time.
  -- First time, just after pushing frames, ask for commands in Push mode.
  leader <- getLeaderUI
  assert (leader == aid `blame` (leader, aid)) skip
  let inputHumanCmd msg = do
        stopRunning
        humanCommand msg
  tryWith inputHumanCmd $ do
    srunning <- getsClient srunning
    maybe abort (continueRun leader) srunning
--  addSmell leader  -- TODO: instead do for all non-spawning factions

-- | Continue running in the given direction.
continueRun :: (MonadActionAbort m, MonadClient m)
            => ActorId -> (Vector, Int) -> m CmdSer
continueRun leader dd = do
  (dir, distNew) <- continueRunDir leader dd
  modifyClient $ \cli -> cli {srunning = Just (dir, distNew)}
  -- Attacks and opening doors disallowed when continuing to run.
  return $ RunSer leader dir

-- | Determine and process the next human player command. The argument is
-- the last abort message due to running, if any.
humanCommand :: forall m. (MonadActionAbort m, MonadClientUI m)
             => Msg
             -> m CmdSer
humanCommand msgRunAbort = do
  -- The frame state is now Push.
  kmPush <- case msgRunAbort of
    "" -> getKeyCommand (Just True)
    _  -> do
      slides <- promptToSlideshow msgRunAbort
      getKeyOverlayCommand $ head $ runSlideshow slides
  -- The frame state is now None and remains so between each pair
  -- of lines of @loop@ (but can change within called actions).
  let loop :: K.KM -> m CmdSer
      loop km = do
        -- Messages shown, so update history and reset current report.
        recordHistory
        -- On abort, just reset state and call loop again below.
        -- Each abort that gets this far generates a slide to be shown.
        (mcmdS, slides) <- runWriterT $ tryWithSlide (return Nothing) $ do
          -- Look up the key.
          Binding{kcmd} <- askBinding
          case M.lookup km kcmd of
            Just (_, _, cmd) -> do
              -- Query and clear the last command key.
              lastKey <- getsClient slastKey
              -- TODO: perhaps replace slastKey
              -- with test 'kmNext == km'
              -- or an extra arg to 'loop'.
              -- Depends on whether slastKey
              -- is needed in other parts of code.
              modifyClient (\st -> st {slastKey = Just km})
              if (Just km == lastKey)
                then cmdHumanSem Clear
                else cmdHumanSem cmd
            Nothing -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                       in abortWith msgKey
        -- The command was aborted or successful and if the latter,
        -- possibly took some time.
        case mcmdS of
          Just cmdS -> assert (null (runSlideshow slides) `blame` slides) $ do
            -- Exit the loop and let other actors act. No next key needed
            -- and no slides could have been generated.
            modifyClient (\st -> st {slastKey = Nothing})
            return cmdS
          Nothing ->
            -- If no time taken, rinse and repeat.
            -- Analyse the obtained slides.
            case reverse (runSlideshow slides) of
              [] -> do
                -- Nothing special to be shown; by default draw current state.
                modifyClient (\st -> st {slastKey = Nothing})
                sli <- promptToSlideshow ""
                kmNext <- getKeyOverlayCommand $ head $ runSlideshow sli
                loop kmNext
              sLast : sls -> do
                -- Show, one by one, all but the last slide.
                -- Note: the code that generates the slides is responsible
                -- for inserting the @more@ prompt.
                b <- getManyConfirms [km] $ toSlideshow $ reverse sls
                -- Display the last slide while waiting for the next key,
                -- or display current state if slideshow interrupted.
                kmNext <- if b
                          then getKeyOverlayCommand sLast
                          else do
                            modifyClient (\st -> st {slastKey = Nothing})
                            sli <- promptToSlideshow ""
                            getKeyOverlayCommand $ head $ runSlideshow sli
                -- Look up and perform the next command.
                loop kmNext
  loop kmPush
