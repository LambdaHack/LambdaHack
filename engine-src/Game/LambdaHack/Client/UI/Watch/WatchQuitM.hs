{-# LANGUAGE TupleSections #-}
-- | Display all the initial (not including high scores) screens at game over.
module Game.LambdaHack.Client.UI.Watch.WatchQuitM
  ( quitFactionUI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , displayGameOverLoot, displayGameOverAnalytics, displayGameOverLore
  , viewLoreItems
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Analytics
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

quitFactionUI :: MonadClientUI m
              => FactionId -> Maybe Status
              -> Maybe (FactionAnalytics, GenerationAnalytics)
              -> m ()
quitFactionUI fid toSt manalytics = do
  ClientOptions{sexposeItems} <- getsClient soptions
  fact <- getsState $ (EM.! fid) . sfactionD
  let fidName = MU.Text $ gname fact
      person = if fhasGender $ gplayer fact then MU.PlEtc else MU.Sg3rd
      horror = isHorrorFact fact
      camping = maybe True ((== Camping) . stOutcome) toSt
  side <- getsClient sside
  when (fid == side && not camping) $ do
    tellGameClipPS
    resetGameStart
  gameMode <- getGameMode
  allNframes <- getsSession sallNframes
  let startingPart = case toSt of
        _ | horror -> Nothing  -- Ignore summoned actors' factions.
        Just Status{stOutcome=stOutcome@Restart, stNewGame=Just gn} ->
          Just $ MU.Text $ nameOutcomeVerb stOutcome
                           <+> "to restart in"
                           <+> displayGroupName gn
                           <+> "mode"
                             -- when multiplayer: "order mission restart in"
        Just Status{stOutcome=Restart, stNewGame=Nothing} ->
          error $ "" `showFailure` (fid, toSt)
        Just Status{stOutcome} -> Just $ MU.Text $ nameOutcomeVerb stOutcome
          -- when multiplayer, for @Camping@: "order save and exit"
        Nothing -> Nothing
      middlePart = case toSt of
        _ | fid /= side -> Nothing
        Just Status{stOutcome} -> lookup stOutcome $ mendMsg gameMode
        Nothing -> Nothing
      partingPart = if fid /= side || allNframes == -1
                    then Nothing
                    else endMessageOutcome . stOutcome <$> toSt
  case startingPart of
    Nothing -> return ()
    Just sp ->
      let blurb = makeSentence [MU.SubjectVerb person MU.Yes fidName sp]
      in msgLnAdd MsgFinalOutcome blurb
  case (toSt, partingPart) of
    (Just status, Just pp) -> do
      noConfirmsGame <- isNoConfirmsGame
      go <- if noConfirmsGame
            then return False
            else displaySpaceEsc ColorFull ""  -- short, just @startingPart@
      recordHistory
        -- we are going to exit or restart, so record and clear, but only once
      (itemBag, total) <- getsState $ calculateTotal side
      when go $ do
        case middlePart of
          Nothing -> return ()
          Just sp1 -> do
            factionD <- getsState sfactionD
            itemToF <- getsState $ flip itemToFull
            let getTrunkFull (aid, b) = (aid, itemToF $ btrunk b)
            ourTrunks <- getsState $ map getTrunkFull
                                     . fidActorNotProjGlobalAssocs side
            let smartFaction fact2 = fleaderMode (gplayer fact2) /= Nothing
                canBeSmart = any (smartFaction . snd)
                canBeOurFaction = any (\(fid2, _) -> fid2 == side)
                smartEnemy trunkFull =
                  let possible =
                        possibleActorFactions (itemKind trunkFull) factionD
                  in not (canBeOurFaction possible) && canBeSmart possible
                smartEnemiesOurs = filter (smartEnemy . snd) ourTrunks
                uniqueActor trunkFull = IA.checkFlag Ability.Unique
                                        $ aspectRecordFull trunkFull
                uniqueEnemiesOurs = filter (uniqueActor . snd) smartEnemiesOurs
                smartUniqueEnemyCaptured = not $ null uniqueEnemiesOurs
                smartEnemyCaptured = not $ null smartEnemiesOurs
            smartEnemySentence <- case uniqueEnemiesOurs ++ smartEnemiesOurs of
              [] -> return ""
              (enemyAid, _) : _ -> do
                bUI <- getsSession $ getActorUI enemyAid
                return $! makePhrase [MU.Capitalize (partActor bUI)] <> "?"
            let won = maybe False ((`elem` victoryOutcomes) . stOutcome) toSt
                lost = maybe False ((`elem` deafeatOutcomes) . stOutcome) toSt
                msgClass | won = MsgGoodMiscEvent
                         | lost = MsgBadMiscEvent
                         | otherwise = MsgNeutralEvent
                (sp2, escPrompt) =
                  if | lost -> ("", "Accept the unacceptable?")
                     | smartUniqueEnemyCaptured ->
                       ( "\nOh, wait, who is this, towering behind your escaping crew?" <+> smartEnemySentence <+> "This changes everything. For everybody. Everywhere. Forever. Did you plan for this? Are you sure it was your idea?"
                       , "What happens now?" )
                     | smartEnemyCaptured ->
                       ( "\nOh, wait, who is this, hunched among your escaping crew?" <+> smartEnemySentence <+> "Suddenly, this makes your crazy story credible. Suddenly, the door of knowledge opens again."
                       , "How will you play that move?" )
                     | otherwise -> ("", "Let's see what we've got here.")
            msgAdd msgClass sp1
            msgAdd MsgFactionIntel sp2
            void $ displaySpaceEsc ColorFull escPrompt
        case manalytics of
          Nothing -> return ()
          Just (factionAn, generationAn) ->
            cycleLore []
              [ displayGameOverLoot (itemBag, total) generationAn
              , displayGameOverLore SOrgan True generationAn
              , displayGameOverAnalytics factionAn generationAn
              , displayGameOverLore SCondition sexposeItems generationAn
              , displayGameOverLore SBlast True generationAn
              , displayGameOverLore SEmbed True generationAn ]
      go2 <- if noConfirmsGame then return False else do
        -- Show score for any UI client after any kind of game exit,
        -- even though it's saved only for human UI clients at game over
        -- (that is not a noConfirms or benchmark game).
        scoreSlides <- scoreToSlideshow total status
        km <- getConfirms ColorFull [K.spaceKM, K.escKM] scoreSlides
        return $! km == K.spaceKM
      let epilogue = do
            when camping $ msgAdd MsgPromptGeneric "Saving..."
            -- Don't leave frozen old prompts on the browser screen.
            pushReportFrame
      if go2 && not noConfirmsGame && not camping then do
        msgAdd MsgPromptGeneric $ pp <+> "(Press RET to have one last look at the arena of your struggle before it gets forgotten.)"
        slides <-
          reportToSlideshowKeepHalt True [K.returnKM, K.spaceKM, K.escKM]
        km <- getConfirms ColorFull [K.returnKM, K.spaceKM, K.escKM] slides
        if km == K.returnKM then do
          -- Enter aiming mode. At exit, game arena is wiped out.
          lidV <- viewedLevelUI
          let saimMode = Just $ AimMode lidV defaultDetailLevel
          modifySession $ \sess -> sess { sreqDelay = ReqDelayHandled
                                        , saimMode }
        else epilogue
      else do
        when (not noConfirmsGame || camping) $ do
          -- The last prompt stays onscreen during shutdown, etc.
          msgAdd MsgPromptGeneric pp
          epilogue
    _ ->
      when (isJust startingPart && (stOutcome <$> toSt) == Just Killed) $ do
        msgAdd MsgTutorialHint "When a whole faction gets eliminated, no new members of the party will ever appear and its stashed belongings may remain far off, unclaimed and undefended. While some adventures require elimination a faction (to be verified in the adventure description screen in the help menu), for others it's an optional task, if possible at all. Instead, finding an exit may be necessary to win. It's enough if one character finds and triggers the exit. Others automatically follow, duly hauling all common belongings."
        -- Needed not to overlook the competitor dying in raid scenario.
        displayMore ColorFull ""

displayGameOverLoot :: MonadClientUI m
                    => (ItemBag, Int) -> GenerationAnalytics -> m K.KM
displayGameOverLoot (heldBag, total) generationAn = do
  ClientOptions{sexposeItems} <- getsClient soptions
  COps{coitem} <- getsState scops
  -- We assume "gold grain", not "grain" with label "of gold":
  let currencyName = IK.iname $ okind coitem $ ouniqGroup coitem IK.S_CURRENCY
      generationItem = generationAn EM.! SItem
      itemBag =
        if sexposeItems
        then let generationBag = EM.map (\k -> (-k, [])) generationItem
             in heldBag `EM.union` generationBag
        else heldBag
      promptFun iid itemFull2 k =
        let worth = itemPrice 1 $ itemKind itemFull2
            lootMsg = if worth == 0 then "" else
              let pile = if k <= 1 then "exemplar" else "hoard"
              in makeSentence $
                   ["this treasure", pile, "is worth"]
                   ++ (if k > 1 then [ MU.Cardinal k, "times"] else [])
                   ++ [MU.CarWs worth $ MU.Text currencyName]
            holdsMsg =
              let n = generationItem EM.! iid
              in if | max 0 k == 1 && n == 1 ->
                      "You keep the only specimen extant:"
                    | max 0 k == 0 && n == 1 ->
                      "You don't have the only hypothesized specimen:"
                    | max 0 k == 0 && n == 0 ->
                      "No such specimen was recorded:"
                    | otherwise ->
                        makePhrase [ "You hold"
                                   , if k == n
                                     then "all pieces"
                                     else MU.CardinalAWs (max 0 k) "piece"
                                   , "out of"
                                   , MU.Car n
                                   , "scattered:" ]
        in lootMsg <+> holdsMsg
  dungeonTotal <- getsState sgold
  let promptGold = spoilsBlurb currencyName total dungeonTotal
      -- Total number of items is meaningless in the presence of so much junk.
      prompt =
        promptGold
        <+> (if sexposeItems
             then "Non-positive count means none held but this many generated."
             else "")
  viewLoreItems "GameOverLoot" itemBag prompt promptFun (MLore SItem)

displayGameOverAnalytics :: MonadClientUI m
                         => FactionAnalytics -> GenerationAnalytics
                         -> m K.KM
displayGameOverAnalytics factionAn generationAn = do
  ClientOptions{sexposeActors} <- getsClient soptions
  side <- getsClient sside
  ItemRoles itemRoles <- getsSession sroles
  let ourAn = akillCounts
              $ EM.findWithDefault emptyAnalytics side factionAn
      foesAn = EM.unionsWith (+)
               $ concatMap EM.elems $ catMaybes
               $ map (`EM.lookup` ourAn) [KillKineticMelee .. KillOtherPush]
      killedBagIncludingProjectiles = EM.map (, []) foesAn
      killedBag = EM.filterWithKey
                    (\iid _ -> iid `ES.member` (itemRoles EM.! STrunk))
                    killedBagIncludingProjectiles
      generationTrunk = generationAn EM.! STrunk
      trunkBag =
        if sexposeActors
        then let generationBag = EM.map (\k -> (-k, [])) generationTrunk
             in killedBag `EM.union` generationBag
        else killedBag
      total = sum $ filter (> 0) $ map fst $ EM.elems trunkBag
      -- Not just "killed 1 out of 4", because it's sometimes "2 out of 1",
      -- if an enemy was revived.
      promptFun :: ItemId -> ItemFull-> Int -> Text
      promptFun iid _ k =
        let n = generationTrunk EM.! iid
        in makePhrase [ "You recall the adversary, which you killed on"
                      , MU.CarWs (max 0 k) "occasion", "while reports mention"
                      , MU.CarWs n "individual", "in total:" ]
      prompt =
        makeSentence ["your team vanquished", MU.CarWs total "adversary"]
          -- total reported would include our own, so not given
        <+> (if sexposeActors
             then "Non-positive count means none killed but this many reported."
             else "")
  viewLoreItems "GameOverAnalytics" trunkBag prompt promptFun (MLore STrunk)

displayGameOverLore :: MonadClientUI m
                    => SLore -> Bool -> GenerationAnalytics -> m K.KM
displayGameOverLore slore exposeCount generationAn = do
  let generationLore = generationAn EM.! slore
      generationBag = EM.map (\k -> (if exposeCount then k else 1, []))
                             generationLore
      total = sum $ map fst $ EM.elems generationBag
      promptFun :: ItemId -> ItemFull-> Int -> Text
      promptFun _ _ k =
        makeSentence
          [ "this", MU.Text (ppSLore slore), "manifested during your quest"
          , MU.CarWs k "time" ]
      verb = if | slore `elem` [SCondition, SBlast] -> "experienced"
                | slore == SEmbed -> "strived through"
                | otherwise -> "lived among"
      prompt = case total of
        0 -> makeSentence [ "you didn't experience any"
                          , MU.Ws $ MU.Text (headingSLore slore)
                          , "this time" ]
        1 -> makeSentence [ "you", verb, "the following"
                          , MU.Text (headingSLore slore) ]
        _ -> makeSentence [ "you", verb, "the following variety of"
                          , MU.CarWs total $ MU.Text (headingSLore slore) ]
  viewLoreItems ("GameOverLore" ++ show slore)
                generationBag prompt promptFun (MLore slore)

viewLoreItems :: forall m . MonadClientUI m
              => String -> ItemBag -> Text
              -> (ItemId -> ItemFull -> Int -> Text)
              -> ItemDialogMode
              -> m K.KM
viewLoreItems menuName trunkBag prompt promptFun dmode = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  FontSetup{..} <- getFontSetup
  arena <- getArenaUI
  itemToF <- getsState $ flip itemToFull
  let keys = [K.spaceKM, K.mkChar '<', K.mkChar '>', K.escKM]
      iids = sortIids itemToF $ EM.assocs trunkBag
  msgAdd MsgPromptGeneric prompt
  io <- itemOverlay arena iids dmode
  itemSlides <- overlayToSlideshow (rheight - 2) keys io
  let displayInRightPane :: KeyOrSlot -> m OKX
      displayInRightPane ekm = case ekm of
        _ | isSquareFont propFont -> return emptyOKX
        Left{} -> return emptyOKX
        Right slot -> do
          -- Mono font used, because lots of numbers in these blurbs
          -- and because some prop fonts wider than mono (e.g., in the
          -- dejavuBold font set).
          -- Lower width, to permit extra vertical space at the start,
          -- because gameover menu prompts are sometimes wide and/or long.
          okxItemLorePointedAt monoFont (rwidth - 2) True iids 0 promptFun
                               (fromEnum slot)
      viewAtSlot :: SlotChar -> m K.KM
      viewAtSlot slot = do
        km <- displayItemLore iids 0 promptFun (fromEnum slot) False
        case K.key km of
          K.Space -> viewLoreItems menuName trunkBag prompt promptFun dmode
          K.Esc -> return km
          _ -> error $ "" `showFailure` km
  ekm <- displayChoiceScreenWithRightPane displayInRightPane True
           menuName ColorFull False itemSlides keys
  case ekm of
    Left km | km `elem` [K.spaceKM, K.mkChar '<', K.mkChar '>', K.escKM] ->
      return km
    Left km -> error $ "" `showFailure` km
    Right slot -> viewAtSlot slot
