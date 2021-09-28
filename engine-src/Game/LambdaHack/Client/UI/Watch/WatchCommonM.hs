{-# LANGUAGE TupleSections #-}
-- | Common code for displaying atomic update and SFX commands.
module Game.LambdaHack.Client.UI.Watch.WatchCommonM
  ( pushFrame, fadeOutOrIn, markDisplayNeeded, lookAtMove
  , aidVerbMU, aidVerbDuplicateMU, itemVerbMUGeneral, itemVerbMU
  , itemVerbMUShort, itemAidVerbMU, mitemAidVerbMU, itemAidDistinctMU
  , manyItemsAidVerbMU
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.Animation
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Definition.Ability as Ability

-- | Push the frame depicting the current level to the frame queue.
-- Only one line of the report is shown, as in animations,
-- because it may not be our turn, so we can't clear the message
-- to see what is underneath.
pushFrame :: MonadClientUI m => Bool -> m ()
pushFrame delay = do
  -- The delay before reaction to keypress was too long in case of many
  -- projectiles flying and ending flight, so frames need to be skipped.
  keyPressed <- anyKeyPressed
  unless keyPressed $ do
    lidV <- viewedLevelUI
    FontSetup{propFont} <- getFontSetup
    frame <- basicFrameWithoutReport lidV propFont
    -- Pad with delay before and after to let player see, e.g., door being
    -- opened a few ticks after it came into vision, the same turn.
    displayFrames lidV $
      if delay then [Nothing, Just frame, Nothing] else [Just frame]

fadeOutOrIn :: MonadClientUI m => Bool -> m ()
fadeOutOrIn out = do
  arena <- getArenaUI
  CCUI{coscreen} <- getsSession sccui
  animMap <- rndToActionUI $ fadeout coscreen out 2
  animFrs <- renderAnimFrames True arena animMap
  displayFrames arena (tail animFrs)  -- no basic frame between fadeout and in

markDisplayNeeded :: MonadClientUI m => LevelId -> m ()
markDisplayNeeded lid = do
  lidV <- viewedLevelUI
  when (lidV == lid) $ modifySession $ \sess -> sess {sdisplayNeeded = True}

lookAtMove :: MonadClientUI m => ActorId -> m ()
lookAtMove aid = do
  mleader <- getsClient sleader
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  aimMode <- getsSession saimMode
  when (not (bproj body)
        && bfid body == side
        && isNothing aimMode) $ do  -- aiming does a more extensive look
    stashBlurb <- lookAtStash (blid body) (bpos body)
    (itemsBlurb, _) <- lookAtItems True (bpos body) aid Nothing
    let msgClass = if Just aid == mleader
                   then MsgAtFeetMajor
                   else MsgAtFeetMinor
        blurb = stashBlurb <+> itemsBlurb
    unless (T.null blurb) $
      msgAdd msgClass blurb
  fact <- getsState $ (EM.! bfid body) . sfactionD
  adjBigAssocs <- getsState $ adjacentBigAssocs body
  adjProjAssocs <- getsState $ adjacentProjAssocs body
  if not (bproj body) && bfid body == side then do
    let foe (_, b2) = isFoe (bfid body) fact (bfid b2)
        adjFoes = filter foe $ adjBigAssocs ++ adjProjAssocs
    unless (null adjFoes) stopPlayBack
  else when (isFoe (bfid body) fact side) $ do
    let our (_, b2) = bfid b2 == side
        adjOur = filter our adjBigAssocs
    unless (null adjOur) stopPlayBack

aidVerbMU :: (MonadClientUI m, MsgShared a) => a -> ActorId -> MU.Part -> m ()
aidVerbMU msgClass aid verb = do
  subject <- partActorLeader aid
  msgAdd msgClass $ makeSentence [MU.SubjectVerbSg subject verb]

aidVerbDuplicateMU :: (MonadClientUI m, MsgShared a)
                   => a -> ActorId -> MU.Part -> m Bool
aidVerbDuplicateMU msgClass aid verb = do
  subject <- partActorLeader aid
  msgAddDuplicate msgClass (makeSentence [MU.SubjectVerbSg subject verb])

itemVerbMUGeneral :: MonadClientUI m
                  => Bool -> ItemId -> ItemQuant -> MU.Part -> Container
                  -> m Text
itemVerbMUGeneral verbose iid kit@(k, _) verb c = assert (k > 0) $ do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  lid <- getsState $ lidFromC c
  localTime <- getsState $ getLocalTime lid
  itemFull <- getsState $ itemToFull iid
  side <- getsClient sside
  factionD <- getsState sfactionD
  let arItem = aspectRecordFull itemFull
      partItemWsChosen | verbose = partItemWs
                       | otherwise = partItemWsShort
      subject = partItemWsChosen rwidth side factionD k localTime itemFull kit
      msg | k > 1 && not (IA.checkFlag Ability.Condition arItem) =
              makeSentence [MU.SubjectVerb MU.PlEtc MU.Yes subject verb]
          | otherwise = makeSentence [MU.SubjectVerbSg subject verb]
  return $! msg

itemVerbMU :: (MonadClientUI m, MsgShared a)
           => a -> ItemId -> ItemQuant -> MU.Part -> Container -> m ()
itemVerbMU msgClass iid kit verb c = do
  msg <- itemVerbMUGeneral True iid kit verb c
  msgAdd msgClass msg

itemVerbMUShort :: (MonadClientUI m, MsgShared a)
                => a -> ItemId -> ItemQuant -> MU.Part -> Container
                -> m ()
itemVerbMUShort msgClass iid kit verb c = do
  msg <- itemVerbMUGeneral False iid kit verb c
  msgAdd msgClass msg

itemAidVerbMU :: (MonadClientUI m, MsgShared a)
              => a -> ActorId -> MU.Part -> ItemId -> Either Int Int
              -> m ()
itemAidVerbMU msgClass aid verb iid ek = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  factionD <- getsState sfactionD
  let lid = blid body
      fakeKit = quantSingle
  localTime <- getsState $ getLocalTime lid
  subject <- partActorLeader aid
  -- The item may no longer be in @c@, but it was.
  itemFull <- getsState $ itemToFull iid
  let object = case ek of
        Left n ->
          partItemWs rwidth side factionD n localTime itemFull fakeKit
        Right n ->
          let (name1, powers) =
                partItemShort rwidth side factionD localTime itemFull fakeKit
          in MU.Phrase ["the", MU.Car1Ws n name1, powers]
      msg = makeSentence [MU.SubjectVerbSg subject verb, object]
  msgAdd msgClass msg

mitemAidVerbMU :: (MonadClientUI m, MsgShared a)
               => a -> ActorId -> MU.Part -> ItemId -> Maybe MU.Part
               -> m ()
mitemAidVerbMU msgClass aid verb iid msuffix = do
  itemD <- getsState sitemD
  case msuffix of
    Just suffix | iid `EM.member` itemD ->
      itemAidVerbMU msgClass aid (MU.Phrase [verb, suffix]) iid (Right 1)
    _ -> do
#ifdef WITH_EXPENSIVE_ASSERTIONS
      side <- getsClient sside
      b <- getsState $ getActorBody aid
      bUI <- getsSession $ getActorUI aid
      -- It's not actually expensive, but it's particularly likely
      -- to fail with wild content, indicating server game rules logic
      -- needs to be fixed/extended.
      -- Observer from another faction may receive the effect information
      -- from the server, because the affected actor is visible,
      -- but the position of the item may be out of FOV. This is fine;
      -- the message is then shorter, because only the effect was seen,
      -- while the cause remains misterious.
      assert (isNothing msuffix  -- item description not requested
              || bfid b /= side  -- not from affected faction; only observing
              `blame` "item never seen by the affected actor"
              `swith` (aid, b, bUI, verb, iid, msuffix)) $
#endif
        aidVerbMU msgClass aid verb

itemAidDistinctMU :: MonadClientUI m
                  => MsgClassDistinct -> ActorId -> MU.Part -> MU.Part -> ItemId
                  -> m ()
itemAidDistinctMU msgClass aid verbShow verbSave iid = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  factionD <- getsState sfactionD
  let lid = blid body
      fakeKit = quantSingle
  localTime <- getsState $ getLocalTime lid
  subject <- partActorLeader aid
  -- The item may no longer be in @c@, but it was.
  itemFull <- getsState $ itemToFull iid
  let object = let (name, powers) =
                     partItem rwidth side factionD localTime itemFull fakeKit
               in MU.Phrase [name, powers]
      t1 = makeSentence [MU.SubjectVerbSg subject verbShow, object]
      t2 = makeSentence [MU.SubjectVerbSg subject verbSave, object]
      dotsIfShorter = if t1 == t2 then "" else ".."
  msgAddDistinct msgClass (t1 <> dotsIfShorter, t2)

manyItemsAidVerbMU :: (MonadClientUI m, MsgShared a)
                   => a -> ActorId -> MU.Part
                   -> [(ItemId, ItemQuant)] -> (Int -> Either (Maybe Int) Int)
                   -> m ()
manyItemsAidVerbMU msgClass aid verb sortedAssocs ekf = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  factionD <- getsState sfactionD
  let lid = blid body
      fakeKit = quantSingle
  localTime <- getsState $ getLocalTime lid
  subject <- partActorLeader aid
  -- The item may no longer be in @c@, but it was.
  itemToF <- getsState $ flip itemToFull
  let object (iid, (k, _)) =
        let itemFull = itemToF iid
        in case ekf k of
          Left (Just n) ->
            partItemWs rwidth side factionD n localTime itemFull fakeKit
          Left Nothing ->
            let (name, powers) =
                  partItem rwidth side factionD localTime itemFull fakeKit
            in MU.Phrase [name, powers]
          Right n ->
            let (name1, powers) =
                  partItemShort rwidth side factionD localTime itemFull fakeKit
            in MU.Phrase ["the", MU.Car1Ws n name1, powers]
      msg = makeSentence [ MU.SubjectVerbSg subject verb
                         , MU.WWandW $ map object sortedAssocs]
  msgAdd msgClass msg
