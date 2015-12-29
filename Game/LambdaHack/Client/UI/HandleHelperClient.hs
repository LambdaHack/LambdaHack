-- | Helper functions for both inventory management and human commands.
module Game.LambdaHack.Client.UI.HandleHelperClient
  ( memberCycle, memberBack, pickLeader, partyAfterLeader
  , cursorPointerFloor, cursorPointerEnemy
  , moveCursorHuman, tgtFloorHuman, tgtEnemyHuman
  , epsIncrHuman, tgtClearHuman, doLook
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Control.Monad (void, when)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List (find, findIndex, sortBy)
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonClient
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK

-- | Switches current member to the next on the level, if any, wrapping.
memberCycle :: MonadClientUI m => Bool -> m Slideshow
memberCycle verbose = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader
  let autoLvl = snd $ autoDungeonLevel fact
  case filter (\(_, b) -> blid b == blid body) hs of
    _ | autoLvl -> failMsg $ showReqFailure NoChangeLvlLeader
    [] -> failMsg "cannot pick any other member on this level"
    (np, b) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader" `twith` (leader, np, b)) ()
      return mempty

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberBack :: MonadClientUI m => Bool -> m Slideshow
memberBack verbose = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  hs <- partyAfterLeader leader
  let (autoDun, autoLvl) = autoDungeonLevel fact
  case reverse hs of
    _ | autoDun -> failMsg $ showReqFailure NoChangeDunLeader
    _ | autoLvl -> failMsg $ showReqFailure NoChangeLvlLeader
    [] -> failMsg "no other member in the party"
    (np, b) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `twith` (leader, np, b)) ()
      return mempty

partyAfterLeader :: MonadStateRead m => ActorId -> m [(ActorId, Actor)]
partyAfterLeader leader = do
  faction <- getsState $ bfid . getActorBody leader
  allA <- getsState $ EM.assocs . sactorD
  let factionA = filter (\(_, body) ->
        not (bproj body) && bfid body == faction) allA
      hs = sortBy (comparing keySelected) factionA
      i = fromMaybe (-1) $ findIndex ((== leader) . fst) hs
      (lt, gt) = (take i hs, drop (i + 1) hs)
  return $! gt ++ lt

-- | Select a faction leader. False, if nothing to do.
pickLeader :: MonadClientUI m => Bool -> ActorId -> m Bool
pickLeader verbose aid = do
  leader <- getLeaderUI
  stgtMode <- getsSession stgtMode
  if leader == aid
    then return False -- already picked
    else do
      pbody <- getsState $ getActorBody aid
      let !_A = assert (not (bproj pbody)
                        `blame` "projectile chosen as the leader"
                        `twith` (aid, pbody)) ()
      -- Even if it's already the leader, give his proper name, not 'you'.
      let subject = partActor pbody
      when verbose $ msgAdd $ makeSentence [subject, "picked as a leader"]
      -- Update client state.
      s <- getState
      modifyClient $ updateLeader aid s
      -- Move the cursor, if active, to the new level.
      case stgtMode of
        Nothing -> return ()
        Just _ ->
          modifySession $ \sess -> sess {stgtMode = Just $ TgtMode $ blid pbody}
      -- Inform about items, etc.
      lookMsg <- lookAt False "" True (bpos pbody) aid ""
      when verbose $ msgAdd lookMsg
      return True

cursorPointerFloor :: MonadClientUI m => Bool -> Bool -> m Slideshow
cursorPointerFloor verbose addMoreMsg = do
  km <- getsSession slastKM
  lidV <- viewedLevel
  Level{lxsize, lysize} <- getLevel lidV
  case K.pointer km of
    Just(Point{..}) | px >= 0 && py - mapStartY >= 0
                      && px < lxsize && py - mapStartY < lysize -> do
      let scursor = TPoint lidV $ Point px (py - mapStartY)
      modifySession $ \sess -> sess {stgtMode = Just $ TgtMode lidV}
      modifyClient $ \cli -> cli {scursor}
      if verbose then
        doLook addMoreMsg
      else do
        displayPush ""  -- flash the targeting line and path
        displayDelay  -- for a bit longer
        return mempty
    _ -> do
      void $ stopPlayBack
      return mempty

cursorPointerEnemy :: MonadClientUI m => Bool -> Bool -> m Slideshow
cursorPointerEnemy verbose addMoreMsg = do
  km <- getsSession slastKM
  lidV <- viewedLevel
  Level{lxsize, lysize} <- getLevel lidV
  case K.pointer km of
    Just(Point{..}) | px >= 0 && py - mapStartY >= 0
                      && px < lxsize && py - mapStartY < lysize -> do
      bsAll <- getsState $ actorAssocs (const True) lidV
      let newPos = Point px (py - mapStartY)
          scursor =
            case find (\(_, m) -> bpos m == newPos) bsAll of
              Just (im, _) -> TEnemy im True
              Nothing -> TPoint lidV newPos
      modifySession $ \sess -> sess {stgtMode = Just $ TgtMode lidV}
      modifyClient $ \cli -> cli {scursor}
      if verbose then
        doLook addMoreMsg
      else do
        displayPush ""  -- flash the targeting line and path
        displayDelay  -- for a bit longer
        return mempty
    _ -> do
      void $ stopPlayBack
      return mempty

-- | Move the cursor. Assumes targeting mode.
moveCursorHuman :: MonadClientUI m => Vector -> Int -> m Slideshow
moveCursorHuman dir n = do
  leader <- getLeaderUI
  stgtMode <- getsSession stgtMode
  let lidV = maybe (assert `failure` leader) tgtLevelId stgtMode
  Level{lxsize, lysize} <- getLevel lidV
  lpos <- getsState $ bpos . getActorBody leader
  scursor <- getsClient scursor
  cursorPos <- cursorToPos
  let cpos = fromMaybe lpos cursorPos
      shiftB pos = shiftBounded lxsize lysize pos dir
      newPos = iterate shiftB cpos !! n
  if newPos == cpos then failMsg "never mind"
  else do
    let tgt = case scursor of
          TVector{} -> TVector $ newPos `vectorToFrom` lpos
          _ -> TPoint lidV newPos
    modifyClient $ \cli -> cli {scursor = tgt}
    doLook False

-- | Cycle targeting mode. Do not change position of the cursor,
-- switch among things at that position.
tgtFloorHuman :: MonadClientUI m => m Slideshow
tgtFloorHuman = do
  lidV <- viewedLevel
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  cursorPos <- cursorToPos
  scursor <- getsClient scursor
  stgtMode <- getsSession stgtMode
  bsAll <- getsState $ actorAssocs (const True) lidV
  let cursor = fromMaybe lpos cursorPos
      tgt = case scursor of
        _ | isNothing stgtMode ->  -- first key press: keep target
          scursor
        TEnemy a True -> TEnemy a False
        TEnemy{} -> TPoint lidV cursor
        TEnemyPos{} -> TPoint lidV cursor
        TPoint{} -> TVector $ cursor `vectorToFrom` lpos
        TVector{} ->
          -- For projectiles, we pick here the first that would be picked
          -- by '*', so that all other projectiles on the tile come next,
          -- without any intervening actors from other tiles.
          case find (\(_, m) -> Just (bpos m) == cursorPos) bsAll of
            Just (im, _) -> TEnemy im True
            Nothing -> TPoint lidV cursor
  modifySession $ \sess -> sess {stgtMode = Just $ TgtMode lidV}
  modifyClient $ \cli -> cli {scursor = tgt}
  doLook False

tgtEnemyHuman :: MonadClientUI m => m Slideshow
tgtEnemyHuman = do
  lidV <- viewedLevel
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  cursorPos <- cursorToPos
  scursor <- getsClient scursor
  stgtMode <- getsSession stgtMode
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  bsAll <- getsState $ actorAssocs (const True) lidV
  let ordPos (_, b) = (chessDist lpos $ bpos b, bpos b)
      dbs = sortBy (comparing ordPos) bsAll
      pickUnderCursor =  -- switch to the actor under cursor, if any
        let i = fromMaybe (-1)
                $ findIndex ((== cursorPos) . Just . bpos . snd) dbs
        in splitAt i dbs
      (permitAnyActor, (lt, gt)) = case scursor of
        TEnemy a permit | isJust stgtMode ->  -- pick next enemy
          let i = fromMaybe (-1) $ findIndex ((== a) . fst) dbs
          in (permit, splitAt (i + 1) dbs)
        TEnemy a permit ->  -- first key press, retarget old enemy
          let i = fromMaybe (-1) $ findIndex ((== a) . fst) dbs
          in (permit, splitAt i dbs)
        TEnemyPos _ _ _ permit -> (permit, pickUnderCursor)
        _ -> (False, pickUnderCursor)  -- the sensible default is only-foes
      gtlt = gt ++ lt
      isEnemy b = isAtWar fact (bfid b)
                  && not (bproj b)
                  && bhp b > 0
      lf = filter (isEnemy . snd) gtlt
      tgt | permitAnyActor = case gtlt of
        (a, _) : _ -> TEnemy a True
        [] -> scursor  -- no actors in sight, stick to last target
          | otherwise = case lf of
        (a, _) : _ -> TEnemy a False
        [] -> scursor  -- no seen foes in sight, stick to last target
  -- Register the chosen enemy, to pick another on next invocation.
  modifySession $ \sess -> sess {stgtMode = Just $ TgtMode lidV}
  modifyClient $ \cli -> cli {scursor = tgt}
  doLook False

-- | Tweak the @eps@ parameter of the targeting digital line.
epsIncrHuman :: MonadClientUI m => Bool -> m Slideshow
epsIncrHuman b = do
  stgtMode <- getsSession stgtMode
  if isJust stgtMode
    then do
      modifyClient $ \cli -> cli {seps = seps cli + if b then 1 else -1}
      return mempty
    else failMsg "never mind"  -- no visual feedback, so no sense

tgtClearHuman :: MonadClientUI m => m Slideshow
tgtClearHuman = do
  leader <- getLeaderUI
  tgt <- getsClient $ getTarget leader
  case tgt of
    Just _ -> do
      modifyClient $ updateTarget leader (const Nothing)
      return mempty
    Nothing -> do
      scursorOld <- getsClient scursor
      b <- getsState $ getActorBody leader
      let scursor = case scursorOld of
            TEnemy _ permit -> TEnemy leader permit
            TEnemyPos _ _ _ permit -> TEnemy leader permit
            TPoint{} -> TPoint (blid b) (bpos b)
            TVector{} -> TVector (Vector 0 0)
      modifyClient $ \cli -> cli {scursor}
      doLook False

-- | Perform look around in the current position of the cursor.
-- Normally expects targeting mode and so that a leader is picked.
doLook :: MonadClientUI m => Bool -> m Slideshow
doLook addMoreMsg = do
  Kind.COps{cotile=Kind.Ops{ouniqGroup}} <- getsState scops
  let unknownId = ouniqGroup "unknown space"
  stgtMode <- getsSession stgtMode
  case stgtMode of
    Nothing -> return mempty
    Just tgtMode -> do
      leader <- getLeaderUI
      let lidV = tgtLevelId tgtMode
      lvl <- getLevel lidV
      cursorPos <- cursorToPos
      per <- getPerFid lidV
      b <- getsState $ getActorBody leader
      let p = fromMaybe (bpos b) cursorPos
          canSee = ES.member p (totalVisible per)
      inhabitants <- if canSee
                     then getsState $ posToActors p lidV
                     else return []
      seps <- getsClient seps
      mnewEps <- makeLine False b p seps
      itemToF <- itemToFullClient
      let aims = isJust mnewEps
          enemyMsg = case inhabitants of
            [] -> ""
            (_, body) : rest ->
                 -- Even if it's the leader, give his proper name, not 'you'.
                 let subjects = map (partActor . snd) inhabitants
                     subject = MU.WWandW subjects
                     verb = "be here"
                     desc =
                       if not (null rest)  -- many actors, only list names
                       then ""
                       else case itemDisco $ itemToF (btrunk body) (1, []) of
                         Nothing -> ""  -- no details, only show the name
                         Just ItemDisco{itemKind} -> IK.idesc itemKind
                     pdesc = if desc == "" then "" else "(" <> desc <> ")"
                 in makeSentence [MU.SubjectVerbSg subject verb] <+> pdesc
          vis | lvl `at` p == unknownId = "that is"
              | not canSee = "you remember"
              | not aims = "you are aware of"
              | otherwise = "you see"
      -- Show general info about current position.
      lookMsg <- lookAt True vis canSee p leader enemyMsg
{- targeting is kind of a menu (or at least mode), so this is menu inside
   a menu, which is messy, hence disabled until UI overhauled:
      -- Check if there's something lying around at current position.
      is <- getsState $ getCBag $ CFloor lidV p
      if EM.size is <= 2 then
        promptToSlideshow lookMsg
      else do
        msgAdd lookMsg  -- TODO: do not add to history
        floorItemOverlay lidV p
-}
      promptToSlideshow $ lookMsg <+> if addMoreMsg then moreMsg else ""
