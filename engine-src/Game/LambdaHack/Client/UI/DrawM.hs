-- {-# OPTIONS_GHC -fprof-auto #-}
-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.DrawM
  ( targetDesc, targetDescXhair, drawHudFrame
  , checkWarningHP, checkWarningCalm
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , drawFrameTerrain, drawFrameContent
  , drawFramePath, drawFrameActor, drawFrameExtra, drawFrameStatus
  , drawArenaStatus, drawLeaderStatus, drawLeaderDamage, drawSelected
  , checkWarnings
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Monad.ST.Strict
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Word (Word16, Word32)
import           GHC.Exts (inline)
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.Frontend (frontendName)
import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.CaveKind (cname)
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.ModeKind as MK
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind, isUknownSpace)
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

targetDesc :: MonadClientUI m => Maybe Target -> m (Maybe Text, Maybe Text)
targetDesc mtarget = do
  arena <- getArenaUI
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  let describeActorTarget aid = do
        side <- getsClient sside
        b <- getsState $ getActorBody aid
        bUI <- getsSession $ getActorUI aid
        actorMaxSk <- getsState $ getActorMaxSkills aid
        let percentage =
             100 * bhp b
              `div` xM (max 5 $ Ability.getSk Ability.SkMaxHP actorMaxSk)
            chs n = "[" <> T.replicate n "*"
                        <> T.replicate (4 - n) "_" <> "]"
            stars = chs $ fromEnum $ max 0 $ min 4 $ percentage `div` 20
            hpIndicator = if bfid b == side then Nothing else Just stars
        return (Just $ bname bUI, hpIndicator)
  case mtarget of
    Just (TEnemy aid) -> describeActorTarget aid
    Just (TNonEnemy aid) -> describeActorTarget aid
    Just (TPoint tgoal lid p) -> case tgoal of
      TEnemyPos{} -> do
        let hotText = if lid == lidV && arena == lidV
                      then "hot spot" <+> tshow p
                      else "a hot spot on level" <+> tshow (abs $ fromEnum lid)
        return (Just hotText, Nothing)
      _ -> do  -- the other goals can be invalidated by now anyway and it's
               -- better to say what there is rather than what there isn't
        pointedText <-
          if lid == lidV && arena == lidV
          then do
            bag <- getsState $ getFloorBag lid p
            case EM.assocs bag of
              [] -> return $! "exact spot" <+> tshow p
              [(iid, kit@(k, _))] -> do
                localTime <- getsState $ getLocalTime lid
                itemFull <- getsState $ itemToFull iid
                side <- getsClient sside
                factionD <- getsState sfactionD
                let (name, powers) =
                      partItem side factionD localTime itemFull kit
                return $! makePhrase [MU.Car1Ws k name, powers]
              _ -> return $! "many items at" <+> tshow p
          else return $! "an exact spot on level" <+> tshow (abs $ fromEnum lid)
        return (Just pointedText, Nothing)
    Just TVector{} ->
      case mleader of
        Nothing -> return (Just "a relative shift", Nothing)
        Just aid -> do
          tgtPos <- getsState $ aidTgtToPos aid lidV mtarget
          let invalidMsg = "an invalid relative shift"
              validMsg p = "shift to" <+> tshow p
          return (Just $ maybe invalidMsg validMsg tgtPos, Nothing)
    Nothing -> return (Nothing, Nothing)

targetDescXhair :: MonadClientUI m
                => m (Maybe Text, Maybe Text, Maybe Watchfulness)
targetDescXhair = do
  sxhair <- getsSession sxhair
  (mhairDesc, mxhairHP) <- targetDesc sxhair
  let maid = case sxhair of
        Just (TEnemy a) -> Just a
        Just (TNonEnemy a) -> Just a
        _ -> Nothing
  case maid of
    Nothing -> return (mhairDesc, mxhairHP, Nothing)
    Just aid -> do
      watchfulness <- bwatch <$> getsState (getActorBody aid)
      return $ (mhairDesc, mxhairHP, Just watchfulness)

drawFrameTerrain :: forall m. MonadClientUI m => LevelId -> m (U.Vector Word32)
drawFrameTerrain drawnLevelId = do
  COps{corule=RuleContent{rXmax}, cotile, coTileSpeedup} <- getsState scops
  StateClient{smarkSuspect} <- getClient
  -- Not @ScreenContent@, because indexing in level's data.
  Level{ltile=PointArray.Array{avector}, lembed} <- getLevel drawnLevelId
  totVisible <- totalVisible <$> getPerFid drawnLevelId
  frameStatus <- drawFrameStatus drawnLevelId
  let dis :: PointI -> ContentId TileKind -> Color.AttrCharW32
      {-# INLINE dis #-}
      dis pI tile =
        let TK.TileKind{tsymbol, tcolor, tcolor2} = okind cotile tile
            -- @smarkSuspect@ can be turned off easily, so let's overlay it
            -- over both visible and remembered tiles.
            fg :: Color.Color
            fg | smarkSuspect > 0
                 && Tile.isSuspect coTileSpeedup tile = Color.BrMagenta
               | smarkSuspect > 1
                 && Tile.isHideAs coTileSpeedup tile = Color.Magenta
               | -- Converting maps is cheaper than converting points
                 -- and this function is a bottleneck, so we hack a bit.
                 pI `IS.member` ES.enumSetToIntSet totVisible
                 -- If all embeds spent, mark it with darker colour.
                 && not (Tile.isEmbed coTileSpeedup tile
                         && pI `IM.notMember`
                              EM.enumMapToIntMap lembed) = tcolor
               | otherwise = tcolor2
        in Color.attrChar2ToW32 fg tsymbol
      g :: PointI -> Word16 -> Word32
      g !pI !tile = Color.attrCharW32 $ dis pI (toContentId tile)
      caveVector :: U.Vector Word32
      caveVector = U.imap g avector
      messageVector =
        U.replicate rXmax (Color.attrCharW32 Color.spaceAttrW32)
      statusVector = U.fromListN (2 * rXmax) $ map Color.attrCharW32 frameStatus
  -- The vector package is so smart that the 3 vectors are not allocated
  -- separately at all, but written to the big vector at once.
  -- But even with double allocation it would be faster than writing
  -- to a mutable vector via @FrameForall@.
  return $ U.concat [messageVector, caveVector, statusVector]

drawFrameContent :: forall m. MonadClientUI m => LevelId -> m FrameForall
drawFrameContent drawnLevelId = do
  COps{corule=RuleContent{rXmax}} <- getsState scops
  SessionUI{smarkSmell} <- getSession
  -- Not @ScreenContent@, because indexing in level's data.
  Level{lsmell, ltime, lfloor} <- getLevel drawnLevelId
  itemToF <- getsState $ flip itemToFull
  let {-# INLINE viewItemBag #-}
      viewItemBag _ floorBag = case EM.toDescList floorBag of
        (iid, _kit) : _ -> viewItem $ itemToF iid
        [] -> error $ "lfloor not sparse" `showFailure` ()
      viewSmell :: PointI -> Time -> Color.AttrCharW32
      {-# INLINE viewSmell #-}
      viewSmell pI sml =
        let fg = toEnum $ pI `rem` 13 + 2
            smlt = smellTimeout `timeDeltaSubtract`
                     (sml `timeDeltaToFrom` ltime)
        in Color.attrChar2ToW32 fg (timeDeltaToDigit smellTimeout smlt)
      mapVAL :: forall a s. (PointI -> a -> Color.AttrCharW32) -> [(PointI, a)]
             -> FrameST s
      {-# INLINE mapVAL #-}
      mapVAL f l v = do
        let g :: (PointI, a) -> ST s ()
            g (!pI, !a0) = do
              let w = Color.attrCharW32 $ f pI a0
              VM.write v (pI + rXmax) w
        mapM_ g l
      -- We don't usually show embedded items, because normally we don't
      -- want them to clutter the display. If they are really important,
      -- the tile they reside on has special colours and changes as soon
      -- as the item disappears. In the remaining cases, the main menu
      -- UI setting for suspect terrain highlights most tiles with embeds.
      upd :: FrameForall
      upd = FrameForall $ \v -> do
        mapVAL viewItemBag (IM.assocs $ EM.enumMapToIntMap lfloor) v
        when smarkSmell $
          mapVAL viewSmell (filter ((> ltime) . snd)
                            $ IM.assocs $ EM.enumMapToIntMap lsmell) v
  return upd

drawFramePath :: forall m. MonadClientUI m => LevelId -> m FrameForall
drawFramePath drawnLevelId = do
 SessionUI{saimMode} <- getSession
 if isNothing saimMode then return $! FrameForall $ \_ -> return () else do
  COps{corule=RuleContent{rXmax, rYmax}, coTileSpeedup} <- getsState scops
  StateClient{seps} <- getClient
  -- Not @ScreenContent@, because pathing in level's map.
  Level{ltile=PointArray.Array{avector}} <- getLevel drawnLevelId
  totVisible <- totalVisible <$> getPerFid drawnLevelId
  mleader <- getsClient sleader
  mpos <- getsState $ \s -> bpos . (`getActorBody` s) <$> mleader
  xhairPosRaw <- xhairToPos
  let xhairPos = fromMaybe (fromMaybe originPoint mpos) xhairPosRaw
  bline <- case mleader of
    Just leader -> do
      Actor{bpos, blid} <- getsState $ getActorBody leader
      return $! if blid /= drawnLevelId
                then []
                else fromMaybe [] $ bla rXmax rYmax seps bpos xhairPos
    _ -> return []
  mpath <- maybe (return Nothing) (\aid -> do
    mtgtMPath <- getsClient $ EM.lookup aid . stargetD
    case mtgtMPath of
      Just TgtAndPath{tapPath=tapPath@(Just AndPath{pathGoal})}
        | pathGoal == xhairPos -> return tapPath
      _ -> getCachePath aid xhairPos) mleader
  assocsAtxhair <- getsState $ posToAidAssocs xhairPos drawnLevelId
  let lpath = if null bline then [] else maybe [] pathList mpath
      shiftedBTrajectory = case assocsAtxhair of
        (_, Actor{btrajectory = Just p, bpos = prPos}) : _->
          trajectoryToPath prPos (fst p)
        _ -> []
      shiftedLine = if null shiftedBTrajectory
                    then bline
                    else shiftedBTrajectory
      acOnPathOrLine :: Char.Char -> Point -> ContentId TileKind
                     -> Color.AttrCharW32
      acOnPathOrLine !ch !p0 !tile =
        let fgOnPathOrLine =
              case ( ES.member p0 totVisible
                   , Tile.isWalkable coTileSpeedup tile ) of
                _ | isUknownSpace tile -> Color.BrBlack
                _ | Tile.isSuspect coTileSpeedup tile -> Color.BrMagenta
                (True, True)   -> Color.BrGreen
                (True, False)  -> Color.BrCyan
                (False, True)  -> Color.Green
                (False, False) -> Color.Cyan
        in Color.attrChar2ToW32 fgOnPathOrLine ch
      mapVTL :: forall s. (Point -> ContentId TileKind -> Color.AttrCharW32)
             -> [Point]
             -> FrameST s
      mapVTL f l v = do
        let g :: Point -> ST s ()
            g !p0 = do
              let pI = fromEnum p0
                  tile = avector U.! pI
                  w = Color.attrCharW32 $ f p0 (toContentId tile)
              VM.write v (pI + rXmax) w
        mapM_ g l
      upd :: FrameForall
      upd = FrameForall $ \v -> do
        mapVTL (acOnPathOrLine ';') lpath v
        mapVTL (acOnPathOrLine '*') shiftedLine v  -- overwrites path
  return upd

drawFrameActor :: forall m. MonadClientUI m => LevelId -> m FrameForall
drawFrameActor drawnLevelId = do
  COps{corule=RuleContent{rXmax}} <- getsState scops
  SessionUI{sactorUI, sselected, sUIOptions} <- getSession
  -- Not @ScreenContent@, because indexing in level's data.
  Level{lbig, lproj} <- getLevel drawnLevelId
  SessionUI{saimMode} <- getSession
  side <- getsClient sside
  mleader <- getsClient sleader
  s <- getState
  let {-# INLINE viewBig #-}
      viewBig aid =
          let Actor{bhp, bfid, btrunk, bwatch} = getActorBody aid s
              ActorUI{bsymbol, bcolor} = sactorUI EM.! aid
              Item{jfid} = getItemBody btrunk s
              symbol | bhp > 0 = bsymbol
                     | otherwise = '%'
              dominated = maybe False (/= bfid) jfid
              leaderColor = if isJust saimMode
                            then Color.HighlightYellowAim
                            else Color.HighlightYellow
              bg = if | mleader == Just aid -> leaderColor
                      | bwatch == WSleep -> Color.HighlightGreen
                      | dominated -> if bfid == side  -- dominated by us
                                     then Color.HighlightWhite
                                     else Color.HighlightMagenta
                      | ES.member aid sselected -> Color.HighlightBlue
                      | otherwise -> Color.HighlightNone
              fg | bfid /= side || bhp <= 0 = bcolor
                 | otherwise =
                let (hpCheckWarning, calmCheckWarning) =
                      checkWarnings sUIOptions aid s
                in if hpCheckWarning || calmCheckWarning
                   then Color.Red
                   else bcolor
         in Color.attrCharToW32 $ Color.AttrChar Color.Attr{..} symbol
      {-# INLINE viewProj #-}
      viewProj as = case as of
        aid : _ ->
          let ActorUI{bsymbol, bcolor} = sactorUI EM.! aid
              bg = Color.HighlightNone
              fg = bcolor
         in Color.attrCharToW32 $ Color.AttrChar Color.Attr{..} bsymbol
        [] -> error $ "lproj not sparse" `showFailure` ()
      mapVAL :: forall a s. (a -> Color.AttrCharW32) -> [(PointI, a)]
             -> FrameST s
      {-# INLINE mapVAL #-}
      mapVAL f l v = do
        let g :: (PointI, a) -> ST s ()
            g (!pI, !a0) = do
              let w = Color.attrCharW32 $ f a0
              VM.write v (pI + rXmax) w
        mapM_ g l
      upd :: FrameForall
      upd = FrameForall $ \v -> do
        mapVAL viewProj (IM.assocs $ EM.enumMapToIntMap lproj) v
        mapVAL viewBig (IM.assocs $ EM.enumMapToIntMap lbig) v
          -- big actor overlay projectiles
  return upd

drawFrameExtra :: forall m. MonadClientUI m
               => ColorMode -> LevelId -> m FrameForall
drawFrameExtra dm drawnLevelId = do
  COps{corule=RuleContent{rXmax, rYmax}} <- getsState scops
  SessionUI{saimMode, smarkVision} <- getSession
  -- Not @ScreenContent@, because indexing in level's data.
  totVisible <- totalVisible <$> getPerFid drawnLevelId
  mxhairPos <- xhairToPos
  mtgtPos <- do
    mleader <- getsClient sleader
    case mleader of
      Nothing -> return Nothing
      Just leader -> do
        mtgt <- getsClient $ getTarget leader
        getsState $ aidTgtToPos leader drawnLevelId mtgt
  side <- getsClient sside
  factionD <- getsState sfactionD
  let visionMarks =
        if smarkVision
        then IS.toList $ ES.enumSetToIntSet totVisible
        else []
      backlightVision :: Color.AttrChar -> Color.AttrChar
      backlightVision ac = case ac of
        Color.AttrChar (Color.Attr fg _) ch ->
          Color.AttrChar (Color.Attr fg Color.HighlightGrey) ch
      writeSquare !hi (Color.AttrChar (Color.Attr fg bg) ch) =
        let hiUnlessLeader | bg == Color.HighlightYellow = bg
                           | otherwise = hi
        in Color.AttrChar (Color.Attr fg hiUnlessLeader) ch
      turnBW (Color.AttrChar _ ch) = Color.AttrChar Color.defAttr ch
      mapVL :: forall s. (Color.AttrChar -> Color.AttrChar) -> [PointI]
            -> FrameST s
      mapVL f l v = do
        let g :: PointI -> ST s ()
            g !pI = do
              w0 <- VM.read v (pI + rXmax)
              let w = Color.attrCharW32 . Color.attrCharToW32
                      . f . Color.attrCharFromW32 . Color.AttrCharW32 $ w0
              VM.write v (pI + rXmax) w
        mapM_ g l
      -- Here @rXmax@ and @rYmax@ are correct, because we are not
      -- turning the whole screen into black&white, but only the level map.
      lDungeon = [0..rXmax * rYmax - 1]
      xhairColor = if isJust saimMode
                   then Color.HighlightRedAim
                   else Color.HighlightRed
      locateStash (fid, fact) = case gstash fact of
        Just (lid, pos) | lid == drawnLevelId ->
          let stashColor = if fid == side
                           then Color.HighlightWhite
                           else Color.HighlightMagenta
          in Just (pos, stashColor)
        _ -> Nothing
      stashesToDisplay = mapMaybe locateStash $ EM.assocs factionD
      upd :: FrameForall
      upd = FrameForall $ \v -> do
        when (isJust saimMode) $ mapVL backlightVision visionMarks v
        case mtgtPos of
          Nothing -> return ()
          Just p -> mapVL (writeSquare Color.HighlightGrey) [fromEnum p] v
        mapM_ (\(pos, color) -> mapVL (writeSquare color) [fromEnum pos] v)
              stashesToDisplay
        case mxhairPos of  -- overwrites target
          Nothing -> return ()
          Just p -> mapVL (writeSquare xhairColor) [fromEnum p] v
        when (dm == ColorBW) $ mapVL turnBW lDungeon v
  return upd

drawFrameStatus :: MonadClientUI m => LevelId -> m AttrString
drawFrameStatus drawnLevelId = do
  cops@COps{corule=RuleContent{rXmax=_rXmax}} <- getsState scops
  SessionUI{sselected, saimMode, swaitTimes, sitemSel} <- getSession
  mleader <- getsClient sleader
  xhairPos <- xhairToPos
  mbfs <- maybe (return Nothing) (\aid -> Just <$> getCacheBfs aid) mleader
  (mhairDesc, mxhairHP, mxhairWatchfulness) <- targetDescXhair
  lvl <- getLevel drawnLevelId
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  (mblid, mbpos, mbodyUI) <- case mleader of
    Just leader -> do
      Actor{bpos, blid} <- getsState $ getActorBody leader
      bodyUI <- getsSession $ getActorUI leader
      return (Just blid, Just bpos, Just bodyUI)
    Nothing -> return (Nothing, Nothing, Nothing)
  let widthX = 80
      widthTgt = 39
      widthStatus = widthX - widthTgt - 1
      arenaStatus = drawArenaStatus cops lvl widthStatus
      leaderStatusWidth = 23
  leaderStatus <- drawLeaderStatus swaitTimes
  (selectedStatusWidth, selectedStatus)
    <- drawSelected drawnLevelId (widthStatus - leaderStatusWidth) sselected
  let speedStatusWidth = widthStatus - leaderStatusWidth - selectedStatusWidth
  speedDisplay <- case mleader of
    Nothing -> return []
    Just leader -> do
      actorMaxSk <- getsState $ getActorMaxSkills leader
      kitAssRaw <- getsState $ kitAssocs leader [CEqp, COrgan]
      let speed = Ability.getSk Ability.SkSpeed actorMaxSk
          unknownBonus = unknownSpeedBonus $ map (fst . snd) kitAssRaw
          speedString = displaySpeed speed ++ if unknownBonus then "?" else ""
          conditionBonus = conditionSpeedBonus $ map snd kitAssRaw
          cspeed = case compare conditionBonus 0 of
            EQ -> Color.White
            GT -> Color.Green
            LT -> Color.Red
      return $! map (Color.attrChar2ToW32 cspeed) speedString
  let speedStatus = if length speedDisplay >= speedStatusWidth
                    then []
                    else speedDisplay ++ [Color.spaceAttrW32]
      displayPathText mp mt =
        let (plen, llen) | Just target <- mp
                         , Just bfs <- mbfs
                         , Just bpos <- mbpos
                         , mblid == Just drawnLevelId
                         = ( fromMaybe 0 (accessBfs bfs target)
                           , chessDist bpos target )
                         | otherwise = (0, 0)
            pText | plen == 0 = ""
                  | otherwise = "p" <> tshow plen
            lText | llen == 0 = ""
                  | otherwise = "l" <> tshow llen
            text = fromMaybe (pText <+> lText) mt
        in if T.null text then "" else " " <> text
      -- The indicators must fit, they are the actual information.
      pathCsr = displayPathText xhairPos mxhairHP
      trimTgtDesc n t = assert (not (T.null t) && n > 2 `blame` (t, n)) $
        if T.length t <= n then t else T.take (n - 3) t <> "..."
      -- The indicators must fit, they are the actual information.
      widthXhairOrItem = widthTgt - T.length pathCsr
      nMember = MU.Ord $ 1 + sum (EM.elems $ gvictims fact)
      fallback = if MK.fleaderMode (gplayer fact) == MK.LeaderNull
                 then "This faction never picks a pointman"
                 else makePhrase
                        ["Waiting for", nMember, "team member to spawn"]
      leaderName bUI = trimTgtDesc (widthTgt - 10) (bname bUI)
      leaderBlurbLong = maybe fallback (\bUI ->
        "Pointman:" <+> leaderName bUI) mbodyUI
      leaderBlurbShort = maybe fallback leaderName mbodyUI
  ours <- getsState $ fidActorNotProjGlobalAssocs side
  ns <- getsState $ EM.size . getFactionStashBag side
  let na = length ours
      nl = ES.size $ ES.fromList $ map (blid . snd) ours
      -- To be replaced by something more useful.
      teamBlurb = textToAS $ trimTgtDesc widthTgt $
        makePhrase [ "Team:"
                   , MU.CarWs na "actor", "on"
                   , MU.CarWs nl "level" <> ","
                   , "stash", MU.Car ns ]
      markSleepTgtDesc
        | mxhairWatchfulness /= Just WSleep = textToAS
        | otherwise = textFgToAS Color.Green
      xhairBlurb =
        maybe
          teamBlurb (\t ->
            if isJust saimMode
            then textToAS "Crosshair:"
                 <+:> markSleepTgtDesc (trimTgtDesc (widthXhairOrItem - 11) t)
            else markSleepTgtDesc (trimTgtDesc widthXhairOrItem t))
          mhairDesc
      tgtOrItem
        | Just (iid, fromCStore, _) <- sitemSel
        , Just leader <- mleader
        = do
            b <- getsState $ getActorBody leader
            bag <- getsState $ getBodyStoreBag b fromCStore
            case iid `EM.lookup` bag of
              Nothing -> return (xhairBlurb, pathCsr)
              Just kit@(k, _) -> do
                localTime <- getsState $ getLocalTime (blid b)
                itemFull <- getsState $ itemToFull iid
                factionD <- getsState sfactionD
                let (name, powers) =
                      partItem (bfid b) factionD localTime itemFull kit
                    t = makePhrase [MU.Car1Ws k name, powers]
                return (textToAS $ "Item:" <+> trimTgtDesc (widthTgt - 6) t, "")
        | otherwise =
            return (xhairBlurb, pathCsr)
  (xhairLine, pathXhairOrNull) <- tgtOrItem
  damageStatus <- maybe (return []) (drawLeaderDamage widthTgt) mleader
  let damageStatusWidth = length damageStatus
      withForLeader = widthTgt - damageStatusWidth - 1
      leaderBottom =
        if | T.length leaderBlurbShort > withForLeader -> ""
           | T.length leaderBlurbLong > withForLeader -> leaderBlurbShort
           | otherwise -> leaderBlurbLong
      damageGap = blankAttrString
                  $ widthTgt - damageStatusWidth - T.length leaderBottom
      xhairGap = blankAttrString (widthTgt - T.length pathXhairOrNull
                                         - length xhairLine)
      xhairStatus = xhairLine ++ xhairGap ++ textToAS pathXhairOrNull
      selectedGap = blankAttrString (widthStatus - leaderStatusWidth
                                               - selectedStatusWidth
                                               - length speedStatus)
      status = arenaStatus
               <+:> xhairStatus
               <> selectedStatus ++ selectedGap ++ speedStatus ++ leaderStatus
               <+:> (textToAS leaderBottom ++ damageGap ++ damageStatus)
  -- Keep it at least partially lazy, to avoid allocating the whole list:
  return
#ifdef WITH_EXPENSIVE_ASSERTIONS
    $ assert (length status == 2 * _rXmax
              `blame` map Color.charFromW32 status)
#endif
        status

-- | Draw the whole screen: level map and status area.
drawHudFrame :: MonadClientUI m => ColorMode -> LevelId -> m PreFrame
drawHudFrame dm drawnLevelId = do
  baseTerrain <- drawFrameTerrain drawnLevelId
  updContent <- drawFrameContent drawnLevelId
  updPath <- drawFramePath drawnLevelId
  updActor <- drawFrameActor drawnLevelId
  updExtra <- drawFrameExtra dm drawnLevelId
  let upd = FrameForall $ \v -> do
        unFrameForall updContent v
        -- vty frontend is screen-reader friendly, so avoid visual fluff
        unless (frontendName == "vty") $ unFrameForall updPath v
        unFrameForall updActor v
        unFrameForall updExtra v
  return (baseTerrain, upd)

-- Comfortably accomodates 3-digit level numbers and 25-character
-- level descriptions (currently enforced max).
drawArenaStatus :: COps -> Level -> Int -> AttrString
drawArenaStatus COps{cocave}
                Level{lkind, ldepth=Dice.AbsDepth ld, lseen, lexpl}
                width =
  let ck = okind cocave lkind
      seenN = 100 * lseen `div` max 1 lexpl
      seenTxt | seenN >= 100 = "all"
              | otherwise = T.justifyLeft 3 ' ' (tshow seenN <> "%")
      lvlN = T.justifyLeft 2 ' ' (tshow ld)
      seenStatus = "[" <> seenTxt <+> "seen]"
  in textToAS $ T.justifyLeft width ' '
              $ T.take 29 (lvlN <+> T.justifyLeft 26 ' ' (cname ck))
                <+> seenStatus

drawLeaderStatus :: MonadClientUI m => Int -> m AttrString
drawLeaderStatus waitT = do
  time <- getsState stime
  let calmHeaderText = "Calm"
      hpHeaderText = "HP"
      slashes = ["/", "|", "\\", "|"]
      waitGlobal = timeFit time timeTurn
  sUIOptions <- getsSession sUIOptions
  mleader <- getsClient sleader
  case mleader of
    Just leader -> do
      b <- getsState $ getActorBody leader
      actorMaxSk <- getsState $ getActorMaxSkills leader
      (hpCheckWarning, calmCheckWarning)
        <- getsState $ checkWarnings sUIOptions leader
      bdark <- getsState $ \s -> not (actorInAmbient b s)
      let showTrunc x = let t = show x
                        in if length t > 3
                           then if x > 0 then "***" else "---"
                           else t
          waitSlash | bwatch b == WSleep = waitGlobal
                    | otherwise = abs waitT
          -- This is a valuable feedback for the otherwise hard to observe
          -- 'wait' command or for passing of time when sole leader sleeps.
          slashPick = slashes !! (max 0 waitSlash `mod` length slashes)
          addColor c = map (Color.attrChar2ToW32 c)
          checkDelta ResDelta{..}
            | fst resCurrentTurn < 0 || fst resPreviousTurn < 0
              = addColor Color.BrRed  -- alarming news have priority
            | snd resCurrentTurn > 0 || snd resPreviousTurn > 0
              = addColor Color.BrGreen
            | otherwise = stringToAS  -- only if nothing at all noteworthy
          checkSleep body resDelta
            | bwatch body == WSleep = addColor Color.Green
            | otherwise = checkDelta resDelta
          calmAddAttr = checkSleep b $ bcalmDelta b
          -- We only show ambient light, because in fact client can't tell
          -- if a tile is lit, because it it's seen it may be due to ambient
          -- or dynamic light or due to infravision.
          darkPick | bdark = "."
                   | otherwise = ":"
          calmHeader = calmAddAttr $ calmHeaderText <> darkPick
          maxCalm = max 0 $ Ability.getSk Ability.SkMaxCalm actorMaxSk
          calmText = showTrunc (bcalm b `divUp` oneM)
                     <> (if bdark then slashPick else "/")
                     <> showTrunc maxCalm
          bracePick | actorWaits b = "}"
                    | otherwise = ":"
          hpAddAttr = checkDelta $ bhpDelta b
          hpHeader = hpAddAttr $ hpHeaderText <> bracePick
          maxHP = max 0 $ Ability.getSk Ability.SkMaxHP actorMaxSk
          hpText = showTrunc (bhp b `divUp` oneM)
                   <> (if not bdark then slashPick else "/")
                   <> showTrunc maxHP
          justifyRight n t = replicate (n - length t) ' ' ++ t
          colorWarning w full | w = addColor Color.Red
                              | full = addColor Color.Magenta
                              | otherwise = stringToAS
      return $! calmHeader
                <> colorWarning calmCheckWarning (bcalm b > xM maxCalm)
                                (justifyRight 7 calmText)
                <+:> hpHeader
                <> colorWarning hpCheckWarning (bhp b > xM maxHP)
                                (justifyRight 7 hpText)
    Nothing -> do
      -- This is a valuable feedback for passing of time while faction
      -- leaderless and especially while temporarily actor-less..
      let slashPick = slashes !! (max 0 waitGlobal `mod` length slashes)
      return $! stringToAS (calmHeaderText ++ ":  --" ++ slashPick ++ "--")
                <+:> stringToAS (hpHeaderText <> ":  --/--")

drawLeaderDamage :: MonadClientUI m => Int -> ActorId -> m AttrString
drawLeaderDamage width leader = do
  kitAssRaw <- getsState $ kitAssocs leader [CEqp, COrgan]
  actorSk <- leaderSkillsClientUI
  actorMaxSk <- getsState $ getActorMaxSkills leader
  let hasTimeout itemFull =
        let arItem = aspectRecordFull itemFull
            timeout = IA.aTimeout arItem
        in timeout > 0
      hasEffect itemFull =
        any IK.forApplyEffect $ IK.ieffects $ itemKind itemFull
      ppDice :: (Int, ItemFullKit) -> [(Bool, AttrString)]
      ppDice (nch, (itemFull, (k, _))) =
        let tdice = show $ IK.idamage $ itemKind itemFull
            tdiceEffect = if hasEffect itemFull
                          then map Char.toUpper tdice
                          else tdice
        in if hasTimeout itemFull
           then replicate (k - nch)
                  (False, map (Color.attrChar2ToW32 Color.Cyan) tdiceEffect)
                ++ replicate nch
                     (True, map (Color.attrChar2ToW32 Color.BrCyan) tdiceEffect)
           else [(True, map (Color.attrChar2ToW32 Color.BrBlue) tdiceEffect)]
      lbonus :: AttrString
      lbonus =
        let bonusRaw = Ability.getSk Ability.SkHurtMelee actorMaxSk
            bonus = min 200 $ max (-200) bonusRaw
            unknownBonus = unknownMeleeBonus $ map (fst . snd) kitAssRaw
            tbonus = if bonus == 0
                     then if unknownBonus then "+?" else ""
                     else (if bonus > 0 then "+" else "")
                          <> show bonus
                          <> (if bonus /= bonusRaw then "$" else "")
                          <> if unknownBonus then "%?" else "%"
            conditionBonus = conditionMeleeBonus $ map snd kitAssRaw
            cbonus = case compare conditionBonus 0 of
              EQ -> Color.White
              GT -> Color.Green
              LT -> Color.Red
        in map (Color.attrChar2ToW32 cbonus) tbonus
  let kitAssOnlyWeapons =
        filter (IA.checkFlag Ability.Meleeable
                . aspectRecordFull . fst . snd) kitAssRaw
  discoBenefit <- getsClient sdiscoBenefit
  strongest <- map (second snd . snd) <$>
    pickWeaponM True (Just discoBenefit) kitAssOnlyWeapons actorSk leader
  let (lT, lRatherNoT) = span (hasTimeout . fst . snd) strongest
      strongestToDisplay = lT ++ take 1 lRatherNoT
      lToDisplay = concatMap ppDice strongestToDisplay
      (ldischarged, lrest) = span (not . fst) lToDisplay
      lWithBonus = case map snd lrest of
        [] -> []  -- unlikely; means no timeout-free organ
        l1 : rest -> (l1 ++ lbonus) : rest
      lFlat = intercalate [Color.spaceAttrW32]
              $ map snd ldischarged ++ lWithBonus
      lFits = if length lFlat > width
              then take (width - 3) lFlat ++ stringToAS "..."
              else lFlat
  return $! lFits

drawSelected :: MonadClientUI m
             => LevelId -> Int -> ES.EnumSet ActorId -> m (Int, AttrString)
drawSelected drawnLevelId width selected = do
  mleader <- getsClient sleader
  side <- getsClient sside
  sactorUI <- getsSession sactorUI
  ours <- getsState $ filter (not . bproj . snd)
                      . inline actorAssocs (== side) drawnLevelId
  let oursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) ours
      viewOurs (aid, Actor{bhp, bwatch}, ActorUI{bsymbol, bcolor}) =
        -- Sleep considered before being selected, because sleeping
        -- actors can't move, so selection is mostly irrelevant.
        -- Domination not considered at all, because map already shows it
        -- and so here is the only place where selection is conveyed.
        let bg = if | mleader == Just aid -> Color.HighlightYellow
                    | bwatch == WSleep -> Color.HighlightGreen
                    | ES.member aid selected -> Color.HighlightBlue
                    | otherwise -> Color.HighlightNone
            sattr = Color.Attr {Color.fg = bcolor, bg}
        in Color.attrCharToW32 $ Color.AttrChar sattr
           $ if bhp > 0 then bsymbol else '%'
      maxViewed = width - 2
      len = length oursUI
      star = let fg = case ES.size selected of
                   0 -> Color.BrBlack
                   n | n == len -> Color.BrWhite
                   _ -> Color.defFG
                 char = if len > maxViewed then '$' else '*'
             in Color.attrChar2ToW32 fg char
      viewed = map viewOurs $ take maxViewed
               $ sortOn keySelected oursUI
  return (min width (len + 2), [star] ++ viewed ++ [Color.spaceAttrW32])

checkWarningHP :: UIOptions -> ActorId -> Int64 -> State -> Bool
checkWarningHP UIOptions{uhpWarningPercent} leader hp s =
  let actorMaxSk = getActorMaxSkills leader s
      maxHp = Ability.getSk Ability.SkMaxHP actorMaxSk
  in hp <= xM (uhpWarningPercent * maxHp `div` 100)

checkWarningCalm :: UIOptions -> ActorId -> Int64 -> State -> Bool
checkWarningCalm UIOptions{uhpWarningPercent} leader calm s =
  let b = getActorBody leader s
      actorMaxSk = getActorMaxSkills leader s
      isImpression iid =
        maybe False (> 0) $ lookup "impressed" $ IK.ifreq $ getIidKind iid s
      isImpressed = any isImpression $ EM.keys $ borgan b
      maxCalm = Ability.getSk Ability.SkMaxCalm actorMaxSk
  in calm <= xM (uhpWarningPercent * maxCalm `div` 100)
     && isImpressed

checkWarnings :: UIOptions -> ActorId -> State -> (Bool, Bool)
checkWarnings uiOptions leader s =
  let b = getActorBody leader s
  in ( checkWarningHP uiOptions leader (bhp b) s
     , checkWarningCalm uiOptions leader (bcalm b) s )
