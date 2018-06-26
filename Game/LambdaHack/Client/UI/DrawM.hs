-- {-# OPTIONS_GHC -fprof-auto #-}
-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.DrawM
  ( targetDescLeader, drawBaseFrame
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , targetDesc, targetDescXhair, drawFrameTerrain, drawFrameContent
  , drawFramePath, drawFrameActor, drawFrameExtra, drawFrameStatus
  , drawArenaStatus, drawLeaderStatus, drawLeaderDamage, drawSelected
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.Monad.ST.Strict
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Ord
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Word (Word16)
import           Game.LambdaHack.Client.UI.UIOptions
import           GHC.Exts (inline)
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.BfsM
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import           Game.LambdaHack.Common.ContentData
import qualified Game.LambdaHack.Common.Dice as Dice
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
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.CaveKind (cname)
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.ModeKind as MK
import           Game.LambdaHack.Content.TileKind (TileKind, isUknownSpace)
import qualified Game.LambdaHack.Content.TileKind as TK

targetDesc :: MonadClientUI m => Maybe Target -> m (Maybe Text, Maybe Text)
targetDesc mtarget = do
  arena <- getArenaUI
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  case mtarget of
    Just (TEnemy aid _) -> do
      side <- getsClient sside
      b <- getsState $ getActorBody aid
      bUI <- getsSession $ getActorUI aid
      ar <- getsState $ getActorAspect aid
      let percentage = 100 * bhp b `div` xM (max 5 $ IA.aMaxHP ar)
          chs n = "[" <> T.replicate n "*"
                      <> T.replicate (4 - n) "_" <> "]"
          stars = chs $ fromEnum $ max 0 $ min 4 $ percentage `div` 20
          hpIndicator = if bfid b == side then Nothing else Just stars
      return (Just $ bname bUI, hpIndicator)
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
                let (_, _, name, stats) =
                      partItem side factionD localTime itemFull kit
                return $! makePhrase
                          $ if k == 1
                            then [name, stats]  -- "a sword" too wordy
                            else [MU.CarWs k name, stats]
              _ -> return $! "many items at" <+> tshow p
          else return $! "an exact spot on level" <+> tshow (abs $ fromEnum lid)
        return (Just pointedText, Nothing)
    Just target@TVector{} ->
      case mleader of
        Nothing -> return (Just "a relative shift", Nothing)
        Just aid -> do
          tgtPos <- getsState $ aidTgtToPos aid lidV target
          let invalidMsg = "an invalid relative shift"
              validMsg p = "shift to" <+> tshow p
          return (Just $ maybe invalidMsg validMsg tgtPos, Nothing)
    Nothing -> return (Nothing, Nothing)

targetDescLeader :: MonadClientUI m => ActorId -> m (Maybe Text, Maybe Text)
targetDescLeader leader = do
  tgt <- getsClient $ getTarget leader
  targetDesc tgt

targetDescXhair :: MonadClientUI m => m (Text, Maybe Text)
targetDescXhair = do
  sxhair <- getsSession sxhair
  first fromJust <$> targetDesc (Just sxhair)

drawFrameTerrain :: forall m. MonadClientUI m => LevelId -> m FrameForall
drawFrameTerrain drawnLevelId = do
  COps{coTileSpeedup, cotile} <- getsState scops
  StateClient{smarkSuspect} <- getClient
  Level{lxsize, ltile=PointArray.Array{avector}} <- getLevel drawnLevelId
  totVisible <- totalVisible <$> getPerFid drawnLevelId
  let dis :: Int -> ContentId TileKind -> Color.AttrCharW32
      {-# INLINE dis #-}
      dis pI tile = case okind cotile tile of
        TK.TileKind{tsymbol, tcolor, tcolor2} ->
          -- Passing @p0@ as arg in place of @pI@ is much more costly.
          let p0 :: Point
              {-# INLINE p0 #-}
              p0 = PointArray.punindex lxsize pI
              -- @smarkSuspect@ can be turned off easily, so let's overlay it
              -- over both visible and remembered tiles.
              fg :: Color.Color
              {-# INLINE fg #-}
              fg | smarkSuspect > 0
                   && Tile.isSuspect coTileSpeedup tile = Color.BrMagenta
                 | smarkSuspect > 1
                   && Tile.isHideAs coTileSpeedup tile = Color.Magenta
                 | ES.member p0 totVisible = tcolor
                 | otherwise = tcolor2
          in Color.attrChar2ToW32 fg tsymbol
      mapVT :: forall s. (Int -> ContentId TileKind -> Color.AttrCharW32)
            -> FrameST s
      {-# INLINE mapVT #-}
      mapVT f v = do
        let g :: Int -> Word16 -> ST s ()
            g !pI !tile = do
              let w = Color.attrCharW32 $ f pI (ContentId tile)
              VM.write v (pI + lxsize) w
        U.imapM_ g avector
      upd :: FrameForall
      upd = FrameForall $ \v -> mapVT dis v  -- should be eta-expanded; lazy
  return upd

drawFrameContent :: forall m. MonadClientUI m => LevelId -> m FrameForall
drawFrameContent drawnLevelId = do
  SessionUI{smarkSmell} <- getSession
  Level{lxsize, lsmell, ltime, lfloor} <- getLevel drawnLevelId
  itemToF <- getsState $ flip itemToFull
  let {-# INLINE viewItemBag #-}
      viewItemBag _ floorBag = case EM.toDescList floorBag of
        (iid, _kit) : _ -> viewItem $ itemToF iid
        [] -> error $ "lfloor not sparse" `showFailure` ()
      viewSmell :: Point -> Time -> Color.AttrCharW32
      {-# INLINE viewSmell #-}
      viewSmell p0 sml =
        let fg = toEnum $ fromEnum p0 `rem` 14 + 1
            smlt = sml `timeDeltaToFrom` ltime
        in Color.attrChar2ToW32 fg (timeDeltaToDigit smellTimeout smlt)
      mapVAL :: forall a s. (Point -> a -> Color.AttrCharW32) -> [(Point, a)]
             -> FrameST s
      {-# INLINE mapVAL #-}
      mapVAL f l v = do
        let g :: (Point, a) -> ST s ()
            g (!p0, !a0) = do
              let pI = PointArray.pindex lxsize p0
                  w = Color.attrCharW32 $ f p0 a0
              VM.write v (pI + lxsize) w
        mapM_ g l
      upd :: FrameForall
      upd = FrameForall $ \v -> do
        mapVAL viewItemBag (EM.assocs lfloor) v
        when smarkSmell $
          mapVAL viewSmell (filter ((> ltime) . snd) $ EM.assocs lsmell) v
  return upd

drawFramePath :: forall m. MonadClientUI m => LevelId -> m FrameForall
drawFramePath drawnLevelId = do
 SessionUI{saimMode} <- getSession
 if isNothing saimMode then return $! FrameForall $ \_ -> return () else do
  COps{coTileSpeedup} <- getsState scops
  StateClient{seps} <- getClient
  Level{lxsize, lysize, ltile=PointArray.Array{avector}}
    <- getLevel drawnLevelId
  totVisible <- totalVisible <$> getPerFid drawnLevelId
  mleader <- getsClient sleader
  xhairPosRaw <- xhairToPos
  let xhairPos = fromMaybe originPoint xhairPosRaw
  s <- getState
  bline <- case mleader of
    Just leader -> do
      Actor{bpos, blid} <- getsState $ getActorBody leader
      return $! if blid /= drawnLevelId
                then []
                else fromMaybe [] $ bla lxsize lysize seps bpos xhairPos
    _ -> return []
  mpath <- maybe (return Nothing) (\aid -> Just <$> do
    mtgtMPath <- getsClient $ EM.lookup aid . stargetD
    case mtgtMPath of
      Just TgtAndPath{tapPath=tapPath@AndPath{pathGoal}}
        | pathGoal == xhairPos -> return tapPath
      _ -> getCachePath aid xhairPos) mleader
  let lpath = if null bline then []
              else maybe [] (\case
                NoPath -> []
                AndPath {pathList} -> pathList) mpath
      xhairHere = find (\(_, m) -> xhairPos == bpos m)
                       (inline actorAssocs (const True) drawnLevelId s)
      shiftedBTrajectory = case xhairHere of
        Just (_, Actor{btrajectory = Just p, bpos = prPos}) ->
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
                (True, False)  -> Color.BrRed
                (False, True)  -> Color.Green
                (False, False) -> Color.Red
        in Color.attrChar2ToW32 fgOnPathOrLine ch
      mapVTL :: forall s. (Point -> ContentId TileKind -> Color.AttrCharW32)
             -> [Point]
             -> FrameST s
      mapVTL f l v = do
        let g :: Point -> ST s ()
            g !p0 = do
              let pI = PointArray.pindex lxsize p0
                  tile = avector U.! pI
                  w = Color.attrCharW32 $ f p0 (ContentId tile)
              VM.write v (pI + lxsize) w
        mapM_ g l
      upd :: FrameForall
      upd = FrameForall $ \v -> do
        mapVTL (acOnPathOrLine ';') lpath v
        mapVTL (acOnPathOrLine '*') shiftedLine v  -- overwrites path
  return upd

drawFrameActor :: forall m. MonadClientUI m => LevelId -> m FrameForall
drawFrameActor drawnLevelId = do
  SessionUI{sactorUI, sselected, sUIOptions} <- getSession
  Level{lxsize, lactor} <- getLevel drawnLevelId
  side <- getsClient sside
  mleader <- getsClient sleader
  s <- getState
  let {-# INLINE viewActor #-}
      viewActor _ as = case as of
        aid : _ ->
          let Actor{bhp, bproj, bfid, btrunk} = getActorBody aid s
              ActorUI{bsymbol, bcolor} = sactorUI EM.! aid
              Item{jfid} = getItemBody btrunk s
              symbol | bhp > 0 || bproj = bsymbol
                     | otherwise = '%'
              dominated = maybe False (/= bfid) jfid
              bg = if bproj then Color.HighlightNone else case mleader of
                Just leader | aid == leader -> Color.HighlightRed
                _ -> if | aid `ES.member` sselected -> Color.HighlightBlue
                        | dominated -> if bfid == side  -- dominated by us
                                       then Color.HighlightWhite
                                       else Color.HighlightMagenta
                        | otherwise -> Color.HighlightNone
              fg | bfid /= side || bproj || bhp <= 0 = bcolor
                 | otherwise =
                let (hpCheckWarning, calmCheckWarning) =
                      checkWarnings sUIOptions aid s
                in if hpCheckWarning || calmCheckWarning
                   then Color.Red
                   else bcolor
         in Color.attrCharToW32 $ Color.AttrChar Color.Attr{..} symbol
        [] -> error $ "lactor not sparse" `showFailure` ()
      mapVAL :: forall a s. (Point -> a -> Color.AttrCharW32) -> [(Point, a)]
             -> FrameST s
      {-# INLINE mapVAL #-}
      mapVAL f l v = do
        let g :: (Point, a) -> ST s ()
            g (!p0, !a0) = do
              let pI = PointArray.pindex lxsize p0
                  w = Color.attrCharW32 $ f p0 a0
              VM.write v (pI + lxsize) w
        mapM_ g l
      upd :: FrameForall
      upd = FrameForall $ \v ->
        mapVAL viewActor (EM.assocs lactor) v
  return upd

drawFrameExtra :: forall m. MonadClientUI m
               => ColorMode -> LevelId -> m FrameForall
drawFrameExtra dm drawnLevelId = do
  SessionUI{saimMode, smarkVision} <- getSession
  Level{lxsize, lysize} <- getLevel drawnLevelId
  totVisible <- totalVisible <$> getPerFid drawnLevelId
  mxhairPos <- xhairToPos
  mtgtPos <- do
    mleader <- getsClient sleader
    case mleader of
      Nothing -> return Nothing
      Just leader -> do
        mtgt <- getsClient $ getTarget leader
        case mtgt of
          Nothing -> return Nothing
          Just tgt -> getsState $ aidTgtToPos leader drawnLevelId tgt
  let visionMarks =
        if smarkVision
        then map (PointArray.pindex lxsize) $ ES.toList totVisible
        else []
      backlightVision :: Color.AttrChar -> Color.AttrChar
      backlightVision ac = case ac of
        Color.AttrChar (Color.Attr fg _) ch ->
          Color.AttrChar (Color.Attr fg Color.HighlightGrey) ch
      writeSquare !hi (Color.AttrChar (Color.Attr fg bg) ch) =
        let hiUnlessLeader | bg == Color.HighlightRed = bg
                           | otherwise = hi
        in Color.AttrChar (Color.Attr fg hiUnlessLeader) ch
      turnBW (Color.AttrChar _ ch) = Color.AttrChar Color.defAttr ch
      mapVL :: forall s. (Color.AttrChar -> Color.AttrChar) -> [Int]
            -> FrameST s
      mapVL f l v = do
        let g :: Int -> ST s ()
            g !pI = do
              w0 <- VM.read v (pI + lxsize)
              let w = Color.attrCharW32 . Color.attrCharToW32
                      . f . Color.attrCharFromW32 . Color.AttrCharW32 $ w0
              VM.write v (pI + lxsize) w
        mapM_ g l
      lDungeon = [0..lxsize * lysize - 1]
      upd :: FrameForall
      upd = FrameForall $ \v -> do
        when (isJust saimMode) $ mapVL backlightVision visionMarks v
        case mtgtPos of
          Nothing -> return ()
          Just p -> mapVL (writeSquare Color.HighlightGrey)
                          [PointArray.pindex lxsize p] v
        case mxhairPos of  -- overwrites target
          Nothing -> return ()
          Just p -> mapVL (writeSquare Color.HighlightYellow)
                          [PointArray.pindex lxsize p] v
        when (dm == ColorBW) $ mapVL turnBW lDungeon v
  return upd

drawFrameStatus :: MonadClientUI m => LevelId -> m AttrLine
drawFrameStatus drawnLevelId = do
  cops <- getsState scops
  SessionUI{sselected, saimMode, swaitTimes, sitemSel} <- getSession
  mleader <- getsClient sleader
  xhairPos <- xhairToPos
  tgtPos <- leaderTgtToPos
  mbfs <- maybe (return Nothing) (\aid -> Just <$> getCacheBfs aid) mleader
  (mtgtDesc, mtargetHP) <-
    maybe (return (Nothing, Nothing)) targetDescLeader mleader
  (xhairDesc, mxhairHP) <- targetDescXhair
  lvl <- getLevel drawnLevelId
  (mblid, mbpos, mbodyUI) <- case mleader of
    Just leader -> do
      Actor{bpos, blid} <- getsState $ getActorBody leader
      bodyUI <- getsSession $ getActorUI leader
      return (Just blid, Just bpos, Just bodyUI)
    Nothing -> return (Nothing, Nothing, Nothing)
  let widthX = 80
      widthTgt = 39
      widthStats = widthX - widthTgt - 1
      arenaStatus = drawArenaStatus cops lvl widthStats
      displayPathText mp mt =
        let (plen, llen) = case (mp, mbfs, mbpos) of
              (Just target, Just bfs, Just bpos)
                | mblid == Just drawnLevelId ->
                  (fromMaybe 0 (accessBfs bfs target), chessDist bpos target)
              _ -> (0, 0)
            pText | plen == 0 = ""
                  | otherwise = "p" <> tshow plen
            lText | llen == 0 = ""
                  | otherwise = "l" <> tshow llen
            text = fromMaybe (pText <+> lText) mt
        in if T.null text then "" else " " <> text
      -- The indicators must fit, they are the actual information.
      pathCsr = displayPathText xhairPos mxhairHP
      trimTgtDesc n t = assert (not (T.null t) && n > 2 `blame` (t, n)) $
        if T.length t <= n then t
        else let ellipsis = "..."
                 fitsPlusOne = T.take (n - T.length ellipsis + 1) t
                 fits = if T.last fitsPlusOne == ' '
                        then T.init fitsPlusOne
                        else let lw = T.words fitsPlusOne
                             in T.unwords $ init lw
             in fits <> ellipsis
      xhairText =
        let n = widthTgt - T.length pathCsr - 8
        in (if isJust saimMode then "x-hair>" else "X-hair:")
           <+> trimTgtDesc n xhairDesc
      xhairGap = emptyAttrLine (widthTgt - T.length pathCsr
                                         - T.length xhairText)
      xhairStatus = textToAL xhairText ++ xhairGap ++ textToAL pathCsr
      leaderStatusWidth = 23
  leaderStatus <- drawLeaderStatus swaitTimes
  (selectedStatusWidth, selectedStatus)
    <- drawSelected drawnLevelId (widthStats - leaderStatusWidth) sselected
  damageStatus <- drawLeaderDamage (widthStats - leaderStatusWidth
                                               - selectedStatusWidth)
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let statusGap = emptyAttrLine (widthStats - leaderStatusWidth
                                            - selectedStatusWidth
                                            - length damageStatus)
      tgtOrItem n = do
        let fallback = if MK.fleaderMode (gplayer fact) == MK.LeaderNull
                       then "This faction never picks a leader"
                       else "Waiting for a team member to spawn"
            leaderName =
              maybe fallback (\body ->
                "Leader:" <+> trimTgtDesc n (bname body)) mbodyUI
            tgtBlurb = maybe leaderName (\t ->
              "Target:" <+> trimTgtDesc n t) mtgtDesc
        case (sitemSel, mleader) of
          (Just (iid, fromCStore, _), Just leader) -> do
            b <- getsState $ getActorBody leader
            bag <- getsState $ getBodyStoreBag b fromCStore
            case iid `EM.lookup` bag of
              Nothing -> return $! tgtBlurb
              Just kit@(k, _) -> do
                localTime <- getsState $ getLocalTime (blid b)
                itemFull <- getsState $ itemToFull iid
                factionD <- getsState sfactionD
                let (_, _, name, stats) =
                      partItem (bfid b) factionD localTime itemFull kit
                    t = makePhrase
                        $ if k == 1
                          then [name, stats]  -- "a sword" too wordy
                          else [MU.CarWs k name, stats]
                return $! "Item:" <+> trimTgtDesc n t
          _ -> return $! tgtBlurb
      -- The indicators must fit, they are the actual information.
      pathTgt = displayPathText tgtPos mtargetHP
  targetText <- tgtOrItem $ widthTgt - T.length pathTgt - 8
  let targetGap = emptyAttrLine (widthTgt - T.length pathTgt
                                          - T.length targetText)
      targetStatus = textToAL targetText ++ targetGap ++ textToAL pathTgt
  return $! arenaStatus <+:> xhairStatus
            <> selectedStatus ++ statusGap ++ damageStatus ++ leaderStatus
               <+:> targetStatus

-- | Draw the whole screen: level map and status area.
drawBaseFrame :: MonadClientUI m => ColorMode -> LevelId -> m FrameForall
drawBaseFrame dm drawnLevelId = do
  Level{lxsize, lysize} <- getLevel drawnLevelId
  updTerrain <- drawFrameTerrain drawnLevelId
  updContent <- drawFrameContent drawnLevelId
  updPath <- drawFramePath drawnLevelId
  updActor <- drawFrameActor drawnLevelId
  updExtra <- drawFrameExtra dm drawnLevelId
  frameStatus <- drawFrameStatus drawnLevelId
  let !_A = assert (length frameStatus == 2 * lxsize
                    `blame` map Color.charFromW32 frameStatus) ()
      upd = FrameForall $ \v -> do
        unFrameForall updTerrain v
        unFrameForall updContent v
        unFrameForall updPath v
        unFrameForall updActor v
        unFrameForall updExtra v
        unFrameForall (writeLine (lxsize * (lysize + 1)) frameStatus) v
  return upd

-- Comfortably accomodates 3-digit level numbers and 25-character
-- level descriptions (currently enforced max).
drawArenaStatus :: COps -> Level -> Int -> AttrLine
drawArenaStatus COps{cocave}
                Level{lkind, ldepth=Dice.AbsDepth ld, lseen, lexpl}
                width =
  let ck = okind cocave lkind
      seenN = 100 * lseen `div` max 1 lexpl
      seenTxt | seenN >= 100 = "all"
              | otherwise = T.justifyLeft 3 ' ' (tshow seenN <> "%")
      lvlN = T.justifyLeft 2 ' ' (tshow ld)
      seenStatus = "[" <> seenTxt <+> "seen]"
  in textToAL $ T.justifyLeft width ' '
              $ T.take 29 (lvlN <+> T.justifyLeft 26 ' ' (cname ck))
                <+> seenStatus

checkWarnings :: UIOptions -> ActorId -> State -> (Bool, Bool)
checkWarnings UIOptions{uhpWarningPercent} leader s =
  let b = getActorBody leader s
      ar = getActorAspect leader s
      isImpression iid =
        maybe False (> 0) $ lookup "impressed" $ IK.ifreq $ getIidKind iid s
      isImpressed = any isImpression $ EM.keys $ borgan b
      hpCheckWarning = bhp b <= xM (uhpWarningPercent * IA.aMaxHP ar `div` 100)
      calmCheckWarning =
        bcalm b <= xM (uhpWarningPercent * IA.aMaxCalm ar `div` 100)
        && isImpressed
  in (hpCheckWarning, calmCheckWarning)

drawLeaderStatus :: MonadClientUI m => Int -> m AttrLine
drawLeaderStatus waitT = do
  let calmHeaderText = "Calm"
      hpHeaderText = "HP"
  sUIOptions <- getsSession sUIOptions
  mleader <- getsClient sleader
  case mleader of
    Just leader -> do
      b <- getsState $ getActorBody leader
      ar <- getsState $ getActorAspect leader
      (hpCheckWarning, calmCheckWarning)
        <- getsState $ checkWarnings sUIOptions leader
      bdark <- getsState $ \s -> not (actorInAmbient b s)
      let showTrunc x = let t = show x
                        in if length t > 3
                           then if x > 0 then "***" else "---"
                           else t
          -- This is a valuable feedback for the otherwise hard to observe
          -- 'wait' command.
          slashes = ["/", "|", "\\", "|"]
          slashPick = slashes !! (max 0 waitT `mod` length slashes)
          addColor c = map (Color.attrChar2ToW32 c)
          checkDelta ResDelta{..}
            | fst resCurrentTurn < 0 || fst resPreviousTurn < 0
              = addColor Color.BrRed  -- alarming news have priority
            | snd resCurrentTurn > 0 || snd resPreviousTurn > 0
              = addColor Color.BrGreen
            | otherwise = stringToAL  -- only if nothing at all noteworthy
          calmAddAttr = checkDelta $ bcalmDelta b
          -- We only show ambient light, because in fact client can't tell
          -- if a tile is lit, because it it's seen it may be due to ambient
          -- or dynamic light or due to infravision.
          darkPick | bdark = "."
                   | otherwise = ":"
          calmHeader = calmAddAttr $ calmHeaderText <> darkPick
          calmText = showTrunc (bcalm b `divUp` oneM)
                     <> (if bdark || not (braced b)
                         then slashPick
                         else "/")
                     <> showTrunc (max 0 $ IA.aMaxCalm ar)
          bracePick | braced b  = "}"
                    | otherwise = ":"
          hpAddAttr = checkDelta $ bhpDelta b
          hpHeader = hpAddAttr $ hpHeaderText <> bracePick
          hpText = showTrunc (bhp b `divUp` oneM)
                   <> (if braced b || not bdark
                       then slashPick
                       else "/")
                   <> showTrunc (max 0 $ IA.aMaxHP ar)
          justifyRight n t = replicate (n - length t) ' ' ++ t
          colorWarning w = if w then addColor Color.Red else stringToAL
      return $! calmHeader
                <> colorWarning calmCheckWarning (justifyRight 7 calmText)
                <+:> hpHeader
                <> colorWarning hpCheckWarning (justifyRight 7 hpText)
    Nothing -> return $! stringToAL (calmHeaderText ++ ":  --/--")
                         <+:> stringToAL (hpHeaderText <> ":  --/--")

drawLeaderDamage :: MonadClientUI m => Int -> m AttrLine
drawLeaderDamage width = do
  mleader <- getsClient sleader
  (tdice, tbonus, cbonus) <- case mleader of
    Just leader -> do
      kitAssRaw <- getsState $ kitAssocs leader [CEqp, COrgan]
      actorSk <- leaderSkillsClientUI
      actorAspect <- getsState sactorAspect
      let kitAssOnlyWeapons =
            filter (IK.isMelee . itemKind . fst . snd) kitAssRaw
      strongest <- pickWeaponM Nothing kitAssOnlyWeapons actorSk leader
      let damage = case strongest of
            [] -> ("0", "", Color.White)
            (_, (_, (itemFull, _))) : _ ->
              let tdice = show $ IK.idamage $ itemKind itemFull
                  bonusRaw = IA.aHurtMelee $ actorAspect EM.! leader
                  bonus = min 200 $ max (-200) bonusRaw
                  unknownBonus = unknownMeleeBonus $ map (fst . snd) kitAssRaw
                  tbonus = if bonus == 0
                           then if unknownBonus then "+?" else ""
                           else (if bonus > 0 then "+" else "")
                                <> show bonus
                                <> (if bonus /= bonusRaw then "$" else "")
                                <> if unknownBonus then "%?" else "%"
                  tmpBonus = tmpMeleeBonus $ map snd kitAssRaw
                  cbonus = case compare tmpBonus 0 of
                    EQ -> Color.White
                    GT -> Color.Green
                    LT -> Color.Red
             in (tdice, tbonus, cbonus)
      return $! damage
    Nothing -> return ("", "", Color.White)
  let addColorDice = map (Color.attrChar2ToW32 Color.BrCyan)
      addColorBonus = map (Color.attrChar2ToW32 cbonus)
  return $! if null tdice || length tdice + length tbonus >= width then []
            else addColorDice tdice ++ addColorBonus tbonus
                 ++ [Color.spaceAttrW32]

drawSelected :: MonadClientUI m
             => LevelId -> Int -> ES.EnumSet ActorId -> m (Int, AttrLine)
drawSelected drawnLevelId width selected = do
  mleader <- getsClient sleader
  side <- getsClient sside
  sactorUI <- getsSession sactorUI
  ours <- getsState $ filter (not . bproj . snd)
                      . inline actorAssocs (== side) drawnLevelId
  let oursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) ours
      viewOurs (aid, Actor{bhp}, ActorUI{bsymbol, bcolor}) =
        let bg = if | mleader == Just aid -> Color.HighlightRed
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
               $ sortBy (comparing keySelected) oursUI
  return (min width (len + 2), [star] ++ viewed ++ [Color.spaceAttrW32])
