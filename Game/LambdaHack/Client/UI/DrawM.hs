{-# LANGUAGE RankNTypes, TupleSections #-}
-- {-# OPTIONS_GHC -fprof-auto #-}
-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.DrawM
  ( targetDescLeader, drawBaseFrame
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , targetDesc, targetDescXhair, drawFrameBody, drawFrameStatus
  , drawArenaStatus, drawLeaderStatus, drawLeaderDamage, drawSelected
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Monad.ST.Strict
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Ord
import qualified Data.Text as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.New as New
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Word
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.BfsM
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import qualified Game.LambdaHack.Common.KindOps as KindOps
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.TileKind (TileKind, isUknownSpace)
import qualified Game.LambdaHack.Content.TileKind as TK

targetDesc :: MonadClientUI m => Maybe Target -> m (Text, Maybe Text)
targetDesc target = do
  arena <- getArenaUI
  lidV <- viewedLevelUI
  mleader <- getsClient _sleader
  case target of
    Just (TEnemy aid _) -> do
      side <- getsClient sside
      b <- getsState $ getActorBody aid
      actorAspect <- getsClient sactorAspect
      let ar = case EM.lookup aid actorAspect of
            Just aspectRecord -> aspectRecord
            Nothing -> assert `failure` aid
          percentage = 100 * bhp b `div` xM (max 5 $ aMaxHP ar)
          stars | percentage < 20  = "[____]"
                | percentage < 40  = "[*___]"
                | percentage < 60  = "[**__]"
                | percentage < 80  = "[***_]"
                | otherwise        = "[****]"
          hpIndicator = if bfid b == side then Nothing else Just stars
      return (bname b, hpIndicator)
    Just (TEnemyPos _ lid p _) -> do
      let hotText = if lid == lidV && arena == lidV
                    then "hot spot" <+> tshow p
                    else "a hot spot on level" <+> tshow (abs $ fromEnum lid)
      return (hotText, Nothing)
    Just (TPoint lid p) -> do
      pointedText <-
        if lid == lidV && arena == lidV
        then do
          bag <- getsState $ getFloorBag lid p
          case EM.assocs bag of
            [] -> return $! "exact spot" <+> tshow p
            [(iid, kit@(k, _))] -> do
              localTime <- getsState $ getLocalTime lid
              itemToF <- itemToFullClient
              let (_, name, stats) = partItem CGround localTime (itemToF iid kit)
              return $! makePhrase $ if k == 1
                                     then [name, stats]  -- "a sword" too wordy
                                     else [MU.CarWs k name, stats]
            _ -> return $! "many items at" <+> tshow p
        else return $! "an exact spot on level" <+> tshow (abs $ fromEnum lid)
      return (pointedText, Nothing)
    Just TVector{} ->
      case mleader of
        Nothing -> return ("a relative shift", Nothing)
        Just aid -> do
          tgtPos <- aidTgtToPos aid lidV target
          let invalidMsg = "an invalid relative shift"
              validMsg p = "shift to" <+> tshow p
          return (maybe invalidMsg validMsg tgtPos, Nothing)
    Nothing -> return ("crosshair location", Nothing)

targetDescLeader :: MonadClientUI m => ActorId -> m (Text, Maybe Text)
targetDescLeader leader = do
  tgt <- getsClient $ getTarget leader
  targetDesc tgt

targetDescXhair :: MonadClientUI m => m (Text, Maybe Text)
targetDescXhair = do
  sxhair <- getsClient sxhair
  targetDesc $ Just sxhair

drawFrameBody :: forall m. MonadClientUI m
              => ColorMode -> LevelId -> New.New U.Vector Word32
              -> m (New.New U.Vector Word32)
{-# NOINLINE drawFrameBody #-}
drawFrameBody dm drawnLevelId new = do
  Kind.COps{coTileSpeedup, cotile=Kind.Ops{okind}} <- getsState scops
  SessionUI{sselected, saimMode, smarkVision, smarkSmell} <- getSession
  StateClient{seps, smarkSuspect} <- getClient
  Level{ lxsize, lysize, lsmell, ltime, lfloor, lactor, lhidden
       , ltile=PointArray.Array{avector} }
    <- getLevel drawnLevelId
  let doMarkSuspect = smarkSuspect && lhidden > 0
  mleader <- getsClient _sleader
  xhairPosRaw <- xhairToPos
  let xhairPos = fromMaybe originPoint xhairPosRaw
  s <- getState
  totVisible <- totalVisible <$> getPerFid drawnLevelId
  -- The basic drawing routine.
  let dis :: Int -> Kind.Id TileKind -> Color.AttrCharW32
      {-# NOINLINE dis #-}
      dis !pI !tile =
        let !p0 = PointArray.punindex lxsize pI
            viewSmell sml =
              let fg = toEnum $ fromEnum p0 `rem` 14 + 1
                  smlt = sml `timeDeltaToFrom` ltime
              in Color.attrCharToW32
                 $ Color.AttrChar (Color.defAttr {Color.fg})
                                  (timeDeltaToDigit smellTimeout smlt)
            viewActor aid Actor{bsymbol, bcolor, bhp, bproj} =
              Color.attrCharToW32
              $ Color.AttrChar Color.Attr{fg=bcolor, bg} symbol
             where symbol | bhp > 0 || bproj = bsymbol
                          | otherwise = '%'
                   bg = case mleader of
                     Just leader | aid == leader -> Color.BrRed
                     _ -> if aid `ES.notMember` sselected
                          then Color.defBG
                          else Color.BrBlue
            viewItemBag floorBag = case EM.toDescList floorBag of
              (iid, _) : _ -> Color.attrCharToW32 $ viewItem $ getItemBody iid s
              [] -> assert `failure` "lfloor not sparse" `twith` ()
            -- TODO: instead, write tiles first, then overwrite with others
            viewTile = case okind tile of
              TK.TileKind{tsymbol, tcolor, tcolor2} ->
                -- smarkSuspect can be turned off easily, so let's overlay it
                -- over both visible and remembered tiles.
                if | doMarkSuspect
                     && Tile.isSuspect coTileSpeedup tile ->
                     Color.attrChar2ToW32 Color.BrCyan tsymbol
                   | ES.member p0 totVisible ->
                     Color.attrChar2ToW32 tcolor tsymbol
                   | otherwise ->
                     Color.attrChar2ToW32 tcolor2 tsymbol
        in case EM.findWithDefault [] p0 lactor of
          [] -> case EM.lookup p0 lsmell of
            Just sml | sml > ltime && smarkSmell -> viewSmell sml
            _ -> case EM.lookup p0 lfloor of
              Nothing -> viewTile
              Just floorBag -> viewItemBag floorBag
          aid : _ -> viewActor aid (getActorBody aid s)
  -- Aiming mode drawing routine.
  bline <- case mleader of
    Just leader -> do
      Actor{bpos, blid} <- getsState $ getActorBody leader
      return $! if blid /= drawnLevelId
                then []
                else maybe [] (delete xhairPos)
                     $ bla lxsize lysize seps bpos xhairPos
    _ -> return []
  let bfsAndPathFromLeader leader = Just <$> getCacheBfsAndPath leader xhairPos
      pathFromLeader leader = (Just . (,NoPath)) <$> getCacheBfs leader
  bfsmpath <- if isJust saimMode
              then maybe (return Nothing) bfsAndPathFromLeader mleader
              else maybe (return Nothing) pathFromLeader mleader
  let deleteXhair = delete xhairPos
      mpath = if null bline then []
              else maybe [] (\(_, mp) -> case mp of
                NoPath -> []
                AndPath {pathList} -> deleteXhair pathList) bfsmpath
      xhairHere = find (\(_, m) -> xhairPos == bpos m)
                       (actorAssocs (const True) drawnLevelId s)
      shiftedBTrajectory = case xhairHere of
        Just (_, Actor{btrajectory = Just p, bpos = prPos}) ->
          deleteXhair $ trajectoryToPath prPos (fst p)
        _ -> []
      aimCharAtrr :: Int -> Kind.Id TileKind -> Color.AttrCharW32
                  -> Color.AttrCharW32
      aimCharAtrr !pI !tile !ac =
        let p0 = PointArray.punindex lxsize pI
            fgOnPathOrLine =
              case ( ES.member p0 totVisible
                      , Tile.isWalkable coTileSpeedup tile ) of
                _ | isUknownSpace tile -> Color.BrBlack
                _ | Tile.isSuspect coTileSpeedup tile -> Color.BrCyan
                (True, True)   -> Color.BrGreen
                (True, False)  -> Color.BrRed
                (False, True)  -> Color.Green
                (False, False) -> Color.Red
            attrOnPathOrLine = Color.defAttr {Color.fg = fgOnPathOrLine}
        in if | elem p0 bline || elem p0 shiftedBTrajectory ->
                Color.attrCharToW32 $ Color.AttrChar attrOnPathOrLine '*'
              | elem p0 mpath ->
                Color.attrCharToW32 $ Color.AttrChar attrOnPathOrLine ';'
              | smarkVision && ES.member p0 totVisible ->
                case Color.attrCharFromW32 ac of
                  Color.AttrChar (Color.Attr fg _) ch ->
                    Color.attrCharToW32
                    $ Color.AttrChar (Color.Attr fg Color.Blue) ch
              | otherwise -> ac
      writeXhair !(Color.AttrChar (Color.Attr fg _) ch) =
        Color.AttrChar (Color.Attr fg Color.BrYellow) ch
      turnBW !(Color.AttrChar _ ch) = Color.AttrChar Color.defAttr ch
  let mapV :: forall s. (Color.AttrChar -> Color.AttrChar) -> [Int]
           -> G.Mutable U.Vector s Word32 -> ST s ()
      mapV !f !l !v = do
        let g :: Int -> ST s ()
            g !pI = do
              w0 <- VM.read v pI
              let w = Color.attrCharW32 . Color.attrCharToW32
                      . f . Color.attrCharFromW32 . Color.AttrCharW32 $ w0
              VM.write v pI w
        mapM_ g l
      mapVT :: forall s. (Int -> Kind.Id TileKind -> Color.AttrCharW32)
            -> G.Mutable U.Vector s Word32 -> ST s ()
      {-# INLINE mapVT #-}
      mapVT f v = do
        let g :: Int -> Word8 -> ST s ()
            g !pI !tile = do
              let w = Color.attrCharW32 $ f pI (KindOps.Id tile)
              VM.write v (pI + lxsize) w
        U.imapM_ g avector
      mapVTA :: forall s. (Int -> Kind.Id TileKind -> Color.AttrCharW32
                                                   -> Color.AttrCharW32)
             -> G.Mutable U.Vector s Word32 -> ST s ()
      {-# INLINE mapVTA #-}
      mapVTA f v = do
        let g :: Int -> Word8 -> ST s ()
            g !pI !tile = do
              w0 <- VM.read v pI
              let w = Color.attrCharW32 $ f pI (KindOps.Id tile)
                                        $ Color.AttrCharW32 w0
              VM.write v (pI + lxsize) w
        U.imapM_ g avector
      lDungeon = [lxsize..lxsize * (lysize - 1)]
      upd :: forall s. G.Mutable U.Vector s Word32 -> ST s ()
      {-# NOINLINE upd #-}
      upd v = do
        mapVT dis v
        when (isJust saimMode) $ mapVTA aimCharAtrr v
        when (dm == ColorBW) $ mapV turnBW lDungeon v
        case xhairPosRaw of
          Nothing -> return ()
          Just xhairP -> mapV writeXhair [PointArray.pindex lxsize xhairP] v
  return $! New.modify upd new

drawFrameStatus :: MonadClientUI m => LevelId -> m AttrLine
drawFrameStatus drawnLevelId = do
  SessionUI{sselected, saimMode, swaitTimes, sitemSel} <- getSession
  mleader <- getsClient _sleader
  tgtPos <- leaderTgtToPos
  xhairPos <- xhairToPos
  let anyPos = fromMaybe originPoint xhairPos
        -- if xhair invalid, e.g., on a wrong level; @draw@ ignores it later on
      bfsAndPathFromLeader leader = Just <$> getCacheBfsAndPath leader anyPos
      pathFromLeader leader = (Just . (,NoPath)) <$> getCacheBfs leader
  bfsmpath <- if isJust saimMode
              then maybe (return Nothing) bfsAndPathFromLeader mleader
              else maybe (return Nothing) pathFromLeader mleader
  (tgtDesc, mtargetHP) <-
    maybe (return ("------", Nothing)) targetDescLeader mleader
  (xhairDesc, mxhairHP) <- targetDescXhair
  sexplored <- getsClient sexplored
  lvl <- getLevel drawnLevelId
  (mblid, mbpos) <- case mleader of
    Just leader -> do
      Actor{bpos, blid} <- getsState $ getActorBody leader
      return (Just blid, Just bpos)
    Nothing -> return (Nothing, Nothing)
  let widthX = 80
      widthTgt = 39
      widthStats = widthX - widthTgt - 1
      arenaStatus = drawArenaStatus (ES.member drawnLevelId sexplored) lvl
                                    widthStats
      displayPathText mp mt =
        let (plen, llen) = case (mp, bfsmpath, mbpos) of
              (Just target, Just (bfs, _), Just bpos)
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
      trimTgtDesc n t = assert (not (T.null t) && n > 2) $
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
  let statusGap = emptyAttrLine (widthStats - leaderStatusWidth
                                            - selectedStatusWidth
                                            - length damageStatus)
      tgtOrItem n = do
        let tgtBlurb = "Target:" <+> trimTgtDesc n tgtDesc
        case (sitemSel, mleader) of
          (Just (fromCStore, iid), Just leader) -> do  -- TODO: factor out
            b <- getsState $ getActorBody leader
            bag <- getsState $ getBodyStoreBag b fromCStore
            case iid `EM.lookup` bag of
              Nothing -> return $! tgtBlurb
              Just kit@(k, _) -> do
                localTime <- getsState $ getLocalTime (blid b)
                itemToF <- itemToFullClient
                let (_, name, stats) =
                      partItem fromCStore localTime (itemToF iid kit)
                    t = makePhrase
                        $ if k == 1
                          then [name, stats]  -- "a sword" too wordy
                          else [MU.CarWs k name, stats]
                return $! "Object:" <+> trimTgtDesc n t
          _ -> return $! tgtBlurb
      -- The indicators must fit, they are the actual information.
      pathTgt = displayPathText tgtPos mtargetHP
  targetText <- tgtOrItem $ widthTgt - T.length pathTgt - 8
  let targetGap = emptyAttrLine (widthTgt - T.length pathTgt
                                          - T.length targetText)
      targetStatus = textToAL targetText ++ targetGap ++ textToAL pathTgt
  return $! concat [ arenaStatus <+:> xhairStatus
                   , selectedStatus ++ statusGap ++ damageStatus ++ leaderStatus
                     <+:> targetStatus ]

-- | Draw the whole screen: level map and status area.
-- Pass at most a single page if overlay of text unchanged
-- to the frontends to display separately or overlay over map,
-- depending on the frontend.
drawBaseFrame :: MonadClientUI m => ColorMode -> LevelId -> m SingleFrame
{-# NOINLINE drawBaseFrame #-}
drawBaseFrame dm drawnLevelId = do
  let lxsize = fst normalLevelBound + 1  -- TODO
      lysize = snd normalLevelBound + 1
      canvasLength = lysize + 3
      new = New.create $ VM.replicate (lxsize * canvasLength)
                                      (Color.attrCharW32 Color.spaceAttrW32)
  withBody <- drawFrameBody dm drawnLevelId new
  frameStatus <- drawFrameStatus drawnLevelId
  let f v (pI, ac32) = VM.write v pI (Color.attrCharW32 ac32)
      !_A = assert (length frameStatus == 2 * lxsize
                    `blame` map Color.charFromW32 frameStatus) ()
      l = zip [lxsize * (lysize + 1)..] frameStatus
      withAll = New.modify (\v -> mapM_ (f v) l) withBody
      singleFrame = PointArray.Array lxsize canvasLength (G.new withAll)
  return $! SingleFrame{..}

-- Comfortably accomodates 3-digit level numbers and 25-character
-- level descriptions (currently enforced max).
drawArenaStatus :: Bool -> Level -> Int -> AttrLine
drawArenaStatus explored Level{ldepth=AbsDepth ld, ldesc, lseen, lclear} width =
  let seenN = 100 * lseen `div` max 1 lclear
      seenTxt | explored || seenN >= 100 = "all"
              | otherwise = T.justifyLeft 3 ' ' (tshow seenN <> "%")
      lvlN = T.justifyLeft 2 ' ' (tshow ld)
      seenStatus = "[" <> seenTxt <+> "seen]"
  in textToAL $ T.justifyLeft width ' '
              $ T.take 29 (lvlN <+> T.justifyLeft 26 ' ' ldesc) <+> seenStatus

drawLeaderStatus :: MonadClient m => Int -> m AttrLine
drawLeaderStatus waitT = do
  let calmHeaderText = "Calm"
      hpHeaderText = "HP"
  mleader <- getsClient _sleader
  case mleader of
    Just leader -> do
      actorAspect <- getsClient sactorAspect
      s <- getState
      let ar = case EM.lookup leader actorAspect of
            Just aspectRecord -> aspectRecord
            Nothing -> assert `failure`leader
          showTrunc :: Show a => a -> String
          showTrunc = (\t -> if length t > 3 then "***" else t) . show
          (darkL, bracedL, hpDelta, calmDelta,
           ahpS, bhpS, acalmS, bcalmS) =
            let b@Actor{bhp, bcalm} = getActorBody leader s
            in ( not (actorInAmbient b s)
               , braced b, bhpDelta b, bcalmDelta b
               , showTrunc $ aMaxHP ar, showTrunc (bhp `divUp` oneM)
               , showTrunc $ aMaxCalm ar, showTrunc (bcalm `divUp` oneM))
          -- This is a valuable feedback for the otherwise hard to observe
          -- 'wait' command.
          slashes = ["/", "|", "\\", "|"]
          slashPick = slashes !! (max 0 (waitT - 1) `mod` length slashes)
          addColor c t =
            map (Color.attrCharToW32 . Color.AttrChar (Color.Attr c Color.defBG)) t
          checkDelta ResDelta{..}
            | resCurrentTurn < 0 || resPreviousTurn < 0
              = addColor Color.BrRed  -- alarming news have priority
            | resCurrentTurn > 0 || resPreviousTurn > 0
              = addColor Color.BrGreen
            | otherwise = stringToAL  -- only if nothing at all noteworthy
          calmAddAttr = checkDelta calmDelta
          darkPick | darkL   = "."
                   | otherwise = ":"
          calmHeader = calmAddAttr $ calmHeaderText <> darkPick
          calmText = bcalmS <> (if darkL then slashPick else "/") <> acalmS
          bracePick | bracedL   = "}"
                    | otherwise = ":"
          hpAddAttr = checkDelta hpDelta
          hpHeader = hpAddAttr $ hpHeaderText <> bracePick
          hpText = bhpS <> (if bracedL then slashPick else "/") <> ahpS
          justifyRight n t = replicate (n - length t) ' ' ++ t
      return $! calmHeader <> stringToAL (justifyRight 7 calmText)
                <+:> hpHeader <> stringToAL (justifyRight 7 hpText)
    Nothing -> return $! stringToAL (calmHeaderText ++ ":  --/--")
                         <+:> stringToAL (hpHeaderText <> ":  --/--")

drawLeaderDamage :: MonadClient m => Int -> m AttrLine
drawLeaderDamage width = do
  mleader <- getsClient _sleader
  let addColor s =
        map (Color.attrCharToW32 . Color.AttrChar (Color.Attr Color.BrCyan Color.defBG)) s
  stats <- case mleader of
    Just leader -> do
      allAssocsRaw <- fullAssocsClient leader [CEqp, COrgan]
      let allAssocs = filter (isMelee . itemBase . snd) allAssocsRaw
      actorSk <- actorSkillsClient leader
      actorAspect <- getsClient sactorAspect
      strongest <- pickWeaponM allAssocs actorSk actorAspect leader False
      let damage = case strongest of
            [] -> "0"
            (_averageDmg, (_, itemFull)) : _ ->
              let getD :: IK.Effect -> Maybe Dice.Dice -> Maybe Dice.Dice
                  getD (IK.Hurt dice) acc = Just $ dice + fromMaybe 0 acc
                  getD (IK.Burn dice) acc = Just $ dice + fromMaybe 0 acc
                  getD _ acc = acc
                  mdice = case itemDisco itemFull of
                    Just ItemDisco{itemKind=IK.ItemKind{IK.ieffects}} ->
                      foldr getD Nothing ieffects
                    Nothing -> Nothing
                  tdice = case mdice of
                    Nothing -> "0"
                    Just dice -> show dice
                  bonus = aHurtMelee $ actorAspect EM.! leader
                  unknownBonus = unknownMelee $ map snd allAssocs
                  tbonus = if bonus == 0
                           then if unknownBonus then "+?" else ""
                           else (if bonus > 0 then "+" else "")
                                <> show bonus
                                <> if unknownBonus then "%?" else "%"
             in tdice <> tbonus
      return $! damage
    Nothing -> return ""
  return $! if null stats || length stats >= width then []
            else addColor $ stats <> " "

-- TODO: colour some texts using the faction's colour
drawSelected :: MonadClient m
             => LevelId -> Int -> ES.EnumSet ActorId -> m (Int, AttrLine)
drawSelected drawnLevelId width selected = do
  mleader <- getsClient _sleader
  side <- getsClient sside
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) drawnLevelId
  let viewOurs (aid, Actor{bsymbol, bcolor, bhp}) =
        let bg = if | mleader == Just aid -> Color.BrRed
                    | ES.member aid selected -> Color.BrBlue
                    | otherwise -> Color.defBG
            sattr = Color.Attr {Color.fg = bcolor, bg}
        in Color.attrCharToW32 $ Color.AttrChar sattr $ if bhp > 0 then bsymbol else '%'
      maxViewed = width - 2
      len = length ours
      star = let fg = case ES.size selected of
                   0 -> Color.BrBlack
                   n | n == len -> Color.BrWhite
                   _ -> Color.defFG
                 char = if len > maxViewed then '$' else '*'
             in Color.attrCharToW32 $ Color.AttrChar Color.defAttr{Color.fg} char
      viewed = map viewOurs $ take maxViewed
               $ sortBy (comparing keySelected) ours
  return (min width (len + 2), [star] ++ viewed ++ [Color.spaceAttrW32])
