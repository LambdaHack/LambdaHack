-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.DrawM
  ( targetDescLeader, drawBaseFrame
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Ord
import qualified Data.Text as T

import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.BfsM
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor as Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified NLP.Miniutter.English as MU

targetDesc :: MonadClientUI m => Maybe Target -> m (Text, Maybe Text)
targetDesc target = do
  lidV <- viewedLevelUI
  mleader <- getsClient _sleader
  case target of
    Just (TEnemy aid _) -> do
      side <- getsClient sside
      b <- getsState $ getActorBody aid
      maxHP <- sumOrganEqpClient IK.EqpSlotAddMaxHP aid
      let percentage = 100 * bhp b `div` xM (max 5 maxHP)
          stars | percentage < 20  = "[____]"
                | percentage < 40  = "[*___]"
                | percentage < 60  = "[**__]"
                | percentage < 80  = "[***_]"
                | otherwise        = "[****]"
          hpIndicator = if bfid b == side then Nothing else Just stars
      return (bname b, hpIndicator)
    Just (TEnemyPos _ lid p _) -> do
      let hotText = if lid == lidV
                    then "hot spot" <+> tshow p
                    else "a hot spot on level" <+> tshow (abs $ fromEnum lid)
      return (hotText, Nothing)
    Just (TPoint lid p) -> do
      pointedText <-
        if lid == lidV
        then do
          bag <- getsState $ getCBag (CFloor lid p)
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

-- TODO: split up and generally rewrite.
-- | Draw the whole screen: level map and status area.
-- Pass at most a single page if overlay of text unchanged
-- to the frontends to display separately or overlay over map,
-- depending on the frontend.
drawBaseFrame :: MonadClientUI m => ColorMode -> LevelId -> m SingleFrame
drawBaseFrame dm drawnLevelId = do
  cops <- getsState scops
  SessionUI{sselected, saimMode, smarkVision, smarkSmell, swaitTimes}
    <- getSession
  sitemSel <- getsSession sitemSel
  mleader <- getsClient _sleader
  tgtPos <- leaderTgtToPos
  xhairPos <- xhairToPos
  let anyPos = fromMaybe originPoint xhairPos
        -- if xhair invalid, e.g., on a wrong level; @draw@ ignores it later on
      pathFromLeader leader = Just <$> getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  (tgtDesc, mtargetHP) <- maybe (return ("------", Nothing)) targetDescLeader mleader
  (xhairDesc, mxhairHP) <- targetDescXhair
  s <- getState
  cli@StateClient{ seps, sexplored, smarkSuspect}
    <- getClient
  per <- getPerFid drawnLevelId
  let Kind.COps{cotile=cotile@Kind.Ops{okind=tokind, ouniqGroup}} = cops
      (lvl@Level{lxsize, lysize, lsmell, ltime}) = sdungeon s EM.! drawnLevelId
      (bline, mblid, mbpos) = case (xhairPos, mleader) of
        (Just xhair, Just leader) ->
          let Actor{bpos, blid} = getActorBody leader s
          in if blid /= drawnLevelId
             then ( [], Just blid, Just bpos )
             else ( maybe [] (delete xhair)
                    $ bla lxsize lysize seps bpos xhair
                  , Just blid
                  , Just bpos )
        _ -> ([], Nothing, Nothing)
      deleteXhair = maybe id (\xhair -> delete xhair) xhairPos
      mpath = maybe Nothing (\(_, mp) -> if null bline
                                         then Nothing
                                         else deleteXhair <$> mp) bfsmpath
      actorsHere = actorAssocs (const True) drawnLevelId s
      xhairHere = find (\(_, m) -> xhairPos == Just (Actor.bpos m))
                        actorsHere
      shiftedBTrajectory = case xhairHere of
        Just (_, Actor{btrajectory = Just p, bpos = prPos}) ->
          deleteXhair $ trajectoryToPath prPos (fst p)
        _ -> []
      unknownId = ouniqGroup "unknown space"
      dis pos0 =
        let tile = lvl `at` pos0
            tk = tokind tile
            floorBag = EM.findWithDefault EM.empty pos0 $ lfloor lvl
            (itemSlots, _) = sslots cli
            bagItemSlots = EM.filter (`EM.member` floorBag) itemSlots
            floorIids = EM.elems bagItemSlots  -- first slot will be shown
            sml = EM.findWithDefault timeZero pos0 lsmell
            smlt = sml `timeDeltaToFrom` ltime
            viewActor Actor{bsymbol, bcolor, bhp, bproj} =
              (symbol, Color.Attr{fg=bcolor, bg=Color.defBG})
             where
              symbol | bhp <= 0 && not bproj = '%'
                     | otherwise = bsymbol
            rainbow p = Color.defAttr {Color.fg =
                                         toEnum $ fromEnum p `rem` 14 + 1}
            -- smarkSuspect is an optional overlay, so let's overlay it
            -- over both visible and invisible tiles.
            vcolor
              | smarkSuspect && Tile.isSuspect cotile tile = Color.BrCyan
              | vis = TK.tcolor tk
              | otherwise = TK.tcolor2 tk
            fgOnPathOrLine = case (vis, Tile.isWalkable cotile tile) of
              _ | tile == unknownId -> Color.BrBlack
              _ | Tile.isSuspect cotile tile -> Color.BrCyan
              (True, True)   -> Color.BrGreen
              (True, False)  -> Color.BrRed
              (False, True)  -> Color.Green
              (False, False) -> Color.Red
            atttrOnPathOrLine = Color.defAttr {Color.fg = fgOnPathOrLine}
            maidBody = find (\(_, m) -> pos0 == Actor.bpos m) actorsHere
            (char, attr0) =
              case snd <$> maidBody of
                _ | isJust saimMode
                    && (elem pos0 bline || elem pos0 shiftedBTrajectory) ->
                  ('*', atttrOnPathOrLine)  -- line takes precedence over path
                _ | isJust saimMode
                    && maybe False (elem pos0) mpath ->
                  (';', atttrOnPathOrLine)
                Just m -> viewActor m
                _ | smarkSmell && sml > ltime ->
                  (timeDeltaToDigit smellTimeout smlt, rainbow pos0)
                  | otherwise ->
                  case floorIids of
                    [] -> (TK.tsymbol tk, Color.defAttr {Color.fg=vcolor})
                    iid : _ -> viewItem $ getItemBody iid s
            vis = ES.member pos0 $ totalVisible per
            maid = fst <$> maidBody
            a = case dm of
                  ColorBW -> Color.defAttr
                  ColorFull -> if | mleader == maid && isJust maid ->
                                    attr0 {Color.bg = Color.BrRed}
                                  | Just pos0 == xhairPos ->
                                    attr0 {Color.bg = Color.BrYellow}
                                  | maybe False (`ES.member` sselected) maid ->
                                    attr0 {Color.bg = Color.BrBlue}
                                  | smarkVision && vis ->
                                    attr0 {Color.bg = Color.Blue}
                                  | otherwise ->
                                    attr0
        in Color.AttrChar a char
      widthX = 80
      widthTgt = 39
      widthStats = widthX - widthTgt
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
      xhairGap = T.replicate (widthTgt - T.length pathCsr
                                        - T.length xhairText) " "
      xhairStatus = toAttrLine $ xhairText <> xhairGap <> pathCsr
      minLeaderStatusWidth = 19  -- covers 3-digit HP
  selectedStatus <- drawSelected drawnLevelId
                                 (widthStats - minLeaderStatusWidth)
                                 sselected
  leaderStatus <- drawLeaderStatus swaitTimes
                                   (widthStats - length selectedStatus)
  damageStatus <- drawLeaderDamage (widthStats - length leaderStatus
                                               - length selectedStatus)
  nameStatus <- drawPlayerName (widthStats - length leaderStatus
                                           - length selectedStatus
                                           - length damageStatus)
  let tgtOrItem n = do
        let tgtBlurb = "Target:" <+> trimTgtDesc n tgtDesc
        case (sitemSel, mleader) of
          (Just (fromCStore, iid), Just leader) -> do  -- TODO: factor out
            bag <- getsState $ getActorBag leader fromCStore
            case iid `EM.lookup` bag of
              Nothing -> return $! tgtBlurb
              Just kit@(k, _) -> do
                b <- getsState $ getActorBody leader
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
      statusGap = toAttrLine $ T.replicate (widthStats - length leaderStatus
                                                       - length selectedStatus
                                                       - length damageStatus
                                                       - length nameStatus) " "
      -- The indicators must fit, they are the actual information.
      pathTgt = displayPathText tgtPos mtargetHP
  targetText <- tgtOrItem $ widthTgt - T.length pathTgt - 8
  let targetGap = T.replicate (widthTgt - T.length pathTgt
                                        - T.length targetText) " "
      targetStatus = toAttrLine $ targetText <> targetGap <> pathTgt
      sfBottom =
        [ arenaStatus ++ xhairStatus
        , selectedStatus ++ nameStatus ++ statusGap
          ++ damageStatus ++ leaderStatus
          ++ targetStatus ]
      fLine y =
        let f l x = let ac = dis $ Point x y in ac : l
        in foldl' f [] [lxsize-1,lxsize-2..0]
      emptyLine = toAttrLine $ T.replicate lxsize " "
      singleFrame =
        let f l y = let !line = fLine y in line : l
        in emptyLine : foldl' f [] [lysize-1,lysize-2..0] ++ sfBottom
  return $! SingleFrame{..}

-- Comfortably accomodates 3-digit level numbers and 25-character
-- level descriptions (currently enforced max).
drawArenaStatus :: Bool -> Level -> Int -> AttrLine
drawArenaStatus explored Level{ldepth=AbsDepth ld, ldesc, lseen, lclear} width =
  let seenN = 100 * lseen `div` max 1 lclear
      seenTxt | explored || seenN >= 100 = "all"
              | otherwise = T.justifyLeft 3 ' ' (tshow seenN <> "%")
      lvlN = T.justifyLeft 2 ' ' (tshow ld)
      seenStatus = "[" <> seenTxt <+> "seen] "
  in toAttrLine $ T.justifyLeft width ' '
                $ T.take 29 (lvlN <+> T.justifyLeft 26 ' ' ldesc) <+> seenStatus

drawLeaderStatus :: MonadClient m => Int -> Int -> m AttrLine
drawLeaderStatus waitT width = do
  mleader <- getsClient _sleader
  s <- getState
  let addColor c t = map (Color.AttrChar $ Color.Attr c Color.defBG)
                         (T.unpack t)
      maxLeaderStatusWidth = 23  -- covers 3-digit HP and 2-digit Calm
      (calmHeaderText, hpHeaderText) = if width < maxLeaderStatusWidth
                                       then ("C", "H")
                                       else ("Calm", "HP")
  case mleader of
    Just leader -> do
      activeItems <- activeItemsClient leader
      let (darkL, bracedL, hpDelta, calmDelta,
           ahpS, bhpS, acalmS, bcalmS) =
            let b@Actor{bhp, bcalm} = getActorBody leader s
                amaxHP = sumSlotNoFilter IK.EqpSlotAddMaxHP activeItems
                amaxCalm = sumSlotNoFilter IK.EqpSlotAddMaxCalm activeItems
            in ( not (actorInAmbient b s)
               , braced b, bhpDelta b, bcalmDelta b
               , tshow $ max 0 amaxHP, tshow (bhp `divUp` oneM)
               , tshow $ max 0 amaxCalm, tshow (bcalm `divUp` oneM))
          -- This is a valuable feedback for the otherwise hard to observe
          -- 'wait' command.
          slashes = ["/", "|", "\\", "|"]
          slashPick = slashes !! (max 0 (waitT - 1) `mod` length slashes)
          checkDelta ResDelta{..}
            | resCurrentTurn < 0 || resPreviousTurn < 0
              = addColor Color.BrRed  -- alarming news have priority
            | resCurrentTurn > 0 || resPreviousTurn > 0
              = addColor Color.BrGreen
            | otherwise = toAttrLine  -- only if nothing at all noteworthy
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
      return $! calmHeader <> toAttrLine (T.justifyRight 6 ' ' calmText <> " ")
                <> hpHeader <> toAttrLine (T.justifyRight 6 ' ' hpText <> " ")
    Nothing -> return $! toAttrLine $ calmHeaderText <> ": --/-- "
                                   <> hpHeaderText <> ": --/-- "

drawLeaderDamage :: MonadClient m => Int -> m AttrLine
drawLeaderDamage width = do
  mleader <- getsClient _sleader
  let addColor t = map (Color.AttrChar $ Color.Attr Color.BrCyan Color.defBG)
                   (T.unpack t)
  stats <- case mleader of
    Just leader -> do
      actorSk <- actorSkillsClient leader
      b <- getsState $ getActorBody leader
      localTime <- getsState $ getLocalTime (blid b)
      allAssocs <- fullAssocsClient leader [CEqp, COrgan]
      let activeItems = map snd allAssocs
          calmE = calmEnough b $ map snd allAssocs
          forced = assert (not $ bproj b) False
          permitted = permittedPrecious calmE forced
          preferredPrecious = either (const False) id . permitted
          strongest = strongestMelee False localTime allAssocs
          strongestPreferred = filter (preferredPrecious . snd . snd) strongest
          damage = case strongestPreferred of
            _ | EM.findWithDefault 0 Ability.AbMelee actorSk <= 0 -> "0"
            [] -> "0"
            (_average, (_, itemFull)) : _ ->
              let getD :: IK.Effect -> Maybe Dice.Dice -> Maybe Dice.Dice
                  getD (IK.Hurt dice) acc = Just $ dice + fromMaybe 0 acc
                  getD (IK.Burn dice) acc = Just $ dice + fromMaybe 0 acc
                  getD _ acc = acc
                  mdice = case itemDisco itemFull of
                    Just ItemDisco{itemAE=Just ItemAspectEffect{jeffects}} ->
                      foldr getD Nothing jeffects
                    Just ItemDisco{itemKind} ->
                      foldr getD Nothing (IK.ieffects itemKind)
                    Nothing -> Nothing
                  tdice = case mdice of
                    Nothing -> "0"
                    Just dice -> tshow dice
                  bonus = sumSlotNoFilter IK.EqpSlotAddHurtMelee activeItems
                  unknownBonus = unknownMelee activeItems
                  tbonus = if bonus == 0
                           then if unknownBonus then "+?" else ""
                           else (if bonus > 0 then "+" else "")
                                <> tshow bonus
                                <> if unknownBonus then "%?" else "%"
             in tdice <> tbonus
      return $! damage
    Nothing -> return ""
  return $! if T.null stats || T.length stats >= width then []
            else addColor $ stats <> " "

-- TODO: colour some texts using the faction's colour
drawSelected :: MonadClient m
             => LevelId -> Int -> ES.EnumSet ActorId -> m AttrLine
drawSelected drawnLevelId width selected = do
  mleader <- getsClient _sleader
  side <- getsClient sside
  allOurs <- getsState $ filter ((== side) . bfid) . EM.elems . sactorD
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) drawnLevelId
  let viewOurs (aid, Actor{bsymbol, bcolor, bhp}) =
        let bg = if | mleader == Just aid -> Color.BrRed
                    | ES.member aid selected -> Color.BrBlue
                    | otherwise -> Color.defBG
            sattr = Color.Attr {Color.fg = bcolor, bg}
        in Color.AttrChar sattr $ if bhp > 0 then bsymbol else '%'
      maxViewed = width - 2
      star = let fg = case ES.size selected of
                   0 -> Color.BrBlack
                   n | n == length ours -> Color.BrWhite
                   _ -> Color.defFG
                 char = if length ours > maxViewed then '$' else '*'
             in Color.AttrChar Color.defAttr{Color.fg} char
      viewed = map viewOurs $ take maxViewed
               $ sortBy (comparing keySelected) ours
      -- Don't show anything if the only actor in the dungeon is the leader.
      -- He's clearly highlighted on the level map, anyway.
      party = if length allOurs == 1 && length ours == 1 || null ours
              then []
              else [star] ++ viewed ++ toAttrLine " "
  return $! party

drawPlayerName :: MonadClient m => Int -> m AttrLine
drawPlayerName width = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let nameN n t =
        let fitWords [] = []
            fitWords l@(_ : rest) = if sum (map T.length l) + length l - 1 > n
                                    then fitWords rest
                                    else l
        in T.unwords $ reverse $ fitWords $ reverse $ T.words t
      ourName = nameN (width - 1) $ fname $ gplayer fact
  return $! if T.null ourName || T.length ourName >= width
            then []
            else toAttrLine $ ourName <> " "
