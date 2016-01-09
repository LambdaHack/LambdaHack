-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.DrawClient
  ( ColorMode(..)
  , draw
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List.Compat
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
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
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK

-- | Color mode for the display.
data ColorMode =
    ColorFull  -- ^ normal, with full colours
  | ColorBW    -- ^ black+white only

-- TODO: split up and generally rewrite.
-- | Draw the whole screen: level map and status area.
-- Pass at most a single page if overlay of text unchanged
-- to the frontends to display separately or overlay over map,
-- depending on the frontend.
draw :: MonadClient m
     => ColorMode -> LevelId
     -> Maybe Point -> Maybe Point
     -> Maybe (PointArray.Array BfsDistance, Maybe [Point])
     -> (Text, Maybe Text) -> (Text, Maybe Text)
     -> ES.EnumSet ActorId -> Maybe TgtMode -> Bool -> Bool -> Int
     -> m SingleFrame
draw dm drawnLevelId cursorPos tgtPos bfsmpathRaw
     (cursorDesc, mcursorHP) (targetDesc, mtargetHP)
     selected stgtMode smarkVision smarkSmell swaitTimes = do
  cops <- getsState scops
  mleader <- getsClient _sleader
  s <- getState
  cli@StateClient{ seps, sexplored, smarkSuspect}
    <- getClient
  per <- getPerFid drawnLevelId
  let Kind.COps{cotile=cotile@Kind.Ops{okind=tokind, ouniqGroup}} = cops
      (lvl@Level{lxsize, lysize, lsmell, ltime}) = sdungeon s EM.! drawnLevelId
      (bline, mblid, mbpos) = case (cursorPos, mleader) of
        (Just cursor, Just leader) ->
          let Actor{bpos, blid} = getActorBody leader s
          in if blid /= drawnLevelId
             then ( [], Just blid, Just bpos )
             else ( maybe [] (delete cursor)
                    $ bla lxsize lysize seps bpos cursor
                  , Just blid
                  , Just bpos )
        _ -> ([], Nothing, Nothing)
      deleteCursor = maybe id (\cursor -> delete cursor) cursorPos
      mpath = maybe Nothing (\(_, mp) -> if null bline
                                         then Nothing
                                         else deleteCursor <$> mp) bfsmpathRaw
      actorsHere = actorAssocs (const True) drawnLevelId s
      cursorHere = find (\(_, m) -> cursorPos == Just (Actor.bpos m))
                        actorsHere
      shiftedBTrajectory = case cursorHere of
        Just (_, Actor{btrajectory = Just p, bpos = prPos}) ->
          deleteCursor $ trajectoryToPath prPos (fst p)
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
                _ | isJust stgtMode
                    && (elem pos0 bline || elem pos0 shiftedBTrajectory) ->
                  ('*', atttrOnPathOrLine)  -- line takes precedence over path
                _ | isJust stgtMode
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
                  ColorFull -> if mleader == maid && isJust maid
                               then attr0 {Color.bg = Color.BrRed}
                               else if Just pos0 == cursorPos
                               then attr0 {Color.bg = Color.BrYellow}
                               else if maybe False (`ES.member` selected) maid
                               then attr0 {Color.bg = Color.BrBlue}
                               else if smarkVision && vis
                               then attr0 {Color.bg = Color.Blue}
                               else attr0
        in Color.AttrChar a char
      widthX = 80
      widthTgt = 39
      widthStats = widthX - widthTgt
      arenaStatus = drawArenaStatus (ES.member drawnLevelId sexplored) lvl
                                    widthStats
      displayPathText mp mt =
        let (plen, llen) = case (mp, bfsmpathRaw, mbpos) of
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
      pathCsr = displayPathText cursorPos mcursorHP
      trimTgtDesc n t = assert (not (T.null t) && n > 2) $
        if T.length t <= n then t
        else let ellipsis = "..."
                 fitsPlusOne = T.take (n - T.length ellipsis + 1) t
                 fits = if T.last fitsPlusOne == ' '
                        then T.init fitsPlusOne
                        else let lw = T.words fitsPlusOne
                             in T.unwords $ init lw
             in fits <> ellipsis
      cursorText =
        let n = widthTgt - T.length pathCsr - 8
        in (if isJust stgtMode then "x-hair>" else "X-hair:")
           <+> trimTgtDesc n cursorDesc
      cursorGap = T.replicate (widthTgt - T.length pathCsr
                                        - T.length cursorText) " "
      cursorStatus = toAttrLine $ cursorText <> cursorGap <> pathCsr
      minLeaderStatusWidth = 19  -- covers 3-digit HP
  selectedStatus <- drawSelected drawnLevelId
                                 (widthStats - minLeaderStatusWidth)
                                 selected
  leaderStatus <- drawLeaderStatus swaitTimes
                                   (widthStats - length selectedStatus)
  damageStatus <- drawLeaderDamage (widthStats - length leaderStatus
                                               - length selectedStatus)
  nameStatus <- drawPlayerName (widthStats - length leaderStatus
                                           - length selectedStatus
                                           - length damageStatus)
  let statusGap = toAttrLine $ T.replicate (widthStats - length leaderStatus
                                                    - length selectedStatus
                                                    - length damageStatus
                                                    - length nameStatus) " "
      -- The indicators must fit, they are the actual information.
      pathTgt = displayPathText tgtPos mtargetHP
      targetText =
        let n = widthTgt - T.length pathTgt - 8
        in "Target:" <+> trimTgtDesc n targetDesc
      targetGap = T.replicate (widthTgt - T.length pathTgt
                                        - T.length targetText) " "
      targetStatus = toAttrLine $ targetText <> targetGap <> pathTgt
      sfBottom =
        [ arenaStatus ++ cursorStatus
        , selectedStatus ++ nameStatus ++ statusGap
          ++ damageStatus ++ leaderStatus
          ++ targetStatus ]
      fLine y =
        let f l x = let ac = dis $ Point x y in ac : l
        in foldl' f [] [lxsize-1,lxsize-2..0]
      emptyLine = toAttrLine $ T.replicate lxsize " "
      sfLevel = toOverlayRaw $ emptyLine :
        let f l y = let !line = fLine y in line : l
        in foldl' f [] [lysize-1,lysize-2..0] ++ sfBottom
  return $! SingleFrame{..}

-- Comfortably accomodates 3-digit level numbers and 25-character
-- level descriptions (currently enforced max).
drawArenaStatus :: Bool -> Level -> Int -> [Color.AttrChar]
drawArenaStatus explored Level{ldepth=AbsDepth ld, ldesc, lseen, lclear} width =
  let seenN = 100 * lseen `div` max 1 lclear
      seenTxt | explored || seenN >= 100 = "all"
              | otherwise = T.justifyLeft 3 ' ' (tshow seenN <> "%")
      lvlN = T.justifyLeft 2 ' ' (tshow ld)
      seenStatus = "[" <> seenTxt <+> "seen] "
  in toAttrLine $ T.justifyLeft width ' '
             $ T.take 29 (lvlN <+> T.justifyLeft 26 ' ' ldesc) <+> seenStatus

drawLeaderStatus :: MonadClient m => Int -> Int -> m [Color.AttrChar]
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

drawLeaderDamage :: MonadClient m => Int -> m [Color.AttrChar]
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
          calm10 = calmEnough10 b $ map snd allAssocs
          forced = assert (not $ bproj b) False
          permitted = permittedPrecious calm10 forced
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
             => LevelId -> Int -> ES.EnumSet ActorId -> m [Color.AttrChar]
drawSelected drawnLevelId width selected = do
  mleader <- getsClient _sleader
  side <- getsClient sside
  allOurs <- getsState $ filter ((== side) . bfid) . EM.elems . sactorD
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) drawnLevelId
  let viewOurs (aid, Actor{bsymbol, bcolor, bhp}) =
        let bg = if mleader == Just aid
                 then Color.BrRed
                 else if ES.member aid selected
                      then Color.BrBlue
                      else Color.defBG
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

drawPlayerName :: MonadClient m => Int -> m [Color.AttrChar]
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
