-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.UI.DrawClient
  ( ColorMode(..)
  , draw
  ) where

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Common.Actor as Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Item as Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind

-- | Color mode for the display.
data ColorMode =
    ColorFull  -- ^ normal, with full colours
  | ColorBW    -- ^ black+white only

-- TODO: split up and generally rewrite.
-- | Draw the whole screen: level map and status area.
-- Pass at most a single page if overlay of text unchanged
-- to the frontends to display separately or overlay over map,
-- depending on the frontend.
draw :: Bool -> ColorMode -> Kind.COps -> Perception -> LevelId
     -> Maybe ActorId -> Maybe Point -> Maybe Point
     -> Maybe (PointArray.Array BfsDistance, Maybe [Point])
     -> StateClient -> State
     -> Text -> Text -> Overlay
     -> SingleFrame
draw sfBlank dm cops per drawnLevelId mleader cursorPos tgtPos bfsmpathRaw
     cli@StateClient{ stgtMode, seps, sdisco, sexplored
                    , smarkVision, smarkSmell, smarkSuspect, swaitTimes } s
     cursorDesc targetDesc sfTop =
  let Kind.COps{cotile=cotile@Kind.Ops{okind=tokind, ouniqGroup}} = cops
      (lvl@Level{lxsize, lysize, lsmell, ltime}) = sdungeon s EM.! drawnLevelId
      (bl, blLength, mblid) = case (cursorPos, mleader) of
        (Just cursor, Just leader) ->
          let Actor{bpos, blid} = getActorBody leader s
          in if blid /= drawnLevelId
             then ([cursor], 0, Just blid)
             else ( fromMaybe [] $ bla lxsize lysize seps bpos cursor
                  , chessDist bpos cursor
                  , Just blid)
        _ -> ([], 0, Nothing)
      mpath = maybe Nothing (\(_, mp) -> if blLength == 0
                                         then Nothing
                                         else mp) bfsmpathRaw
      actorsHere = actorAssocs (const True) drawnLevelId s
      cursorHere = find (\(_, m) -> cursorPos == Just (Actor.bpos m))
                   actorsHere
      shiftedBTrajectory = case cursorHere of
        Just (_, Actor{btrajectory = Just p, bpos = prPos}) -> trajectoryToPath prPos p
        _ -> []
      unknownId = ouniqGroup "unknown space"
      dis pos0 =
        let tile = lvl `at` pos0
            tk = tokind tile
            floorBag = lvl `atI` pos0
            (letterSlots, numberSlots) = sslots cli
            bagLetterSlots = EM.filter (`EM.member` floorBag) letterSlots
            bagNumberSlots = IM.filter (`EM.member` floorBag) numberSlots
            floorIids = reverse (EM.elems bagLetterSlots)
                        ++ IM.elems bagNumberSlots
            sml = EM.findWithDefault timeZero pos0 lsmell
            smlt = sml `timeDeltaToFrom` ltime
            viewActor aid Actor{bsymbol, bcolor, bhp, bproj}
              | Just aid == mleader = (symbol, inverseVideo)
              | otherwise = (symbol, Color.defAttr {Color.fg = bcolor})
             where
              symbol | bhp <= 0 && not bproj = '%'
                     | otherwise = bsymbol
            rainbow p = Color.defAttr {Color.fg =
                                         toEnum $ fromEnum p `rem` 14 + 1}
            -- smarkSuspect is an optional overlay, so let's overlay it
            -- over both visible and invisible tiles.
            vcolor
              | smarkSuspect && Tile.isSuspect cotile tile = Color.BrCyan
              | vis = tcolor tk
              | otherwise = tcolor2 tk
            viewItem i =
              ( jsymbol i
              , Color.defAttr {Color.fg = flavourToColor $ jflavour i} )
            fgOnPathOrLine = case (vis, Tile.isWalkable cotile tile) of
              _ | tile == unknownId -> Color.BrBlack
              _ | Tile.isSuspect cotile tile -> Color.BrCyan
              (True, True)   -> Color.BrGreen
              (True, False)  -> Color.BrRed
              (False, True)  -> Color.Green
              (False, False) -> Color.Red
            atttrOnPathOrLine = if Just pos0 `elem` [cursorPos, tgtPos]
                                then inverseVideo {Color.fg = fgOnPathOrLine}
                                else Color.defAttr {Color.fg = fgOnPathOrLine}
            (char, attr0) =
              case find (\(_, m) -> pos0 == Actor.bpos m) actorsHere of
                _ | isJust stgtMode
                    && (elem pos0 bl || elem pos0 shiftedBTrajectory) ->
                  ('*', atttrOnPathOrLine)  -- line takes precedence over path
                _ | isJust stgtMode
                    && (maybe False (elem pos0) mpath) ->
                  (';', atttrOnPathOrLine)
                Just (aid, m) -> viewActor aid m
                _ | smarkSmell && smlt > Delta timeZero ->
                  (timeDeltaToDigit smellTimeout smlt, rainbow pos0)
                  | otherwise ->
                  case floorIids of
                    [] -> (tsymbol tk, Color.defAttr {Color.fg = vcolor})
                    iid : _ -> viewItem $ getItemBody iid s
            vis = ES.member pos0 $ totalVisible per
            a = case dm of
                  ColorBW -> Color.defAttr
                  ColorFull -> if smarkVision && vis
                               then attr0 {Color.bg = Color.Blue}
                               else attr0
        in Color.AttrChar a char
      widthX = 80
      widthTgt = 39
      widthStats = widthX - widthTgt
      showN2 n = T.justifyRight 2 ' ' (tshow n)
      addAttr t = map (Color.AttrChar Color.defAttr) (T.unpack t)
      arenaStatus = drawArenaStatus (ES.member drawnLevelId sexplored) lvl
                                    widthStats
      displayPathText mp =
        let len = case (mp, bfsmpathRaw) of
              (Just target, Just (bfs, _)) | mblid == Just drawnLevelId ->
                fromMaybe 0 (accessBfs bfs target)
              _ -> 0
            pText | len == 0 = ""
                  | otherwise = "(path" <+> showN2 len <> ")"
        in " " <> pText
      -- The indicators must fit, they are the actual information.
      pathCsr = displayPathText cursorPos
      -- TODO: when too long descriptions, use Csr and Tgt
      cursorText =
        let space = widthTgt - T.length pathCsr
        in T.take space
           $ (if isJust stgtMode then "cursor>" else "Cursor:")
             <+> cursorDesc
      cursorGap = T.replicate (widthTgt - T.length pathCsr
                                        - T.length cursorText) " "
      cursorStatus = addAttr $ cursorText <> cursorGap <> pathCsr
      leaderStatus = drawLeaderStatus cops s swaitTimes mleader
                                      widthStats
      selectedStatus = drawSelected cli s drawnLevelId mleader
                                    (widthStats - length leaderStatus)
      damageStatus = drawLeaderDamage cops s sdisco mleader
                                      (widthStats - length leaderStatus
                                                  - length selectedStatus)
      nameStatus = drawPlayerName cli s (widthStats - length leaderStatus
                                                    - length selectedStatus
                                                    - length damageStatus)
      statusGap = addAttr $ T.replicate (widthStats - length leaderStatus
                                                    - length selectedStatus
                                                    - length damageStatus
                                                    - length nameStatus) " "
      -- The indicators must fit, they are the actual information.
      pathTgt = displayPathText tgtPos
      targetText =
        let space = widthTgt - T.length pathTgt
        in T.take space
           $ "Target:" <+> targetDesc
      targetGap = T.replicate (widthTgt - T.length pathTgt
                                        - T.length targetText) " "
      targetStatus = addAttr $ targetText <> targetGap <> pathTgt
      sfBottom =
        [ encodeLine $ arenaStatus ++ cursorStatus
        , encodeLine $ selectedStatus ++ nameStatus ++ statusGap
                       ++ damageStatus ++ leaderStatus
                       ++ targetStatus ]
      fLine y = encodeLine $
        let f l x = let ac = dis $ Point x y in ac : l
        in foldl' f [] [lxsize-1,lxsize-2..0]
      sfLevel =  -- fully evaluated
        let f l y = let !line = fLine y in line : l
        in foldl' f [] [lysize-1,lysize-2..0]
  in SingleFrame{..}

inverseVideo :: Color.Attr
inverseVideo = Color.Attr { Color.fg = Color.bg Color.defAttr
                          , Color.bg = Color.fg Color.defAttr }

-- Comfortably accomodates 3-digit level numbers and 25-character
-- level descriptions (currently enforced max).
drawArenaStatus :: Bool -> Level -> Int -> [Color.AttrChar]
drawArenaStatus explored Level{ldepth, ldesc, lseen, lclear} width =
  let addAttr t = map (Color.AttrChar Color.defAttr) (T.unpack t)
      seenN = 100 * lseen `div` lclear
      seenTxt | explored || seenN >= 100 = "all"
              | otherwise = T.justifyLeft 3 ' ' (tshow seenN <> "%")
      lvlN = T.justifyLeft 2 ' ' (tshow $ abs ldepth)
      seenStatus = T.justifyLeft 11 ' ' ("[" <> seenTxt <+> "seen]")
  in addAttr $ T.justifyLeft width ' '
             $ T.take 29 (lvlN <+> T.justifyLeft 26 ' ' ldesc) <+> seenStatus

drawLeaderStatus :: Kind.COps -> State
                 -> Int -> Maybe ActorId -> Int
                 -> [Color.AttrChar]
drawLeaderStatus cops s waitTimes mleader _width =
  let addAttr t = map (Color.AttrChar Color.defAttr) (T.unpack t)
      addColor c t = map (Color.AttrChar $ Color.Attr c Color.defBG)
                         (T.unpack t)
      stats = case mleader of
        Just leader ->
          let Kind.COps{coactor=Kind.Ops{okind}} = cops
              (bracedL, hpPeriod, calmDelta, ahpS, bhpS, acalmS, bcalmS) =
                let b@Actor{bkind, bhp, bcalm} = getActorBody leader s
                    ActorKind{ahp, acalm} = okind bkind
                in ( braced b, regenHPPeriod b s, bcalmDelta b
                   , tshow (Dice.maxDice ahp), tshow bhp
                   , tshow (Dice.maxDice acalm), tshow bcalm )
              calmAddAttr | calmDelta > 0 = addColor Color.Green
                          | calmDelta < 0 = addColor Color.Red
                          | otherwise = addAttr
              calmHeader = calmAddAttr $ "C:"
              calmText = bcalmS <> "/" <> acalmS
              -- Indicate the actor is braced (was waiting last move).
              -- It's a useful feedback for the otherwise hard to observe
              -- 'wait' command.
              slashes = ["/", "|", "\\", "|"]
              slashPick | bracedL =
                slashes !! (max 0 (waitTimes - 1) `mod` length slashes)
                        | otherwise = "/"
              bracePick | bracedL   = "}"
                        | otherwise = ":"
              hpAddAttr | hpPeriod > 0 = addColor Color.Green
                        | otherwise = addAttr
              hpHeader = hpAddAttr $ "H" <> bracePick
              hpText = bhpS <> slashPick <> ahpS
          in calmHeader <> addAttr (T.justifyRight 6 ' ' calmText <> " ")
             <> hpHeader <> addAttr (T.justifyRight 6 ' ' hpText <> " ")
        Nothing -> addAttr $
             "C: --/--"
             <+> "H: --/--"
  in stats

drawLeaderDamage :: Kind.COps -> State -> Discovery
                 -> Maybe ActorId -> Int
                 -> [Color.AttrChar]
drawLeaderDamage cops s sdisco mleader width =
  let addAttr t = map (Color.AttrChar Color.defAttr) (T.unpack t)
      stats = case mleader of
        Just leader ->
          let b = getActorBody leader s
              eqpAssocs = getEqpAssocs b s
              damage = case Item.strongestSword cops eqpAssocs of
                Just (_, (_, sw)) ->
                  case Item.jkind sdisco sw of
                    Just _ ->
                      case jeffect sw of
                        Hurt dice p -> tshow dice <> "+" <> tshow p
                        _ -> ""
                    Nothing -> "5d1"  -- TODO: ?
                Nothing -> "5d1"  -- TODO; use the item 'fist'
          in T.justifyLeft 8 ' '
               ("D:" <> (if T.length damage < 6 then " " else "") <> damage)
        Nothing -> "D: ---  "
  in if T.length stats > width then [] else addAttr $ stats <> " "

-- TODO: colour some texts using the faction's colour
drawSelected :: StateClient -> State -> LevelId -> Maybe ActorId -> Int
             -> [Color.AttrChar]
drawSelected cli s drawnLevelId mleader width =
  let selected = sselected cli
      viewOurs (aid, Actor{bsymbol, bcolor, bhp})
        | otherwise =
          let cattr = Color.defAttr {Color.fg = bcolor}
              sattr
               | Just aid == mleader = inverseVideo
               | ES.member aid selected =
                   -- TODO: in the future use a red rectangle instead
                   -- of background and mark them on the map, too;
                   -- also, perhaps blink all selected on the map,
                   -- when selection changes
                   if bcolor /= Color.Blue
                   then cattr {Color.bg = Color.Blue}
                   else cattr {Color.bg = Color.Magenta}
               | otherwise = cattr
          in ( (bhp > 0, bsymbol /= '@', bsymbol, bcolor, aid)
             , Color.AttrChar sattr $ if bhp > 0 then bsymbol else '%' )
      ours = filter (not . bproj . snd)
             $ actorAssocs (== sside cli) drawnLevelId s
      maxViewed = width - 2
      -- Don't show anything if the only actor on the level is the leader.
      -- He's clearly highlighted on the level map, anyway.
      star = let sattr = case ES.size selected of
                   0 -> Color.defAttr {Color.fg = Color.BrBlack}
                   n | n == length ours ->
                     Color.defAttr {Color.bg = Color.Blue}
                   _ -> Color.defAttr
                 char = if length ours > maxViewed then '$' else '*'
             in Color.AttrChar sattr char
      viewed = take maxViewed $ sort $ map viewOurs ours
      addAttr t = map (Color.AttrChar Color.defAttr) (T.unpack t)
      allOurs = filter ((== sside cli) . bfid) $ EM.elems $ sactorD s
      party = if length allOurs <= 1
              then []
              else [star] ++ map snd viewed ++ addAttr " "
  in party

drawPlayerName :: StateClient -> State -> Int
               -> [Color.AttrChar]
drawPlayerName cli s width =
  let addAttr t = map (Color.AttrChar Color.defAttr) (T.unpack t)
      fact = sfactionD s EM.! sside cli
      nameN n t = let firstWord = head $ T.words t
                  in if T.length firstWord > n then "" else firstWord
      ourName = nameN (width - 1) $ playerName $ gplayer fact
  in if T.length ourName > width then [] else addAttr $ ourName <> " "
