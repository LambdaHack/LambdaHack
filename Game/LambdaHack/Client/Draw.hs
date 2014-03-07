-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.Draw
  ( ColorMode(..), draw
  ) where

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor as Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import qualified Game.LambdaHack.Common.Color as Color
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
import Game.LambdaHack.Common.Random
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
      (bl, blLength) = case (cursorPos, mleader) of
        (Just cursor, Just leader) ->
          let Actor{bpos, blid} = getActorBody leader s
          in if blid /= drawnLevelId
             then ([cursor], 0)
             else ( fromMaybe [] $ bla lxsize lysize seps bpos cursor
                  , chessDist bpos cursor )
        _ -> ([], 0)
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
            items = lvl `atI` pos0
            sml = EM.findWithDefault timeZero pos0 lsmell
            smlt = sml `timeAdd` timeNegate ltime
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
                _ | smarkSmell && smlt > timeZero ->
                  (timeToDigit smellTimeout smlt, rainbow pos0)
                  | otherwise ->
                  case EM.keys items of
                    [] -> (tsymbol tk, Color.defAttr {Color.fg = vcolor})
                    i : _ -> viewItem $ getItemBody i s
            vis = ES.member pos0 $ totalVisible per
            visPl = case (mleader, bfsmpathRaw) of
              (Just leader, Just (bfs, _)) ->
                 let Actor{bpos} = getActorBody leader s
                 in posAimsPos bfs bpos pos0
              _ -> False
            a = case dm of
                  ColorBW   -> Color.defAttr
                  ColorFull ->
                    if smarkVision
                    then if visPl
                         then attr0 {Color.bg = Color.Magenta}
                         else if vis
                              then attr0 {Color.bg = Color.Blue}
                              else attr0
                    else attr0
        in Color.AttrChar a char
      showN2 n = T.justifyRight 2 ' ' (tshow n)
      addAttr t = map (Color.AttrChar Color.defAttr) (T.unpack t)
      arenaStatus = drawArenaStatus (ES.member drawnLevelId sexplored) lvl
      cursorText = (if isJust stgtMode then "cursor>" else "Cursor:")
                   <+> cursorDesc
      lineText = let space = 40 - T.length cursorText - 1
                     lText | blLength == 0 = ""
                           | otherwise = "(line" <+> showN2 blLength <> ")"
                 in if T.length lText > space
                    then ""
                    else T.justifyRight space ' ' lText
      cursorStatus = addAttr $ T.justifyLeft 40 ' ' $ cursorText <+> lineText
      selectedStatus = drawSelected cli s drawnLevelId mleader
      leaderStatus = drawLeaderStatus cops s sdisco swaitTimes mleader
      targetText = "Target:" <+> targetDesc
      pathText = let space = 40 - T.length targetText - 1
                     len = case (tgtPos, bfsmpathRaw) of
                       (Just target, Just (bfs, _)) ->
                         fromMaybe 0 (accessBfs bfs target)
                       _ -> 0
                     pText | len == 0 = ""
                           | otherwise = "(path" <+> showN2 len <> ")"
                 in if T.length pText > space
                    then ""
                    else T.justifyRight space ' ' pText
      targetStatus = addAttr $ T.justifyLeft 40 ' ' $ targetText <+> pathText
      sfBottom =
        [ encodeLine $ arenaStatus ++ cursorStatus
        , encodeLine $ selectedStatus ++ leaderStatus ++ targetStatus ]
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

drawArenaStatus :: Bool -> Level -> [Color.AttrChar]
drawArenaStatus explored Level{ldepth, ldesc, lseen, lclear} =
  let addAttr t = map (Color.AttrChar Color.defAttr) (T.unpack t)
      seenN = 100 * lseen `div` lclear
      seenTxt | explored || seenN >= 100 = "all"
              | otherwise = T.justifyLeft 3 ' ' (tshow seenN <> "%")
      lvlN = T.justifyLeft 2 ' ' (tshow $ abs ldepth)
      seenStatus = T.justifyLeft 11 ' ' ("[" <> seenTxt <+> "seen]")
  in addAttr $ lvlN <+> T.justifyLeft 25 ' ' ldesc <+> seenStatus

drawLeaderStatus :: Kind.COps -> State -> Discovery
                 -> Int -> Maybe ActorId
                 -> [Color.AttrChar]
drawLeaderStatus cops s sdisco waitTimes mleader =
  let addAttr t = map (Color.AttrChar Color.defAttr) (T.unpack t)
      stats = case mleader of
        Just leader ->
          let Kind.COps{coactor=Kind.Ops{okind}} = cops
              (eqpAssocs, bracedL, ahpS, bhpS) =
                let mpl@Actor{bkind, bhp} = getActorBody leader s
                    ActorKind{ahp} = okind bkind
                in (getEqpAssocs mpl s, braced mpl,
                    tshow (maxDice ahp), tshow bhp)
              damage = case Item.strongestSword cops eqpAssocs of
                Just (_, (_, sw)) ->
                  case Item.jkind sdisco sw of
                    Just _ ->
                      case jeffect sw of
                        Hurt dice p -> tshow dice <> "+" <> tshow p
                        _ -> ""
                    Nothing -> "5d1"  -- TODO: ?
                Nothing -> "5d1"  -- TODO; use the item 'fist'
              -- Indicate the actor is braced (was waiting last move).
              -- It's a useful feedback for the otherwise hard to observe
              -- 'wait' command.
              slashes = ["|", "\\", "|", "/"]
              slashPick | bracedL =
                slashes !! (max 0 (waitTimes - 1) `mod` length slashes)
                        | otherwise = "/"
              bracePick | bracedL   = "}"
                        | otherwise = ":"
              hpText = bhpS <> slashPick <> ahpS
          in T.justifyLeft 12 ' '
               ("Dmg:" <> (if T.length damage < 8 then " " else "") <> damage)
             <+> "HP" <> bracePick <> T.justifyRight 7 ' ' hpText
             <> " "
        Nothing ->
             T.justifyLeft 12 ' ' "Dmg: ---"
             <+> T.justifyRight 10 ' ' "HP:  --/--"
             <> " "
  in addAttr stats

drawSelected :: StateClient -> State -> LevelId -> Maybe ActorId
             -> [Color.AttrChar]
drawSelected cli s drawnLevelId mleader =
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
             , Color.AttrChar sattr bsymbol )
      ours = actorNotProjAssocs (== sside cli) drawnLevelId s
      maxViewed = 14
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
      nameN n t = T.justifyLeft n ' '
                  $ let firstWord = head $ T.words t
                    in if T.length firstWord > n then "" else firstWord
      fact = sfactionD s EM.! sside cli
      ourName n = addAttr $ nameN n $ playerName $ gplayer fact
      party = if length allOurs <= 1
              then ourName $ maxViewed + 1
              else [star] ++ map snd viewed ++ addAttr " "
                   ++ ourName (maxViewed - 1 - length viewed)
  in party ++ addAttr (T.replicate (maxViewed + 2 - length party) " ")
