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
import Game.LambdaHack.Common.Animation (SingleFrame (..))
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Effect
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Item as Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
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
draw :: ColorMode -> Kind.COps -> Perception -> LevelId -> Maybe ActorId
     -> Maybe Point -> Maybe [Point] -> StateClient -> State -> Overlay
     -> SingleFrame
draw dm cops per drawnLevelId mleader tgtPos mpath
     cli@StateClient{ stgtMode, scursor, seps, sdisco, sselected
                    , smarkVision, smarkSmell, smarkSuspect, swaitTimes }
     s sfTop =
  let Kind.COps{cotile=Kind.Ops{okind=tokind, ouniqGroup}} = cops
      (lvl@Level{ ldepth, lxsize, lysize, lsmell
                , ldesc, ltime, lseen, lclear }) = sdungeon s EM.! drawnLevelId
      wealth = case mleader of
        Nothing -> 0
        Just leader -> snd $ calculateTotal (getActorBody leader s) s
      bl = case (scursor, mleader) of
        (Just cursor, Just leader) ->
          let Actor{bpos, blid} = getActorBody leader s
          in if blid /= drawnLevelId
             then [cursor]
             else fromMaybe [] $ bla lxsize lysize seps bpos cursor
        _ -> []
      inverseVideo = Color.Attr{ Color.fg = Color.bg Color.defAttr
                               , Color.bg = Color.fg Color.defAttr
                               }
      actorsHere = actorAssocs (const True) drawnLevelId s
      cursorHere = find (\(_, m) -> scursor == Just (Actor.bpos m)) actorsHere
      shiftedBPath = case cursorHere of
        Just (_, Actor{bpath = Just p, bpos = prPos}) -> shiftPath prPos p
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
            vcolor t
              | smarkSuspect && F.Suspect `elem` tfeature t = Color.BrCyan
              | vis = tcolor t
              | otherwise = tcolor2 t
            viewItem i =
              ( jsymbol i
              , Color.defAttr {Color.fg = flavourToColor $ jflavour i} )
            fgOnPathOrLine = case (vis, F.Walkable `elem` tfeature tk) of
              _ | tile == unknownId -> Color.BrBlack
              (True, True)   -> Color.BrGreen
              (True, False)  -> Color.BrRed
              (False, True)  -> Color.Green
              (False, False) -> Color.Red
            atttrOnPathOrLine = if Just pos0 `elem` [scursor, tgtPos]
                                then inverseVideo {Color.fg = fgOnPathOrLine}
                                else Color.defAttr {Color.fg = fgOnPathOrLine}
            (char, attr0) =
              case find (\(_, m) -> pos0 == Actor.bpos m) actorsHere of
                _ | isJust stgtMode
                    && (elem pos0 bl || elem pos0 shiftedBPath) ->
                  ('*', atttrOnPathOrLine)  -- line takes precedence over path
                _ | isJust stgtMode
                    && (maybe False (elem pos0) mpath) ->
                  (';', atttrOnPathOrLine)
                Just (aid, m) -> viewActor aid m
                _ | smarkSmell && smlt > timeZero ->
                  (timeToDigit smellTimeout smlt, rainbow pos0)
                  | otherwise ->
                  case EM.keys items of
                    [] -> (tsymbol tk, Color.defAttr {Color.fg = vcolor tk})
                    i : _ -> viewItem $ getItemBody i s
            vis = ES.member pos0 $ totalVisible per
            visPl =
              maybe False (\leader -> actorSeesPos per leader pos0) mleader
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
      leaderStatus = drawLeaderStatus cops s sdisco ltime swaitTimes mleader
      seenN = 100 * lseen `div` lclear
      seenTxt | seenN == 100 = "all"
              | otherwise = T.justifyRight 2 ' ' (showT seenN) <> "%"
      lvlN = T.justifyLeft 2 ' ' (showT $ abs ldepth)
      stats =
        T.justifyLeft 11 ' ' ("[" <> seenTxt <+> "seen]") <+>
        T.justifyLeft 9 ' ' ("$:" <+> showT wealth) <+>
        leaderStatus
      widthForDesc = lxsize - T.length stats - T.length lvlN - 3
      status = lvlN <+> T.justifyLeft widthForDesc ' ' ldesc <+> stats
      fLine y =
        let f l x = let !ac = dis $ Point x y in ac : l
        in foldl' f [] [lxsize-1,lxsize-2..0]
      viewOurs (aid, Actor{bsymbol, bcolor, bhp})
        | otherwise =
          let cattr = Color.defAttr {Color.fg = bcolor}
              sattr
               | Just aid == mleader = inverseVideo
               | ES.member aid sselected =
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
      -- Don't show anything if the only actor on the level is the leader.
      -- He's clearly highlighted on the level map, anyway.
      star = let sattr = case ES.size sselected of
                   0 -> Color.defAttr {Color.fg = Color.BrBlack}
                   n | n == length ours ->
                     Color.defAttr {Color.bg = Color.Blue}
                   _ -> Color.defAttr
             in Color.AttrChar sattr '*'
      party | map (Just . fst) ours == [mleader] = []
            | otherwise = star : map snd (sort $ map viewOurs ours)
      topLine = take (lxsize - length party)
                     (repeat (Color.AttrChar Color.defAttr ' '))
                ++ party
      sfLevel =  -- fully evaluated
        let f l y = let !line = fLine y in line : l
        in topLine : foldl' f [] [lysize-1,lysize-2..0]
      sfBottom = toWidth (lxsize - 1) status
  in SingleFrame{..}

drawLeaderStatus :: Kind.COps -> State -> Discovery
                 -> Time -> Int -> Maybe ActorId
                 -> Text
drawLeaderStatus cops s sdisco ltime waitTimes mleader =
  case mleader of
    Just leader ->
      let Kind.COps{coactor=Kind.Ops{okind}} = cops
          (bitems, bracedL, ahpS, bhpS) =
            let mpl@Actor{bkind, bhp} = getActorBody leader s
                ActorKind{ahp} = okind bkind
            in (getActorItem leader s, braced mpl ltime,
                showT (maxDice ahp), showT bhp)
          damage = case Item.strongestSword cops bitems of
            Just (_, (_, sw)) ->
              case Item.jkind sdisco sw of
                Just _ ->
                  case jeffect sw of
                    Hurt dice p -> showT dice <> "+" <> showT p
                    _ -> ""
                Nothing -> "3d1"  -- TODO: ?
            Nothing -> "3d1"  -- TODO; use the item 'fist'
          -- Indicate the actor is braced (was waiting last move).
          -- It's a useful feedback for the otherwise hard to observe
          -- 'wait' command.
          braceChars = ['{', '\\', 'X', '\\', '}', '/', 'X', '/']
          braceChar = braceChars
                      !! (max 0 (waitTimes - 1) `mod` length braceChars)
          braceSign | bracedL   = T.singleton braceChar
                    | otherwise = " "
      in T.justifyLeft 11 ' ' ("Dmg:" <+> damage) <+>
         T.justifyLeft 13 ' ' (braceSign <> "HP:" <+> bhpS
                               <+> "(" <> ahpS <> ")")
    Nothing ->
         T.justifyLeft 11 ' ' ("Dmg:" <+> "---") <+>
         T.justifyLeft 13 ' ' (" " <> "HP:" <+> "--"
                               <+> "(" <> "--" <> ")")
