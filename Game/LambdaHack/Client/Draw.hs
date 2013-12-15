-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
module Game.LambdaHack.Client.Draw
  ( ColorMode(..), draw
  ) where

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.List as L
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
import Game.LambdaHack.Common.PointXY
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
-- | Draw the whole screen: level map, status area and, at most,
-- a single page overlay of text divided into lines.
draw :: ColorMode -> Kind.COps -> Perception -> LevelId -> Maybe ActorId
     -> StateClient -> State -> Overlay
     -> SingleFrame
draw dm cops per drawnLevelId mleader
     StateClient{ stgtMode, scursor, seps, sdisco
                , smarkVision, smarkSmell, smarkSuspect }
     s overlay =
  let Kind.COps{cotile=Kind.Ops{okind=tokind, ouniqGroup}} = cops
      (lvl@Level{ ldepth, lxsize, lysize, lsmell
                , ldesc, ltime, lseen, lclear }) = sdungeon s EM.! drawnLevelId
      (msgTop, over, msgBottom) = stringByLocation lxsize lysize overlay
      wealth = case mleader of
        Nothing -> 0
        Just leader -> snd $ calculateTotal (getActorBody leader s) s
      bl = case (scursor, mleader) of
        (Just cursor, Just leader) ->
          let Actor{bpos, blid} = getActorBody leader s
          in if blid /= drawnLevelId
             then []
             else fromMaybe [] $ bla lxsize lysize seps bpos cursor
        _ -> []
      dis pxy =
        let pos0 = toPoint lxsize pxy
            tile = lvl `at` pos0
            tk = tokind tile
            items = lvl `atI` pos0
            sml = EM.findWithDefault timeZero pos0 lsmell
            smlt = sml `timeAdd` timeNegate ltime
            inverseVideo = Color.Attr{ Color.fg = Color.bg Color.defAttr
                                     , Color.bg = Color.fg Color.defAttr
                                     }
            viewActor aid Actor{bsymbol, bcolor, bhp, bproj}
              | Just aid == mleader = (symbol, inverseVideo)
              | otherwise = (symbol, Color.defAttr {Color.fg = bcolor})
             where
              symbol | bhp <= 0 && not bproj = '%'
                     | otherwise = bsymbol
            rainbow p = Color.defAttr {Color.fg =
                                         toEnum $ fromEnum p `rem` 14 + 1}
            actorsHere = actorAssocs (const True) drawnLevelId s
            -- smarkSuspect is an optional overlay, so let's overlay it
            -- over both visible and invisible tiles.
            vcolor t
              | smarkSuspect && F.Suspect `elem` tfeature t = Color.BrCyan
              | vis = tcolor t
              | otherwise = tcolor2 t
            viewItem i = ( jsymbol i
                         , Color.defAttr {Color.fg =
                                            flavourToColor $ jflavour i} )
            (char, attr0) =
              case ( L.find (\ (_, m) -> pos0 == Actor.bpos m) actorsHere
                   , L.find (\ (_, m) -> scursor == Just (Actor.bpos m))
                       actorsHere ) of
                (_, actorTgt) | isJust stgtMode
                                && (L.elem pos0 bl
                                    || (case actorTgt of
                                          Just (_, Actor{ bpath=Just p
                                                        , bpos=prPos }) ->
                                            L.elem pos0 $ shiftPath prPos p
                                          _ -> False))
                    -> let unknownId = ouniqGroup "unknown space"
                           fg = case (vis, F.Walkable `elem` tfeature tk) of
                                  _ | tile == unknownId -> Color.BrBlack
                                  (True, True)   -> Color.BrGreen
                                  (True, False)  -> Color.BrRed
                                  (False, True)  -> Color.Green
                                  (False, False) -> Color.Red
                       in ('*', if Just pos0 == scursor -- highlight cursor
                                then inverseVideo {Color.fg = fg}
                                else Color.defAttr {Color.fg = fg})
                (Just (aid, m), _) -> viewActor aid m
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
        in case over pxy of
             Just c -> Color.AttrChar Color.defAttr c
             _      -> Color.AttrChar a char
      leaderStatus = drawLeaderStatus cops s sdisco ltime mleader
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
      toWidth :: Int -> Text -> Text
      toWidth n x = T.take n (T.justifyLeft n ' ' x)
      fLine y =
        let f l x = let !ac = dis (PointXY (x, y)) in ac : l
        in L.foldl' f [] [lxsize-1,lxsize-2..0]
      sfLevel =  -- Fully evaluated.
        let f l y = let !line = fLine y in line : l
        in L.foldl' f [] [lysize-1,lysize-2..0]
      sfTop = toWidth lxsize msgTop
      sfBottom = toWidth (lxsize - 1) $ fromMaybe status msgBottom
  in SingleFrame{..}

drawLeaderStatus :: Kind.COps -> State -> Discovery -> Time -> Maybe ActorId
                 -> Text
drawLeaderStatus cops s sdisco ltime mleader =
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
          braceSign | bracedL   = "{"
                    | otherwise = " "
      in T.justifyLeft 11 ' ' ("Dmg:" <+> damage) <+>
         T.justifyLeft 13 ' ' (braceSign <> "HP:" <+> bhpS
                               <+> "(" <> ahpS <> ")")
    Nothing ->
         T.justifyLeft 11 ' ' ("Dmg:" <+> "---") <+>
         T.justifyLeft 13 ' ' (" " <> "HP:" <+> "--"
                               <+> "(" <> "--" <> ")")
