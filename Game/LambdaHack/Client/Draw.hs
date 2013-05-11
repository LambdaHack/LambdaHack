{-# LANGUAGE OverloadedStrings #-}
-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Client.Draw
  ( ColorMode(..), draw
  ) where

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Client.Animation (SingleFrame (..))
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor as Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Effect
import qualified Game.LambdaHack.Common.Feature as F
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
draw :: ColorMode -> Kind.COps -> Perception -> LevelId -> ActorId
     -> StateClient -> State -> Overlay
     -> SingleFrame
draw dm cops per drawnLevelId leader
     cli@StateClient{ stgtMode, scursor, seps, sdisco
                    , smarkVision, smarkSmell, smarkSuspect }
     s overlay =
  let Kind.COps{ coactor=Kind.Ops{okind}
               , cotile=Kind.Ops{okind=tokind, ouniqGroup} } = cops
      (lvl@Level{ ldepth, lxsize, lysize, lsmell
                , ldesc, ltime, lseen, lclear }) = sdungeon s EM.! drawnLevelId
      (bitems, bracedL, ahpS, asmellL, bhpS, bposL, blidL) =
        let mpl@Actor{bkind, bhp, bpos, blid} = getActorBody leader s
            ActorKind{ahp, asmell} = okind bkind
        in (getActorItem leader s, braced mpl ltime, showT (maxDice ahp),
            asmell, showT bhp, bpos, blid)
      (msgTop, over, msgBottom) = stringByLocation lxsize lysize overlay
      -- TODO:
      sVisBG = if smarkVision
               then \ vis visPl -> if visPl
                                   then Color.Magenta
                                   else if vis
                                        then Color.Blue
                                        else Color.defBG
               else \ _vis _visPl -> Color.defBG
      (_, wealth) = calculateTotal (sside cli) drawnLevelId s
      damage  = case Item.strongestSword cops bitems of
                  Just (_, (_, sw)) ->
                    case Item.jkind sdisco sw of
                      Just _ ->
                        case jeffect sw of
                          Hurt dice p ->
                            showT dice <> "+" <> showT p
                          _ -> ""
                      Nothing -> "3d1"  -- TODO: ?
                  Nothing -> "3d1"  -- TODO; use the item 'fist'
      bl = case scursor of
        Nothing -> []
        Just cursor -> fromMaybe [] $ bla lxsize lysize seps bposL cursor
      dis pxy =
        let pos0 = toPoint lxsize pxy
            tile = lvl `at` pos0
            tk = tokind tile
            items = lvl `atI` pos0
            sml = EM.findWithDefault timeZero pos0 lsmell
            smlt = sml `timeAdd` timeNegate ltime
            viewActor aid Actor{bkind, bsymbol, bcolor, bhp, bproj}
              | aid == leader = (symbol, Color.defBG)
              | otherwise = (symbol, color)
             where
              ActorKind{asymbol, acolor} = okind bkind
              color  = fromMaybe acolor bcolor
              symbol | bhp <= 0 && not bproj = '%'
                     | otherwise = fromMaybe asymbol bsymbol
            rainbow p = toEnum $ fromEnum p `rem` 14 + 1
            actorsHere = actorAssocs (const True) drawnLevelId s
            vcolor t = if vis
                       then if smarkSuspect && F.Suspect `elem` tfeature t
                            then Color.BrCyan
                            else tcolor t
                       else tcolor2 t
            (char, fg0) =
              case ( L.find (\ (_, m) -> pos0 == Actor.bpos m) actorsHere
                   , L.find (\ (_, m) -> scursor == Just (Actor.bpos m))
                       actorsHere ) of
                (_, actorTgt) | isJust stgtMode
                                && (drawnLevelId == blidL
                                    && L.elem pos0 bl
                                    || (case actorTgt of
                                          Just (_, Actor{ bpath=Just p
                                                        , bpos=prPos }) ->
                                            L.elem pos0 $ shiftPath prPos p
                                          _ -> False))
                    ->
                      let unknownId = ouniqGroup "unknown space"
                      in ('*', case (vis, F.Walkable `elem` tfeature tk) of
                                 _ | tile == unknownId -> Color.BrBlack
                                 (True, True)   -> Color.BrGreen
                                 (True, False)  -> Color.BrRed
                                 (False, True)  -> Color.Green
                                 (False, False) -> Color.Red)
                (Just (aid, m), _) -> viewActor aid m
                _ | (smarkSmell || asmellL) && smlt > timeZero ->
                  (timeToDigit smellTimeout smlt, rainbow pos0)
                  | otherwise ->
                  case EM.keys items of
                    [] -> (tsymbol tk, vcolor tk)
                    i : _ -> Item.viewItem $ getItemBody i s
            vis = ES.member pos0 $ totalVisible per
            visPl = actorSeesLoc per leader pos0
            bg0 = if isJust stgtMode && Just pos0 == scursor
                  then Color.defFG       -- highlight target cursor
                  else sVisBG vis visPl  -- FOV debug or standard bg
            reverseVideo = Color.Attr{ fg = Color.bg Color.defAttr
                                     , bg = Color.fg Color.defAttr
                                     }
            optVisually attr@Color.Attr{fg, bg} =
              if (fg == Color.defBG)
                 || (bg == Color.defFG && fg == Color.defFG)
              then reverseVideo
              else attr
            a = case dm of
                  ColorBW   -> Color.defAttr
                  ColorFull -> optVisually Color.Attr{fg = fg0, bg = bg0}
        in case over pxy of
             Just c -> Color.AttrChar Color.defAttr c
             _      -> Color.AttrChar a char
      seenN = 100 * lseen `div` lclear
      seenTxt | seenN == 100 = "all"
              | otherwise = T.justifyRight 2 ' ' (showT seenN) <> "%"
      -- Indicate the actor is braced (was waiting last move).
      -- It's a useful feedback for the otherwise hard to observe
      -- 'wait' command.
      braceSign | bracedL   = "{"
                | otherwise = " "
      lvlN = T.justifyLeft 2 ' ' (showT ldepth)
      stats =
        T.justifyLeft 11 ' ' ("[" <> seenTxt <+> "seen]") <+>
        T.justifyLeft 9 ' ' ("$:" <+> showT wealth) <+>
        T.justifyLeft 11 ' ' ("Dmg:" <+> damage) <+>
        T.justifyLeft 13 ' ' (braceSign <> "HP:" <+> bhpS
                              <+> "(" <> ahpS <> ")")
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
      sfTop = toWidth lxsize $ msgTop
      sfBottom = toWidth (lxsize - 1) $ fromMaybe status $ msgBottom
  in SingleFrame{..}
