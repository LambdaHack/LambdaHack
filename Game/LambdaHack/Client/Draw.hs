{-# LANGUAGE OverloadedStrings #-}
-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Client.Draw
  ( ColorMode(..), draw, animate
  ) where

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Actor as Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Animation (Animation, Frames, SingleFrame (..),
                                         renderAnim)
import Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Effect
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Item as Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Vector

-- | Color mode for the display.
data ColorMode =
    ColorFull  -- ^ normal, with full colours
  | ColorBW    -- ^ black+white only

-- TODO: split up and generally rewrite.
-- | Draw the whole screen: level map, status area and, at most,
-- a single page overlay of text divided into lines.
draw :: ColorMode -> Kind.COps -> Perception -> StateClient -> State -> Overlay
     -> SingleFrame
draw dm cops per
     cli@StateClient{stgtMode, scursor, seps, sdebugCli}
     s overlay =
  let Kind.COps{ coactor=Kind.Ops{okind}
               , coitem=Kind.Ops{okind=iokind}
               , cotile=Kind.Ops{okind=tokind, ouniqGroup} } = cops
      DebugModeCli{smarkVision, smarkSmell} = sdebugCli
      (drawnLevelId, lvl@Level{ldepth, lxsize, lysize, lsmell,
                               ldesc, ltime, lseen, lclear}) =
        case stgtMode of
          Nothing -> (getArena cli s, sdungeon s EM.! getArena cli s)
          Just tgtM -> (tgtLevelId tgtM, sdungeon s EM.! tgtLevelId tgtM)
      mleader = sleader cli
      (bitems, bracedL, ahpS, asmellL, bhpS, bposL) =
        case mleader of
          Nothing -> ([], False, "--", False, "--", undefined)
          Just leader ->
            let mpl@Actor{bkind, bhp, bpos} = getActorBody leader s
                ActorKind{ahp, asmell} = okind bkind
            in (getActorItem leader s, braced mpl ltime, showT (maxDice ahp),
                asmell, showT bhp, bpos)
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
                  Just sw ->
                    case Item.jkind (sdisco s) sw of
                      Just swk ->
                        case ieffect $ iokind swk of
                          Wound dice ->
                            showT dice <> "+" <> showT (Item.jpower sw)
                          _ -> showT (Item.jpower sw)
                      Nothing -> "3d1"  -- TODO: ?
                  Nothing -> "3d1"  -- TODO; use the item 'fist'
      bl | isNothing mleader = []
         | otherwise = case scursor of
        Nothing -> []
        Just cursor -> fromMaybe [] $ bla lxsize lysize seps bposL cursor
      dis pxy =
        let pos0 = toPoint lxsize pxy
            tile = lvl `at` pos0
            tk = tokind tile
            items = lvl `atI` pos0
            sml = EM.findWithDefault timeZero pos0 lsmell
            smlt = sml `timeAdd` timeNegate ltime
            viewActor loc Actor{bkind, bsymbol, bcolor, bhp}
              | isJust mleader
                && loc == bposL
                && drawnLevelId == getArena cli s =
                  (symbol, Color.defBG)  -- highlight leader
              | otherwise = (symbol, color)
             where
              ActorKind{asymbol, acolor} = okind bkind
              color  = fromMaybe acolor bcolor
              symbol | bhp <= 0 = '%'
                     | otherwise = fromMaybe asymbol bsymbol
            rainbow p = toEnum $ fromEnum p `rem` 14 + 1
            actorsHere = actorList (const True) drawnLevelId s
            (char, fg0) =
              case ( L.find (\ m -> pos0 == Actor.bpos m) actorsHere
                   , L.find (\ m -> scursor == Just (Actor.bpos m))
                       actorsHere ) of
                (_, actorTgt) | isJust stgtMode
                                && (drawnLevelId == getArena cli s
                                    && L.elem pos0 bl
                                    || (case actorTgt of
                                           Just (Actor{ bpath=Just p
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
                (Just m, _) -> viewActor pos0 m
                _ | (smarkSmell || asmellL) && smlt > timeZero ->
                  (timeToDigit smellTimeout smlt, rainbow pos0)
                  | otherwise ->
                  case EM.keys items of
                    [] -> (tsymbol tk, if vis then tcolor tk else tcolor2 tk)
                    i : _ -> Item.viewItem $ getItemBody i s
            vis = ES.member pos0 $ totalVisible per
            visPl =
              maybe False (\leader -> actorSeesLoc per leader pos0) mleader
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

-- TODO: restrict the animation to 'per' before drawing.
-- | Render animations on top of the current screen frame.
animate :: StateClient -> State -> Perception -> Animation
        -> Frames
animate cli@StateClient{sreport} s per anim =
  let Level{lxsize, lysize} = sdungeon s EM.! getArena cli s
      over = renderReport sreport
      topLineOnly = padMsg lxsize over
      basicFrame = draw ColorFull (scops s) per cli s [topLineOnly]
  in renderAnim lxsize lysize basicFrame anim
