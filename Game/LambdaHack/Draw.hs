{-# LANGUAGE OverloadedStrings #-}
-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Draw
  ( ColorMode(..), draw, animate
  ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Actor as Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation (Animation, Frames, SingleFrame (..),
                                  renderAnim)
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
     StateClient{stgtMode, scursor, seps, sdebugCli}
     s@State{sdisco, sarena, sdungeon}
     overlay =
  let Kind.COps{ coactor=Kind.Ops{okind}
               , coitem=Kind.Ops{okind=iokind}
               , cotile=Kind.Ops{okind=tokind, ouniqGroup} } = cops
      DebugModeCli{smarkVision, smarkSmell} = sdebugCli
      (drawnLevelId, lvl@Level{lxsize, lysize, lsmell,
                               ldesc, lactor, ltime, lseen, lclear}) =
        case stgtMode of
          TgtOff -> (sarena, getArena s)
          _ -> (tgtLevelId stgtMode, sdungeon M.! tgtLevelId stgtMode)
      mpl@Actor{bkind, bhp, bpos} = getPlayerBody s
      bitems = getPlayerItem s
      ActorKind{ahp, asmell} = okind bkind
      reachable = debugTotalReachable per
      visible   = totalVisible per
      (msgTop, over, msgBottom) = stringByLocation lxsize lysize overlay
      -- TODO:
      sVisBG = if smarkVision
               then \ vis rea -> if vis
                                 then Color.Blue
                                 else if rea
                                      then Color.Magenta
                                      else Color.defBG
               else \ _vis _rea -> Color.defBG
      (_, wealth)  = calculateTotal s
      damage  = case Item.strongestSword cops bitems of
                  Just sw ->
                    case Item.jkind sdisco sw of
                      Just swk ->
                        case ieffect $ iokind swk of
                          Wound dice ->
                            showT dice <> "+" <> showT (Item.jpower sw)
                          _ -> showT (Item.jpower sw)
                      Nothing -> "3d1"  -- TODO: ?
                  Nothing -> "3d1"  -- TODO; use the item 'fist'
      bl = fromMaybe [] $ bla lxsize lysize seps bpos scursor
      dis pxy =
        let pos0 = toPoint lxsize pxy
            tile = lvl `at` pos0
            tk = tokind tile
            items = lvl `atI` pos0
            sml = IM.findWithDefault timeZero pos0 lsmell
            smlt = sml `timeAdd` timeNegate ltime
            viewActor loc Actor{bkind = bkind2, bsymbol, bcolor}
              | loc == bpos && drawnLevelId == sarena =
                  (symbol, Color.defBG)  -- highlight player
              | otherwise = (symbol, color)
             where
              ActorKind{asymbol, acolor} = okind bkind2
              color  = fromMaybe acolor  bcolor
              symbol = fromMaybe asymbol bsymbol
            rainbow loc = toEnum $ loc `rem` 14 + 1
            actorsHere = IM.elems lactor
            (char, fg0) =
              case ( L.find (\ m -> pos0 == Actor.bpos m) actorsHere
                   , L.find (\ m -> scursor == Actor.bpos m) actorsHere ) of
                (_, actorTgt) | stgtMode /= TgtOff
                                && (drawnLevelId == sarena
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
                _ | (smarkSmell || asmell) && smlt > timeZero ->
                  (timeToDigit smellTimeout smlt, rainbow pos0)
                  | otherwise ->
                  case items of
                    [] -> (tsymbol tk, if vis then tcolor tk else tcolor2 tk)
                    i : _ -> Item.viewItem i
            vis = IS.member pos0 visible
            rea = IS.member pos0 reachable
            bg0 = if stgtMode /= TgtOff && pos0 == scursor
                  then Color.defFG     -- highlight target cursor
                  else sVisBG vis rea  -- FOV debug or standard bg
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
      braceSign | braced mpl ltime = "{"
                | otherwise = " "
      lvlN = T.justifyLeft 2 ' ' (showT $ levelNumber drawnLevelId)
      stats =
        T.justifyLeft 11 ' ' ("[" <> seenTxt <+> "seen]") <+>
        T.justifyLeft 9 ' ' ("$:" <+> showT wealth) <+>
        T.justifyLeft 11 ' ' ("Dmg:" <+> damage) <+>
        T.justifyLeft 13 ' ' (braceSign <> "HP:" <+> showT bhp
                              <+> "(" <> showT (maxDice ahp) <> ")")
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
animate cli@StateClient{sreport} loc per anim =
  let Level{lxsize, lysize} = getArena loc
      over = renderReport sreport
      topLineOnly = padMsg lxsize over
      basicFrame = draw ColorFull (scops loc) per cli loc [topLineOnly]
  in renderAnim lxsize lysize basicFrame anim
