{-# LANGUAGE OverloadedStrings #-}
-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Draw
  ( ColorMode(..), draw, animate
  ) where

import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Msg
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Animation (SingleFrame(..), Animation, rederAnim)
import Game.LambdaHack.State
import Game.LambdaHack.PointXY
import Game.LambdaHack.Point
import Game.LambdaHack.Vector
import Game.LambdaHack.Level
import Game.LambdaHack.Effect
import Game.LambdaHack.Perception
import Game.LambdaHack.Actor as Actor
import Game.LambdaHack.ActorState
import qualified Game.LambdaHack.Dungeon as Dungeon
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Item as Item
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Kind as Kind
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Time
import Game.LambdaHack.Config

-- | Color mode for the display.
data ColorMode =
    ColorFull  -- ^ normal, with full colours
  | ColorBW    -- ^ black+white only

-- TODO: split up and generally rewrite.
-- | Draw the whole screen: level map, status area and, at most,
-- a single page overlay of text divided into lines.
draw :: ColorMode -> Kind.COps -> Perception -> State -> Overlay
     -> SingleFrame
draw dm cops per s@State{ scursor=Cursor{..}
                        , sflavour, slid, splayer, sdebug
                        } overlay =
  let Kind.COps{ coactor=Kind.Ops{okind}
               , coitem=coitem@Kind.Ops{okind=iokind}
               , cotile=Kind.Ops{okind=tokind, ouniqGroup} } = cops
      DebugMode{smarkVision, somniscient} = sdebug
      lvl@Level{lxsize, lysize, lsmell, ldesc, lactor, ltime, lclear, lseen} =
        slevel s
      (_, mpl@Actor{bkind, bhp, bloc}, bitems) = findActorAnyLevel splayer s
      ActorKind{ahp, asmell} = okind bkind
      reachable = debugTotalReachable per
      visible   = totalVisible per
      (msgTop, over, msgBottom) = stringByLocation lxsize lysize overlay
      (sSml, sVis) = case smarkVision of
        Just Blind -> (True, True)
        Just _  -> (False, True)
        Nothing | asmell -> (True, False)
        Nothing -> (False, False)
      lAt    = if somniscient then at else rememberAt
      liAt   = if somniscient then atI else rememberAtI
      sVisBG = if sVis
               then \ vis rea -> if vis
                                 then Color.Blue
                                 else if rea
                                      then Color.Magenta
                                      else Color.defBG
               else \ _vis _rea -> Color.defBG
      (_, wealth)  = calculateTotal coitem s
      damage  = case Item.strongestSword cops bitems of
                  Just sw -> case ieffect $ iokind $ Item.jkind sw of
                    Wound dice -> showT dice <> "+" <> showT (Item.jpower sw)
                    _ -> showT (Item.jpower sw)
                  Nothing -> "3d1"  -- TODO; use the item 'fist'
      bl = fromMaybe [] $ bla lxsize lysize ceps bloc clocation
      dis pxy =
        let loc0 = toPoint lxsize pxy
            tile = lvl `lAt` loc0
            tk = tokind tile
            items = lvl `liAt` loc0
            sml = IM.findWithDefault timeZero loc0 lsmell
            smlt = sml `timeAdd` timeNegate ltime
            viewActor loc Actor{bkind = bkind2, bsymbol, bcolor}
              | loc == bloc && slid == creturnLn =
                  (symbol, Color.defBG)  -- highlight player
              | otherwise = (symbol, color)
             where
              ActorKind{asymbol, acolor} = okind bkind2
              color  = fromMaybe acolor  bcolor
              symbol = fromMaybe asymbol bsymbol
            rainbow loc = toEnum $ loc `rem` 14 + 1
            actorsHere = IM.elems lactor
            (char, fg0) =
              case ( L.find (\ m -> loc0 == Actor.bloc m) actorsHere
                   , L.find (\ m -> clocation == Actor.bloc m) actorsHere ) of
                (_, actorTgt) | ctargeting /= TgtOff
                                && (slid == creturnLn
                                    && L.elem loc0 bl
                                    || (case actorTgt of
                                           Just (Actor{ btarget=TPath p
                                                      , bloc=prLoc }) ->
                                             L.elem loc0 $ shiftPath prLoc p
                                           _ -> False))
                    ->
                      let unknownId = ouniqGroup "unknown space"
                      in ('*', case (vis, F.Walkable `elem` tfeature tk) of
                                 _ | tile == unknownId -> Color.BrBlack
                                 (True, True)   -> Color.BrGreen
                                 (True, False)  -> Color.BrRed
                                 (False, True)  -> Color.Green
                                 (False, False) -> Color.Red)
                (Just m, _) | somniscient || vis -> viewActor loc0 m
                _ | sSml && smlt > timeZero ->
                  (timeToDigit (smellTimeout s) smlt, rainbow loc0)
                  | otherwise ->
                  case items of
                    [] -> (tsymbol tk, if vis then tcolor tk else tcolor2 tk)
                    i : _ -> Item.viewItem coitem (Item.jkind i) sflavour
            vis = IS.member loc0 visible
            rea = IS.member loc0 reachable
            bg0 = if ctargeting /= TgtOff && loc0 == clocation
                  then Color.defFG     -- highlight target cursor
                  else sVisBG vis rea  -- FOV debug or standard bg
            reverseVideo = Color.Attr{ fg = Color.bg Color.defaultAttr
                                     , bg = Color.fg Color.defaultAttr
                                     }
            optVisually attr@Color.Attr{fg, bg} =
              if (fg == Color.defBG)
                 || (bg == Color.defFG && fg == Color.defFG)
              then reverseVideo
              else attr
            a = case dm of
                  ColorBW   -> Color.defaultAttr
                  ColorFull -> optVisually Color.Attr{fg = fg0, bg = bg0}
        in case over pxy of
             Just c -> Color.AttrChar Color.defaultAttr c
             _      -> Color.AttrChar a char
      seenN = 100 * lseen `div` lclear
      seenTxt | seenN == 100 = "all"
              | otherwise = T.justifyRight 2 ' ' (showT seenN) <> "%"
      -- Indicate the actor is braced (was waiting last move).
      -- It's a useful feedback for the otherwise hard to observe
      -- 'wait' command.
      braceSign | braced mpl ltime = "{"
                | otherwise = " "
      lvlN = T.justifyLeft 2 ' ' (showT $ Dungeon.levelNumber slid)
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

-- | Render animations on top of the current screen frame.
animate :: State -> Diary -> Kind.COps -> Perception -> Animation
        -> [Maybe SingleFrame]
animate s Diary{sreport} cops per anim =
  let Level{lxsize, lysize} = slevel s
      over = renderReport sreport
      topLineOnly = padMsg lxsize over
      basicFrame = draw ColorFull cops per s [topLineOnly]
  in rederAnim lxsize lysize basicFrame anim
