-- | Display game data on the screen using one of the available frontends
-- (determined at compile time with cabal flags).
{-# LANGUAGE CPP #-}
module Game.LambdaHack.Display
  ( -- * Re-exported frontend
    FrontendSession, startup, shutdown, frontendName
    -- * Derived operations
  , ColorMode(..), displayLevel, displayNothing
  , getConfirm, getKey, getYesNo
  ) where

-- Wrapper for selected Display frontend.

#ifdef CURSES
import Game.LambdaHack.Display.Curses as D
#elif VTY
import Game.LambdaHack.Display.Vty as D
#elif STD
import Game.LambdaHack.Display.Std as D
#else
import Game.LambdaHack.Display.Gtk as D
#endif

import qualified Data.Char as Char
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Msg
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.State
import Game.LambdaHack.PointXY
import Game.LambdaHack.Point
import Game.LambdaHack.Level
import Game.LambdaHack.Effect
import Game.LambdaHack.Perception
import Game.LambdaHack.Tile
import Game.LambdaHack.Actor as Actor
import Game.LambdaHack.ActorState
import qualified Game.LambdaHack.Dungeon as Dungeon
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Item as Item
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.FOV
import qualified Game.LambdaHack.Feature as F

-- | Waits for a SPACE or ESC.
getConfirm :: FrontendSession -> Bool -> IO Bool
getConfirm fs doPush =
  let loop dp = do
        (e, _) <- nextEvent fs dp
        case e of
          K.Space    -> return True
          K.Esc      -> return False
          _          -> loop Nothing
  in loop (Just doPush)

-- | Wait for a player keypress.
getKey :: FrontendSession -> Bool -> IO (K.Key, K.Modifier)
getKey fs doPush = nextEvent fs (Just doPush)

-- | A yes-no confirmation.
getYesNo :: FrontendSession -> IO Bool
getYesNo fs =
  let loop dp = do
        (e, _) <- nextEvent fs dp
        case e of
          K.Char 'y' -> return True
          K.Char 'n' -> return False
          K.Esc      -> return False
          _          -> loop Nothing
  in loop (Just False)

-- | Color mode for the display.
data ColorMode =
    ColorFull  -- ^ normal, with full colours
  | ColorBW    -- ^ black+white only

displayNothing :: FrontendSession -> IO Bool
displayNothing fs = do
  display fs True False Nothing
  return True

-- TODO: split up and generally rewrite.
-- | Display the whole screen: level map, messages and status area
-- and multi-page overlaid information, if any.
displayLevel :: Bool -> ColorMode -> FrontendSession -> Kind.COps
             -> Perception -> State -> Overlay -> IO Bool
displayLevel doPush dm fs cops per
             s@State{ scursor=Cursor{..}
                    , stime, sflavour, slid, splayer, sanim, sdebug }
             overlay =
  let Kind.COps{ coactor=Kind.Ops{okind}
               , coitem=coitem@Kind.Ops{okind=iokind}
               , cotile=Kind.Ops{okind=tokind, ouniqGroup} } = cops
      DebugMode{smarkVision, somniscient} = sdebug
      lvl@Level{lxsize, lysize, lsmell, ldesc, lactor} = slevel s
      (_, Actor{bkind, bhp, bloc, bdir}, bitems) = findActorAnyLevel splayer s
      ActorKind{ahp, asmell} = okind bkind
      reachable = debugTotalReachable per
      visible   = totalVisible per
      (msgTop, ns, over) = stringByLocation lxsize lysize overlay
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
      damage  = case Item.strongestSword coitem bitems of
                  Just sw -> case ieffect $ iokind $ Item.jkind sw of
                    Wound dice -> show dice ++ "+" ++ show (Item.jpower sw)
                    _ -> show (Item.jpower sw)
                  Nothing -> "3d1"  -- TODO; use the item 'fist'
      bl = bla lxsize lysize ceps bloc clocation
      dis offset p@(PointXY (x0, y0)) =
        let loc0 = toPoint lxsize p
            tile = lvl `lAt` loc0
            tk = tokind tile
            items = lvl `liAt` loc0
            sm = smelltime $ IM.findWithDefault (SmellTime 0) loc0 lsmell
            sml = (sm - stime) `div` 100
            viewActor loc Actor{bkind = bkind2, bsymbol}
              | loc == bloc && slid == creturnLn =
                  (symbol, Color.defBG)  -- highlight player
              | otherwise = (symbol, acolor)
             where
              ActorKind{asymbol, acolor} = okind bkind2
              symbol = fromMaybe asymbol bsymbol
            viewSmell :: Int -> Char
            viewSmell k
              | k > 9     = '*'
              | k < 0     = '-'
              | otherwise = Char.intToDigit k
            rainbow loc = toEnum $ loc `rem` 14 + 1
            (char, fg0) =
              case L.find (\ m -> loc0 == Actor.bloc m) (IM.elems lactor) of
                _ | ctargeting /= TgtOff
                    && slid == creturnLn
                    && L.elem loc0 bl ->
                      let unknownId = ouniqGroup "unknown space"
                      in ('*', case (vis, F.Walkable `elem` tfeature tk) of
                                 _ | tile == unknownId -> Color.BrBlack
                                 (True, True)   -> Color.BrGreen
                                 (True, False)  -> Color.BrRed
                                 (False, True)  -> Color.Green
                                 (False, False) -> Color.Red)
                Just m | somniscient || vis -> viewActor loc0 m
                _ | sSml && sml >= 0 -> (viewSmell sml, rainbow loc0)
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
              if (fg == Color.defBG) || (bg == Color.defFG && fg == Color.defFG)
              then reverseVideo
              else attr
            a = case dm of
                  ColorBW   -> Color.defaultAttr
                  ColorFull -> optVisually Color.Attr{fg = fg0, bg = bg0}
        in case over (x0, y0 + offset) of
             Just c -> Color.AttrChar Color.defaultAttr c
             _      -> Color.AttrChar a char
      status =
        take 28 (ldesc ++ repeat ' ') ++
        take 9 ("L: " ++ show (Dungeon.levelNumber slid) ++ repeat ' ') ++
        take 11 ("$: " ++ show wealth ++ repeat ' ') ++
        take 14 ("Dmg: " ++ damage ++ repeat ' ') ++
        take 32 ("HP: " ++ show bhp ++
                 " (" ++ show (maxDice ahp) ++ ")" ++ repeat ' ')
      toWidth :: Int -> String -> String
      toWidth n x = take n (x ++ repeat ' ')
      disp n mesg =
        let offset = lysize * n
            fLine y =
              let f l x = let !ac = dis offset (PointXY (x, y)) in ac : l
              in L.foldl' f [] [lxsize-1,lxsize-2..0]
            sfLevel =  -- Fully evaluated.
              let f l y = let !line = fLine y in line : l
              in L.foldl' f [] [lysize-1,lysize-2..0]
            sfTop = toWidth lxsize mesg
            sfBottom = toWidth lxsize status
        in Color.SingleFrame{..}
      -- Make sure overlays or multi-line messages do not obscure animations.
      msgAnim = if ns == 0 then msgTop else ""
      basicFrame = disp (-1) msgAnim
      modifyFrame Color.SingleFrame{sfLevel = levelOld, ..} am =
        let fLine y lineOld =
              let f l (x, acOld) =
                    let loc = toPoint lxsize (PointXY (x, y))
                        !ac = fromMaybe acOld $ IM.lookup loc am
                    in ac : l
              in L.foldl' f [] (zip [lxsize-1,lxsize-2..0] (reverse lineOld))
            sfLevel =  -- Fully evaluated.
              let f l (y, lineOld) = let !line = fLine y lineOld in line : l
              in L.foldl' f [] (zip [lysize-1,lysize-2..0] (reverse levelOld))
        in Color.SingleFrame{..}
      playAnimations [] = return ()
      playAnimations (am : ams) = assert doPush $ do
        display fs True False (Just $ modifyFrame basicFrame am)
        playAnimations ams
      -- Perform overlay pages slideshow.
      perf k =
        if k < ns - 1
        then do
          display fs doPush False $ Just $ disp k msgTop
          b <- getConfirm fs doPush
          if b
            then perf (k + 1)
            else return False
        else do
          -- Play animations after the last overlay frame
          -- that requires confirmation.
          playAnimations sanim
          -- Speed up, by remving all empty frames,
          -- playing the move frames if the player is running.
          let isRunning = isJust bdir
          -- Show the basic frame. If there are overlays, that's the last
          -- overlay frame, the one that does not require confirmation.
          display fs doPush isRunning $ Just $ disp k msgTop
          return True
  in perf 0
