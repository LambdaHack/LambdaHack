{-# LANGUAGE CPP #-}
module Game.LambdaHack.Display
  ( FrontendSession, startup, shutdown, frontendName
  , nextCommandD, displayLevel, ColorMode(..)
  ) where

-- wrapper for selected Display frontend

#ifdef CURSES
import Game.LambdaHack.Display.Curses as D
#elif VTY
import Game.LambdaHack.Display.Vty as D
#elif STD
import Game.LambdaHack.Display.Std as D
#else
import Game.LambdaHack.Display.Gtk as D
#endif

-- Display routines that are independent of the selected display frontend.

import qualified Data.Char as Char
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe

import Game.LambdaHack.Msg
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.State
import Game.LambdaHack.Geometry
import Game.LambdaHack.Loc
import Game.LambdaHack.Level
import Game.LambdaHack.Effect
import Game.LambdaHack.Perception
import Game.LambdaHack.Actor as Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Item as Item
import qualified Game.LambdaHack.Keys as K
import Game.LambdaHack.WorldLoc
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Kind as Kind

-- | Next event translated to a canonical form.
nextCommandD :: FrontendSession -> IO K.Key
nextCommandD fs = do
  e <- nextEvent fs
  return $ K.canonMoveKey e

-- | Waits for a space or return or escape. The last two act this way,
-- to let keys that request information toggle display of the information off.
getConfirmD :: FrontendSession -> IO Bool
getConfirmD fs = do
  e <- nextCommandD fs
  case e of
    K.Char ' ' -> return True
    K.Char '?' -> return True
    K.Char '*' -> return True
    K.Return   -> return True
    K.Esc      -> return False
    _          -> getConfirmD fs

splitOverlay :: Int -> String -> [[String]]
splitOverlay s xs = splitOverlay' (lines xs)
 where
  splitOverlay' ls
    | length ls <= s = [ls]  -- everything fits on one screen
    | otherwise      = let (pre,post) = splitAt (s - 1) ls
                       in  (pre ++ [more]) : splitOverlay' post

-- | Returns a function that looks up the characters in the
-- string by location. Takes the height of the display plus
-- the string. Returns also the number of screens required
-- to display all of the string.
stringByLocation :: Y -> String -> (Int, (X, Y) -> Maybe Char)
stringByLocation sy xs =
  let ls = splitOverlay sy xs
      m  = M.fromList (zip [0..] (L.map (M.fromList . zip [0..]) (concat ls)))
      k  = length ls
  in (k, \ (x, y) -> M.lookup y m >>= \ n -> M.lookup x n)

data ColorMode = ColorFull | ColorBW

displayLevel :: ColorMode -> FrontendSession -> Kind.COps
             -> Perceptions -> State
             -> Msg -> Maybe String -> IO Bool
displayLevel dm fs cops per
             s@State{scursor, stime, sflavour, slid, splayer, ssensory, sdisplay}
             msg moverlay =
  let Kind.COps{ coactor=Kind.Ops{okind}
               , coitem=coitem@Kind.Ops{okind=iokind}
               , cotile=Kind.Ops{okind=tokind} } = cops
      lvl@Level{lxsize = sx, lysize = sy, lsmell = smap, ldesc} = slevel s
      (_, Actor{bkind, bhp, bloc}, bitems) = findActorAnyLevel splayer s
      ActorKind{ahp} = okind bkind
      reachable = debugTotalReachable per
      visible   = totalVisible per
      overlay   = fromMaybe "" moverlay
      (ns, over) = stringByLocation sy overlay -- n overlay screens needed
      sSml   = ssensory == Smell
      sVis   = case ssensory of Vision _ -> True; _ -> False
      sOmn   = sdisplay == Omniscient
      lAt    = if sOmn then at else rememberAt
      liAt   = if sOmn then atI else rememberAtI
      sVisBG = if sVis
               then \ vis rea -> if vis
                                 then Color.Blue
                                 else if rea
                                      then Color.Magenta
                                      else Color.defBG
                else \ _vis _rea -> Color.defBG
      wealth  = L.sum $ L.map (Item.itemPrice coitem) bitems
      damage  = case Item.strongestSword coitem bitems of
                  Just sw -> case ieffect $ iokind $ Item.jkind sw of
                    Wound dice -> show dice ++ " + " ++ show (Item.jpower sw)
                    _ -> show (Item.jpower sw)
                  Nothing -> "3d1"  -- TODO; use the item 'fist'
      hs      = levelHeroList s
      ms      = levelMonsterList s
      dis n loc0 =
        let tile = lvl `lAt` loc0
            items = lvl `liAt` loc0
            sm = smelltime $ IM.findWithDefault (SmellTime 0) loc0 smap
            sml = (sm - stime) `div` 100
            viewActor loc Actor{bkind = bkind2, bsymbol}
              | loc == bloc && slid == creturnLn scursor =
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
              case L.find (\ m -> loc0 == Actor.bloc m) (hs ++ ms) of
                Just m | sOmn || vis -> viewActor loc0 m
                _ | sSml && sml >= 0 -> (viewSmell sml, rainbow loc0)
                  | otherwise ->
                  case items of
                    [] -> let u = tokind tile
                          in (tsymbol u, if vis then tcolor u else tcolor2 u)
                    i : _ -> Item.viewItem coitem (Item.jkind i) sflavour
            vis = IS.member loc0 visible
            rea = IS.member loc0 reachable
            bg0 = if ctargeting scursor /= TgtOff && loc0 == clocation scursor
                  then Color.defFG     -- highlight target cursor
                  else sVisBG vis rea  -- FOV debug
            reverseVideo = (snd Color.defaultAttr, fst Color.defaultAttr)
            optVisually (fg, bg) =
              if (fg == Color.defBG) || (bg == Color.defFG && fg == Color.defFG)
              then reverseVideo
              else (fg, bg)
            a = case dm of
                  ColorBW   -> Color.defaultAttr
                  ColorFull -> optVisually (fg0, bg0)
        in case over (fromLoc sx loc0 `shiftXY` (0, sy * n)) of
             Just c -> (Color.defaultAttr, c)
             _      -> (a, char)
      status =
        take 27 (ldesc ++ repeat ' ') ++
        take 7 ("L: " ++ show (levelNumber slid) ++ repeat ' ') ++
        take 9 ("T: " ++ show (stime `div` 10) ++ repeat ' ') ++
        take 8 ("$: " ++ show wealth ++ repeat ' ') ++
        take 15 ("Dmg: " ++ damage ++ repeat ' ') ++
        take 30 ("HP: " ++ show bhp ++
                 " (" ++ show (maxDice ahp) ++ ")" ++ repeat ' ')
      width = fst normalLevelBound + 1
      toWidth :: Int -> String -> String
      toWidth n x = take n (x ++ repeat ' ')
      disp n mesg = display (0, 0, sx - 1, sy - 1) fs (dis n)
                      (toWidth width mesg) (toWidth width status)
      msgs = splitMsg (fst normalLevelBound + 1) msg
      perf k []     = perfo k ""
      perf k [xs]   = perfo k xs
      perf k (x:xs) = disp ns (x ++ more) >> getConfirmD fs >>= \ b ->
                      if b then perf k xs else return False
      perfo k xs
        | k < ns - 1 = disp k xs >> getConfirmD fs >>= \ b ->
                       if b then perfo (k+1) xs else return False
        | otherwise = disp k xs >> return True
  in perf 0 msgs
