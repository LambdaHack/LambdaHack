{-# LANGUAGE CPP #-}
module Game.LambdaHack.Display
  ( FrontendSession, display, startup, shutdown, frontendName
  , nextCommandD, displayBlankConfirmD, getConfirmD, getOptionalConfirmD
  , getYesNoD, displayLevel, ColorMode(..)
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
import Control.Monad.IO.Class
import Data.Maybe

import Game.LambdaHack.Msg
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.State
import Game.LambdaHack.Geometry
import Game.LambdaHack.Loc
import Game.LambdaHack.Level
import Game.LambdaHack.Perception
import Game.LambdaHack.Actor as Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Item as Item
import qualified Game.LambdaHack.Keys as K
import Game.LambdaHack.WorldLoc
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Kind as Kind

-- | Next event translated to a canonical form.
nextCommandD :: MonadIO m => FrontendSession -> M.Map K.Key K.Key -> m K.Key
nextCommandD fs macros = do
  e <- liftIO $ nextEvent fs
  return $ fromMaybe (K.canonMoveKey e) (M.lookup e macros)

-- | Displays a message on a blank screen. Waits for confirmation.
displayBlankConfirmD :: FrontendSession -> M.Map K.Key K.Key -> String
                     -> IO Bool
displayBlankConfirmD fs macros txt =
  let x = txt ++ more
      doBlank = const (Color.defaultAttr, ' ')
      (lx, ly) = normalLevelBound  -- TODO: query terminal size instead
  in do
    display (0, 0, lx, ly) (fst normalLevelBound + 1) fs doBlank x ""
    getConfirmD fs macros

-- | Waits for a space or return or '?' or '*'. The last two act this way,
-- to let keys that request information toggle display the information off.
getConfirmD :: MonadIO m => FrontendSession -> M.Map K.Key K.Key -> m Bool
getConfirmD fs macros =
  getOptionalConfirmD return (const $ getConfirmD fs macros) fs macros

getOptionalConfirmD :: MonadIO m =>
                      (Bool -> m a)
                    -> (K.Key -> m a)
                    -> FrontendSession -> M.Map K.Key K.Key
                    -> m a
getOptionalConfirmD h k fs macros = do
  e <- liftIO $ nextCommandD fs macros
  case e of
    K.Char ' ' -> h True
    K.Char '?' -> h True
    K.Char '*' -> h True
    K.Return   -> h True
    K.Esc      -> h False
    _          -> k e

-- | A yes-no confirmation.
getYesNoD :: MonadIO m => FrontendSession -> M.Map K.Key K.Key -> m Bool
getYesNoD fs macros = do
  e <- liftIO $ nextCommandD fs macros
  case e of
    K.Char 'y' -> return True
    K.Char 'n' -> return False
    K.Esc      -> return False
    _          -> getYesNoD fs macros

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

displayLevel :: ColorMode -> (FrontendSession, M.Map K.Key K.Key, Kind.COps)
             -> Perceptions -> State
             -> Msg -> Maybe String -> IO Bool
displayLevel dm (fs, macros, cops) per
             s@State{scursor, stime, sflavour, slid, splayer, ssensory, sdisplay}
             msg moverlay =
  let Kind.COps{ coactor=Kind.Ops{okind}
               , coitem
               , cotile=Kind.Ops{okind=tokind} } = cops
      lvl@Level{lxsize = sx, lysize = sy, lsmell = smap} = slevel s
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
                  Just sw -> 3 + Item.jpower sw
                  Nothing -> 3
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
            bg0 = if ctargeting scursor && loc0 == clocation scursor
                  then Color.defFG     -- highlight targeting cursor
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
        take 30 (levelName slid ++ repeat ' ') ++
        take 10 ("T: " ++ show (stime `div` 10) ++ repeat ' ') ++
        take 10 ("$: " ++ show wealth ++ repeat ' ') ++
        take 10 ("Dmg: " ++ show damage ++ repeat ' ') ++
        take 20 ("HP: " ++ show bhp ++
                 " (" ++ show (maxDice ahp) ++ ")" ++ repeat ' ')
      disp n mesg = display (0, 0, sx - 1, sy - 1) (fst normalLevelBound + 1)
                      fs (dis n) mesg status
      msgs = splitMsg (fst normalLevelBound + 1) msg
      perf k []     = perfo k ""
      perf k [xs]   = perfo k xs
      perf k (x:xs) = disp ns (x ++ more) >> getConfirmD fs macros >>= \ b ->
                      if b then perf k xs else return False
      perfo k xs
        | k < ns - 1 = disp k xs >> getConfirmD fs macros >>= \ b ->
                       if b then perfo (k+1) xs else return False
        | otherwise = disp k xs >> return True
  in perf 0 msgs
