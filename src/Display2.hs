module Display2 (module Display, module Display2) where

import Data.Set as S
import Data.List as L
import Data.Map as M

import Message
import Display
import State
import Geometry
import Level
import Perception
import Monster
import Item

-- | Displays a message on a blank screen. Waits for confirmation.
displayBlankConfirm :: Session -> String -> IO Bool
displayBlankConfirm session txt =
  let x = txt ++ more
  in  do
        display ((0,0),(0,length x - 1)) session (const (attr, ' ')) x ""
        getConfirm session

-- | Waits for a space or return.
getConfirm :: Session -> IO Bool
getConfirm session =
  getOptionalConfirm session return (const $ getConfirm session)

getOptionalConfirm :: Session -> (Bool -> IO a) -> (String -> IO a) -> IO a
getOptionalConfirm session h k =
  do
    e <- nextEvent session
    handleModifier e (getOptionalConfirm session h k) $
      case e of
        "space"  -> h True
        "Return" -> h True
        "Escape" -> h False
        _        -> k e

-- | Handler that ignores modifier events as they are
--   currently produced by the Gtk frontend.
handleModifier :: String -> IO a -> IO a -> IO a
handleModifier e h k =
  case e of
    "Shift_R"   -> h
    "Shift_L"   -> h
    "Control_L" -> h
    "Control_R" -> h
    "Super_L"   -> h
    "Super_R"   -> h
    "Menu"      -> h
    "Alt_L"     -> h
    "Alt_R"     -> h
    _           -> k

-- | Configurable event handler for the direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as open/close.
handleDirection :: String -> ((Y,X) -> IO ()) -> IO () -> IO ()
handleDirection e h k =
  case e of
    "k" -> h (-1,0)
    "j" -> h (1,0)
    "h" -> h (0,-1)
    "l" -> h (0,1)
    "y" -> h (-1,-1)
    "u" -> h (-1,1)
    "b" -> h (1,-1)
    "n" -> h (1,1)
    _   -> k

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
stringByLocation :: Y -> String -> (Int, Loc -> Maybe Char)
stringByLocation sy xs =
  let
    ls   = splitOverlay sy xs
    m    = M.fromList (zip [0..] (L.map (M.fromList . zip [0..]) (concat ls)))
    k    = length ls
  in
    (k, \ (y,x) -> M.lookup y m >>= \ n -> M.lookup x n)

displayLevel :: Session -> Level -> Perception -> State -> Message -> IO ()
displayLevel session lvl per state msg = displayOverlay session lvl per state msg "" >> return ()

displayOverlay :: Session -> Level -> Perception -> State -> Message -> String -> IO Bool
displayOverlay session (lvl@(Level nm sz@(sy,sx) ms smap nlmap lmeta))
                     per
                     (state@(State { splayer = player@(Monster { mhp = php, mdir = pdir, mloc = ploc }), stime = time, sassocs = assocs }))
                     msg overlay =
    let
      reachable = preachable per
      visible   = pvisible per
      sSml    = ssensory state == Smell
      sVis    = ssensory state == Vision
      sOmn    = sdisplay state == Omniscient
      sTer    = case sdisplay state of Terrain n -> n; _ -> 0
      lAt     = if sOmn || sTer > 0 then at else rememberAt
      lVision = if sVis
                  then \ vis rea ->
                       if      vis then setBG blue
                       else if rea then setBG magenta
                                   else id
                  else \ vis rea -> id
      (n,over) = stringByLocation (sy+1) overlay -- n is the number of overlay screens
      gold    = maybe 0 (icount . fst) $ findItem (\ i -> iletter i == Just '$') (mitems player)
      disp n msg = 
        display ((0,0),sz) session 
                 (\ loc -> let tile = nlmap `lAt` loc
                               sml  = ((smap ! loc) - time) `div` 100
                               vis  = S.member loc visible
                               rea  = S.member loc reachable
                               (rv,ra) = case L.find (\ m -> loc == mloc m) (player:ms) of
                                           _ | sTer > 0          -> viewTerrain sTer False (tterrain tile)
                                           Just m | sOmn || vis  -> viewMonster (mtype m) 
                                           _ | sSml && sml >= 0  -> viewSmell sml
                                             | otherwise         -> viewTile vis tile assocs
                               vision = lVision vis rea
                           in
                             case over (loc `shift` ((sy+1) * n, 0)) of
                               Just c  ->  (attr, c)
                               _       ->  (ra . vision $ attr, rv))
                msg
                (take 40 (levelName nm ++ repeat ' ') ++
                 take 10 ("$: " ++ show gold ++ repeat ' ') ++
                 take 15 ("HP: " ++ show php ++ " (" ++ show playerHP ++ ")" ++ repeat ' ') ++
                 take 10 ("T: " ++ show (time `div` 10) ++ repeat ' '))
      msgs = splitMsg sx msg
      perf k []     = perfo k ""
      perf k [xs]   = perfo k xs
      perf k (x:xs) = disp n (x ++ more) >> getConfirm session >>= \ b ->
                      if b then perf k xs else return False
      perfo k xs
        | k < n - 1 = disp k xs >> getConfirm session >>= \ b ->
                      if b then perfo (k+1) xs else return False
        | otherwise = disp k xs >> return True
    in perf 0 msgs


