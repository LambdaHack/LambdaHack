module EffectAction where

import Control.Monad
import Control.Monad.State hiding (State)
import Data.Function
import Data.List as L
import Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Set as S
import System.Time

import Action
import Display hiding (display)
import Dungeon
import Geometry
import Grammar
import qualified HighScores as H
import Item
import qualified ItemKind
import qualified Keys as K
import Level
import LevelState
import Message
import Movable
import MovableState
import MovableKind
import MovableAdd
import Perception
import Random
import State
import qualified Config
import qualified Save
import Terrain
import qualified Effect

-- The effectToAction function and all it depends on.
-- This file should not depend on Action.hs nor ItemAction.hs.

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The bool result indicates if the actors identify the effect.
effectToAction :: Effect.Effect -> Actor -> Actor -> Int -> String ->
                  Action Bool
effectToAction Effect.NoEffect source target power msg = do
  pl <- gets splayer
  when (source == pl) $ messageAdd "Nothing happens."
  return False
effectToAction Effect.Heal source target power msg = do
  m <- gets (getActor target)
  if mhp m >= nhpMax (mkind m) || power <= 0
    then return False
    else do
      focusIfAHero target
      let upd m = m { mhp = min (nhpMax (mkind m)) (mhp m + power) }
      updateAnyActor target upd
      pl <- gets splayer
      when (target == pl) $ messageAdd "You feel better."  -- TODO: use msg, if perceived, etc. Eliminate "you" in singular, but keep it in plural.
      return True
effectToAction (Effect.Wound nDm) source target power msg = do
  n <- liftIO $ rndToIO $ rollDice nDm
  if (n + power <= 0) then return False else do
    focusIfAHero target
    pl <- gets splayer
    sm <- gets (getActor source)
    tm <- gets (getActor target)
    per <- currentPerception
    let newHP  = mhp tm - n - power
        killed = newHP <= 0
        svis = mloc sm `S.member` ptvisible per
        tvis = mloc tm `S.member` ptvisible per
    -- Determine how the player perceives the event.
    if source == target && not tvis
       then return ()  -- Unseen monster quaffs a potion of wounding.
       else messageAdd $
         if source == target && target == pl
         then "You feel wounded."
         else if not tvis
              then "You hear some noises."
              else if source == target || not svis
                   then subjectMovableVerb (mkind tm) "yell" ++ " in pain."  -- TODO: use msg
                   else let combatVerb = if killed then "kill" else "hit"
                        in  subjectVerbMObject sm combatVerb tm msg
    updateAnyActor target $ \ m -> m { mhp = newHP }  -- Damage the target.
    when killed $ do
      -- Place the actor's possessions on the map.
      dropItemsAt (mitems tm) (mloc tm)
      -- Clean bodies up.
      pl <- gets splayer
      if target == pl
        then checkPartyDeath  -- kills the player and checks game over
        else modify (deleteActor target)  -- kills the enemy
    return True
effectToAction Effect.Dominate source target power msg =
  if isAMonster target  -- Monsters have weaker will than heroes.
  then do
         b <- selectPlayer target
         -- Prevent AI from getting a few free moves until new player ready.
         updatePlayerBody (\ m -> m { mtime = 0})
         stopRunning
         return b
  else return False
effectToAction Effect.SummonFriend source target power msg = do
  tm <- gets (getActor target)
  if isAHero source
    then summonHeroes (1 + power) (mloc tm)
    else summonMonsters (1 + power) (mloc tm)
  return True
effectToAction Effect.SummonEnemy source target power msg = do
  tm <- gets (getActor target)
  if not $ isAHero source
    then summonHeroes (1 + power) (mloc tm)
    else summonMonsters (1 + power) (mloc tm)
  return True
effectToAction Effect.ApplyPerfume source target _ _ = do
  pl <- gets splayer
  if source == pl && target == pl
    then messageAdd "Tastes like water. No good." >>
         return False
    else do
      let upd lvl = lvl { lsmell = M.map (const (-100)) (lsmell lvl) }
      modify (updateLevel upd)
      messageAdd "The fragrance quells all scents."
      return True
effectToAction Effect.Regneration source target power msg =
  effectToAction Effect.Heal source target power msg
effectToAction Effect.Searching source target power msg = do
  pl <- gets splayer
  when (source == pl) $ messageAdd "It gets lost and you search in vain."
  return True

-- | The source actor affects the target actor, with a given item.
-- If either actor is a hero, the item may get identified.
itemEffectAction :: Item -> Actor -> Actor -> Action Bool
itemEffectAction item source target = do
  state <- get
  let effect = ItemKind.jeffect $ ItemKind.getIK $ ikind item
      msg = " with " ++ objectItem state item
  b <- effectToAction effect source target (ipower item) msg
  -- If something happens, the item gets identified.
  when (b && (isAHero source || isAHero target)) $ discover item
  return b

-- | Given item is now known to the player.
discover :: Item -> Action ()
discover i = modify (updateDiscoveries (S.insert (ikind i)))

-- | Selects a movable for the player, based on the actor.
-- Focuses on the hero if level changed. False, if nothing to do.
selectPlayer :: Actor -> Action Bool
selectPlayer actor =
  do
    pl <- gets splayer
    if (actor == pl)
      then return False -- already selected
      else do
        state <- get
        case findActorAnyLevel actor state of
          Nothing -> abortWith $ "No such member of the party."
          Just (nln, pbody) -> do
            -- Make the new actor the player-controlled actor.
            modify (\ s -> s { splayer = actor })
            -- Record the original level of the new player.
            modify (updateCursor (\ c -> c { creturnLn = nln }))
            -- Switch to the level.
            lvlSwitch nln
            -- Set smell display, depending on player capabilities.
            -- This also reset FOV mode.
            modify (\ s -> s { ssensory = if MovableKind.nsmell (mkind pbody)
                                          then Smell
                                          else Implicit })
            -- Announce.
            messageAdd $ subjectMovable (mkind pbody) ++ " selected."
            return True

focusIfAHero :: Actor -> Action ()
focusIfAHero target =
  if isAHero target
  then do
    -- Focus on the hero being wounded.
    b <- selectPlayer target
    -- Extra prompt, in case many heroes wounded in one turn.
    when b $ messageAddMore >> return ()
  else return ()

summonHeroes :: Int -> Loc -> Action ()
summonHeroes n loc = modify (\ state -> iterate (addHero loc) state !! n)

summonMonsters :: Int -> Loc -> Action ()
summonMonsters n loc = do
  let fmk = Frequency $ L.zip (L.map nfreq dungeonMonsters) dungeonMonsters
  mk <- liftIO $ rndToIO $ frequency fmk
  modify (\ state -> iterate (addMonster mk (nhpMax mk) loc) state !! n)

-- | Remove dead heroes, check if game over.
-- For now we only check the selected hero, but if poison, etc.
-- is implemented, we'd need to check all heroes on the level.
checkPartyDeath :: Action ()
checkPartyDeath =
  do
    ahs    <- gets allHeroesAnyLevel
    pl     <- gets splayer
    pbody  <- gets getPlayerBody
    config <- gets sconfig
    when (mhp pbody <= 0) $ do  -- TODO: change to guard? define mzero? Why are the writes to to files performed when I call abort later? That probably breaks the laws of MonadPlus.
      messageAddMore
      go <- messageMoreConfirm $ subjectMovableVerb (mkind pbody) "die" ++ "."
      let firstDeathEnds = Config.get config "heroes" "firstDeathEnds"
      if firstDeathEnds
        then gameOver go
        else case L.filter (\ (actor, _) -> actor /= pl) ahs of
               [] -> gameOver go
               (actor, _nln) : _ -> do
                 message "The survivors carry on."
                 -- Remove the dead player.
                 modify (deleteActor pl)
                 -- At this place the invariant that the player exists fails.
                 -- Focus on the new hero (invariant not needed).
                 assertTrue $ selectPlayer actor
                 -- At this place the invariant is restored again.

-- | End game, showing the ending screens, if requested.
gameOver :: Bool -> Action ()
gameOver showEndingScreens =
  do
    when showEndingScreens $ do
      state <- get
      ln <- gets (lname . slevel)
      let total = calculateTotal state
          status = H.Killed ln
      handleScores True status total
      messageMore "Let's hope another party can save the day!"
    end

-- | Calculate loot's worth for heroes on the current level.
calculateTotal :: State -> Int
calculateTotal s =
  L.sum $ L.map itemPrice $ L.concatMap mitems (levelHeroList s)

dropItemsAt :: [Item] -> Loc -> Action ()
dropItemsAt is loc = modify (updateLevel (scatterItems is loc))

-- | Handle current score and display it with the high scores. Scores
-- should not be shown during the game, because ultimately the worth of items might give
-- information about the nature of the items.
-- False if display of the scores was void or interrupted by the user
handleScores :: Bool -> H.Status -> Int -> Action Bool
handleScores write status total =
  if (total == 0)
  then return False
  else do
    config  <- gets sconfig
    time    <- gets stime
    curDate <- liftIO getClockTime
    let points = case status of
                   H.Killed _ -> (total + 1) `div` 2
                   _ -> total
    let score = H.ScoreRecord points (-time) curDate status
    (placeMsg, slideshow) <- liftIO $ H.register config write score
    messageOverlaysConfirm placeMsg slideshow

-- | Perform a level switch to a given level. False, if nothing to do.
lvlSwitch :: LevelName -> Action Bool
lvlSwitch nln =
  do
    ln <- gets (lname . slevel)
    if (nln == ln)
      then return False
      else do
        level   <- gets slevel
        dungeon <- gets sdungeon
        -- put back current level
        -- (first put back, then get, in case we change to the same level!)
        let full = putDungeonLevel level dungeon
        -- get new level
        let (new, ndng) = getDungeonLevel nln full
        modify (\ s -> s { sdungeon = ndng, slevel = new })
        return True

-- effectToAction does not depend on this function right now, but it might,
-- and I know no better place to put it.
displayItems :: Message -> Bool -> [Item] -> Action Bool
displayItems msg sorted is = do
  state <- get
  let inv = unlines $
            L.map (\ i ->
                    letterLabel (iletter i) ++ objectItem state i ++ " ")
            ((if sorted then sortBy (cmpLetter' `on` iletter) else id) is)
  let ovl = inv ++ more
  message msg
  overlay ovl

stopRunning :: Action ()
stopRunning = updatePlayerBody (\ p -> p { mdir = Nothing })
