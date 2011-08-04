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
import Control.Exception (assert)

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
import qualified Effect
import WorldLoc

-- The effectToAction function and all it depends on.
-- This file should not depend on Action.hs nor ItemAction.hs.

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The bool result indicates if the actors identify the effect.
-- TODO: separately define messages for the case when source == target
-- and for the other case; then use the messages outside of effectToAction,
-- depending on the returned bool, perception and identity of the actors.
effectToAction :: Effect.Effect -> Actor -> Actor -> Int ->
                  Action (Bool, String)
effectToAction Effect.NoEffect source target power = nullEffect
effectToAction Effect.Heal _source target power = do
  tm <- gets (getActor target)
  if mhp tm >= nhpMax (mkind tm) || power <= 0
    then nullEffect
    else do
      focusIfAHero target
      let upd m = m { mhp = min (nhpMax (mkind m)) (mhp m + power) }
      updateAnyActor target upd
      return (True, subjectMovableVerb (mkind tm) "feel" ++ " better.")
effectToAction (Effect.Wound nDm) source target power = do
  n <- liftIO $ rndToIO $ rollDice nDm
  if (n + power <= 0) then nullEffect else do
    focusIfAHero target
    tm <- gets (getActor target)
    let newHP  = mhp tm - n - power
        killed = newHP <= 0
        msg = if source == target  -- a potion of wounding, etc.
              then subjectMovableVerb (mkind tm) "feel"
                   ++ if killed then " mortally" else ""
                   ++ " wounded."
              else if killed
                   then if isAHero target
                        then ""
                        else subjectMovableVerb (mkind tm) "die" ++ "."
                   else if isAHero target
                        then subjectMovableVerb (mkind tm) "lose"
                             ++ " " ++ show (n + power) ++ "HP."
                        else subjectMovableVerb (mkind tm) "hiss" ++ " in pain."
    updateAnyActor target $ \ m -> m { mhp = newHP }  -- Damage the target.
    when killed $ do
      -- Place the actor's possessions on the map.
      modify (updateLevel (dropItemsAt (mitems tm) (mloc tm)))
      -- Clean bodies up.
      pl <- gets splayer
      if target == pl
        then checkPartyDeath  -- kills the player and checks game over
        else modify (deleteActor target)  -- kills the enemy
    return (True, msg)
effectToAction Effect.Dominate source target power =
  if isAMonster target  -- Monsters have weaker will than heroes.
  then do
         assertTrue $ selectPlayer target
         -- Prevent AI from getting a few free moves until new player ready.
         updatePlayerBody (\ m -> m { mtime = 0})
         stopRunning
         display
         return (True, "")
  else nullEffect
effectToAction Effect.SummonFriend source target power = do
  tm <- gets (getActor target)
  if isAHero source
    then summonHeroes (1 + power) (mloc tm)
    else summonMonsters (1 + power) (mloc tm)
  return (True, "")
effectToAction Effect.SummonEnemy source target power = do
  tm <- gets (getActor target)
  if not $ isAHero source  -- a trick: monster player will summon a hero
    then summonHeroes (1 + power) (mloc tm)
    else summonMonsters (1 + power) (mloc tm)
  return (True, "")
effectToAction Effect.ApplyPerfume source target _ =
  if source == target
  then return (True, "Tastes like water, but with a strong rose scent.")
  else do
    let upd lvl = lvl { lsmell = M.map (const (-100)) (lsmell lvl) }
    modify (updateLevel upd)
    return (True, "The fragrance quells all scents in the vicinity.")
effectToAction Effect.Regneration source target power =
  effectToAction Effect.Heal source target power
effectToAction Effect.Searching source target power =
  return (True, "It gets lost and you search in vain.")

nullEffect :: Action (Bool, String)
nullEffect = return (False, "Nothing happens.")

-- | The source actor affects the target actor, with a given item.
-- If either actor is a hero, the item may get identified.
itemEffectAction :: Actor -> Actor -> Item -> Action Bool
itemEffectAction source target item = do
  state <- get
  pl    <- gets splayer
  tm    <- gets (getActor target)
  per   <- currentPerception
  let effect = ItemKind.jeffect $ ItemKind.getIK $ ikind item
  -- The message describes the target part of the action.
  (b, msg) <- effectToAction effect source target (ipower item)
  -- Determine how the player perceives the event.
  -- TODO: factor it out as a function messageActor
  -- and messageActorVerb (incorporating subjectActorVerb).
  if mloc tm `S.member` ptvisible per
     then messageAdd msg
     else if not b
          then return ()  -- Victim is not seen, nothing interestng happens.
          else messageAdd "You hear some noises."
  -- If something happens, the item gets identified.
  when (b && (isAHero source || isAHero target)) $ discover item
  return b

-- | Given item is now known to the player.
discover :: Item -> Action ()
discover i = do
  state <- get
  let ik = ikind i
      obj = unwords $ tail $ words $ objectItem state i
      msg = "The " ++ obj ++ " turns out to be "
      kind = ItemKind.getIK ik
      alreadyIdentified = L.length (ItemKind.jflavour kind) == 1 ||
                          ik `S.member` sdiscoveries state
  if alreadyIdentified
    then return ()
    else do
           modify (updateDiscoveries (S.insert ik))
           state <- get
           messageAdd $ msg ++ objectItem state i ++ "."

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
    -- Display status line for the new hero.
    when b $ display >> return ()
  else return ()

summonHeroes :: Int -> Loc -> Action ()
summonHeroes n loc =
  assert (n > 0) $ do
  newHeroIndex <- gets (fst . scounter)
  modify (\ state -> iterate (addHero loc) state !! n)
  assertTrue $ selectPlayer (AHero newHeroIndex)
  -- Display status line for the new hero.
  display >> return ()

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
      go <- messageMoreConfirm True $
              subjectMovableVerb (mkind pbody) "die" ++ "."
      history  -- Prevent the messages from being repeated.
      let firstDeathEnds = Config.get config "heroes" "firstDeathEnds"
      if firstDeathEnds
        then gameOver go
        else case L.filter (\ (actor, _) -> actor /= pl) ahs of
               [] -> gameOver go
               (actor, _nln) : _ -> do
                 messageAdd "The survivors carry on."
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

-- | Handle current score and display it with the high scores. Scores
-- should not be shown during the game,
-- because ultimately the worth of items might give
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
lvlSwitch :: LevelId -> Action Bool
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
            L.map (\ i -> letterLabel (iletter i) ++ objectItem state i ++ " ")
              ((if sorted then sortBy (cmpLetter' `on` iletter) else id) is)
  let ovl = inv ++ more
  messageWipeAndSet msg
  overlay ovl

stopRunning :: Action ()
stopRunning = updatePlayerBody (\ p -> p { mdir = Nothing })

-- | Store current message in the history and reset current message.
history :: Action ()
history =
  do
    (_, sx) <- gets (lsize . slevel)
    msg     <- resetMessage
    config  <- gets sconfig
    let historyMax = Config.get config "ui" "historyMax"
        -- TODO: not ideal, continuations of sentences are atop beginnings.
        split = splitMsg sx (msg ++ " ")
    unless (L.null msg) $
      modify (updateHistory (take historyMax . (L.reverse split ++)))
