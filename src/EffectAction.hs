module EffectAction where

import Control.Monad
import Control.Monad.State hiding (State, state)
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.Time
import Control.Exception (assert)

import Action
import Actor
import ActorState
import qualified ActorKind
import ActorAdd
import Display hiding (display)
import Dungeon
import Geometry
import Grammar
import qualified HighScores as H
import Item
import qualified ItemKind
import Level
import Message
import Perception
import Random
import State
import qualified Config
import qualified Effect
import WorldLoc
import qualified Kind

-- The effectToAction function and all it depends on.
-- This file should not depend on Action.hs nor ItemAction.hs.

-- | The source actor affects the target actor, with a given effect and power.
-- The second argument is verbosity of the resulting message.
-- TODO: instead of verbosity return msg components and tailor them outside?
-- Both actors are on the current level and can be the same actor.
-- The bool result indicates if the actors identify the effect.
-- TODO: separately define messages for the case when source == target
-- and for the other case; then use the messages outside of effectToAction,
-- depending on the returned bool, perception and identity of the actors.
effectToAction :: Effect.Effect -> Int -> ActorId -> ActorId -> Int ->
                  Action (Bool, String)
effectToAction Effect.NoEffect _ _ _ _ = nullEffect
effectToAction Effect.Heal _ _source target power = do
  let bhpMax m = maxDice (ActorKind.bhp $ Kind.getKind $ akind m)
  tm <- gets (getActor target)
  if ahp tm >= bhpMax tm || power <= 0
    then nullEffect
    else do
      focusIfAHero target
      let upd m = m { ahp = min (bhpMax m) (ahp m + power) }
      updateAnyActor target upd
      return (True, subjectActorVerb tm "feel" ++ " better.")
effectToAction (Effect.Wound nDm) verbosity source target power = do
  n <- liftIO $ rndToIO $ rollDice nDm
  if n + power <= 0 then nullEffect else do
    focusIfAHero target
    tm <- gets (getActor target)
    let newHP  = ahp tm - n - power
        killed = newHP <= 0
        msg
          | source == target =  -- a potion of wounding, etc.
            subjectActorVerb tm "feel" ++
              if killed then " mortally" else "" ++ " wounded."
          | killed =
            if isAHero target then "" else
              subjectActorVerb tm "die" ++ "."
          | verbosity <= 0 = ""
          | isAHero target =
            subjectActorVerb tm "lose" ++
              " " ++ show (n + power) ++ "HP."
          | otherwise = subjectActorVerb tm "hiss" ++ " in pain."
    updateAnyActor target $ \ m -> m { ahp = newHP }  -- Damage the target.
    when killed $ do
      -- Place the actor's possessions on the map.
      modify (updateLevel (dropItemsAt (aitems tm) (aloc tm)))
      -- Clean bodies up.
      pl <- gets splayer
      if target == pl
        then checkPartyDeath  -- kills the player and checks game over
        else modify (deleteActor target)  -- kills the enemy
    return (True, msg)
effectToAction Effect.Dominate _ _source target _power =
  if isAMonster target  -- Monsters have weaker will than heroes.
  then do
         assertTrue $ selectPlayer target
         -- Prevent AI from getting a few free moves until new player ready.
         updatePlayerBody (\ m -> m { atime = 0})
         display
         return (True, "")
  else nullEffect
effectToAction Effect.SummonFriend _ source target power = do
  tm <- gets (getActor target)
  if isAHero source
    then summonHeroes (1 + power) (aloc tm)
    else summonMonsters (1 + power) (aloc tm)
  return (True, "")
effectToAction Effect.SummonEnemy _ source target power = do
  tm <- gets (getActor target)
  if not $ isAHero source  -- a trick: monster player will summon a hero
    then summonHeroes (1 + power) (aloc tm)
    else summonMonsters (1 + power) (aloc tm)
  return (True, "")
effectToAction Effect.ApplyPerfume _ source target _ =
  if source == target
  then return (True, "Tastes like water, but with a strong rose scent.")
  else do
    let upd lvl = lvl { lsmell = M.map (const (-100)) (lsmell lvl) }
    modify (updateLevel upd)
    return (True, "The fragrance quells all scents in the vicinity.")
effectToAction Effect.Regneration verbosity source target power =
  effectToAction Effect.Heal verbosity source target power
effectToAction Effect.Searching _ _source _target _power =
  return (True, "It gets lost and you search in vain.")
effectToAction Effect.Teleport _ _ _ _ = nullEffect  -- TODO

nullEffect :: Action (Bool, String)
nullEffect = return (False, "Nothing happens.")

-- | The source actor affects the target actor, with a given item.
-- If either actor is a hero, the item may get identified.
itemEffectAction :: Int -> ActorId -> ActorId -> Item -> Action Bool
itemEffectAction verbosity source target item = do
  tm  <- gets (getActor target)
  per <- currentPerception
  let effect = ItemKind.jeffect $ Kind.getKind $ ikind item
  -- The message describes the target part of the action.
  (b, msg) <- effectToAction effect verbosity source target (ipower item)
  -- Determine how the player perceives the event.
  -- TODO: factor it out as a function messageActor
  -- and messageActorVerb (incorporating subjectActorVerb).
  if aloc tm `S.member` ptvisible per
     then messageAdd msg
     else unless b $
            -- victim is not seen and but somethig interestng happens
            messageAdd "You hear some noises."
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
      kind = Kind.getKind ik
      alreadyIdentified = L.length (ItemKind.jflavour kind) == 1 ||
                          ik `S.member` sdiscoveries state
  unless alreadyIdentified $ do
    modify (updateDiscoveries (S.insert ik))
    state2 <- get
    messageAdd $ msg ++ objectItem state2 i ++ "."

-- | Make the actor controlled by the player.
-- Focus on the actor if level changes. False, if nothing to do.
selectPlayer :: ActorId -> Action Bool
selectPlayer actor =
  do
    pl <- gets splayer
    if actor == pl
      then return False -- already selected
      else do
        state <- get
        case findActorAnyLevel actor state of
          Nothing -> abortWith "No such member of the party."
          Just (nln, pbody) -> do
            -- Make the new actor the player-controlled actor.
            modify (\ s -> s { splayer = actor })
            -- Record the original level of the new player.
            modify (updateCursor (\ c -> c { creturnLn = nln }))
            -- Don't continue an old run, if any.
            stopRunning
            -- Switch to the level.
            lvlSwitch nln
            -- Set smell display, depending on player capabilities.
            -- This also resets FOV mode.
            modify (\ s -> s { ssensory =
                                 if ActorKind.bsmell $ Kind.getKind $
                                    akind pbody
                                 then Smell
                                 else Implicit })
            -- Announce.
            messageAdd $ subjectActor pbody ++ " selected."
            return True

focusIfAHero :: ActorId -> Action ()
focusIfAHero target =
  when (isAHero target) $ do
    -- Focus on the hero being wounded/displaced/etc.
    b <- selectPlayer target
    -- Display status line for the new hero.
    when b $ display >> return ()

summonHeroes :: Int -> Loc -> Action ()
summonHeroes n loc =
  assert (n > 0) $ do
  newHeroId <- gets (fst . scounter)
  modify (\ state -> iterate (addHero loc) state !! n)
  assertTrue $ selectPlayer (AHero newHeroId)
  -- Display status line for the new hero.
  display >> return ()

summonMonsters :: Int -> Loc -> Action ()
summonMonsters n loc = do
  (mk, k) <- liftIO $ rndToIO $ frequency Kind.frequency
  hp <- liftIO $ rndToIO $ rollDice $ ActorKind.bhp k
  modify (\ state ->
           iterate (addMonster mk hp loc) state !! n)

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
    when (ahp pbody <= 0) $ do  -- TODO: change to guard? define mzero? Why are the writes to to files performed when I call abort later? That probably breaks the laws of MonadPlus.
      go <- messageMoreConfirm ColorBW $
              subjectActorVerb pbody "die" ++ "."
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
  L.sum $ L.map itemPrice $ L.concatMap aitems (levelHeroList s)

-- | Handle current score and display it with the high scores. Scores
-- should not be shown during the game,
-- because ultimately the worth of items might give
-- information about the nature of the items.
-- False if display of the scores was void or interrupted by the user
handleScores :: Bool -> H.Status -> Int -> Action Bool
handleScores write status total =
  if total == 0
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
    if nln == ln
      then return False
      else do
        level <- gets slevel
        dng   <- gets sdungeon
        -- put back current level
        -- (first put back, then get, in case we change to the same level!)
        let full = putDungeonLevel level dng
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
              ((if sorted then L.sortBy (cmpLetter' `on` iletter) else id) is)
  let ovl = inv ++ more
  messageReset msg
  overlay ovl

stopRunning :: Action ()
stopRunning = updatePlayerBody (\ p -> p { adir = Nothing })

-- | Store current message in the history and reset current message.
history :: Action ()
history =
  do
    (_, sx) <- gets (lsize . slevel)
    msg     <- currentMessage
    messageClear
    config  <- gets sconfig
    let historyMax = Config.get config "ui" "historyMax"
        -- TODO: not ideal, continuations of sentences are atop beginnings.
        splitS = splitMsg sx (msg ++ " ")
    unless (L.null msg) $
      modify (updateHistory (take historyMax . (L.reverse splitS ++)))
