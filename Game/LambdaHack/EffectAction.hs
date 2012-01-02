module Game.LambdaHack.EffectAction where

import Control.Monad
import Control.Monad.State hiding (State, state)
import Data.Function
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Maybe
import System.Time

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.ActorAdd
import Game.LambdaHack.Display
import Game.LambdaHack.Grammar
import Game.LambdaHack.Geometry
import Game.LambdaHack.Loc
import qualified Game.LambdaHack.HighScores as H
import Game.LambdaHack.Item
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.LevelState
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Effect as Effect
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Save as Save

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
effectToAction :: Effect.Effect -> Int -> ActorId -> ActorId -> Int
               -> Action (Bool, String)
effectToAction Effect.NoEffect _ _ _ _ = nullEffect
effectToAction Effect.Heal _ _source target power = do
  coactor@Kind.Ops{okind} <- contentf Kind.coactor
  let bhpMax m = maxDice (ahp $ okind $ bkind m)
  tm <- gets (getActor target)
  if bhp tm >= bhpMax tm || power <= 0
    then nullEffect
    else do
      focusIfAHero target
      updateAnyActor target (addHp coactor power)  -- TODO: duplicates maxDice, etc.
      return (True, subjectActorVerb coactor tm "feel" ++ " better.")
effectToAction (Effect.Wound nDm) verbosity source target power = do
  coactor <- contentf Kind.coactor
  n <- rndToAction $ rollDice nDm
  if n + power <= 0 then nullEffect else do
    focusIfAHero target
    tm <- gets (getActor target)
    let newHP  = bhp tm - n - power
        killed = newHP <= 0
        msg
          | source == target =  -- a potion of wounding, etc.
            subjectActorVerb coactor tm "feel" ++
              -- TODO: this is displayed too late, after hero death, etc.
              if killed then " mortally" else "" ++ " wounded."
          | killed =
            if isAHero target
            then ""
            else subjectActorVerb coactor tm "die" ++ "."
          | verbosity <= 0 = ""
          | isAHero target =
            subjectActorVerb coactor tm "lose" ++
              " " ++ show (n + power) ++ "HP."
          | otherwise = subjectActorVerb coactor tm "hiss" ++ " in pain."
    updateAnyActor target $ \ m -> m { bhp = newHP }  -- Damage the target.
    when killed $ do
      -- Place the actor's possessions on the map.
      bitems <- gets (getActorItem target)
      modify (updateLevel (dropItemsAt bitems (bloc tm)))
      -- Clean bodies up.
      pl <- gets splayer
      if target == pl
        then checkPartyDeath  -- kills the player and checks game over
        else modify (deleteActor target)  -- kills the enemy
    return (True, msg)
effectToAction Effect.Dominate _ source target _power =
  if isAMonster target  -- Monsters have weaker will than heroes.
  then do
    selectPlayer target
      >>= assert `trueM` (source, target, "player dominates himself")
    -- Prevent AI from getting a few free moves until new player ready.
    updatePlayerBody (\ m -> m { btime = 0})
    displayAll
    return (True, "")
  else nullEffect
effectToAction Effect.SummonFriend _ source target power = do
  tm <- gets (getActor target)
  if isAHero source
    then summonHeroes (1 + power) (bloc tm)
    else summonMonsters (1 + power) (bloc tm)
  return (True, "")
effectToAction Effect.SummonEnemy _ source target power = do
  tm <- gets (getActor target)
  if not $ isAHero source  -- a trick: monster player will summon a hero
    then summonHeroes (1 + power) (bloc tm)
    else summonMonsters (1 + power) (bloc tm)
  return (True, "")
effectToAction Effect.ApplyPerfume _ source target _ =
  if source == target
  then return (True, "Tastes like water, but with a strong rose scent.")
  else do
    let upd lvl = lvl { lsmell = IM.empty }
    modify (updateLevel upd)
    return (True, "The fragrance quells all scents in the vicinity.")
effectToAction Effect.Regeneration verbosity source target power =
  effectToAction Effect.Heal verbosity source target power
effectToAction Effect.Searching _ _source _target _power =
  return (True, "It gets lost and you search in vain.")
effectToAction Effect.Ascend _ _ target power = do
  coactor <- contentf Kind.coactor
  tm <- gets (getActor target)
  effLvlvGoUp (power + 1)
  return (True, subjectActorVerb coactor tm "get" ++ " yanked upwards.")
effectToAction Effect.Descend _ _ target power = do
  coactor <- contentf Kind.coactor
  tm <- gets (getActor target)
  effLvlvGoUp (- (power + 1))
  return (True, subjectActorVerb coactor tm "get" ++ " yanked downwards.")

nullEffect :: Action (Bool, String)
nullEffect = return (False, "Nothing happens.")

effLvlvGoUp :: Int -> Action ()
effLvlvGoUp k = do
  targeting <- gets (ctargeting . scursor)
  pbody     <- gets getPlayerBody
  pl        <- gets splayer
  slid      <- gets slid
  st        <- get
  case whereTo st k of
    Nothing -> do -- we are at the "end" of the dungeon
      b <- msgYesNo "Really escape the dungeon?"
      if b
        then fleeDungeon
        else abortWith "Game resumed."
    Just (nln, nloc) ->
      assert (nln /= slid `blame` (nln, "stairs looped")) $
      tryWith (abortWith "somebody blocks the staircase") $ do
        bitems <- gets getPlayerItem
        -- Remove the player from the old level.
        modify (deleteActor pl)
        hs <- gets levelHeroList
        -- Monsters hear that players not on the level. Cancel smell.
        -- Reduces memory load and savefile size.
        when (L.null hs) $
          modify (updateLevel (updateSmell (const IM.empty)))
        -- At this place the invariant that the player exists fails.
        -- Change to the new level (invariant not needed).
        modify (\ state -> state {slid = nln})
        -- Add the player to the new level.
        modify (insertActor pl pbody)
        modify (updateAnyActorItem pl (const bitems))
        -- At this place the invariant is restored again.
        -- Land the player at the other end of the stairs.
        updatePlayerBody (\ p -> p { bloc = nloc })
        -- Change the level of the player recorded in cursor.
        modify (updateCursor (\ c -> c { creturnLn = nln }))
        -- Bail out if anybody blocks the staircase.
        inhabitants <- gets (locToActors nloc)
        when (length inhabitants > 1) abort
        -- The invariant "at most one actor on a tile" restored.
        -- Create a backup of the savegame.
        state <- get
        liftIO $ do
          Save.saveGame state
          Save.mvBkp (sconfig state)
        when (targeting /= TgtOff) doLook  -- TODO: lags behind perception

-- | Hero has left the dungeon.
fleeDungeon :: Action ()
fleeDungeon = do
  coitem <- contentf Kind.coitem
  state <- get
  let total = calculateTotal coitem state
      items = L.concat $ IM.elems $ lheroItem $ slevel state
  if total == 0
    then do
      go <- msgClear >> msgMoreConfirm ColorFull "Coward!"
      when go $
        msgMore "Next time try to grab some loot before escape!"
      end
    else do
      let winMsg = "Congratulations, you won! Your loot, worth " ++
                   show total ++ " gold, is:"  -- TODO: use the name of the '$' item instead
      displayItems winMsg True items
      go <- session getConfirm
      when go $ do
        go2 <- handleScores True H.Victor total
        when go2 $ msgMore "Can it be done better, though?"
      end

-- | The source actor affects the target actor, with a given item.
-- If either actor is a hero, the item may get identified.
itemEffectAction :: Int -> ActorId -> ActorId -> Item -> Action Bool
itemEffectAction verbosity source target item = do
  Kind.Ops{okind} <- contentf Kind.coitem
  tm  <- gets (getActor target)
  per <- currentPerception
  let effect = ieffect $ okind $ jkind item
  -- The msg describes the target part of the action.
  (b, msg) <- effectToAction effect verbosity source target (jpower item)
  -- Determine how the player perceives the event.
  -- TODO: factor it out as a function msgActor
  -- and msgActorVerb (incorporating subjectActorVerb).
  if bloc tm `IS.member` totalVisible per
     then msgAdd msg
     else unless b $
            -- victim is not seen and but somethig interestng happens
            msgAdd "You hear some noises."
  -- If something happens, the item gets identified.
  when (b && (isAHero source || isAHero target)) $ discover item
  return b

-- | Given item is now known to the player.
discover :: Item -> Action ()
discover i = do
  cops@Kind.Ops{okind} <- contentf Kind.coitem
  state <- get
  let ik = jkind i
      obj = unwords $ tail $ words $ objectItem cops state i
      msg = "The " ++ obj ++ " turns out to be "
      kind = okind ik
      alreadyIdentified = L.length (iflavour kind) == 1
                          || ik `S.member` sdisco state
  unless alreadyIdentified $ do
    modify (updateDiscoveries (S.insert ik))
    state2 <- get
    msgAdd $ msg ++ objectItem cops state2 i ++ "."

-- | Make the actor controlled by the player.
-- Focus on the actor if level changes. False, if nothing to do.
selectPlayer :: ActorId -> Action Bool
selectPlayer actor = do
  cops@Kind.Ops{okind} <- contentf Kind.coactor
  pl <- gets splayer
  targeting <- gets (ctargeting . scursor)
  if actor == pl
    then return False -- already selected
    else do
      state <- get
      when (absentHero actor state) $ abortWith "No such member of the party."
      let (nln, pbody, _) = findActorAnyLevel actor state
      -- Make the new actor the player-controlled actor.
      modify (\ s -> s { splayer = actor })
      -- Record the original level of the new player.
      modify (updateCursor (\ c -> c { creturnLn = nln }))
      -- Don't continue an old run, if any.
      stopRunning
      -- Switch to the level.
      modify (\ s -> s{slid = nln})
      -- Set smell display, depending on player capabilities.
      -- This also resets FOV mode.
      modify (\ s -> s { ssensory =
                           if asmell $ okind $
                              bkind pbody
                           then Smell
                           else Implicit })
      -- Announce.
      msgAdd $ subjectActor cops pbody ++ " selected."
      when (targeting /= TgtOff) doLook
      return True

focusIfAHero :: ActorId -> Action ()
focusIfAHero target =
  when (isAHero target) $ do
    -- Focus on the hero being wounded/displaced/etc.
    b <- selectPlayer target
    -- Display status line for the new hero.
    when b $ displayAll >> return ()

summonHeroes :: Int -> Loc -> Action ()
summonHeroes n loc =
  assert (n > 0) $ do
  cops <- contentOps
  newHeroId <- gets (fst . scounter)
  modify (\ state -> iterate (addHero cops loc) state !! n)
  selectPlayer (AHero newHeroId)
    >>= assert `trueM` (newHeroId, "player summons himself")
  -- Display status line for the new hero.
  displayAll >> return ()

summonMonsters :: Int -> Loc -> Action ()
summonMonsters n loc = do
  Kind.COps{cotile, coactor=Kind.Ops{opick, okind}} <- contentOps
  mk <- rndToAction $ opick (const True)
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  modify (\ state ->
           iterate (addMonster cotile mk hp loc) state !! n)

-- | Remove dead heroes (or dominated monsters), check if game over.
-- For now we only check the selected hero and at current level,
-- but if poison, etc. is implemented, we'd need to check all heroes
-- on any level.
checkPartyDeath :: Action ()
checkPartyDeath = do
  cops   <- contentf Kind.coactor
  ahs    <- gets allHeroesAnyLevel
  pl     <- gets splayer
  pbody  <- gets getPlayerBody
  config <- gets sconfig
  when (bhp pbody <= 0) $ do  -- TODO: change to guard? define mzero? Why are the writes to the files performed when I call abort later? That probably breaks the laws of MonadPlus.
    go <- msgMoreConfirm ColorBW $
            subjectActorVerb cops pbody "die" ++ "."
    history  -- Prevent the msgs from being repeated.
    let firstDeathEnds = Config.get config "heroes" "firstDeathEnds"
    if firstDeathEnds
      then gameOver go
      else case L.filter (\ (actor, _) -> actor /= pl) ahs of
             [] -> gameOver go
             (actor, _nln) : _ -> do
               msgAdd "The survivors carry on."
               -- Remove the dead player.
               modify deletePlayer
               -- At this place the invariant that the player exists fails.
               -- Focus on the new hero (invariant not needed).
               selectPlayer actor
                 >>= assert `trueM` (pl, actor, "player resurrects")
               -- At this place the invariant is restored again.

-- | End game, showing the ending screens, if requested.
gameOver :: Bool -> Action ()
gameOver showEndingScreens = do
  when showEndingScreens $ do
    cops  <- contentf Kind.coitem
    state <- get
    slid  <- gets slid
    let total = calculateTotal cops state
        status = H.Killed slid
    handleScores True status total
    msgMore "Let's hope another party can save the day!"
  end

-- | Calculate loot's worth for heroes on the current level.
calculateTotal :: Kind.Ops ItemKind -> State -> Int
calculateTotal cops s =
  L.sum $ L.map (itemPrice cops) $ L.concat $ IM.elems $ lheroItem $ slevel s

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
    msgOverlaysConfirm placeMsg slideshow

-- effectToAction does not depend on this function right now, but it might,
-- and I know no better place to put it.
displayItems :: Msg -> Bool -> [Item] -> Action Bool
displayItems msg sorted is = do
  cops  <- contentf Kind.coitem
  state <- get
  let inv = unlines $
            L.map (\ i -> letterLabel (jletter i)
                          ++ objectItem cops state i ++ " ")
              ((if sorted then L.sortBy (cmpLetter' `on` jletter) else id) is)
  let ovl = inv ++ more
  msgReset msg
  overlay ovl

stopRunning :: Action ()
stopRunning = updatePlayerBody (\ p -> p { bdir = Nothing })

-- | Store current msg in the history and reset current msg.
history :: Action ()
history = do
  msg <- currentMsg
  msgClear
  config <- gets sconfig
  let historyMax = Config.get config "ui" "historyMax"
      -- TODO: not ideal, continuations of sentences are atop beginnings.
      splitS = splitMsg (fst normalLevelBound + 1) (msg ++ " ")
  unless (L.null msg) $
    modify (updateHistory (take historyMax . (L.reverse splitS ++)))

-- | Perform look around in the current location of the cursor.
-- TODO: depending on tgt, show extra info about tile or monster or both
doLook :: Action ()
doLook = do
  cops   <- contentOps
  loc    <- gets (clocation . scursor)
  state  <- get
  lvl    <- gets slevel
  per    <- currentPerception
  target <- gets (btarget . getPlayerBody)
  pl        <- gets splayer
  let canSee = IS.member loc (totalVisible per)
      monsterMsg =
        if canSee
        then case L.find (\ m -> bloc m == loc) (levelMonsterList state) of
               Just m  -> subjectActor (Kind.coactor cops) m ++ " is here. "
               Nothing -> ""
        else ""
      vis = if not $ loc `IS.member` totalVisible per
            then " (not visible)"  -- by party
            else if actorReachesLoc pl loc per (Just pl)
                 then ""
                 else " (not reachable)"  -- by hero
      mode = case target of
               TEnemy _ _ -> "[targeting monster" ++ vis ++ "] "
               TLoc _     -> "[targeting location" ++ vis ++ "] "
               TCursor    -> "[targeting current" ++ vis ++ "] "
      -- general info about current loc
      lookMsg = mode ++ lookAt cops True canSee state lvl loc monsterMsg
      -- check if there's something lying around at current loc
      is = lvl `rememberAtI` loc
  if length is <= 2
    then msgAdd lookMsg
    else do
      displayItems lookMsg False is
      session getConfirm
      msgAdd ""
