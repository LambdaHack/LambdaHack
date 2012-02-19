-- | The effectToAction function and all it depends on.
-- This file should not depend on Actions.hs nor ItemAction.hs.
-- TODO: Add an export list and document after it's rewritten according to #17.
module Game.LambdaHack.EffectAction where

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Control.Monad
import Control.Monad.State hiding (State, state)
import Data.Function
import Data.Version
import Data.Maybe
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import System.Time

import Game.LambdaHack.Misc
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Display
import Game.LambdaHack.Grammar
import Game.LambdaHack.Point
import qualified Game.LambdaHack.HighScore as H
import Game.LambdaHack.Item
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Random
import Game.LambdaHack.State
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Effect as Effect
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Save as Save

-- TODO: instead of verbosity return msg components and tailor them outside?
-- TODO: separately define messages for the case when source == target
-- and for the other case; then use the messages outside of effectToAction,
-- depending on the returned bool, perception and identity of the actors.

-- | The source actor affects the target actor, with a given effect and power.
-- The second argument is verbosity of the resulting message.
-- Both actors are on the current level and can be the same actor.
-- The bool result indicates if the actors identify the effect.
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
      updateAnyActor target (addHp coactor power)  -- TODO: duplicate code in  bhpMax and addHp
      return (True, actorVerbExtra coactor tm "feel" "better")
effectToAction (Effect.Wound nDm) verbosity source target power = do
  coactor <- contentf Kind.coactor
  pl <- gets splayer
  n  <- rndToAction $ rollDice nDm
  if n + power <= 0 then nullEffect else do
    focusIfAHero target
    tm <- gets (getActor target)
    let newHP  = bhp tm - n - power
        killed = newHP <= 0
        msg
          | killed =
            if target == pl
            then ""  -- handled later on in checkPartyDeath
            else actorVerb coactor tm "die"
          | source == target =  -- a potion of wounding, etc.
            actorVerbExtra coactor tm "feel" "wounded"
          | verbosity <= 0 = ""
          | target == pl =
            actorVerbExtra coactor tm "lose" $
              show (n + power) ++ "HP"
          | otherwise = actorVerbExtra coactor tm "hiss" "in pain"
    updateAnyActor target $ \ m -> m { bhp = newHP }  -- Damage the target.
    when killed $ do
      -- Place the actor's possessions on the map.
      bitems <- gets (getActorItem target)
      modify (updateLevel (dropItemsAt bitems (bloc tm)))
      -- Clean bodies up.
      if target == pl
        then checkPartyDeath  -- kills the player and checks game over
        else modify (deleteActor target)  -- kills the enemy
    return (True, msg)
effectToAction Effect.Dominate _ source target _power = do
  s <- get
  if not $ isAHero s target
    then do  -- Monsters have weaker will than heroes.
      selectPlayer target
        >>= assert `trueM` (source, target, "player dominates himself")
      -- Prevent AI from getting a few free moves until new player ready.
      updatePlayerBody (\ m -> m { btime = 0})
      displayAll
      return (True, "")
    else if source == target
         then do
           lm <- gets levelHeroList
           lxsize <- gets (lxsize . slevel)
           lysize <- gets (lysize . slevel)
           let cross m = bloc m : vicinityCardinal lxsize lysize (bloc m)
               vis = L.concatMap cross lm
           rememberList vis
           return (True, "A dozen voices yells in anger.")
         else nullEffect
effectToAction Effect.SummonFriend _ source target power = do
  tm <- gets (getActor target)
  s <- get
  if isAHero s source
    then summonHeroes (1 + power) (bloc tm)
    else summonMonsters (1 + power) (bloc tm)
  return (True, "")
effectToAction Effect.SummonEnemy _ source target power = do
  tm <- gets (getActor target)
  s  <- get
  if not $ isAHero s source  -- a trick: monster player summons a hero
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
effectToAction Effect.Ascend _ source target power = do
  tm <- gets (getActor target)
  s  <- get
  coactor <- contentf Kind.coactor
  if not $ isAHero s target
    then squashActor source target
    else effLvlGoUp (power + 1)
  -- TODO: The following message too late if a monster squashed:
  return (True, actorVerbExtra coactor tm "find" "a shortcut upstrairs")
effectToAction Effect.Descend _ source target power = do
  tm <- gets (getActor target)
  s  <- get
  coactor <- contentf Kind.coactor
  if not $ isAHero s target
    then squashActor source target
    else effLvlGoUp (- (power + 1))
  -- TODO: The following message too late if a monster squashed:
  return (True, actorVerbExtra coactor tm "find" "a shortcut downstairs")

nullEffect :: Action (Bool, String)
nullEffect = return (False, "Nothing happens.")

-- TODO: refactor with actorAttackActor.
squashActor :: ActorId -> ActorId -> Action ()
squashActor source target = do
  Kind.COps{coactor, coitem=Kind.Ops{okind, ouniqGroup}} <- contentOps
  sm <- gets (getActor source)
  tm <- gets (getActor target)
  let h2hKind = ouniqGroup "weight"
      power = maxDeep $ ipower $ okind h2hKind
      h2h = Item h2hKind power Nothing 1
      verb = iverbApply $ okind h2hKind
      msg = actorVerbActorExtra coactor sm verb tm " in a staircase accident"
  msgAdd msg
  itemEffectAction 0 source target h2h
    >>= assert `trueM` (source, target, "affected")

effLvlGoUp :: Int -> Action ()
effLvlGoUp k = do
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
        -- Remember the level (e.g., for a teleport via scroll on the floor).
        remember
        -- Remove the player from the old level.
        modify (deleteActor pl)
        hs <- gets levelHeroList
        -- Monsters hear that players not on the level. Cancel smell.
        -- Reduces memory load and savefile size.
        when (L.null hs) $
          modify (updateLevel (updateSmell (const IM.empty)))
        -- At this place the invariant that the player exists fails.
        -- Change to the new level (invariant not needed).
        modify (\ s -> s {slid = nln})
        -- Add the player to the new level.
        modify (insertActor pl pbody)
        modify (updateAnyActorItem pl (const bitems))
        -- At this place the invariant is restored again.
        inhabitants <- gets (locToActor nloc)
        case inhabitants of
          Nothing -> return ()
          Just h | isAHero st h ->
            -- Bail out if a party member blocks the staircase.
            abort
          Just m ->
            -- Somewhat of a workaround: squash monster blocking the staircase.
            squashActor pl m
        -- Verify the monster on the staircase died.
        inhabitants2 <- gets (locToActor nloc)
        when (isJust inhabitants2) $ assert `failure` inhabitants2
        -- Land the player at the other end of the stairs.
        updatePlayerBody (\ p -> p { bloc = nloc })
        -- Change the level of the player recorded in cursor.
        modify (updateCursor (\ c -> c { creturnLn = nln }))
        -- The invariant "at most one actor on a tile" restored.
        -- Create a backup of the savegame.
        state <- get
        diary <- currentDiary
        liftIO $ Save.saveGameBkp state diary
        when (targeting /= TgtOff) doLook  -- TODO: lags behind perception

-- | The player leaves the dungeon.
fleeDungeon :: Action ()
fleeDungeon = do
  coitem <- contentf Kind.coitem
  state <- get
  let (items, total) = calculateTotal coitem state
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
-- If the event is seen, the item may get identified.
itemEffectAction :: Int -> ActorId -> ActorId -> Item -> Action Bool
itemEffectAction verbosity source target item = do
  Kind.Ops{okind} <- contentf Kind.coitem
  sm  <- gets (getActor source)
  tm  <- gets (getActor target)
  per <- currentPerception
  pl  <- gets splayer
  s   <- get
  let effect = ieffect $ okind $ jkind item
  -- The msg describes the target part of the action.
  (b, msg) <- effectToAction effect verbosity source target (jpower item)
  if isAHero s source ||
     isAHero s target ||
     pl == source ||
     pl == target ||
     (bloc tm `IS.member` totalVisible per &&
      bloc sm `IS.member` totalVisible per)
    then do
      -- Party sees or affected, so reported.
      msgAdd msg
      -- Party sees or affected, so if interesting, the item gets identified.
      when b $ discover item
    else
      -- Hidden, but if interesting then heard.
      when b $ msgAdd "You hear some noises."
  return b

-- | Make the item known to the player.
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
  coactor <- contentf Kind.coactor
  pl <- gets splayer
  targeting <- gets (ctargeting . scursor)
  if actor == pl
    then return False -- already selected
    else do
      state <- get
      let (nln, pbody, _) = findActorAnyLevel actor state
      -- Make the new actor the player-controlled actor.
      modify (\ s -> s { splayer = actor })
      -- Record the original level of the new player.
      modify (updateCursor (\ c -> c { creturnLn = nln }))
      -- Don't continue an old run, if any.
      stopRunning
      -- Switch to the level.
      modify (\ s -> s{slid = nln})
      -- Announce.
      msgAdd $ capActor coactor pbody ++ " selected."
      when (targeting /= TgtOff) doLook
      return True

focusIfAHero :: ActorId -> Action ()
focusIfAHero target = do
  s <- get
  when (isAHero s target) $ do
    -- Focus on the hero being wounded/displaced/etc.
    b <- selectPlayer target
    -- Display status line for the new hero.
    when b $ void displayAll

summonHeroes :: Int -> Point -> Action ()
summonHeroes n loc =
  assert (n > 0) $ do
  cops <- contentOps
  newHeroId <- gets scounter
  modify (\ state -> iterate (addHero cops loc) state !! n)
  selectPlayer (AHero newHeroId)
    >>= assert `trueM` (newHeroId, "player summons himself")
  -- Display status line for the new hero.
  void displayAll

summonMonsters :: Int -> Point -> Action ()
summonMonsters n loc = do
  Kind.COps{cotile, coactor=Kind.Ops{opick, okind}} <- contentOps
  mk <- rndToAction $ opick "summon" (const True)
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  modify (\ state ->
           iterate (addMonster cotile mk hp loc) state !! n)

-- | Update player memory.
remember :: Action ()
remember = do
  per <- currentPerception
  let vis = IS.toList (totalVisible per)
  rememberList vis

rememberList :: [Point] -> Action ()
rememberList vis = do
  lvl <- gets slevel
  let rememberTile = [(loc, lvl `at` loc) | loc <- vis]
  modify (updateLevel (updateLRMap (Kind.// rememberTile)))
  let alt Nothing      = Nothing
      alt (Just ([], _)) = Nothing
      alt (Just (t, _))  = Just (t, t)
      rememberItem = IM.alter alt
  modify (updateLevel (updateIMap (\ m -> L.foldr rememberItem m vis)))

-- | Remove dead heroes (or dead dominated monsters). Check if game is over.
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
  when (bhp pbody <= 0) $ do  -- TODO: change to guard? define mzero as abort? Why are the writes to the files performed when I call abort later? That probably breaks the laws of MonadPlus. Or is the tryWith abort handler placed after the write to files?
    go <- msgMoreConfirm ColorBW $ actorVerb cops pbody "die"
    history  -- Prevent the msgs from being repeated.
    let firstDeathEnds = Config.get config "heroes" "firstDeathEnds"
    if firstDeathEnds
      then gameOver go
      else case L.filter ((/= pl) . AHero) ahs of
             [] -> gameOver go
             actor : _ -> do
               msgAdd "The survivors carry on."
               -- One last look at the beautiful world.
               remember
               -- Remove the dead player.
               modify deletePlayer
               -- At this place the invariant that the player exists fails.
               -- Focus on the new hero (invariant not needed).
               selectPlayer (AHero actor)
                 >>= assert `trueM` (pl, actor, "player resurrects")
               -- At this place the invariant is restored again.

-- | End game, showing the ending screens, if requested.
gameOver :: Bool -> Action ()
gameOver showEndingScreens = do
  when showEndingScreens $ do
    cops  <- contentf Kind.coitem
    state <- get
    slid  <- gets slid
    let (_, total) = calculateTotal cops state
        status = H.Killed slid
    handleScores True status total
    msgMore "Let's hope another party can save the day!"
  end

-- | Handle current score and display it with the high scores.
-- False if display of the scores was void or interrupted by the user.
--
-- Warning: scores are shown during the game,
-- so we should be careful not to leak secret information through them
-- (e.g., the nature of the items through the total worth of inventory).
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
    session getConfirm

-- effectToAction does not depend on this function right now, but it might,
-- and I know no better place to put it.
displayItems :: Msg -> Bool -> [Item] -> Action Bool
displayItems msg sorted is = do
  cops  <- contentf Kind.coitem
  state <- get
  let inv = unlines $
            L.map (\ i -> letterLabel (jletter i)
                          ++ objectItem cops state i ++ " ")
              ((if sorted
                then L.sortBy (cmpLetterMaybe `on` jletter)
                else id) is)
  let ovl = inv ++ msgEnd
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
      splitS = splitMsg (fst normalLevelBound + 1) msg 0
      takeMax diary =
        take historyMax $
          L.map (padMsg (fst normalLevelBound + 1)) splitS ++ shistory diary
  unless (L.null msg) $ do
    diary <- currentDiary
    diaryReset $ diary {shistory = takeMax diary}

-- TODO: depending on tgt, show extra info about tile or monster or both
-- | Perform look around in the current location of the cursor.
doLook :: Action ()
doLook = do
  cops@Kind.COps{coactor} <- contentOps
  loc    <- gets (clocation . scursor)
  state  <- get
  lvl    <- gets slevel
  per    <- currentPerception
  target <- gets (btarget . getPlayerBody)
  pl     <- gets splayer
  let canSee = IS.member loc (totalVisible per)
      monsterMsg =
        if canSee
        then case L.find (\ m -> bloc m == loc) (levelMonsterList state) of
               Just m  -> actorVerbExtra coactor m "be" "here" ++ " "
               Nothing -> ""
        else ""
      vis | not $ loc `IS.member` totalVisible per =
              " (not visible)"  -- by party
          | actorReachesLoc pl loc per (Just pl) = ""
          | otherwise = " (not reachable)"  -- by hero
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
      session getConfirm >> msgAdd ""  -- TODO: a hack; instead keep current overlay in the state to keep it from being overwritten on the screen in Turn.hs, just as msg is kept, and reset each turn

gameVersion :: Action ()
gameVersion = do
  Kind.COps{corule} <- contentOps
  let pathsVersion = rpathsVersion $ stdRuleset corule
      msg = "Version " ++ showVersion pathsVersion
            ++ " (frontend: " ++ frontendName
            ++ ", engine: LambdaHack " ++ showVersion Self.version ++ ")"
  abortWith msg
