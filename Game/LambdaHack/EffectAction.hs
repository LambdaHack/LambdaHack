-- | The effectToAction function and all it depends on.
-- This file should not depend on Actions.hs nor ItemAction.hs.
-- TODO: Add an export list and document after it's rewritten according to #17.
module Game.LambdaHack.EffectAction where

import Control.Monad
import Control.Monad.State hiding (State, state)
import Data.Function
import Data.Maybe
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Monoid

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Draw
import Game.LambdaHack.Grammar
import Game.LambdaHack.Point
import Game.LambdaHack.Item
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Level
import Game.LambdaHack.Misc
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.Time
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Effect as Effect
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Animation (twirlSplash, blockHit, deathBody)
import qualified Game.LambdaHack.Dungeon as Dungeon

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: Rnd a -> Action a
rndToAction r = do
  g <- gets srandom
  let (a, ng) = runState r g
  modify (\ state -> state {srandom = ng})
  return a

-- | Update actor stats. Works for actors on other levels, too.
updateAnyActor :: ActorId -> (Actor -> Actor) -> Action ()
updateAnyActor actor f = modify (updateAnyActorBody actor f)

-- | Update player-controlled actor stats.
updatePlayerBody :: (Actor -> Actor) -> Action ()
updatePlayerBody f = do
  pl <- gets splayer
  updateAnyActor pl f

-- TODO: instead of verbosity return msg components and tailor them outside?
-- TODO: separately define messages for the case when source == target
-- and for the other case; then use the messages outside of effectToAction,
-- depending on the returned bool, perception and identity of the actors.

-- | The source actor affects the target actor, with a given effect and power.
-- The second argument is verbosity of the resulting message.
-- Both actors are on the current level and can be the same actor.
-- The first bool result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
-- The second bool tells if the effect was seen by or affected the party.
effectToAction :: Effect.Effect -> Int -> ActorId -> ActorId -> Int -> Bool
               -> Action (Bool, Bool)
effectToAction effect verbosity source target power block = do
  oldTm <- gets (getActor target)
  let oldHP = bhp oldTm
  (b, msg) <- eff effect verbosity source target power
  s <- get
  -- If the target killed outright by the effect (e.g., in a recursive call),
  -- there's nothing left to do. TODO: hacky; aren't messages lost?
  if not (memActor target s)
   then return (b, False)
   else do
    sm  <- gets (getActor source)
    tm  <- gets (getActor target)
    per <- getPerception
    pl  <- gets splayer
    let sloc = bloc sm
        tloc = bloc tm
        svisible = sloc `IS.member` totalVisible per
        tvisible = tloc `IS.member` totalVisible per
        newHP = bhp $ getActor target s
    bb <-
     if isAHero s source ||
        isAHero s target ||
        pl == source ||
        pl == target ||
        -- Target part of message shown below, so target visibility checked.
        tvisible
     then do
      -- Party sees the effect or is affected by it.
      msgAdd msg
      -- Try to show an animation. Sometimes, e.g., when HP is unchaged,
      -- the animation will not be shown.
      cops <- getCOps
      diary <- getDiary
      let locs = (breturn tvisible tloc,
                  breturn svisible sloc)
          anim | newHP > oldHP =
            twirlSplash locs Color.BrBlue Color.Blue
               | newHP < oldHP && block =
            blockHit    locs Color.BrRed  Color.Red
               | newHP < oldHP && not block =
            twirlSplash locs Color.BrRed  Color.Red
               | otherwise = mempty
          animFrs = animate s diary cops per anim
      mapM_ displayFramePush $ Nothing : animFrs
      return (b, True)
     else do
      -- Hidden, but if interesting then heard.
      when b $ msgAdd "You hear some noises."
      return (b, False)
    -- Now kill the actor, if needed. For monsters, no "die" message
    -- is shown below. It should have been shown in @eff@.
    when (newHP <= 0) $ do
      -- Place the actor's possessions on the map.
      bitems <- gets (getActorItem target)
      modify (updateLevel (dropItemsAt bitems tloc))
      -- Clean bodies up.
      if target == pl
        then  -- Kill the player and check game over.
          checkPartyDeath
        else  -- Kill the enemy.
          modify (deleteActor target)
    return bb

-- | The boolean part of the result says if the ation was interesting
-- and the string part describes how the target reacted
-- (not what the source did).
eff :: Effect.Effect -> Int -> ActorId -> ActorId -> Int
    -> Action (Bool, String)
eff Effect.NoEffect _ _ _ _ = nullEffect
eff Effect.Heal _ _source target power = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getCOps
  let bhpMax m = maxDice (ahp $ okind $ bkind m)
  tm <- gets (getActor target)
  if bhp tm >= bhpMax tm || power <= 0
    then nullEffect
    else do
      void $ focusIfOurs target
      updateAnyActor target (addHp coactor power)
      return (True, actorVerb coactor tm "feel" "better")
eff (Effect.Wound nDm) verbosity source target power = do
  Kind.COps{coactor} <- getCOps
  s <- get
  n <- rndToAction $ rollDice nDm
  if n + power <= 0 then nullEffect else do
    void $ focusIfOurs target
    pl <- gets splayer
    tm <- gets (getActor target)
    let newHP = bhp tm - n - power
        msg
          | newHP <= 0 =
            if target == pl
            then ""  -- Handled later on in checkPartyDeath. Suspense.
            else -- Not as important, so let the player read the message
                 -- about monster death while he watches the combat animation.
              if isProjectile s target
              then actorVerb coactor tm "drop" "down"
              else actorVerb coactor tm "die" ""
          | source == target =  -- a potion of wounding, etc.
            actorVerb coactor tm "feel" "wounded"
          | verbosity <= 0 = ""
          | target == pl =
            actorVerb coactor tm "lose" $
              show (n + power) ++ "HP"
          | otherwise = actorVerb coactor tm "hiss" "in pain"
    updateAnyActor target $ \ m -> m { bhp = newHP }  -- Damage the target.
    return (True, msg)
eff Effect.Dominate _ source target _power = do
  s <- get
  if not $ isAHero s target
    then do  -- Monsters have weaker will than heroes.
      selectPlayer target
        >>= assert `trueM` (source, target, "player dominates himself")
      -- Sync the monster with the hero move time for better display
      -- of missiles and for the domination to actually take one player's turn.
      updatePlayerBody (\ m -> m { btime = stime s})
      -- Display status line and FOV for the newly controlled actor.
      fr <- drawPrompt ColorBW ""
      mapM_ displayFramePush [Nothing, Just fr, Nothing]
      return (True, "")
    else if source == target
         then do
           lm <- gets hostileList
           lxsize <- gets (lxsize . slevel)
           lysize <- gets (lysize . slevel)
           let cross m = bloc m : vicinityCardinal lxsize lysize (bloc m)
               vis = L.concatMap cross lm
           rememberList vis
           return (True, "A dozen voices yells in anger.")
         else nullEffect
eff Effect.SummonFriend _ source target power = do
  tm <- gets (getActor target)
  s <- get
  if isAHero s source
    then summonHeroes (1 + power) (bloc tm)
    else summonMonsters (1 + power) (bloc tm)
  return (True, "")
eff Effect.SummonEnemy _ source target power = do
  tm <- gets (getActor target)
  s  <- get
  -- A trick: monster player summons a hero.
  if isAHero s source
    then summonMonsters (1 + power) (bloc tm)
    else summonHeroes (1 + power) (bloc tm)
  return (True, "")
eff Effect.ApplyPerfume _ source target _ =
  if source == target
  then return (True, "Tastes like water, but with a strong rose scent.")
  else do
    let upd lvl = lvl { lsmell = IM.empty }
    modify (updateLevel upd)
    return (True, "The fragrance quells all scents in the vicinity.")
eff Effect.Regeneration verbosity source target power =
  eff Effect.Heal verbosity source target power
eff Effect.Searching _ _source _target _power =
  return (True, "It gets lost and you search in vain.")
eff Effect.Ascend _ source target power = do
  tm <- gets (getActor target)
  s  <- get
  Kind.COps{coactor} <- getCOps
  void $ focusIfOurs target
  if not $ isAHero s target  -- not target /= pl: to squash friendly monster
    then squashActor source target
    else effLvlGoUp (power + 1)
  -- TODO: The following message too late if a monster squashed by going up,
  -- unless it's ironic. ;) The same below.
  s2 <- get
  return $ if maybe Camping snd (squit s2) == Victor
    then (True, "")
    else (True, actorVerb coactor tm "find" "a way upstairs")
eff Effect.Descend _ source target power = do
  tm <- gets (getActor target)
  s  <- get
  Kind.COps{coactor} <- getCOps
  void $ focusIfOurs target
  if not $ isAHero s target
    then squashActor source target
    else effLvlGoUp (- (power + 1))
  s2 <- get
  return $ if maybe Camping snd (squit s2) == Victor
    then (True, "")
    else (True, actorVerb coactor tm "find" "a way downstairs")

nullEffect :: Action (Bool, String)
nullEffect = return (False, "Nothing happens.")

-- TODO: refactor with actorAttackActor.
squashActor :: ActorId -> ActorId -> Action ()
squashActor source target = do
  Kind.COps{coactor, coitem=Kind.Ops{okind, ouniqGroup}} <- getCOps
  sm <- gets (getActor source)
  tm <- gets (getActor target)
  let h2hKind = ouniqGroup "weight"
      power = maxDeep $ ipower $ okind h2hKind
      h2h = Item h2hKind power Nothing 1
      verb = iverbApply $ okind h2hKind
      msg = actorVerbActor coactor sm verb tm "in a staircase accident"
  msgAdd msg
  itemEffectAction 0 source target h2h False
  s <- get
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (memActor target s) `blame` (source, target, "not killed")) $
    return ()

effLvlGoUp :: Int -> Action ()
effLvlGoUp k = do
  pbody     <- gets getPlayerBody
  pl        <- gets splayer
  slid      <- gets slid
  st        <- get
  cops      <- getCOps
  lvl <- gets slevel
  case whereTo st k of
    Nothing -> fleeDungeon -- we are at the "end" of the dungeon
    Just (nln, nloc) ->
      assert (nln /= slid `blame` (nln, "stairs looped")) $ do
        bitems <- gets getPlayerItem
        -- Remember the level (e.g., for a teleport via scroll on the floor).
        remember
        -- Remove the player from the old level.
        modify (deleteActor pl)
        hs <- gets heroList
        -- Monsters hear that players not on the level. Cancel smell.
        -- Reduces memory load and savefile size.
        when (L.null hs) $
          modify (updateLevel (updateSmell (const IM.empty)))
        -- At this place the invariant that the player exists fails.
        -- Change to the new level (invariant not needed).
        switchLevel nln
        -- The player can now be safely added to the new level.
        modify (insertActor pl pbody)
        modify (updateAnyActorItem pl (const bitems))
        -- At this place the invariant is restored again.
        inhabitants <- gets (locToActor nloc)
        case inhabitants of
          Nothing -> return ()
-- Broken if the effect happens, e.g. via a scroll and abort is not enough.
--          Just h | isAHero st h ->
--            -- Bail out if a party member blocks the staircase.
--            abortWith "somebody blocks the staircase"
          Just m ->
            -- Aquash an actor blocking the staircase.
            -- This is not a duplication with the other calls to squashActor,
            -- because here an inactive actor is squashed.
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
        saveGameBkp
        state <- get
        msgAdd $ lookAt cops False True state lvl nloc ""

-- | Change level and reset it's time and update the times of all actors.
-- The player may be added to @lactor@ of the new level only after
-- this operation is executed.
switchLevel :: Dungeon.LevelId -> Action ()
switchLevel nln = do
  timeCurrent <- gets stime
  slid <- gets slid
  when (slid /= nln) $ do
    -- Switch to the new level.
    modify (\ s -> s {slid = nln})
    timeLastVisited <- gets stime
    let diff = timeAdd timeCurrent $ timeNegate timeLastVisited
    when (diff /= timeZero) $ do
      -- Reset the level time.
      modify $ updateTime $ const timeCurrent
      -- Update the times of all actors.
      let upd m@Actor{btime} = m {btime = timeAdd btime diff}
      modify (updateLevel (updateActorDict (IM.map upd)))

-- | The player leaves the dungeon.
fleeDungeon :: Action ()
fleeDungeon = do
  Kind.COps{coitem} <- getCOps
  s <- get
  go <- displayYesNo "This is the way out. Really leave now?"
  recordHistory  -- Prevent repeating the ending msgs.
  when (not go) $ abortWith "Game resumed."
  let (items, total) = calculateTotal coitem s
  modify (\ st -> st {squit = Just (False, Victor)})
  if total == 0
  then do
    -- The player can back off at each of these steps.
    go1 <- displayMore ColorBW
             "Afraid of the challenge? Leaving so soon and empty-handed?"
    when (not go1) $ abortWith "Brave soul!"
    go2 <- displayMore ColorBW
            "This time try to grab some loot before escape!"
    when (not go2) $ abortWith "Here's your chance!"
  else do
    let winMsg = "Congratulations, you won! Here's your loot, worth " ++
                 show total ++ " gold."  -- TODO: use the name of the '$' item instead
    io <- itemOverlay True True items
    tryIgnore $ displayOverAbort winMsg io
    modify (\ st -> st {squit = Just (True, Victor)})

-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified.
itemEffectAction :: Int -> ActorId -> ActorId -> Item -> Bool -> Action ()
itemEffectAction verbosity source target item block = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getCOps
  st <- get
  slidOld <- gets slid
  let effect = ieffect $ okind $ jkind item
  -- The msg describes the target part of the action.
  (b1, b2) <- effectToAction effect verbosity source target (jpower item) block
  -- Party sees or affected, and the effect interesting,
  -- so the item gets identified.
  when (b1 && b2) $ discover item
  -- Destroys attacking actor and its items: a hack for projectiles.
  slidNew <- gets slid
  modify (\ s -> s {slid = slidOld})
  when (isProjectile st source) $
    modify (deleteActor source)
  modify (\ s -> s {slid = slidNew})

-- | Make the item known to the player.
discover :: Item -> Action ()
discover i = do
  Kind.COps{coitem=coitem@Kind.Ops{okind}} <- getCOps
  state <- get
  let ik = jkind i
      obj = unwords $ tail $ words $ objectItem coitem state i
      msg = "The " ++ obj ++ " turns out to be "
      kind = okind ik
      alreadyIdentified = L.length (iflavour kind) == 1
                          || ik `S.member` sdisco state
  unless alreadyIdentified $ do
    modify (updateDiscoveries (S.insert ik))
    state2 <- get
    msgAdd $ msg ++ objectItem coitem state2 i ++ "."

-- | Make the actor controlled by the player. Switch level, if needed.
-- False, if nothing to do. Should only be invoked as a direct result
-- of a player action or the selected player actor death.
selectPlayer :: ActorId -> Action Bool
selectPlayer actor = do
  Kind.COps{coactor} <- getCOps
  pl    <- gets splayer
  cops  <- getCOps
  lvl   <- gets slevel
  if actor == pl
    then return False -- already selected
    else do
      state <- get
      let (nln, pbody, _) = findActorAnyLevel actor state
      -- Switch to the new level.
      switchLevel nln
      -- Make the new actor the player-controlled actor.
      modify (\ s -> s {splayer = actor})
      -- Record the original level of the new player.
      modify (updateCursor (\ c -> c {creturnLn = nln}))
      -- Don't continue an old run, if any.
      stopRunning
      -- Announce.
      msgAdd $ capActor coactor pbody ++ " selected."
      msgAdd $ lookAt cops False True state lvl (bloc pbody) ""
      return True

-- TODO: center screen, flash the background, etc. Perhaps wait for SPACE.
-- | Focus on the hero being wounded/displaced/etc.
focusIfOurs :: ActorId -> Action Bool
focusIfOurs target = do
  s  <- get
  pl <- gets splayer
  if isAHero s target || target == pl
    then return True
    else return False

summonHeroes :: Int -> Point -> Action ()
summonHeroes n loc =
  assert (n > 0) $ do
  cops <- getCOps
  newHeroId <- gets scounter
  modify (\ state -> iterate (addHero cops loc) state !! n)
  b <- focusIfOurs newHeroId
  assert (b `blame` (newHeroId, "player summons himself")) $
    return ()

summonMonsters :: Int -> Point -> Action ()
summonMonsters n loc = do
  Kind.COps{ cotile
           , coactor=Kind.Ops{opick, okind}
           , cofact=Kind.Ops{opick=fopick, oname=foname}} <- getCOps
  bfaction <- rndToAction $ fopick "spawn" (const True)
  -- Spawn frequency required greater than zero, but otherwise ignored.
  let inFaction m = isJust $ lookup (foname bfaction) (afreq m)
  -- Summon frequency used for picking the actor.
  mk <- rndToAction $ opick "summon" inFaction
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  modify (\ s -> iterate (addMonster cotile mk hp loc
                            bfaction False) s !! n)

-- | Remove dead heroes (or dead dominated monsters). Check if game is over.
-- For now we only check the selected hero and at current level,
-- but if poison, etc. is implemented, we'd need to check all heroes
-- on any level.
checkPartyDeath :: Action ()
checkPartyDeath = do
  cops@Kind.COps{coactor} <- getCOps
  per    <- getPerception
  ahs    <- gets allHeroesAnyLevel
  pl     <- gets splayer
  pbody  <- gets getPlayerBody
  config <- gets sconfig
  when (bhp pbody <= 0) $ do
    msgAdd $ actorVerb coactor pbody "die" ""
    go <- displayMore ColorBW ""
    recordHistory  -- Prevent repeating the "die" msgs.
    let firstDeathEnds = Config.get config "heroes" "firstDeathEnds"
        bodyToCorpse = updateAnyActor pl $ \ body -> body {bsymbol = Just '%'}
        animateDeath = do
          diary  <- getDiary
          s <- get
          let animFrs = animate s diary cops per $ deathBody (bloc pbody)
          mapM_ displayFramePush $ animFrs
        animateGameOver = do
          animateDeath
          bodyToCorpse
          gameOver go
    if firstDeathEnds
      then animateGameOver
      else case L.filter (/= pl) ahs of
             [] -> animateGameOver
             actor : _ -> do
               msgAdd "The survivors carry on."
               animateDeath
               -- One last look at the beautiful world.
               remember
               -- Remove the dead player.
               modify deletePlayer
               -- At this place the invariant that the player exists fails.
               -- Select the new player-controlled hero (invariant not needed),
               -- but don't draw a frame for him with focusIfOurs,
               -- in case the focus changes again during the same turn.
               -- He's just a random next guy in the line.
               selectPlayer actor
                 >>= assert `trueM` (pl, actor, "player resurrects")
               -- At this place the invariant is restored again.

-- | End game, showing the ending screens, if requested.
gameOver :: Bool -> Action ()
gameOver showEndingScreens = do
  slid <- gets slid
  modify (\ st -> st {squit = Just (False, Killed slid)})
  when showEndingScreens $ do
    Kind.COps{coitem} <- getCOps
    s <- get
    dng <- gets sdungeon
    time <- gets stime
    let (items, total) = calculateTotal coitem s
        deepest = Dungeon.levelNumber slid  -- use deepest visited instead of level of death
        depth = Dungeon.depth dng
        failMsg | timeFit time timeTurn < 300 =
          "That song shall be short."
                | total < 100 =
          "Born poor, dies poor."
                | deepest < 4 && total < 500 =
          "This should end differently."
                | deepest < depth - 1 =
          "This defeat brings no dishonour."
                | deepest < depth =
          "That is your name. 'Almost'."
                | otherwise =
          "Dead heroes make better legends."
        loseMsg = failMsg ++ " You left " ++
                  show total ++ " gold and some junk."  -- TODO: use the name of the '$' item instead
    if null items
      then modify (\ st -> st {squit = Just (True, Killed slid)})
      else do
        io <- itemOverlay True True items
        tryIgnore $ do
          displayOverAbort loseMsg io
          modify (\ st -> st {squit = Just (True, Killed slid)})

-- | Create a list of item names, split into many overlays.
itemOverlay ::Bool -> Bool -> [Item] -> Action [Overlay]
itemOverlay sorted cheat is = do
  Kind.COps{coitem} <- getCOps
  state <- get
  lysize <- gets (lysize . slevel)
  let inv = L.map (\ i -> letterLabel (jletter i)
                          ++ objectItemCheat coitem cheat state i ++ " ")
              ((if sorted
                then L.sortBy (cmpLetterMaybe `on` jletter)
                else id) is)
  return $ splitOverlay lysize inv

stopRunning :: Action ()
stopRunning = updatePlayerBody (\ p -> p { bdir = Nothing })

-- | Perform look around in the current location of the cursor.
doLook :: ActionFrame ()
doLook = do
  cops@Kind.COps{coactor} <- getCOps
  loc    <- gets (clocation . scursor)
  state  <- get
  lvl    <- gets slevel
  hms    <- gets (lactor . slevel)
  per    <- getPerception
  target <- gets (btarget . getPlayerBody)
  pl     <- gets splayer
  targeting <- gets (ctargeting . scursor)
  assert (targeting /= TgtOff) $ do
    let canSee = IS.member loc (totalVisible per)
        ihabitant | canSee = L.find (\ m -> bloc m == loc) (IM.elems hms)
                  | otherwise = Nothing
        monsterMsg =
          maybe "" (\ m -> actorVerb coactor m "be" "here" ++ " ") ihabitant
        vis | not $ loc `IS.member` totalVisible per =
                " (not visible)"  -- by party
            | actorReachesLoc pl loc per (Just pl) = ""
            | otherwise = " (not reachable)"  -- by hero
        mode = case target of
                 TEnemy _ _ -> "[targeting monster" ++ vis ++ "] "
                 TLoc _     -> "[targeting location" ++ vis ++ "] "
                 TPath _    -> "[targeting path" ++ vis ++ "] "
                 TCursor    -> "[targeting current" ++ vis ++ "] "
        -- Show general info about current loc.
        lookMsg = mode ++ lookAt cops True canSee state lvl loc monsterMsg
        -- Check if there's something lying around at current loc.
        is = lvl `rememberAtI` loc
    io <- itemOverlay False False is
    if length is > 2
      then displayOverlays lookMsg "" io
      else do
        fr <- drawPrompt ColorFull lookMsg
        returnFrame fr
