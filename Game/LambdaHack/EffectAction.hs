{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | The effectToAction function and all it depends on.
-- This file should not depend on Actions.hs nor ItemAction.hs.
-- TODO: Add an export list and document after it's rewritten according to #17.
module Game.LambdaHack.EffectAction where

import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Writer.Strict (WriterT, tell)
import Data.Function
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (mempty)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation (twirlSplash, blockHit, deathBody)
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Config
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Draw
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Misc
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

default (Text)

-- | Sentences such as \"Dog barks loudly.\"
actorVerb :: Kind.Ops ActorKind -> Actor -> Text -> Text
actorVerb coactor a v =
  makeSentence [MU.SubjectVerbSg (partActor coactor a) (MU.Text v)]

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadAction m => Rnd a -> m a
rndToAction r = do
  g <- getsServer srandom
  let (a, ng) = St.runState r g
  modifyServer (\ ser -> ser {srandom = ng})
  return a

-- | Update actor stats. Works for actors on other levels, too.
updateAnyActor :: MonadAction m => ActorId -> (Actor -> Actor) -> m ()
updateAnyActor actor f = modifyGlobal (updateAnyActorBody actor f)

-- | Update player-controlled actor stats.
updatePlayerBody :: MonadAction m => (Actor -> Actor) -> m ()
updatePlayerBody f = do
  pl <- getsGlobal splayer
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
effectToAction :: MonadAction m => Effect.Effect -> Int -> ActorId -> ActorId -> Int -> Bool
               -> m (Bool, Bool)
effectToAction effect verbosity source target power block = do
  oldTm <- getsGlobal (getActor target)
  let oldHP = bhp oldTm
  (b, msg) <- eff effect verbosity source target power
  s <- getGlobal
  -- If the target killed outright by the effect (e.g., in a recursive call),
  -- there's nothing left to do. TODO: hacky; aren't messages lost?
  if not (memActor target s)
   then return (b, False)
   else do
    sm  <- getsGlobal (getActor source)
    tm  <- getsGlobal (getActor target)
    per <- askPerception
    pl  <- getsGlobal splayer
    let sloc = bloc sm
        tloc = bloc tm
        svisible = sloc `IS.member` totalVisible per
        tvisible = tloc `IS.member` totalVisible per
        newHP = bhp $ getActor target s
    bb <-
     if isAHero s source ||
        isAHero s target ||
        -- Target part of message shown below, so target visibility checked.
        tvisible
     then do
      -- Party sees the effect or is affected by it.
      msgAdd msg
      -- Try to show an animation. Sometimes, e.g., when HP is unchaged,
      -- the animation will not be shown.
      cops <- getsGlobal scops
      cli <- getClient
      loc <- getLocal
      let locs = (breturn tvisible tloc,
                  breturn svisible sloc)
          anim | newHP > oldHP =
            twirlSplash locs Color.BrBlue Color.Blue
               | newHP < oldHP && block =
            blockHit    locs Color.BrRed  Color.Red
               | newHP < oldHP && not block =
            twirlSplash locs Color.BrRed  Color.Red
               | otherwise = mempty
          animFrs = animate cli loc cops per anim
      displayFramesPush $ Nothing : animFrs
      return (b, True)
     else do
      -- Hidden, but if interesting then heard.
      when b $ msgAdd "You hear some noises."
      return (b, False)
    -- Now kill the actor, if needed. For monsters, no "die" message
    -- is shown below. It should have been shown in @eff@.
    when (newHP <= 0) $ do
      -- Place the actor's possessions on the map.
      bitems <- getsGlobal (getActorItem target)
      modifyGlobal (updateArena (dropItemsAt bitems tloc))
      -- Clean bodies up.
      if target == pl
        then  -- Kill the player and check game over.
          checkPartyDeath
        else  -- Kill the enemy.
          modifyGlobal (deleteActor target)
    return bb

-- | The boolean part of the result says if the ation was interesting
-- and the string part describes how the target reacted
-- (not what the source did).
eff :: MonadAction m => Effect.Effect -> Int -> ActorId -> ActorId -> Int
    -> m (Bool, Text)
eff Effect.NoEffect _ _ _ _ = nullEffect
eff Effect.Heal _ _source target power = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsGlobal scops
  let bhpMax m = maxDice (ahp $ okind $ bkind m)
  tm <- getsGlobal (getActor target)
  if bhp tm >= bhpMax tm || power <= 0
    then nullEffect
    else do
      void $ focusIfOurs target
      updateAnyActor target (addHp coactor power)
      return (True, actorVerb coactor tm "feel better")
eff (Effect.Wound nDm) verbosity source target power = do
  Kind.COps{coactor} <- getsGlobal scops
  s <- getGlobal
  n <- rndToAction $ rollDice nDm
  if n + power <= 0 then nullEffect else do
    void $ focusIfOurs target
    pl <- getsGlobal splayer
    tm <- getsGlobal (getActor target)
    let newHP = bhp tm - n - power
        msg
          | newHP <= 0 =
            if target == pl
            then ""  -- Handled later on in checkPartyDeath. Suspense.
            else -- Not as important, so let the player read the message
                 -- about monster death while he watches the combat animation.
              if isProjectile s target
              then actorVerb coactor tm "drop down"
              else actorVerb coactor tm "die"
          | source == target =  -- a potion of wounding, etc.
            actorVerb coactor tm "feel wounded"
          | verbosity <= 0 = ""
          | target == pl =
            actorVerb coactor tm $ "lose" <+> showT (n + power) <> "HP"
          | otherwise = actorVerb coactor tm "hiss in pain"
    updateAnyActor target $ \ m -> m { bhp = newHP }  -- Damage the target.
    return (True, msg)
eff Effect.Dominate _ source target _power = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsGlobal scops
  s <- getGlobal
  if not $ isAHero s target
    then do  -- Monsters have weaker will than heroes.
      selectPlayer target
        >>= assert `trueM` (source, target, "player dominates himself")
      -- Halve the speed as a side-effect of domination.
      let halfSpeed :: Actor -> Maybe Speed
          halfSpeed Actor{bkind} =
            let speed = aspeed $ okind bkind
            in Just $ speedScale (1%2) speed
      -- Sync the monster with the hero move time for better display
      -- of missiles and for the domination to actually take one player's turn.
      updatePlayerBody (\ m -> m { bfaction = sside s
                                 , btime = getTime s
                                 , bspeed = halfSpeed m })
      -- Display status line and FOV for the newly controlled actor.
      sli <- promptToSlideshow ""
      fr <- drawOverlay ColorBW $ head $ runSlideshow sli
      displayFramesPush [Nothing, Just fr, Nothing]
      return (True, "")
    else if source == target
         then do
           lm <- getsGlobal hostileList
           lxsize <- getsGlobal (lxsize . getArena)
           lysize <- getsGlobal (lysize . getArena)
           let cross m = bloc m : vicinityCardinal lxsize lysize (bloc m)
               vis = IS.fromList $ concatMap cross lm
           lvl <- getsGlobal getArena
           rememberList vis lvl
           return (True, "A dozen voices yells in anger.")
         else nullEffect
eff Effect.SummonFriend _ source target power = do
  tm <- getsGlobal (getActor target)
  s <- getGlobal
  if isAHero s source
    then summonHeroes (1 + power) (bloc tm)
    else summonMonsters (1 + power) (bloc tm)
  return (True, "")
eff Effect.SummonEnemy _ source target power = do
  tm <- getsGlobal (getActor target)
  s  <- getGlobal
  if isAHero s source
    then summonMonsters (1 + power) (bloc tm)
    else summonHeroes (1 + power) (bloc tm)
  return (True, "")
eff Effect.ApplyPerfume _ source target _ =
  if source == target
  then return (True, "Tastes like water, but with a strong rose scent.")
  else do
    let upd lvl = lvl { lsmell = IM.empty }
    modifyGlobal (updateArena upd)
    return (True, "The fragrance quells all scents in the vicinity.")
eff Effect.Regeneration verbosity source target power =
  eff Effect.Heal verbosity source target power
eff Effect.Searching _ _source _target _power =
  return (True, "It gets lost and you search in vain.")
eff Effect.Ascend _ source target power = do
  Kind.COps{coactor} <- getsGlobal scops
  tm <- getsGlobal (getActor target)
  void $ focusIfOurs target
  -- A faction that spawns cannot switch levels (nor move between levels).
  -- Otherwise it would constantly go to a distant level, spawn actors there
  -- and swarm any opponent arriving on the level.
  glo <- getGlobal
  if isSpawningFaction glo (bfaction tm)
    then squashActor source target
    else effLvlGoUp (power + 1)
  -- TODO: The following message too late if a monster squashed by going up,
  -- unless it's ironic. ;) The same below.
  ser <- getServer
  return $ if maybe Camping snd (squit ser) == Victor
           then (True, "")
           else (True, actorVerb coactor tm "find a way upstairs")
eff Effect.Descend _ source target power = do
  Kind.COps{coactor} <- getsGlobal scops
  tm <- getsGlobal (getActor target)
  void $ focusIfOurs target
  glo <- getGlobal
  if isSpawningFaction glo (bfaction tm)
    then squashActor source target
    else effLvlGoUp (- (power + 1))
  ser <- getServer
  return $ if maybe Camping snd (squit ser) == Victor
           then (True, "")
           else (True, actorVerb coactor tm "find a way downstairs")

nullEffect :: MonadActionRO m => m (Bool, Text)
nullEffect = return (False, "Nothing happens.")

-- TODO: refactor with actorAttackActor.
squashActor :: MonadAction m => ActorId -> ActorId -> m ()
squashActor source target = do
  Kind.COps{coactor, coitem=Kind.Ops{okind, ouniqGroup}} <- getsGlobal scops
  sm <- getsGlobal (getActor source)
  tm <- getsGlobal (getActor target)
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  let h2hKind = ouniqGroup "weight"
      kind = okind h2hKind
      power = maxDeep $ ipower kind
      h2h = buildItem flavour discoRev h2hKind kind 1 power
      verb = iverbApply kind
      msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor sm) verb
        , partActor coactor tm
        , "in a staircase accident" ]
  msgAdd msg
  itemEffectAction 0 source target h2h False
  s <- getGlobal
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (memActor target s) `blame` (source, target, "not killed")) $
    return ()

effLvlGoUp :: MonadAction m => Int -> m ()
effLvlGoUp k = do
  pbody     <- getsGlobal getPlayerBody
  pl        <- getsGlobal splayer
  sarena      <- getsGlobal sarena
  st        <- getGlobal
  cops <- getsGlobal scops
  case whereTo st k of
    Nothing -> fleeDungeon -- we are at the "end" of the dungeon
    Just (nln, nloc) ->
      assert (nln /= sarena `blame` (nln, "stairs looped")) $ do
        bitems <- getsGlobal getPlayerItem
        -- Remember the level (e.g., for a teleport via scroll on the floor).
        remember
        -- Remove the player from the old level.
        modifyGlobal (deleteActor pl)
        hs <- getsGlobal heroList
        -- Monsters hear that players not on the level. Cancel smell.
        -- Reduces memory load and savefile size.
        when (null hs) $
          modifyGlobal (updateArena (updateSmell (const IM.empty)))
        -- At this place the invariant that the player exists fails.
        -- Change to the new level (invariant not needed).
        switchLevel nln
        -- The player can now be safely added to the new level.
        modifyGlobal (insertActor pl pbody)
        modifyGlobal (updateAnyActorItem pl (const bitems))
        -- At this place the invariant is restored again.
        inhabitants <- getsGlobal (locToActor nloc)
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
        inhabitants2 <- getsGlobal (locToActor nloc)
        when (isJust inhabitants2) $ assert `failure` inhabitants2
        -- Land the player at the other end of the stairs.
        updatePlayerBody (\ p -> p { bloc = nloc })
        -- Change the level of the player recorded in cursor.
        modifyClient (updateCursor (\ c -> c { creturnLn = nln }))
        -- The invariant "at most one actor on a tile" restored.
        -- Create a backup of the savegame.
        saveGameBkp
        loc <- getLocal
        msgAdd $ lookAt cops False True loc nloc ""

-- | Change level and reset it's time and update the times of all actors.
-- The player may be added to @lactor@ of the new level only after
-- this operation is executed.
switchLevel :: MonadAction m => LevelId -> m ()
switchLevel nln = do
  remember  -- record changes before the level is frozen; we could do that at the end of the functino as well, recording changes after the level is thawed
  timeCurrent <- getsGlobal getTime
  sarena <- getsGlobal sarena
  when (sarena /= nln) $ do
    -- Switch to the new level.
    modifyGlobal (\ s -> s {sarena = nln})
    modifyLocal (\ s -> s {sarena = nln})  -- hack
    timeLastVisited <- getsGlobal getTime
    let diff = timeAdd timeCurrent $ timeNegate timeLastVisited
    when (diff /= timeZero) $ do
      -- Reset the level time.
      modifyGlobal $ updateTime $ const timeCurrent
      modifyLocal $ updateTime $ const timeCurrent  -- hack
      -- Update the times of all actors.
      let upd m@Actor{btime} = m {btime = timeAdd btime diff}
      modifyGlobal (updateArena (updateActor (IM.map upd)))

-- | The player leaves the dungeon.
fleeDungeon :: MonadAction m => m ()
fleeDungeon = do
  Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsGlobal scops
  s <- getGlobal
  go <- displayYesNo "This is the way out. Really leave now?"
  recordHistory  -- Prevent repeating the ending msgs.
  when (not go) $ abortWith "Game resumed."
  let (items, total) = calculateTotal s
  modifyServer (\ st -> st {squit = Just (False, Victor)})
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
    let currencyName = MU.Text $ oname $ ouniqGroup "currency"
        winMsg = makePhrase
          [ "Congratulations, you won!"
          , "Here's your loot, worth"
          , MU.NWs total currencyName
          , "." ]
    discoS <- getsGlobal sdisco
    io <- itemOverlay discoS True items
    sarenaes <- overlayToSlideshow winMsg io
    void $ getManyConfirms [] sarenaes
    modifyServer (\ st -> st {squit = Just (True, Victor)})

-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified.
itemEffectAction :: MonadAction m => Int -> ActorId -> ActorId -> Item -> Bool -> m ()
itemEffectAction verbosity source target item block = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsGlobal scops
  st <- getGlobal
  sarenaOld <- getsGlobal sarena
  discoS <- getsGlobal sdisco
  let effect = ieffect $ okind $ fromJust $ jkind discoS item
  -- The msg describes the target part of the action.
  (b1, b2) <- effectToAction effect verbosity source target (jpower item) block
  -- Party sees or affected, and the effect interesting,
  -- so the item gets identified.
  when (b1 && b2) $ discover item
  -- Destroys attacking actor and its items: a hack for projectiles.
  sarenaNew <- getsGlobal sarena
  modifyGlobal (\ s -> s {sarena = sarenaOld})
  when (isProjectile st source) $
    modifyGlobal (deleteActor source)
  modifyGlobal (\ s -> s {sarena = sarenaNew})

-- | Make the item known to the player.
discover :: MonadAction m => Item -> m ()
discover i = do
  Kind.COps{coitem} <- getsGlobal scops
  oldDisco <- getsLocal sdisco
  discoS <- getsGlobal sdisco
  let ix = jkindIx i
      ik = discoS M.! ix
  unless (ix `M.member` oldDisco) $ do
    modifyLocal (updateDiscoveries (M.insert ix ik))
    disco <- getsLocal sdisco
    let (object1, object2) = partItem coitem oldDisco i
        msg = makeSentence
          [ "the", MU.SubjectVerbSg (MU.Phrase [object1, object2])
                                    "turn out to be"
          , partItemAW coitem disco i ]
    msgAdd msg

-- | Make the actor controlled by the player. Switch level, if needed.
-- False, if nothing to do. Should only be invoked as a direct result
-- of a player action or the selected player actor death.
selectPlayer :: MonadAction m => ActorId -> m Bool
selectPlayer actor = do
  cops@Kind.COps{coactor} <- getsLocal scops
  pl    <- getsLocal splayer
  if actor == pl
    then return False -- already selected
    else do
      state <- getLocal
      let (nln, pbody, _) = findActorAnyLevel actor state
      -- Switch to the new level.
      switchLevel nln
      -- Make the new actor the player-controlled actor.
      modifyGlobal (\ s -> s {splayer = actor})  -- a hack
      modifyLocal (\ s -> s {splayer = actor})
      -- Record the original level of the new player.
      modifyClient (updateCursor (\ c -> c {creturnLn = nln}))
      -- Don't continue an old run, if any.
      stopRunning
      -- Announce.
      msgAdd $ makeSentence [partActor coactor pbody, "selected"]
      loc <- getLocal
      msgAdd $ lookAt cops False True loc (bloc pbody) ""
      return True

selectHero :: MonadAction m => Int -> m ()
selectHero k = do
  s <- getLocal
  case tryFindHeroK s k of
    Nothing  -> abortWith "No such member of the party."
    Just aid -> void $ selectPlayer aid

-- TODO: center screen, flash the background, etc. Perhaps wait for SPACE.
-- | Focus on the hero being wounded/displaced/etc.
focusIfOurs :: MonadActionRO m => ActorId -> m Bool
focusIfOurs target = do
  s  <- getLocal
  if isAHero s target
    then return True
    else return False

summonHeroes :: MonadAction m => Int -> Point -> m ()
summonHeroes n loc =
  assert (n > 0) $ do
  cops <- getsGlobal scops
  newHeroId <- getsServer scounter
  configUI <- askConfigUI
  s <- getGlobal
  ser <- getServer
  let (sN, serN) = iterate (uncurry $ addHero cops loc configUI) (s, ser) !! n
  putGlobal sN
  putServer serN
  b <- focusIfOurs newHeroId
  assert (b `blame` (newHeroId, "player summons himself")) $
    return ()

-- TODO: merge with summonHeroes; disregard "spawn" factions and "spawn"
-- flags for monsters; only check 'summon"
summonMonsters :: MonadAction m => Int -> Point -> m ()
summonMonsters n loc = do
  sfaction <- getsGlobal sfaction
  Kind.COps{ cotile
           , coactor=Kind.Ops{opick, okind}
           , cofact=Kind.Ops{opick=fopick, oname=foname}} <- getsGlobal scops
  spawnKindId <- rndToAction $ fopick "spawn" (const True)
  -- Spawn frequency required greater than zero, but otherwise ignored.
  let inFactionKind m = isJust $ lookup (foname spawnKindId) (afreq m)
  -- Summon frequency used for picking the actor.
  mk <- rndToAction $ opick "summon" inFactionKind
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  let bfaction = fst $ fromJust
                 $ find (\(_, fa) -> gkind fa == spawnKindId)
                 $ IM.toList sfaction
  s <- getGlobal
  ser <- getServer
  let (sN, serN) = iterate (uncurry $ addMonster cotile mk hp loc
                                                 bfaction False) (s, ser) !! n
  putGlobal sN
  putServer serN

-- | Remove dead heroes (or dead dominated monsters). Check if game is over.
-- For now we only check the selected hero and at current level,
-- but if poison, etc. is implemented, we'd need to check all heroes
-- on any level.
checkPartyDeath :: MonadAction m => m ()
checkPartyDeath = do
  cops@Kind.COps{coactor} <- getsGlobal scops
  per    <- askPerception
  ahs    <- getsGlobal allHeroesAnyLevel
  pl     <- getsGlobal splayer
  pbody  <- getsGlobal getPlayerBody
  Config{configFirstDeathEnds} <- getsServer sconfig
  when (bhp pbody <= 0) $ do
    msgAdd $ actorVerb coactor pbody "die"
    go <- displayMore ColorBW ""
    recordHistory  -- Prevent repeating the "die" msgs.
    let bodyToCorpse = updateAnyActor pl $ \ body -> body {bsymbol = Just '%'}
        animateDeath = do
          cli  <- getClient
          loc <- getLocal
          let animFrs = animate cli loc cops per $ deathBody (bloc pbody)
          displayFramesPush animFrs
        animateGameOver = do
          animateDeath
          bodyToCorpse
          gameOver go
    if configFirstDeathEnds
      then animateGameOver
      else case filter (/= pl) ahs of
             [] -> animateGameOver
             actor : _ -> do
               msgAdd "The survivors carry on."
               animateDeath
               -- One last look at the beautiful world.
               remember
               -- Remove the dead player.
               modifyGlobal deletePlayer
               -- At this place the invariant that the player exists fails.
               -- Select the new player-controlled hero (invariant not needed),
               -- but don't draw a frame for him with focusIfOurs,
               -- in case the focus changes again during the same turn.
               -- He's just a random next guy in the line.
               selectPlayer actor
                 >>= assert `trueM` (pl, actor, "player resurrects")
               -- At this place the invariant is restored again.

-- | End game, showing the ending screens, if requested.
gameOver :: MonadAction m => Bool -> m ()
gameOver showEndingScreens = do
  sarena <- getsGlobal sarena
  modifyServer (\st -> st {squit = Just (False, Killed sarena)})
  when showEndingScreens $ do
    Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsGlobal scops
    s <- getGlobal
    sdepth <- getsGlobal sdepth
    time <- getsGlobal getTime
    let (items, total) = calculateTotal s
        deepest = levelNumber sarena  -- use deepest visited instead of level of death
        failMsg | timeFit time timeTurn < 300 =
          "That song shall be short."
                | total < 100 =
          "Born poor, dies poor."
                | deepest < 4 && total < 500 =
          "This should end differently."
                | deepest < sdepth - 1 =
          "This defeat brings no dishonour."
                | deepest < sdepth =
          "That is your name. 'Almost'."
                | otherwise =
          "Dead heroes make better legends."
        currencyName = MU.Text $ oname $ ouniqGroup "currency"
        loseMsg = makePhrase
          [ failMsg
          , "You left"
          , MU.NWs total currencyName
          , "and some junk." ]
    if null items
      then modifyServer (\st -> st {squit = Just (True, Killed sarena)})
      else do
        discoS <- getsGlobal sdisco
        io <- itemOverlay discoS True items
        sarenaes <- overlayToSlideshow loseMsg io
        go <- getManyConfirms [] sarenaes
        when go $ modifyServer (\st -> st {squit = Just (True, Killed sarena)})

-- | Create a list of item names.
itemOverlay :: MonadActionRO m => Discoveries -> Bool -> [Item] -> m Overlay
itemOverlay disco sorted is = do
  Kind.COps{coitem} <- getsLocal scops
  let items | sorted = sortBy (cmpLetterMaybe `on` jletter) is
            | otherwise = is
      pr i = makePhrase [ letterLabel (jletter i)
                        , partItemNWs coitem disco i ]
             <> " "
  return $ map pr items

stopRunning :: MonadAction m => m ()
stopRunning = updatePlayerBody (\ p -> p { bdir = Nothing })

-- | Perform look around in the current location of the cursor.
doLook :: MonadAction m => WriterT Slideshow m ()
doLook = do
  cops@Kind.COps{coactor} <- getsLocal scops
  p    <- getsClient (clocation . scursor)
  loc  <- getLocal
  clvl   <- getsLocal getArena
  hms    <- getsLocal (lactor . getArena)
  per    <- askPerception
  target <- getsLocal (btarget . getPlayerBody)
  pl     <- getsLocal splayer
  targeting <- getsClient (ctargeting . scursor)
  assert (targeting /= TgtOff) $ do
    let canSee = IS.member p (totalVisible per)
        ihabitant | canSee = find (\ m -> bloc m == p) (IM.elems hms)
                  | otherwise = Nothing
        monsterMsg = maybe "" (\ m -> actorVerb coactor m "be here") ihabitant
        vis | not $ p `IS.member` totalVisible per =
                " (not visible)"  -- by party
            | actorReachesLoc pl p per = ""
            | otherwise = " (not reachable)"  -- by hero
        mode = case target of
                 TEnemy _ _ -> "[targeting monster" <> vis <> "]"
                 TLoc _     -> "[targeting location" <> vis <> "]"
                 TPath _    -> "[targeting path" <> vis <> "]"
                 TCursor    -> "[targeting current" <> vis <> "]"
        -- Show general info about current p.
        lookMsg = mode <+> lookAt cops True canSee loc p monsterMsg
        -- Check if there's something lying around at current p.
        is = clvl `atI` p
    modifyClient (\st -> st {slastKey = Nothing})
    if length is <= 2
      then do
        sarenaes <- promptToSlideshow lookMsg
        tell sarenaes
      else do
       disco <- getsLocal sdisco
       io <- itemOverlay disco False is
       sarenaes <- overlayToSlideshow lookMsg io
       tell sarenaes
