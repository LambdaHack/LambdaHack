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
import qualified Game.LambdaHack.Dungeon as Dungeon
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
  modifyServer (\ state -> state {srandom = ng})
  return a

-- | Update actor stats. Works for actors on other levels, too.
updateAnyActor :: MonadAction m => ActorId -> (Actor -> Actor) -> m ()
updateAnyActor actor f = modifyServer (updateAnyActorBody actor f)

-- | Update player-controlled actor stats.
updatePlayerBody :: MonadAction m => (Actor -> Actor) -> m ()
updatePlayerBody f = do
  pl <- getsServer splayer
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
  oldTm <- getsServer (getActor target)
  let oldHP = bhp oldTm
  (b, msg) <- eff effect verbosity source target power
  s <- getServer
  -- If the target killed outright by the effect (e.g., in a recursive call),
  -- there's nothing left to do. TODO: hacky; aren't messages lost?
  if not (memActor target s)
   then return (b, False)
   else do
    sm  <- getsServer (getActor source)
    tm  <- getsServer (getActor target)
    per <- askPerception
    pl  <- getsServer splayer
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
      cops <- askCOps
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
      bitems <- getsServer (getActorItem target)
      modifyServer (updateLevel (dropItemsAt bitems tloc))
      -- Clean bodies up.
      if target == pl
        then  -- Kill the player and check game over.
          checkPartyDeath
        else  -- Kill the enemy.
          modifyServer (deleteActor target)
    return bb

-- | The boolean part of the result says if the ation was interesting
-- and the string part describes how the target reacted
-- (not what the source did).
eff :: MonadAction m => Effect.Effect -> Int -> ActorId -> ActorId -> Int
    -> m (Bool, Text)
eff Effect.NoEffect _ _ _ _ = nullEffect
eff Effect.Heal _ _source target power = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- askCOps
  let bhpMax m = maxDice (ahp $ okind $ bkind m)
  tm <- getsServer (getActor target)
  if bhp tm >= bhpMax tm || power <= 0
    then nullEffect
    else do
      void $ focusIfOurs target
      updateAnyActor target (addHp coactor power)
      return (True, actorVerb coactor tm "feel better")
eff (Effect.Wound nDm) verbosity source target power = do
  Kind.COps{coactor} <- askCOps
  s <- getServer
  n <- rndToAction $ rollDice nDm
  if n + power <= 0 then nullEffect else do
    void $ focusIfOurs target
    pl <- getsServer splayer
    tm <- getsServer (getActor target)
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
  Kind.COps{coactor=Kind.Ops{okind}} <- askCOps
  s <- getServer
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
      updatePlayerBody (\ m -> m { bfaction = sfaction s
                                 , btime = stime s
                                 , bspeed = halfSpeed m })
      -- Display status line and FOV for the newly controlled actor.
      sli <- promptToSlideshow ""
      fr <- drawOverlay ColorBW $ head $ runSlideshow sli
      displayFramesPush [Nothing, Just fr, Nothing]
      return (True, "")
    else if source == target
         then do
           lm <- getsServer hostileList
           lxsize <- getsServer (lxsize . slevel)
           lysize <- getsServer (lysize . slevel)
           let cross m = bloc m : vicinityCardinal lxsize lysize (bloc m)
               vis = IS.fromList $ concatMap cross lm
           rememberList vis
           return (True, "A dozen voices yells in anger.")
         else nullEffect
eff Effect.SummonFriend _ source target power = do
  tm <- getsServer (getActor target)
  s <- getServer
  if isAHero s source
    then summonHeroes (1 + power) (bloc tm)
    else summonMonsters (1 + power) (bloc tm)
  return (True, "")
eff Effect.SummonEnemy _ source target power = do
  tm <- getsServer (getActor target)
  s  <- getServer
  if isAHero s source
    then summonMonsters (1 + power) (bloc tm)
    else summonHeroes (1 + power) (bloc tm)
  return (True, "")
eff Effect.ApplyPerfume _ source target _ =
  if source == target
  then return (True, "Tastes like water, but with a strong rose scent.")
  else do
    let upd lvl = lvl { lsmell = IM.empty }
    modifyServer (updateLevel upd)
    return (True, "The fragrance quells all scents in the vicinity.")
eff Effect.Regeneration verbosity source target power =
  eff Effect.Heal verbosity source target power
eff Effect.Searching _ _source _target _power =
  return (True, "It gets lost and you search in vain.")
eff Effect.Ascend _ source target power = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- askCOps
  tm <- getsServer (getActor target)
  void $ focusIfOurs target
  if aiq (okind (bkind tm)) < 13  -- monsters can't use stairs
    then squashActor source target
    else effLvlGoUp (power + 1)
  -- TODO: The following message too late if a monster squashed by going up,
  -- unless it's ironic. ;) The same below.
  s2 <- getServer
  return $ if maybe Camping snd (squit s2) == Victor
           then (True, "")
           else (True, actorVerb coactor tm "find a way upstairs")
eff Effect.Descend _ source target power = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- askCOps
  tm <- getsServer (getActor target)
  void $ focusIfOurs target
  if aiq (okind (bkind tm)) < 13  -- monsters can't use stairs
    then squashActor source target
    else effLvlGoUp (- (power + 1))
  s2 <- getServer
  return $ if maybe Camping snd (squit s2) == Victor
           then (True, "")
           else (True, actorVerb coactor tm "find a way downstairs")

nullEffect :: MonadActionPure m => m (Bool, Text)
nullEffect = return (False, "Nothing happens.")

-- TODO: refactor with actorAttackActor.
squashActor :: MonadAction m => ActorId -> ActorId -> m ()
squashActor source target = do
  Kind.COps{coactor, coitem=Kind.Ops{okind, ouniqGroup}} <- askCOps
  sm <- getsServer (getActor source)
  tm <- getsServer (getActor target)
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
  s <- getServer
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (memActor target s) `blame` (source, target, "not killed")) $
    return ()

effLvlGoUp :: MonadAction m => Int -> m ()
effLvlGoUp k = do
  pbody     <- getsServer getPlayerBody
  pl        <- getsServer splayer
  slid      <- getsServer slid
  st        <- getServer
  cops <- askCOps
  clvl <- getsServer slevelClient
  case whereTo st k of
    Nothing -> fleeDungeon -- we are at the "end" of the dungeon
    Just (nln, nloc) ->
      assert (nln /= slid `blame` (nln, "stairs looped")) $ do
        bitems <- getsServer getPlayerItem
        -- Remember the level (e.g., for a teleport via scroll on the floor).
        remember
        -- Remove the player from the old level.
        modifyServer (deleteActor pl)
        hs <- getsServer heroList
        -- Monsters hear that players not on the level. Cancel smell.
        -- Reduces memory load and savefile size.
        when (null hs) $
          modifyServer (updateLevel (updateSmell (const IM.empty)))
        -- At this place the invariant that the player exists fails.
        -- Change to the new level (invariant not needed).
        switchLevel nln
        -- The player can now be safely added to the new level.
        modifyServer (insertActor pl pbody)
        modifyServer (updateAnyActorItem pl (const bitems))
        -- At this place the invariant is restored again.
        inhabitants <- getsServer (locToActor nloc)
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
        inhabitants2 <- getsServer (locToActor nloc)
        when (isJust inhabitants2) $ assert `failure` inhabitants2
        -- Land the player at the other end of the stairs.
        updatePlayerBody (\ p -> p { bloc = nloc })
        -- Change the level of the player recorded in cursor.
        modifyServer (updateCursor (\ c -> c { creturnLn = nln }))
        -- The invariant "at most one actor on a tile" restored.
        -- Create a backup of the savegame.
        saveGameBkp
        state <- getServer
        msgAdd $ lookAt cops False True state clvl nloc ""

-- | Change level and reset it's time and update the times of all actors.
-- The player may be added to @lactor@ of the new level only after
-- this operation is executed.
switchLevel :: MonadAction m => Dungeon.LevelId -> m ()
switchLevel nln = do
  timeCurrent <- getsServer stime
  slid <- getsServer slid
  when (slid /= nln) $ do
    -- Switch to the new level.
    modifyServer (\ s -> s {slid = nln})
    timeLastVisited <- getsServer stime
    let diff = timeAdd timeCurrent $ timeNegate timeLastVisited
    when (diff /= timeZero) $ do
      -- Reset the level time.
      modifyServer $ updateTime $ const timeCurrent
      -- Update the times of all actors.
      let upd m@Actor{btime} = m {btime = timeAdd btime diff}
      modifyServer (updateLevel (updateActor (IM.map upd)))

-- | The player leaves the dungeon.
fleeDungeon :: MonadAction m => m ()
fleeDungeon = do
  Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- askCOps
  s <- getServer
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
    discoS <- getsServer sdiscoS
    io <- itemOverlay discoS True items
    slides <- overlayToSlideshow winMsg io
    void $ getManyConfirms [] slides
    modifyServer (\ st -> st {squit = Just (True, Victor)})

-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified.
itemEffectAction :: MonadAction m => Int -> ActorId -> ActorId -> Item -> Bool -> m ()
itemEffectAction verbosity source target item block = do
  Kind.COps{coitem=Kind.Ops{okind}} <- askCOps
  st <- getServer
  slidOld <- getsServer slid
  discoS <- getsServer sdiscoS
  let effect = ieffect $ okind $ fromJust $ jkind discoS item
  -- The msg describes the target part of the action.
  (b1, b2) <- effectToAction effect verbosity source target (jpower item) block
  -- Party sees or affected, and the effect interesting,
  -- so the item gets identified.
  when (b1 && b2) $ discover item
  -- Destroys attacking actor and its items: a hack for projectiles.
  slidNew <- getsServer slid
  modifyServer (\ s -> s {slid = slidOld})
  when (isProjectile st source) $
    modifyServer (deleteActor source)
  modifyServer (\ s -> s {slid = slidNew})

-- | Make the item known to the player.
discover :: MonadAction m => Item -> m ()
discover i = do
  Kind.COps{coitem} <- askCOps
  oldDisco <- getsServer sdisco
  discoS <- getsServer sdiscoS
  let ix = jkindIx i
      ik = discoS M.! ix
  unless (ix `M.member` oldDisco) $ do
    modifyServer (updateDiscoveries (M.insert ix ik))
    disco <- getsServer sdisco
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
  cops@Kind.COps{coactor} <- askCOps
  pl    <- getsServer splayer
  clvl  <- getsServer slevelClient
  if actor == pl
    then return False -- already selected
    else do
      state <- getServer
      let (nln, pbody, _) = findActorAnyLevel actor state
      -- Switch to the new level.
      switchLevel nln
      -- Make the new actor the player-controlled actor.
      modifyServer (\ s -> s {splayer = actor})
      -- Record the original level of the new player.
      modifyServer (updateCursor (\ c -> c {creturnLn = nln}))
      -- Don't continue an old run, if any.
      stopRunning
      -- Announce.
      msgAdd $ makeSentence [partActor coactor pbody, "selected"]
      msgAdd $ lookAt cops False True state clvl (bloc pbody) ""
      return True

selectHero :: MonadAction m => Int -> m ()
selectHero k = do
  s <- getServer
  case tryFindHeroK s k of
    Nothing  -> abortWith "No such member of the party."
    Just aid -> void $ selectPlayer aid

-- TODO: center screen, flash the background, etc. Perhaps wait for SPACE.
-- | Focus on the hero being wounded/displaced/etc.
focusIfOurs :: MonadActionPure m => ActorId -> m Bool
focusIfOurs target = do
  s  <- getServer
  if isAHero s target
    then return True
    else return False

summonHeroes :: MonadAction m => Int -> Point -> m ()
summonHeroes n loc =
  assert (n > 0) $ do
  cops <- askCOps
  newHeroId <- getsServer scounter
  configUI <- askConfigUI
  modifyServer (\ state -> iterate (addHero cops loc configUI) state !! n)
  b <- focusIfOurs newHeroId
  assert (b `blame` (newHeroId, "player summons himself")) $
    return ()

-- TODO: probably not the right semantics: for each kind of factions
-- that spawn, only the first faction with that kind is every picked.
summonMonsters :: MonadAction m => Int -> Point -> m ()
summonMonsters n loc = do
  factions <- getsServer sfactions
  Kind.COps{ cotile
           , coactor=Kind.Ops{opick, okind}
           , cofact=Kind.Ops{opick=fopick, oname=foname}} <- askCOps
  spawnKindId <- rndToAction $ fopick "spawn" (const True)
  -- Spawn frequency required greater than zero, but otherwise ignored.
  let inFactionKind m = isJust $ lookup (foname spawnKindId) (afreq m)
  -- Summon frequency used for picking the actor.
  mk <- rndToAction $ opick "summon" inFactionKind
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  let bfaction = fst $ fromJust
                 $ find (\(_, fa) -> gkind fa == spawnKindId)
                 $ IM.toList factions
  modifyServer (\ s -> iterate (addMonster cotile mk hp loc
                                           bfaction False) s !! n)

-- | Remove dead heroes (or dead dominated monsters). Check if game is over.
-- For now we only check the selected hero and at current level,
-- but if poison, etc. is implemented, we'd need to check all heroes
-- on any level.
checkPartyDeath :: MonadAction m => m ()
checkPartyDeath = do
  cops@Kind.COps{coactor} <- askCOps
  per    <- askPerception
  ahs    <- getsServer allHeroesAnyLevel
  pl     <- getsServer splayer
  pbody  <- getsServer getPlayerBody
  Config{configFirstDeathEnds} <- getsServer sconfig
  when (bhp pbody <= 0) $ do
    msgAdd $ actorVerb coactor pbody "die"
    go <- displayMore ColorBW ""
    recordHistory  -- Prevent repeating the "die" msgs.
    let bodyToCorpse = updateAnyActor pl $ \ body -> body {bsymbol = Just '%'}
        animateDeath = do
          diary  <- getDiary
          s <- getServer
          let animFrs = animate s diary cops per $ deathBody (bloc pbody)
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
               modifyServer deletePlayer
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
  slid <- getsServer slid
  modifyServer (\st -> st {squit = Just (False, Killed slid)})
  when showEndingScreens $ do
    Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- askCOps
    s <- getServer
    dng <- getsServer sdungeon
    time <- getsServer stime
    let (items, total) = calculateTotal s
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
        currencyName = MU.Text $ oname $ ouniqGroup "currency"
        loseMsg = makePhrase
          [ failMsg
          , "You left"
          , MU.NWs total currencyName
          , "and some junk." ]
    if null items
      then modifyServer (\st -> st {squit = Just (True, Killed slid)})
      else do
        discoS <- getsServer sdiscoS
        io <- itemOverlay discoS True items
        slides <- overlayToSlideshow loseMsg io
        go <- getManyConfirms [] slides
        when go $ modifyServer (\st -> st {squit = Just (True, Killed slid)})

-- | Create a list of item names.
itemOverlay :: MonadActionPure m => Discoveries -> Bool -> [Item] -> m Overlay
itemOverlay disco sorted is = do
  Kind.COps{coitem} <- askCOps
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
  cops@Kind.COps{coactor} <- askCOps
  loc    <- getsServer (clocation . scursor)
  state  <- getServer
  clvl   <- getsServer slevelClient
  hms    <- getsServer (lactor . slevel)
  per    <- askPerception
  target <- getsServer (btarget . getPlayerBody)
  pl     <- getsServer splayer
  targeting <- getsServer (ctargeting . scursor)
  assert (targeting /= TgtOff) $ do
    let canSee = IS.member loc (totalVisible per)
        ihabitant | canSee = find (\ m -> bloc m == loc) (IM.elems hms)
                  | otherwise = Nothing
        monsterMsg = maybe "" (\ m -> actorVerb coactor m "be here") ihabitant
        vis | not $ loc `IS.member` totalVisible per =
                " (not visible)"  -- by party
            | actorReachesLoc pl loc per = ""
            | otherwise = " (not reachable)"  -- by hero
        mode = case target of
                 TEnemy _ _ -> "[targeting monster" <> vis <> "]"
                 TLoc _     -> "[targeting location" <> vis <> "]"
                 TPath _    -> "[targeting path" <> vis <> "]"
                 TCursor    -> "[targeting current" <> vis <> "]"
        -- Show general info about current loc.
        lookMsg = mode <+> lookAt cops True canSee state clvl loc monsterMsg
        -- Check if there's something lying around at current loc.
        is = clvl `rememberAtI` loc
    modifyServer (\st -> st {slastKey = Nothing})
    if length is <= 2
      then do
        slides <- promptToSlideshow lookMsg
        tell slides
      else do
       disco <- getsServer sdisco
       io <- itemOverlay disco False is
       slides <- overlayToSlideshow lookMsg io
       tell slides
