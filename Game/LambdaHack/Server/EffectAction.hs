{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | The @effectToAction@ function and related operations.
-- TODO: document
module Game.LambdaHack.Server.EffectAction where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU
import qualified Data.EnumSet as ES
import Data.Ratio ((%))
import qualified Data.Char as Char

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.State
import Game.LambdaHack.Server.CmdAtomicAction
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency

default (Text)

-- | Sentences such as \"Dog barks loudly.\".
actorVerb :: Kind.Ops ActorKind -> Actor -> Text -> Text
actorVerb coactor a v =
  makeSentence [MU.SubjectVerbSg (partActor coactor a) (MU.Text v)]

-- TODO: center screen, flash the background, etc. Perhaps wait for SPACE.
-- | Focus on the hero being wounded/displaced/etc.
focusIfOurs :: MonadActionAbort m => ActorId -> m Bool
focusIfOurs _target = return True

-- | The source actor affects the target actor, with a given effect and power.
-- The second argument is verbosity of the resulting message.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
effectToAction :: MonadServerChan m
               => Effect.Effect -> Int -> ActorId -> ActorId -> Int -> Bool
               -> m Bool
effectToAction effect verbosity source target power block = do
  oldS <- getsState (getActorBody source)
  oldT <- getsState (getActorBody target)
  let oldHP = bhp oldT
  (b, msg) <- eff effect verbosity source target power
  -- We assume if targed moved to a level, which is not the current level,
  -- his HP is unchanged.
  memTm <- getsState $ memActor target
  newHP <- if memTm
           then getsState (bhp . getActorBody target)
           else return oldHP
  -- Target part of message sent here, so only target visibility checked.
  void $ broadcastPosUI [bpos oldT]
         $ EffectCli msg (bpos oldT, bpos oldS) (newHP - oldHP) block
  -- TODO: use broadcastPosCli2 and to those that don't see the pos show that:
  -- when b $ msgAdd "You hear some noises."
  when (newHP <= 0) $ checkPartyDeath target
  return b

-- | The boolean part of the result says if the ation was interesting
-- and the string part describes how the target reacted
-- (not what the source did).
eff :: MonadServerChan m => Effect.Effect -> Int -> ActorId -> ActorId -> Int
    -> m (Bool, Text)
eff Effect.NoEffect _ _ _ _ = nullEffect
eff Effect.Heal _ _source target power = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsState scops
  tm <- getsState (getActorBody target)
  let bhpMax = maxDice (ahp $ okind $ bkind tm)
  if bhp tm >= bhpMax || power <= 0
    then nullEffect
    else do
      void $ focusIfOurs target
      let deltaHP = min power (bhpMax - bhp tm)
      healAtomic deltaHP target
      return (True, actorVerb coactor tm "feel better")
eff (Effect.Wound nDm) verbosity source target power = do
  Kind.COps{coactor} <- getsState scops
  s <- getState
  n <- rndToAction $ rollDice nDm
  let deltaHP = - (n + power)
  if deltaHP >= 0 then nullEffect else do
    void $ focusIfOurs target
    tm <- getsState (getActorBody target)
    isSpawning <- getsState $ flip isSpawningFaction $ bfaction tm
    let msg
          | bhp tm + deltaHP <= 0 =
            if isSpawning
            then ""  -- Handled later on in checkPartyDeath. Suspense.
            else -- Not as important, so let the player read the message
                 -- about monster death while he watches the combat animation.
              if isProjectile s target
              then actorVerb coactor tm "drop down"
              else actorVerb coactor tm "die"
          | source == target =  -- a potion of wounding, etc.
            actorVerb coactor tm "feel wounded"
          | verbosity <= 0 = ""
          | isSpawning =
            actorVerb coactor tm $ "lose" <+> showT (n + power) <> "HP"
          | otherwise = actorVerb coactor tm "hiss in pain"
    -- Damage the target.
    healAtomic deltaHP target
    return (True, msg)
eff Effect.Dominate _ source target _power = do
  arena <- getsState sarena
  side <- getsState sside
  if source == target
    then do
      genemy <- getsState $ genemy . getSide
      lm <- getsState $ actorNotProjList (`elem` genemy) . getArena
      lxsize <- getsState (lxsize . getArena)
      lysize <- getsState (lysize . getArena)
      let cross m = bpos m : vicinityCardinal lxsize lysize (bpos m)
          vis = ES.fromList $ concatMap cross lm
      lvl <- getsState getArena
      sendUpdateCli side $ RememberCli arena vis lvl
      return (True, "A dozen voices yells in anger.")
    else do
      Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
      Actor{bspeed, bkind} <- getsState (getActorBody target)
      -- Halve the speed as a side-effect of domination.
      let speed = fromMaybe (aspeed $ okind bkind) bspeed
          delta = speedScale (1%2) speed
      when (delta > speedZero) $ hasteAtomic target (speedNegate delta)
      -- TODO: Perhaps insert a turn of delay here to allow countermeasures.
      dominateAtomic target
      sendQueryCli side (SelectLeaderCli target arena)
        >>= assert `trueM` (arena, target, "leader dominates himself")
      -- Display status line and FOV for the new actor.
--TODO      sli <- promptToSlideshow ""
--      fr <- drawOverlay ColorBW $ head $ runSlideshow sli
--      displayFramesPush [Nothing, Just fr, Nothing]
      return (True, "")
eff Effect.SummonFriend _ source target power = do
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  summonFriends (bfaction sm) (1 + power) (bpos tm)
  return (True, "")
eff Effect.SpawnMonster _ _source target power = do
  tm <- getsState (getActorBody target)
  void $ spawnMonsters (1 + power) (bpos tm)
  return (True, "")
eff Effect.ApplyPerfume _ source target _ =
  if source == target
  then return (True, "Tastes like water, but with a strong rose scent.")
  else do
    let upd lvl = lvl { lsmell = EM.empty }
    modifyState (updateArena upd)
    return (True, "The fragrance quells all scents in the vicinity.")
eff Effect.Regeneration verbosity source target power =
  eff Effect.Heal verbosity source target power
eff Effect.Searching _ _source _target _power =
  return (True, "It gets lost and you search in vain.")
eff Effect.Ascend _ _ target power = do
  Kind.COps{coactor} <- getsState scops
  tm <- getsState (getActorBody target)
  void $ focusIfOurs target
  effLvlGoUp target (power + 1)
  -- TODO: The following message too late if a monster squashed by going up,
  -- unless it's ironic. ;) The same below.
  gquit <- getsState $ gquit . getSide
  return $ if maybe Camping snd gquit == Victor
           then (True, "")
           else (True, actorVerb coactor tm "find a way upstairs")
eff Effect.Descend _ _ target power = do
  Kind.COps{coactor} <- getsState scops
  tm <- getsState (getActorBody target)
  void $ focusIfOurs target
  effLvlGoUp target (- (power + 1))
  gquit <- getsState $ gquit . getSide
  return $ if maybe Camping snd gquit == Victor
           then (True, "")
           else (True, actorVerb coactor tm "find a way downstairs")

nullEffect :: MonadActionAbort m => m (Bool, Text)
nullEffect = return (False, "Nothing happens.")

-- TODO: refactor with actorAttackActor or perhaps displace the other
-- actor or swap positions with it, instead of squashing.
squashActor :: MonadServerChan m => ActorId -> ActorId -> m ()
squashActor source target = do
  Kind.COps{{-coactor,-} coitem=Kind.Ops{okind, ouniqGroup}} <- getsState scops
--  sm <- getsState (getActorBody source)
--  tm <- getsState (getActorBody target)
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  let h2hKind = ouniqGroup "weight"
      kind = okind h2hKind
      power = maxDeep $ ipower kind
      h2h = buildItem flavour discoRev h2hKind kind power
--      verb = iverbApply kind
--      msg = makeSentence
--        [ MU.SubjectVerbSg (partActor coactor sm) verb
--        , partActor coactor tm
--        , "in a staircase accident" ]
--  msgAdd msg
  itemEffectAction 0 source target h2h False
  s <- getState
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (memActor target s) `blame` (source, target, "not killed")) $
    return ()

effLvlGoUp :: MonadServerChan m => ActorId -> Int -> m ()
effLvlGoUp aid k = do
  pbodyCurrent <- getsState $ getActorBody aid
  bitems <- getsState $ getActorBag aid
  arena <- getsState sarena
  glo <- getState
  case whereTo glo arena k of
    Nothing -> fleeDungeon -- we are at the "end" of the dungeon
    Just (nln, npos) ->
      assert (nln /= arena `blame` (nln, "stairs looped")) $ do
        timeCurrent <- getsState getTime
        -- Remove the actor from the old level.
        killAtomic aid pbodyCurrent
        -- Remember the level (e.g., when teleporting via scroll on the floor,
        -- register the scroll vanished, also let the other factions register
        -- the actor vanished in case they switch to this level from another
        -- level). Perception is unchanged, so for one turn (this level turn)
        -- there will be visibility left on the old actor location.
        remember
        -- Only spawning factions left on the level, so no new smell generate.
        -- Cancel smell. Reduces memory load and savefile size.
        hs <- getsState $ actorList (not . isSpawningFaction glo) . getArena
        when (null hs) $
          modifyState (updateArena (updateSmell (const EM.empty)))
        -- Change arena, but not the leader yet. The actor will become
        -- a leader, but he is not inserted into the new level yet.
        modifyState $ updateSelectedArena nln
        -- Sync the actor time with the level time.
        timeLastVisited <- getsState getTime
        let diff = timeAdd (btime pbodyCurrent) (timeNegate timeCurrent)
            pbody = pbodyCurrent {btime = timeAdd timeLastVisited diff}
        -- The actor is added to the new level, but there can be other actors
        -- at his old position or at his new position.
        spawnAtomic aid pbody
        modifyState (updateActorItem aid (const bitems))
        -- Reset level and leader for all factions.
        broadcastCli [return . (/= bfaction pbody)] $ InvalidateArenaCli nln
        sendUpdateCli (bfaction pbody) $ SwitchLevelCli aid nln pbody bitems
        -- Checking actors at the new posiiton of the aid.
        inhabitants <- getsState (posToActor npos)
        case inhabitants of
          Nothing -> return ()
-- Broken if the effect happens, e.g. via a scroll and abort is not enough.
--          Just h | isAHero gloh ->
--            -- Bail out if a party member blocks the staircase.
--            abortWith "somebody blocks the staircase"
          Just m ->
            -- Aquash an actor blocking the staircase.
            -- This is not a duplication with the other calls to squashActor,
            -- because here an inactive actor is squashed.
            squashActor aid m
        -- Verify the monster on the staircase died.
        inhabitants2 <- getsState (posToActor npos)
        when (isJust inhabitants2) $ assert `failure` inhabitants2
        -- Land the aid at the other end of the stairs, which is now
        -- clear of other actors.
        modifyState $ updateActorBody aid $ \b -> b { bpos = npos }
        -- The property of at most one actor on a tile is restored.
        -- Create a backup of the savegame.
        saveGameBkp

-- TODO: right now it only works for human factions (sendQueryUI).
-- | The leader leaves the dungeon.
fleeDungeon :: MonadServerChan m => m ()
fleeDungeon = do
  Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
  glo <- getState
  side <- getsState sside
  go <- sendQueryUI side $ ConfirmYesNoCli
          "This is the way out. Really leave now?"
  when (not go) $ abortWith "Game resumed."
  let (bag, total) = calculateTotal glo
      upd f = f {gquit = Just (False, Victor)}
  modifyState $ updateSide upd
  if total == 0
  then do
    -- The player can back off at each of these steps.
    go1 <- sendQueryUI side $ ConfirmMoreBWCli
             "Afraid of the challenge? Leaving so soon and empty-handed?"
    when (not go1) $ abortWith "Brave soul!"
    go2 <- sendQueryUI side $ ConfirmMoreBWCli
            "This time try to grab some loot before escape!"
    when (not go2) $ abortWith "Here's your chance!"
  else do
    let currencyName = MU.Text $ oname $ ouniqGroup "currency"
        winMsg = makeSentence
          [ "Congratulations, you won!"
          , "Here's your loot, worth"
          , MU.NWs total currencyName ]
    discoS <- getsState sdisco
    sendUpdateUI side $ ShowItemsCli discoS winMsg bag
    let upd2 f = f {gquit = Just (True, Victor)}
    modifyState $ updateSide upd2

-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified.
itemEffectAction :: MonadServerChan m
                 => Int -> ActorId -> ActorId -> Item -> Bool -> m ()
itemEffectAction verbosity source target item block = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  sm <- getsState (getActorBody source)
  -- Destroys attacking actor: a hack for projectiles.
  when (bproj sm) $ killAtomic source sm
  discoS <- getsState sdisco
  let effect = ieffect $ okind $ fromJust $ jkind discoS item
  -- The msg describes the target part of the action.
  b <- effectToAction effect verbosity source target (jpower item) block
  -- The effect is interesting so the item gets identified, if seen.
  when b $ discover discoS item

-- TODO: send to all clients that see tpos.
-- | Make the item known to the leader.
discover :: MonadServerChan m => Discoveries -> Item -> m ()
discover discoS i = do
  side <- getsState sside
  let ix = jkindIx i
      ik = discoS EM.! ix
  sendUpdateCli side $ DiscoverCli ik i

addActor :: MonadServer m
         => Kind.Id ActorKind -> FactionId -> Point -> Int
         -> Maybe Char -> Maybe Text
         -> m ()
addActor mk bfaction ppos hp msymbol mname = do
  Kind.COps{cotile} <- getsState scops
  time <- getsState getTime
  pos <- getsState $ nearbyFreePos cotile ppos
  let m = actorTemplate mk msymbol mname hp pos time bfaction False
  acounter <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ acounter}
  spawnAtomic acounter m

-- TODO: apply this special treatment only to actors with symbol '@'.
-- | Create a new hero on the current level, close to the given position.
addHero :: MonadServer m => FactionId -> Point -> [(Int, Text)] -> m ()
addHero bfaction ppos configHeroNames = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsState scops
  let mk = heroKindId coactor
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  mhs <- mapM (\n -> getsState $ \s -> tryFindHeroK s bfaction n) [0..9]
  let freeHeroK = elemIndex Nothing mhs
      n = fromMaybe 100 freeHeroK
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      name = findHeroName configHeroNames n
      startHP = hp - (hp `div` 5) * min 3 n
  addActor mk bfaction ppos startHP (Just symbol) (Just name)

-- | Find a hero name in the config file, or create a stock name.
findHeroName :: [(Int, Text)] -> Int -> Text
findHeroName configHeroNames n =
  let heroName = lookup n configHeroNames
  in fromMaybe ("hero number" <+> showT n) heroName

summonFriends :: MonadServer m => FactionId -> Int -> Point -> m ()
summonFriends bfaction n pos = assert (n > 0) $ do
  Kind.COps{ coactor=coactor@Kind.Ops{opick}
           , cofact=Kind.Ops{okind} } <- getsState scops
  faction <- getsState sfaction
  let fact = okind $ gkind $ faction EM.! bfaction
  replicateM_ n $ do
    mk <- rndToAction $ opick (fname fact) (const True)
    if mk == heroKindId coactor
      then addHero bfaction pos []
      else addMonster mk bfaction pos

-- | Create a new monster on the level, at a given position
-- and with a given actor kind and HP.
addMonster :: MonadServer m
           => Kind.Id ActorKind -> FactionId -> Point -> m ()
addMonster mk bfaction ppos = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  addActor mk bfaction ppos hp Nothing Nothing

-- | Spawn monsters of any spawning faction, friendly or not.
-- To be used for spontaneous spawning of monsters and for the spawning effect.
spawnMonsters :: MonadServer m => Int -> Point -> m (Maybe FactionId)
spawnMonsters n pos = do
  Kind.COps{ coactor=Kind.Ops{opick}
           , cofact=Kind.Ops{okind} } <- getsState scops
  faction <- getsState sfaction
  let f (fid, fa) =
        let kind = okind (gkind fa)
        in if fspawn kind <= 0
           then Nothing
           else Just (fspawn kind, (kind, fid))
  case catMaybes $ map f $ EM.assocs faction of
    [] -> return Nothing  -- no faction spawns
    spawnList -> do
      let freq = toFreq "spawn" spawnList
      (spawnKind, bfaction) <- rndToAction $ frequency freq
      replicateM_ n $ do
        mk <- rndToAction $ opick (fname spawnKind) (const True)
        addMonster mk bfaction pos
      return $ Just bfaction

-- | Remove a dead actor. Check if game over.
checkPartyDeath :: MonadServerChan m => ActorId -> m ()
checkPartyDeath target = do
  tm <- getsState $ getActorBody target
  Config{configFirstDeathEnds} <- getsServer sconfig
  -- Place the actor's possessions on the map.
  bitems <- getsState $ getActorBag target
  modifyState $ updateArena $ dropItemsAt bitems $ bpos tm
  let fid = bfaction tm
  isHuman <- getsState $ flip isHumanFaction fid
  let animateDeath = do
        broadcastPosUI [bpos tm] (AnimateDeathCli target)
        if isHuman then sendQueryUI fid CarryOnCli else return False
      animateGameOver = do
        go <- animateDeath
        modifyState $ updateActorBody target $ \b -> b {bsymbol = Just '%'}
        -- TODO: add side argument to gameOver instead
        side <- getsState sside
        switchGlobalSelectedSide fid
        gameOver go
        switchGlobalSelectedSide side
  isSpawning <- getsState $ flip isSpawningFaction fid
  -- For a swapning faction, no messages nor animations are shown below.
  -- A message was shown in @eff@ and that's enough.
  when (not isSpawning) $ do
    if configFirstDeathEnds
      then animateGameOver
      else void $ animateDeath
  -- Remove the dead actor.
  killAtomic target tm
  -- We don't register that the lethal potion on the floor
  -- is used up. If that's a problem, add a one turn delay
  -- to the effect of all lethal objects (displaying an "agh! dying"
  -- message). Other factions do register that the actor (and potion)
  -- is gone, because the level is not switched and so perception
  -- is recorded for this level.

-- | End game, showing the ending screens, if requested.
gameOver :: MonadServerChan m => Bool -> m ()
gameOver showEndingScreens = do
  arena <- getsState sarena
  deepest <- getsState $ ldepth . getArena  -- TODO: use deepest visited instead of current
  let upd f = f {gquit = Just (False, Killed arena)}
  modifyState $ updateSide upd
  when showEndingScreens $ do
    Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
    s <- getState
    depth <- getsState sdepth
    time <- getsState getTime
    let (bag, total) = calculateTotal s
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
    if EM.null bag
      then do
        let upd2 f = f {gquit = Just (True, Killed arena)}
        modifyState $ updateSide upd2
      else do
        discoS <- getsState sdisco
        -- TODO: do this for the killed factions, not for side
        side <- getsState sside
        go <- sendQueryUI side
              $ ConfirmShowItemsCli discoS loseMsg bag
        when go $ do
          let upd2 f = f {gquit = Just (True, Killed arena)}
          modifyState $ updateSide upd2
