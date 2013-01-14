{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | The @effectToAction@ function and related operations.
-- TODO: document
module Game.LambdaHack.EffectAction where

import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio ((%))
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Command
import Game.LambdaHack.Config
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

default (Text)

-- | Sentences such as \"Dog barks loudly.\".
actorVerb :: Kind.Ops ActorKind -> Actor -> Text -> Text
actorVerb coactor a v =
  makeSentence [MU.SubjectVerbSg (partActor coactor a) (MU.Text v)]

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadServer m => Rnd a -> m a
rndToAction r = do
  g <- getsServer srandom
  let (a, ng) = St.runState r g
  modifyServer (\ ser -> ser {srandom = ng})
  return a

-- TODO: center screen, flash the background, etc. Perhaps wait for SPACE.
-- | Focus on the hero being wounded/displaced/etc.
focusIfOurs :: MonadActionRoot m => ActorId -> m Bool
focusIfOurs _target = do
--  s  <- getLocal
  if True -- isAHero s target
    then return True
    else return False

-- | The source actor affects the target actor, with a given effect and power.
-- The second argument is verbosity of the resulting message.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
effectToAction :: MonadServerChan m
               => Effect.Effect -> Int -> ActorId -> ActorId -> Int -> Bool
               -> m Bool
effectToAction effect verbosity source target power block = do
  oldS <- getsGlobal (getActorBody target)
  oldT <- getsGlobal (getActorBody target)
  let oldHP = bhp oldT
  (b, msg) <- eff effect verbosity source target power
  -- We assume if targed moved to a level, which is not the current level,
  -- his HP is unchanged.
  memTm <- getsGlobal $ memActor target
  newHP <- if memTm
           then getsGlobal (bhp . getActorBody target)
           else return oldHP
  -- Target part of message sent here, so only target visibility checked.
  void $ sendToPlayers [bpos oldT]
         $ EffectCli msg (bpos oldT, bpos oldS) (newHP - oldHP) block
  -- TODO: use sendToPlayers2 and to those that don't see the pos show that:
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
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsGlobal scops
  let bhpMax m = maxDice (ahp $ okind $ bkind m)
  tm <- getsGlobal (getActorBody target)
  if bhp tm >= bhpMax tm || power <= 0
    then nullEffect
    else do
      void $ focusIfOurs target
      modifyGlobal $ updateActorBody target (addHp coactor power)
      return (True, actorVerb coactor tm "feel better")
eff (Effect.Wound nDm) verbosity source target power = do
  Kind.COps{coactor} <- getsGlobal scops
  s <- getGlobal
  n <- rndToAction $ rollDice nDm
  if n + power <= 0 then nullEffect else do
    void $ focusIfOurs target
    tm <- getsGlobal (getActorBody target)
    isSpawning <- getsGlobal $ flip isSpawningFaction $ bfaction tm
    let newHP = bhp tm - n - power
        msg
          | newHP <= 0 =
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
    modifyGlobal $ updateActorBody target $ \ m -> m { bhp = newHP }
    return (True, msg)
eff Effect.Dominate _ source target _power = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsGlobal scops
  s <- getGlobal
  arena <- getsGlobal sarena
  if not $ isAHero s target
    then do  -- Monsters have weaker will than heroes.
      selectLeaderSer target arena
        >>= assert `trueM` (source, arena, target, "leader dominates himself")
      -- Halve the speed as a side-effect of domination.
      let halfSpeed :: Actor -> Maybe Speed
          halfSpeed Actor{bkind} =
            let speed = aspeed $ okind bkind
            in Just $ speedScale (1%2) speed
      -- Sync the monster with the hero move time for better display
      -- of missiles and for the domination to actually take one player's turn.
      modifyGlobal $ updateActorBody target $ \ b -> b { bfaction = sside s
                                                       , btime = getTime s
                                                       , bspeed = halfSpeed b }
      -- Display status line and FOV for the new actor.
--TODO      sli <- promptToSlideshow ""
--      fr <- drawOverlay ColorBW $ head $ runSlideshow sli
--      displayFramesPush [Nothing, Just fr, Nothing]
      return (True, "")
    else if source == target
         then do
           lm <- getsGlobal hostileList
           lxsize <- getsGlobal (lxsize . getArena)
           lysize <- getsGlobal (lysize . getArena)
           let cross m = bpos m : vicinityCardinal lxsize lysize (bpos m)
               vis = IS.fromList $ concatMap cross lm
           lvl <- getsGlobal getArena
           side <- getsGlobal sside
           sendUpdateCli side $ RememberCli arena vis lvl
           return (True, "A dozen voices yells in anger.")
         else nullEffect
eff Effect.SummonFriend _ source target power = do
  tm <- getsGlobal (getActorBody target)
  s <- getGlobal
  if isAHero s source
    then summonHeroes (1 + power) (bpos tm)
    else summonMonsters (1 + power) (bpos tm)
  return (True, "")
eff Effect.SummonEnemy _ source target power = do
  tm <- getsGlobal (getActorBody target)
  s  <- getGlobal
  if isAHero s source
    then summonMonsters (1 + power) (bpos tm)
    else summonHeroes (1 + power) (bpos tm)
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
eff Effect.Ascend _ _ target power = do
  Kind.COps{coactor} <- getsGlobal scops
  tm <- getsGlobal (getActorBody target)
  void $ focusIfOurs target
  effLvlGoUp target (power + 1)
  -- TODO: The following message too late if a monster squashed by going up,
  -- unless it's ironic. ;) The same below.
  side <- getsGlobal sside
  gquit <- getsGlobal $ gquit . (IM.! side) . sfaction
  return $ if maybe Camping snd gquit == Victor
           then (True, "")
           else (True, actorVerb coactor tm "find a way upstairs")
eff Effect.Descend _ _ target power = do
  Kind.COps{coactor} <- getsGlobal scops
  tm <- getsGlobal (getActorBody target)
  void $ focusIfOurs target
  effLvlGoUp target (- (power + 1))
  side <- getsGlobal sside
  gquit <- getsGlobal $ gquit . (IM.! side) . sfaction
  return $ if maybe Camping snd gquit == Victor
           then (True, "")
           else (True, actorVerb coactor tm "find a way downstairs")

nullEffect :: MonadActionRoot m => m (Bool, Text)
nullEffect = return (False, "Nothing happens.")

-- TODO: refactor with actorAttackActor or perhaps displace the other
-- actor or swap positions with it, instead of squashing.
squashActor :: MonadServerChan m => ActorId -> ActorId -> m ()
squashActor source target = do
  Kind.COps{{-coactor,-} coitem=Kind.Ops{okind, ouniqGroup}} <- getsGlobal scops
--  sm <- getsGlobal (getActorBody source)
--  tm <- getsGlobal (getActorBody target)
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  let h2hKind = ouniqGroup "weight"
      kind = okind h2hKind
      power = maxDeep $ ipower kind
      h2h = buildItem flavour discoRev h2hKind kind 1 power
--      verb = iverbApply kind
--      msg = makeSentence
--        [ MU.SubjectVerbSg (partActor coactor sm) verb
--        , partActor coactor tm
--        , "in a staircase accident" ]
--  msgAdd msg
  itemEffectAction 0 source target h2h False
  s <- getGlobal
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (memActor target s) `blame` (source, target, "not killed")) $
    return ()

effLvlGoUp :: MonadServerChan m => ActorId -> Int -> m ()
effLvlGoUp aid k = do
  pbodyCurrent <- getsGlobal $ getActorBody aid
  bitems <- getsGlobal $ getActorItem aid
  arena <- getsGlobal sarena
  glo <- getGlobal
  case whereTo glo arena k of
    Nothing -> fleeDungeon -- we are at the "end" of the dungeon
    Just (nln, npos) ->
      assert (nln /= arena `blame` (nln, "stairs looped")) $ do
        timeCurrent <- getsGlobal getTime
        -- Remove the actor from the old level.
        modifyGlobal (deleteActor aid)
        -- Remember the level (e.g., when teleporting via scroll on the floor,
        -- register the scroll vanished, also let the other factions register
        -- the actor vanished in case they switch to this level from another
        -- level). Perception is unchanged, so for one turn (this level turn)
        -- there will be visibility left on the old actor location.
        remember
        -- Monsters hear no foe is left on the level. Cancel smell.
        -- Reduces memory load and savefile size.
        hs <- getsGlobal heroList
        when (null hs) $
          modifyGlobal (updateArena (updateSmell (const IM.empty)))
        -- Change arena, but not the leader yet. The actor will become
        -- a leader, but he is not inserted into the new level yet.
        modifyGlobal $ updateSelectedArena nln
        -- Sync the actor time with the level time.
        timeLastVisited <- getsGlobal getTime
        let diff = timeAdd (btime pbodyCurrent) (timeNegate timeCurrent)
            pbody = pbodyCurrent {btime = timeAdd timeLastVisited diff}
        -- The actor is added to the new level, but there can be other actors
        -- at his old position or at his new position.
        modifyGlobal (insertActor aid pbody)
        modifyGlobal (updateActorItem aid (const bitems))
        -- Reset level and leader for all factions.
        void $ sendUpdateClis $ \fid ->
          if fid /= bfaction pbody
          then InvalidateArenaCli nln
          else SwitchLevelCli aid nln pbody bitems
        -- Checking actors at the new posiiton of the aid.
        inhabitants <- getsGlobal (posToActor npos)
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
        inhabitants2 <- getsGlobal (posToActor npos)
        when (isJust inhabitants2) $ assert `failure` inhabitants2
        -- Land the aid at the other end of the stairs, which is now
        -- clear of other actors.
        modifyGlobal $ updateActorBody aid $ \b -> b { bpos = npos }
        -- The property of at most one actor on a tile is restored.
        -- Create a backup of the savegame.
        saveGameBkp

-- | The leader leaves the dungeon.
fleeDungeon :: MonadServerChan m => m ()
fleeDungeon = do
  Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsGlobal scops
  glo <- getGlobal
  side <- getsGlobal sside
  go <- sendQueryCli side $ ConfirmYesNoCli
          "This is the way out. Really leave now?"
  when (not go) $ abortWith "Game resumed."
  let (items, total) = calculateTotal glo
      upd f = f {gquit = Just (False, Victor)}
  modifyGlobal $ updateSide upd
  if total == 0
  then do
    -- The player can back off at each of these steps.
    go1 <- sendQueryCli side $ ConfirmMoreBWCli
             "Afraid of the challenge? Leaving so soon and empty-handed?"
    when (not go1) $ abortWith "Brave soul!"
    go2 <- sendQueryCli side $ ConfirmMoreBWCli
            "This time try to grab some loot before escape!"
    when (not go2) $ abortWith "Here's your chance!"
  else do
    let currencyName = MU.Text $ oname $ ouniqGroup "currency"
        winMsg = makeSentence
          [ "Congratulations, you won!"
          , "Here's your loot, worth"
          , MU.NWs total currencyName ]
    discoS <- getsGlobal sdisco
    sendUpdateCli side $ ShowItemsCli discoS winMsg items
    let upd2 f = f {gquit = Just (True, Victor)}
    modifyGlobal $ updateSide upd2

-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified.
itemEffectAction :: MonadServerChan m
                 => Int -> ActorId -> ActorId -> Item -> Bool -> m ()
itemEffectAction verbosity source target item block = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsGlobal scops
  st <- getGlobal
  -- Destroys attacking actor and its items: a hack for projectiles.
  when (isProjectile st source) $
    modifyGlobal (deleteActor source)
  discoS <- getsGlobal sdisco
  let effect = ieffect $ okind $ fromJust $ jkind discoS item
  -- The msg describes the target part of the action.
  b <- effectToAction effect verbosity source target (jpower item) block
  -- The effect is interesting so the item gets identified, if seen.
  when b $ discover discoS item

-- TODO: send to all clients that see tpos.
-- | Make the item known to the leader.
discover :: MonadServerChan m => Discoveries -> Item -> m ()
discover discoS i = do
  side <- getsGlobal sside
  let ix = jkindIx i
      ik = discoS M.! ix
  sendUpdateCli side $ DiscoverCli ik i

selectLeaderSer :: MonadServerChan m => ActorId -> LevelId -> m Bool
selectLeaderSer actor lid = do
  side <- getsGlobal sside
  b <- sendQueryCli side $ SelectLeaderCli actor lid
  modifyGlobal $ updateSelectedArena lid
  return b

summonHeroes :: MonadServer m => Int -> Point -> m ()
summonHeroes n pos =
  assert (n > 0) $ do
  cops <- getsGlobal scops
  newHeroId <- getsServer scounter
  s <- getGlobal
  ser <- getServer
  side <- getsGlobal sside
  let (sN, serN) = iterate (uncurry $ addHero cops pos side) (s, ser) !! n
  putGlobal sN
  putServer serN
  b <- focusIfOurs newHeroId
  assert (b `blame` (newHeroId, "leader summons himself")) $
    return ()

-- TODO: merge with summonHeroes; disregard "spawn" and "playable" factions and "spawn" flags for monsters; only check 'summon"
summonMonsters :: MonadServer m => Int -> Point -> m ()
summonMonsters n pos = do
  faction <- getsGlobal sfaction
  Kind.COps{ cotile
           , coactor=Kind.Ops{opick, okind}
           , cofact=Kind.Ops{opick=fopick, oname=foname}} <- getsGlobal scops
  spawnKindId <- rndToAction $ fopick "playable" (\k -> fspawn k > 0)
  -- Spawn frequency required greater than zero, but otherwise ignored.
  let inFactionKind m = isJust $ lookup (foname spawnKindId) (afreq m)
  -- Summon frequency used for picking the actor.
  mk <- rndToAction $ opick "summon" inFactionKind
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  let bfaction = fst $ fromJust
                 $ find (\(_, fa) -> gkind fa == spawnKindId)
                 $ IM.toList faction
  s <- getGlobal
  ser <- getServer
  let (sN, serN) = iterate (uncurry $ addMonster cotile mk hp pos
                                                 bfaction False) (s, ser) !! n
  putGlobal sN
  putServer serN

-- | Remove a dead actor. Check if game over.
checkPartyDeath :: MonadServerChan m => ActorId -> m ()
checkPartyDeath target = do
  pbody <- getsGlobal $ getActorBody target
  Config{configFirstDeathEnds} <- getsServer sconfig
  -- Place the actor's possessions on the map.
  bitems <- getsGlobal (getActorItem target)
  modifyGlobal $ updateArena $ dropItemsAt bitems $ bpos pbody
  let fid = bfaction pbody
      animateDeath = do
        sendToPlayers [bpos pbody] (AnimateDeathCli target)
        sendQueryCli fid $ CarryOnCli
      animateGameOver = do
        go <- animateDeath
        modifyGlobal $ updateActorBody target $ \b -> b {bsymbol = Just '%'}
        -- TODO: add side argument to gameOver instead
        side <- getsGlobal sside
        switchGlobalSelectedSide fid
        gameOver go
        switchGlobalSelectedSide side
  isSpawning <- getsGlobal $ flip isSpawningFaction fid
  -- For a swapning faction, no messages nor animations are shown below.
  -- A message was shown in @eff@ and that's enough.
  when (not isSpawning) $ do
    if configFirstDeathEnds
      then animateGameOver
      else void $ animateDeath
  -- Remove the dead actor.
  modifyGlobal $ deleteActor target
  -- We don't register that the lethal potion on the floor
  -- is used up. If that's a problem, add a one turn delay
  -- to the effect of all lethal objects (displaying an "agh! dying"
  -- message). Other factions do register that the actor (and potion)
  -- is gone, because the level is not switched and so perception
  -- is recorded for this level.

-- | End game, showing the ending screens, if requested.
gameOver :: MonadServerChan m => Bool -> m ()
gameOver showEndingScreens = do
  arena <- getsGlobal sarena
  let upd f = f {gquit = Just (False, Killed arena)}
  modifyGlobal $ updateSide upd
  when showEndingScreens $ do
    Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsGlobal scops
    s <- getGlobal
    depth <- getsGlobal sdepth
    time <- getsGlobal getTime
    let (items, total) = calculateTotal s
        deepest = levelNumber arena  -- use deepest visited instead of level of death
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
      then do
        let upd2 f = f {gquit = Just (True, Killed arena)}
        modifyGlobal $ updateSide upd2
      else do
        discoS <- getsGlobal sdisco
        side <- getsGlobal sside
        go <- sendQueryCli side $ ConfirmShowItemsCli discoS loseMsg items
        when go $ do
          let upd2 f = f {gquit = Just (True, Killed arena)}
          modifyGlobal $ updateSide upd2
