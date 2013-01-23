{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | The @effectToAction@ function and related operations.
-- TODO: document
module Game.LambdaHack.Server.EffectAction where

import Control.Monad
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
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind
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
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

default (Text)

-- | Sentences such as \"Dog barks loudly.\".
actorVerb :: Kind.Ops ActorKind -> Actor -> Text -> Text
actorVerb coactor a v =
  makeSentence [MU.SubjectVerbSg (partActor coactor a) (MU.Text v)]

-- | Create a new monster in the level, at a given position
-- and with a given actor kind and HP.
addMonster :: Kind.Ops TileKind -> Kind.Id ActorKind -> Int -> Point
           -> FactionId -> Bool -> State -> StateServer
           -> (State, StateServer)
addMonster cotile mk hp ppos bfaction bproj s ser@StateServer{scounter} =
  let loc = nearbyFreePos cotile ppos s
      m = template mk Nothing Nothing hp loc (getTime s) bfaction bproj
  in ( updateArena (updateActor (IM.insert scounter m)) s
     , ser {scounter = scounter + 1} )

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
  oldS <- getsState (getActorBody target)
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
  let bhpMax m = maxDice (ahp $ okind $ bkind m)
  tm <- getsState (getActorBody target)
  if bhp tm >= bhpMax tm || power <= 0
    then nullEffect
    else do
      void $ focusIfOurs target
      modifyState $ updateActorBody target (addHp coactor power)
      return (True, actorVerb coactor tm "feel better")
eff (Effect.Wound nDm) verbosity source target power = do
  Kind.COps{coactor} <- getsState scops
  s <- getState
  n <- rndToAction $ rollDice nDm
  if n + power <= 0 then nullEffect else do
    void $ focusIfOurs target
    tm <- getsState (getActorBody target)
    isSpawning <- getsState $ flip isSpawningFaction $ bfaction tm
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
    modifyState $ updateActorBody target $ \ m -> m { bhp = newHP }
    return (True, msg)
eff Effect.Dominate _ source target _power = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  s <- getState
  arena <- getsState sarena
  if source == target
    then do
      genemy <- getsState $ genemy . getSide
      lm <- getsState $ actorNotProjList (`elem` genemy) . getArena
      lxsize <- getsState (lxsize . getArena)
      lysize <- getsState (lysize . getArena)
      let cross m = bpos m : vicinityCardinal lxsize lysize (bpos m)
          vis = IS.fromList $ concatMap cross lm
      lvl <- getsState getArena
      side <- getsState sside
      sendUpdateCli side $ RememberCli arena vis lvl
      return (True, "A dozen voices yells in anger.")
    else do
      selectLeaderSer target arena
        >>= assert `trueM` (source, arena, target, "leader dominates himself")
      -- Halve the speed as a side-effect of domination.
      let halfSpeed :: Actor -> Maybe Speed
          halfSpeed Actor{bkind} =
            let speed = aspeed $ okind bkind
            in Just $ speedScale (1%2) speed
      -- Sync the monster with the hero move time for better display
      -- of missiles and for the domination to actually take one player's turn.
      modifyState $ updateActorBody target $ \ b -> b { bfaction = sside s
                                                       , btime = getTime s
                                                       , bspeed = halfSpeed b }
      -- Display status line and FOV for the new actor.
--TODO      sli <- promptToSlideshow ""
--      fr <- drawOverlay ColorBW $ head $ runSlideshow sli
--      displayFramesPush [Nothing, Just fr, Nothing]
      return (True, "")
eff Effect.SummonFriend _ source target power = do
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  s <- getState
  if isSpawningFaction s (bfaction sm)
    then summonMonsters (1 + power) (bpos tm)
    else summonHeroes (1 + power) (bpos tm)
  return (True, "")
eff Effect.SummonEnemy _ source target power = do
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  s  <- getState
  if isSpawningFaction s (bfaction sm)
    then summonHeroes (1 + power) (bpos tm)
    else summonMonsters (1 + power) (bpos tm)
  return (True, "")
eff Effect.ApplyPerfume _ source target _ =
  if source == target
  then return (True, "Tastes like water, but with a strong rose scent.")
  else do
    let upd lvl = lvl { lsmell = IM.empty }
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
  side <- getsState sside
  gquit <- getsState $ gquit . (IM.! side) . sfaction
  return $ if maybe Camping snd gquit == Victor
           then (True, "")
           else (True, actorVerb coactor tm "find a way upstairs")
eff Effect.Descend _ _ target power = do
  Kind.COps{coactor} <- getsState scops
  tm <- getsState (getActorBody target)
  void $ focusIfOurs target
  effLvlGoUp target (- (power + 1))
  side <- getsState sside
  gquit <- getsState $ gquit . (IM.! side) . sfaction
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
      h2h = buildItem flavour discoRev h2hKind kind 1 power
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
  bitems <- getsState $ getActorItem aid
  arena <- getsState sarena
  glo <- getState
  case whereTo glo arena k of
    Nothing -> fleeDungeon -- we are at the "end" of the dungeon
    Just (nln, npos) ->
      assert (nln /= arena `blame` (nln, "stairs looped")) $ do
        timeCurrent <- getsState getTime
        -- Remove the actor from the old level.
        modifyState (deleteActor aid)
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
          modifyState (updateArena (updateSmell (const IM.empty)))
        -- Change arena, but not the leader yet. The actor will become
        -- a leader, but he is not inserted into the new level yet.
        modifyState $ updateSelectedArena nln
        -- Sync the actor time with the level time.
        timeLastVisited <- getsState getTime
        let diff = timeAdd (btime pbodyCurrent) (timeNegate timeCurrent)
            pbody = pbodyCurrent {btime = timeAdd timeLastVisited diff}
        -- The actor is added to the new level, but there can be other actors
        -- at his old position or at his new position.
        modifyState (insertActor aid pbody)
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

-- | The leader leaves the dungeon.
fleeDungeon :: MonadServerChan m => m ()
fleeDungeon = do
  Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
  glo <- getState
  side <- getsState sside
  go <- sendQueryUI side $ ConfirmYesNoCli
          "This is the way out. Really leave now?"
  when (not go) $ abortWith "Game resumed."
  let (items, total) = calculateTotal glo
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
    sendUpdateUI side $ ShowItemsCli discoS winMsg items
    let upd2 f = f {gquit = Just (True, Victor)}
    modifyState $ updateSide upd2

-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified.
itemEffectAction :: MonadServerChan m
                 => Int -> ActorId -> ActorId -> Item -> Bool -> m ()
itemEffectAction verbosity source target item block = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  st <- getState
  -- Destroys attacking actor and its items: a hack for projectiles.
  when (isProjectile st source) $
    modifyState (deleteActor source)
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
      ik = discoS M.! ix
  sendUpdateCli side $ DiscoverCli ik i

selectLeaderSer :: MonadServerChan m => ActorId -> LevelId -> m Bool
selectLeaderSer actor lid = do
  side <- getsState sside
  b <- sendQueryCli side $ SelectLeaderCli actor lid
  modifyState $ updateSelectedArena lid
  return b

summonHeroes :: MonadServer m => Int -> Point -> m ()
summonHeroes n pos =
  assert (n > 0) $ do
  cops <- getsState scops
  newHeroId <- getsServer scounter
  s <- getState
  ser <- getServer
  side <- getsState sside
  let (sN, serN) = iterate (uncurry $ addHero cops pos side []) (s, ser) !! n
  putState sN
  putServer serN
  b <- focusIfOurs newHeroId
  assert (b `blame` (newHeroId, "leader summons himself")) $
    return ()

-- TODO: merge with summonHeroes; disregard "spawn" and "playable" factions and "spawn" flags for monsters; only check 'summon"
summonMonsters :: MonadServer m => Int -> Point -> m ()
summonMonsters n pos = do
  faction <- getsState sfaction
  Kind.COps{ cotile
           , coactor=Kind.Ops{opick, okind}
           , cofact=Kind.Ops{opick=fopick, oname=foname}} <- getsState scops
  spawnKindId <- rndToAction $ fopick "playable" (\k -> fspawn k > 0)
  -- Spawn frequency required greater than zero, but otherwise ignored.
  let inFactionKind m = isJust $ lookup (foname spawnKindId) (afreq m)
  -- Summon frequency used for picking the actor.
  mk <- rndToAction $ opick "summon" inFactionKind
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  let bfaction = fst $ fromJust
                 $ find (\(_, fa) -> gkind fa == spawnKindId)
                 $ IM.toList faction
  s <- getState
  ser <- getServer
  let (sN, serN) = iterate (uncurry $ addMonster cotile mk hp pos
                                                 bfaction False) (s, ser) !! n
  putState sN
  putServer serN

-- | Remove a dead actor. Check if game over.
checkPartyDeath :: MonadServerChan m => ActorId -> m ()
checkPartyDeath target = do
  pbody <- getsState $ getActorBody target
  Config{configFirstDeathEnds} <- getsServer sconfig
  -- Place the actor's possessions on the map.
  bitems <- getsState (getActorItem target)
  modifyState $ updateArena $ dropItemsAt bitems $ bpos pbody
  let fid = bfaction pbody
      animateDeath = do
        broadcastPosUI [bpos pbody] (AnimateDeathCli target)
        sendQueryUI fid $ CarryOnCli
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
  modifyState $ deleteActor target
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
  let upd f = f {gquit = Just (False, Killed arena)}
  modifyState $ updateSide upd
  when showEndingScreens $ do
    Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
    s <- getState
    depth <- getsState sdepth
    time <- getsState getTime
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
        modifyState $ updateSide upd2
      else do
        discoS <- getsState sdisco
        side <- getsState sside
        go <- sendQueryUI side $ ConfirmShowItemsCli discoS loseMsg items
        when go $ do
          let upd2 f = f {gquit = Just (True, Killed arena)}
          modifyState $ updateSide upd2
