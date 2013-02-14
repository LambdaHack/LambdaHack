{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Effect semantics.
-- TODO: document
module Game.LambdaHack.Server.EffectSem where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Ratio ((%))
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdAtomic
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
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency

default (Text)

-- + Semantics of effects

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
effectSem :: MonadServerChan m
          => Effect.Effect -> ActorId -> ActorId -> Int
          -> WriterT [Atomic] m Bool
effectSem ef source target power = do
  tb <- getsState $ getActorBody target
  -- We assume if targed moved to a level, which is not the current level,
  -- his HP is unchanged.
  let oldHP = bhp tb
      arena = blid tb
  b <- eff ef source target power
  memTm <- getsState $ memActor target arena
  newHP <- if memTm
           then getsState $ bhp . getActorBody target
           else return oldHP
  when (newHP <= 0) $ checkPartyDeath target
  return b

-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified. This function
-- is mutually recursive with @effect@ and so it's a part of @Effect@
-- semantics.
itemEffect :: MonadServerChan m
           => ActorId -> ActorId -> Item
           -> WriterT [Atomic] m ()
itemEffect source target item = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  sm <- getsState $ getActorBody source
  -- Destroys attacking actor: a hack for projectiles.
  when (bproj sm) $ tellCmdAtomic $ DestroyActorA source sm
  discoS <- getsState sdisco
  let ef = ieffect $ okind $ fromJust $ jkind discoS item
  b <- effectSem ef source target (jpower item)
  -- The effect is interesting so the item gets identified, if seen.
  when b $ discover discoS item

-- TODO: send to all clients that see tpos.
-- | Make the item known to the leader.
discover :: MonadServerChan m => Discoveries -> Item -> m ()
discover discoS i = do
  let ix = jkindIx i
      _ik = discoS EM.! ix
  return ()
-- TODO: send to all clients that see tpos.
--  sendUpdateCli undefined $ DiscoverCli ik i

-- | The boolean part of the result says if the ation was interesting
-- and the string part describes how the target reacted
-- (not what the source did).
eff :: MonadServerChan m
    => Effect.Effect -> ActorId -> ActorId -> Int
    -> WriterT [Atomic] m Bool
eff effect source target power = case effect of
  Effect.NoEffect -> effectNoEffect
  Effect.Heal -> effectHeal target power
  Effect.Wound nDm -> effectWound nDm source target power
  Effect.Mindprobe -> effectMindprobe target
  Effect.Dominate | source /= target -> effectDominate source target
  Effect.Dominate -> eff Effect.Mindprobe source target power
  Effect.SummonFriend -> effectSummonFriend source target power
  Effect.SpawnMonster -> effectSpawnMonster target power
  Effect.CreateItem -> effectCreateItem target power
  Effect.ApplyPerfume -> effectApplyPerfume source target
  Effect.Regeneration -> eff Effect.Heal source target power
  Effect.Searching -> effectSearching source
  Effect.Ascend -> effectAscend target power
  Effect.Descend -> effectDescend target power

-- + Individual semantic functions for effects

-- ** NoEffect

effectNoEffect :: MonadActionAbort m => m Bool
effectNoEffect = return False

-- ** Heal

effectHeal :: MonadServer m
           => ActorId -> Int -> WriterT [Atomic] m Bool
effectHeal target power = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  tm <- getsState $ getActorBody target
  let bhpMax = maxDice (ahp $ okind $ bkind tm)
  if bhp tm >= bhpMax || power <= 0
    then effectNoEffect
    else do
      let deltaHP = min power (bhpMax - bhp tm)
      tellCmdAtomic $ HealActorA target deltaHP
      tellDescAtomic $ EffectA target Effect.Heal
      return True

-- ** Wound

effectWound :: MonadServer m
            => RollDice -> ActorId -> ActorId -> Int
            -> WriterT [Atomic] m Bool
effectWound nDm source target power = do
  n <- rndToAction $ rollDice nDm
  let deltaHP = - (n + power)
  if deltaHP >= 0
    then effectNoEffect
    else do
      -- Damage the target.
      tellCmdAtomic $ HealActorA target deltaHP
      when (source == target) $
        tellDescAtomic $ EffectA source $ Effect.Wound nDm
      return True

-- ** Mindprobe

effectMindprobe :: MonadServerChan m
                => ActorId -> WriterT [Atomic] m Bool
effectMindprobe target = do
  tb <- getsState (getActorBody target)
  let arena = blid tb
  genemy <- getsState $ genemy . (EM.! bfaction tb) . sfaction
  lxsize <- getsLevel arena lxsize
  lysize <- getsLevel arena lysize
  lvl <- getsLevel arena id
  lb <- getsState $ actorNotProjList (`elem` genemy) arena
  let cross b = bpos b : vicinityCardinal lxsize lysize (bpos b)
      vis = ES.fromList $ concatMap cross lb
  sendUpdateCli (bfaction tb) $ RemCli vis lvl arena
  tellDescAtomic $ EffectA target Effect.Mindprobe
  return True

-- ** Dominate

effectDominate :: MonadServerChan m
               => ActorId -> ActorId -> WriterT [Atomic] m Bool
effectDominate source target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  sm <- getsState (getActorBody source)
  tm@Actor{bspeed, bkind} <- getsState (getActorBody target)
  -- Halve the speed as a side-effect of domination.
  let speed = fromMaybe (aspeed $ okind bkind) bspeed
      delta = speedScale (1%2) speed
  when (delta > speedZero) $
    tellCmdAtomic $ HasteActorA target (speedNegate delta)
  -- TODO: Perhaps insert a turn of delay here to allow countermeasures.
  tellCmdAtomic $ DominateActorA target (bfaction tm) (bfaction sm)
  leaderOld <- getsState $ gleader . (EM.! bfaction sm) . sfaction
  tellCmdAtomic $ LeadFactionA (bfaction sm) leaderOld (Just target)
  return True

-- ** SummonFriend

effectSummonFriend :: MonadServer m
                   => ActorId -> ActorId -> Int
                   -> WriterT [Atomic] m Bool
effectSummonFriend source target power = do
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  summonFriends (bfaction sm) (1 + power) (bpos tm) (blid tm)
  return True

summonFriends :: MonadServer m
              => FactionId -> Int -> Point -> LevelId
              -> WriterT [Atomic] m ()
summonFriends bfaction n pos arena = assert (n > 0) $ do
  Kind.COps{ coactor=coactor@Kind.Ops{opick}
           , cofact=Kind.Ops{okind} } <- getsState scops
  faction <- getsState sfaction
  let fact = okind $ gkind $ faction EM.! bfaction
  replicateM_ n $ do
    mk <- rndToAction $ opick (fname fact) (const True)
    if mk == heroKindId coactor
      then addHero bfaction pos arena []
      else addMonster mk bfaction pos arena

addActor :: MonadServer m
         => Kind.Id ActorKind -> FactionId -> Point -> LevelId -> Int
         -> Maybe Char -> Maybe Text
         -> WriterT [Atomic] m ()
addActor mk bfaction ppos lid hp msymbol mname = do
  Kind.COps{cotile} <- getsState scops
  time <- getsState $ getTime lid
  pos <- getsState $ nearbyFreePos cotile ppos lid
  let m = actorTemplate mk msymbol mname hp pos lid time bfaction False
  acounter <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ acounter}
  tellCmdAtomic $ CreateActorA acounter m
  mleader <- getsState $ gleader . (EM.! bfaction) . sfaction
  when (mleader == Nothing) $
    tellCmdAtomic $ LeadFactionA bfaction Nothing (Just acounter)

-- TODO: apply this special treatment only to actors with symbol '@'.
-- | Create a new hero on the current level, close to the given position.
addHero :: MonadServer m
        => FactionId -> Point -> LevelId -> [(Int, Text)]
        -> WriterT [Atomic] m ()
addHero bfaction ppos arena configHeroNames = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsState scops
  let mk = heroKindId coactor
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  mhs <- mapM (\n -> getsState $ \s -> tryFindHeroK s bfaction n) [0..9]
  let freeHeroK = elemIndex Nothing mhs
      n = fromMaybe 100 freeHeroK
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      name = findHeroName configHeroNames n
      startHP = hp - (hp `div` 5) * min 3 n
  addActor mk bfaction ppos arena startHP (Just symbol) (Just name)

-- | Find a hero name in the config file, or create a stock name.
findHeroName :: [(Int, Text)] -> Int -> Text
findHeroName configHeroNames n =
  let heroName = lookup n configHeroNames
  in fromMaybe ("hero number" <+> showT n) heroName

-- ** SpawnMonster

effectSpawnMonster :: MonadServer m
                   => ActorId -> Int -> WriterT [Atomic] m Bool
effectSpawnMonster target power = do
  tm <- getsState (getActorBody target)
  void $ spawnMonsters (1 + power) (bpos tm) (blid tm)
  return True

-- | Spawn monsters of any spawning faction, friendly or not.
-- To be used for spontaneous spawning of monsters and for the spawning effect.
spawnMonsters :: MonadServer m
              => Int -> Point -> LevelId
              -> WriterT [Atomic] m (Maybe FactionId)
spawnMonsters n pos arena = do
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
        addMonster mk bfaction pos arena
      return $ Just bfaction

-- | Create a new monster on the level, at a given position
-- and with a given actor kind and HP.
addMonster :: MonadServer m
           => Kind.Id ActorKind -> FactionId -> Point -> LevelId
           -> WriterT [Atomic] m ()
addMonster mk bfaction ppos lid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  addActor mk bfaction ppos lid hp Nothing Nothing

-- ** CreateItem

effectCreateItem :: MonadServer m
                 => ActorId -> Int -> WriterT [Atomic] m Bool
effectCreateItem target power = do
  tm <- getsState $ getActorBody target
  void $ createItems (1 + power) (bpos tm) (blid tm)
  return True

createItems :: MonadServer m
            => Int -> Point -> LevelId -> WriterT [Atomic] m ()
createItems n pos lid = do
  Kind.COps{coitem} <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  ldepth <- getsLevel lid ldepth
  depth <- getsState sdepth
  replicateM_ n $ do
    (item, k, _) <- rndToAction $ newItem coitem flavour discoRev ldepth depth
    itemRev <- getsServer sitemRev
    case HM.lookup item itemRev of
      Just iid ->
        -- TODO: try to avoid this case, to make items more interesting
        tellCmdAtomic $ CreateItemA lid iid item k (CFloor pos)
      Nothing -> do
        icounter <- getsServer sicounter
        modifyServer $ \ser ->
          ser { sicounter = succ icounter
              , sitemRev = HM.insert item icounter (sitemRev ser)}
        tellCmdAtomic $ CreateItemA lid icounter item k (CFloor pos)

-- ** ApplyPerfume

effectApplyPerfume :: MonadActionRO m
                   => ActorId -> ActorId -> WriterT [Atomic] m Bool
effectApplyPerfume source target =
  if source == target
  then return False
  else do
    tm <- getsState $ getActorBody target
    oldSmell <- getsLevel (blid tm) lsmell
    let diffL = map (\(p, sm) -> (p, (Just sm, Nothing))) $ EM.assocs oldSmell
    tellCmdAtomic $ AlterSmellA (blid tm) diffL
    tellDescAtomic $ EffectA target Effect.ApplyPerfume
    return True

-- ** Regeneration

-- ** Searching

effectSearching :: MonadActionAbort m => ActorId -> WriterT [Atomic] m Bool
effectSearching source = do
  tellDescAtomic $ EffectA source Effect.Searching
  return True

-- ** Ascend

effectAscend :: MonadServerChan m
             => ActorId -> Int -> WriterT [Atomic] m Bool
effectAscend target power = do
  effLvlGoUp target (power + 1)
  tellDescAtomic $ EffectA target Effect.Ascend
  return True

effLvlGoUp :: MonadServerChan m => ActorId -> Int -> WriterT [Atomic] m ()
effLvlGoUp aid k = do
  pbodyCurrent <- getsState $ getActorBody aid
  let arena = blid pbodyCurrent
  glo <- getState
  case whereTo glo arena k of
    Nothing -> fleeDungeon (bfaction pbodyCurrent) (blid pbodyCurrent)
               -- We are at the "end" of the dungeon.
    Just (nln, npos) ->
      assert (nln /= arena `blame` (nln, "stairs looped")) $ do
        timeCurrent <- getsState $ getTime arena
        -- Remove the actor from the old level.
        tellCmdAtomic $ DestroyActorA aid pbodyCurrent
        -- Remember the level (e.g., when teleporting via scroll on the floor,
        -- register the scroll vanished, also let the other factions register
        -- the actor vanished in case they switch to this level from another
        -- level). Perception is unchanged, so for one turn (this level turn)
        -- there will be visibility left on the old actor location.
        -- remember
        -- TODO: wipe out smell on save instead, based on timeLastVisited
        -- -- Only spawning factions left on the level, so no new smell generated.
        -- -- Cancel smell. Reduces memory load and savefile size.
        -- hs <- getsState $ actorList (not . isSpawningFaction glo) . getArena
        -- when (null hs) $ do
        --   oldSmell <- getsState $ lsmell . getArena
        --   setSmellAtomic oldSmell EM.empty
        -- Change arena, but not the leader yet. The actor will become
        -- a leader, but he is not inserted into the new level yet.
-- TODO:        modifyState $ updateSelectedArena nln
        -- Sync the actor time with the level time.
        timeLastVisited <- getsState $ getTime arena
        let diff = timeAdd (btime pbodyCurrent) (timeNegate timeCurrent)
            pbody = pbodyCurrent { btime = timeAdd timeLastVisited diff
                                 , bpos = npos }
        -- The actor is added to the new level, but there can be other actors
        -- at his old position or at his new position.
        tellCmdAtomic $ CreateActorA aid pbody
        -- Checking actors at the new posiiton of the aid.
        inhabitants <- getsState (posToActor npos arena)
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
        inhabitants2 <- getsState (posToActor npos arena)
        when (isJust inhabitants2) $ assert `failure` inhabitants2
        -- The property of at most one actor on a tile is restored.
        -- Create a backup of the savegame.
        saveGameBkp

-- TODO: refactor with actorAttackActor or perhaps displace the other
-- actor or swap positions with it, instead of squashing.
squashActor :: MonadServerChan m
            => ActorId -> ActorId -> WriterT [Atomic] m ()
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
  itemEffect source target h2h
  s <- getState
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (target `EM.member` sactorD s) `blame` (source, target, "not killed")) $
    return ()

-- ** Descend

effectDescend :: MonadServerChan m
              => ActorId -> Int -> WriterT [Atomic] m Bool
effectDescend target power = do
  effLvlGoUp target (- (power + 1))
  tellDescAtomic $ EffectA target Effect.Descend
  return True

-- * Assorted helper functions

-- TODO: right now it only works for human factions (sendQueryUI).
-- | The leader leaves the dungeon.
fleeDungeon :: MonadServerChan m => FactionId -> LevelId -> m ()
fleeDungeon fid lid = do
  Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
  glo <- getState
  go <- sendQueryUI fid $ ConfirmYesNoCli
          "This is the way out. Really leave now?"
  when (not go) $ abortWith "Game resumed."
  let (bag, total) = calculateTotal fid lid glo
      _upd f = f {gquit = Just (False, Victor)}
-- TODO: move to StateServer?
  return ()
--  modifyState $ updateSide upd
  if total == 0
  then do
    -- The player can back off at each of these steps.
    go1 <- sendQueryUI fid $ ConfirmMoreBWCli
             "Afraid of the challenge? Leaving so soon and empty-handed?"
    when (not go1) $ abortWith "Brave soul!"
    go2 <- sendQueryUI fid $ ConfirmMoreBWCli
            "This time try to grab some loot before escape!"
    when (not go2) $ abortWith "Here's your chance!"
  else do
    let currencyName = MU.Text $ oname $ ouniqGroup "currency"
        winMsg = makeSentence
          [ "Congratulations, you won!"
          , "Here's your loot, worth"
          , MU.NWs total currencyName ]
    void $ sendQueryUI fid $ ConfirmShowItemsFloorCli winMsg bag
    let _upd2 f = f {gquit = Just (True, Victor)}
-- TODO: move to StateServer?
    return ()
--    modifyState $ updateSide upd2

-- | Drop all actor's items.
dropAllItems :: MonadActionRO m => ActorId -> WriterT [Atomic] m ()
dropAllItems aid = do
  b <- getsState $ getActorBody aid
  let f (iid, k) =
        tellCmdAtomic
        $ MoveItemA (blid b) iid k (actorContainer aid (binv b) iid)
                                   (CFloor $ bpos b)
  mapM_ f $ EM.assocs $ bbag b

-- | Remove a dead actor. Check if game over.
checkPartyDeath :: MonadServerChan m => ActorId -> WriterT [Atomic] m ()
checkPartyDeath target = do
  tm <- getsState $ getActorBody target
  Config{configFirstDeathEnds} <- getsServer sconfig
  -- Place the actor's possessions on the map.
  dropAllItems target
  let fid = bfaction tm
  isHuman <- getsState $ flip isHumanFaction fid
  let animateDeath = do
        broadcastPosUI [bpos tm] (blid tm) (AnimateDeathCli target)
        if isHuman then sendQueryUI fid CarryOnCli else return False
      animateGameOver = do
        _go <- animateDeath
-- TODO; perhaps set in clients instead
--        modifyState $ updateActorBody target $ \b -> b {bsymbol = Just '%'}
        -- TODO: add side argument to gameOver instead
--        side <- getsState sside
--        switchGlobalSelectedSide fid
-- TODO: perhaps move to loop
--        gameOver go
        return ()
--        switchGlobalSelectedSide side
  isSpawning <- getsState $ flip isSpawningFaction fid
  -- For a swapning faction, no messages nor animations are shown below.
  -- A message was shown in @eff@ and that's enough.
  when (not isSpawning) $ do
    if configFirstDeathEnds
      then animateGameOver
      else void $ animateDeath
  -- Remove the dead actor.
  tellCmdAtomic $ DestroyActorA target tm
  electLeader (bfaction tm) (blid tm)
  -- We don't register that the lethal potion on the floor
  -- is used up. If that's a problem, add a one turn delay
  -- to the effect of all lethal objects (displaying an "agh! dying"
  -- message). Other factions do register that the actor (and potion)
  -- is gone, because the level is not switched and so perception
  -- is recorded for this level.

electLeader :: MonadServer m => FactionId -> LevelId -> WriterT [Atomic] m ()
electLeader fid lid = do
  mleader <- getsState $ gleader . (EM.! fid) . sfaction
  when (isNothing mleader) $ do
    actorD <- getsState sactorD
    let party = filter (\(_, b) ->
                  bfaction b == fid && not (bproj b)) $ EM.assocs actorD
    when (not $ null $ party) $ do
      onLevel <- getsState $ actorNotProjAssocs (== fid) lid
      let leader = fst $ head $ onLevel ++ party
      tellCmdAtomic $ LeadFactionA fid Nothing (Just leader)

-- | End game, showing the ending screens, if requested.
gameOver :: (MonadAction m, MonadServerChan m)
         => FactionId -> LevelId -> Bool -> m ()
gameOver fid arena showEndingScreens = do
  deepest <- getsLevel arena ldepth  -- TODO: use deepest visited instead of current
  let upd f = f {gquit = Just (False, Killed arena)}
  modifyState $ updateFaction (EM.adjust upd fid)
  when showEndingScreens $ do
    Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
    s <- getState
    depth <- getsState sdepth
    time <- undefined  -- TODO: sum over all levels? getsState getTime
    let (bag, total) = calculateTotal fid arena s
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
        modifyState $ updateFaction (EM.adjust upd2 fid)
      else do
        -- TODO: do this for the killed factions, not for side
        go <- sendQueryUI fid $ ConfirmShowItemsFloorCli loseMsg bag
        when go $ do
          let upd2 f = f {gquit = Just (True, Killed arena)}
          modifyState $ updateFaction (EM.adjust upd2 fid)
