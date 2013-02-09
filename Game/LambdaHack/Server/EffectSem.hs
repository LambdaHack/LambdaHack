{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Effect semantics.
-- TODO: document
module Game.LambdaHack.Server.EffectSem where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, tell)
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
-- The second argument is verbosity of the resulting message.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
effectSem :: MonadServerChan m
          => Effect.Effect -> Int -> ActorId -> ActorId -> Int -> Bool
          -> WriterT [CmdAtomic] m Bool
effectSem ef verbosity source target power block = do
  oldS <- getsState (getActorBody source)
  oldT <- getsState (getActorBody target)
  let oldHP = bhp oldT
  (b, msg) <- eff ef verbosity source target power
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

-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified. This function
-- is mutually recursive with @effect@, hence it's a part of @Effect@
-- semantics.
itemEffect :: MonadServerChan m
           => Int -> ActorId -> ActorId -> Item -> Bool
           -> WriterT [CmdAtomic] m ()
itemEffect verbosity source target item block = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  sm <- getsState $ getActorBody source
  -- Destroys attacking actor: a hack for projectiles.
  when (bproj sm) $ tell [KillAtomic source sm]
  discoS <- getsState sdisco
  let ef = ieffect $ okind $ fromJust $ jkind discoS item
  -- The msg describes the target part of the action.
  b <- effectSem ef verbosity source target (jpower item) block
  -- The effect is interesting so the item gets identified, if seen.
  when b $ discover discoS item

-- TODO: send to all clients that see tpos.
-- | Make the item known to the leader.
discover :: MonadServerChan m => Discoveries -> Item -> m ()
discover discoS i = do
  let ix = jkindIx i
      ik = discoS EM.! ix
-- TODO: send to all clients that see tpos.
  sendUpdateCli undefined $ DiscoverCli ik i

-- | The boolean part of the result says if the ation was interesting
-- and the string part describes how the target reacted
-- (not what the source did).
eff :: MonadServerChan m
    => Effect.Effect -> Int -> ActorId -> ActorId -> Int
    -> WriterT [CmdAtomic] m (Bool, Text)
eff Effect.NoEffect _ _ _ _ = effectNoEffect
eff Effect.Heal _ _ target power = effectHeal target power
eff (Effect.Wound nDm) verbosity source target power =
  effectWound nDm verbosity source target power
eff Effect.Dominate _ source target _ =
  effectDominate source target
eff Effect.SummonFriend _ source target power =
  effectSummonFriend source target power
eff Effect.SpawnMonster _ _ target power =
  effectSpawnMonster target power
eff Effect.CreateItem _ _ target power =
  effectCreateItem target power
eff Effect.ApplyPerfume _ source target _ =
  effectApplyPerfume source target
eff Effect.Regeneration verbosity source target power =
  eff Effect.Heal verbosity source target power
eff Effect.Searching _ _source _target _power =
  return (True, "It gets lost and you search in vain.")
eff Effect.Ascend _ _ target power = effectAscend target power
eff Effect.Descend _ _ target power = effectDescend target power

-- + Individual semantic functions for effects

-- ** NoEffect

effectNoEffect :: MonadActionAbort m => m (Bool, Text)
effectNoEffect = return (False, "Nothing happens.")

-- ** Heal

effectHeal :: MonadServer m
           => ActorId -> Int -> WriterT [CmdAtomic] m (Bool, Text)
effectHeal target power = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsState scops
  tm <- getsState (getActorBody target)
  let bhpMax = maxDice (ahp $ okind $ bkind tm)
  if bhp tm >= bhpMax || power <= 0
    then effectNoEffect
    else do
      void $ focusIfOurs target
      let deltaHP = min power (bhpMax - bhp tm)
      tell [HealAtomic deltaHP target]
      return (True, actorVerb coactor tm "feel better")

-- ** Wound

effectWound :: MonadServer m
            => RollDice -> Int -> ActorId -> ActorId -> Int
            -> WriterT [CmdAtomic] m (Bool, Text)
effectWound nDm verbosity source target power = do
  Kind.COps{coactor} <- getsState scops
  isProjTarget <- getsState $ flip isProjectile target
  n <- rndToAction $ rollDice nDm
  let deltaHP = - (n + power)
  if deltaHP >= 0 then effectNoEffect else do
    void $ focusIfOurs target
    tm <- getsState (getActorBody target)
    isSpawning <- getsState $ flip isSpawningFaction $ bfaction tm
    let msg
          | bhp tm + deltaHP <= 0 =
            if isSpawning
            then ""  -- Handled later on in checkPartyDeath. Suspense.
            else -- Not as important, so let the player read the message
                 -- about monster death while he watches the combat animation.
              if isProjTarget
              then actorVerb coactor tm "drop down"
              else actorVerb coactor tm "die"
          | source == target =  -- a potion of wounding, etc.
            actorVerb coactor tm "feel wounded"
          | verbosity <= 0 = ""
          | isSpawning =
            actorVerb coactor tm $ "lose" <+> showT (n + power) <> "HP"
          | otherwise = actorVerb coactor tm "hiss in pain"
    -- Damage the target.
    tell [HealAtomic deltaHP target]
    return (True, msg)

-- ** Dominate

effectDominate :: MonadServerChan m
               => ActorId -> ActorId -> WriterT [CmdAtomic] m (Bool, Text)
effectDominate source target = do
  sm <- getsState (getActorBody source)
  arena <- getsState sarena
  if source == target
    then do
      genemy <- getsState $ genemy . (EM.! bfaction sm) . sfaction
      lxsize <- getsState (lxsize . getArena)
      lysize <- getsState (lysize . getArena)
      lvl <- getsState getArena
      let lm = actorNotProjList (`elem` genemy) lvl
          cross m = bpos m : vicinityCardinal lxsize lysize (bpos m)
          vis = ES.fromList $ concatMap cross lm
      sendUpdateCli (bfaction sm) $ RemCli vis lvl
      return (True, "A dozen voices yells in anger.")
    else do
      Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
      tm@Actor{bspeed, bkind} <- getsState (getActorBody target)
      -- Halve the speed as a side-effect of domination.
      let speed = fromMaybe (aspeed $ okind bkind) bspeed
          delta = speedScale (1%2) speed
      when (delta > speedZero) $ tell [HasteAtomic target (speedNegate delta)]
      -- TODO: Perhaps insert a turn of delay here to allow countermeasures.
      tell [DominateAtomic (bfaction tm) (bfaction sm) target]
      sendQueryCli (bfaction sm) (SelectLeaderCli target arena)
        >>= assert `trueM` (arena, target, "leader dominates himself")
      -- Display status line and FOV for the new actor.
--TODO      sli <- promptToSlideshow ""
--      fr <- drawOverlay ColorBW $ head $ runSlideshow sli
--      displayFramesPush [Nothing, Just fr, Nothing]
      return (True, "")

-- ** SummonFriend

effectSummonFriend :: MonadServer m
                   => ActorId -> ActorId -> Int
                   -> WriterT [CmdAtomic] m (Bool, Text)
effectSummonFriend source target power = do
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  summonFriends (bfaction sm) (1 + power) (bpos tm)
  return (True, "")

summonFriends :: MonadServer m
              => FactionId -> Int -> Point -> WriterT [CmdAtomic] m ()
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

addActor :: MonadServer m
         => Kind.Id ActorKind -> FactionId -> Point -> Int
         -> Maybe Char -> Maybe Text
         -> WriterT [CmdAtomic] m ()
addActor mk bfaction ppos hp msymbol mname = do
  Kind.COps{cotile} <- getsState scops
  time <- getsState getTime
  pos <- getsState $ nearbyFreePos cotile ppos
  let m = actorTemplate mk msymbol mname hp pos time bfaction False
  acounter <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ acounter}
  tell [SpawnAtomic acounter m]

-- TODO: apply this special treatment only to actors with symbol '@'.
-- | Create a new hero on the current level, close to the given position.
addHero :: MonadServer m
        => FactionId -> Point -> [(Int, Text)] -> WriterT [CmdAtomic] m ()
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

-- ** SpawnMonster

effectSpawnMonster :: MonadServer m
                   => ActorId -> Int -> WriterT [CmdAtomic] m (Bool, Text)
effectSpawnMonster target power = do
  tm <- getsState (getActorBody target)
  void $ spawnMonsters (1 + power) (bpos tm)
  return (True, "")

-- | Spawn monsters of any spawning faction, friendly or not.
-- To be used for spontaneous spawning of monsters and for the spawning effect.
spawnMonsters :: MonadServer m
              => Int -> Point -> WriterT [CmdAtomic] m (Maybe FactionId)
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

-- | Create a new monster on the level, at a given position
-- and with a given actor kind and HP.
addMonster :: MonadServer m
           => Kind.Id ActorKind -> FactionId -> Point
           -> WriterT [CmdAtomic] m ()
addMonster mk bfaction ppos = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  addActor mk bfaction ppos hp Nothing Nothing

-- ** CreateItem

effectCreateItem :: MonadServer m
                 => ActorId -> Int -> WriterT [CmdAtomic] m (Bool, Text)
effectCreateItem target power = do
  tm <- getsState (getActorBody target)
  void $ createItems (1 + power) (bpos tm)
  return (True, "")

createItems :: MonadServer m => Int -> Point -> WriterT [CmdAtomic] m ()
createItems n pos = do
  Kind.COps{coitem} <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  ldepth <- getsState $ ldepth . getArena
  depth <- getsState sdepth
  replicateM_ n $ do
    (item, k, _) <- rndToAction $ newItem coitem flavour discoRev ldepth depth
    itemRev <- getsServer sitemRev
    case HM.lookup item itemRev of
      Just iid ->
        -- TODO: try to avoid this case, to make items more interesting
        tell [CreateItemAtomic iid item k (CFloor pos)]
      Nothing -> do
        icounter <- getsServer sicounter
        modifyServer $ \ser ->
          ser { sicounter = succ icounter
              , sitemRev = HM.insert item icounter (sitemRev ser)}
        tell [CreateItemAtomic icounter item k (CFloor pos)]

-- ** ApplyPerfume

effectApplyPerfume :: MonadActionRO m
                   => ActorId -> ActorId -> WriterT [CmdAtomic] m (Bool, Text)
effectApplyPerfume source target =
  if source == target
  then return (True, "Tastes like water, but with a strong rose scent.")
  else do
    oldSmell <- getsState $ lsmell . getArena
    let diffL = map (\(p, sm) -> (p, (Just sm, Nothing))) $ EM.assocs oldSmell
    tell [AlterSmellAtomic diffL]
    return (True, "The fragrance quells all scents in the vicinity.")

-- ** Regeneration

-- ** Searching

-- ** Ascend

effectAscend :: MonadServerChan m
             => ActorId -> Int -> WriterT [CmdAtomic] m (Bool, Text)
effectAscend target power = useStairs target (power + 1) "find a way upstairs"

useStairs :: MonadServerChan m
          => ActorId -> Int -> Msg -> WriterT [CmdAtomic] m (Bool, Text)
useStairs target delta msg = do
  Kind.COps{coactor} <- getsState scops
  tm <- getsState (getActorBody target)
  void $ focusIfOurs target
  effLvlGoUp target delta
  gquit <- getsState $ gquit . (EM.! bfaction tm) . sfaction
  return $ if maybe Camping snd gquit == Victor
           then (True, "")
           else (True, actorVerb coactor tm msg)

effLvlGoUp :: MonadServerChan m => ActorId -> Int -> WriterT [CmdAtomic] m ()
effLvlGoUp aid k = do
  pbodyCurrent <- getsState $ getActorBody aid
  arena <- getsState sarena
  glo <- getState
  case whereTo glo arena k of
    Nothing -> fleeDungeon (bfaction pbodyCurrent)
               -- We are at the "end" of the dungeon.
    Just (nln, npos) ->
      assert (nln /= arena `blame` (nln, "stairs looped")) $ do
        timeCurrent <- getsState getTime
        -- Remove the actor from the old level.
        tell [KillAtomic aid pbodyCurrent]
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
        timeLastVisited <- getsState getTime
        let diff = timeAdd (btime pbodyCurrent) (timeNegate timeCurrent)
            pbody = pbodyCurrent { btime = timeAdd timeLastVisited diff
                                 , bpos = npos }
        -- The actor is added to the new level, but there can be other actors
        -- at his old position or at his new position.
        tell [SpawnAtomic aid pbody]
        -- Reset level and leader for all factions.
        broadcastCli [return . (/= bfaction pbody)] $ InvalidateArenaCli nln
--        sendUpdateCli (bfaction pbody) $ SwitchLevelCli aid nln pbody bbag
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
        -- The property of at most one actor on a tile is restored.
        -- Create a backup of the savegame.
        saveGameBkp

-- TODO: refactor with actorAttackActor or perhaps displace the other
-- actor or swap positions with it, instead of squashing.
squashActor :: MonadServerChan m
            => ActorId -> ActorId -> WriterT [CmdAtomic] m ()
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
  itemEffect 0 source target h2h False
  s <- getState
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (memActor target s) `blame` (source, target, "not killed")) $
    return ()

-- ** Descend

effectDescend :: MonadServerChan m
              => ActorId -> Int -> WriterT [CmdAtomic] m (Bool, Text)
effectDescend target power =
  useStairs target (- (power + 1)) "find a way downstairs"

-- * Assorted helper functions

-- | Sentences such as \"Dog barks loudly.\".
actorVerb :: Kind.Ops ActorKind -> Actor -> Text -> Text
actorVerb coactor a v =
  makeSentence [MU.SubjectVerbSg (partActor coactor a) (MU.Text v)]

-- TODO: center screen, flash the background, etc. Perhaps wait for SPACE.
-- | Focus on the hero being wounded/displaced/etc.
focusIfOurs :: MonadActionAbort m => ActorId -> m Bool
focusIfOurs _target = return True

-- TODO: right now it only works for human factions (sendQueryUI).
-- | The leader leaves the dungeon.
fleeDungeon :: MonadServerChan m => FactionId -> m ()
fleeDungeon fid = do
  Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
  glo <- getState
  go <- sendQueryUI fid $ ConfirmYesNoCli
          "This is the way out. Really leave now?"
  when (not go) $ abortWith "Game resumed."
  let (bag, total) = calculateTotal fid glo
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
dropAllItems :: MonadActionRO m => ActorId -> WriterT [CmdAtomic] m ()
dropAllItems aid = do
  pos <- getsState $ bpos . getActorBody aid
  bag <- getsState $ getActorBag aid
  let f (iid, k) = tell [MoveItemAtomic iid k (CActor aid) (CFloor pos)]
  mapM_ f $ EM.assocs bag

-- | Remove a dead actor. Check if game over.
checkPartyDeath :: MonadServerChan m => ActorId -> WriterT [CmdAtomic] m ()
checkPartyDeath target = do
  tm <- getsState $ getActorBody target
  Config{configFirstDeathEnds} <- getsServer sconfig
  -- Place the actor's possessions on the map.
  dropAllItems target
  let fid = bfaction tm
  isHuman <- getsState $ flip isHumanFaction fid
  let animateDeath = do
        broadcastPosUI [bpos tm] (AnimateDeathCli target)
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
  tell [KillAtomic target tm]
  -- We don't register that the lethal potion on the floor
  -- is used up. If that's a problem, add a one turn delay
  -- to the effect of all lethal objects (displaying an "agh! dying"
  -- message). Other factions do register that the actor (and potion)
  -- is gone, because the level is not switched and so perception
  -- is recorded for this level.

-- | End game, showing the ending screens, if requested.
gameOver :: (MonadAction m, MonadServerChan m) => FactionId -> Bool -> m ()
gameOver fid showEndingScreens = do
  arena <- getsState sarena
  deepest <- getsState $ ldepth . getArena  -- TODO: use deepest visited instead of current
  let upd f = f {gquit = Just (False, Killed arena)}
  modifyState $ updateFaction (EM.adjust upd fid)
  when showEndingScreens $ do
    Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
    s <- getState
    depth <- getsState sdepth
    time <- getsState getTime
    let (bag, total) = calculateTotal fid s
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
