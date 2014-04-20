{-# LANGUAGE TupleSections #-}
-- | Handle effects (most often caused by requests sent by clients).
module Game.LambdaHack.Server.HandleEffectServer
  ( itemEffect, effectSem
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Key (mapWithKeyM_)
import Data.Maybe
import Data.Ratio ((%))
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Dice as Dice
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemFeature as IF
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.CommonServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicServer
import Game.LambdaHack.Server.State

-- + Semantics of effects

-- TODO: when h2h items have ItemId, replace Item with ItemId
-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified. This function
-- is mutually recursive with @effect@ and so it's a part of @Effect@
-- semantics.
itemEffect :: (MonadAtomic m, MonadServer m)
           => ActorId -> ActorId -> ItemId -> Item -> CStore
           -> m ()
itemEffect source target iid item cstore = do
  postb <- getsState $ getActorBody source
  discoS <- getsServer sdisco
  let ik = fromJust $ jkind discoS item
      ef = jeffect item
      miidCstore = Just (iid, cstore)
  b <- effectSem ef source target miidCstore
  -- The effect is interesting so the item gets identified, if seen
  -- (the item was at the source actor's position, so his old position
  -- is given, since the actor and/or the item may be moved by the effect;
  -- we'd need to track not only position of atomic commands and factions,
  -- but also which items they relate to, to be fully accurate).
  when b $ execUpdAtomic $ UpdDiscover (blid postb) (bpos postb) iid ik

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
effectSem :: (MonadAtomic m, MonadServer m)
          => Effect.Effect Int -> ActorId -> ActorId -> Maybe (ItemId, CStore)
          -> m Bool
effectSem effect source target miidCstore = do
  sb <- getsState $ getActorBody source
  let execSfx = execSfxAtomic $ SfxEffect (bfid sb) target effect
  case effect of
    Effect.NoEffect -> effectNoEffect target
    Effect.Heal p -> effectHeal execSfx p source target
    Effect.Hurt nDm p -> effectHurt nDm p source target
    Effect.Haste p -> effectHaste execSfx p target
    Effect.Mindprobe _ -> effectMindprobe source target
    Effect.Dominate | source /= target -> effectDominate execSfx source target
    Effect.Dominate ->
      effectSem (Effect.Mindprobe undefined) source target miidCstore
    Effect.Impress -> effectImpress source target
    Effect.CallFriend p -> effectCallFriend p source target
    Effect.Summon p -> effectSummon p target
    Effect.CreateItem p -> effectCreateItem p target
    Effect.ApplyPerfume -> effectApplyPerfume execSfx source target
    Effect.Burn p -> effectBurn execSfx p source target miidCstore
    Effect.Blast p -> effectBlast execSfx p source target
    Effect.Regeneration p -> effectSem (Effect.Heal p) source target miidCstore
    Effect.Steadfastness p -> effectSteadfastness execSfx p target
    Effect.Ascend p -> effectAscend execSfx p target
    Effect.Escape{} -> effectEscape target

-- + Individual semantic functions for effects

-- ** NoEffect

effectNoEffect :: MonadAtomic m => ActorId -> m Bool
effectNoEffect target = do
  tb <- getsState $ getActorBody target
  -- FactionId is that of the target, as a simplification.
  execSfxAtomic $ SfxEffect (bfid tb) target Effect.NoEffect
  return False

-- ** Heal

effectHeal :: MonadAtomic m => m () -> Int -> ActorId -> ActorId -> m Bool
effectHeal execSfx power source target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  tb <- getsState $ getActorBody target
  let bhpMax = Dice.maxDice (ahp $ okind $ bkind tb)
      deltaHP = min power (max 0 $ bhpMax - bhp tb)
  if deltaHP == 0
    then effectNoEffect target
    else do
      execUpdAtomic $ UpdHealActor target deltaHP
      when (deltaHP < 0 && source /= target) $ halveCalm target
      execSfx
      return True

halveCalm :: MonadAtomic m => ActorId -> m ()
halveCalm target = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsState scops
  tb <- getsState $ getActorBody target
  let calmMax = Dice.maxDice $ acalm $ okind $ bkind tb
      calmUpperBound = if hpTooLow coactor tb
                       then 0  -- to trigger domination, etc.
                       else calmMax `div` 2
      deltaCalm = min (-2) (calmUpperBound - bcalm tb)
  -- HP loss decreases Calm by at least 2, to overcome Calm regen,
  -- when far from shooting foe and to avoid "hears something",
  -- which is emitted for decrease -1.
  execUpdAtomic $ UpdCalmActor target deltaCalm

-- ** Hurt

effectHurt :: (MonadAtomic m, MonadServer m)
            => Dice.Dice -> Int -> ActorId -> ActorId
            -> m Bool
effectHurt nDm power source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  n <- rndToAction $ castDice 0 0 nDm
  let block = braced tb && bhp tb > 0
      deltaHP = - max 1 ((n + power) `div` if block then 2 else 1)
  -- Damage the target.
  execUpdAtomic $ UpdHealActor target deltaHP
  when (source /= target) $ halveCalm target
  execSfxAtomic $ SfxEffect (bfid sb) target $
    if source == target
    then Effect.Heal deltaHP
    else Effect.Hurt nDm deltaHP{-hack-}
  return True

-- ** Haste

effectHaste :: MonadAtomic m => m () -> Int -> ActorId -> m Bool
effectHaste execSfx power target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  tb <- getsState $ getActorBody target
  let baseSpeed = aspeed $ okind $ bkind tb
      scaledSpeed = speedScale ((100 + fromIntegral power) % 100) baseSpeed
      incrSpeed = speedAdd scaledSpeed (speedNegate (bspeed tb))
      deltaSpeed = if (power > 0) == (incrSpeed > speedZero)
                   then incrSpeed
                   else speedZero
  if deltaSpeed == speedZero
    then effectNoEffect target
    else do
      execUpdAtomic $ UpdHasteActor target deltaSpeed
      execSfx
      return True

-- ** Mindprobe

effectMindprobe :: MonadAtomic m => ActorId -> ActorId -> m Bool
effectMindprobe source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let lid = blid tb
  fact <- getsState $ (EM.! bfid tb) . sfactionD
  lb <- getsState $ actorRegularList (isAtWar fact) lid
  let nEnemy = length lb
  if nEnemy == 0 || bproj tb then
    effectNoEffect target
  else do
    execSfxAtomic $ SfxEffect (bfid sb) target $ Effect.Mindprobe nEnemy
    return True

-- ** Dominate

effectDominate :: (MonadAtomic m, MonadServer m)
               => m () -> ActorId -> ActorId -> m Bool
effectDominate execSfx source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if bfid tb == bfid sb || bproj tb then
    effectNoEffect target
  else do
    execSfx
    dominateFid (bfid sb) target
    execSfx
    return True

-- ** Impress

effectImpress :: (MonadAtomic m, MonadServer m)
              => ActorId -> ActorId -> m Bool
effectImpress source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if boldfid tb == bfid sb || bproj tb then do
    -- TODO: Don't spam with shrapnel causing NoEffect and then uncomment.
    -- effectNoEffect target
    return False
  else do
    execSfxAtomic $ SfxEffect (bfid sb) target Effect.Impress
    execUpdAtomic $ UpdOldFidActor target (boldfid tb) (bfid sb)
    return True

-- ** SummonFriend

effectCallFriend :: (MonadAtomic m, MonadServer m)
                   => Int -> ActorId -> ActorId
                   -> m Bool
effectCallFriend power source target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  sb <- getsState (getActorBody source)
  tb <- getsState (getActorBody target)
  ps <- getsState $ nearbyFreePoints (const True) (bpos tb) (blid tb)
  summonFriends (bfid sb) (take power ps) (blid tb)
  return True

summonFriends :: (MonadAtomic m, MonadServer m)
              => FactionId -> [Point] -> LevelId
              -> m ()
summonFriends bfid ps lid = do
  cops@Kind.COps{ coactor=Kind.Ops{opick}
                , cofaction=Kind.Ops{okind} } <- getsState scops
  time <- getsState $ getLocalTime lid
  fact <- getsState $ (EM.! bfid) . sfactionD
  let fkind = okind $ gkind fact
  forM_ ps $ \p -> do
    let summonName = fname fkind
    mk <- rndToAction $ fmap (fromMaybe $ assert `failure` summonName)
                        $ opick summonName (const True)
    if isHeroFact cops fact
      then addHero bfid p lid [] Nothing time
      else addMonster mk bfid p lid time
  -- No leader election needed, bebause an alive actor of the same faction
  -- causes the effect, so there is already a leader.

-- ** SpawnMonster

effectSummon :: (MonadAtomic m, MonadServer m)
             => Int -> ActorId -> m Bool
effectSummon power target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  tb <- getsState (getActorBody target)
  ps <- getsState $ nearbyFreePoints (const True) (bpos tb) (blid tb)
  time <- getsState $ getLocalTime (blid tb)
  mfid <- pickFaction "summon" (const True)
  case mfid of
    Nothing -> return False  -- no faction summons
    Just fid -> do
      spawnMonsters (take power ps) (blid tb) time fid
      return True

-- | Roll a faction based on faction kind frequency key.
pickFaction :: MonadServer m
            => Text -> ((FactionId, Faction) -> Bool)
            -> m (Maybe FactionId)
pickFaction freqChoice ffilter = do
  Kind.COps{cofaction=Kind.Ops{okind}} <- getsState scops
  factionD <- getsState sfactionD
  let f (fid, fact) = let kind = okind (gkind fact)
                          g n = (n, fid)
                      in fmap g $ lookup freqChoice $ ffreq kind
      flist = mapMaybe f $ filter ffilter $ EM.assocs factionD
      freq = toFreq ("pickFaction" <+> freqChoice) flist
  if nullFreq freq then return Nothing
  else fmap Just $ rndToAction $ frequency freq

-- ** CreateItem

effectCreateItem :: (MonadAtomic m, MonadServer m)
                 => Int -> ActorId -> m Bool
effectCreateItem power target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  tb <- getsState $ getActorBody target
  void $ createItems power (bpos tb) (blid tb)
  return True

-- ** ApplyPerfume

effectApplyPerfume :: (MonadAtomic m, MonadServer m)
                   => m () -> ActorId -> ActorId -> m Bool
effectApplyPerfume execSfx source target =
  if source == target
  then effectImpress source target
  else do
    tb <- getsState $ getActorBody target
    Level{lsmell} <- getLevel $ blid tb
    let f p fromSm =
          execUpdAtomic $ UpdAlterSmell (blid tb) p (Just fromSm) Nothing
    mapWithKeyM_ f lsmell
    execSfx
    void $ effectImpress source target
    return True

-- ** Burn

effectBurn :: (MonadAtomic m, MonadServer m)
           => m () -> Int -> ActorId -> ActorId -> Maybe (ItemId, CStore)
           -> m Bool
effectBurn execSfx power source target miidCstore = do
  if source == target
  then case miidCstore of
    Just (iid, cstore) | cstore `elem` [CGround, CEqp] -> do
      -- Toggle on/off.
      Kind.COps{coitem=Kind.Ops{opick, okind}} <- getsState scops
      flavour <- getsServer sflavour
      discoRev <- getsServer sdiscoRev
      discoS <- getsServer sdisco
      item <- getsState $ getItemBody iid
      let ikOld = fromJust $ jkind discoS item
          kindOld = okind ikOld
          toChange (IF.ChangeTo group) = Just group
          toChange _ = Nothing
          groupsToChangeTo = mapMaybe toChange $ ifeature kindOld
      case groupsToChangeTo of
        [] -> effectNoEffect target
        group : _TODO -> do
          ikNew <- rndToAction $ fmap (fromMaybe $ assert `failure` group)
                                 $ opick group (const True)
          let kindNew = okind ikNew
              itemNew = buildItem flavour discoRev ikNew kindNew (jeffect item)
          bag <- getsState $ getActorBag target cstore
          let k = bag EM.! iid
              container = CActor target cstore
          execUpdAtomic $ UpdLoseItem iid item k container
          void $ registerItem itemNew k container True
          return False
    _ -> effectNoEffect target
  else do
    -- Damage from both impact and fire.
    void $ effectHurt 0 (2 * power) source target
    execSfx
    return True

-- ** Blast

effectBlast :: (MonadAtomic m, MonadServer m)
            => m () -> Int -> ActorId -> ActorId -> m Bool
effectBlast execSfx _power _source _target = do
  -- TODO: make target deaf: prevents Calm decrease through proximity,
  -- or makes it random or also doubles calm decrease through hits
  -- or calm can't get above half --- all depends if it's temporary or not
  execSfx
  return True

-- ** Regeneration

-- ** Steadfastness

effectSteadfastness :: MonadAtomic m => m () -> Int -> ActorId -> m Bool
effectSteadfastness execSfx power target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  tb <- getsState $ getActorBody target
  let bcalmMax = Dice.maxDice (acalm $ okind $ bkind tb)
      deltaCalm = min power (bcalmMax - bcalm tb)
  if deltaCalm <= 0
    then effectNoEffect target
    else do
      execUpdAtomic $ UpdCalmActor target deltaCalm
      execSfx
      return True

-- ** Ascend

effectAscend :: MonadAtomic m => m () -> Int -> ActorId -> m Bool
effectAscend execSfx power target = do
  tb <- getsState $ getActorBody target
  mfail <- effLvlGoUp target power
  case mfail of
    Nothing -> do
      execSfx
      return True
    Just failMsg -> do
      execSfxAtomic $ SfxMsgFid (bfid tb) failMsg
      return False

effLvlGoUp :: MonadAtomic m => ActorId -> Int -> m (Maybe Msg)
effLvlGoUp aid k = do
  b1 <- getsState $ getActorBody aid
  ais1 <- getsState $ getCarriedAssocs b1
  let lid1 = blid b1
      pos1 = bpos b1
  (lid2, pos2) <- getsState $ whereTo lid1 pos1 k . sdungeon
  if lid2 == lid1 && pos2 == pos1 then
    return $ Just "The effect fizzles: no more levels in this direction."
  else if bproj b1 then
    assert `failure` "projectiles can't exit levels" `twith` (aid, k, b1)
  else do
    let switch1 = void $ switchLevels1 ((aid, b1), ais1)
        switch2 = do
          -- Make the intiator of the stair move the leader,
          -- to let him clear the stairs for other to follow.
          let mlead = Just aid
          -- Move the actor to where the inhabitants were, if any.
          switchLevels2 lid2 pos2 ((aid, b1), ais1) mlead
          -- Verify only one non-projectile actor on every tile.
          !_ <- getsState $ posToActors pos1 lid1  -- assertion is inside
          !_ <- getsState $ posToActors pos2 lid2  -- assertion is inside
          return ()
    -- The actor will be added to the new level, but there can be other actors
    -- at his new position.
    inhabitants <- getsState $ posToActors pos2 lid2
    case inhabitants of
      [] -> do
        switch1
        switch2
      ((_, b2), _) : _ -> do
        -- Alert about the switch.
        let subjects = map (partActor . snd . fst) inhabitants
            subject = MU.WWandW subjects
            verb = "be pushed to another level"
            msg2 = makeSentence [MU.SubjectVerbSg subject verb]
        -- Only tell one player, even if many actors, because then
        -- they are projectiles, so not too important.
        execSfxAtomic $ SfxMsgFid (bfid b2) msg2
        -- Move the actor out of the way.
        switch1
        -- Move the inhabitant out of the way and to where the actor was.
        let moveInh inh = do
              -- Preserve old the leader, since the actor is pushed, so possibly
              -- has nothing worhwhile to do on the new level (and could try
              -- to switch back, if made a leader, leading to a loop).
              inhMLead <- switchLevels1 inh
              switchLevels2 lid1 pos1 inh inhMLead
        mapM_ moveInh inhabitants
        -- Move the actor to his destination.
        switch2
    return Nothing

switchLevels1 :: MonadAtomic m
              => ((ActorId, Actor), [(ItemId, Item)]) -> m (Maybe ActorId)
switchLevels1 ((aid, bOld), ais) = do
  let side = bfid bOld
  mleader <- getsState $ gleader . (EM.! side) . sfactionD
  -- Prevent leader pointing to a non-existing actor.
  mlead <-
    if not (bproj bOld) && isJust mleader then do
      execUpdAtomic $ UpdLeadFaction side mleader Nothing
      return mleader
    else return Nothing
  -- Remove the actor from the old level.
  -- Onlookers see somebody disappear suddenly.
  -- @DestroyActorA@ is too loud, so use @LoseActorA@ instead.
  execUpdAtomic $ UpdLoseActor aid bOld ais
  return mlead

switchLevels2 :: MonadAtomic m
              => LevelId -> Point
              -> ((ActorId, Actor), [(ItemId, Item)]) -> Maybe ActorId
              -> m ()
switchLevels2 lidNew posNew ((aid, bOld), ais) mlead = do
  let lidOld = blid bOld
      side = bfid bOld
  assert (lidNew /= lidOld `blame` "stairs looped" `twith` lidNew) skip
  -- Sync the actor time with the level time.
  timeOld <- getsState $ getLocalTime lidOld
  timeLastVisited <- getsState $ getLocalTime lidNew
  -- This time calculation may cause a double move of a foe of the same
  -- speed, but this is OK --- the foe didn't have a chance to move
  -- before, because the arena went inactive, so he moves now one more time.
  let delta = btime bOld `timeDeltaToFrom` timeOld
      bNew = bOld { blid = lidNew
                  , btime = timeShift timeLastVisited delta
                  , bpos = posNew
                  , boldpos = posNew  -- new level, new direction
                  , boldlid = lidOld }  -- record old level
  -- Materialize the actor at the new location.
  -- Onlookers see somebody appear suddenly. The actor himself
  -- sees new surroundings and has to reset his perception.
  execUpdAtomic $ UpdCreateActor aid bNew ais
  when (isJust mlead) $ execUpdAtomic $ UpdLeadFaction side Nothing mlead

-- ** Escape

-- | The faction leaves the dungeon.
effectEscape :: (MonadAtomic m, MonadServer m) => ActorId -> m Bool
effectEscape aid = do
  -- Obvious effect, nothing announced.
  b <- getsState $ getActorBody aid
  let fid = bfid b
      keepArena fact = playerLeader (gplayer fact) && not (isSpawnFact fact)
  fact <- getsState $ (EM.! fid) . sfactionD
  if not (keepArena fact) || bproj b then
    return False
  else do
    deduceQuits b $ Status Escape (fromEnum $ blid b) ""
    return True
