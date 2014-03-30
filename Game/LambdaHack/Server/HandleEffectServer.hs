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
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
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
           => ActorId -> ActorId -> Maybe ItemId -> Item
           -> m ()
itemEffect source target miid item = do
  postb <- getsState $ getActorBody source
  discoS <- getsServer sdisco
  let ik = fromJust $ jkind discoS item
      ef = jeffect item
  b <- effectSem ef source target
  -- The effect is interesting so the item gets identified, if seen
  -- (the item was at the source actor's position, so his old position
  -- is given, since the actor and/or the item may be moved by the effect;
  -- we'd need to track not only position of atomic commands and factions,
  -- but also which items they relate to, to be fully accurate).
  let atomic iid = execUpdAtomic $ UpdDiscover (blid postb) (bpos postb) iid ik
  when b $ maybe skip atomic miid

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
effectSem :: (MonadAtomic m, MonadServer m)
          => Effect.Effect Int -> ActorId -> ActorId
          -> m Bool
effectSem effect source target = case effect of
  Effect.NoEffect -> effectNoEffect target
  Effect.Heal p -> effectHeal p target
  Effect.Hurt nDm p -> effectWound nDm p source target
  Effect.Mindprobe _ -> effectMindprobe target
  Effect.Dominate | source /= target -> effectDominate source target
  Effect.Dominate -> effectSem (Effect.Mindprobe undefined) source target
  Effect.CallFriend p -> effectCallFriend p source target
  Effect.Summon p -> effectSummon p target
  Effect.CreateItem p -> effectCreateItem p target
  Effect.ApplyPerfume -> effectApplyPerfume source target
  Effect.Regeneration p -> effectSem (Effect.Heal p) source target
  Effect.Steadfastness p -> effectSteadfastness p target
  Effect.Ascend p -> effectAscend p target
  Effect.Escape{} -> effectEscape target

-- + Individual semantic functions for effects

-- ** NoEffect

effectNoEffect :: MonadAtomic m => ActorId -> m Bool
effectNoEffect target = do
  execSfxAtomic $ SfxEffect target Effect.NoEffect
  return False

-- ** Heal

effectHeal :: MonadAtomic m => Int -> ActorId -> m Bool
effectHeal power target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  tm <- getsState $ getActorBody target
  let bhpMax = Dice.maxDice (ahp $ okind $ bkind tm)
      deltaHP = min power (max 0 $ bhpMax - bhp tm)
  if deltaHP == 0
    then do
      execSfxAtomic $ SfxEffect target Effect.NoEffect
      return False
    else do
      execUpdAtomic $ UpdHealActor target deltaHP
      when (deltaHP < 0) $ halveCalm target
      execSfxAtomic $ SfxEffect target $ Effect.Heal deltaHP
      return True

halveCalm :: MonadAtomic m => ActorId -> m ()
halveCalm target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  tb <- getsState $ getActorBody target
  let calmMax = Dice.maxDice $ acalm $ okind $ bkind tb
      calmCur = bcalm tb
      deltaCalm = calmMax `div` 2 - calmCur
  when (deltaCalm < 0) $ execUpdAtomic $ UpdCalmActor target deltaCalm

-- ** Wound

effectWound :: (MonadAtomic m, MonadServer m)
            => Dice.Dice -> Int -> ActorId -> ActorId
            -> m Bool
effectWound nDm power source target = do
  n <- rndToAction $ castDice 0 0 nDm
  let deltaHP = - (n + power)
  if deltaHP >= 0
    then do
      execSfxAtomic $ SfxEffect target Effect.NoEffect
      return False
    else do
      -- Damage the target.
      execUpdAtomic $ UpdHealActor target deltaHP
      halveCalm target
      execSfxAtomic $ SfxEffect target $
        if source == target
        then Effect.Heal deltaHP
        else Effect.Hurt nDm deltaHP{-hack-}
      return True

-- ** Mindprobe

effectMindprobe :: MonadAtomic m
                => ActorId -> m Bool
effectMindprobe target = do
  tb <- getsState $ getActorBody target
  let lid = blid tb
  fact <- getsState $ (EM.! bfid tb) . sfactionD
  lb <- getsState $ actorRegularList (isAtWar fact) lid
  let nEnemy = length lb
  if nEnemy == 0 || bproj tb then do
    execSfxAtomic $ SfxEffect target Effect.NoEffect
    return False
  else do
    execSfxAtomic $ SfxEffect target $ Effect.Mindprobe nEnemy
    return True

-- ** Dominate

effectDominate :: (MonadAtomic m, MonadServer m)
               => ActorId -> ActorId -> m Bool
effectDominate source target = do
  Kind.COps{coactor=Kind.Ops{okind}, cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if bfid tb == bfid sb || bproj tb then do
    execSfxAtomic $ SfxEffect target Effect.NoEffect
    return False
  else do
    -- Announce domination before the actor changes sides.
    execSfxAtomic $ SfxEffect target Effect.Dominate
    -- Only record the first domination as a kill.
    when (boldfid tb == bfid tb) $ execUpdAtomic $ UpdRecordKill target 1
    -- TODO: Perhaps insert a turn of delay here to allow countermeasures.
    electLeader (bfid tb) (blid tb) target
    deduceKilled tb
    ais <- getsState $ getCarriedAssocs tb
    execUpdAtomic $ UpdLoseActor target tb ais
    let baseSpeed = aspeed $ okind $ bkind tb
        bNew = tb {bfid = bfid sb, boldfid = bfid tb,
                   bcalm = 0, bspeed = baseSpeed}
    execUpdAtomic $ UpdCreateActor target bNew ais
    -- Halve the speed as a side-effect of first domination, or keep it reset
    -- if dominating again. TODO: teach AI to prefer dominating again.
    let halfSpeed = speedScale (1%2) baseSpeed
        delta | boldfid tb == bfid tb = speedNegate halfSpeed  -- slow down
              | otherwise = speedZero
    when (delta /= speedZero) $ execUpdAtomic $ UpdHasteActor target delta
    mleaderOld <- getsState $ gleader . (EM.! bfid sb) . sfactionD
    -- Keep the leader if he is on stairs. We don't want to clog stairs.
    keepLeader <- case mleaderOld of
      Nothing -> return False
      Just leaderOld -> do
        body <- getsState $ getActorBody leaderOld
        lvl <- getLevel $ blid body
        return $! Tile.isStair cotile $ lvl `at` bpos body
    unless keepLeader $
      -- Focus on the dominated actor, by making him a leader.
      execUpdAtomic $ UpdLeadFaction (bfid sb) mleaderOld (Just target)
    return True

-- ** SummonFriend

effectCallFriend :: (MonadAtomic m, MonadServer m)
                   => Int -> ActorId -> ActorId
                   -> m Bool
effectCallFriend power source target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  Kind.COps{cotile} <- getsState scops
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  ps <- getsState $ nearbyFreePoints cotile (const True) (bpos tm) (blid tm)
  summonFriends (bfid sm) (take power ps) (blid tm)
  return True

summonFriends :: (MonadAtomic m, MonadServer m)
              => FactionId -> [Point] -> LevelId
              -> m ()
summonFriends bfid ps lid = do
  Kind.COps{ coactor=coactor@Kind.Ops{opick}
           , cofaction=Kind.Ops{okind} } <- getsState scops
  time <- getsState $ getLocalTime lid
  factionD <- getsState sfactionD
  let fact = okind $ gkind $ factionD EM.! bfid
  forM_ ps $ \p -> do
    let summonName = fname fact
    mk <- rndToAction $ fmap (fromMaybe $ assert `failure` summonName)
                        $ opick summonName (const True)
    if mk == heroKindId coactor
      then addHero bfid p lid [] Nothing time
      else addMonster mk bfid p lid time
  -- No leader election needed, bebause an alive actor of the same faction
  -- causes the effect, so there is already a leader.

-- ** SpawnMonster

effectSummon :: (MonadAtomic m, MonadServer m)
             => Int -> ActorId -> m Bool
effectSummon power target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  Kind.COps{cotile} <- getsState scops
  tm <- getsState (getActorBody target)
  ps <- getsState $ nearbyFreePoints cotile (const True) (bpos tm) (blid tm)
  time <- getsState $ getLocalTime (blid tm)
  mfid <- pickFaction "summon" (const True)
  case mfid of
    Nothing -> return False  -- no faction summons
    Just fid -> do
      spawnMonsters (take power ps) (blid tm) time fid
      return True

-- | Roll a faction based on faction kind frequency key.
pickFaction :: MonadServer m
            => Text
            -> ((FactionId, Faction) -> Bool)
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
  tm <- getsState $ getActorBody target
  void $ createItems power (bpos tm) (blid tm)
  return True

-- ** ApplyPerfume

effectApplyPerfume :: MonadAtomic m
                   => ActorId -> ActorId -> m Bool
effectApplyPerfume source target =
  if source == target
  then do
    execSfxAtomic $ SfxEffect target Effect.NoEffect
    return False
  else do
    tm <- getsState $ getActorBody target
    Level{lsmell} <- getLevel $ blid tm
    let f p fromSm =
          execUpdAtomic $ UpdAlterSmell (blid tm) p (Just fromSm) Nothing
    mapWithKeyM_ f lsmell
    execSfxAtomic $ SfxEffect target Effect.ApplyPerfume
    return True

-- ** Regeneration

-- ** Steadfastness

effectSteadfastness :: MonadAtomic m => Int -> ActorId -> m Bool
effectSteadfastness power target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  tm <- getsState $ getActorBody target
  let bcalmMax = Dice.maxDice (acalm $ okind $ bkind tm)
      deltaCalm = min power (max 0 $ bcalmMax - bcalm tm)
  if deltaCalm == 0
    then do
      execSfxAtomic $ SfxEffect target Effect.NoEffect
      return False
    else do
      execUpdAtomic $ UpdCalmActor target deltaCalm
      execSfxAtomic $ SfxEffect target $ Effect.Steadfastness deltaCalm
      return True

-- ** Ascend

effectAscend :: MonadAtomic m => Int -> ActorId -> m Bool
effectAscend power target = do
  mfail <- effLvlGoUp target power
  case mfail of
    Nothing -> do
      execSfxAtomic $ SfxEffect target $ Effect.Ascend power
      return True
    Just failMsg -> do
      b <- getsState $ getActorBody target
      execSfxAtomic $ SfxMsgFid (bfid b) failMsg
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
              => LevelId -> Point -> ((ActorId, Actor), [(ItemId, Item)])
              -> Maybe ActorId
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
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  let fid = bfid b
  fact <- getsState $ (EM.! fid) . sfactionD
  if not (isHeroFact cops fact) || bproj b then
    return False
  else do
    deduceQuits b $ Status Escape (fromEnum $ blid b) ""
    return True
