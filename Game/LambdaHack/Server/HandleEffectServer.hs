{-# LANGUAGE TupleSections #-}
-- | Handle effects (most often caused by requests sent by clients).
module Game.LambdaHack.Server.HandleEffectServer
  ( applyItem, itemEffect, effectSem
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Key (mapWithKeyM_)
import Data.Maybe
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Dice as Dice
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemFeature as IF
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.CommonServer
import Game.LambdaHack.Server.EndServer
import Game.LambdaHack.Server.ItemServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicServer
import Game.LambdaHack.Server.State

-- + Semantics of effects

applyItem :: (MonadAtomic m, MonadServer m)
          => Bool -> ActorId -> ItemId -> CStore -> m ()
applyItem turnOff aid iid cstore = do
  -- We have to destroy the item before the effect affects the item
  -- or the actor holding it or standing on it (later on we could
  -- lose track of the item and wouldn't be able to destroy it) .
  -- This is OK, because we don't remove the item type
  -- from the item dictionary, just an individual copy from the container.
  itemToF <- itemToFullServer
  item <- getsState $ getItemBody iid
  bag <- getsState $ getActorBag aid cstore
  let (k, isOn) = bag EM.! iid
      itemFull = itemToF iid (k, isOn)
      consumable = IF.Consumable `elem` jfeature item
  if consumable then do
    -- TODO: don't destroy if not really used up; also, don't take time?
    cs <- actorConts iid 1 aid cstore
    mapM_ (\(_, c) -> execUpdAtomic
                      $ UpdDestroyItem iid item (1, isOn) c) cs
    execSfxAtomic $ SfxActivate aid iid (1, isOn)
    itemEffect aid aid iid itemFull
  else when turnOff $
    execUpdAtomic $ UpdMoveItem iid k aid cstore isOn cstore (not isOn)

-- TODO: when h2h items have ItemId, replace Item with ItemId
-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified. This function
-- is mutually recursive with @effect@ and so it's a part of @Effect@
-- semantics.
itemEffect :: (MonadAtomic m, MonadServer m)
           => ActorId -> ActorId -> ItemId -> ItemFull
           -> m ()
itemEffect source target iid itemFull = do
  postb <- getsState $ getActorBody source
  case itemDisco itemFull of
    Just ItemDisco{itemKindId, itemAE=Just ItemAspectEffect{jeffects}} -> do
      bs <- mapM (\ef -> effectSem ef source target) jeffects
      -- The effect is interesting so the item gets identified, if seen
      -- (the item was at the source actor's position, so his old position
      -- is given, since the actor and/or the item may be moved by the effect;
      -- we'd need to track not only position of atomic commands and factions,
      -- but also which items they relate to, to be fully accurate).
      when (and bs) $ do
        seed <- getsServer $ (EM.! iid) . sitemSeedD
        execUpdAtomic $ UpdDiscover (blid postb) (bpos postb)
                                    iid itemKindId seed
    _ -> assert `failure` (source, target, iid, itemFull)

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
effectSem :: (MonadAtomic m, MonadServer m)
          => Effect.Effect Int -> ActorId -> ActorId
          -> m Bool
effectSem effect source target = do
  sb <- getsState $ getActorBody source
  let execSfx = execSfxAtomic $ SfxEffect (bfid sb) target effect
  case effect of
    Effect.NoEffect -> effectNoEffect target
    Effect.Heal p -> effectHeal execSfx p source target
    Effect.Hurt nDm p -> effectHurt nDm p source target
    Effect.Dominate -> effectDominate execSfx source target
    Effect.Impress -> effectImpress source target
    Effect.CallFriend p -> effectCallFriend p source target
    Effect.Summon p -> effectSummon p target
    Effect.CreateItem p -> effectCreateItem p target
    Effect.ApplyPerfume -> effectApplyPerfume execSfx source target
    Effect.Burn p -> effectBurn execSfx p source target
    Effect.Blast p -> effectBlast execSfx p source target
    Effect.Ascend p -> effectAscend execSfx p source target
    Effect.Escape{} -> effectEscape target
    Effect.Paralyze p -> effectParalyze execSfx p target
    Effect.InsertMove p -> effectInsertMove execSfx p target
    Effect.DropBestWeapon -> effectDropBestWeapon execSfx target
    Effect.DropEqp symbol hit -> effectDropEqp execSfx hit target symbol
    Effect.SendFlying p1 p2 ->
      effectSendFlying execSfx p1 p2 source target Nothing
    Effect.PushActor p1 p2 ->
      effectSendFlying execSfx p1 p2 source target (Just True)
    Effect.PullActor p1 p2 ->
      effectSendFlying execSfx p1 p2 source target (Just False)
    Effect.Teleport p -> effectTeleport execSfx p target
    Effect.ActivateEqp symbol -> effectActivateEqp execSfx target symbol
    Effect.TimedAspect{} -> effectNoEffect target  -- TODO

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
  sallAssocs <- fullAssocsServer source [CEqp, CBody]
  tallAssocs <- fullAssocsServer target [CEqp, CBody]
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  n <- rndToAction $ castDice 0 0 nDm
  let block = braced tb
      -- OFF shield doesn't hinder attacks, so also does not protect.
      sshieldMult = case strongestShield True sallAssocs of
        [] -> 100
        (p, _) : _ -> p
      tshieldMult = case strongestShield True tallAssocs of
        [] -> 100
        (p, _) : _ -> p
      mult = sshieldMult * tshieldMult * (if block then 100 else 50)
      deltaHP = - max 1 (mult * (n + power) `divUp` (100 * 100 * 100))
  -- Damage the target.
  execUpdAtomic $ UpdHealActor target deltaHP
  when (source /= target) $ halveCalm target
  execSfxAtomic $ SfxEffect (bfid sb) target $
    if source == target
    then Effect.Heal deltaHP
    else Effect.Hurt nDm deltaHP{-hack-}
  return True

-- ** Dominate

effectDominate :: (MonadAtomic m, MonadServer m)
               => m () -> ActorId -> ActorId -> m Bool
effectDominate execSfx source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if bproj tb then
    effectNoEffect target
  else if bfid tb == bfid sb then
    effectImpress source target
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
  Kind.COps{cotile} <- getsState scops
  sb <- getsState (getActorBody source)
  tb <- getsState (getActorBody target)
  let validTile t = not $ Tile.hasFeature cotile F.NoActor t
  ps <- getsState $ nearbyFreePoints validTile (bpos tb) (blid tb)
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
  Kind.COps{cotile} <- getsState scops
  tb <- getsState (getActorBody target)
  let validTile t = not $ Tile.hasFeature cotile F.NoActor t
  ps <- getsState $ nearbyFreePoints validTile (bpos tb) (blid tb)
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
  then effectNoEffect target
  else do
    tb <- getsState $ getActorBody target
    Level{lsmell} <- getLevel $ blid tb
    let f p fromSm =
          execUpdAtomic $ UpdAlterSmell (blid tb) p (Just fromSm) Nothing
    mapWithKeyM_ f lsmell
    execSfx
    return True

-- ** Burn

effectBurn :: (MonadAtomic m, MonadServer m)
           => m () -> Int -> ActorId -> ActorId
           -> m Bool
effectBurn execSfx power source target = do
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

-- ** Ascend

-- Note that projectiles can be teleported, too, for extra fun.
effectAscend :: (MonadAtomic m, MonadServer m)
             => m () -> Int -> ActorId -> ActorId -> m Bool
effectAscend execSfx k source aid = do
  b1 <- getsState $ getActorBody aid
  ais1 <- getsState $ getCarriedAssocs b1
  let lid1 = blid b1
      pos1 = bpos b1
  (lid2, pos2) <- getsState $ whereTo lid1 pos1 k . sdungeon
  if lid2 == lid1 && pos2 == pos1 then do
    execSfxAtomic $ SfxMsgFid (bfid b1) "No more levels in this direction."
    let effect = Effect.Teleport 30  -- powerful teleport
    effectSem effect source aid
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
    execSfx
    return True

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

-- ** Paralyze

-- | Advance target actor time by this many time clips. Not by actor moves,
-- to hurt fast actors more.
effectParalyze :: (MonadAtomic m, MonadServer m)
               => m () -> Int -> ActorId -> m Bool
effectParalyze execSfx p target = assert (p > 0) $ do
  let t = timeDeltaScale (Delta timeClip) p
  execUpdAtomic $ UpdAgeActor target t
  execSfx
  return True

-- ** InsertMove

-- | Give target actor the given number of extra moves. Don't give
-- an absolute amount of time units, to benefit slow actors more.
effectInsertMove :: (MonadAtomic m, MonadServer m)
                 => m () -> Int -> ActorId -> m Bool
effectInsertMove execSfx p target = assert (p > 0) $ do
  b <- getsState $ getActorBody target
  let tpm = ticksPerMeter $ bspeed b
      t = timeDeltaScale tpm (-p)
  execUpdAtomic $ UpdAgeActor target t
  execSfx
  return True

-- ** DropBestWeapon

-- | Make the target actor drop his best weapon (stack).
effectDropBestWeapon :: (MonadAtomic m, MonadServer m)
                     => m () -> ActorId -> m Bool
effectDropBestWeapon execSfx target = do
  cops <- getsState scops
  allAssocs <- fullAssocsServer target [CEqp]
  case strongestSword cops True allAssocs of  -- ignore OFF weapons
    (_, (iid, _)) : _ -> do
      b <- getsState $ getActorBody target
      let kIsOn = beqp b EM.! iid
      dropEqpItem target b False iid kIsOn
      execSfx
      return True
    [] -> return False

-- ** DropEqp

-- | Make the target actor drop all items in his equiment with the given symbol
-- (not just a random one, or cluttering equipment with rubbish
-- would be beneficial).
effectDropEqp :: (MonadAtomic m, MonadServer m)
              => m () -> Bool -> ActorId -> Char -> m Bool
effectDropEqp execSfx hit target symbol = do
  b <- getsState $ getActorBody target
  mapActorEqp_ (\iid kIsOn -> do
    item <- getsState $ getItemBody iid
    when (symbol == ' ' || jsymbol item == symbol) $
      dropEqpItem target b hit iid kIsOn) b
  execSfx
  return True

-- ** SendFlying

-- | Shend the target actor flying like a projectile. The arguments correspond
-- to @ToThrow@ and @Linger@ properties of items. If the actors are adjacent,
-- the vector is directed outwards, if no, inwards, if it's the same actor,
-- boldpos is used, if it can't, a random outward vector of length 10 is picked.
effectSendFlying :: (MonadAtomic m, MonadServer m)
                 => m () -> Int -> Int -> ActorId -> ActorId -> Maybe Bool
                 -> m Bool
effectSendFlying execSfx toThrow linger source target modePush = do
  mv <- sendFlyingVector source target modePush
  case mv of
    Nothing -> effectNoEffect target
    Just v -> do
      Kind.COps{cotile} <- getsState scops
      tb <- getsState $ getActorBody target
      lvl@Level{lxsize, lysize} <- getLevel (blid tb)
      let eps = 0
          fpos = bpos tb `shift` v
      case bla lxsize lysize eps (bpos tb) fpos of
        Nothing -> assert `failure` (fpos, tb)
        Just [] -> assert `failure` "projecting from the edge of level"
                          `twith` (fpos, tb)
        Just (pos : rest) -> do
          let t = lvl `at` pos
          if not $ Tile.isWalkable cotile t
            then effectNoEffect target  -- supported by a wall
            else do
              let -- TODO: add weight field to actor, unless we just
                  -- sum weigths of all body parts
                  weight = 70000  -- 70 kg
                  path = bpos tb : pos : rest
                  (trajectoryNew, speed) =
                    computeTrajectory weight toThrow linger path
                  speedOld = bspeed tb
                  speedDelta = speedAdd speed (speedNegate speedOld)
              execUpdAtomic $ UpdTrajectoryActor target (btrajectory tb)
                                                        (Just trajectoryNew)
              execUpdAtomic $ UpdHasteActor target speedDelta
              execSfx
              return True

sendFlyingVector :: (MonadAtomic m, MonadServer m)
                 => ActorId -> ActorId -> Maybe Bool -> m (Maybe Vector)
sendFlyingVector source target modePush = do
  sb <- getsState $ getActorBody source
  if source == target then do
    if modePush == Just False then return Nothing
    else if boldpos sb == bpos sb then rndToAction $ do
      z <- randomR (-10, 10)
      Just <$> oneOf [Vector 10 z, Vector (-10) z, Vector z 10, Vector z (-10)]
    else
      return $ Just $ vectorToFrom (bpos sb) (boldpos sb)
  else do
    tb <- getsState $ getActorBody target
    return $ if modePush == Just True || adjacent (bpos sb) (bpos tb)
             then if modePush == Just False then Nothing
                  else let pos = if chessDist (boldpos sb) (bpos tb)
                                    > chessDist (bpos sb) (bpos tb)
                                 then boldpos sb  -- avoid cardinal dir
                                 else bpos sb
                       in Just $ vectorToFrom (bpos tb) pos
             else if modePush == Just True then Nothing
                  else Just $ vectorToFrom (bpos sb) (bpos tb)

-- ** Teleport

-- | Teleport the target actor.
-- Note that projectiles can be teleported, too, for extra fun.
effectTeleport :: (MonadAtomic m, MonadServer m)
               => m () -> Int -> ActorId -> m Bool
effectTeleport execSfx range target = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody target
  Level{ltile} <- getLevel (blid b)
  as <- getsState $ actorList (const True) (blid b)
  let spos = bpos b
      dMinMax rmin rmax pos = let d = chessDist spos pos
                              in d >= rmin && d <= rmax
      dist rmin rmax pos _ = dMinMax rmin rmax pos
  tpos <- rndToAction $ findPosTry 200 ltile
    (\p t -> Tile.isWalkable cotile t
             && (not (dMinMax (range - 9) (range - 9) p)  -- don't loop
                 || not (Tile.hasFeature cotile F.NoActor t)
                    && unoccupied as p))
    [ dist (range - 1) (range - 1)
    , dist (range - 1 - range `div` 9) (range - 1 - range `div` 9)
    , dist (range - 1 - range `div` 7) (range - 1 - range `div` 7)
    , dist (range - 1 - range `div` 5) (range - 1 - range `div` 5)
    ]
  if not (dMinMax (range - 9) (range - 9) tpos) then
    effectNoEffect target
  else do
    execUpdAtomic $ UpdMoveActor target spos tpos
    execSfx
    return True

-- ** ActivateEqp

-- | Activate all activable items with the given symbol
-- in the target actor's equipment (there's no variant that activates
-- a random one, to avoid the incentive for carrying garbage).
-- Only one item of each stack is activated (and possibly consumed).
effectActivateEqp :: (MonadAtomic m, MonadServer m)
                  => m () -> ActorId -> Char -> m Bool
effectActivateEqp execSfx target symbol = do
  b <- getsState $ getActorBody target
  mapActorEqp_ (\iid _ -> do
    item <- getsState $ getItemBody iid
    when (symbol == ' ' || jsymbol item == symbol) $
      applyItem False target iid CEqp) b
  execSfx
  return True
