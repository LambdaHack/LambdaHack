{-# LANGUAGE TupleSections #-}
-- | Handle effects (most often caused by requests sent by clients).
module Game.LambdaHack.Server.HandleEffectServer
  ( applyItem, itemEffect, itemEffectAndDestroy, effectsSem
  , dropEqpItem, armorHurtBonus
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Bits (xor)
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
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Server.CommonServer
import Game.LambdaHack.Server.ItemServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicServer
import Game.LambdaHack.Server.StartServer
import Game.LambdaHack.Server.State

-- + Semantics of effects

applyItem :: (MonadAtomic m, MonadServer m)
          => ActorId -> ItemId -> CStore -> m ()
applyItem aid iid cstore = do
  itemToF <- itemToFullServer
  bag <- getsState $ getActorBag aid cstore
  let k = bag EM.! iid
      itemFull = itemToF iid k
  execSfxAtomic $ SfxActivate aid iid 1
  itemEffectAndDestroy aid aid iid itemFull cstore

itemEffectAndDestroy :: (MonadAtomic m, MonadServer m)
                     => ActorId -> ActorId -> ItemId -> ItemFull -> CStore
                     -> m ()
itemEffectAndDestroy source target iid itemFull cstore = do
  -- We have to destroy the item before the effect affects the item
  -- or the actor holding it or standing on it (later on we could
  -- lose track of the item and wouldn't be able to destroy it) .
  -- This is OK, because we don't remove the item type from various
  -- item dictionaries, just an individual copy from the container,
  -- so, e.g., the item can be identified after it's removed.
  let item = itemBase itemFull
      durable = Effect.Durable `elem` jfeature item
      periodic = isJust $ strengthFromEqpSlot Effect.EqpSlotPeriodic itemFull
      c = CActor source cstore
  unless (durable && periodic) $ do
    when (not durable) $
      execUpdAtomic $ UpdLoseItem iid item 1 c
    triggered <- itemEffect source target iid itemFull False False
    -- If none of item's effects was performed, we try to recreate the item.
    -- Regardless, we don't rewind the time, because some info is gained
    -- (that the item does not exhibit any effects in the given context).
    when (not triggered && not durable) $
      execUpdAtomic $ UpdSpotItem iid item 1 c

-- | The source actor affects the target actor, with a given item.
-- If any of the effect effect fires up, the item gets identified. This function
-- is mutually recursive with @effect@ and so it's a part of @Effect@
-- semantics.
itemEffect :: (MonadAtomic m, MonadServer m)
           => ActorId -> ActorId -> ItemId -> ItemFull -> Bool -> Bool
           -> m Bool
itemEffect source target iid itemFull onSmash periodic = do
  case itemDisco itemFull of
    Just ItemDisco{itemKindId, itemAE=Just ItemAspectEffect{jeffects}} -> do
      let effs | onSmash = strengthOnSmash itemFull
               | otherwise = jeffects
      triggered <- effectsSem effs source target periodic
      -- The effect fires up, so the item gets identified, if seen
      -- (the item was at the source actor's position, so his old position
      -- is given, since the actor and/or the item may be moved by the effect;
      -- we'd need to track not only position of atomic commands and factions,
      -- but also which items they relate to, to be fully accurate).
      when triggered $ do
        postb <- getsState $ getActorBody source
        seed <- getsServer $ (EM.! iid) . sitemSeedD
        execUpdAtomic $ UpdDiscover (blid postb) (bpos postb)
                                    iid itemKindId seed
      return triggered
    _ -> assert `failure` (source, target, iid, itemFull)

effectsSem :: (MonadAtomic m, MonadServer m)
           => [Effect.Effect Int] -> ActorId -> ActorId -> Bool
           -> m Bool
effectsSem effects source target periodic = do
  trs <- mapM (\ef -> effectSem ef source target) effects
  let triggered = or trs
  sb <- getsState $ getActorBody source
  -- Announce no effect, which is rare and wastes time, so noteworthy.
  unless (triggered  -- some effect triggered, so feedback comes from them
          || null effects  -- no effects present, no feedback needed
          || periodic  -- don't spam from fizzled periodic effects
          || bproj sb) $  -- don't spam, projectiles can be very numerous
    execSfxAtomic $ SfxEffect (bfid sb) target $ Effect.NoEffect ""
  return triggered

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect actually fired up,
-- as opposed to fizzled.
effectSem :: (MonadAtomic m, MonadServer m)
          => Effect.Effect Int -> ActorId -> ActorId
          -> m Bool
effectSem effect source target = do
  sb <- getsState $ getActorBody source
  -- @execSfx@ usually comes last in effect semantics, but not always
  -- and we are likely to introduce more variety.
  let execSfx = execSfxAtomic $ SfxEffect (bfid sb) target effect
  case effect of
    Effect.NoEffect _ -> return False
    Effect.RefillHP p -> effectRefillHP execSfx p source target
    Effect.Hurt nDm -> effectHurt nDm source target
    Effect.RefillCalm p -> effectRefillCalm execSfx p target
    Effect.Dominate -> effectDominate source target
    Effect.Impress -> effectImpress execSfx source target
    Effect.CallFriend p -> effectCallFriend p source target
    Effect.Summon freqs p -> effectSummon freqs p source target
    Effect.CreateItem p -> effectCreateItem p target
    Effect.ApplyPerfume -> effectApplyPerfume execSfx target
    Effect.Burn p -> effectBurn execSfx p source target
    Effect.Ascend p -> effectAscend execSfx p source target
    Effect.Escape{} -> effectEscape target
    Effect.Paralyze p -> effectParalyze execSfx p target
    Effect.InsertMove p -> effectInsertMove execSfx p target
    Effect.DropBestWeapon -> effectDropBestWeapon execSfx target
    Effect.DropEqp symbol hit -> effectDropEqp execSfx hit target symbol
    Effect.SendFlying tmod ->
      effectSendFlying execSfx tmod source target Nothing
    Effect.PushActor tmod ->
      effectSendFlying execSfx tmod source target (Just True)
    Effect.PullActor tmod ->
      effectSendFlying execSfx tmod source target (Just False)
    Effect.Teleport p -> effectTeleport execSfx p target
    Effect.PolyItem cstore -> effectPolyItem execSfx cstore target
    Effect.Identify cstore -> effectIdentify execSfx cstore target
    Effect.ActivateInv symbol -> effectActivateInv execSfx target symbol
    Effect.Explode t -> effectExplode execSfx t target
    Effect.OneOf l -> effectOneOf l source target
    Effect.OnSmash _ -> return False  -- ignored under normal circumstances
    Effect.TimedAspect{} -> return False  -- TODO

-- + Individual semantic functions for effects

-- ** RefillHP

effectRefillHP :: (MonadAtomic m, MonadServer m)
           => m () -> Int -> ActorId -> ActorId -> m Bool
effectRefillHP execSfx power source target = do
  tb <- getsState $ getActorBody target
  hpMax <- sumOrganEqpServer Effect.EqpSlotAddMaxHP target
  let deltaHP = min (xM power) (max 0 $ xM hpMax - bhp tb)
  if deltaHP == 0
    then return False
    else do
      execUpdAtomic $ UpdRefillHP target deltaHP
      when (deltaHP < 0 && source /= target && not (bproj tb)) $
        halveCalm target
      execSfx
      return True

halveCalm :: (MonadAtomic m, MonadServer m)
          => ActorId -> m ()
halveCalm target = do
  tb <- getsState $ getActorBody target
  activeItems <- activeItemsServer target
  let calmMax = sumSlotNoFilter Effect.EqpSlotAddMaxCalm activeItems
      calmUpperBound = if hpTooLow tb activeItems
                       then 0  -- to trigger domination, etc.
                       else xM calmMax `div` 2
      deltaCalm = min minusTwoM (calmUpperBound - bcalm tb)
  -- HP loss decreases Calm by at least minusTwoM, to overcome Calm regen,
  -- when far from shooting foe and to avoid "hears something",
  -- which is emitted for decrease minusM.
  execUpdAtomic $ UpdRefillCalm target deltaCalm

-- ** Hurt

effectHurt :: (MonadAtomic m, MonadServer m)
           => Dice.Dice -> ActorId -> ActorId
           -> m Bool
effectHurt nDm source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  n <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  hurtBonus <- armorHurtBonus source target
  let block = braced tb
      mult = (100 + hurtBonus) * (if block then 50 else 100)
      deltaHP = - (max oneM  -- at least 1 HP taken
                   $ fromIntegral mult * xM n `divUp` (100 * 100))
      deltaDiv = fromIntegral $ deltaHP `divUp` oneM
  -- Damage the target.
  execUpdAtomic $ UpdRefillHP target deltaHP
  when (source /= target && not (bproj tb)) $ halveCalm target
  execSfxAtomic $ SfxEffect (bfid sb) target $
    if source == target
    then Effect.RefillHP deltaDiv  -- no SfxStrike, so treat as any heal/wound
    else Effect.Hurt (Dice.intToDice deltaDiv)  -- avoid spam; SfxStrike sent
  return True

armorHurtBonus :: (MonadAtomic m, MonadServer m)
               => ActorId -> ActorId
               -> m Int
armorHurtBonus source target = do
  sactiveItems <- activeItemsServer source
  tactiveItems <- activeItemsServer target
  sb <- getsState $ getActorBody source
  return $! if bproj sb
            then sumSlotNoFilter Effect.EqpSlotAddHurtRanged sactiveItems
                 - sumSlotNoFilter Effect.EqpSlotAddArmorRanged tactiveItems
            else sumSlotNoFilter Effect.EqpSlotAddHurtMelee sactiveItems
                 - sumSlotNoFilter Effect.EqpSlotAddArmorMelee tactiveItems

-- ** RefillCalm

effectRefillCalm ::  (MonadAtomic m, MonadServer m)
           => m () -> Int -> ActorId -> m Bool
effectRefillCalm execSfx power target = do
  tb <- getsState $ getActorBody target
  calmMax <- sumOrganEqpServer Effect.EqpSlotAddMaxCalm target
  let deltaCalm = min (xM power) (max 0 $ xM calmMax - bcalm tb)
  if deltaCalm == 0
    then return False
    else do
      execUpdAtomic $ UpdRefillCalm target deltaCalm
      execSfx
      return True

-- ** Dominate

effectDominate :: (MonadAtomic m, MonadServer m)
               => ActorId -> ActorId -> m Bool
effectDominate source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if bproj tb then
    return False
  else if bfid tb == bfid sb then
    effectSem Effect.Impress source target
  else
    dominateFidSfx (bfid sb) target

-- ** Impress

effectImpress :: (MonadAtomic m, MonadServer m)
              => m () -> ActorId -> ActorId -> m Bool
effectImpress execSfx source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if boldfid tb == bfid sb || bproj tb then
    return False
  else do
    execSfx
    execUpdAtomic $ UpdOldFidActor target (boldfid tb) (bfid sb)
    return True

-- ** SummonFriend

effectCallFriend :: (MonadAtomic m, MonadServer m)
                   => Int -> ActorId -> ActorId
                   -> m Bool
effectCallFriend power source target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  activeItems <- activeItemsServer source
  let legal = source == target
              && hpEnough sb activeItems
              && bhp sb >= xM 10  -- prevent spam from regenerating wimps
  if not legal then return False
  else do
    let hpMax = max 1 $ sumSlotNoFilter Effect.EqpSlotAddMaxHP activeItems
        deltaHP = - xM hpMax `div` 3
    execUpdAtomic $ UpdRefillHP source deltaHP
    let validTile t = not $ Tile.hasFeature cotile F.NoActor t
        lid = blid sb
    ps <- getsState $ nearbyFreePoints validTile (bpos sb) lid
    time <- getsState $ getLocalTime lid
    recruitActors (take power ps) lid time (bfid sb)

-- ** Summon

effectSummon :: (MonadAtomic m, MonadServer m)
             => Freqs -> Int -> ActorId -> ActorId -> m Bool
effectSummon actorFreq power source target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  activeItems <- activeItemsServer source
  let legal = source == target
              && (bproj sb
                  || calmEnough sb activeItems
                     && bcalm sb >= xM 10)
  if not legal then return False
  else do
    let calmMax = max 1 $ sumSlotNoFilter Effect.EqpSlotAddMaxCalm activeItems
        deltaCalm = - xM calmMax `div` 3
    unless (bproj sb) $ execUpdAtomic $ UpdRefillCalm source deltaCalm
    let validTile t = not $ Tile.hasFeature cotile F.NoActor t
    ps <- getsState $ nearbyFreePoints validTile (bpos sb) (blid sb)
    localTime <- getsState $ getLocalTime (blid sb)
    -- Make sure summoned actors start acting after the summoner.
    let sourceTime = timeShift localTime $ ticksPerMeter $ bspeed sb activeItems
        afterTime = timeShift sourceTime $ Delta timeClip
    bs <- forM (take power ps) $ \p -> do
      maid <- addAnyActor actorFreq (blid sb) afterTime (Just p)
      case maid of
        Nothing ->
          -- Don't make this item useless.
          effectSem (Effect.CallFriend 1) source target
        Just aid -> do
          b <- getsState $ getActorBody aid
          mleader <- getsState $ gleader . (EM.! bfid b) . sfactionD
          when (isNothing mleader) $
            execUpdAtomic $ UpdLeadFaction (bfid b) Nothing (Just aid)
          return True
    return $! or bs

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
                   => m () -> ActorId -> m Bool
effectApplyPerfume execSfx target = do
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
  void $ effectHurt (Dice.intToDice $ 2 * power) source target
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
  -- @UpdDestroyActor@ is too loud, so use @UpdLoseActor@ instead.
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
effectEscape target = do
  -- Obvious effect, nothing announced.
  b <- getsState $ getActorBody target
  let fid = bfid b
  fact <- getsState $ (EM.! fid) . sfactionD
  if not (keepArenaFact fact) || bproj b then
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
  b <- getsState $ getActorBody target
  if bproj b || bhp b <= 0
    then return False
    else do
      let t = timeDeltaScale (Delta timeClip) p
      execUpdAtomic $ UpdAgeActor target t
      execSfx
      return True

-- ** InsertMove

-- TODO: Replace with SpeedBurst that lasts just 1 turn,
-- but make sure the cost of this item activation is vs previous speed
-- | Give target actor the given number of extra moves. Don't give
-- an absolute amount of time units, to benefit slow actors more.
effectInsertMove :: (MonadAtomic m, MonadServer m)
                 => m () -> Int -> ActorId -> m Bool
effectInsertMove execSfx p target = assert (p > 0) $ do
  b <- getsState $ getActorBody target
  activeItems <- activeItemsServer target
  let tpm = ticksPerMeter $ bspeed b activeItems
      t = timeDeltaScale tpm (-p)
  execUpdAtomic $ UpdAgeActor target t
  execSfx
  return True

-- ** DropBestWeapon

-- | Make the target actor drop his best weapon (stack).
effectDropBestWeapon :: (MonadAtomic m, MonadServer m)
                     => m () -> ActorId -> m Bool
effectDropBestWeapon execSfx target = do
  allAssocs <- fullAssocsServer target [CEqp]
  case strongestSlotNoFilter Effect.EqpSlotWeapon allAssocs of
    (_, (iid, _)) : _ -> do
      b <- getsState $ getActorBody target
      let k = beqp b EM.! iid
      dropEqpItem target b False iid k
      execSfx
      return True
    [] ->
      return False

-- | Drop a single actor's item. Note that if there multiple copies,
-- at most one explodes to avoid excessive carnage and UI clutter
-- (let's say, the multiple explosions interfere with each other or perhaps
-- larger quantities of explosives tend to be packaged more safely).
dropEqpItem :: (MonadAtomic m, MonadServer m)
            => ActorId -> Actor -> Bool -> ItemId -> Int -> m ()
dropEqpItem aid b hit iid k = do
  item <- getsState $ getItemBody iid
  itemToF <- itemToFullServer
  let container = CActor aid CEqp
      fragile = Effect.Fragile `elem` jfeature item
      durable = Effect.Durable `elem` jfeature item
      isDestroyed = hit && not durable || bproj b && fragile
      itemFull = itemToF iid k
  if isDestroyed then do
    -- Feedback from hit, or it's shrapnel, so no @UpdDestroyItem@.
    execUpdAtomic $ UpdLoseItem iid item k container
    void $ itemEffect aid aid iid itemFull True False
  else do
    mvCmd <- generalMoveItem iid k (CActor aid CEqp)
                                   (CActor aid CGround)
    mapM_ execUpdAtomic mvCmd

-- ** DropEqp

-- | Make the target actor drop all items in his equiment with the given symbol
-- (not just a random one, or cluttering equipment with rubbish
-- would be beneficial).
effectDropEqp :: (MonadAtomic m, MonadServer m)
              => m () -> Bool -> ActorId -> Char -> m Bool
effectDropEqp execSfx hit target symbol = do
  b <- getsState $ getActorBody target
  effectTransformEqp execSfx target symbol CEqp $
    dropEqpItem target b hit

effectTransformEqp :: forall m. (MonadAtomic m, MonadServer m)
                   => m () -> ActorId -> Char -> CStore
                   -> (ItemId -> Int -> m ())
                   -> m Bool
effectTransformEqp execSfx target symbol cstore m = do
  let hasSymbol (iid, _) = do
        item <- getsState $ getItemBody iid
        return $! jsymbol item == symbol
  assocsCStore <- getsState $ EM.assocs . getActorBag target cstore
  is <- if symbol == ' '
        then return assocsCStore
        else filterM hasSymbol assocsCStore
  if null is
    then return False
    else do
      mapM_ (uncurry m) is
      execSfx
      return True

-- ** SendFlying

-- | Shend the target actor flying like a projectile. The arguments correspond
-- to @ToThrow@ and @Linger@ properties of items. If the actors are adjacent,
-- the vector is directed outwards, if no, inwards, if it's the same actor,
-- boldpos is used, if it can't, a random outward vector of length 10
-- is picked.
effectSendFlying :: (MonadAtomic m, MonadServer m)
                 => m () -> Effect.ThrowMod
                 -> ActorId -> ActorId -> Maybe Bool
                 -> m Bool
effectSendFlying execSfx Effect.ThrowMod{..} source target modePush = do
  v <- sendFlyingVector source target modePush
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
        then return False  -- supported by a wall
        else do
          weightAssocs <- fullAssocsServer target [CInv, CEqp, COrgan]
          let weight = sum $ map (jweight . itemBase . snd) weightAssocs
              path = bpos tb : pos : rest
              (trajectory, (speed, _)) =
                computeTrajectory weight throwVelocity throwLinger path
              ts = Just (trajectory, speed)
          unless (btrajectory tb == ts) $
            execUpdAtomic $ UpdTrajectory target (btrajectory tb) ts
          execSfx
          return True

sendFlyingVector :: (MonadAtomic m, MonadServer m)
                 => ActorId -> ActorId -> Maybe Bool -> m Vector
sendFlyingVector source target modePush = do
  sb <- getsState $ getActorBody source
  if source == target then do
    if boldpos sb == bpos sb then rndToAction $ do
      z <- randomR (-10, 10)
      oneOf [Vector 10 z, Vector (-10) z, Vector z 10, Vector z (-10)]
    else
      return $! vectorToFrom (bpos sb) (boldpos sb)
  else do
    tb <- getsState $ getActorBody target
    let (sp, tp) = if adjacent (bpos sb) (bpos tb)
                   then let pos = if chessDist (boldpos sb) (bpos tb)
                                     > chessDist (bpos sb) (bpos tb)
                                  then boldpos sb  -- avoid cardinal dir
                                  else bpos sb
                        in (pos, bpos tb)
                   else (bpos sb, bpos tb)
        pushV = vectorToFrom tp sp
        pullV = vectorToFrom sp tp
    return $! case modePush of
                Just True -> pushV
                Just False -> pullV
                Nothing | adjacent (bpos sb) (bpos tb) -> pushV
                Nothing -> pullV

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
      dMinMax delta pos =
        let d = chessDist spos pos
        in d >= range - delta && d <= range + delta
      dist delta pos _ = dMinMax delta pos
  tpos <- rndToAction $ findPosTry 200 ltile
    (\p t -> Tile.isWalkable cotile t
             && (not (dMinMax 9 p)  -- don't loop, very rare
                 || not (Tile.hasFeature cotile F.NoActor t)
                    && unoccupied as p))
    [ dist $ 1
    , dist $ 1 + range `div` 9
    , dist $ 1 + range `div` 7
    , dist $ 1 + range `div` 5
    , dist $ 5
    , dist $ 7
    ]
  if not (dMinMax 9 tpos) then
    return False  -- very rare
  else do
    execUpdAtomic $ UpdMoveActor target spos tpos
    execSfx
    return True

-- ** PolyItem

effectPolyItem :: (MonadAtomic m, MonadServer m)
               => m () -> CStore -> ActorId -> m Bool
effectPolyItem execSfx cstore target = do
  allAssocs <- fullAssocsServer target [cstore]
  case allAssocs of
    [] -> return False
    (iid, itemFull@ItemFull{..}) : _ -> case itemDisco of
      Just ItemDisco{itemKind} -> do
        let maxCount = Dice.maxDice $ icount itemKind
        if itemK >= maxCount
        then do
          let c = CActor target cstore
          execUpdAtomic $ UpdDestroyItem iid itemBase maxCount c
          execSfx
          effectCreateItem 1 target
        else do
          tb <- getsState $ getActorBody target
          execSfxAtomic $ SfxMsgFid (bfid tb) $
            "The purpose is served by" <+> tshow maxCount
            <+> "pieces of this item, not by" <+> tshow itemK <> "."
          return False
      _ -> assert `failure` (cstore, target, iid, itemFull)

-- ** Identify

effectIdentify :: (MonadAtomic m, MonadServer m)
               => m () -> CStore -> ActorId -> m Bool
effectIdentify execSfx cstore target = do
  allAssocs <- fullAssocsServer target [cstore]
  case allAssocs of
    [] -> return False
    (iid, itemFull@ItemFull{..}) : _ -> case itemDisco of
      Just ItemDisco{..} -> do
        -- TODO: use this (but faster, via traversing effects with 999)
        -- also to prevent sending any other UpdDiscover.
        let ided = Effect.Identified `elem` ifeature itemKind
            itemSecret = itemNoAE itemFull
            statsObvious = textAllAE False cstore itemFull
                           == textAllAE False cstore itemSecret
        if ided && statsObvious
          then return False
          else do
            execSfx
            tb <- getsState $ getActorBody target
            seed <- getsServer $ (EM.! iid) . sitemSeedD
            execUpdAtomic $ UpdDiscover (blid tb) (bpos tb) iid itemKindId seed
            return True
      _ -> assert `failure` (cstore, target, iid, itemFull)

-- ** ActivateInv

-- | Activate all activable items with the given symbol
-- in the target actor's equipment (there's no variant that activates
-- a random one, to avoid the incentive for carrying garbage).
-- Only one item of each stack is activated (and possibly consumed).
effectActivateInv :: (MonadAtomic m, MonadServer m)
                  => m () -> ActorId -> Char -> m Bool
effectActivateInv execSfx target symbol = do
  effectTransformEqp execSfx target symbol CInv $ \iid _ ->
    applyItem target iid CInv

-- ** Explode

effectExplode :: (MonadAtomic m, MonadServer m)
              => m () -> Text -> ActorId -> m Bool
effectExplode execSfx cgroup target = do
  tb <- getsState $ getActorBody target
  let itemFreq = [(cgroup, 1)]
      container = CActor target CEqp
  m2 <- rollAndRegisterItem (blid tb) itemFreq container False
  let (iid, (ItemFull{..}, _)) = fromMaybe (assert `failure` cgroup) m2
      Point x y = bpos tb
      projectN k100 n = do
        -- We pick a point at the border, not inside, to have a uniform
        -- distribution for the points the line goes through at each distance
        -- from the source. Otherwise, e.g., the points on cardinal
        -- and diagonal lines from the source would be more common.
        let fuzz = 2 + (k100 `xor` (itemK * n)) `mod` 9
            k = if itemK >= 8 && n < 8 then 0
                else if n < 8 && n >= 4 then 4 else n
            ps = take k $
              [ Point (x - 12) $ y + fuzz
              , Point (x - 12) $ y - fuzz
              , Point (x + 12) $ y + fuzz
              , Point (x + 12) $ y - fuzz
              , flip Point (y - 12) $ x + fuzz
              , flip Point (y - 12) $ x - fuzz
              , flip Point (y + 12) $ x + fuzz
              , flip Point (y + 12) $ x - fuzz
              ]
        forM_ ps $ \tpxy -> do
          let req = ReqProject tpxy k100 iid CEqp
          mfail <- projectFail target tpxy k100 iid CEqp True
          case mfail of
            Nothing -> return ()
            Just ProjectBlockTerrain -> return ()
            Just ProjectBlockActor | not $ bproj tb -> return ()
            Just failMsg -> execFailure target req failMsg
  -- All shrapnels bounce off obstacles many times before they destruct.
  forM_ [101..201] $ \k100 -> do
    bag2 <- getsState $ beqp . getActorBody target
    let mn2 = EM.lookup iid bag2
    maybe skip (projectN k100) mn2
  bag3 <- getsState $ beqp . getActorBody target
  let mn3 = EM.lookup iid bag3
  maybe skip (\k -> execUpdAtomic
                    $ UpdLoseItem iid itemBase k container) mn3
  execSfx
  return True  -- we avoid verifying that at least one projectile got off

-- ** OneOf

effectOneOf :: (MonadAtomic m, MonadServer m)
            => [Effect.Effect Int] -> ActorId -> ActorId -> m Bool
effectOneOf l source target = do
  ef <-  rndToAction $ oneOf l
  effectSem ef source target
