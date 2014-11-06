{-# LANGUAGE TupleSections #-}
-- | Handle effects (most often caused by requests sent by clients).
module Game.LambdaHack.Server.HandleEffectServer
  ( applyItem, itemEffectAndDestroy, effectAndDestroy, itemEffectCause
  , dropEqpItem, armorHurtBonus
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Bits (xor)
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.Key (mapWithKeyM_)
import Data.List
import Data.Maybe
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Faction
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
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK
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
  execSfxAtomic $ SfxActivate aid iid cstore
  let c = CActor aid cstore
  itemEffectAndDestroy aid aid iid c

itemEffectAndDestroy :: (MonadAtomic m, MonadServer m)
                     => ActorId -> ActorId -> ItemId -> Container
                     -> m ()
itemEffectAndDestroy source target iid c = do
  discoEffect <- getsServer sdiscoEffect
  case EM.lookup iid discoEffect of
    Just ItemAspectEffect{jeffects, jaspects} -> do
      bag <- getsState $ getCBag c
      case iid `EM.lookup` bag of
        Nothing -> assert `failure` (source, target, iid, c)
        Just kit ->
          effectAndDestroy source target iid c False jeffects jaspects kit
    _ -> assert `failure` (source, target, iid, c)

effectAndDestroy :: (MonadAtomic m, MonadServer m)
                 => ActorId -> ActorId -> ItemId -> Container -> Bool
                 -> [IK.Effect Int] -> [IK.Aspect Int] -> ItemQuant
                 -> m ()
effectAndDestroy source target iid c periodic effs aspects kitK@(k, it) = do
  let mtimeout = let timeoutAspect :: IK.Aspect a -> Bool
                     timeoutAspect IK.Timeout{} = True
                     timeoutAspect _ = False
                 in find timeoutAspect aspects
  lid <- getsState $ lidFromC c
  localTime <- getsState $ getLocalTime lid
  let it1 = case mtimeout of
        Just (IK.Timeout timeout) ->
          let timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
              pending startT = timeShift startT timeoutTurns > localTime
          in filter pending it
        _ -> []
      len = length it1
      recharged = len < k
  assert (len <= k `blame` (kitK, source, target, iid, c)) skip
  -- If there is no Timeout, but there are Recharging,
  -- then such effects are disabled whenever the item is affected
  -- by a Discharge attack (TODO).
  it2 <- case mtimeout of
    Just (IK.Timeout _) | recharged ->
      return $ localTime : it1
    _ ->
      -- TODO: if has timeout and not recharged, report failure
      return it1
  when (it /= it2) $ execUpdAtomic $ UpdTimeItem iid c it it2
  -- If the activation is not periodic, trigger at least the effects
  -- that are not recharging and so don't depend on @recharged@.
  when (not periodic || recharged) $ do
    -- We have to destroy the item before the effect affects the item
    -- or the actor holding it or standing on it (later on we could
    -- lose track of the item and wouldn't be able to destroy it) .
    -- This is OK, because we don't remove the item type from various
    -- item dictionaries, just an individual copy from the container,
    -- so, e.g., the item can be identified after it's removed.
    let mtmp = let tmpEffect :: IK.Effect a -> Bool
                   tmpEffect IK.Temporary{} = True
                   tmpEffect (IK.Recharging IK.Temporary{}) = True
                   tmpEffect _ = False
               in find tmpEffect effs
    item <- getsState $ getItemBody iid
    let durable = IK.Durable `elem` jfeature item
        imperishable = durable || periodic && isNothing mtmp
        kit = (1, take 1 it2)
    unless imperishable $
      execUpdAtomic $ UpdLoseItem iid item kit c
    -- At this point, the item is potentially no longer in container @c@.
    triggered <- itemEffectDisco source target iid recharged periodic effs
    -- If none of item's effects was performed, we try to recreate the item.
    -- Regardless, we don't rewind the time, because some info is gained
    -- (that the item does not exhibit any effects in the given context).
    unless (triggered || imperishable) $
      execUpdAtomic $ UpdSpotItem iid item kit c

itemEffectCause :: (MonadAtomic m, MonadServer m)
                => ActorId -> Point -> IK.Effect Int
                -> m Bool
itemEffectCause aid tpos ef = do
  sb <- getsState $ getActorBody aid
  let c = CEmbed (blid sb) tpos
  bag <- getsState $ getCBag c
  case EM.assocs bag of
    [(iid, kit)] -> do
      -- No block against tile, hence unconditional.
      discoEffect <- getsServer sdiscoEffect
      let aspects = case EM.lookup iid discoEffect of
            Just ItemAspectEffect{jaspects} -> jaspects
            _ -> assert `failure` (aid, tpos, ef, iid)
      execSfxAtomic $ SfxTrigger aid tpos $ TK.Cause ef
      effectAndDestroy aid aid iid c False [ef] aspects kit
      return True
    ab -> assert `failure` (aid, tpos, ab)

-- | The source actor affects the target actor, with a given item.
-- If any of the effects fires up, the item gets identified. This function
-- is mutually recursive with @effect@ and so it's a part of @Effect@
-- semantics.
itemEffectDisco :: (MonadAtomic m, MonadServer m)
                => ActorId -> ActorId -> ItemId -> Bool -> Bool
                -> [IK.Effect Int]
                -> m Bool
itemEffectDisco source target iid recharged periodic effs = do
  discoKind <- getsServer sdiscoKind
  item <- getsState $ getItemBody iid
  case EM.lookup (jkindIx item) discoKind of
    Just itemKindId -> do
      triggered <- itemEffect source target iid recharged periodic effs
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
    _ -> assert `failure` (source, target, iid, item)

itemEffect :: (MonadAtomic m, MonadServer m)
           => ActorId -> ActorId -> ItemId -> Bool -> Bool
           -> [IK.Effect Int]
           -> m Bool
itemEffect source target iid recharged periodic effects = do
  trs <- mapM (effectSem source target iid recharged) effects
  let triggered = or trs
  sb <- getsState $ getActorBody source
  -- Announce no effect, which is rare and wastes time, so noteworthy.
  unless (triggered  -- some effect triggered, so feedback comes from them
          || null effects  -- no effects present, no feedback needed
          || periodic  -- don't spam from fizzled periodic effects
          || bproj sb) $  -- don't spam, projectiles can be very numerous
    execSfxAtomic $ SfxEffect (bfid sb) target $ IK.NoEffect ""
  return triggered

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The item may or may not still be in the container.
-- The boolean result indicates if the effect actually fired up,
-- as opposed to fizzled.
effectSem :: (MonadAtomic m, MonadServer m)
          => ActorId -> ActorId -> ItemId -> Bool -> IK.Effect Int
          -> m Bool
effectSem source target iid recharged effect = do
  let recursiveCall = effectSem source target iid recharged
  sb <- getsState $ getActorBody source
  -- @execSfx@ usually comes last in effect semantics, but not always
  -- and we are likely to introduce more variety.
  let execSfx = execSfxAtomic $ SfxEffect (bfid sb) target effect
  case effect of
    IK.NoEffect _ -> return False
    IK.RefillHP p -> effectRefillHP False execSfx p source target
    IK.OverfillHP p -> effectRefillHP True execSfx p source target
    IK.Hurt nDm -> effectHurt nDm source target False
    IK.RefillCalm p -> effectRefillCalm False execSfx p target
    IK.OverfillCalm p -> effectRefillCalm True execSfx p target
    IK.Dominate -> effectDominate recursiveCall source target
    IK.Impress -> effectImpress execSfx source target
    IK.CallFriend p -> effectCallFriend p source target
    IK.Summon freqs p -> effectSummon recursiveCall freqs p source target
    IK.CreateItem p -> effectCreateItem p target
    IK.ApplyPerfume -> effectApplyPerfume execSfx target
    IK.Burn p -> effectBurn execSfx p source target
    IK.Ascend p -> effectAscend recursiveCall execSfx p target
    IK.Escape{} -> effectEscape target
    IK.Paralyze p -> effectParalyze execSfx p target
    IK.InsertMove p -> effectInsertMove execSfx p target
    IK.DropBestWeapon -> effectDropBestWeapon execSfx target
    IK.DropEqp symbol hit -> effectDropEqp execSfx hit target symbol
    IK.SendFlying tmod ->
      effectSendFlying execSfx tmod source target Nothing
    IK.PushActor tmod ->
      effectSendFlying execSfx tmod source target (Just True)
    IK.PullActor tmod ->
      effectSendFlying execSfx tmod source target (Just False)
    IK.Teleport p -> effectTeleport execSfx p target
    IK.PolyItem cstore -> effectPolyItem execSfx cstore target
    IK.Identify cstore -> effectIdentify execSfx cstore target
    IK.ActivateInv symbol -> effectActivateInv execSfx target symbol
    IK.Explode t -> effectExplode execSfx t target
    IK.OneOf l -> effectOneOf recursiveCall l
    IK.OnSmash _ -> return False  -- ignored under normal circumstances
    IK.Recharging e -> effectRecharging recursiveCall e recharged
    IK.CreateOrgan nDm t -> effectCreateOrgan target nDm t
    IK.Temporary _ -> effectTemporary execSfx source iid

-- + Individual semantic functions for effects

-- ** RefillHP

-- Unaffected by armor.
effectRefillHP :: (MonadAtomic m, MonadServer m)
               => Bool -> m () -> Int -> ActorId -> ActorId -> m Bool
effectRefillHP overfill execSfx power source target = do
  tb <- getsState $ getActorBody target
  hpMax <- sumOrganEqpServer IK.EqpSlotAddMaxHP target
  let overMax | overfill = xM hpMax * 10  -- arbitrary limit to scumming
              | otherwise = xM hpMax
      serious = overfill && source /= target && not (bproj tb)
      upperBound = if serious then xM hpMax else maxBound
      deltaHP | power > 0 = min (xM power) (max 0 $ overMax - bhp tb)
              | otherwise = min (xM power) (upperBound - bhp tb)
  if deltaHP == 0
    then return False
    else do
      execUpdAtomic $ UpdRefillHP target deltaHP
      when (deltaHP < 0 && serious) $ halveCalm target
      execSfx
      return True

halveCalm :: (MonadAtomic m, MonadServer m)
          => ActorId -> m ()
halveCalm target = do
  tb <- getsState $ getActorBody target
  activeItems <- activeItemsServer target
  let calmMax = sumSlotNoFilter IK.EqpSlotAddMaxCalm activeItems
      upperBound = if hpTooLow tb activeItems
                   then 0  -- to trigger domination, etc.
                   else max (xM calmMax) (bcalm tb) `div` 2
      deltaCalm = min minusTwoM (upperBound - bcalm tb)
  -- HP loss decreases Calm by at least minusTwoM, to overcome Calm regen,
  -- when far from shooting foe and to avoid "hears something",
  -- which is emitted for decrease @minusM@.
  execUpdAtomic $ UpdRefillCalm target deltaCalm

-- ** Hurt

-- Modified by armor.
effectHurt :: (MonadAtomic m, MonadServer m)
           => Dice.Dice -> ActorId -> ActorId -> Bool
           -> m Bool
effectHurt nDm source target silent = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  hpMax <- sumOrganEqpServer IK.EqpSlotAddMaxHP target
  n <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  hurtBonus <- armorHurtBonus source target
  let block = braced tb
      mult = (100 + hurtBonus) * (if block then 50 else 100)
      rawDeltaHP = - (max oneM  -- at least 1 HP taken
                          (fromIntegral mult * xM n `divUp` (100 * 100)))
      serious = source /= target && not (bproj tb)
      upperBound = if serious then xM hpMax else maxBound
      deltaHP = min rawDeltaHP (upperBound - bhp tb)
      deltaDiv = fromIntegral $ deltaHP `divUp` oneM
  -- Damage the target.
  execUpdAtomic $ UpdRefillHP target deltaHP
  when serious $ halveCalm target
  unless silent $ execSfxAtomic $ SfxEffect (bfid sb) target $
    if source == target
    then IK.RefillHP deltaDiv  -- no SfxStrike, so treat as any heal/wound
    else IK.Hurt (Dice.intToDice deltaDiv)  -- SfxStrike sent, avoid spam
  return True

armorHurtBonus :: (MonadAtomic m, MonadServer m)
               => ActorId -> ActorId
               -> m Int
armorHurtBonus source target = do
  sactiveItems <- activeItemsServer source
  tactiveItems <- activeItemsServer target
  sb <- getsState $ getActorBody source
  return $! if bproj sb
            then sumSlotNoFilter IK.EqpSlotAddHurtRanged sactiveItems
                 - sumSlotNoFilter IK.EqpSlotAddArmorRanged tactiveItems
            else sumSlotNoFilter IK.EqpSlotAddHurtMelee sactiveItems
                 - sumSlotNoFilter IK.EqpSlotAddArmorMelee tactiveItems

-- ** RefillCalm

effectRefillCalm ::  (MonadAtomic m, MonadServer m)
           => Bool -> m () -> Int -> ActorId -> m Bool
effectRefillCalm overfill execSfx power target = do
  tb <- getsState $ getActorBody target
  calmMax <- sumOrganEqpServer IK.EqpSlotAddMaxCalm target
  let overMax | overfill = xM calmMax * 10  -- arbitrary limit to scumming
              | otherwise = xM calmMax
  let deltaCalm = min (xM power) (max 0 $ overMax - bcalm tb)
  if deltaCalm == 0
    then return False
    else do
      execUpdAtomic $ UpdRefillCalm target deltaCalm
      execSfx
      return True

-- ** Dominate

effectDominate :: (MonadAtomic m, MonadServer m)
               => (IK.Effect Int -> m Bool)
               -> ActorId -> ActorId
               -> m Bool
effectDominate recursiveCall source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  if bproj tb then
    return False
  else if bfid tb == bfid sb then
    recursiveCall IK.Impress
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
    let hpMax = max 1 $ sumSlotNoFilter IK.EqpSlotAddMaxHP activeItems
        deltaHP = - xM hpMax `div` 3
    execUpdAtomic $ UpdRefillHP source deltaHP
    let validTile t = not $ Tile.hasFeature cotile TK.NoActor t
        lid = blid sb
    ps <- getsState $ nearbyFreePoints validTile (bpos sb) lid
    time <- getsState $ getLocalTime lid
    recruitActors (take power ps) lid time (bfid sb)

-- ** Summon

effectSummon :: (MonadAtomic m, MonadServer m)
             => (IK.Effect Int -> m Bool)
             -> Freqs ItemKind -> Int
             -> ActorId -> ActorId
             -> m Bool
effectSummon recursiveCall actorFreq power source target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  activeItems <- activeItemsServer source
  let legal = source == target && (bproj sb || calmEnough10 sb activeItems)
  if not legal then return False
  else do
    let calmMax = max 1 $ sumSlotNoFilter IK.EqpSlotAddMaxCalm activeItems
        deltaCalm = - xM calmMax `div` 3
    unless (bproj sb) $ execUpdAtomic $ UpdRefillCalm source deltaCalm
    let validTile t = not $ Tile.hasFeature cotile TK.NoActor t
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
          recursiveCall (IK.CallFriend 1)
        Just aid -> do
          b <- getsState $ getActorBody aid
          mleader <- getsState $ gleader . (EM.! bfid b) . sfactionD
          when (isNothing mleader) $
            execUpdAtomic
            $ UpdLeadFaction (bfid b) Nothing (Just (aid, Nothing))
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

-- Damage from both impact and fire. Modified by armor.
effectBurn :: (MonadAtomic m, MonadServer m)
           => m () -> Int -> ActorId -> ActorId
           -> m Bool
effectBurn execSfx power source target = do
  void $ effectHurt (Dice.intToDice power) source target True
  execSfx
  return True

-- ** Ascend

-- Note that projectiles can be teleported, too, for extra fun.
effectAscend :: (MonadAtomic m, MonadServer m)
             => (IK.Effect Int -> m Bool)
             -> m () -> Int -> ActorId
             -> m Bool
effectAscend recursiveCall execSfx k aid = do
  b1 <- getsState $ getActorBody aid
  ais1 <- getsState $ getCarriedAssocs b1
  let lid1 = blid b1
      pos1 = bpos b1
  (lid2, pos2) <- getsState $ whereTo lid1 pos1 k . sdungeon
  if lid2 == lid1 && pos2 == pos1 then do
    execSfxAtomic $ SfxMsgFid (bfid b1) "No more levels in this direction."
    recursiveCall $ IK.Teleport 30  -- powerful teleport
  else do
    let switch1 = void $ switchLevels1 ((aid, b1), ais1)
        switch2 = do
          -- Make the initiator of the stair move the leader,
          -- to let him clear the stairs for others to follow.
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
              => ((ActorId, Actor), [(ItemId, Item)])
              -> m (Maybe ActorId)
switchLevels1 ((aid, bOld), ais) = do
  let side = bfid bOld
  mleader <- getsState $ gleader . (EM.! side) . sfactionD
  -- Prevent leader pointing to a non-existing actor.
  mlead <-
    if not (bproj bOld) && isJust mleader then do
      execUpdAtomic $ UpdLeadFaction side mleader Nothing
      return $ fst <$> mleader
        -- outside of a client we don't know the real tgt of aid, hence fst
    else return Nothing
  -- Remove the actor from the old level.
  -- Onlookers see somebody disappear suddenly.
  -- @UpdDestroyActor@ is too loud, so use @UpdLoseActor@ instead.
  execUpdAtomic $ UpdLoseActor aid bOld ais
  return mlead

switchLevels2 ::(MonadAtomic m, MonadServer m)
              => LevelId -> Point
              -> ((ActorId, Actor), [(ItemId, Item)]) -> Maybe ActorId
              -> m ()
switchLevels2 lidNew posNew ((aid, bOld), ais) mlead = do
  let lidOld = blid bOld
      side = bfid bOld
  assert (lidNew /= lidOld `blame` "stairs looped" `twith` lidNew) skip
  -- Sync the actor time with the level time.
  timeOld <- getsState $ getLocalTime lidOld
  timeLastActive <- getsState $ getLocalTime lidNew
  -- This time calculation may cause a double move of a foe of the same
  -- speed, but this is OK --- the foe didn't have a chance to move
  -- before, because the arena went inactive, so he moves now one more time.
  let delta = timeLastActive `timeDeltaToFrom` timeOld
      shiftByDelta = (`timeShift` delta)
      computeNewTimeout :: ItemQuant -> ItemQuant
      computeNewTimeout (k, it) = (k, map shiftByDelta it)
      setTimeout :: ItemBag -> ItemBag
      setTimeout bag = EM.map computeNewTimeout bag
      bNew = bOld { blid = lidNew
                  , btime = shiftByDelta $ btime bOld
                  , bpos = posNew
                  , boldpos = posNew  -- new level, new direction
                  , boldlid = lidOld  -- record old level
                  , borgan = setTimeout $ borgan bOld
                  , beqp = setTimeout $ beqp bOld }
  -- Materialize the actor at the new location.
  -- Onlookers see somebody appear suddenly. The actor himself
  -- sees new surroundings and has to reset his perception.
  execUpdAtomic $ UpdCreateActor aid bNew ais
  case mlead of
    Nothing -> return ()
    Just leader ->
      execUpdAtomic $ UpdLeadFaction side Nothing (Just (leader, Nothing))

-- ** Escape

-- | The faction leaves the dungeon.
effectEscape :: (MonadAtomic m, MonadServer m) => ActorId -> m Bool
effectEscape target = do
  -- Obvious effect, nothing announced.
  b <- getsState $ getActorBody target
  let fid = bfid b
  fact <- getsState $ (EM.! fid) . sfactionD
  if not (fcanEscape $ gplayer fact) || bproj b then
    return False
  else do
    deduceQuits b $ Status Escape (fromEnum $ blid b) Nothing
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
  case strongestSlotNoFilter IK.EqpSlotWeapon allAssocs of
    (_, (iid, _)) : _ -> do
      b <- getsState $ getActorBody target
      let kit = beqp b EM.! iid
      dropEqpItem target b False iid kit
      execSfx
      return True
    [] ->
      return False

-- | Drop a single actor's item. Note that if there multiple copies,
-- at most one explodes to avoid excessive carnage and UI clutter
-- (let's say, the multiple explosions interfere with each other or perhaps
-- larger quantities of explosives tend to be packaged more safely).
dropEqpItem :: (MonadAtomic m, MonadServer m)
            => ActorId -> Actor -> Bool -> ItemId -> ItemQuant -> m ()
dropEqpItem aid b hit iid kit@(k, _) = do
  item <- getsState $ getItemBody iid
  let c = CActor aid CEqp
      fragile = IK.Fragile `elem` jfeature item
      durable = IK.Durable `elem` jfeature item
      isDestroyed = hit && not durable || bproj b && fragile
  if isDestroyed then do
    discoEffect <- getsServer sdiscoEffect
    let aspects = case EM.lookup iid discoEffect of
          Just ItemAspectEffect{jaspects} -> jaspects
          _ -> assert `failure` (aid, iid)
    itemToF <- itemToFullServer
    let itemFull = itemToF iid kit
        effs = strengthOnSmash itemFull
    effectAndDestroy aid aid iid c False effs aspects kit
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
                   -> (ItemId -> ItemQuant -> m ())
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
                 => m () -> IK.ThrowMod
                 -> ActorId -> ActorId -> Maybe Bool
                 -> m Bool
effectSendFlying execSfx IK.ThrowMod{..} source target modePush = do
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
                 || not (Tile.hasFeature cotile TK.NoActor t)
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
        let maxCount = Dice.maxDice $ IK.icount itemKind
        if itemK >= maxCount
        then do
          let c = CActor target cstore
              kit = (maxCount, take maxCount itemTimer)
          execUpdAtomic $ UpdDestroyItem iid itemBase kit c
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
        let ided = IK.Identified `elem` IK.ifeature itemKind
            itemSecret = itemNoAE itemFull
            c = CActor target cstore
            statsObvious = textAllAE False c itemFull
                           == textAllAE False c itemSecret
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
              => m () -> GroupName ItemKind -> ActorId -> m Bool
effectExplode execSfx cgroup target = do
  tb <- getsState $ getActorBody target
  let itemFreq = [(cgroup, 1)]
      container = CActor target CEqp
  m2 <- rollAndRegisterItem (blid tb) itemFreq container False
  let (iid, (ItemFull{..}, _)) = fromMaybe (assert `failure` cgroup) m2
      Point x y = bpos tb
      projectN k100 (n, _) = do
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
  maybe skip (\kit -> execUpdAtomic
                      $ UpdLoseItem iid itemBase kit container) mn3
  execSfx
  return True  -- we avoid verifying that at least one projectile got off

-- ** OneOf

effectOneOf :: (MonadAtomic m, MonadServer m)
            => (IK.Effect Int -> m Bool)
            -> [IK.Effect Int]
            -> m Bool
effectOneOf recursiveCall l = do
  ef <- rndToAction $ oneOf l
  recursiveCall ef

-- ** Recharging

effectRecharging :: (MonadAtomic m, MonadServer m)
                 => (IK.Effect Int -> m Bool)
                 -> IK.Effect Int -> Bool
                 -> m Bool
effectRecharging recursiveCall e recharged =
  if recharged
  then recursiveCall e
  else return False

-- ** CreateOrgan

effectCreateOrgan :: (MonadAtomic m, MonadServer m)
                  => ActorId -> Dice.Dice -> GroupName ItemKind
                  -> m Bool
effectCreateOrgan target nDm grp = do
  k <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) nDm
  let c = CActor target COrgan
  bagBefore <- getsState $ getCBag c
  tb <- getsState $ getActorBody target
  let litemFreq = [(grp, 1)]
  m4 <- rollItem (blid tb) litemFreq
  let (itemKnown, itemFull, seed, _) = case m4 of
        Nothing -> assert `failure` (blid tb, litemFreq, c)
        Just i4 -> i4
  itemRev <- getsServer sitemRev
  let mquant = case HM.lookup itemKnown itemRev of
        Nothing -> Nothing
        Just iid -> (iid,) <$> iid `EM.lookup` bagBefore
  case mquant of
    Just (iid, (1, afterIt)) -> do  -- already has such an item, increase timer
      let newIt = case afterIt of
            [] -> []  -- permanent item, we don't touch it
            timer : rest ->  -- we increase duration only by half delta
              let halfTurns = timeDeltaScale (Delta timeTurn) k `timeDeltaDiv` 2
                  newTimer = timer `timeShift` halfTurns
              in newTimer : rest
      when (afterIt /= newIt) $
        execUpdAtomic $ UpdTimeItem iid c afterIt newIt  -- TODO: announce
      return True
    _ -> do
      -- Multiple such items, so it's a periodic poison, etc., so just stack,
      -- resetting the timer; or no such items at all, so create one.
      iid <- registerItem (itemBase itemFull) itemKnown seed
                          (itemK itemFull) c True
      bagAfter <- getsState $ getCBag c
      localTime <- getsState $ getLocalTime (blid tb)
      let newTimer = localTime `timeShift` timeDeltaScale (Delta timeTurn) k
          (afterK, afterIt) = case iid `EM.lookup` bagAfter of
            Nothing -> assert `failure` (iid, bagAfter, c)
            Just kit -> kit
          newIt = replicate afterK newTimer
      when (afterIt /= newIt) $
        execUpdAtomic $ UpdTimeItem iid c afterIt newIt
      return True

-- ** Temporary

effectTemporary :: (MonadAtomic m, MonadServer m)
                => m () -> ActorId -> ItemId
                -> m Bool
effectTemporary execSfx source iid = do
  bag <- getsState $ getCBag $ CActor source COrgan
  case iid `EM.lookup` bag of
    Just _ -> skip  -- still some copies left of a multi-copy tmp item
    Nothing -> execSfx  -- last copy just destroyed
  return True
