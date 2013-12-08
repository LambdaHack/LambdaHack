-- | Effect semantics.
-- TODO: document
module Game.LambdaHack.Server.EffectSem
  ( -- + Semantics of effects
    itemEffect, effectSem
    -- * Assorted operations
  , createItems, addHero, spawnMonsters, electLeader, deduceKilled
  ) where

import Control.Monad
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.Key (mapWithKeyM_)
import Data.List
import Data.Maybe
import Data.Ratio ((%))
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency

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
  sb <- getsState $ getActorBody source
  discoS <- getsServer sdisco
  let ik = fromJust $ jkind discoS item
      ef = jeffect item
  b <- effectSem ef source target
  -- The effect is interesting so the item gets identified, if seen
  -- (the item is in source actor's inventory, so his position is given).
  let atomic iid = execCmdAtomic $ DiscoverA (blid sb) (bpos sb) iid ik
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
  Effect.Searching p -> effectSearching p source
  Effect.Ascend p -> effectAscend p target
  Effect.Escape -> effectEscape target

-- + Individual semantic functions for effects

-- ** NoEffect

effectNoEffect :: MonadAtomic m => ActorId -> m Bool
effectNoEffect target = do
  execSfxAtomic $ EffectD target Effect.NoEffect
  return False

-- ** Heal

effectHeal :: MonadAtomic m
           => Int -> ActorId -> m Bool
effectHeal power target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  tm <- getsState $ getActorBody target
  let bhpMax = maxDice (ahp $ okind $ bkind tm)
  if power > 0 && bhp tm >= bhpMax
    then do
      execSfxAtomic $ EffectD target Effect.NoEffect
      return False
    else do
      let deltaHP = min power (bhpMax - bhp tm)
      execCmdAtomic $ HealActorA target deltaHP
      execSfxAtomic $ EffectD target $ Effect.Heal deltaHP
      return True

-- ** Wound

effectWound :: (MonadAtomic m, MonadServer m)
            => RollDice -> Int -> ActorId -> ActorId
            -> m Bool
effectWound nDm power source target = do
  n <- rndToAction $ castDice nDm
  let deltaHP = - (n + power)
  if deltaHP >= 0
    then do
      execSfxAtomic $ EffectD target Effect.NoEffect
      return False
    else do
      -- Damage the target.
      execCmdAtomic $ HealActorA target deltaHP
      execSfxAtomic $ EffectD target $
        if source == target
        then Effect.Heal deltaHP
        else Effect.Hurt nDm deltaHP{-hack-}
      return True

-- ** Mindprobe

effectMindprobe :: MonadAtomic m
                => ActorId -> m Bool
effectMindprobe target = do
  tb <- getsState (getActorBody target)
  let lid = blid tb
  fact <- getsState $ (EM.! bfid tb) . sfactionD
  lb <- getsState $ actorNotProjList (isAtWar fact) lid
  let nEnemy = length lb
  if nEnemy == 0 then do
    execSfxAtomic $ EffectD target Effect.NoEffect
    return False
  else do
    execSfxAtomic $ EffectD target $ Effect.Mindprobe nEnemy
    return True

-- ** Dominate

effectDominate :: (MonadAtomic m, MonadServer m)
               => ActorId -> ActorId -> m Bool
effectDominate source target = do
  sb <- getsState (getActorBody source)
  tb <- getsState (getActorBody target)
  if bfid tb == bfid sb then do
    execSfxAtomic $ EffectD target Effect.NoEffect
    return False
  else do
    -- Announce domination before the actor changes sides.
    execSfxAtomic $ EffectD target Effect.Dominate
    -- TODO: Perhaps insert a turn of delay here to allow countermeasures.
    electLeader (bfid tb) (blid tb) target
    ais <- getsState $ getActorItem target
    execCmdAtomic $ LoseActorA target tb ais
    let bNew = tb {bfid = bfid sb}
    execCmdAtomic $ CreateActorA target bNew ais
    leaderOld <- getsState $ gleader . (EM.! bfid sb) . sfactionD
    -- Halve the speed as a side-effect of domination.
    let speed = bspeed bNew
        delta = speedScale (1%2) speed
    when (delta > speedZero) $
      execCmdAtomic $ HasteActorA target (speedNegate delta)
    execCmdAtomic $ LeadFactionA (bfid sb) leaderOld (Just target)
    deduceKilled tb  -- tb (not bNew), because that's how we saw him last
    return True

electLeader :: MonadAtomic m => FactionId -> LevelId -> ActorId -> m ()
electLeader fid lid aidDead = do
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  when (isNothing mleader || mleader == Just aidDead) $ do
    actorD <- getsState sactorD
    let ours (_, b) = bfid b == fid && not (bproj b)
        party = filter ours $ EM.assocs actorD
    onLevel <- getsState $ actorNotProjAssocs (== fid) lid
    let mleaderNew = listToMaybe $ filter (/= aidDead)
                     $ map fst $ onLevel ++ party
    execCmdAtomic $ LeadFactionA fid mleader mleaderNew

deduceKilled :: (MonadAtomic m, MonadServer m) => Actor -> m ()
deduceKilled body = do
  let fid = bfid body
  spawn <- getsState $ isSpawnFaction fid
  summon <- getsState $ isSummonFaction fid
  Config{configFirstDeathEnds} <- getsServer sconfig
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD
  when (not spawn && not summon
        && (isNothing mleader || configFirstDeathEnds)) $
    deduceQuits body $ Status Killed (fromEnum $ blid body) ""

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
           , cofact=Kind.Ops{okind} } <- getsState scops
  time <- getsState $ getLocalTime lid
  factionD <- getsState sfactionD
  let fact = okind $ gkind $ factionD EM.! bfid
  forM_ ps $ \ p -> do
    mk <- rndToAction $ opick (fname fact) (const True)
    if mk == heroKindId coactor
      then addHero bfid p lid [] Nothing time
      else addMonster mk bfid p lid time
  -- No leader election needed, bebause an alive actor of the same faction
  -- causes the effect, so there is already a leader.

addActor :: (MonadAtomic m, MonadServer m)
         => Kind.Id ActorKind -> FactionId -> Point -> LevelId -> Int
         -> Char -> Text -> Color.Color -> Time
         -> m ActorId
addActor mk bfid pos lid hp bsymbol bname bcolor time = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  let kind = okind mk
      speed = aspeed kind
      m = actorTemplate mk bsymbol bname bcolor speed hp Nothing pos lid time
                        bfid False
  acounter <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ acounter}
  execCmdAtomic $ CreateActorA acounter m []
  return acounter

-- TODO: apply this special treatment only to actors with symbol '@'.
-- | Create a new hero on the current level, close to the given position.
addHero :: (MonadAtomic m, MonadServer m)
        => FactionId -> Point -> LevelId -> [(Int, Text)] -> Maybe Int -> Time
        -> m ActorId
addHero bfid ppos lid configHeroNames mNumber time = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsState scops
  Faction{gcolor, gplayer} <- getsState $ (EM.! bfid) . sfactionD
  let kId = heroKindId coactor
  hp <- rndToAction $ castDice $ ahp $ okind kId
  mhs <- mapM (\n -> getsState $ \s -> tryFindHeroK s bfid n) [0..9]
  let freeHeroK = elemIndex Nothing mhs
      n = fromMaybe (fromMaybe 100 freeHeroK) mNumber
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      name | gcolor == Color.BrWhite =
        fromMaybe ("Hero" <+> showT n) $ lookup n configHeroNames
           | otherwise = playerName gplayer <+> "Hero" <+> showT n
      startHP = hp - (hp `div` 5) * min 3 n
  addActor
    kId bfid ppos lid startHP symbol name gcolor time

-- ** SpawnMonster

effectSummon :: (MonadAtomic m, MonadServer m)
             => Int -> ActorId -> m Bool
effectSummon power target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  Kind.COps{cotile} <- getsState scops
  tm <- getsState (getActorBody target)
  ps <- getsState $ nearbyFreePoints cotile (const True) (bpos tm) (blid tm)
  time <- getsState $ getLocalTime (blid tm)
  spawnMonsters (take power ps) (blid tm) (const True) time "summon"
  return True

-- | Spawn monsters of any spawn or summon faction, friendly or not.
-- To be used for spontaneous spawning of monsters and for the summon effect.
spawnMonsters :: (MonadAtomic m, MonadServer m)
              => [Point] -> LevelId -> ((FactionId, Faction) -> Bool)
              -> Time -> Text
              -> m ()
spawnMonsters ps lid filt time freqChoice = assert (not $ null ps) $ do
  Kind.COps{ coactor=Kind.Ops{opick}
           , cofact=Kind.Ops{okind} } <- getsState scops
  factionD <- getsState sfactionD
  -- TODO: rewrite with opick?
  let f (fid, fact) = let kind = okind (gkind fact)
                          g n = (n, (kind, fid))
                      in fmap g $ lookup freqChoice $ ffreq kind
  case mapMaybe f $ filter filt $ EM.assocs factionD of
    [] -> return ()  -- no faction spawns
    spawnList -> do
      let freq = toFreq "spawnMonsters" spawnList
      (spawnKind, bfid) <- rndToAction $ frequency freq
      laid <- forM ps $ \ p -> do
        mk <- rndToAction $ opick (fname spawnKind) (const True)
        addMonster mk bfid p lid time
      mleader <- getsState $ gleader . (EM.! bfid) . sfactionD
      when (isNothing mleader) $
        execCmdAtomic $ LeadFactionA bfid Nothing (Just $ head laid)

-- | Create a new monster on the level, at a given position
-- and with a given actor kind and HP.
addMonster :: (MonadAtomic m, MonadServer m)
           => Kind.Id ActorKind -> FactionId -> Point -> LevelId -> Time
           -> m ActorId
addMonster mk bfid ppos lid time = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  let kind = okind mk
  hp <- rndToAction $ castDice $ ahp kind
  addActor mk bfid ppos lid hp (asymbol kind) (aname kind) (acolor kind) time

-- ** CreateItem

effectCreateItem :: (MonadAtomic m, MonadServer m)
                 => Int -> ActorId -> m Bool
effectCreateItem power target = assert (power > 0) $ do
  -- Obvious effect, nothing announced.
  tm <- getsState $ getActorBody target
  void $ createItems power (bpos tm) (blid tm)
  return True

createItems :: (MonadAtomic m, MonadServer m)
            => Int -> Point -> LevelId -> m ()
createItems n pos lid = do
  Kind.COps{coitem} <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  Level{ldepth} <- getLevel lid
  depth <- getsState sdepth
  replicateM_ n $ do
    (item, k, _) <- rndToAction $ newItem coitem flavour discoRev ldepth depth
    itemRev <- getsServer sitemRev
    case HM.lookup item itemRev of
      Just iid ->
        -- TODO: try to avoid this case, to make items more interesting
        execCmdAtomic $ CreateItemA iid item k (CFloor lid pos)
      Nothing -> do
        icounter <- getsServer sicounter
        modifyServer $ \ser ->
          ser { sicounter = succ icounter
              , sitemRev = HM.insert item icounter (sitemRev ser)}
        execCmdAtomic $ CreateItemA icounter item k (CFloor lid pos)

-- ** ApplyPerfume

effectApplyPerfume :: MonadAtomic m
                   => ActorId -> ActorId -> m Bool
effectApplyPerfume source target =
  if source == target
  then do
    execSfxAtomic $ EffectD target Effect.NoEffect
    return False
  else do
    tm <- getsState $ getActorBody target
    Level{lsmell} <- getLevel $ blid tm
    let f p fromSm =
          execCmdAtomic $ AlterSmellA (blid tm) p (Just fromSm) Nothing
    mapWithKeyM_ f lsmell
    execSfxAtomic $ EffectD target Effect.ApplyPerfume
    return True

-- ** Regeneration

-- ** Searching

-- TODO or to remove.
effectSearching :: MonadAtomic m => Int -> ActorId -> m Bool
effectSearching power source = do
  execSfxAtomic $ EffectD source $ Effect.Searching power
  return True

-- ** Ascend

effectAscend :: MonadAtomic m => Int -> ActorId -> m Bool
effectAscend power target = do
  b <- effLvlGoUp target power
  when b $ execSfxAtomic $ EffectD target $ Effect.Ascend power
  return b

effLvlGoUp :: MonadAtomic m => ActorId -> Int -> m Bool
effLvlGoUp aid k = do
  b1 <- getsState $ getActorBody aid
  ais1 <- getsState $ getActorItem aid
  let lid1 = blid b1
      pos1 = bpos b1
  (lid2, pos2) <- getsState $ whereTo lid1 pos1 k
  if lid2 == lid1 && pos2 == pos1 then return False
  else do
    -- The actor is added to the new level, but there can be other actors
    -- at his new position.
    inhabitants <- getsState $ posToActor pos2 lid2
    case inhabitants of
      Nothing ->
        -- Move the actor out of the way.
        switchLevels1 aid
      Just aid2 -> do
        b2 <- getsState $ getActorBody aid2
        ais2 <- getsState $ getActorItem aid2
        -- Alert about the switch.
        let part2 = partActor b2
            verb = "be pushed to another level"
            msg2 = makeSentence [MU.SubjectVerbSg part2 verb]
        execSfxAtomic $ MsgFidD (bfid b2) msg2
        -- Move the actor out of the way.
        switchLevels1 aid
        -- Move the inhabitant out of the way.
        switchLevels1 aid2
        -- Move the inhabitant to where the actor was.
        switchLevels2 aid2 b2 ais2 lid1 pos1
    -- Move the actor to where the inhabitant was, if any.
    switchLevels2 aid b1 ais1 lid2 pos2
    -- Verify only one actor on every tile.
    !_ <- getsState $ posToActor pos1 lid1  -- assertion is inside
    !_ <- getsState $ posToActor pos2 lid2  -- assertion is inside
    return True

switchLevels1 :: MonadAtomic m => ActorId -> m ()
switchLevels1 aid = do
  bOld <- getsState $ getActorBody aid
  ais <- getsState $ getActorItem aid
  let side = bfid bOld
  mleader <- getsState $ gleader . (EM.! side) . sfactionD
  -- Prevent leader pointing to a non-existing actor.
  when (isJust mleader) $  -- trouble, if the actors are of the same faction
    execCmdAtomic $ LeadFactionA side mleader Nothing
  -- Remove the actor from the old level.
  -- Onlookers see somebody disappear suddenly.
  -- @DestroyActorA@ is too loud, so use @LoseActorA@ instead.
  execCmdAtomic $ LoseActorA aid bOld ais

switchLevels2 :: MonadAtomic m
              => ActorId -> Actor -> [(ItemId, Item)] -> LevelId -> Point
              -> m ()
switchLevels2 aid bOld ais lidNew posNew = do
  let lidOld = blid bOld
      side = bfid bOld
  assert (lidNew /= lidOld `blame` "stairs looped" `twith` lidNew) skip
  -- Sync the actor time with the level time.
  timeOld <- getsState $ getLocalTime lidOld
  timeLastVisited <- getsState $ getLocalTime lidNew
  -- This time calculation may cause a double move of a foe of the same
  -- speed, but this is OK --- the foe didn't have a chance to move
  -- before, because the arena went inactive, so he moves now one more time.
  let delta = timeAdd (btime bOld) (timeNegate timeOld)
      bNew = bOld { blid = lidNew
                  , btime = timeAdd timeLastVisited delta
                  , bwait = timeZero  -- no longer braced
                  , bpos = posNew
                  , boldpos = posNew  -- new level, new direction
                  , bpath = Nothing }
  mleader <- getsState $ gleader . (EM.! side) . sfactionD
  -- Materialize the actor at the new location.
  -- Onlookers see somebody appear suddenly. The actor himself
  -- sees new surroundings and has to reset his perception.
  execCmdAtomic $ CreateActorA aid bNew ais
  -- Changing levels is so important, that the leader changes.
  -- This also helps the actor clear the staircase and so avoid
  -- being pushed back to the level he came from by another actor.
  when (isNothing mleader) $  -- trouble, if the actors are of the same faction
    execCmdAtomic $ LeadFactionA side Nothing (Just aid)

-- ** Escape

-- | The faction leaves the dungeon.
effectEscape :: (MonadAtomic m, MonadServer m) => ActorId -> m Bool
effectEscape aid = do
  -- Obvious effect, nothing announced.
  b <- getsState $ getActorBody aid
  let fid = bfid b
  spawn <- getsState $ isSpawnFaction fid
  summon <- getsState $ isSummonFaction fid
  if spawn || summon then return False
  else do
    deduceQuits b $ Status Escape (fromEnum $ blid b) ""
    return True
