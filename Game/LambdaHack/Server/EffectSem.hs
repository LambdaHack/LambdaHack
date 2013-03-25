{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Effect semantics.
-- TODO: document
module Game.LambdaHack.Server.EffectSem
  ( -- + Semantics of effects
    itemEffect, effectSem
    -- * Assorted operations
  , createItems, addHero, spawnMonsters
  ) where

import Control.Monad
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Ratio ((%))
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency

default (Text)

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
  tb <- getsState $ getActorBody target
  discoS <- getsServer sdisco
  let ik = fromJust $ jkind discoS item
      ef = jeffect item
  b <- effectSem ef source target
  -- The effect is interesting so the item gets identified, if seen.
  let atomic iid = execCmdAtomic $ DiscoverA (blid tb) (bpos tb) iid ik
  when b $ maybe skip atomic miid

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
effectSem :: (MonadAtomic m, MonadServer m)
          => Effect.Effect Int -> ActorId -> ActorId
          -> m Bool
effectSem effect source target = case effect of
  Effect.NoEffect -> effectNoEffect
  Effect.Heal p -> effectHeal p target
  Effect.Hurt nDm p -> effectWound nDm p source target
  Effect.Mindprobe _ -> effectMindprobe target
  Effect.Dominate | source /= target -> effectDominate source target
  Effect.Dominate -> effectSem (Effect.Mindprobe undefined) source target
  Effect.SummonFriend p -> effectSummonFriend p source target
  Effect.SpawnMonster p -> effectSpawnMonster p target
  Effect.CreateItem p -> effectCreateItem p target
  Effect.ApplyPerfume -> effectApplyPerfume source target
  Effect.Regeneration p -> effectSem (Effect.Heal p) source target
  Effect.Searching p -> effectSearching p source
  Effect.Ascend p -> effectAscend p target
  Effect.Descend p -> effectDescend p target

-- + Individual semantic functions for effects

-- ** NoEffect

effectNoEffect :: Monad m => m Bool
effectNoEffect = return False

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
  n <- rndToAction $ rollDice nDm
  let deltaHP = - (n + power)
  if deltaHP >= 0
    then return False
    else do
      -- Damage the target.
      execCmdAtomic $ HealActorA target deltaHP
      if source == target then
        execSfxAtomic $ EffectD target $ Effect.Heal deltaHP
      else
        execSfxAtomic $ EffectD target $ Effect.Hurt nDm power
      return True

-- ** Mindprobe

effectMindprobe :: MonadAtomic m
                => ActorId -> m Bool
effectMindprobe target = do
  tb <- getsState (getActorBody target)
  let arena = blid tb
  genemy <- getsState $ genemy . (EM.! bfaction tb) . sfaction
  lb <- getsState $ actorNotProjList (`elem` genemy) arena
  let nEnemy = length lb
  if nEnemy == 0 then do
    execSfxAtomic $ EffectD target Effect.NoEffect
    return False
  else do
    execSfxAtomic $ EffectD target $ Effect.Mindprobe nEnemy
    return True

-- ** Dominate

effectDominate :: MonadAtomic m
               => ActorId -> ActorId -> m Bool
effectDominate source target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  sb <- getsState (getActorBody source)
  tb <- getsState (getActorBody target)
  if bfaction tb == bfaction sb then do
    execSfxAtomic $ EffectD target Effect.NoEffect
    return False
  else do
    -- Halve the speed as a side-effect of domination.
    let speed = fromMaybe (aspeed $ okind $ bkind tb) $ bspeed tb
        delta = speedScale (1%2) speed
    when (delta > speedZero) $
      execCmdAtomic $ HasteActorA target (speedNegate delta)
    -- TODO: Perhaps insert a turn of delay here to allow countermeasures.
    execCmdAtomic $ DominateActorA target (bfaction tb) (bfaction sb)
    leaderOld <- getsState $ gleader . (EM.! bfaction sb) . sfaction
    execCmdAtomic $ LeadFactionA (bfaction sb) leaderOld (Just target)
    return True

-- ** SummonFriend

effectSummonFriend :: (MonadAtomic m, MonadServer m)
                   => Int -> ActorId -> ActorId
                   -> m Bool
effectSummonFriend power source target = do
  Kind.COps{cotile} <- getsState scops
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  ps <- getsState $ nearbyFreePoints cotile (bpos tm) (blid tm)
  summonFriends (bfaction sm) (take power ps) (blid tm)
  return True

summonFriends :: (MonadAtomic m, MonadServer m)
              => FactionId -> [Point] -> LevelId
              -> m ()
summonFriends bfaction ps arena = do
  Kind.COps{ coactor=coactor@Kind.Ops{opick}
           , cofact=Kind.Ops{okind} } <- getsState scops
  faction <- getsState sfaction
  let fact = okind $ gkind $ faction EM.! bfaction
  forM_ ps $ \ p -> do
    mk <- rndToAction $ opick (fname fact) (const True)
    if mk == heroKindId coactor
      then addHero bfaction p arena [] Nothing
      else addMonster mk bfaction p arena
  -- No leader election needed, bebause an alive actor of the same faction
  -- causes the effect, so there is already a leader.

addActor :: (MonadAtomic m, MonadServer m)
         => Kind.Id ActorKind -> FactionId -> Point -> LevelId -> Int
         -> Maybe Char -> Maybe Text
         -> m ActorId
addActor mk bfaction pos lid hp msymbol mname = do
  time <- getsState $ getLocalTime lid
  let m = actorTemplate mk msymbol mname hp pos lid time bfaction False
  acounter <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ acounter}
  execCmdAtomic $ CreateActorA acounter m []
  return acounter

-- TODO: apply this special treatment only to actors with symbol '@'.
-- | Create a new hero on the current level, close to the given position.
addHero :: (MonadAtomic m, MonadServer m)
        => FactionId -> Point -> LevelId -> [(Int, Text)] -> Maybe Int
        -> m ActorId
addHero bfaction ppos arena configHeroNames mNumber = do
  Kind.COps{coactor=coactor@Kind.Ops{okind}} <- getsState scops
  let mk = heroKindId coactor
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  mhs <- mapM (\n -> getsState $ \s -> tryFindHeroK s bfaction n) [0..9]
  let freeHeroK = elemIndex Nothing mhs
      n = fromMaybe (fromMaybe 100 freeHeroK) mNumber
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

effectSpawnMonster :: (MonadAtomic m, MonadServer m)
                   => Int -> ActorId -> m Bool
effectSpawnMonster power target = do
  Kind.COps{cotile} <- getsState scops
  tm <- getsState (getActorBody target)
  ps <- getsState $ nearbyFreePoints cotile (bpos tm) (blid tm)
  spawnMonsters (take power ps) (blid tm)
  return True

-- | Spawn monsters of any spawning faction, friendly or not.
-- To be used for spontaneous spawning of monsters and for the spawning effect.
spawnMonsters :: (MonadAtomic m, MonadServer m)
              => [Point] -> LevelId
              -> m ()
spawnMonsters ps arena = assert (not $ null ps) $ do
  Kind.COps{ coactor=Kind.Ops{opick}
           , cofact=Kind.Ops{okind} } <- getsState scops
  faction <- getsState sfaction
  let f (fid, fa) =
        let kind = okind (gkind fa)
        in if fspawn kind <= 0
           then Nothing
           else Just (fspawn kind, (kind, fid))
  case catMaybes $ map f $ EM.assocs faction of
    [] -> return ()  -- no faction spawns
    spawnList -> do
      let freq = toFreq "spawn" spawnList
      (spawnKind, bfaction) <- rndToAction $ frequency freq
      laid <- forM ps $ \ p -> do
        mk <- rndToAction $ opick (fname spawnKind) (const True)
        addMonster mk bfaction p arena
      mleader <- getsState $ gleader . (EM.! bfaction) . sfaction
      when (mleader == Nothing) $ do
        execCmdAtomic $ LeadFactionA bfaction Nothing (Just $ head laid)
        execSfxAtomic $ FadeinD bfaction True

-- | Create a new monster on the level, at a given position
-- and with a given actor kind and HP.
addMonster :: (MonadAtomic m, MonadServer m)
           => Kind.Id ActorKind -> FactionId -> Point -> LevelId
           -> m ActorId
addMonster mk bfaction ppos lid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  hp <- rndToAction $ rollDice $ ahp $ okind mk
  addActor mk bfaction ppos lid hp Nothing Nothing

-- ** CreateItem

effectCreateItem :: (MonadAtomic m, MonadServer m)
                 => Int -> ActorId -> m Bool
effectCreateItem power target = do
  tm <- getsState $ getActorBody target
  void $ createItems power (bpos tm) (blid tm)
  return True

createItems :: (MonadAtomic m, MonadServer m)
            => Int -> Point -> LevelId -> m ()
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
    oldSmell <- getsLevel (blid tm) lsmell
    let f (p, fromSm) =
          execCmdAtomic $ AlterSmellA (blid tm) p (Just fromSm) Nothing
    mapM_ f $ EM.assocs oldSmell
    execSfxAtomic $ EffectD target Effect.ApplyPerfume
    return True

-- ** Regeneration

-- ** Searching

effectSearching :: MonadAtomic m => Int -> ActorId -> m Bool
effectSearching power source = do
  execSfxAtomic $ EffectD source $ Effect.Searching power
  return True

-- ** Ascend

effectAscend :: (MonadAtomic m, MonadServer m)
             => Int -> ActorId -> m Bool
effectAscend power target = do
  effLvlGoUp target power
  execSfxAtomic $ EffectD target $ Effect.Ascend power
  return True

effLvlGoUp :: (MonadAtomic m, MonadServer m) => ActorId -> Int -> m ()
effLvlGoUp aid k = do
  bOld <- getsState $ getActorBody aid
  ais <- getsState $ getActorItem aid
  let arenaOld = blid bOld
      side = bfaction bOld
  whereto <- getsState $ \s -> whereTo s arenaOld k
  case whereto of
    Nothing -> -- We are at the "end" of the dungeon.
               fleeDungeon side arenaOld
    Just (arenaNew, posNew) ->
      assert (arenaNew /= arenaOld `blame` (arenaNew, "stairs looped")) $ do
        timeOld <- getsState $ getLocalTime arenaOld
        -- Leader always set to the actor changing levels.
        mleader <- getsState $ gleader . (EM.! side) . sfaction
        execCmdAtomic $ LeadFactionA side mleader Nothing
        -- Remove the actor from the old level.
        -- Onlookers see somebody uses a staircase.
        -- No need to report that he disappears.
        execCmdAtomic $ LoseActorA aid bOld ais
        -- Sync the actor time with the level time.
        timeLastVisited <- getsState $ getLocalTime arenaNew
        let delta = timeAdd (btime bOld) (timeNegate timeOld)
            bNew = bOld { blid = arenaNew
                        , btime = timeAdd timeLastVisited delta
                        , bpos = posNew }
        -- The actor is added to the new level, but there can be other actors
        -- at his new position.
        inhabitants <- getsState $ posToActor posNew arenaNew
        -- Onlookers see somebody appear suddenly. The actor himself
        -- sees new surroundings and has to reset his perception.
        execCmdAtomic $ CreateActorA aid bNew ais
        execCmdAtomic $ LeadFactionA side Nothing (Just aid)
        case inhabitants of
          Nothing -> return ()
                     -- TODO: Bail out if a friend blocks the staircase.
          Just m ->
            -- Squash the actor blocking the staircase.
            squashActor aid m
        -- Verify the actor on the staircase died, so only one actor left.
        void $ getsState $ posToActor posNew arenaNew
        -- The property of at most one actor on a tile is restored.

-- | The faction leaves the dungeon.
fleeDungeon :: MonadAtomic m
            => FactionId -> LevelId -> m ()
fleeDungeon fid lid = do
  (_, total) <- getsState $ calculateTotal fid lid
  oldSt <- getsState $ gquit . (EM.! fid) . sfaction
  if total == 0
    then execCmdAtomic $ QuitFactionA fid oldSt $ Just (False, Victor)
    else execCmdAtomic $ QuitFactionA fid oldSt $ Just (True, Victor)

-- TODO: refactor with actorAttackActor or perhaps move aside the other
-- actor or swap positions with it, instead of squashing.
squashActor :: (MonadAtomic m, MonadServer m)
            => ActorId -> ActorId -> m ()
squashActor source target = do
  Kind.COps{coitem=Kind.Ops{okind, ouniqGroup}} <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  let h2hKind = ouniqGroup "weight"
      kind = okind h2hKind
      item = buildItem flavour discoRev h2hKind kind
             $ Effect.Hurt (RollDice 99 99) 42
  itemEffect source target Nothing item
  execSfxAtomic $ StrikeD source target item HitD
--      verb = iverbApply kind
--        , "in a staircase accident" ]
  actorD <- getsState sactorD
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (target `EM.member` actorD)
          `blame` (source, target, "not killed")) skip

-- ** Descend

effectDescend :: (MonadAtomic m, MonadServer m)
              => Int -> ActorId -> m Bool
effectDescend power target = do
  effLvlGoUp target (-power)
  execSfxAtomic $ EffectD target $ Effect.Descend power
  return True
