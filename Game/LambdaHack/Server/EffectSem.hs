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
import Control.Monad.Writer.Strict (WriterT)
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
import Game.LambdaHack.CmdAtomic
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
itemEffect :: MonadServer m
           => ActorId -> ActorId -> Maybe ItemId -> Item
           -> WriterT [Atomic] m ()
itemEffect source target miid item = do
  tb <- getsState $ getActorBody target
  discoS <- getsServer sdisco
  let ik = fromJust $ jkind discoS item
      ef = jeffect item
  b <- effectSem ef source target
  -- The effect is interesting so the item gets identified, if seen.
  let atomic iid = tellCmdAtomic $ DiscoverA (blid tb) (bpos tb) iid ik
  when b $ maybe skip atomic miid

-- | The source actor affects the target actor, with a given effect and power.
-- Both actors are on the current level and can be the same actor.
-- The boolean result indicates if the effect was spectacular enough
-- for the actors to identify it (and the item that caused it, if any).
effectSem :: MonadServer m
          => Effect.Effect Int -> ActorId -> ActorId
          -> WriterT [Atomic] m Bool
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

effectHeal :: MonadActionRO m
           => Int -> ActorId -> WriterT [Atomic] m Bool
effectHeal power target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  tm <- getsState $ getActorBody target
  let bhpMax = maxDice (ahp $ okind $ bkind tm)
  if power > 0 && bhp tm >= bhpMax
    then do
      tellDescAtomic $ EffectD target Effect.NoEffect
      return False
    else do
      let deltaHP = min power (bhpMax - bhp tm)
      tellCmdAtomic $ HealActorA target deltaHP
      tellDescAtomic $ EffectD target $ Effect.Heal deltaHP
      return True

-- ** Wound

effectWound :: MonadServer m
            => RollDice -> Int -> ActorId -> ActorId
            -> WriterT [Atomic] m Bool
effectWound nDm power source target = do
  n <- rndToAction $ rollDice nDm
  let deltaHP = - (n + power)
  if deltaHP >= 0
    then return False
    else do
      -- Damage the target.
      tellCmdAtomic $ HealActorA target deltaHP
      if source == target then
        tellDescAtomic $ EffectD target $ Effect.Heal deltaHP
      else
        tellDescAtomic $ EffectD target $ Effect.Hurt nDm power
      return True

-- ** Mindprobe

effectMindprobe :: MonadActionRO m
                => ActorId -> WriterT [Atomic] m Bool
effectMindprobe target = do
  tb <- getsState (getActorBody target)
  let arena = blid tb
  genemy <- getsState $ genemy . (EM.! bfaction tb) . sfaction
  lb <- getsState $ actorNotProjList (`elem` genemy) arena
  let nEnemy = length lb
  if nEnemy == 0 then do
    tellDescAtomic $ EffectD target Effect.NoEffect
    return False
  else do
    tellDescAtomic $ EffectD target $ Effect.Mindprobe nEnemy
    return True

-- ** Dominate

effectDominate :: MonadActionRO m
               => ActorId -> ActorId -> WriterT [Atomic] m Bool
effectDominate source target = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  sb <- getsState (getActorBody source)
  tb <- getsState (getActorBody target)
  if bfaction tb == bfaction sb then do
    tellDescAtomic $ EffectD target Effect.NoEffect
    return False
  else do
    -- Halve the speed as a side-effect of domination.
    let speed = fromMaybe (aspeed $ okind $ bkind tb) $ bspeed tb
        delta = speedScale (1%2) speed
    when (delta > speedZero) $
      tellCmdAtomic $ HasteActorA target (speedNegate delta)
    -- TODO: Perhaps insert a turn of delay here to allow countermeasures.
    tellCmdAtomic $ DominateActorA target (bfaction tb) (bfaction sb)
    leaderOld <- getsState $ gleader . (EM.! bfaction sb) . sfaction
    tellCmdAtomic $ LeadFactionA (bfaction sb) leaderOld (Just target)
    return True

-- ** SummonFriend

effectSummonFriend :: MonadServer m
                   => Int -> ActorId -> ActorId
                   -> WriterT [Atomic] m Bool
effectSummonFriend power source target = do
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
  tellCmdAtomic $ CreateActorA acounter m []
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
                   => Int -> ActorId -> WriterT [Atomic] m Bool
effectSpawnMonster power target = do
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
                 => Int -> ActorId -> WriterT [Atomic] m Bool
effectCreateItem power target = do
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
        tellCmdAtomic $ CreateItemA iid item k (CFloor lid pos)
      Nothing -> do
        icounter <- getsServer sicounter
        modifyServer $ \ser ->
          ser { sicounter = succ icounter
              , sitemRev = HM.insert item icounter (sitemRev ser)}
        tellCmdAtomic $ CreateItemA icounter item k (CFloor lid pos)

-- ** ApplyPerfume

effectApplyPerfume :: MonadActionRO m
                   => ActorId -> ActorId -> WriterT [Atomic] m Bool
effectApplyPerfume source target =
  if source == target
  then do
    tellDescAtomic $ EffectD target Effect.NoEffect
    return False
  else do
    tm <- getsState $ getActorBody target
    oldSmell <- getsLevel (blid tm) lsmell
    let diffL = map (\(p, sm) -> (p, (Just sm, Nothing))) $ EM.assocs oldSmell
    tellCmdAtomic $ AlterSmellA (blid tm) diffL
    tellDescAtomic $ EffectD target Effect.ApplyPerfume
    return True

-- ** Regeneration

-- ** Searching

effectSearching :: Monad m => Int -> ActorId -> WriterT [Atomic] m Bool
effectSearching power source = do
  tellDescAtomic $ EffectD source $ Effect.Searching power
  return True

-- ** Ascend

effectAscend :: MonadServer m
             => Int -> ActorId -> WriterT [Atomic] m Bool
effectAscend power target = do
  effLvlGoUp target (power + 1)
  tellDescAtomic $ EffectD target $ Effect.Ascend power
  return True

effLvlGoUp :: MonadServer m => ActorId -> Int -> WriterT [Atomic] m ()
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
        timeOld <- getsState $ getTime arenaOld
        -- Leader always set to the actor changing levels.
        mleader <- getsState $ gleader . (EM.! side) . sfaction
        tellCmdAtomic $ LeadFactionA side mleader Nothing
        -- Remove the actor from the old level.
        -- Onlookers see somebody uses a staircase.
        -- No need to report that he disappears.
        tellCmdAtomic $ LoseActorA aid bOld ais
        -- Sync the actor time with the level time.
        timeLastVisited <- getsState $ getTime arenaNew
        let delta = timeAdd (btime bOld) (timeNegate timeOld)
            bNew = bOld { blid = arenaNew
                        , btime = timeAdd timeLastVisited delta
                        , bpos = posNew }
        -- The actor is added to the new level, but there can be other actors
        -- at his new position.
        inhabitants <- getsState $ posToActor posNew arenaNew
        -- Onlookers see somebody appear suddenly. The actor himself
        -- sees new surroundings and has to reset his perception.
        tellCmdAtomic $ CreateActorA aid bNew ais
        tellCmdAtomic $ LeadFactionA side Nothing (Just aid)
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
fleeDungeon :: MonadActionRO m
            => FactionId -> LevelId -> WriterT [Atomic] m ()
fleeDungeon fid lid = do
  (_, total) <- getsState $ calculateTotal fid lid
  oldSt <- getsState $ gquit . (EM.! fid) . sfaction
  if total == 0
    then tellCmdAtomic $ QuitFactionA fid oldSt $ Just (False, Victor)
    else tellCmdAtomic $ QuitFactionA fid oldSt $ Just (True, Victor)

-- TODO: refactor with actorAttackActor or perhaps move aside the other
-- actor or swap positions with it, instead of squashing.
squashActor :: MonadServer m
            => ActorId -> ActorId -> WriterT [Atomic] m ()
squashActor source target = do
  Kind.COps{coitem=Kind.Ops{okind, ouniqGroup}} <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  let h2hKind = ouniqGroup "weight"
      kind = okind h2hKind
      item = buildItem flavour discoRev h2hKind kind
             $ Effect.Hurt (RollDice 99 99) 42
  itemEffect source target Nothing item
  tellDescAtomic $ StrikeD source target item HitD
--      verb = iverbApply kind
--        , "in a staircase accident" ]
  actorD <- getsState sactorD
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (target `EM.member` actorD)
          `blame` (source, target, "not killed")) skip

-- ** Descend

effectDescend :: MonadServer m
              => Int -> ActorId -> WriterT [Atomic] m Bool
effectDescend power target = do
  effLvlGoUp target (- (power + 1))
  tellDescAtomic $ EffectD target $ Effect.Descend power
  return True
