{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Effect semantics.
-- TODO: document
module Game.LambdaHack.Server.EffectSem where

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
effectSem :: MonadServer m
          => Effect.Effect -> ActorId -> ActorId -> Int
          -> WriterT [Atomic] m Bool
effectSem effect source target power = case effect of
  Effect.NoEffect -> effectNoEffect
  Effect.Heal -> effectHeal target power
  Effect.Wound nDm -> effectWound nDm source target power
  Effect.Mindprobe _ -> effectMindprobe target
  Effect.Dominate | source /= target -> effectDominate source target
  Effect.Dominate -> effectSem (Effect.Mindprobe undefined) source target power
  Effect.SummonFriend -> effectSummonFriend source target power
  Effect.SpawnMonster -> effectSpawnMonster target power
  Effect.CreateItem -> effectCreateItem target power
  Effect.ApplyPerfume -> effectApplyPerfume source target
  Effect.Regeneration -> effectSem Effect.Heal source target power
  Effect.Searching -> effectSearching source
  Effect.Ascend -> effectAscend target power
  Effect.Descend -> effectDescend target power

-- TODO: when h2h items have ItemId, replace Item with ItemId
-- | The source actor affects the target actor, with a given item.
-- If the event is seen, the item may get identified. This function
-- is mutually recursive with @effect@ and so it's a part of @Effect@
-- semantics.
itemEffect :: MonadServer m
           => ActorId -> ActorId -> Maybe ItemId -> Item
           -> WriterT [Atomic] m ()
itemEffect source target miid item = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  -- Destroys attacking actor: a hack for projectiles.
  when (bproj sb) $ tellCmdAtomic $ DestroyActorA source sb
  discoS <- getsState sdisco
  let ik = fromJust $ jkind discoS item
      ef = ieffect $ okind ik
  b <- effectSem ef source target (jpower item)
  -- The effect is interesting so the item gets identified, if seen.
  let atomic iid = tellCmdAtomic $ DiscoverA (blid tb) (bpos tb) iid ik
  when b $ maybe (return ()) atomic miid

-- + Individual semantic functions for effects

-- ** NoEffect

effectNoEffect :: MonadActionAbort m => m Bool
effectNoEffect = return False

-- ** Heal

effectHeal :: MonadActionRO m
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

effectMindprobe :: MonadActionRO m
                => ActorId -> WriterT [Atomic] m Bool
effectMindprobe target = do
  tb <- getsState (getActorBody target)
  let arena = blid tb
  genemy <- getsState $ genemy . (EM.! bfaction tb) . sfaction
  lb <- getsState $ actorNotProjList (`elem` genemy) arena
  let nEnemy = length lb
  if nEnemy == 0 then do
    tellDescAtomic $ EffectA target Effect.NoEffect
    return False
  else do
    tellDescAtomic $ EffectA target $ Effect.Mindprobe nEnemy
    return True

-- ** Dominate

effectDominate :: MonadActionRO m
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

effectAscend :: MonadServer m
             => ActorId -> Int -> WriterT [Atomic] m Bool
effectAscend target power = do
  effLvlGoUp target (power + 1)
  tellDescAtomic $ EffectA target Effect.Ascend
  return True

effLvlGoUp :: MonadServer m => ActorId -> Int -> WriterT [Atomic] m ()
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

-- | The faction leaves the dungeon.
fleeDungeon :: MonadActionRO m
            => FactionId -> LevelId -> WriterT [Atomic] m ()
fleeDungeon fid lid = do
  (_, total) <- getsState $ calculateTotal fid lid
  oldSt <- getsState $ gquit . (EM.! fid) . sfaction
  if total == 0
    then tellCmdAtomic $ QuitFactionA fid oldSt $ Just (False, Victor)
    else tellCmdAtomic $ QuitFactionA fid oldSt $ Just (True, Victor)

-- TODO: refactor with actorAttackActor or perhaps displace the other
-- actor or swap positions with it, instead of squashing.
squashActor :: MonadServer m
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
  itemEffect source target Nothing h2h
  s <- getState
  -- The monster has to be killed first, before we step there (same turn!).
  assert (not (target `EM.member` sactorD s) `blame` (source, target, "not killed")) $
    return ()

-- ** Descend

effectDescend :: MonadServer m
              => ActorId -> Int -> WriterT [Atomic] m Bool
effectDescend target power = do
  effLvlGoUp target (- (power + 1))
  tellDescAtomic $ EffectA target Effect.Descend
  return True
