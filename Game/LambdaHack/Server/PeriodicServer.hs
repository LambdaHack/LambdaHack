-- | Server operations performed periodically in the game loop
-- and related operations.
module Game.LambdaHack.Server.PeriodicServer
  ( spawnMonsters, generateMonster, addMonster, addHero, dominateFid
  , advanceTime, regenerateLevelHP, leadLevelFlip
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.CommonServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- | Spawn non-hero actors of any faction, friendly or not.
-- To be used for initial dungeon population, spontaneous spawning
-- of monsters and for the summon effect.
spawnMonsters :: (MonadAtomic m, MonadServer m)
              => [Point] -> LevelId -> Time -> FactionId
              -> m ()
spawnMonsters ps lid time fid = assert (not $ null ps) $ do
  Kind.COps{coactor=Kind.Ops{opick}, cofaction=Kind.Ops{okind}} <- getsState scops
  fact <- getsState $ (EM.! fid) . sfactionD
  let spawnName = fname $ okind $ gkind fact
  laid <- forM ps $ \ p -> do
    ak <- rndToAction $ fmap (fromMaybe $ assert `failure` spawnName)
                        $ opick spawnName (const True)
    addMonster ak fid p lid time
  mleader <- getsState $ gleader . (EM.! fid) . sfactionD  -- just changed
  when (isNothing mleader) $
    execUpdAtomic $ UpdLeadFaction fid Nothing (Just $ head laid)

-- | Generate a monster, possibly.
generateMonster :: (MonadAtomic m, MonadServer m) => LevelId -> m ()
generateMonster lid = do
  cops <- getsState scops
  pers <- getsServer sper
  lvl@Level{ldepth} <- getLevel lid
  s <- getState
  let f fid = isSpawnFaction fid s
      spawns = actorRegularList f lid s
  depth <- getsState sdepth
  rc <- rndToAction $ monsterGenChance ldepth depth (length spawns)
  factionD <- getsState sfactionD
  when rc $ do
    time <- getsState $ getLocalTime lid
    let freq = toFreq "spawn"
               $ map (\(fid, fact) -> (playerSpawn $ gplayer fact, fid))
               $ EM.assocs factionD
    mfid <- if nullFreq freq then
              return Nothing
            else fmap Just $ rndToAction $ frequency freq
    case mfid of
      Nothing -> return ()  -- no faction spawns
      Just fid -> do
        let allPers = ES.unions $ map (totalVisible . (EM.! lid))
                      $ EM.elems $ EM.delete fid pers  -- expensive :(
        pos <- rndToAction $ rollSpawnPos cops allPers lid lvl fid s
        spawnMonsters [pos] lid time fid

-- | Create a new monster on the level, at a given position
-- and with a given actor kind and HP.
addMonster :: (MonadAtomic m, MonadServer m)
           => Kind.Id ActorKind -> FactionId -> Point -> LevelId -> Time
           -> m ActorId
addMonster ak bfid ppos lid time = do
  cops@Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  let kind = okind ak
  hp <- rndToAction $ castDice 0 0 $ ahp kind
  calm <- rndToAction $ castDice 0 0 $ acalm kind
  fact <- getsState $ (EM.! bfid) . sfactionD
  pronoun <- if isCivilianFact cops fact
             then rndToAction $ oneOf ["he", "she"]
             else return "it"
  addActor ak bfid ppos lid hp calm (asymbol kind) (aname kind) pronoun
           (acolor kind) time

-- | Create a new hero on the current level, close to the given position.
addHero :: (MonadAtomic m, MonadServer m)
        => FactionId -> Point -> LevelId -> [(Int, (Text, Text))]
        -> Maybe Int -> Time
        -> m ActorId
addHero bfid ppos lid heroNames mNumber time = do
  Kind.COps{ coactor=Kind.Ops{okind, opick}
           , cofaction=Kind.Ops{okind=fokind} } <- getsState scops
  Faction{gcolor, gplayer, gkind} <- getsState $ (EM.! bfid) . sfactionD
  let fName = fname $ fokind gkind
  ak <- rndToAction $ fmap (fromMaybe $ assert `failure` fName)
                      $ opick fName (const True)
  let akind = okind ak
  hp <- rndToAction $ castDice 0 0 $ ahp akind
  calm <- rndToAction $ castDice 0 0 $ acalm akind
  mhs <- mapM (\n -> getsState $ \s -> tryFindHeroK s bfid n) [0..9]
  let freeHeroK = elemIndex Nothing mhs
      n = fromMaybe (fromMaybe 100 freeHeroK) mNumber
      symbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      nameFromNumber 0 = ("Captain", "he")
      nameFromNumber k | k `mod` 7 == 0 = ("Heroine" <+> tshow k, "she")
      nameFromNumber k = ("Hero" <+> tshow k, "he")
      (name, pronoun) | gcolor == Color.BrWhite =
        fromMaybe (nameFromNumber n) $ lookup n heroNames
           | otherwise =
        let (nameN, pronounN) = nameFromNumber n
        in (playerName gplayer <+> nameN, pronounN)
      startHP = hp - (min 10 $ hp `div` 10) * min 5 n
  addActor ak bfid ppos lid startHP calm symbol name pronoun gcolor time

addActor :: (MonadAtomic m, MonadServer m)
         => Kind.Id ActorKind -> FactionId -> Point -> LevelId -> Int -> Int
         -> Char -> Text -> Text -> Color.Color -> Time
         -> m ActorId
addActor ak bfid pos lid hp calm bsymbol bname bpronoun bcolor time = do
  cops@Kind.COps{coactor=Kind.Ops{okind}, coitem} <- getsState scops
  aid <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ aid}
  let kind = okind ak
  -- Create actor.
  fact@Faction{gplayer} <- getsState $ (EM.! bfid) . sfactionD
  DebugModeSer{sdifficultySer} <- getsServer sdebugSer
  nU <- nUI
  -- If no UI factions, the difficulty applies to heroes (for testing).
  let diffHP | playerUI gplayer || nU == 0 && isHeroFact cops fact =
        (ceiling :: Double -> Int) $ fromIntegral hp
                                     * 1.5 ^^ difficultyCoeff sdifficultySer
             | otherwise = hp
      speed = aspeed kind
      b = actorTemplate ak bsymbol bname bpronoun bcolor speed diffHP calm
                        Nothing pos lid time bfid EM.empty False
  execUpdAtomic $ UpdCreateActor aid b []
  -- Create initial actor items.
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  Level{ldepth} <- getLevel lid
  depth <- getsState sdepth
  forM_ (aitems kind) $ \(ikText, cstore) -> do
    let container = CActor aid cstore
        itemFreq = toFreq "create aitems" [(1, ikText)]
    (itemFull, seed, k) <-
      rndToAction $ newItem coitem flavour discoRev itemFreq lid ldepth depth
    -- Here the items are inserted into the actor.
    void $ registerItem itemFull seed (k, True) container False
  return $! aid

rollSpawnPos :: Kind.COps -> ES.EnumSet Point
             -> LevelId -> Level -> FactionId -> State
             -> Rnd Point
rollSpawnPos Kind.COps{cotile} visible
             lid Level{ltile, lxsize, lysize} fid s = do
  let factionDist = max lxsize lysize - 5
      inhabitants = actorList (/= fid) lid s  -- projectiles can have cameras
      as = actorList (const True) lid s
      isLit = Tile.isLit cotile
      distantAtLeast d p _ =
        all (\b -> chessDist (bpos b) p > d) inhabitants
  -- Not considering F.OftenActor, because monsters emerge from hidden ducts,
  -- which are easier to hide in crampy corridors that lit halls.
  findPosTry 100 ltile
    ( \p t -> Tile.isWalkable cotile t
              && not (Tile.hasFeature cotile F.NoActor t)
              && unoccupied as p)
    [ \_ t -> not (isLit t)  -- no such tiles on some maps
    , distantAtLeast factionDist
    , distantAtLeast $ factionDist `div` 2
    , distantAtLeast $ factionDist `div` 4
    , distantAtLeast $ factionDist `div` 6
    , \p _ -> not $ p `ES.member` visible
    , distantAtLeast 3  -- otherwise a fast actor can walk and hit in one turn
    ]

dominateFid :: (MonadAtomic m, MonadServer m)
            => FactionId -> ActorId -> m ()
dominateFid fid target = do
  Kind.COps{coactor=Kind.Ops{okind}, cotile} <- getsState scops
  tb <- getsState $ getActorBody target
  -- Only record the initial domination as a kill.
  when (boldfid tb == bfid tb) $ execUpdAtomic $ UpdRecordKill target 1
  electLeader (bfid tb) (blid tb) target
  deduceKilled tb
  ais <- getsState $ getCarriedAssocs tb
  execUpdAtomic $ UpdLoseActor target tb ais
  let calmMax = Dice.maxDice $ acalm $ okind $ bkind tb
      bNew = tb { bfid = fid
                , boldfid = bfid tb
                , bcalm = calmMax `div` 2 }
  execUpdAtomic $ UpdSpotActor target bNew ais
  mleaderOld <- getsState $ gleader . (EM.! fid) . sfactionD
  -- Keep the leader if he is on stairs. We don't want to clog stairs.
  keepLeader <- case mleaderOld of
    Nothing -> return False
    Just leaderOld -> do
      body <- getsState $ getActorBody leaderOld
      lvl <- getLevel $ blid body
      return $! Tile.isStair cotile $ lvl `at` bpos body
  unless keepLeader $
    -- Focus on the dominated actor, by making him a leader.
    execUpdAtomic $ UpdLeadFaction fid mleaderOld (Just target)

-- | Advance the move time for the given actor and his status effects
-- that are updated once per his move (as opposed to once per a time unit).
advanceTime :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
advanceTime aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let t = ticksPerMeter $ bspeed b
  execUpdAtomic $ UpdAgeActor aid t
  unless (bproj b) $ do
    if bcalm b == 0 && boldfid b /= bfid b
       && playerLeader (gplayer fact) then do  -- animals never dominated
      let execSfx = execSfxAtomic $ SfxEffect (boldfid b) aid Effect.Dominate
      execSfx
      dominateFid (boldfid b) aid
      execSfx
    else do
      allAssocs <- fullAssocsServer aid [CEqp, CBody]
      newCalmDelta <- getsState $ regenCalmDelta b allAssocs
      unless (newCalmDelta == 0 && bcalmDelta b == 0) $
        execUpdAtomic $ UpdCalmActor aid newCalmDelta

-- TODO: generalize to any list of items (or effects) applied to all actors
-- every turn. Specify the list per level in config.
-- TODO: use itemEffect or at least effectSem to get from Regeneration
-- to HealActorA. Also, Applying an item with Regeneration should do the same
-- thing, but immediately (and destroy the item).
-- | Possibly regenerate HP for all actors on the current level.
--
-- We really want leader picking to be a purely UI distinction,
-- so all actors need to regenerate, not just the leaders.
-- Actors on frozen levels don't regenerate. This prevents cheating
-- via sending an actor to a safe level and letting him regenerate there.
regenerateLevelHP :: (MonadAtomic m, MonadServer m) => LevelId -> m ()
regenerateLevelHP lid = do
  time <- getsState $ getLocalTime lid
  let turnN = time `timeFit` timeTurn
      approve (aid, b) = do
        allAssocs <- fullAssocsServer aid [CEqp, CBody]
        hpPeriod <- getsState $ regenHPPeriod b allAssocs
        return $! hpPeriod /= 0 && turnN `mod` hpPeriod == 0
  toRegen <- getsState $ actorRegularAssocs (const True) lid
  appRegen <- filterM approve toRegen
  mapM_ (\(aid, _) -> execUpdAtomic $ UpdHealActor aid 1) appRegen

leadLevelFlip :: (MonadAtomic m, MonadServer m) => m ()
leadLevelFlip = do
  Kind.COps{cotile} <- getsState scops
  let canFlip fact =
        -- We don't have to check @playerLeader@: @gleader@ would be @Nothing@.
        playerAI (gplayer fact) || isSpawnFact fact
      flipFaction fact | not $ canFlip fact = return ()
      flipFaction fact = do
        case gleader fact of
          Nothing -> return ()
          Just leader -> do
            body <- getsState $ getActorBody leader
            lvl2 <- getLevel $ blid body
            let leaderStuck = waitedLastTurn body
                t = lvl2 `at` bpos body
            -- Keep the leader: he is on stairs and not stuck
            -- and we don't want to clog stairs or get pushed to another level.
            unless (not leaderStuck && Tile.isStair cotile t) $ do
              actorD <- getsState sactorD
              let ourLvl (lid, lvl) =
                    ( lid
                    , EM.size (lfloor lvl)
                    , -- Drama levels skipped, hence @Regular@.
                      actorRegularAssocsLvl (== bfid body) lvl actorD )
              ours <- getsState $ map ourLvl . EM.assocs . sdungeon
              -- Non-humans, being born in the dungeon, have a rough idea of
              -- the number of items left on the level and will focus
              -- on levels they started exploring and that have few items
              -- left. This is to to explore them completely, leave them
              -- once and for all and concentrate forces on another level.
              -- In addition, sole stranded actors tend to become leaders
              -- so that they can join the main force ASAP.
              let freqList = [ (k, (lid, a))
                             | (lid, itemN, (a, _) : rest) <- ours
                             , not leaderStuck || lid /= blid body
                             , let len = 1 + (min 10 $ length rest)
                                   k = 1000000 `div` (3 * itemN + len) ]
              unless (null freqList) $ do
                (lid, a) <- rndToAction $ frequency
                                        $ toFreq "leadLevel" freqList
                unless (lid == blid body) $  -- flip levels rather than actors
                  execUpdAtomic
                  $ UpdLeadFaction (bfid body) (Just leader) (Just a)
  factionD <- getsState sfactionD
  mapM_ flipFaction $ EM.elems factionD
