-- | Server operations performed periodically in the game loop
-- and related operations.
module Game.LambdaHack.Server.PeriodicServer
  ( spawnMonsters, generateMonster, addHero, dominateFid
  , advanceTime, leadLevelFlip
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Tuple

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
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
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Server.CommonServer
import Game.LambdaHack.Server.ItemServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- | Spawn non-hero actors of any faction, friendly or not.
-- To be used for initial dungeon population, spontaneous spawning
-- of monsters and for the summon effect.
spawnMonsters :: (MonadAtomic m, MonadServer m)
              => [Point] -> LevelId -> Time -> FactionId
              -> m Bool
spawnMonsters ps lid time fid = assert (not $ null ps) $ do
  Kind.COps{cofaction=Kind.Ops{okind}} <- getsState scops
  fact <- getsState $ (EM.! fid) . sfactionD
  let spawnName = fname $ okind $ gkind fact
  laid <- forM ps $ \ p ->
    if isHeroFact fact
    then addHero fid p lid [] Nothing time
    else addMonster spawnName fid p lid time
  case catMaybes laid of
    [] -> return False
    aid : _ -> do
      mleader <- getsState $ gleader . (EM.! fid) . sfactionD  -- just changed
      when (isNothing mleader) $
        execUpdAtomic $ UpdLeadFaction fid Nothing (Just aid)
      return True

-- | Generate a monster, possibly.
generateMonster :: (MonadAtomic m, MonadServer m) => LevelId -> m ()
generateMonster lid = do
  -- We check the number of current dungeon dwellers (whether spawned or not)
  -- to decide if more should be spawned.
  f <- getsState $ \s fid -> isSpawnFact $ sfactionD s EM.! fid
  spawns <- getsState $ actorRegularList f lid
  totalDepth <- getsState stotalDepth
  -- We do not check @playerSpawn@ of any faction, but just take @lactorFreq@.
  Level{ldepth, lactorFreq} <- getLevel lid
  rc <- rndToAction $ monsterGenChance ldepth totalDepth (length spawns)
  when rc $ loopGenerateMonster lid lactorFreq

loopGenerateMonster :: (MonadServer m, MonadAtomic m)
                    => LevelId -> Freqs -> m ()
loopGenerateMonster lid actorFreq = do
  cops <- getsState scops
  fidName <- rndToAction $ frequency $ toFreq "cplaceFreq" $ map swap actorFreq
  let f (_, fact) = playerFaction (gplayer fact) == fidName
  factionD <- getsState sfactionD
  fid <- rndToAction $ oneOf $ map fst $ filter f $ EM.assocs factionD
  pers <- getsServer sper
  lvl <- getLevel lid
  let allPers = ES.unions $ map (totalVisible . (EM.! lid))
                $ EM.elems $ EM.delete fid pers  -- expensive :(
  rollPos <- getsState $ rollSpawnPos cops allPers lid lvl fid
  pos <- rndToAction rollPos
  time <- getsState $ getLocalTime lid
  void $ spawnMonsters [pos] lid time fid

-- | Create a new monster on the level, at a given position
-- and with a given actor kind and HP.
addMonster :: (MonadAtomic m, MonadServer m)
           => Text -> FactionId -> Point -> LevelId -> Time
           -> m (Maybe ActorId)
addMonster groupName bfid ppos lid time = do
  cops <- getsState scops
  fact <- getsState $ (EM.! bfid) . sfactionD
  pronoun <- if isCivilianFact cops fact
             then rndToAction $ oneOf ["he", "she"]
             else return "it"
  addActor groupName bfid ppos lid id pronoun time

-- | Create a new hero on the current level, close to the given position.
addHero :: (MonadAtomic m, MonadServer m)
        => FactionId -> Point -> LevelId -> [(Int, (Text, Text))]
        -> Maybe Int -> Time
        -> m (Maybe ActorId)
addHero bfid ppos lid heroNames mNumber time = do
  Kind.COps{cofaction=Kind.Ops{okind=okind}} <- getsState scops
  Faction{gcolor, gplayer, gkind} <- getsState $ (EM.! bfid) . sfactionD
  let fName = fname $ okind gkind
  mhs <- mapM (\n -> getsState $ \s -> tryFindHeroK s bfid n) [0..9]
  let freeHeroK = elemIndex Nothing mhs
      n = fromMaybe (fromMaybe 100 freeHeroK) mNumber
      bsymbol = if n < 1 || n > 9 then '@' else Char.intToDigit n
      nameFromNumber 0 = ("Captain", "he")
      nameFromNumber k | k `mod` 7 == 0 = ("Heroine" <+> tshow k, "she")
      nameFromNumber k = ("Hero" <+> tshow k, "he")
      (bname, pronoun) | gcolor == Color.BrWhite =
        fromMaybe (nameFromNumber n) $ lookup n heroNames
                       | otherwise =
        let (nameN, pronounN) = nameFromNumber n
        in (playerName gplayer <+> nameN, pronounN)
      tweakBody b = b {bsymbol, bname, bcolor = gcolor}
  addActor fName bfid ppos lid tweakBody pronoun time

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
  Kind.COps{cotile} <- getsState scops
  tb0 <- getsState $ getActorBody target
  -- Only record the initial domination as a kill.
  disco <- getsServer sdisco
  trunk <- getsState $ getItemBody $ btrunk tb0
  let ikind = disco EM.! jkindIx trunk
  when (boldfid tb0 == bfid tb0) $ execUpdAtomic $ UpdRecordKill target ikind 1
  electLeader (bfid tb0) (blid tb0) target
  fact <- getsState $ (EM.! bfid tb0) . sfactionD
  -- Prevent the faction's stash from being lost in case they are not spawners.
  when (isNothing $ gleader fact) $ moveStores target CSha CInv
  tb <- getsState $ getActorBody target
  deduceKilled tb
  ais <- getsState $ getCarriedAssocs tb
  calmMax <- sumOrganEqpServer Effect.EqpSlotAddMaxCalm target
  execUpdAtomic $ UpdLoseActor target tb ais
  let bNew = tb { bfid = fid
                , boldfid = bfid tb
                , bcalm = max 0 $ xM calmMax `div` 2 }
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

-- | Advance the move time for the given actor, check if he's dominated
-- and update his calm. We don't update calm once per game turn
-- (even though it would make fast actors less overpowered),
-- beucase the effects of close enemies would sometimes manifest only after
-- a couple of player turns (or perhaps never at all, if the player and enemy
-- move away before that moment). A side effect is that under peaceful
-- circumstances, non-max calm cases a consistent regeneration UI indicator
-- to be displayed each turn (not every few turns).
advanceTime :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
advanceTime aid = do
  b <- getsState $ getActorBody aid
  activeItems <- activeItemsServer aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let t = ticksPerMeter $ bspeed b activeItems
  execUpdAtomic $ UpdAgeActor aid t
  unless (bproj b) $ do
    if bcalm b == 0 && boldfid b /= bfid b
       && playerLeader (gplayer fact) then do  -- animals never dominated
      let execSfx = execSfxAtomic $ SfxEffect (boldfid b) aid Effect.Dominate
      execSfx
      dominateFid (boldfid b) aid
      execSfx
    else do
      newCalmDelta <- getsState $ regenCalmDelta b activeItems
      let clearMark = 0
      unless (newCalmDelta <= 0) $
        -- Update delta for the current player turn.
        execUpdAtomic $ UpdRefillCalm aid newCalmDelta
      unless (bcalmDelta b == ResDelta 0 0) $
        -- Clear delta for the next player turn.
        execUpdAtomic $ UpdRefillCalm aid clearMark
      unless (bhpDelta b == ResDelta 0 0) $
        -- Clear delta for the next player turn.
        execUpdAtomic $ UpdRefillHP aid clearMark

leadLevelFlip :: (MonadAtomic m, MonadServer m) => m ()
leadLevelFlip = do
  cops@Kind.COps{cotile} <- getsState scops
  let canFlip fact =
        -- We don't have to check @playerLeader@: @gleader@ would be @Nothing@.
        playerAI (gplayer fact) || isAllMoveFact cops fact
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
