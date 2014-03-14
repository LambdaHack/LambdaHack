{-# LANGUAGE FlexibleContexts, FunctionalDependencies, RankNTypes, TupleSections
             #-}
-- | Basic client monad and related operations.
module Game.LambdaHack.Client.MonadClient
  ( -- * Basic client monad
    MonadClient( getClient, getsClient, modifyClient, putClient, saveClient
               , liftIO  -- exposed only to be implemented, not used
               )
    -- * Assorted primitives
  , mkConfig, restoreGame, removeServerSave, getPerFid
  , rndToAction, aidTgtToPos, aidTgtAims, makeLine, saveName
  , partAidLeader, partActorLeader, unexploredDepth
  , getCacheBfsAndPath, getCacheBfs, accessCacheBfs
  , closestUnknown, closestSmell, furthestKnown, closestTriggers
  , closestItems, closestFoes, actorAbilities
  , debugPrint
  ) where

import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Ini as Ini
import qualified Data.Ini.Reader as Ini
import qualified Data.Ini.Types as Ini
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import System.Directory
import System.FilePath
import Text.Read

import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Ability (Ability)
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.HumanCmd
import qualified Game.LambdaHack.Common.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Utils.File

class MonadStateRead m => MonadClient m where
  getClient    :: m StateClient
  getsClient   :: (StateClient -> a) -> m a
  modifyClient :: (StateClient -> StateClient) -> m ()
  putClient    :: StateClient -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a
  saveClient   :: m ()

saveName :: FactionId -> Bool -> String
saveName side isAI =
  let n = fromEnum side  -- we depend on the numbering hack to number saves
  in (if n > 0
      then "human_" ++ show n
      else "computer_" ++ show (-n))
     ++ if isAI then ".ai.sav" else ".ui.sav"

debugPrint :: MonadClient m => Text -> m ()
debugPrint t = do
  sdbgMsgCli <- getsClient $ sdbgMsgCli . sdebugCli
  when sdbgMsgCli $ liftIO $ Save.delayPrint t

-- | Get the current perception of a client.
getPerFid :: MonadClient m => LevelId -> m Perception
getPerFid lid = do
  fper <- getsClient sfper
  return $! fromMaybe (assert `failure` "no perception at given level"
                              `twith` (lid, fper))
                      $ EM.lookup lid fper

restoreGame :: MonadClient m => m (Maybe (State, StateClient))
restoreGame = do
  Kind.COps{corule} <- getsState scops
  let stdRuleset = Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile stdRuleset
      cfgUIName = rcfgUIName stdRuleset
  side <- getsClient sside
  isAI <- getsClient sisAI
  prefix <- getsClient $ ssavePrefixCli . sdebugCli
  let copies = [( "GameDefinition" </> cfgUIName <.> "default"
                , cfgUIName <.> "ini" )]
      name = fromMaybe "save" prefix <.> saveName side isAI
  liftIO $ Save.restoreGame name copies pathsDataFile

-- | Assuming the client runs on the same machine and for the same
-- user as the server, move the server savegame out of the way.
removeServerSave :: MonadClient m => m ()
removeServerSave = do
  prefix <- getsClient $ ssavePrefixCli . sdebugCli  -- hack: assume the same
  dataDir <- liftIO appDataDir
  let serverSaveFile = dataDir
                       </> fromMaybe "save" prefix
                       <.> serverSaveName
  bSer <- liftIO $ doesFileExist serverSaveFile
  when bSer $ liftIO $ renameFile serverSaveFile (serverSaveFile <.> "bkp")

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadClient m => Rnd a -> m a
rndToAction r = do
  g <- getsClient srandom
  let (a, ng) = St.runState r g
  modifyClient $ \cli -> cli {srandom = ng}
  return a

-- | The part of speech describing the actor or a special name if a leader
-- of the observer's faction. The actor may not be present in the dungeon.
partActorLeader :: MonadClient m => ActorId -> Actor -> m MU.Part
partActorLeader aid b = do
  mleader <- getsClient _sleader
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ -> partActor b

-- | The part of speech describing the actor (designated by actor id
-- and present in the dungeon) or a special name if a leader
-- of the observer's faction.
partAidLeader :: MonadClient m => ActorId -> m MU.Part
partAidLeader aid = do
  b <- getsState $ getActorBody aid
  partActorLeader aid b

parseConfig :: Ini.Config -> Config
parseConfig cfg =
  let configCommands =
        let mkCommand (ident, keydef) =
              case stripPrefix "Macro_" ident of
                Just _ ->
                  let (key, def) = read keydef
                  in (K.mkKM key, def :: (CmdCategory, HumanCmd))
                Nothing -> assert `failure` "wrong macro id" `twith` ident
            section = Ini.allItems "extra_commands" cfg
        in map mkCommand section
      configHeroNames =
        let toNumber (ident, name) =
              case stripPrefix "HeroName_" ident of
                Just n -> (read n, T.pack name)
                Nothing -> assert `failure` "wrong hero name id" `twith` ident
            section = Ini.allItems "hero_names" cfg
        in map toNumber section
      getOption :: forall a. Read a => String -> a
      getOption optionName =
        let lookupFail :: forall b. String -> b
            lookupFail err =
              assert `failure` ("config file access failed:" <+> T.pack err)
                     `twith` (optionName, cfg)
            s = fromMaybe (lookupFail "") $ Ini.getOption "ui" optionName cfg
        in either lookupFail id $ readEither s
      configVi = getOption "movementViKeys_hjklyubn"
      -- The option for Vi keys takes precendence,
      -- because the laptop keys are the default.
      configLaptop = not configVi && getOption "movementLaptopKeys_uk8o79jl"
      configFont = getOption "font"
      configHistoryMax = getOption "historyMax"
      configMaxFps = getOption "maxFps"
      configNoAnim = getOption "noAnim"
      configRunStopMsgs = getOption "runStopMsgs"
  in Config{..}

-- | Read and parse UI config file.
mkConfig :: Kind.Ops RuleKind -> IO Config
mkConfig corule = do
  let stdRuleset = Kind.stdRuleset corule
      cfgUIName = rcfgUIName stdRuleset
      commentsUIDefault = init $ map (drop 2) $ lines $ rcfgUIDefault stdRuleset  -- TODO: init is a hack until Ini accepts empty files
      sUIDefault = unlines commentsUIDefault
      cfgUIDefault = either (assert `failure`) id $ Ini.parse sUIDefault
  dataDir <- appDataDir
  let userPath = dataDir </> cfgUIName <.> "ini"
  cfgUser <- do
    cpExists <- doesFileExist userPath
    if not cpExists
      then return Ini.emptyConfig
      else do
        sUser <- readFile userPath
        return $! either (assert `failure`) id $ Ini.parse sUser
  let cfgUI = M.unionWith M.union cfgUser cfgUIDefault  -- user cfg preferred
      conf = parseConfig cfgUI
  -- Catch syntax errors in complex expressions ASAP,
  return $! deepseq conf conf

-- | Get cached BFS data and path or, if not stored, generate,
-- store and return. Due to laziness, they are not calculated until needed.
getCacheBfsAndPath :: forall m. MonadClient m
                   => ActorId -> Point
                   -> m (PointArray.Array BfsDistance, Maybe [Point])
getCacheBfsAndPath aid target = do
  seps <- getsClient seps
  let pathAndStore :: PointArray.Array BfsDistance
                   -> m (PointArray.Array BfsDistance, Maybe [Point])
      pathAndStore bfs = do
        computePath <- computePathBFS aid
        let mpath = computePath target seps bfs
        modifyClient $ \cli ->
          cli {sbfsD = EM.insert aid (bfs, target, seps, mpath) (sbfsD cli)}
        return (bfs, mpath)
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just (bfs, targetOld, sepsOld, mpath) | targetOld == target
                                            && sepsOld == seps ->
      return (bfs, mpath)
    Just (bfs, _, _, _) -> pathAndStore bfs
    Nothing -> do
      bfs <- computeBFS aid
      pathAndStore bfs

getCacheBfs :: MonadClient m => ActorId -> m (PointArray.Array BfsDistance)
{-# INLINE getCacheBfs #-}
getCacheBfs aid = do
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just (bfs, _, _, _) -> return bfs
    Nothing -> fmap fst $ getCacheBfsAndPath aid (Point 0 0)

computeBFS :: MonadClient m => ActorId -> m (PointArray.Array BfsDistance)
computeBFS = computeAnythingBFS $ \isEnterable passUnknown aid -> do
  b <- getsState $ getActorBody aid
  Level{lxsize, lysize} <- getLevel $ blid b
  let origin = bpos b
      vInitial = PointArray.replicateA lxsize lysize apartBfs
  -- Here we don't want '$!', because we want the BFS data lazy.
  return ${-keep it!-} fillBfs isEnterable passUnknown origin vInitial

computePathBFS :: MonadClient m
               => ActorId
               -> m (Point -> Int -> PointArray.Array BfsDistance
                     -> Maybe [Point])
computePathBFS = computeAnythingBFS $ \isEnterable passUnknown aid -> do
  b <- getsState $ getActorBody aid
  let origin = bpos b
  -- Here we don't want '$!', because we want the BFS data lazy.
  return ${-keep it!-} findPathBfs isEnterable passUnknown origin

computeAnythingBFS :: MonadClient m
                   => ((Point -> Point -> MoveLegal)
                       -> (Point -> Point -> Bool)
                       -> ActorId
                       -> m a)
                   -> ActorId
                   -> m a
computeAnythingBFS fAnything aid = do
  cops@Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  smarkSuspect <- getsClient smarkSuspect
  sisAI <- getsClient sisAI
  -- We treat doors as an open tile and don't add an extra step for opening
  -- the doors, because other actors open and use them, too,
  -- so it's amortized. We treat unknown tiles specially.
  let -- Suspect tiles treated as a kind of unknown.
      passSuspect = smarkSuspect || sisAI  -- AI checks suspects ASAP
      unknownId = ouniqGroup "unknown space"
      chAccess = checkAccess cops lvl
      chDoorAccess = checkDoorAccess cops lvl
      conditions = catMaybes [chAccess, chDoorAccess]
      -- Legality of move from a known tile, assuming doors freely openable.
      isEnterable :: Point -> Point -> MoveLegal
      isEnterable spos tpos =
        let tt = lvl `at` tpos
            allOK = all (\f -> f spos tpos) conditions
        in if tt == unknownId
           then if allOK
                then MoveToUnknown
                else MoveBlocked
           else if Tile.isSuspect cotile tt
                then if passSuspect && allOK
                     then MoveToUnknown
                     else MoveBlocked
                else if Tile.isPassable cotile tt && allOK
                     then MoveToOpen
                     else MoveBlocked
      -- Legality of move from an unknown tile, assuming unknown are open.
      passUnknown :: Point -> Point -> Bool
      passUnknown = case chAccess of  -- spos is unknown, so not a door
        Nothing -> \_ tpos -> let tt = lvl `at` tpos
                              in tt == unknownId
                                 || passSuspect && Tile.isSuspect cotile tt
        Just ch -> \spos tpos -> let tt = lvl `at` tpos
                                 in (tt == unknownId
                                     || passSuspect
                                        && Tile.isSuspect cotile tt)
                                    && ch spos tpos
  fAnything isEnterable passUnknown aid

accessCacheBfs :: MonadClient m => ActorId -> Point -> m (Maybe Int)
{-# INLINE accessCacheBfs #-}
accessCacheBfs aid target = do
  bfs <- getCacheBfs aid
  return $! accessBfs bfs target

-- | Calculate the position of an actor's target.
aidTgtToPos :: MonadClient m
            => ActorId -> LevelId -> Maybe Target -> m (Maybe Point)
aidTgtToPos aid lidV tgt =
  case tgt of
    Just (TEnemy a _) -> do
      body <- getsState $ getActorBody a
      return $! if blid body == lidV
                then Just (bpos body)
                else Nothing
    Just (TEnemyPos _ lid p _) ->
      return $! if lid == lidV then Just p else Nothing
    Just (TPoint lid p) ->
      return $! if lid == lidV then Just p else Nothing
    Just (TVector v) -> do
      b <- getsState $ getActorBody aid
      Level{lxsize, lysize} <- getLevel lidV
      let shifted = shiftBounded lxsize lysize (bpos b) v
      return $! if shifted == bpos b && v /= Vector 0 0
                then Nothing
                else Just shifted
    Nothing -> do
      scursor <- getsClient scursor
      aidTgtToPos aid lidV $ Just scursor

-- | Check whether one is permitted to aim at a target
-- (this is only checked for actors; positions let player
-- shoot at obstacles, e.g., to destroy them).
-- This assumes @aidTgtToPos@ does not return @Nothing@.
--
-- Note: Perception is not enough for the check,
-- because the target actor can be obscured by a glass wall
-- or be out of sight range, but in weapon range.
aidTgtAims :: MonadClient m
           => ActorId -> LevelId -> Maybe Target -> m (Maybe Text)
aidTgtAims aid lidV tgt = do
  case tgt of
    Just (TEnemy a _) -> do
      body <- getsState $ getActorBody a
      let pos = bpos body
      b <- getsState $ getActorBody aid
      if blid b == lidV then do
        seps <- getsClient seps
        (steps, _eps) <- makeLine b pos seps
        if steps == chessDist (bpos b) pos
          then return Nothing
          else return $ Just "aiming line to the opponent blocked"
      else return $ Just "target opponent not on this level"
    Just TEnemyPos{} -> return $ Just "target opponent not visible"
    Just TPoint{} -> return Nothing
    Just TVector{} -> return Nothing
    Nothing -> do
      scursor <- getsClient scursor
      aidTgtAims aid lidV $ Just scursor

-- | Counts the number of steps until the projectile would hit
-- an actor or obstacle. Prefers the given eps.
-- TODO: but modifies eps, if needed.
makeLine :: MonadClient m => Actor -> Point -> Int -> m (Int, Int)
makeLine body fpos eps = do
  cops <- getsState scops
  lvl@Level{lxsize, lysize} <- getLevel (blid body)
  bs <- getsState $ actorNotProjList (const True) (blid body)
  let mbl = bla lxsize lysize eps (bpos body) fpos
  case mbl of
    Just bl@(pos1:_) -> do
      let noActor p = any ((== p) . bpos) bs || p == fpos
      case break noActor bl of
        (flies, hits : _) -> do
          let blRest = flies ++ [hits]
              blZip = zip (bpos body : blRest) blRest
              blAccess = takeWhile (uncurry $ accessible cops lvl) blZip
          mab <- getsState $ posToActor pos1 (blid body)
          if maybe True (bproj . snd . fst) mab then
            return $ (length blAccess, eps)
          else return (0, eps)  -- ProjectBlockActor
        _ -> assert `failure` (body, fpos, bl)
    Just [] -> assert `failure` (body, fpos)
    Nothing -> return (0, eps)  -- ProjectAimOnself

-- | Furthest (wrt paths) known position, except under the actor.
furthestKnown :: MonadClient m => ActorId -> m (Maybe Point)
furthestKnown aid = do
  bfs <- getCacheBfs aid
  getMaxIndex <- rndToAction $ oneOf [ PointArray.maxIndexA
                                     , PointArray.maxLastIndexA ]
  let furthestPos = getMaxIndex bfs
      dist = bfs PointArray.! furthestPos
  return $! if dist <= apartBfs
            then assert `failure` (aid, furthestPos, dist)
            else if dist == succ apartBfs  -- bpos of aid
                 then Nothing
                 else Just furthestPos

-- | Closest reachable unknown tile position, if any.
closestUnknown :: MonadClient m => ActorId -> m (Maybe Point)
closestUnknown aid = do
  bfs <- getCacheBfs aid
  getMinIndex <- rndToAction $ oneOf [ PointArray.minIndexA
                                     , PointArray.minLastIndexA ]
  let closestPos = getMinIndex bfs
      dist = bfs PointArray.! closestPos
  if dist >= apartBfs then do
    body <- getsState $ getActorBody aid
    smarkSuspect <- getsClient smarkSuspect
    sisAI <- getsClient sisAI
    let passSuspect = smarkSuspect || sisAI
    when passSuspect $  -- explored fully, including suspect tiles
      modifyClient $ \cli ->
        cli {sexplored = ES.insert (blid body) (sexplored cli)}
    return Nothing
  else return $ Just closestPos

-- TODO: this is costly, because target has to be changed every
-- turn when walking along trail. But inverting the sort and going
-- to the newest smell, while sometimes faster, may result in many
-- actors following the same trail, unless we wipe the trail as soon
-- as target is assigned (but then we don't know if we should keep the target
-- or not, because somebody already followed it). OTOH, trails are not
-- common and so if wiped they can't incur a large total cost.
-- TODO: remove targets where the smell is likely to get too old by the time
-- the actor gets there.
-- | Finds smells closest to the actor, except under the actor.
closestSmell :: MonadClient m => ActorId -> m [(Int, (Point, Tile.SmellTime))]
closestSmell aid = do
  body <- getsState $ getActorBody aid
  Level{lsmell} <- getLevel $ blid body
  let smells = EM.assocs lsmell
  case smells of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ts = mapMaybe (\x@(p, _) -> fmap (,x) (accessBfs bfs p)) smells
          ds = filter (\(d, _) -> d /= 0) ts  -- bpos of aid
      return $! sortBy (comparing (fst &&& timeNegate . snd . snd)) ds

-- TODO: We assume linear dungeon in @unexploredD@,
-- because otherwise we'd need to calculate shortest paths in a graph, etc.
-- | Closest (wrt paths) triggerable open tiles.
-- The second argument can ever be true only if there's
-- no escape from the dungeon.
closestTriggers :: MonadClient m => Maybe Bool -> Bool -> ActorId -> m [Point]
closestTriggers onlyDir exploredToo aid = do
  Kind.COps{cotile} <- getsState scops
  body <- getsState $ getActorBody aid
  lvl <- getLevel $ blid body
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  unexploredD <- unexploredDepth
  let allExplored = ES.size explored == EM.size dungeon
      unexUp = onlyDir /= Just False && unexploredD 1 (blid body)
      unexDown = onlyDir /= Just True && unexploredD (-1) (blid body)
      unexEffect (Effect.Ascend p) = if p > 0 then unexUp else unexDown
      unexEffect _ =
        -- Escape (or guard) only after exploring, for high score, etc.
        allExplored
      isTrigger
        | exploredToo = \t -> Tile.isWalkable cotile t
                              && not (null $ Tile.causeEffects cotile t)
        | otherwise = \t -> Tile.isWalkable cotile t
                            && any unexEffect (Tile.causeEffects cotile t)
      f :: [Point] -> Point -> Kind.Id TileKind -> [Point]
      f acc p t = if isTrigger t then p : acc else acc
  let triggersAll = PointArray.ifoldlA f [] $ ltile lvl
      -- Don't target stairs under the actor. Most of the time they
      -- are blocked and stay so, so we seek other stairs, if any.
      -- If no other stairs in this direction, let's wait here.
      triggers | length triggersAll > 1 = delete (bpos body) triggersAll
               | otherwise = triggersAll
  case triggers of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\p -> fmap (,p) (accessBfs bfs p)) triggers
      return $! map snd $ sortBy (comparing fst) ds

unexploredDepth :: MonadClient m => m (Int -> LevelId -> Bool)
unexploredDepth = do
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  let allExplored = ES.size explored == EM.size dungeon
      unexploredD p =
        let unex lid = allExplored && lescape (dungeon EM.! lid)
                       || ES.notMember lid explored
                       || unexploredD p lid
        in any unex . ascendInBranch dungeon p
  return unexploredD

-- | Closest (wrt paths) items.
closestItems :: MonadClient m => ActorId -> m ([(Int, (Point, ItemBag))])
closestItems aid = do
  body <- getsState $ getActorBody aid
  Level{lfloor} <- getLevel $ blid body
  let items = EM.assocs lfloor
  case items of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\x@(p, _) -> fmap (,x) (accessBfs bfs p)) items
      return $! sortBy (comparing fst) ds

-- | Closest (wrt paths) enemy actors.
closestFoes :: MonadClient m => ActorId -> m [(Int, (ActorId, Actor))]
closestFoes aid = do
  body <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid body
  foes <- getsState $ actorNotProjAssocs (isAtWar fact) (blid body)
  case foes of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\x@(_, b) -> fmap (,x) (accessBfs bfs (bpos b))) foes
      return $! sortBy (comparing fst) ds

actorAbilities :: MonadClient m => ActorId -> Maybe ActorId -> m [Ability]
actorAbilities aid mleader = do
  Kind.COps{ coactor=Kind.Ops{okind}
           , cofaction=Kind.Ops{okind=fokind} } <- getsState scops
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let factionAbilities
        | Just aid == mleader = fAbilityLeader $ fokind $ gkind fact
        | otherwise = fAbilityOther $ fokind $ gkind fact
  return $! acanDo (okind $ bkind body) `intersect` factionAbilities
