-- | AI procedure for picking the best action for an actor.
module Game.LambdaHack.Client.AI.PickActionM
  ( pickAction
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , actionStrategy, waitBlockNow, yellNow
  , pickup, equipItems, yieldUnneeded, unEquipItems
  , groupByEqpSlot, bestByEqpSlot, harmful, meleeBlocker, meleeAny
  , trigger, projectItem, ApplyItemGroup, applyItem, flee
  , displaceFoe, displaceBlocker, displaceTgt
  , chase, moveTowards, moveOrRunAid
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Either
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Function
import           Data.Ratio

import           Game.LambdaHack.Client.AI.ConditionM
import           Game.LambdaHack.Client.AI.Strategy
import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.BfsM
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.Request
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Definition.Ability
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import qualified Game.LambdaHack.Definition.DefsInternal as DefsInternal

-- | Pick the most desirable AI ation for the actor.
pickAction :: MonadClient m
           => [(ActorId, Actor)] -> [(ActorId, Actor)] -> ActorId -> Bool
           -> m RequestTimed
pickAction foeAssocs friendAssocs aid retry = do
  side <- getsClient sside
  body <- getsState $ getActorBody aid
  let !_A = assert (bfid body == side
                    `blame` "AI tries to move enemy actor"
                    `swith` (aid, bfid body, side)) ()
  let !_A = assert (not (bproj body)
                    `blame` "AI gets to manually move its projectiles"
                    `swith` (aid, bfid body, side)) ()
  stratAction <- actionStrategy foeAssocs friendAssocs (blid body) aid retry
  let bestAction = bestVariant stratAction
      !_A = assert (not (nullFreq bestAction)  -- equiv to nullStrategy
                    `blame` "no AI action for actor"
                    `swith` (stratAction, aid, body)) ()
  -- Run the AI: chose an action from those given by the AI strategy.
  rndToAction $ frequency bestAction

-- AI strategy based on actor's sight, smell, etc.
-- Never empty.
actionStrategy :: MonadClient m
               => [(ActorId, Actor)] -> [(ActorId, Actor)]
               -> LevelId -> ActorId -> Bool
               -> m (Strategy RequestTimed)
actionStrategy foeAssocs friendAssocs lid aid retry = do
  condInMelee <- condInMeleeM lid
  randomAggressionThreshold <- rndToAction $ randomR0 10
  actionStrategyRead condInMelee foeAssocs friendAssocs
                     randomAggressionThreshold lid aid retry

-- This is close to being in @MonadClientRead@, but not quite, due to
-- random numbers used explicitly in a couple of places (e.g., weapon choice),
-- which should be fixed and expressed via @Strategy@ instead,
-- and due to recoding the fleeing preference, which should be
-- factored out and done in the @actionStrategy@ wrapper.
--
-- After the AI code is totally rewritten soon, this should be revisited.
actionStrategyRead :: forall m. MonadClient m
                   => Bool
                   -> [(ActorId, Actor)] -> [(ActorId, Actor)]
                   -> Int -> LevelId -> ActorId -> Bool
                   -> m (Strategy RequestTimed)
actionStrategyRead condInMelee foeAssocs friendAssocs
                   randomAggressionThreshold lid aid retry = do
  COps{coTileSpeedup} <- getsState scops
  mleader <- getsClient sleader
  body <- getsState $ getActorBody aid
  let !_A = assert (blid body == lid) ()
  lvl <- getLevel lid
  localTime <- getsState $ getLocalTime lid
  condAimEnemyTargeted <- condAimEnemyTargetedM aid
  condAimEnemyOrStash <- condAimEnemyOrStashM aid
  condAimEnemyOrRemembered <- condAimEnemyOrRememberedM aid
  condAimNonEnemyPresent <- condAimNonEnemyPresentM aid
  condAimCrucial <- condAimCrucialM aid
  actorMaxSkills <- getsState sactorMaxSkills
  condAnyFoeAdj <- getsState $ anyFoeAdj aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  condOurAdj <- getsState $ any (\(_, b) -> isFriend (bfid body) fact (bfid b))
                            . adjacentBigAssocs body
  oursExploring <- getsState $ oursExploringAssocs (bfid body)
  condAnyHarmfulFoeAdj <- getsState $ anyHarmfulFoeAdj actorMaxSkills aid
  threatDistL <- getsState $ meleeThreatDistList foeAssocs aid
  (fleeL, badVic) <- fleeList foeAssocs aid
  oldFleeD <- getsClient sfleeD
  condSupport1 <- condSupport friendAssocs 1 aid
  condSupport3 <- condSupport friendAssocs 3 aid
  condSolo <- condAloneM friendAssocs aid  -- solo fighters aggresive
  actorSk <- currentSkillsClient aid
  condCanProject <- condCanProjectM (getSk SkProject actorSk) aid
  condAdjTriggerable <- condAdjTriggerableM actorSk aid
  condBlocksFriends <- condBlocksFriendsM aid
  condNoEqpWeapon <- condNoEqpWeaponM aid
  condEnoughGear <- condEnoughGearM aid
  condFloorWeapon <- condFloorWeaponM aid
  condDesirableFloorItem <- condDesirableFloorItemM aid
  condTgtNonmovingEnemy <- condTgtNonmovingEnemyM aid
  explored <- getsClient sexplored
  -- This doesn't treat actors guarding stash specially, so on such levels
  -- man sleeping actors may reside for a long time. Variety, OK.
  let awakeAndNotGuarding (_, b) =
        bpos b /= bpos body
        && bwatch b /= WSleep
        && Just (lid, bpos b) /= gstash fact
      anyFriendOnLevelAwake = any awakeAndNotGuarding friendAssocs
      actorMaxSk = actorMaxSkills EM.! aid
      recentlyFled = maybe False (\(_, time) -> timeRecent5 localTime time)
                           (aid `EM.lookup` oldFleeD)
      prefersSleepWhenAwake = case bwatch body of
        WSleep -> Ability.getSk Ability.SkMoveItem actorMaxSk <= -10
        _ -> prefersSleep actorMaxSk  -- nm @WWake@
      mayFallAsleep = not condAimEnemyOrRemembered
                      && calmFull body actorMaxSk  -- only when fully relaxed
                      && mayContinueSleep
                      && canSleep actorSk
      mayContinueSleep = not condAimEnemyOrStash
                         && not (hpFull body actorSk)
                         && not uneasy
                         && not condAnyFoeAdj
                         && (anyFriendOnLevelAwake  -- friend guards the sleeper
                             || prefersSleepWhenAwake)  -- or he doesn't care
      dozes = case bwatch body of
                WWait n -> n > 0
                _ -> False
              && mayFallAsleep
              && Just aid /= mleader  -- best teammate for a task so stop dozing
      lidExplored = ES.member lid explored
      panicFleeL = fleeL ++ badVic
      condHpTooLow = hpTooLow body actorMaxSk
      heavilyDistressed =  -- actor hit by a proj or similarly distressed
        deltasSerious (bcalmDelta body)
      heavilyDistressedThisTurn =  -- if far from melee, almost sure hit by proj
        deltasSeriousThisTurn (bcalmDelta body)
      condNotCalmEnough = not (calmEnough body actorMaxSk)
      uneasy = heavilyDistressed || condNotCalmEnough || recentlyFled
      speed = gearSpeed actorMaxSk
      speed1_5 = speedScale (3%2) speed
      -- Max skills used, because we need to know if can melee as leader.
      condCanMelee = actorCanMelee actorMaxSkills aid body
      condMeleeBad = not ((condSolo || condSupport1) && condCanMelee)
      -- These are only melee threats.
      condThreat n = not $ null $ takeWhile ((<= n) . fst) threatDistL
      threatAdj = takeWhile ((== 1) . fst) threatDistL
      condManyThreatsAdj = length threatAdj >= 2
      condFastThreatAdj =
        any (\(_, (aid2, _)) ->
              let ar2 = actorMaxSkills EM.! aid2
              in gearSpeed ar2 > speed1_5)
            threatAdj
      condNonStealthyThreatAdj =
        any (\(_, (aid2, b2)) ->
              let ar2 = actorMaxSkills EM.! aid2
              in Ability.getSk Ability.SkShine ar2 > 0
                 || isLit (bpos b2))
            threatAdj
      actorShines = Ability.getSk Ability.SkShine actorMaxSk > 0
      isLit pos = Tile.isLit coTileSpeedup (lvl `at` pos)
        -- solid tiles ignored, because not obvious if dark after removed
      canFleeIntoDark = not $ actorShines || all (isLit . snd) fleeL
      avoidAmbient = not condInMelee && uneasy && not actorShines
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  let condGoalIsLit = case mtgtMPath of
        Just TgtAndPath{tapPath=Just AndPath{pathGoal}} -> isLit pathGoal
        _ -> False
      -- Fleeing makes sense, because either actor can't melee,
      -- or at least won't flee without scoring a hit and return next turn,
      -- due to threat no longer seen (due to blindness or dark).
      fleeingMakesSense =
        not condCanMelee
        || (Ability.getSk Ability.SkSight actorMaxSk > 2
            || Ability.getSk Ability.SkNocto actorMaxSk > 2)
           && (Ability.getSk Ability.SkShine actorMaxSk > 2
               || condNonStealthyThreatAdj || null threatAdj)
      abInSkill sk = getSk sk actorSk > 0
      abInMaxSkill sk = getSk sk actorMaxSk > 0
      runSkills = [SkMove, SkDisplace]  -- not @SkAlter@, to ground sleepers
      stratToFreq :: Int
                  -> m (Strategy RequestTimed)
                  -> m (Frequency RequestTimed)
      stratToFreq scale mstrat = do
        st <- mstrat
        return $! if scale == 0
                  then mzero
                  else scaleFreq scale $ bestVariant st
      -- Order matters within the list, because it's summed with .| after
      -- filtering. Also, the results of prefix, distant and suffix
      -- are summed with .| at the end.
      prefix, suffix:: [([Skill], m (Strategy RequestTimed), Bool)]
      prefix =
        [ ( [SkApply]
          , applyItem actorSk aid ApplyFirstAid
          , not condAnyHarmfulFoeAdj && condHpTooLow)
        , ( [SkAlter]
          , trigger aid ViaStairs
              -- explore next or flee via stairs, even if to wrong level;
              -- in the latter case, may return via different stairs later on
          , condAdjTriggerable && not condAimEnemyOrStash
            && ((condNotCalmEnough || condHpTooLow)  -- flee
                && condMeleeBad && condAnyHarmfulFoeAdj
                || (lidExplored || condEnoughGear)  -- explore
                   && not condDesirableFloorItem) )
        , ( [SkDisplace]
          , displaceFoe aid  -- only swap with an enemy to expose him
                             -- and only if a friend is blocked by us
          , condAnyFoeAdj && condBlocksFriends)  -- later checks foe eligible
        , ( [SkMoveItem]
          , pickup aid True
          , condNoEqpWeapon  -- we assume organ weapons usually inferior
            && condDesirableFloorItem && condFloorWeapon && not condHpTooLow
            && abInMaxSkill SkMelee )
        , ( [SkAlter]
          , trigger aid ViaEscape
          , condAdjTriggerable && not condAimEnemyTargeted
            && not condDesirableFloorItem )  -- collect the last loot
        , ( runSkills
          , flee actorSk aid (not actorShines) fleeL
          , -- Flee either from melee, if our melee is bad and enemy close,
            -- or from missiles, if we can hide in the dark in one step.
            -- Note that we don't know how far ranged threats are or if,
            -- in fact, they hit from all sides or are hidden. Hence we can't
            -- do much except hide in darkness or take off light (elsewhere).
            -- We tend to flee even when over stash (on lit terrain at least),
            -- but only if we can't fling at enemy (e.g., not visible).
            -- This is OK, since otherwise we'd be killed for free
            -- and the stash would be taken just a little later on.
            -- Note: a part of this condition appears in @actorVulnerable@.
            not condFastThreatAdj
            && fleeingMakesSense
            && if | condAnyHarmfulFoeAdj ->
                    -- Here we don't check @condInMelee@ because regardless
                    -- of whether our team melees (including the fleeing ones),
                    -- endangered actors should flee from very close foes.
                    not condCanMelee
                    || condManyThreatsAdj && not condSupport1 && not condSolo
                  | case gstash fact of
                      Nothing -> False
                      Just (lid2, pos) ->
                        lid2 == lid && chessDist pos (bpos body) <= 2
                        -> False
                  | condInMelee -> False
                      -- No fleeing when others melee and no critical threat
                      -- (otherwise no target nor action would be possible).
                  | heavilyDistressedThisTurn  -- and no adj melee
                    || (heavilyDistressed && not (condThreat 2)) ->
                    not condCanMelee || canFleeIntoDark
                      -- Almost surely hit by projectile. So, if can melee,
                      -- don't escape except into the dark.
                      -- Note that even when in dark now, the projectile hit
                      -- might have been enabled by dynamic light
                      -- or light from lying items, so fleeing still needed.
                      -- If heroes stay in the light when under fire,
                      -- they are pummeled by fast ranged foes and can neither
                      -- flee (too slow) nor force them to flee nor kill them.
                      -- Also AI monsters need predictable behaviour to avoid
                      -- having to chase them forever. Ranged aggravating helps
                      -- and melee-less ranged always fleeing when hit helps
                      -- and makes them evading ambushers, perfect for swarms.
                  | condThreat 2  -- melee enemies near
                    || condThreat 5 && heavilyDistressed ->
                         -- enemies not near but maintain fleeing hysteresis,
                         -- but not if due to lack of support, which changes,
                         -- hence @heavilyDistressed@ and not @recentlyFled@
                    not condCanMelee  -- can't melee, flee
                    || -- No support, not alone, either not aggressive
                       -- or can safely project from afar instead. Flee.
                       not condSupport3
                       && not condSolo
                       -- Don't flee if can spend time productively, killing
                       -- an enemy that blocks projecting or walking.
                       && not condAnyFoeAdj
                       -- Extra random aggressiveness if can't project
                       -- and didn't flee recently and so undecided.
                       -- This is hacky; the randomness is outside @Strategy@.
                       && (condCanProject
                           || recentlyFled  -- still no support, keep fleeing
                           || Ability.getSk Ability.SkAggression actorMaxSk
                              < randomAggressionThreshold)
                  | otherwise -> False )  -- melee threats too far
        , ( runSkills  -- no blockers if can't move right now
          , meleeBlocker actorSk aid  -- only melee blocker
          , abInSkill SkMelee
            && (condAnyFoeAdj  -- if foes, don't displace, otherwise friends:
                || not (abInSkill SkDisplace)  -- displace friends, if can
                   && condAimEnemyOrStash) )  -- excited
                        -- So that animals block each other until hero comes
                        -- and then the stronger makes a show for him
                        -- and kills the weaker.
        , ( [SkAlter]
          , trigger aid ViaNothing
          , not condInMelee  -- don't incur overhead
            && condAdjTriggerable
            && not condAimEnemyTargeted )  -- targeting stash is OK, to unblock
                                           -- dungeons if party has only one key
        , ( [SkDisplace]  -- prevents some looping movement
          , displaceBlocker aid retry  -- fires up only when path blocked
          , retry || not condDesirableFloorItem )
        , ( [SkMelee]
          , meleeAny aid
          , condAnyFoeAdj )  -- won't flee nor displace, so let it melee
        , ( runSkills
          , flee actorSk
                 aid  -- rattlesnakes and hornets flee and return when charging
                 ((heavilyDistressedThisTurn && not condAnyHarmfulFoeAdj
                   || (heavilyDistressed && not (condThreat 2)))
                     -- prefer bad but dark spots if under fire
                  && not actorShines)
                 panicFleeL  -- ultimate panic mode; open tiles, if needed
          , condAnyHarmfulFoeAdj )
        ]
      -- Order doesn't matter, scaling does.
      -- These are flattened in @stratToFreq@ (taking only the best variant)
      -- and then summed, so if any of these can fire, it will.
      -- If none can, @suffix@ is tried.
      -- Only the best variant of @chase@ is taken, but it's almost always
      -- good, and if not, the @chase@ in @suffix@ may fix that.
      -- The scaling values for @stratToFreq@ need to be so low-resolution
      -- or we get 32bit @Freqency@ overflows, which would bite us in JS.
      --
      -- Note that all monadic actions here are performed when the strategy
      -- is being chosen so, e.g., none of them can be @flee@ or the actor
      -- would be marked in state as fleeing even when the strategy is
      -- not chosen.
      distant :: [([Skill], m (Frequency RequestTimed), Bool)]
      distant =
        [ ( [SkMoveItem]
          , stratToFreq (if condInMelee then 20 else 20000)
            $ yieldUnneeded aid  -- 20000 to unequip ASAP, unless is thrown
          , True )
        , ( [SkMoveItem]
          , stratToFreq 10
            $ equipItems aid  -- doesn't take long, very useful if safe
          , not (condInMelee
                 || condDesirableFloorItem
                 || uneasy) )
        , ( [SkProject]
          , stratToFreq (if condTgtNonmovingEnemy then 100 else 30)
              -- not too common, to leave missiles for pre-melee dance
            $ projectItem actorSk aid
          , condAimEnemyTargeted && not condInMelee && condCanProject )
        , ( [SkApply]  -- common, because animals have that
          , stratToFreq 10
            $ applyItem actorSk aid ApplyAll  -- use any potion or scroll
          , condAimEnemyTargeted || condThreat 9 )  -- can buff against enemies
        , ( runSkills
          , stratToFreq (if | condInMelee ->
                              4000  -- friends pummeled by target, go to help
                            | not condAimEnemyOrStash ->
                              20  -- if enemy only remembered investigate anyway
                            | not (isLit (bpos body))  -- would need to leave
                              && not actorShines       -- dark, most probably
                              && condGoalIsLit -> 1
                            | otherwise ->
                              200)
            $ chase actorSk aid avoidAmbient retry
          , condCanMelee
            && Just (lid, bpos body) /= gstash fact
            && (if condInMelee then condAimEnemyOrStash
                else (condAimEnemyOrRemembered
                      || condAimNonEnemyPresent)
                     && (not (condThreat 2)
                         || heavilyDistressed  -- if under fire, do something!
                         || speed >= speedAdd speedWalk speedWalk
                              -- low risk of getting hit first
                         || not condMeleeBad)
                       -- this results in animals in corridor never attacking
                       -- (unless distressed by, e.g., being hit by missiles),
                       -- because they can't swarm opponent, which is logical,
                       -- and in rooms they do attack, so not too boring;
                       -- two aliens attack always, because more aggressive
                     && not condDesirableFloorItem) )
        ]
      suffix =
        [ ( [SkMoveItem]
          , pickup aid False  -- e.g., to give to other party members
          , not condInMelee && condDesirableFloorItem && not dozes )
        , ( [SkMoveItem]
          , unEquipItems aid  -- late, because these items not bad
          , not condInMelee && not dozes )
        , ( [SkWait]
          , waitBlockNow  -- try to fall asleep, rarely
          , bwatch body `notElem` [WSleep, WWake]
            && mayFallAsleep
            && prefersSleep actorMaxSk
            && not condAimCrucial)
        , ( runSkills
          , chase actorSk aid avoidAmbient retry
          , not dozes
            && if condInMelee
               then condCanMelee && condAimEnemyOrStash
               else (not (condThreat 2) || not condMeleeBad)
                    && (Just (lid, bpos body) /= gstash fact
                        || heavilyDistressed  -- guard strictly, until harmed
                        || length oursExploring <= 1
                        || condOurAdj  -- or if teammates adjacent
                        || bcalm body < 10) )  -- break loop, avoid domination
        ]
      fallback =  -- Wait until friends sidestep; ensures strategy never empty.
                  -- Also, this is what non-leader heroes do, unless they melee.
        [ ( [SkWait]
          , case bwatch body of
              WSleep -> yellNow  -- we know actor doesn't want to sleep,
                                 -- so celebrate wake up with a bang
              _ -> waitBlockNow  -- block, etc.
          , True )
        , ( runSkills  -- if can't block, at least change something
          , chase actorSk aid avoidAmbient True
          , not condInMelee || condCanMelee && condAimEnemyTargeted )
        , ( [SkDisplace]  -- if can't brace, at least change something
          , displaceBlocker aid True
          , True )
        , ( []
          , yellNow  -- desperate fallback
          , True )
       ]
  -- Check current, not maximal skills, since this can be a leader as well
  -- as non-leader action.
  let checkAction :: ([Skill], m a, Bool) -> Bool
      checkAction (abts, _, cond) = (null abts || any abInSkill abts) && cond
      sumS :: [([Skill], m a, Bool)] -> [m a]
      sumS abAction =
        let as = filter checkAction abAction
        in map (\(_, m, _) -> m) as
      sumF :: [([Skill], m (Frequency RequestTimed), Bool)]
           -> m (Frequency RequestTimed)
      sumF abFreq = do
        let as = filter checkAction abFreq
        -- This is costly: the monadic side-effects are evaluated.
        -- If we roll an unevaluated one until we find one that is permitted,
        -- keeping track of those aready checked might outweigh the savings.
        -- Even worse, without evaluating per-item frequencies,
        -- applying an amazing item may be disregarded in favour of throwing
        -- a mediocre one.
        strats <- mapM (\(_, m, _) -> m) as
        return $! msum strats
      combineWeighted as = liftFrequency <$> sumF as
      sumPrefix = sumS prefix
      comDistant = combineWeighted distant
      sumSuffix = sumS suffix
      sumFallback = sumS fallback
      -- TODO: should be: sumPrefix .| comDistant .| sumSuffix .| sumFallback
      -- but then all side-effects have to be computed beforehand,
      -- breaking the state, e.g., marking actor as fleeing, always.
      sums = sumPrefix ++ [comDistant] ++ sumSuffix ++ sumFallback
      tryStrategies :: [m (Strategy RequestTimed)] -> m (Strategy RequestTimed)
      tryStrategies [] = return mzero
      tryStrategies (m : rest) = do
        str <- m
        if nullStrategy str
        then tryStrategies rest
        else return str  -- don't perform the remaining monadic actions
  if bwatch body == WSleep
     && abInSkill SkWait
     && mayContinueSleep
       -- no check of @canSleep@, because sight lowered by sleeping
  then return $! returN "sleep" ReqWait
  else tryStrategies sums

waitBlockNow :: MonadClientRead m => m (Strategy RequestTimed)
waitBlockNow = return $! returN "wait" ReqWait

yellNow :: MonadClientRead m => m (Strategy RequestTimed)
yellNow = return $! returN "yell" ReqYell

pickup :: MonadClientRead m => ActorId -> Bool -> m (Strategy RequestTimed)
pickup aid onlyWeapon = do
  benItemL <- benGroundItems aid
  b <- getsState $ getActorBody aid
  -- This calmE is outdated when one of the items increases max Calm
  -- (e.g., in pickup, which handles many items at once), but this is OK,
  -- the server accepts item movement based on calm at the start, not end
  -- or in the middle.
  -- The calmE is inaccurate also if an item not IDed, but that's intended
  -- and the server will ignore and warn (and content may avoid that,
  -- e.g., making all rings identified)
  actorMaxSk <- getsState $ getActorMaxSkills aid
  let calmE = calmEnough b actorMaxSk
      isWeapon (_, _, _, itemFull, _) =
        IA.checkFlag Ability.Meleeable $ aspectRecordFull itemFull
      filterWeapon | onlyWeapon = filter isWeapon
                   | otherwise = id
      prepareOne (oldN, l4)
                 (Benefit{benInEqp}, _, iid, _, (itemK, _)) =
        let prep newN toCStore = (newN, (iid, itemK, CGround, toCStore) : l4)
            n = oldN + itemK
        in if | benInEqp && calmE && not (eqpOverfull b n) -> prep n CEqp
              | onlyWeapon -> (oldN, l4)
              | otherwise -> prep n CStash
      (_, prepared) = foldl' prepareOne (0, []) $ filterWeapon benItemL
  return $! if null prepared then reject
            else returN "pickup" $ ReqMoveItems prepared

-- This only concerns items that can be equipped, that is with a slot
-- and with @benInEqp@ (which implies @goesIntoEqp@).
-- Such items are moved between any stores, as needed. In this case,
-- from stash to eqp.
equipItems :: MonadClientRead m => ActorId -> m (Strategy RequestTimed)
equipItems aid = do
  body <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  let calmE = calmEnough body actorMaxSk
  fact <- getsState $ (EM.! bfid body) . sfactionD
  eqpAssocs <- getsState $ kitAssocs aid [CEqp]
  stashAssocs <- getsState $ kitAssocs aid [CStash]
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyOrRemembered <- condAimEnemyOrRememberedM aid
  discoBenefit <- getsClient sdiscoBenefit
  localTime <- getsState $ getLocalTime (blid body)
  fleeD <- getsClient sfleeD
  -- In general, AI always equips the best item in stash if it's better
  -- than the best in equipment. Additionally, if there is space left
  -- in equipment for a future good item, an item from stash may be
  -- equipped if it's not much worse than in equipment.
  -- If the item in question is the best item in stash.
  -- at least one copy must remain in stash.
  let improve :: (Int, [(ItemId, Int, CStore, CStore)])
              -> ( [(Int, (ItemId, ItemFullKit))]
                 , [(Int, (ItemId, ItemFullKit))] )
              -> (Int, [(ItemId, Int, CStore, CStore)])
      improve (oldN, l4) (bestStash, bestEqp) =
        let n = 1 + oldN
        in if eqpOverfull body n then (oldN, l4)
           else case (bestStash, bestEqp) of
             ((_, (iidStash, _)) : _, []) ->
               (n, (iidStash, 1, CStash, CEqp) : l4)
             ((vStash, (iidStash, _)) : _, (vEqp, _) : _) | vStash > vEqp ->
               (n, (iidStash, 1, CStash, CEqp) : l4)
             _ -> case (pluralCopiesOfBest bestStash, bestEqp) of
               ((vStash, (iidStash, _)) : _, (vEqp, _) : _)
                 | not (eqpOverfull body (n + 1))  -- 9 items in equipment
                   && vStash >= vEqp - 20 && vStash > 20 ->
                     -- within 2 damage of the best and not too bad absolutely
                     (n, (iidStash, 1, CStash, CEqp) : l4)
               _ -> (oldN, l4)
      getK (_, (itemK, _)) = itemK
      pluralCopiesOfBest bestStash@((_, (_, itemFullKit)) : rest) =
        if getK itemFullKit > 1 then bestStash else rest
      pluralCopiesOfBest [] = []
      heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltasSerious (bcalmDelta body)
      recentlyFled = maybe False (\(_, time) -> timeRecent5 localTime time)
                           (aid `EM.lookup` fleeD)
      uneasy = condAimEnemyOrRemembered
               || not calmE
               || heavilyDistressed
               || recentlyFled
      canEsc = fcanEscape (gkind fact)
      -- We filter out unneeded items. In particular, we ignore them in eqp
      -- when comparing to items we may want to equip, so that the unneeded
      -- but powerful items don't fool us.
      -- In any case, the unneeded items should be removed from equip
      -- in @yieldUnneeded@ earlier or soon after this check.
      -- In other stores we need to filter, for otherwise we'd have
      -- a loop of equip/yield.
      filterNeeded (_, (itemFull, _)) =
        not (hinders condShineWouldBetray uneasy actorMaxSk itemFull
             || not canEsc && IA.isHumanTrinket (itemKind itemFull))
                  -- don't equip items that block progress, e.g., blowtorch
      bestTwo = bestByEqpSlot discoBenefit
                              (filter filterNeeded stashAssocs)
                              (filter filterNeeded eqpAssocs)
      bEqpStash = foldl' improve (0, []) bestTwo
      (_, prepared) = bEqpStash
  return $! if not calmE || null prepared
            then reject
            else returN "equipItems" $ ReqMoveItems prepared

yieldUnneeded :: MonadClientRead m => ActorId -> m (Strategy RequestTimed)
yieldUnneeded aid = do
  body <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  let calmE = calmEnough body actorMaxSk
  eqpAssocs <- getsState $ kitAssocs aid [CEqp]
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyOrRemembered <- condAimEnemyOrRememberedM aid
  discoBenefit <- getsClient sdiscoBenefit
  localTime <- getsState $ getLocalTime (blid body)
  fleeD <- getsClient sfleeD
  -- Here and in @unEquipItems@ AI may hide from the human player,
  -- in shared stash, the Ring of Speed And Bleeding,
  -- which is a bit harsh, but fair. However any subsequent such
  -- rings will not be picked up at all, so the human player
  -- doesn't lose much fun. Additionally, if AI learns alchemy later on,
  -- they can repair the ring, wield it, drop at death and it's
  -- in play again.
  let heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltasSerious (bcalmDelta body)
      recentlyFled = maybe False (\(_, time) -> timeRecent5 localTime time)
                           (aid `EM.lookup` fleeD)
      uneasy = condAimEnemyOrRemembered
               || not calmE
               || heavilyDistressed
               || recentlyFled
      yieldSingleUnneeded (iidEqp, (itemEqp, (itemK, _))) =
        [ (iidEqp, itemK, CEqp, CStash)
        | harmful discoBenefit iidEqp  -- harmful not shared
          || hinders condShineWouldBetray uneasy actorMaxSk itemEqp ]
      yieldAllUnneeded = concatMap yieldSingleUnneeded eqpAssocs
  return $! if not calmE || null yieldAllUnneeded
            then reject
            else returN "yieldUnneeded" $ ReqMoveItems yieldAllUnneeded

-- This only concerns items that @equipItems@ handles, that is
-- with a slot and with @benInEqp@ (which implies @goesIntoEqp@).
unEquipItems :: MonadClientRead m => ActorId -> m (Strategy RequestTimed)
unEquipItems aid = do
  body <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  let calmE = calmEnough body actorMaxSk
  eqpAssocs <- getsState $ kitAssocs aid [CEqp]
  stashAssocs <- getsState $ kitAssocs aid [CStash]
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyOrRemembered <- condAimEnemyOrRememberedM aid
  discoBenefit <- getsClient sdiscoBenefit
  localTime <- getsState $ getLocalTime (blid body)
  fleeD <- getsClient sfleeD
  -- In general, AI unequips only if equipment is full and better stash item
  -- for another slot is likely to come or if the best (or second best)
  -- item in stash is worse than in equipment and at least one better
  -- item remains in equipment.
  let improve :: ( [(Int, (ItemId, ItemFullKit))]
                 , [(Int, (ItemId, ItemFullKit))] )
              -> [(ItemId, Int, CStore, CStore)]
      improve (bestStash, bestEqp) =
        case bestEqp of
          ((_, (iidEqp, itemEqp)) : _) | getK itemEqp > 1
                                         && bestStash `worseThanEqp` bestEqp ->
            -- To share the best items with others, if they care
            -- and if a better or equal item is not already in stash.
            -- The effect is that after each party member has a copy,
            -- a single copy is permanently kept in stash, to quickly
            -- equip a new-joiner.
            [(iidEqp, 1, CEqp, CStash)]
          _ : bestEqp2@((_, (iidEqp, itemEqp)) : _)
            | getK itemEqp > 1
              && bestStash `worseThanEqp` bestEqp2 ->
            -- To share the second best items with others, if they care
            -- and if a better or equal item is not already in stash.
            -- The effect is the same as with the rule above, but only as long
            -- as the best item is scarce. Then this rule doesn't fire and
            -- every second best item copy is eventually equipped by someone.
            [(iidEqp, getK itemEqp, CEqp, CStash)]
          _ -> case reverse bestEqp of
            bestEqpR@((vEqp, (iidEqp, itemEqp)) : _)
              | eqpOverfull body 1  -- 10 items in equipment
                && (bestStash `betterThanEqp` bestEqpR
                    || getK itemEqp > 1 && vEqp < 20) ->
              -- To make place in eqp for an item better than any ours.
              -- Even a minor boost is removed only if stash has a better one.
              -- Also remove extra copies if the item weak, ih hopes
              -- of a prompt better pickup.
              [(iidEqp, 1, CEqp, CStash)]
            _ -> []
      getK (_, (itemK, _)) = itemK
      worseThanEqp ((vStash, _) : _) ((vEqp, _) : _) = vStash < vEqp
      worseThanEqp [] _ = True
      worseThanEqp _ [] = error "unEquipItems: worseThanEqp: []"
      -- Not @>=@ or we could remove a useful item, without replacing it
      -- with a better or even equal one. We only remove it so if the item
      -- is weak and duplicated in equipment.
      betterThanEqp ((vStash, _) : _) ((vEqp, _) : _) = vStash > vEqp
      betterThanEqp [] _ = False
      betterThanEqp _ [] = error "unEquipItems: betterThanEqp: []"
      heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltasSerious (bcalmDelta body)
      recentlyFled = maybe False (\(_, time) -> timeRecent5 localTime time)
                           (aid `EM.lookup` fleeD)
      uneasy = condAimEnemyOrRemembered
               || not calmE
               || heavilyDistressed
               || recentlyFled
      -- Here we don't need to filter out items that hinder (except in stash)
      -- because they are moved to stash and will be equipped by another actor
      -- at another time, where hindering will be completely different.
      -- If they hinder and we unequip them, all the better.
      -- We filter stash to consider only eligible items in @betterThanEqp@.
      filterNeeded (_, (itemFull, _)) =
        not $ hinders condShineWouldBetray uneasy actorMaxSk itemFull
      bestTwo = bestByEqpSlot discoBenefit
                              (filter filterNeeded stashAssocs)
                              eqpAssocs
      bEqpStash = concatMap improve bestTwo
  return $! if not calmE || null bEqpStash
            then reject
            else returN "unEquipItems" $ ReqMoveItems bEqpStash

groupByEqpSlot :: [(ItemId, ItemFullKit)]
               -> EM.EnumMap EqpSlot [(ItemId, ItemFullKit)]
groupByEqpSlot is =
  let f (iid, itemFullKit) =
        let arItem = aspectRecordFull $ fst itemFullKit
        in case IA.aEqpSlot arItem of
          Nothing -> Nothing
          Just es -> Just (es, [(iid, itemFullKit)])
      withES = mapMaybe f is
  in EM.fromListWith (++) withES

bestByEqpSlot :: DiscoveryBenefit
              -> [(ItemId, ItemFullKit)]
              -> [(ItemId, ItemFullKit)]
              -> [( [(Int, (ItemId, ItemFullKit))]
                  , [(Int, (ItemId, ItemFullKit))] )]
bestByEqpSlot discoBenefit eqpAssocs stashAssocs =
  let eqpMap = EM.map (\g -> (g, [])) $ groupByEqpSlot eqpAssocs
      stashMap = EM.map (\g -> ([], g)) $ groupByEqpSlot stashAssocs
      appendTwo (g1, g2) (h1, h2) = (g1 ++ h1, g2 ++ h2)
      eqpStashMap = EM.unionsWith appendTwo [eqpMap, stashMap]
      bestSingle = strongestSlot discoBenefit
      bestTwo eqpSlot (g1, g2) = (bestSingle eqpSlot g1, bestSingle eqpSlot g2)
  in EM.elems $ EM.mapWithKey bestTwo eqpStashMap

harmful :: DiscoveryBenefit -> ItemId -> Bool
harmful discoBenefit iid =
  -- Items that are known, perhaps recently discovered, and it's now revealed
  -- they should not be kept in equipment, should be unequipped
  -- (either they are harmful or they waste eqp space).
  not $ benInEqp $ discoBenefit EM.! iid

-- If enemy (or even a friend) blocks the way, sometimes melee him
-- even though normally you wouldn't.
-- This is also a trick to make a foe use up its non-durable weapons,
-- e.g., on cheap slow projectiles fired in its path.
meleeBlocker :: MonadClient m
             => Ability.Skills -> ActorId -> m (Strategy RequestTimed)
meleeBlocker actorSk aid = do
  b <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  case mtgtMPath of
    Just TgtAndPath{ tapTgt=TEnemy{}
                   , tapPath=Just AndPath{pathList=q : _, pathGoal} }
      | q == pathGoal -> return reject
        -- not a real blocker, but goal enemy, so defer deciding whether
        -- to melee him to the code that deals with goal enemies
    Just TgtAndPath{tapPath=Just AndPath{pathList=q : _, pathGoal}} -> do
      -- We prefer the goal position, so that we can kill the foe and enter it,
      -- but we accept any @q@ as well.
      lvl <- getLevel (blid b)
      let maim | adjacent (bpos b) pathGoal = Just pathGoal
               | adjacent (bpos b) q = Just q
               | otherwise = Nothing  -- MeleeDistant
          lBlocker = case maim of
            Nothing -> []
            Just aim -> posToAidsLvl aim lvl
      case lBlocker of
        aid2 : _ -> do
          body2 <- getsState $ getActorBody aid2
          actorMaxSk2 <- getsState $ getActorMaxSkills aid2
          -- No problem if there are many projectiles at the spot. We just
          -- attack the first one.
          if | bproj body2  -- displacing saves a move, so don't melee
               && getSk SkDisplace actorSk > 0 ->
               return reject
             | isFoe (bfid b) fact (bfid body2)
                 -- at war with us, so hit, not displace
               || isFriend (bfid b) fact (bfid body2) -- don't start a war
                  && getSk SkDisplace actorSk <= 0
                       -- can't displace
                  && getSk SkMove actorSk > 0  -- blocked move
                  && 3 * bhp body2 < bhp b  -- only get rid of weak friends
                  && gearSpeed actorMaxSk2 <= gearSpeed actorMaxSk -> do
               mel <- maybeToList <$> pickWeaponClient aid aid2
               return $! liftFrequency $ uniformFreq "melee in the way" mel
             | otherwise -> return reject
        [] -> return reject
    _ -> return reject  -- probably no path to the enemy, if any

-- Everybody melees in a pinch, skills and weapons allowing,
-- even though some prefer ranged attacks. However only potentially harmful
-- enemies or those having loot or moving (can follow and spy) are meleed
-- (or those that are in the way, see elsewhere).
-- Projectiles are rather displaced or sidestepped, because it's cheaper
-- and also the projectile may be explosive and so harm anyway
-- and also if ignored it may hit enemies --- AI can't tell.
meleeAny :: MonadClient m => ActorId -> m (Strategy RequestTimed)
meleeAny aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  adjBigAssocs <- getsState $ adjacentBigAssocs b
  actorMaxSkills <- getsState sactorMaxSkills
  let foe b2 = isFoe (bfid b) fact (bfid b2)
      adjFoes = filter (uncurry $ actorWorthKilling actorMaxSkills)
                $ filter (foe . snd) adjBigAssocs
  btarget <- getsClient $ getTarget aid
  mtargets <- case btarget of
    Just (TEnemy aid2) -> do
      b2 <- getsState $ getActorBody aid2
      return $! if adjacent (bpos b2) (bpos b)
                   && actorWorthKilling actorMaxSkills aid2 b2
                then Just [(aid2, b2)]
                else Nothing
    _ -> return Nothing
  let adjTargets = fromMaybe adjFoes mtargets
  mels <- mapM (pickWeaponClient aid . fst) adjTargets
  let freq = uniformFreq "melee adjacent" $ catMaybes mels
  return $! liftFrequency freq

-- The level the actor is on is either explored or the actor already
-- has a weapon equipped, so no need to explore further, he tries to find
-- enemies on other levels, hence triggering terrain.
-- We don't verify any embedded item is targeted by the actor, but at least
-- the actor doesn't target a visible enemy at this point.
-- TODO: In @actionStrategy@ we require minimal @SkAlter@ even for the case
-- of triggerable tile underfoot. Let's say this quirk is a specialization
-- of AI actors, because there are usually many, so not all need to trigger.
trigger :: MonadClientRead m
        => ActorId -> FleeViaStairsOrEscape
        -> m (Strategy RequestTimed)
trigger aid fleeVia = do
  b <- getsState $ getActorBody aid
  lvl <- getLevel (blid b)
  let f pos = case EM.lookup pos $ lembed lvl of
        Nothing -> Nothing
        Just bag -> Just (pos, bag)
      pbags = mapMaybe f $ bpos b : vicinityUnsafe (bpos b)
  efeat <- embedBenefit fleeVia aid pbags
  return $! liftFrequency $ toFreq "trigger"
    [ (ceiling benefit, ReqAlter pos)
    | (benefit, (pos, _)) <- efeat
    , let underFeet = pos == bpos b
    , underFeet
      || not (occupiedBigLvl pos lvl)
         && not (occupiedProjLvl pos lvl) -- AlterBlockActor
         && EM.notMember pos (lfloor lvl) ]  -- AlterBlockItem

projectItem :: MonadClientRead m
            => Ability.Skills -> ActorId -> m (Strategy RequestTimed)
projectItem actorSk aid = do
  btarget <- getsClient $ getTarget aid
  b <- getsState $ getActorBody aid
  -- We query target, not path, because path is not needed for flinging.
  -- Even if unknown tiles exist between us and the target, we assume
  -- they are walkable and not just transparent and we happily try to shoot.
  mfpos <- getsState $ aidTgtToPos (Just aid) (blid b) btarget
  case (btarget, mfpos) of
    (_, Just fpos) | adjacent (bpos b) fpos -> return reject
    (Just (TEnemy aidE), Just fpos) -> do
      actorMaxSkills <- getsState sactorMaxSkills
      body <- getsState $ getActorBody aidE
      if actorWorthChasing actorMaxSkills aidE body then do
        cops <- getsState scops
        lvl <- getLevel (blid b)
        seps <- getsClient seps
        case makeLine False b fpos seps cops lvl of
          Just newEps -> do
            let skill = getSk SkProject actorSk
            -- ProjectAimOnself, ProjectBlockActor, ProjectBlockTerrain
            -- and no actors or obstacles along the path.
            benList <- condProjectListM skill aid
            localTime <- getsState $ getLocalTime (blid b)
            let fRanged (benR, cstore, iid, itemFull, kit) =
                  -- If the item is discharged, neither the kinetic hit nor
                  -- any effects activate, so no point projecting.
                  -- This changes in time, so recharging is not included
                  -- in @condProjectListM@, but checked here, just before fling.
                  let recharged = hasCharge localTime kit
                      arItem = aspectRecordFull itemFull
                      trange = IA.totalRange arItem $ itemKind itemFull
                      bestRange =
                        chessDist (bpos b) fpos + 2  -- margin for fleeing
                      rangeMult =  -- penalize wasted or unsafely low range
                        10 + max 0 (10 - abs (trange - bestRange))
                  in if trange >= chessDist (bpos b) fpos && recharged
                     then Just ( - ceiling (benR * intToDouble rangeMult / 10)
                               , ReqProject fpos newEps iid cstore )
                     else Nothing
                benRanged = mapMaybe fRanged benList
            return $! liftFrequency $ toFreq "projectItem" benRanged
          _ -> return reject
      else return reject
    _ -> return reject

data ApplyItemGroup = ApplyAll | ApplyFirstAid
  deriving Eq

applyItem :: MonadClientRead m
          => Ability.Skills -> ActorId -> ApplyItemGroup
          -> m (Strategy RequestTimed)
applyItem actorSk aid applyGroup = do
  COps{corule} <- getsState scops
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyOrRemembered <- condAimEnemyOrRememberedM aid
  localTime <- getsState $ getLocalTime (blid b)
  let calmE = calmEnough b actorSk
      heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltasSerious (bcalmDelta b)
      uneasy = condAimEnemyOrRemembered
               || not calmE
               || heavilyDistressed
        -- don't take recent fleeing into account when item can be lost
      skill = getSk SkApply actorSk
      -- This detects if the value of keeping the item in eqp is in fact < 0.
      hind = hinders condShineWouldBetray uneasy actorSk
      canEsc = fcanEscape (gkind fact)
      permittedActor cstore itemFull kit =
        fromRight False
        $ permittedApply corule localTime skill calmE cstore itemFull kit
      disqualify :: Bool -> IK.Effect -> Bool
      -- These effects tweak items, which is only situationally beneficial
      -- and not really the best idea while in combat.
      disqualify _ IK.PolyItem = True
      disqualify _ IK.RerollItem = True
      disqualify _ IK.DupItem = True
      disqualify _ IK.Identify = True
      -- This is hard to use and would be wasted recharging stomach.
      disqualify _ IK.Recharge{} = True
      -- This is usually the main effect of item and it's useless without Calm.
      disqualify durable IK.Summon{} =
        durable && (bcalm b < xM 30 || not calmE)
      disqualify durable (IK.AtMostOneOf l) = any (disqualify durable) l
      disqualify durable (IK.OneOf l) = any (disqualify durable) l
      disqualify durable (IK.OnUser eff) = disqualify durable eff
      disqualify durable (IK.AndEffect eff1 eff2) =
        disqualify durable eff1 || disqualify durable eff2
      disqualify durable (IK.OrEffect eff1 eff2) =
        disqualify durable eff1 || disqualify durable eff2
      disqualify durable (IK.SeqEffect effs) = any (disqualify durable) effs
      disqualify durable (IK.When _ eff) = disqualify durable eff
      disqualify durable (IK.Unless _ eff) = disqualify durable eff
      disqualify durable (IK.IfThenElse _ eff1 eff2) =
        disqualify durable eff1 || disqualify durable eff2
      disqualify _ _ = False
      q (Benefit{benInEqp}, cstore, _, itemFull@ItemFull{itemKind}, kit) =
        let arItem = aspectRecordFull itemFull
            durable = IA.checkFlag Durable arItem
        in (not benInEqp  -- can't wear, so OK to break
            || durable  -- can wear, but can't break, even better
            || not (IA.checkFlag Ability.Meleeable arItem)
                 -- anything else expendable
               && hind itemFull)  -- hinders now, so possibly often, so away!
           && permittedActor (Just cstore) itemFull kit
           && not (any (disqualify durable) $ IK.ieffects itemKind)
           && (canEsc || not (IA.isHumanTrinket itemKind))
                -- A hack to prevent monsters from using up treasure
                -- meant for heroes.
      stores = [CStash, CGround, COrgan] ++ [CEqp | calmE]
  discoBenefit <- getsClient sdiscoBenefit
  benList <- getsState $ benAvailableItems discoBenefit aid stores
  getKind <- getsState $ flip getIidKind
  let (myBadGrps, myGoodGrps) = partitionEithers $ mapMaybe (\iid ->
        let itemKind = getKind iid
        in if maybe False (> 0) $ lookup IK.CONDITION $ IK.ifreq itemKind
           then Just $ if benInEqp (discoBenefit EM.! iid)
                       then Right $ DefsInternal.GroupName $ IK.iname itemKind
                         -- conveniently, @iname@ matches @ifreq@
                       else Left $ DefsInternal.GroupName $ IK.iname itemKind
           else Nothing) (EM.keys $ borgan b)
      fTool benAv@( Benefit{benApply}, cstore, iid
                  , itemFull@ItemFull{itemKind}, _ ) =
        let dropsGrps = IK.getDropOrgans itemKind  -- @Impress@ effect included
            dropsBadOrgans =
              not (null myBadGrps)
              && (IK.CONDITION `elem` dropsGrps
                  || not (null (dropsGrps `intersect` myBadGrps)))
            dropsImpressed =
              IK.S_IMPRESSED `elem` myBadGrps
              && (IK.CONDITION `elem` dropsGrps
                  || IK.S_IMPRESSED `elem` dropsGrps)
            dropsGoodOrgans =
              not (null myGoodGrps)
              && (IK.CONDITION `elem` dropsGrps
                  || not (null (dropsGrps `intersect` myGoodGrps)))
            wastesDrop = not dropsBadOrgans && not (null dropsGrps)
            -- Don't include @Ascend@ nor @Teleport@, because maybe no foe near.
            -- Don't include @AtMostOneOf@ nor @OneOf@ because
            -- other effects may kill you.
            getHP (IK.RefillHP p) = max 0 p
            getHP (IK.OnUser eff) = getHP eff
            getHP (IK.AndEffect eff1 eff2) = getHP eff1 + getHP eff2
            getHP (IK.OrEffect eff1 _) = getHP eff1
            getHP (IK.SeqEffect effs) = sum $ map getHP effs
            getHP (IK.When _ eff) = getHP eff
            getHP (IK.Unless _ eff) = getHP eff
            getHP (IK.IfThenElse _ eff1 eff2) = getHP eff1 + getHP eff2
            getHP _ = 0
            healPower = sum $ map getHP $ IK.ieffects itemKind
            wastesHP = xM healPower
                       > xM (Ability.getSk Ability.SkMaxHP actorSk) - bhp b
            durable = IA.checkFlag Durable $ aspectRecordFull itemFull
            situationalBenApply =
              if | dropsBadOrgans -> if dropsImpressed
                                     then benApply + 1000  -- crucial
                                     else benApply + 20
                 | wastesDrop || wastesHP -> benApply - 10
                 | otherwise -> benApply
            coeff CGround = 2  -- pickup turn saved
            coeff COrgan = if durable then 1 else 1000
              -- if not durable, must hinder currently or be very potent
            coeff CEqp = if durable then 1 else 1000
            coeff CStash = 1
            benR = ceiling situationalBenApply * coeff cstore
            canApply =
              situationalBenApply > 0
              && (dropsImpressed || not wastesHP)
                -- waste healing only if it drops impressed;
                -- otherwise apply anything beneficial at will
              && case applyGroup of
                ApplyFirstAid -> q benAv && (healPower > 0 || dropsImpressed)
                  -- when low HP, Calm easy to deplete, so impressed crucial
                ApplyAll -> q benAv && not dropsGoodOrgans
                  -- not an emergency, so don't sacrifice own good conditions
        in if canApply
           then Just (benR, ReqApply iid cstore)
           else Nothing
      benTool = mapMaybe fTool benList
  return $! liftFrequency $ toFreq "applyItem" benTool

-- If low on health or alone, flee in panic, close to the path to target
-- and as far from the attackers, as possible. Usually fleeing from
-- foes will lead towards friends, but we don't insist on that.
flee :: MonadClient m
     => Ability.Skills -> ActorId -> Bool -> [(Int, Point)]
     -> m (Strategy RequestTimed)
flee actorSk aid avoidAmbient fleeL = do
  COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  localTime <- getsState $ getLocalTime (blid b)
  fleeD <- getsClient sfleeD
  let recentlyFled = maybe False (\(_, time) -> timeRecent5 localTime time)
                           (aid `EM.lookup` fleeD)
  -- Regardless if fleeing accomplished, mark the need, but don't forget
  -- the location of initial danger, in case enemies not seen any more,
  -- if not too old.
  unless recentlyFled $
    modifyClient $ \cli ->
      cli {sfleeD = EM.insert aid (bpos b, localTime) (sfleeD cli)}
  lvl <- getLevel $ blid b
  let isAmbient pos = Tile.isLit coTileSpeedup (lvl `at` pos)
                      && Tile.isWalkable coTileSpeedup (lvl `at` pos)
                        -- if solid, will be altered and perhaps darkened
      fleeAmbientAvoided = filter (not . isAmbient . snd) fleeL
      fleeAmbient = if avoidAmbient && not (null fleeAmbientAvoided)
                    then fleeAmbientAvoided
                    else fleeL
  let vVic = map (second (`vectorToFrom` bpos b)) fleeAmbient
      str = liftFrequency $ toFreq "flee" vVic
  mapStrategyM (moveOrRunAid actorSk aid) str

-- The result of all these conditions is that AI displaces rarely,
-- but it can't be helped as long as the enemy is smart enough to form fronts.
displaceFoe :: MonadClientRead m => ActorId -> m (Strategy RequestTimed)
displaceFoe aid = do
  COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  fact <- getsState $ (EM.! bfid b) . sfactionD
  friends <- getsState $ friendRegularList (bfid b) (blid b)
  adjBigAssocs <- getsState $ adjacentBigAssocs b
  let foe (_, b2) = isFoe (bfid b) fact (bfid b2)
      adjFoes = filter foe adjBigAssocs
      walkable p =  -- DisplaceAccess
        Tile.isWalkable coTileSpeedup (lvl `at` p)
      nFriends body = length $ filter (adjacent (bpos body) . bpos) friends
      nFrNew = nFriends b + 1
      qualifyActor (aid2, b2) = do
        case posToAidsLvl (bpos b2) lvl of
          _ | not (walkable (bpos b2))  -- DisplaceAccess
              || boldpos b == Just (bpos b2)
                 && boldpos b2 == Just (bpos b) ->  -- avoid short loops
              return Nothing
          [_] -> do
            actorMaxSk <- getsState $ getActorMaxSkills aid2
            dEnemy <- getsState $ dispEnemy aid aid2 actorMaxSk
              -- DisplaceDying, DisplaceBraced, DisplaceImmobile,
              -- DisplaceSupported
            let nFrOld = nFriends b2
            return $! if dEnemy && nFrOld < nFrNew
                      then Just ( (nFrNew - nFrOld) ^ (2 :: Int)
                                , ReqDisplace aid2 )
                      else Nothing
          _ -> return Nothing  -- DisplaceProjectiles
  foes <- mapM qualifyActor adjFoes
  return $! liftFrequency $ toFreq "displaceFoe" $ catMaybes foes

displaceBlocker :: MonadClientRead m => ActorId -> Bool -> m (Strategy RequestTimed)
displaceBlocker aid retry = do
  b <- getsState $ getActorBody aid
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  case mtgtMPath of
    Just TgtAndPath{ tapTgt=TEnemy{}
                   , tapPath=Just AndPath{pathList=q : _, pathGoal} }
      | q == pathGoal  -- not a real blocker but goal; only try to displace
                       -- if desperate (that is, already tried to melee it)
        && not retry ->
        return reject
    Just TgtAndPath{tapPath=Just AndPath{pathList=q : _}}
      | adjacent (bpos b) q ->  -- not veered off target too much
        displaceTgt aid q retry
    _ -> return reject  -- goal reached

displaceTgt :: MonadClientRead m
            => ActorId -> Point -> Bool -> m (Strategy RequestTimed)
displaceTgt source tpos retry = do
  COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody source
  actorMaxSkills <- getsState sactorMaxSkills
  let !_A = assert (adjacent (bpos b) tpos) ()
  lvl <- getLevel $ blid b
  let walkable p =  -- DisplaceAccess
        Tile.isWalkable coTileSpeedup (lvl `at` p)
  case posToAidsLvl tpos lvl of
    _ | not (walkable tpos) -> return reject  -- DisplaceAccess
    [aid2] -> do
      b2 <- getsState $ getActorBody aid2
      mleader <- getsClient sleader
      if | bwatch b2 `elem` [WSleep, WWake] ->
             return $! returN "displace sleeping" $ ReqDisplace aid2
         | Just aid2 == mleader -> return reject
         | boldpos b == Just tpos
           && boldpos b2 == Just (bpos b) ->
             return reject  -- avoid short loops
         | otherwise -> do
           tfact <- getsState $ (EM.! bfid b2) . sfactionD
           mtgtMPath <- getsClient $ EM.lookup aid2 . stargetD
           enemyTgt <- condAimEnemyOrRememberedM source
           enemyTgt2 <- condAimEnemyOrRememberedM aid2
           case mtgtMPath of
             -- I can see targets of only own team, so no check of @bfid@.
             Just TgtAndPath{tapPath=Just AndPath{pathList=q : _}}
               | q == bpos b ->  -- teammate wants to swap
                 return $! returN "displace mutual" $ ReqDisplace aid2
             Just _ -> return $!
               -- Teammate, possibly without path, for whatever reason.
               if retry  -- me desperate
                  || Just (blid b2, bpos b2) == gstash tfact  -- guarding; lazy
                  || getSk SkDisplace (actorMaxSkills EM.! aid2) <= 0
                       -- can't displace back
                  || enemyTgt && not enemyTgt2
                       -- he doesn't have Enemy target and I have, so push him
                       -- aside, because, for heroes, he will never be a leader,
                       -- so he can't step aside himself
               then returN "displace teammate" $ ReqDisplace aid2
               else reject
             _ -> do  -- an enemy or ally or disoriented teammate
               actorMaxSk <- getsState $ getActorMaxSkills aid2
               dEnemy <- getsState $ dispEnemy source aid2 actorMaxSk
                 -- DisplaceDying, DisplaceBraced, DisplaceImmobile,
                 -- DisplaceSupported
               return $!
                 if bfid b == bfid b2  -- disoriented teammate; doesn't care
                    || isFoe (bfid b2) tfact (bfid b) && dEnemy  -- foe
                    || retry  -- ally, I need to be desperate, as above
                 then returN "displace other" $ ReqDisplace aid2
                 else reject
    _ -> return reject  -- DisplaceProjectiles and no blocker at all

chase :: MonadClientRead m
      => Ability.Skills -> ActorId -> Bool -> Bool -> m (Strategy RequestTimed)
chase actorSk aid avoidAmbient retry = do
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  let -- With no leader, the goal is vague, so permit arbitrary detours.
      relaxed = not $ fhasPointman (gkind fact)
      strAmbient avoid = case mtgtMPath of
        Just TgtAndPath{tapPath=Just AndPath{pathList=q : _, ..}} ->
          if pathGoal == bpos body
          then return reject  -- done; picking up items, etc.
          else moveTowards actorSk aid avoid q pathGoal (relaxed || retry)
        _ -> return reject  -- goal reached or banned ambient lit tile
  strAvoided <- strAmbient avoidAmbient
  str <- if avoidAmbient && nullStrategy strAvoided
         then strAmbient False
         else return strAvoided
  mapStrategyM (moveOrRunAid actorSk aid) str

moveTowards :: MonadClientRead m
            => Ability.Skills -> ActorId -> Bool -> Point -> Point -> Bool
            -> m (Strategy Vector)
moveTowards actorSk aid avoidAmbient target goal relaxed = do
  COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let source = bpos b
      alterSkill = getSk SkAlter actorSk
      !_A = assert (adjacent source target
                    `blame` (source, target, aid, b, goal)) ()
  fact <- getsState $ (EM.! bfid b) . sfactionD
  salter <- getsClient salter
  noFriends <- getsState $ \s p ->
    all (isFoe (bfid b) fact . bfid . snd)
        (posToAidAssocs p (blid b) s)  -- don't kill own projectiles
  let lalter = salter EM.! blid b
      isAmbient pos = Tile.isLit coTileSpeedup (lvl `at` pos)
                      && Tile.isWalkable coTileSpeedup (lvl `at` pos)
                        -- if solid, will be altered and perhaps darkened
      -- Only actors with SkAlter can search for hidden doors, etc.
      enterableHere p = alterSkill >= fromEnum (lalter PointArray.! p)
      permittedHere p | avoidAmbient = enterableHere p && not (isAmbient p)
                      | otherwise = enterableHere p
  -- If target is the final goal, is not occupied and is lit, permit
  -- movement into lit position, regardless.
  if noFriends target && (target == goal && enterableHere target
                          || permittedHere target) then
    return $! returN "moveTowards target" $ target `vectorToFrom` source
  else do
    -- This lets animals mill around, even when blocked,
    -- because they have nothing to lose (unless other animals melee).
    -- Blocked heroes instead don't become leaders and don't move
    -- until friends sidestep to let them reach their goal.
    let goesBack p = Just p == boldpos b
        nonincreasing p = chessDist source goal >= chessDist p goal
        isSensible | relaxed = \p -> noFriends p
                                     && permittedHere p
                   | otherwise = \p -> nonincreasing p
                                       && not (goesBack p)
                                       && noFriends p
                                       && permittedHere p
        sensible = [ ((goesBack p, chessDist p goal), v)
                   | v <- moves
                   , let p = source `shift` v
                   , isSensible p ]
        -- @SortOn@ less efficient here, because function cheap.
        sorted = sortBy (comparing fst) sensible
        groups = map (map snd) $ groupBy ((==) `on` fst) sorted
        freqs = map (liftFrequency . uniformFreq "moveTowards") groups
    return $! foldr (.|) reject freqs

-- Actor moves or searches or alters or attacks.
-- This function is very general, even though it's often used in contexts
-- when only one or two of the many cases can possibly occur.
moveOrRunAid :: MonadClientRead m
             => Ability.Skills -> ActorId -> Vector -> m (Maybe RequestTimed)
moveOrRunAid actorSk source dir = do
  COps{coTileSpeedup} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
  lvl <- getLevel lid
  let walkable =  -- DisplaceAccess
        Tile.isWalkable coTileSpeedup (lvl `at` tpos)
      notLooping body p =  -- avoid displace loops
        boldpos body /= Just p || actorWaits body
      spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
  -- We start by checking actors at the target position,
  -- which gives a partial information (actors can be invisible),
  -- as opposed to accessibility (and items) which are always accurate
  -- (tiles can't be invisible).
  case posToAidsLvl tpos lvl of
    [target] | walkable
               && getSk SkDisplace actorSk > 0
               && notLooping sb tpos -> do
      -- @target@ can be a foe, as well as a friend.
      tb <- getsState $ getActorBody target
      tfact <- getsState $ (EM.! bfid tb) . sfactionD
      actorMaxSk <- getsState $ getActorMaxSkills target
      dEnemy <- getsState $ dispEnemy source target actorMaxSk
        -- DisplaceDying, DisplaceBraced, DisplaceImmobile, DisplaceSupported
      if isFoe (bfid tb) tfact (bfid sb) && not dEnemy
      then return Nothing
      else return $ Just $ ReqDisplace target
    [] | walkable && getSk SkMove actorSk > 0 ->
      -- Movement requires full access. The potential invisible actor is hit.
      return $ Just $ ReqMove dir
    [] | not walkable
         && getSk SkAlter actorSk
              >= Tile.alterMinWalk coTileSpeedup t  -- AlterUnwalked
         -- Only possible if items allowed inside unwalkable tiles:
         && EM.notMember tpos (lfloor lvl) ->  -- AlterBlockItem
      -- Not walkable, but alter skill suffices, so search or alter the tile.
      -- We assume that unalterable unwalkable tiles are protected by high
      -- skill req. We don't alter walkable tiles (e.g., to close doors).
      return $ Just $ ReqAlter tpos
    _ -> return Nothing  -- can't displace, move nor alter
