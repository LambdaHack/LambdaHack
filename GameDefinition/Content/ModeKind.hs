-- | The type of game mode definitions.
module Content.ModeKind
  ( -- * Group names
    groupNamesSingleton, groupNames
  , -- * Content
    content
#ifdef EXPOSE_INTERNAL
  -- * Group name patterns
  , pattern RAID, pattern BRAWL, pattern LONG, pattern CRAWL, pattern FOGGY, pattern SHOOTOUT, pattern PERILOUS, pattern HUNT, pattern NIGHT, pattern ESCAPE, pattern BURNING, pattern ZOO, pattern RANGED, pattern AMBUSH, pattern SAFARI, pattern DIG, pattern SEE, pattern SHORT, pattern CRAWL_EMPTY, pattern CRAWL_SURVIVAL, pattern SAFARI_SURVIVAL, pattern BATTLE, pattern BATTLE_DEFENSE, pattern BATTLE_SURVIVAL, pattern DEFENSE, pattern DEFENSE_EMPTY
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Content.CaveKind hiding (content, groupNames, groupNamesSingleton)
import Content.ItemKindActor
import Content.ModeKindPlayer
import Game.LambdaHack.Content.CaveKind (CaveKind, pattern DEFAULT_RANDOM)
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Defs

-- * Group name patterns

groupNamesSingleton :: [GroupName ModeKind]
groupNamesSingleton =
       [RAID, BRAWL, LONG, CRAWL, FOGGY, SHOOTOUT, PERILOUS, HUNT, NIGHT, ESCAPE, BURNING, ZOO, RANGED, AMBUSH, SAFARI, DIG, SEE, SHORT, CRAWL_EMPTY, CRAWL_SURVIVAL, SAFARI_SURVIVAL, BATTLE, BATTLE_DEFENSE, BATTLE_SURVIVAL, DEFENSE, DEFENSE_EMPTY]

pattern RAID, BRAWL, LONG, CRAWL, FOGGY, SHOOTOUT, PERILOUS, HUNT, NIGHT, ESCAPE, BURNING, ZOO, RANGED, AMBUSH, SAFARI, DIG, SEE, SHORT, CRAWL_EMPTY, CRAWL_SURVIVAL, SAFARI_SURVIVAL, BATTLE, BATTLE_DEFENSE, BATTLE_SURVIVAL, DEFENSE, DEFENSE_EMPTY :: GroupName ModeKind

groupNames :: [GroupName ModeKind]
groupNames = [NO_CONFIRMS]

pattern RAID = GroupName "raid"
pattern BRAWL = GroupName "brawl"
pattern LONG = GroupName "long"
pattern CRAWL = GroupName "crawl"
pattern FOGGY = GroupName "foggy"
pattern SHOOTOUT = GroupName "shootout"
pattern PERILOUS = GroupName "perilous"
pattern HUNT = GroupName "hunt"
pattern NIGHT = GroupName "night"
pattern ESCAPE = GroupName "escape"
pattern BURNING = GroupName "burning"
pattern ZOO = GroupName "zoo"
pattern RANGED = GroupName "ranged"
pattern AMBUSH = GroupName "ambush"
pattern SAFARI = GroupName "safari"
pattern DIG = GroupName "dig"
pattern SEE = GroupName "see"
pattern SHORT = GroupName "short"
pattern CRAWL_EMPTY = GroupName "crawlEmpty"  -- only the first word matters
pattern CRAWL_SURVIVAL = GroupName "crawlSurvival"
pattern SAFARI_SURVIVAL = GroupName "safariSurvival"
pattern BATTLE = GroupName "battle"
pattern BATTLE_DEFENSE = GroupName "battleDefense"
pattern BATTLE_SURVIVAL = GroupName "battleSurvival"
pattern DEFENSE = GroupName "defense"
pattern DEFENSE_EMPTY = GroupName "defenseEmpty"

-- * Content

content :: [ModeKind]
content =
  [raid, brawl, crawl, shootout, hunt, escape, zoo, ambush, safari, dig, see, short, crawlEmpty, crawlSurvival, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverCrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverSafari]

raid,    brawl, crawl, shootout, hunt, escape, zoo, ambush, safari, dig, see, short, crawlEmpty, crawlSurvival, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverCrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverSafari :: ModeKind

-- What other symmetric (two only-one-moves factions) and asymmetric vs crowd
-- scenarios make sense (e.g., are good for a tutorial or for standalone
-- extreme fun or are impossible as part of a crawl)?
-- sparse melee at night: no, shade ambush in brawl is enough
-- dense melee: no, keeping big party together is a chore and big enemy
--   party is less fun than huge enemy party
-- crowd melee in daylight: no, possible in crawl and at night is more fun
-- sparse ranged at night: no, less fun than dense and if no reaction fire,
--   just a camp fest or firing blindly
-- dense ranged in daylight: no, less fun than at night with flares
-- crowd ranged: no, fish in a barrel, less predictable and more fun inside
--   crawl, even without reaction fire

raid = ModeKind
  { msymbol = 'r'
  , mname   = "raid (tutorial, 1)"
  , mfreq   = [(RAID, 1), (CAMPAIGN_SCENARIO, 1)]
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mendMsg = [ (Killed, "This expedition has gone wrong. However, scientific mind does not despair, but analyzes and corrects. Did you perchance awake one animal too many? Did you remember to try using all consumables at your disposal for your immediate survival? Did you choose a challenge with difficulty level within your means? Answer honestly, ponder wisely, experiment methodically.")
              , (Defeated, "Regrettably, the other team snatched the grant, while you were busy contemplating natural phenomena. Science is a competitive sport, as sad as it sounds. It's not enough to make a discovery, you have to get there first.")
              , (Escape, "You've got hold of the machine! Think of the hours of fun taking it apart and putting it back together again! That's a great first step on your quest to solve the typing problems of the world.") ]
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Two heroes vs. Spawned enemies"
      , "* Gather gold"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "An incredibly advanced typing machine worth 100 gold is buried at the exit of this maze. Be the first to find it and fund a research team that makes typing accurate and dependable forever."
  , mreason = "In addition to initiating the (loose) game plot, this scenario serves as an introductory tutorial. Relax, explore, gather loot, find the exit and escape. With some luck, you won't even need to fight anything."
  , mhint   = "You can't use gathered items in your next encounters, so trigger any consumables at will. Feel free to scout with only one of the heroes and keep the other one immobile, e.g., standing guard over the squad's shared inventory stash. If in grave danger, retreat with the scout to join forces with the guard. The more gold collected and the faster the victory, the higher your score in this encounter."
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { msymbol = 'k'
  , mname   = "brawl (tutorial, 2)"
  , mfreq   = [(BRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mendMsg = [ (Killed, "The inquisitive scholars turned out to be envious of our deep insight to the point of outright violence. It would still not result in such a defeat and recanting of our thesis if we figured out to use terrain to protect us from missiles or even completely hide our presence. It would also help if we honourably kept our ground together to the end, at the same time preventing the overwhelming enemy forces from brutishly ganging up on our modest-sized, though valiant, research team.")
              , (Conquer, "That's settled: local compactness *is* necessary for relative completeness, given the assumptions.") ]
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Three heroes vs. Three human enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "Your research team disagrees over a drink with some gentlemen scientists about premises of a relative completeness theorem and there's only one way to settle that."
      -- Not enough space with square fonts and also this is more of a hint than a flavour: Remember to keep your party together when opponents are spotted, or they might be tempted to silence solitary disputants one by one and so win the altercation.
  , mreason = "In addition to advancing game plot, this scenario trains melee, squad formation and stealth. The battle is completely symmetric, both in numbers, goals (incapacitate all enemies) and squad capabilities (only the pointman moves, while all others either melee or wait). Observe and mimic the enemies and savour the fairness --- you won't find any in the main crawl scenario that follows."
  , mhint   = "Run a short distance with Shift or LMB, switch the pointman with Tab, repeat. In open terrain, if you keep distance between teammates, this resembles the leap frog infantry tactics. For best effects, end each sprint behind a cover or concealment.\nIf you get beaten repeatedly, try using all consumables you find. Ponder the hints from the defeat message, in particular the one about keeping your party together once the opponents are spotted. However, if you want to discover a winning tactics on your own, make sure to ignore any such tips until you succeed."
  }

crawl = ModeKind
  { msymbol = 'c'
  , mname   = "long crawl (main)"
  , mfreq   = [(LONG, 1), (CRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mendMsg = [ (Killed, "To think that followers of science and agents of enlightenment would earn death as their reward! Where did we err in our ways? Perhaps nature should not have been disturbed so brashly and the fell beasts woken up from their slumber so eagerly? Perhaps the gathered items should have been used for scientific experiments on the spot rather than hoarded as if of base covetousness? Or perhaps the challenge, chosen freely but without the foreknowledge of the grisly difficulty, was insurmountable and forlorn from the start, despite the enormous power of educated reason at out disposal?")
              , (Escape, "It's better to live to tell the tale than to choke on more than one can swallow. There was no more exquisite cultural artifacts and glorious scientific wonders in these forbidding tunnels anyway. Or were there?") ]
  , mrules  = T.intercalate "\n" $
      [ "* Many levels"
      , "* Three heroes vs. Spawned enemies"
      , "* Gather gold, gems and elixirs"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "Enjoy the peaceful seclusion of these cold austere tunnels, but don't let wanton curiosity, greed and the ever-creeping abstraction madness keep you down there for too long. If you find survivors (whole or perturbed or segmented) of the past scientific missions, exercise extreme caution and engage or ignore at your discretion."
  , mreason = "This is the main, longest and most replayable scenario of the game."
  , mhint   = "If you keep dying, attempt the subsequent game modes as a breather (perhaps at lowered difficulty). They fill the gaps in the plot and teach particular skills that may come in handy and help you discover new tactics of your own or come up with a strategy for staving off the attrition."
  }

-- The trajectory tip is important because of tactics of scout looking from
-- behind a bush and others hiding in mist. If no suitable bushes,
-- fire once and flee into mist or behind cover. Then whomever is out of LOS
-- range or inside mist can shoot at the last seen enemy locations,
-- adjusting aim according to sounds and incoming missile trajectories.
-- If the scout can't find bushes or glass building to set a lookout,
-- the other team members are more spotters and guardians than snipers
-- and that's their only role, so a small party makes sense.
shootout = ModeKind  -- sparse ranged in daylight
  { msymbol = 's'
  , mname   = "foggy shootout (3)"
  , mfreq   = [(FOGGY, 1), (SHOOTOUT, 1), (CAMPAIGN_SCENARIO, 1)]
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mendMsg = []
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Three heroes vs. Three human enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "Whose arguments are most striking and whose ideas fly fastest? Let's scatter up, attack the problems from different angles and find out."
  , mreason = "This scenario teaches the ranged combat skill in the simplified setup of fully symmetric battle."
  , mhint   = "Try to come up with the best squad formation for this tactical challenge. Don't despair if you run out of ammo, because if you aim truly, enemy has few hit points left at this point. In turn, when trying to avoid enemy projectiles, you can display the trajectory of any soaring entity by pointing it with the crosshair in aiming mode."
  }

hunt = ModeKind  -- melee vs ranged with reaction fire in daylight
  { msymbol = 'h'
  , mname   = "perilous hunt (4)"
  , mfreq   = [(PERILOUS, 1), (HUNT, 1), (CAMPAIGN_SCENARIO, 1)]
  , mroster = rosterHunt
  , mcaves  = cavesHunt
  , mendMsg = []
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Seven heroes vs. Seven human enemies capable of concurrent attacks"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "Who is the hunter and who is the prey? The only criterion is last man standing when the chase ends."
  , mreason = "This scenario is quite a tactical challenge, because enemies are allowed to fling their ammo simultaneously at your team, which has no such ability."
  , mhint   = "Try not to outshoot the enemy, but to instead focus more on melee tactics."
  }

escape = ModeKind  -- asymmetric ranged and stealth race at night
  { msymbol = 'e'
  , mname   = "night escape (5)"
  , mfreq   = [(NIGHT, 1), (ESCAPE, 1), (CAMPAIGN_SCENARIO, 1)]
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mendMsg = []
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Three heroes vs. Seven human enemies capable of concurrent attacks"
      , "* Minimize losses"
      , "* Gather gems"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "Dwelling into dark matters is dangerous, so avoid the crowd of firebrand disputants, catch any gems of thought, find a way out and bring back a larger team to shed new light on the field."
  , mreason = "The focus of this installment is on stealthy exploration under the threat of numerically superior enemy."
  , mhint   = ""
  }

zoo = ModeKind  -- asymmetric crowd melee at night
  { msymbol = 'b'
  , mname   = "burning zoo (6)"
  , mfreq   = [(BURNING, 1), (ZOO, 1), (CAMPAIGN_SCENARIO, 1)]
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mendMsg = []
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Five heroes vs. Many enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "The heat of the dispute reaches the nearby Wonders of Science and Nature exhibition, igniting greenery, nets and cages. Crazed animals must be dissuaded from ruining precious scientific equipment and setting back the otherwise fruitful exchange of ideas."
  , mreason = "This is a crowd control exercise, at night, with a raging fire."
  , mhint   = ""
  }

-- The tactic is to sneak in the dark, highlight enemy with thrown torches
-- (and douse thrown enemy torches with blankets) and only if this fails,
-- actually scout using extended noctovision.
-- With reaction fire, larger team is more fun.
--
-- For now, while we have no shooters with timeout, massive ranged battles
-- without reaction fire don't make sense, because then usually only one hero
-- shoots (and often also scouts) and others just gather ammo.
ambush = ModeKind  -- dense ranged with reaction fire vs melee at night
  { msymbol = 'm'
  , mname   = "ranged ambush (7)"
  , mfreq   = [(RANGED, 1), (AMBUSH, 1), (CAMPAIGN_SCENARIO, 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mendMsg = []
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Three heroes with concurrent attacks vs. Unidentified foes"
      , "* Minimize losses"
      , "* Assert control of the situation ASAP"
      ]
  , mdesc   = "Prevent hijacking of your ideas at all cost! Be stealthy, be observant, be aggressive. Fast execution is what makes or breaks a creative team."
  , mreason = "In this scenario, finally, your heroes are able to all use ranged attacks at once, given enough ammunition."
  , mhint   = ""
  }

safari = ModeKind  -- Easter egg available only via screensaver
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [(SAFARI, 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mendMsg = []
  , mrules  = T.intercalate "\n" $
      [ "* Three levels"
      , "* Many teammates capable of concurrent action vs. Many enemies"
      , "* Minimize losses"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "\"In this enactment you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the bottommost level.\" This is a drama script recovered from a monster nest debris."
  , mreason = "This is an Easter egg. The default squad doctrine is that all team members follow the pointman, but it can be changed from the settings submenu of the main menu."
  , mhint   = ""
  }

-- * Testing modes

dig = ModeKind
  { msymbol = 'd'
  , mname   = "dig"
  , mfreq   = [(DIG, 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesDig
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Delve deeper!"
  , mreason = ""
  , mhint   = ""
  }

see = ModeKind
  { msymbol = 'a'
  , mname   = "see"
  , mfreq   = [(SEE, 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesSee
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "See all!"
  , mreason = ""
  , mhint   = ""
  }

short = ModeKind
  { msymbol = 's'
  , mname   = "short"
  , mfreq   = [(SHORT, 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesShort
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "See all short scenarios!"
  , mreason = ""
  , mhint   = ""
  }

crawlEmpty = ModeKind
  { msymbol = 'c'
  , mname   = "crawl empty"
  , mfreq   = [(CRAWL_EMPTY, 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Enjoy the extra legroom."
  , mreason = ""
  , mhint   = ""
  }

crawlSurvival = ModeKind
  { msymbol = 'd'
  , mname   = "crawl survival"
  , mfreq   = [(CRAWL_SURVIVAL, 1)]
  , mroster = rosterCrawlSurvival
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Lure the human intruders deeper and deeper."
  , mreason = ""
  , mhint   = ""
  }

safariSurvival = ModeKind
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [(SAFARI_SURVIVAL, 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "In this enactment you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  , mreason = ""
  , mhint   = ""
  }

battle = ModeKind
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [(BATTLE, 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked against those that unleash the horrors of abstraction."
  , mreason = ""
  , mhint   = ""
  }

battleDefense = ModeKind
  { msymbol = 'f'
  , mname   = "battle defense"
  , mfreq   = [(BATTLE_DEFENSE, 1)]
  , mroster = rosterBattleDefense
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  , mreason = ""
  , mhint   = ""
  }

battleSurvival = ModeKind
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [(BATTLE_SURVIVAL, 1)]
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked for those that ally with the strongest."
  , mreason = ""
  , mhint   = ""
  }

defense = ModeKind  -- perhaps a real scenario in the future
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [(DEFENSE, 1)]
  , mroster = rosterDefense
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Don't let human interlopers defile your abstract secrets and flee unpunished!"
  , mreason = "This is an initial sketch of the reversed crawl game mode. Play on high difficulty to avoid guaranteed victories against the pitiful humans."
  , mhint   = ""
  }

defenseEmpty = ModeKind
  { msymbol = 'e'
  , mname   = "defense empty"
  , mfreq   = [(DEFENSE_EMPTY, 1)]
  , mroster = rosterDefenseEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Lord over empty halls."
  , mreason = ""
  , mhint   = ""
  }

-- * Screensaver modes

screensave :: AutoLeader -> ModeKind -> ModeKind
screensave auto mk =
  let f [] = []
      f ((player, initial) : rest) =
        (player {fleaderMode = LeaderAI auto}, initial) : rest
  in mk { mroster = (mroster mk) {rosterList = f $ rosterList $ mroster mk}
        , mreason = "This is one of the screensaver scenarios, not available from the main menu, with all factions controlled by AI. Feel free to take over or relinquish control at any moment, but to register a legitimate high score, choose a standard scenario instead.\n" <> mreason mk
        }

screensaverRaid = screensave (AutoLeader False False) $ raid
  { mname   = "auto-raid (1)"
  , mfreq   = [(INSERT_COIN, 2), (NO_CONFIRMS, 1)]
  }

screensaverBrawl = screensave (AutoLeader False False) $ brawl
  { mname   = "auto-brawl (2)"
  , mfreq   = [(NO_CONFIRMS, 1)]
  }

screensaverCrawl = screensave (AutoLeader False False) $ crawl
  { mname   = "auto-crawl (long)"
  , mfreq   = [(NO_CONFIRMS, 1)]
  }

screensaverShootout = screensave (AutoLeader False False) $ shootout
  { mname   = "auto-shootout (3)"
  , mfreq   = [(INSERT_COIN, 2), (NO_CONFIRMS, 1)]
  }

screensaverHunt = screensave (AutoLeader False False) $ hunt
  { mname   = "auto-hunt (4)"
  , mfreq   = [(INSERT_COIN, 2), (NO_CONFIRMS, 1)]
  }

screensaverEscape = screensave (AutoLeader False False) $ escape
  { mname   = "auto-escape (5)"
  , mfreq   = [(INSERT_COIN, 2), (NO_CONFIRMS, 1)]
  }

screensaverZoo = screensave (AutoLeader False False) $ zoo
  { mname   = "auto-zoo (6)"
  , mfreq   = [(NO_CONFIRMS, 1)]
  }

screensaverAmbush = screensave (AutoLeader False False) $ ambush
  { mname   = "auto-ambush (7)"
  , mfreq   = [(NO_CONFIRMS, 1)]
  }

-- changing leader by client needed, because of TFollow
screensaverSafari = screensave (AutoLeader False True) $ safari
  { mname   = "auto-safari"
  , mfreq   = [(INSERT_COIN, 1), (NO_CONFIRMS, 1)]
  }

rosterRaid, rosterBrawl, rosterCrawl, rosterShootout, rosterHunt, rosterEscape, rosterZoo, rosterAmbush, rosterSafari, rosterCrawlEmpty, rosterCrawlSurvival, rosterSafariSurvival, rosterBattle, rosterBattleDefense, rosterBattleSurvival, rosterDefense, rosterDefenseEmpty :: Roster

rosterRaid = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiHeroShort}
                   , [(-2, 2, HERO)] )
                 , ( playerAntiHero { fname = "Indigo Founder"
                                    , fhiCondPoly = hiHeroShort }
                   , [(-2, 1, HERO)] )
                 , ( playerAnimal  -- starting over escape
                   , [(-2, 2, ANIMAL)] )
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = [ ("Explorer", "Animal Kingdom")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Founder", "Animal Kingdom")
                  , ("Indigo Founder", "Horror Den") ]
  , rosterAlly = [] }

rosterBrawl = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(-2, 3, BRAWLER_HERO)] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-2, 3, BRAWLER_HERO)] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterCrawl = Roster
  { rosterList = [ ( playerHero
                   , [(-1, 3, HERO)] )
                 , ( playerMonster
                   , [(-4, 1, SCOUT_MONSTER), (-4, 3, MONSTER)] )
                 , ( playerAnimal
                   , -- Fun from the start to avoid empty initial level:
                     [ (-1, 1 + 1 `d` 2, ANIMAL)
                     -- Huge battle at the end:
                     , (-10, 100, MOBILE_ANIMAL) ] ) ]
  , rosterEnemy = [ ("Explorer", "Monster Hive")
                  , ("Explorer", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

-- Exactly one scout gets a sight boost, to help the aggressor, because he uses
-- the scout for initial attack, while camper (on big enough maps)
-- can't guess where the attack would come and so can't position his single
-- scout to counter the stealthy advance.
rosterShootout = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(-5, 1, SCOUT_HERO), (-5, 2, RANGER_HERO)] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-5, 1, SCOUT_HERO), (-5, 2, RANGER_HERO)] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterHunt = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(-6, 7, SOLDIER_HERO)] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-6, 1, SCOUT_HERO), (-6, 6, AMBUSHER_HERO)] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterEscape = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiHeroMedium}
                   , [(-7, 1, SCOUT_HERO), (-7, 2, ESCAPIST_HERO)] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False  -- start on escape
                                    , fneverEmpty = False  -- loot after killing
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-7, 1, SCOUT_HERO), (-7, 6, AMBUSHER_HERO)] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterZoo = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong }
                   , [(-8, 5, SOLDIER_HERO)] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(-8, 100, MOBILE_ANIMAL)] )
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = [ ("Explorer", "Animal Kingdom")
                  , ("Explorer", "Horror Den") ]
  , rosterAlly = [] }

rosterAmbush = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(-9, 1, SCOUT_HERO), (-9, 5, AMBUSHER_HERO)] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-9, 12, SOLDIER_HERO)] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

-- No horrors faction needed, because spawned heroes land in civilian faction.
rosterSafari = Roster
  { rosterList = [ ( playerMonsterTourist
                   , [(-4, 15, MONSTER)] )
                 , ( playerHunamConvict
                   , [(-4, 2, CIVILIAN)] )
                 , ( playerAnimalMagnificent
                   , [(-7, 15, MOBILE_ANIMAL)] )
                 , ( playerAnimalExquisite  -- start on escape
                   , [(-10, 20, MOBILE_ANIMAL)] ) ]
  , rosterEnemy = [ ("Monster Tourist Office", "Hunam Convict")
                  , ( "Monster Tourist Office"
                    , "Animal Magnificent Specimen Variety" )
                  , ( "Monster Tourist Office"
                    , "Animal Exquisite Herds and Packs Galore" )
                  , ( "Animal Magnificent Specimen Variety"
                    , "Hunam Convict" )
                  , ( "Hunam Convict"
                    , "Animal Exquisite Herds and Packs Galore" ) ]
  , rosterAlly = [ ( "Animal Magnificent Specimen Variety"
                   , "Animal Exquisite Herds and Packs Galore" ) ] }

rosterCrawlEmpty = Roster
  { rosterList = [ ( playerHero
                   , [(-1, 1, HERO)] )
                 , (playerHorror, []) ]  -- for spawned and summoned monsters
  , rosterEnemy = []
  , rosterAlly = [] }

rosterCrawlSurvival = rosterCrawl
  { rosterList = [ ( playerAntiHero
                   , [(-1, 3, HERO)] )
                 , ( playerMonster
                   , [(-4, 1, SCOUT_MONSTER), (-4, 3, MONSTER)] )
                 , ( playerAnimal {fhasUI = True}
                   , -- Fun from the start to avoid empty initial level:
                     [ (-1, 1 + 1 `d` 2, ANIMAL)
                     -- Huge battle at the end:
                     , (-10, 100, MOBILE_ANIMAL) ] ) ] }

rosterSafariSurvival = rosterSafari
  { rosterList = [ ( playerMonsterTourist
                       { fleaderMode = LeaderAI $ AutoLeader True True
                       , fhasUI = False }
                   , [(-4, 15, MONSTER)] )
                 , ( playerHunamConvict
                   , [(-4, 3, CIVILIAN)] )
                 , ( playerAnimalMagnificent
                       { fleaderMode = LeaderUI $ AutoLeader True False
                       , fhasUI = True }
                   , [(-7, 20, MOBILE_ANIMAL)] )
                 , ( playerAnimalExquisite
                   , [(-10, 30, MOBILE_ANIMAL)] ) ] }

rosterBattle = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong }
                   , [(-5, 5, SOLDIER_HERO)] )
                 , ( playerMonster {fneverEmpty = True}
                   , [(-5, 35, MOBILE_MONSTER)] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(-5, 30, MOBILE_ANIMAL)] ) ]
  , rosterEnemy = [ ("Explorer", "Monster Hive")
                  , ("Explorer", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterBattleDefense = rosterBattle
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong
                                , fleaderMode =
                                    LeaderAI $ AutoLeader False False
                                , fhasUI = False }
                   , [(-5, 5, SOLDIER_HERO)] )
                 , ( playerMonster { fneverEmpty = True
                                   , fhasUI = True }
                   , [(-5, 35, MOBILE_MONSTER)] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(-5, 30, MOBILE_ANIMAL)] ) ] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong
                                , fleaderMode =
                                    LeaderAI $ AutoLeader False False
                                , fhasUI = False }
                   , [(-5, 5, SOLDIER_HERO)] )
                 , ( playerMonster {fneverEmpty = True}
                   , [(-5, 35, MOBILE_MONSTER)] )
                 , ( playerAnimal { fneverEmpty = True
                                  , fhasUI = True }
                   , [(-5, 30, MOBILE_ANIMAL)] ) ] }

rosterDefense = rosterCrawl
  { rosterList = [ ( playerAntiHero
                   , [(-1, 3, HERO)] )
                 , ( playerAntiMonster
                   , [(-4, 1, SCOUT_MONSTER), (-4, 3, MONSTER)] )
                 , ( playerAnimal
                   , [ (-1, 1 + 1 `d` 2, ANIMAL)
                     , (-10, 100, MOBILE_ANIMAL) ] ) ] }

rosterDefenseEmpty = rosterCrawl
  { rosterList = [ ( playerAntiMonster {fneverEmpty = True}
                   , [(-4, 1, SCOUT_MONSTER)] )
                 , (playerHorror, []) ]  -- for spawned and summoned animals
  , rosterEnemy = []
  , rosterAlly = [] }

cavesRaid, cavesBrawl, cavesCrawl, cavesShootout, cavesHunt, cavesEscape, cavesZoo, cavesAmbush, cavesSafari, cavesDig, cavesSee, cavesShort, cavesCrawlEmpty, cavesBattle :: Caves

cavesRaid = [([-2], [CAVE_RAID])]

cavesBrawl = [([-2], [CAVE_BRAWL])]

listCrawl :: [([Int], [GroupName CaveKind])]
listCrawl =
  [ ([-1], [CAVE_OUTERMOST])
  , ([-2], [CAVE_SHALLOW_ROGUE])
  , ([-3], [CAVE_EMPTY])
  , ([-4, -5, -6], [DEFAULT_RANDOM, CAVE_ROGUE, CAVE_ARENA])
  , ([-7, -8], [CAVE_ROGUE, CAVE_SMOKING])
  , ([-9], [CAVE_LABORATORY])
  , ([-10], [CAVE_MINE]) ]

cavesCrawl = listCrawl

cavesShootout = [([-5], [CAVE_SHOOTOUT])]

cavesHunt = [([-6], [CAVE_HUNT])]

cavesEscape = [([-7], [CAVE_ESCAPE])]

cavesZoo = [([-8], [CAVE_ZOO])]

cavesAmbush = [([-9], [CAVE_AMBUSH])]

cavesSafari = [ ([-4], [CAVE_SAFARI_1])
              , ([-7], [CAVE_SAFARI_2])
              , ([-10], [CAVE_SAFARI_3]) ]

cavesDig = concat $ zipWith (map . renumberCaves)
                            [0, -10 ..]
                            (replicate 100 listCrawl)

renumberCaves :: Int -> ([Int], [GroupName CaveKind])
              -> ([Int], [GroupName CaveKind])
renumberCaves offset (ns, l) = (map (+ offset) ns, l)

cavesSee = let numberCaves n c = ([n], [c])
           in zipWith numberCaves [-1, -2 ..]
              $ concatMap (replicate 10) allCaves

cavesShort = let numberCaves n c = ([n], [c])
             in zipWith numberCaves [-1, -2 ..]
                $ concatMap (replicate 100) $ take 7 allCaves

allCaves :: [GroupName CaveKind]
allCaves =
  [ CAVE_RAID, CAVE_BRAWL, CAVE_SHOOTOUT, CAVE_HUNT, CAVE_ESCAPE, CAVE_ZOO
  , CAVE_AMBUSH
  , CAVE_ROGUE, CAVE_LABORATORY, CAVE_EMPTY, CAVE_ARENA, CAVE_SMOKING
  , CAVE_NOISE, CAVE_MINE ]

cavesCrawlEmpty = cavesCrawl

cavesBattle = [([-5], [CAVE_BATTLE])]
