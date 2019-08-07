-- | Game mode definitions.
module Content.ModeKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ModeKindPlayer
import Game.LambdaHack.Content.CaveKind (CaveKind)
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Defs

content :: [ModeKind]
content =
  [raid, brawl, crawl, shootout, hunt, escape, zoo, ambush, crawlEmpty, crawlSurvival, dig, see, safari, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverCrawl, screensaverSafari]

raid,    brawl, crawl, shootout, hunt, escape, zoo, ambush, crawlEmpty, crawlSurvival, dig, see, safari, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverCrawl, screensaverSafari :: ModeKind

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
  , mfreq   = [("raid", 1), ("campaign scenario", 1)]
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mendMsg = [ (Killed, "This expedition has gone wrong. However, scientific mind does not despair, but analyzes and corrects. Did you perchance awake one animal too many? Did you remember to try using all consumables at your disposal for your immediate survival? Did you choose a challenge with difficulty level within your means? Answer honestly, ponder wisely, experiment methodically.")
              , (Defeated, "Regrettably, the other team snatched the grant, while you were busy contemplating natural phenomena. Science is a competitive sport, as sad as it sounds. It's not enough to make a discovery, you have to get there first.")
              , (Escape, "You've got hold of the machine! Think of the hours of fun taking it apart and putting it back together again! That's a great first step on your quest to solve the typing problems of the world.") ]
  , mdesc   = "An incredibly advanced typing machine worth 100 gold is buried at the exit of this maze. Be the first to find it and fund a research team that makes typing accurate and dependable forever.\n[In addition to initiating the (rather loose) game plot, this scenario serves as an introductory tutorial. There is only one level. Relax, explore, gather loot, find the exit and escape. With some luck, you won't even need to fight anything or use any items. Feel free to scout with only one of the heroes and keep the other one immobile, e.g., standing guard over the squad's shared inventory stash. If in grave danger, retreat with the scout to join forces with the guard. The more gold collected and the faster the victory, the higher the score.]"
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { msymbol = 'k'
  , mname   = "brawl (tutorial, 2)"
  , mfreq   = [("brawl", 1), ("campaign scenario", 1)]
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mendMsg = [ (Killed, "The inquisitive scholars turned out to be envious of our deep insight to the point of outright violence. It would still not result in such a defeat and recanting of our thesis if we figured out to use terrain to protect us from missiles or even completely hide our presence. It would also help if we honourably kept our ground together to the end, at the same time preventing the overwhelming enemy forces from brutishly ganging up on our modest-sized, though valiant, research team.")
              , (Conquer, "That's settled: local compactness *is* necessary for relative completeness, given the assumptions.") ]
  , mdesc   = "Your research team disagrees over a drink with some gentlemen scientists about premises of a relative completeness theorem and there's only one way to settle that. Remember to keep your party together when opponents are spotted, or they might be tempted to silence solitary disputants one by one and so win the altercation.\n[In addition to advancing the game plot, this scenario trains melee, squad formation and stealth. If you feel helpless, ponder the hints from the defeat message. The battle is completely symmetric, both in numbers and the teams' squad capabilities (e.g., only pointman may move, while all others either melee or wait). Observe, mimic and savour the fairness, because you won't find any in the main crawl scenario that follows.]"
  }

crawl = ModeKind
  { msymbol = 'c'
  , mname   = "long crawl (main)"
  , mfreq   = [("long", 1), ("crawl", 1), ("campaign scenario", 1)]
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mendMsg = [ (Killed, "To think that followers of science and agents of enlightenment would earn death as their reward! Where did we err in our ways? Perhaps nature should not have been disturbed so brashly and the fell beasts woken up from their slumber so eagerly? Perhaps the gathered items should have been used for scientific experiments on the spot rather than hoarded as if of base covetousness? Or perhaps the challenge, chosen freely but without the foreknowledge of the grisly difficulty, was insurmountable and forlorn from the start, despite the enormous power of educated reason at out disposal?")
              , (Escape, "It's better to live to tell the tale than to choke on more than one can swallow. There was no more exquisite cultural artifacts and glorious scientific wonders in these forbidding tunnels anyway. Or were there?") ]
  , mdesc   = "Enjoy the peaceful seclusion of these cold austere tunnels, but don't let wanton curiosity, greed and the ever-creeping abstraction madness keep you down there for too long. If you find survivors (whole or perturbed or segmented) of the past scientific missions, exercise extreme caution and engage or ignore at your discretion.\n[This is the main, longest and most replayable scenario of the game. If you keep dying, attempt the next game modes as a breather (perhaps at lowered difficulty). They fill the gaps in the plot and teach particular skills that may come in handy and help you discover new tactics of your own or come up with a strategy for staving off the attrition.]"
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
  , mfreq   = [("foggy", 1), ("shootout", 1), ("campaign scenario", 1)]
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mendMsg = []
  , mdesc   = "Whose arguments are most striking and whose ideas fly fastest? Let's scatter up, attack the problems from different angles and find out.\n[This scenario is a flashback, picking the plot up where brawl (2) left it. It also teaches specifically the ranged combat skill in the simplified setup of fully symmetric battle. Try to come up with the best squad formation for this tactical challenge. Don't despair if you run out of ammo, because if you aim truly, enemy has few hit points left at this point. In turn, when trying to avoid enemy projectiles, you can display the trajectory of any soaring entity by pointing it with the crosshair in aiming mode.]"
  }

hunt = ModeKind  -- melee vs ranged with reaction fire in daylight
  { msymbol = 'h'
  , mname   = "perilous hunt (4)"
  , mfreq   = [("perilous", 1), ("hunt", 1), ("campaign scenario", 1)]
  , mroster = rosterHunt
  , mcaves  = cavesHunt
  , mendMsg = []
  , mdesc   = "Who is the hunter and who is the prey?\n[This is yet another reminiscence of the events that led to the long crawl adventure. This episode is quite a tactical challenge, because enemies are allowed to fling their ammo simultaneously at you team, which has no such capacities and focuses on melee combat instead. Act accordingly.]"
  }

escape = ModeKind  -- asymmetric ranged and stealth race at night
  { msymbol = 'e'
  , mname   = "night escape (5)"
  , mfreq   = [("night", 1), ("escape", 1), ("campaign scenario", 1)]
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mendMsg = []
  , mdesc   = "Dwelling into dark matters is dangerous, so avoid the crowd of firebrand disputants, catch any gems of thought, find a way out and bring back a larger team to shed new light on the field."
  }

zoo = ModeKind  -- asymmetric crowd melee at night
  { msymbol = 'b'
  , mname   = "burning zoo (6)"
  , mfreq   = [("burning", 1), ("zoo", 1), ("campaign scenario", 1)]
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mendMsg = []
  , mdesc   = "The heat of the dispute reaches the nearby Wonders of Science and Nature exhibition, igniting greenery, nets and cages. Crazed animals must be dissuaded from ruining precious scientific equipment and setting back the otherwise fruitful exchange of ideas."
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
  , mfreq   = [("ranged", 1), ("ambush", 1), ("campaign scenario", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mendMsg = []
  , mdesc   = "Prevent hijacking of your ideas at all cost! Be stealthy, be observant, be aggressive. Fast execution is what makes or breaks a creative team."
  }

safari = ModeKind  -- Easter egg available only via screensaver
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mendMsg = []
  , mdesc   = "\"In this enactment you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the bottommost level.\" This is a drama script recovered from a monster nest debris."
  }

-- * Testing modes

dig = ModeKind
  { msymbol = 'd'
  , mname   = "dig"
  , mfreq   = [("dig", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesDig
  , mendMsg = []
  , mdesc   = "Delve deeper!"
  }

see = ModeKind
  { msymbol = 'a'
  , mname   = "see"
  , mfreq   = [("see", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesSee
  , mendMsg = []
  , mdesc   = "See all!"
  }

crawlEmpty = ModeKind
  { msymbol = 'c'
  , mname   = "crawl empty"
  , mfreq   = [("crawl empty", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mdesc   = "Enjoy the free space."
  }

crawlSurvival = ModeKind
  { msymbol = 'd'
  , mname   = "crawl survival"
  , mfreq   = [("crawl survival", 1)]
  , mroster = rosterCrawlSurvival
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mdesc   = "Lure the human intruders deeper and deeper."
  }

safariSurvival = ModeKind
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mendMsg = []
  , mdesc   = "In this enactment you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

battle = ModeKind
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [("battle", 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mendMsg = []
  , mdesc   = "Odds are stacked against those that unleash the horrors of abstraction."
  }

battleDefense = ModeKind
  { msymbol = 'f'
  , mname   = "battle defense"
  , mfreq   = [("battle defense", 1)]
  , mroster = rosterBattleDefense
  , mcaves  = cavesBattle
  , mendMsg = []
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  }

battleSurvival = ModeKind
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [("battle survival", 1)]
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mendMsg = []
  , mdesc   = "Odds are stacked for those that ally with the strongest."
  }

defense = ModeKind  -- perhaps a real scenario in the future
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mdesc   = "Don't let human interlopers defile your abstract secrets and flee unpunished!"
  }

defenseEmpty = ModeKind
  { msymbol = 'e'
  , mname   = "defense empty"
  , mfreq   = [("defense empty", 1)]
  , mroster = rosterDefenseEmpty
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mdesc   = "Lord over."
  }

-- * Screensaver modes

screensave :: AutoLeader -> Roster -> Roster
screensave auto r =
  let f [] = []
      f ((player, initial) : rest) =
        (player {fleaderMode = LeaderAI auto}, initial) : rest
  in r {rosterList = f $ rosterList r}

screensaverRaid = raid
  { mname   = "auto-raid (1)"
  , mfreq   = [("insert coin", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterRaid
  }

screensaverBrawl = brawl
  { mname   = "auto-brawl (2)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterBrawl
  }

screensaverShootout = shootout
  { mname   = "auto-shootout (3)"
  , mfreq   = [("insert coin", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterShootout
  }

screensaverHunt = hunt
  { mname   = "auto-hunt (4)"
  , mfreq   = [("insert coin", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterHunt
  }

screensaverEscape = escape
  { mname   = "auto-escape (5)"
  , mfreq   = [("insert coin", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterEscape
  }

screensaverZoo = zoo
  { mname   = "auto-zoo (6)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterZoo
  }

screensaverAmbush = ambush
  { mname   = "auto-ambush (7)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterAmbush
  }

screensaverCrawl = crawl
  { mname   = "auto-crawl (long)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterCrawl
  }

screensaverSafari = safari
  { mname   = "auto-safari"
  , mfreq   = [("insert coin", 1), ("no confirms", 1)]
  , mroster = -- changing leader by client needed, because of TFollow
              screensave (AutoLeader False True) rosterSafari
  }

rosterRaid, rosterBrawl, rosterShootout, rosterHunt, rosterEscape, rosterZoo, rosterAmbush, rosterCrawl, rosterCrawlEmpty, rosterCrawlSurvival, rosterSafari, rosterSafariSurvival, rosterBattle, rosterBattleDefense, rosterBattleSurvival, rosterDefense, rosterDefenseEmpty :: Roster

rosterRaid = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiHeroShort}
                   , [(-2, 2, "hero")] )
                 , ( playerAntiHero { fname = "Indigo Founder"
                                    , fhiCondPoly = hiHeroShort }
                   , [(-2, 1, "hero")] )
                 , ( playerAnimal  -- starting over escape
                   , [(-2, 2, "animal")] )
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = [ ("Explorer", "Animal Kingdom")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Founder", "Animal Kingdom")
                  , ("Indigo Founder", "Horror Den") ]
  , rosterAlly = [] }

rosterBrawl = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(-2, 3, "brawler hero")] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-2, 3, "brawler hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

-- Exactly one scout gets a sight boost, to help the aggressor, because he uses
-- the scout for initial attack, while camper (on big enough maps)
-- can't guess where the attack would come and so can't position his single
-- scout to counter the stealthy advance.
rosterShootout = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(-5, 1, "scout hero"), (-5, 2, "ranger hero")] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-5, 1, "scout hero"), (-5, 2, "ranger hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterHunt = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(-6, 7, "soldier hero")] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-6, 1, "scout hero"), (-6, 6, "ambusher hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterEscape = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiHeroMedium}
                   , [(-7, 1, "scout hero"), (-7, 2, "escapist hero")] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False  -- start on escape
                                    , fneverEmpty = False  -- loot after killing
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-7, 1, "scout hero"), (-7, 6, "ambusher hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterZoo = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong }
                   , [(-8, 5, "soldier hero")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(-8, 100, "mobile animal")] )
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = [ ("Explorer", "Animal Kingdom")
                  , ("Explorer", "Horror Den") ]
  , rosterAlly = [] }

rosterAmbush = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(-9, 1, "scout hero"), (-9, 5, "ambusher hero")] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(-9, 12, "soldier hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterCrawl = Roster
  { rosterList = [ ( playerHero
                   , [(-1, 3, "hero")] )
                 , ( playerMonster
                   , [(-4, 1, "scout monster"), (-4, 3, "monster")] )
                 , ( playerAnimal
                   , -- Fun from the start to avoid empty initial level:
                     [ (-1, 1 + 1 `d` 2, "animal")
                     -- Huge battle at the end:
                     , (-10, 100, "mobile animal") ] ) ]
  , rosterEnemy = [ ("Explorer", "Monster Hive")
                  , ("Explorer", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterCrawlEmpty = Roster
  { rosterList = [ ( playerHero
                   , [(-1, 1, "hero")] )
                 , (playerHorror, []) ]  -- for spawned and summoned monsters
  , rosterEnemy = []
  , rosterAlly = [] }

rosterCrawlSurvival = rosterCrawl
  { rosterList = [ ( playerAntiHero
                   , [(-1, 3, "hero")] )
                 , ( playerMonster
                   , [(-4, 1, "scout monster"), (-4, 3, "monster")] )
                 , ( playerAnimal {fhasUI = True}
                   , -- Fun from the start to avoid empty initial level:
                     [ (-1, 1 + 1 `d` 2, "animal")
                     -- Huge battle at the end:
                     , (-10, 100, "mobile animal") ] ) ] }

-- No horrors faction needed, because spawned heroes land in civilian faction.
rosterSafari = Roster
  { rosterList = [ ( playerMonsterTourist
                   , [(-4, 15, "monster")] )
                 , ( playerHunamConvict
                   , [(-4, 2, "civilian")] )
                 , ( playerAnimalMagnificent
                   , [(-7, 15, "mobile animal")] )
                 , ( playerAnimalExquisite  -- start on escape
                   , [(-10, 20, "mobile animal")] ) ]
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

rosterSafariSurvival = rosterSafari
  { rosterList = [ ( playerMonsterTourist
                       { fleaderMode = LeaderAI $ AutoLeader True True
                       , fhasUI = False }
                   , [(-4, 15, "monster")] )
                 , ( playerHunamConvict
                   , [(-4, 3, "civilian")] )
                 , ( playerAnimalMagnificent
                       { fleaderMode = LeaderUI $ AutoLeader True False
                       , fhasUI = True }
                   , [(-7, 20, "mobile animal")] )
                 , ( playerAnimalExquisite
                   , [(-10, 30, "mobile animal")] ) ] }

rosterBattle = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong }
                   , [(-5, 5, "soldier hero")] )
                 , ( playerMonster {fneverEmpty = True}
                   , [(-5, 35, "mobile monster")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(-5, 30, "mobile animal")] ) ]
  , rosterEnemy = [ ("Explorer", "Monster Hive")
                  , ("Explorer", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterBattleDefense = rosterBattle
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong
                                , fleaderMode =
                                    LeaderAI $ AutoLeader False False
                                , fhasUI = False }
                   , [(-5, 5, "soldier hero")] )
                 , ( playerMonster { fneverEmpty = True
                                   , fhasUI = True }
                   , [(-5, 35, "mobile monster")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(-5, 30, "mobile animal")] ) ] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong
                                , fleaderMode =
                                    LeaderAI $ AutoLeader False False
                                , fhasUI = False }
                   , [(-5, 5, "soldier hero")] )
                 , ( playerMonster {fneverEmpty = True}
                   , [(-5, 35, "mobile monster")] )
                 , ( playerAnimal { fneverEmpty = True
                                  , fhasUI = True }
                   , [(-5, 30, "mobile animal")] ) ] }

rosterDefense = rosterCrawl
  { rosterList = [ ( playerAntiHero
                   , [(-1, 3, "hero")] )
                 , ( playerAntiMonster
                   , [(-4, 1, "scout monster"), (-4, 3, "monster")] )
                 , ( playerAnimal
                   , [ (-1, 1 + 1 `d` 2, "animal")
                     , (-10, 100, "mobile animal") ] ) ] }

rosterDefenseEmpty = rosterCrawl
  { rosterList = [ ( playerAntiMonster {fneverEmpty = True}
                   , [(-4, 1, "scout monster")] )
                 , (playerHorror, []) ]  -- for spawned and summoned animals
  , rosterEnemy = []
  , rosterAlly = [] }

cavesRaid, cavesBrawl, cavesShootout, cavesHunt, cavesEscape, cavesZoo, cavesAmbush, cavesCrawl, cavesDig, cavesSee, cavesSafari, cavesBattle :: Caves

cavesRaid = [([-2], ["caveRaid"])]

cavesBrawl = [([-2], ["caveBrawl"])]

cavesShootout = [([-5], ["caveShootout"])]

cavesHunt = [([-6], ["caveHunt"])]

cavesEscape = [([-7], ["caveEscape"])]

cavesZoo = [([-8], ["caveZoo"])]

cavesAmbush = [([-9], ["caveAmbush"])]

listCrawl :: [([Int], [GroupName CaveKind])]
listCrawl =
  [ ([-1], ["caveOutermost"])
  , ([-2], ["caveShallowRogue"])
  , ([-3], ["caveEmpty"])
  , ([-4, -5, -6], ["default random", "caveRogue", "caveArena"])
  , ([-7, -8], ["caveRogue", "caveSmoking"])
  , ([-9], ["caveLaboratory"])
  , ([-10], ["caveMine"]) ]

cavesCrawl = listCrawl

renumberCaves :: Int -> ([Int], [GroupName CaveKind])
              -> ([Int], [GroupName CaveKind])
renumberCaves offset (ns, l) = (map (+ offset) ns, l)

cavesDig = concat $ zipWith (map . renumberCaves)
                            [0, -10 ..]
                            (replicate 100 listCrawl)

cavesSee = let numberCaves n c = ([n], [c])
           in zipWith numberCaves [-1, -2 ..]
              $ concatMap (replicate 8) allCaves

allCaves :: [GroupName CaveKind]
allCaves =
  [ "caveRaid", "caveBrawl", "caveShootout", "caveHunt", "caveEscape", "caveZoo"
  , "caveAmbush"
  , "caveRogue", "caveLaboratory", "caveEmpty", "caveArena", "caveSmoking"
  , "caveNoise", "caveMine" ]

cavesSafari = [ ([-4], ["caveSafari1"])
              , ([-7], ["caveSafari2"])
              , ([-10], ["caveSafari3"]) ]

cavesBattle = [([-5], ["caveBattle"])]
