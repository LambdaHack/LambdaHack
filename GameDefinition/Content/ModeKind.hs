-- | Game mode definitions.
module Content.ModeKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.IntMap.Strict as IM

import Content.ModeKindPlayer
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.CaveKind (CaveKind)
import Game.LambdaHack.Content.ModeKind

content :: [ModeKind]
content =
  [raid, brawl, shootout, escape, zoo, ambush, crawl, crawlEmpty, crawlSurvival, dig, see, safari, safariSurvival, battle, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverCrawl, screensaverSafari]

raid,    brawl, shootout, escape, zoo, ambush, crawl, crawlEmpty, crawlSurvival, dig, see, safari, safariSurvival, battle, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverCrawl, screensaverSafari :: ModeKind

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
-- crowd ranged: no, fish in a barel, less predictable and more fun inside
--   crawl, even without reaction fire

raid = ModeKind  -- mini-crawl
  { msymbol = 'r'
  , mname   = "raid (1)"
  , mfreq   = [("raid", 1), ("campaign scenario", 1)]
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mdesc   = "An incredibly advanced typing machine worth 100 gold is buried at the exit of this maze. Be the first to find it and fund a research team that makes typing accurate and dependable forever."
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { msymbol = 'k'
  , mname   = "brawl (2)"
  , mfreq   = [("brawl", 1), ("campaign scenario", 1)]
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mdesc   = "Your engineering team disagrees over a drink with some gentlemen scientists about premises of a relative completeness theorem and there's only one way to settle that. Remember to keep your party together, or the opposing team might be tempted to gang upon a solitary disputant."
  }

-- The trajectory tip is important because of tactics of scout looking from
-- behind a bush and others hiding in mist. If no suitable bushes,
-- fire once and flee into mist or behind cover. Then whomever is out of LOS
-- range or inside mist can shoot at the last seen enemy locations,
-- adjusting and according to ounds and incoming missile trajectories.
-- If the scount can't find bushes or glass building to set a lookout,
-- the other team member are more spotters and guardians than snipers
-- and that's their only role, so a small party makes sense.
shootout = ModeKind  -- sparse ranged in daylight
  { msymbol = 's'
  , mname   = "shootout (3)"
  , mfreq   = [("shootout", 1), ("campaign scenario", 1)]
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mdesc   = "Whose arguments are most striking and whose ideas fly fastest? Let's scatter up, attack the problems from different angles and find out. (To display the trajectory of any soaring entity, point it with the crosshair in aiming mode.)"
  }

escape = ModeKind  -- asymmetric ranged and stealth race at night
  { msymbol = 'e'
  , mname   = "escape (4)"
  , mfreq   = [("escape", 1), ("campaign scenario", 1)]
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mdesc   = "Dwelling into dark matters is dangerous, so avoid the crowd of firebrand disputants, catch any gems of thought, find a way out and bring back a larger team to shed new light on the field."
  }

zoo = ModeKind  -- asymmetric crowd melee at night
  { msymbol = 'b'
  , mname   = "zoo (5)"
  , mfreq   = [("zoo", 1), ("campaign scenario", 1)]
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mdesc   = "The heat of the dispute reaches the nearby Wonders of Science and Nature exhibition, igniting greenery, nets and cages. Crazed animals must be prevented from ruining precious scientific equipment and setting back the otherwise fruitful exchange of ideas."
  }

-- The tactic is to sneak in the dark, highlight enemy with thrown torches
-- (and douse thrown enemy torches with blankets) and only if this fails,
-- actually scout using extended noctovision.
-- With reaction fire, larger team is more fun.
--
-- For now, while we have no shooters with timeout, massive ranged battles
-- without reaction fire don't make sense, because then usually only one hero
-- shoots (and often also scouts) and others just gather ammo.
ambush = ModeKind  -- dense ranged with reaction fire at night
  { msymbol = 'm'
  , mname   = "ambush (6)"
  , mfreq   = [("ambush", 1), ("campaign scenario", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mdesc   = "Prevent hijacking of your ideas at all cost! Be stealthy, be aggressive. Fast execution is what makes or breaks a creative team."
  }

crawl = ModeKind
  { msymbol = 'c'
  , mname   = "crawl (long)"
  , mfreq   = [("crawl", 1), ("campaign scenario", 1)]
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mdesc   = "Enjoy the peaceful seclusion of these cold austere tunnels, but don't let wanton curiosity, greed and the ever-creeping abstraction madness keep you down there for too long. If you find survivors (whole or perturbed or segmented) of the past scientific missions, exercise extreme caution and engage or ignore at your discretion."
  }

safari = ModeKind  -- easter egg available only via screensaver
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mdesc   = "\"In this enactment you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the bottommost level.\" This is a drama script recovered from a monster nest debris."
  }

-- * Testing modes

dig = ModeKind
  { msymbol = 'd'
  , mname   = "dig"
  , mfreq   = [("dig", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesDig
  , mdesc   = "Delve deeper!"
  }

see = ModeKind
  { msymbol = 'a'
  , mname   = "see"
  , mfreq   = [("see", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesSee
  , mdesc   = "See all!"
  }

crawlEmpty = ModeKind
  { msymbol = 'c'
  , mname   = "crawl empty"
  , mfreq   = [("crawl empty", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesCrawl
  , mdesc   = "Enjoy the free space."
  }

crawlSurvival = ModeKind
  { msymbol = 'd'
  , mname   = "crawl survival"
  , mfreq   = [("crawl survival", 1)]
  , mroster = rosterCrawlSurvival
  , mcaves  = cavesCrawl
  , mdesc   = "Lure the human intruders deeper and deeper."
  }

safariSurvival = ModeKind
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mdesc   = "In this enactment you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

battle = ModeKind
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [("battle", 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked against those that unleash the horrors of abstraction."
  }

battleSurvival = ModeKind
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [("battle survival", 1)]
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  }

defense = ModeKind  -- perhaps a real scenario in the future
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesCrawl
  , mdesc   = "Don't let human interlopers defile your abstract secrets and flee unpunished!"
  }

defenseEmpty = ModeKind
  { msymbol = 'e'
  , mname   = "defense empty"
  , mfreq   = [("defense empty", 1)]
  , mroster = rosterDefenseEmpty
  , mcaves  = cavesCrawl
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
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterRaid
  }

screensaverBrawl = brawl
  { mname   = "auto-brawl (2)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterBrawl
  }

screensaverShootout = shootout
  { mname   = "auto-shootout (3)"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterShootout
  }

screensaverEscape = escape
  { mname   = "auto-escape (4)"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterEscape
  }

screensaverZoo = zoo
  { mname   = "auto-zoo (5)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterZoo
  }

screensaverAmbush = ambush
  { mname   = "auto-ambush (6)"
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
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = -- changing leader by client needed, because of TFollow
              screensave (AutoLeader False True) rosterSafari
  }

rosterRaid, rosterBrawl, rosterShootout, rosterEscape, rosterZoo, rosterAmbush, rosterCrawl, rosterCrawlEmpty, rosterCrawlSurvival, rosterSafari, rosterSafariSurvival, rosterBattle, rosterBattleSurvival, rosterDefense, rosterDefenseEmpty :: Roster

rosterRaid = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiRaid}
                   , [(-2, 1, "hero")] )
                 , ( playerAntiHero { fname = "Indigo Founder"
                                    , fhiCondPoly = hiRaid }
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
                                , fhiCondPoly = hiDweller }
                   , [(-3, 3, "hero")] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiDweller }
                   , [(-3, 3, "hero")] )
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
                                , fhiCondPoly = hiDweller }
                   , [(-5, 1, "scout hero"), (-5, 2, "ranger hero")] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiDweller }
                   , [(-5, 1, "scout hero"), (-5, 2, "ranger hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterEscape = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiEscapist}
                   , [(-7, 1, "scout hero"), (-7, 2, "escapist hero")] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False  -- start on escape
                                    , fneverEmpty = False  -- loot after killing
                                    , fhiCondPoly = hiDweller }
                   , [(-7, 1, "scout hero"), (-7, 7, "ambusher hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Explorer", "Indigo Researcher")
                  , ("Explorer", "Horror Den")
                  , ("Indigo Researcher", "Horror Den") ]
  , rosterAlly = [] }

rosterZoo = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiDweller }
                   , [(-8, 5, "soldier hero")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(-8, 100, "mobile animal")] )
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = [ ("Explorer", "Animal Kingdom")
                  , ("Explorer", "Horror Den") ]
  , rosterAlly = [] }

rosterAmbush = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiDweller }
                   , [(-9, 1, "scout hero"), (-9, 5, "ambusher hero")] )
                 , ( playerAntiHero { fname = "Indigo Researcher"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiDweller }
                   , [(-9, 1, "scout hero"), (-9, 5, "ambusher hero")] )
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
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = []
  , rosterAlly = [] }

rosterCrawlSurvival = rosterCrawl
  { rosterList = [ ( playerHero { fleaderMode =
                                    LeaderAI $ AutoLeader True False
                                , fhasUI = False }
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
                   , [(-4, 3, "civilian")] )
                 , ( playerAnimalMagnificent
                   , [(-7, 20, "mobile animal")] )
                 , ( playerAnimalExquisite  -- start on escape
                   , [(-10, 30, "mobile animal")] ) ]
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
                                , fhiCondPoly = hiDweller }
                   , [(-5, 5, "soldier hero")] )
                 , ( playerMonster {fneverEmpty = True}
                   , [(-5, 35, "mobile monster")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(-5, 30, "mobile animal")] ) ]
  , rosterEnemy = [ ("Explorer", "Monster Hive")
                  , ("Explorer", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiDweller
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
                 , (playerHorror, []) ]  -- for summoned animals
  , rosterEnemy = []
  , rosterAlly = [] }

cavesRaid, cavesBrawl, cavesShootout, cavesEscape, cavesZoo, cavesAmbush, cavesCrawl, cavesDig, cavesSee, cavesSafari, cavesBattle :: Caves

cavesRaid = IM.fromList [(-2, "caveRaid")]

cavesBrawl = IM.fromList [(-3, "caveBrawl")]

cavesShootout = IM.fromList [(-5, "caveShootout")]

cavesEscape = IM.fromList [(-7, "caveEscape")]

cavesZoo = IM.fromList [(-8, "caveZoo")]

cavesAmbush = IM.fromList [(-9, "caveAmbush")]

listCrawl :: [(Int, GroupName CaveKind)]
listCrawl =
  [ (-1, "outermost")
  , (-2, "shallow random 2")
  , (-3, "caveEmpty") ]
  ++ zip [-4, -5] (repeat "default random")
  ++ zip [-6, -7, -8, -9] (repeat "deep random")
  ++ [(-10, "caveNoise2")]

cavesCrawl = IM.fromList listCrawl

cavesDig = IM.fromList $ zip [-1, -2 ..] $ map snd $ concat
                       $ replicate 100 listCrawl

cavesSee = IM.fromList $ zip [-1, -2 ..] $ concat $ map (replicate 10)
  [ "caveRaid", "caveBrawl", "caveShootout", "caveEscape", "caveZoo"
  , "caveAmbush"
  , "caveRogue", "caveLaboratory", "caveEmpty", "caveArena", "caveArena2"
  , "caveNoise", "caveNoise2" ]

cavesSafari = IM.fromList [ (-4, "caveSafari1")
                          , (-7, "caveSafari2")
                          , (-10, "caveSafari3") ]

cavesBattle = IM.fromList [(-5, "caveBattle")]
