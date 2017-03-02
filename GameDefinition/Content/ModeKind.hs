-- | Game mode definitions.
module Content.ModeKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.IntMap.Strict as IM

import Content.ModeKindPlayer
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ModeKind

cdefs :: ContentDef ModeKind
cdefs = ContentDef
  { getSymbol = msymbol
  , getName = mname
  , getFreq = mfreq
  , validateSingle = validateSingleModeKind
  , validateAll = validateAllModeKind
  , content = contentFromList
      [raid, brawl, shootout, escape, ambush, exploration, safari, safariSurvival, battle, battleSurvival, defense, boardgame, screensaverSafari, screensaverRaid, screensaverBrawl, screensaverAmbush]
  }
raid,        brawl, shootout, escape, ambush, exploration, safari, safariSurvival, battle, battleSurvival, defense, boardgame, screensaverSafari, screensaverRaid, screensaverBrawl, screensaverAmbush :: ModeKind

raid = ModeKind
  { msymbol = 'r'
  , mname   = "raid"
  , mfreq   = [("raid", 1), ("campaign scenario", 1)]
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mdesc   = "An incredibly advanced typing machine worth 100 gold is buried at the other end of this maze. Be the first to claim it and fund a research team that makes typing accurate and dependable forever."
  }

brawl = ModeKind
  { msymbol = 'k'
  , mname   = "brawl"
  , mfreq   = [("brawl", 1), ("campaign scenario", 1)]
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mdesc   = "Your engineering team disagreed over a drink with some gentlemen scientists about premises of a relative completeness theorem and there's only one way to settle that. Remember to keep together so that neither team is tempted to gang upon a solitary disputant."
  }

-- The trajectory tip is important because of tactics of scout looking from
-- behind a bush and others hiding in mist. If no suitable bushes,
-- fire once and flee into mist or behind cover. Then whomever is out of LOS
-- range or inside mist can shoot at the last seen enemy locations,
-- adjusting and according to ounds and incoming missile trajectories.
-- If the scount can't find bushes or glass building to set a lookout,
-- the other team member are more spotters and guardians than snipers
-- and that's their only role, so a small party makes sense.
shootout = ModeKind
  { msymbol = 's'
  , mname   = "shootout"
  , mfreq   = [("shootout", 1), ("campaign scenario", 1)]
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mdesc   = "Whose arguments are most striking and whose ideas fly fastest? Let's scatter up, attack the problems from different angles and find out. To display the trajectory of any soaring entity, point it with the crosshair in aiming mode."
  }

escape = ModeKind
  { msymbol = 'e'
  , mname   = "escape"
  , mfreq   = [("escape", 1), ("campaign scenario", 1)]
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mdesc   = "Dwelling into dark matters is dangerous. Avoid the crowd of firebrand disputants, catch any gems of thought, find a way out and bring back a larger team to shed new light on the field."
  }

-- The tactic is to sneak in the dark, highlight enemy with thrown torches
-- (and douse thrown enemy torches with blankets) and only if this fails,
-- actually scout using extended noctovision.
-- With reaction fire, larger team is more fun.
ambush = ModeKind
  { msymbol = 'm'
  , mname   = "ambush"
  , mfreq   = [("ambush", 1), ("campaign scenario", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mdesc   = "Prevent highjacking of your ideas at all cost! Be stealthy, be aggresive. Fast execution is what makes or breaks a creative team."
  }

exploration = ModeKind
  { msymbol = 'c'
  , mname   = "crawl (long)"
  , mfreq   = [ ("crawl (long)", 1), ("exploration", 1)
              , ("campaign scenario", 1) ]
  , mroster = rosterExploration
  , mcaves  = cavesExploration
  , mdesc   = "Don't let wanton curiosity, greed and the creeping abstraction madness keep you down there in the darkness for too long!"
  }

safari = ModeKind  -- easter egg available only via screensaver
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mdesc   = "\"In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the bottommost level.\" This is a VR recording recovered from a monster nest debris."
  }

safariSurvival = ModeKind  -- testing scenario
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

battle = ModeKind  -- testing scenario
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [("battle", 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked against those that unleash the horrors of abstraction."
  }

battleSurvival = ModeKind  -- testing scenario
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [("battle survival", 1)]
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  }

defense = ModeKind  -- testing scenario; perhaps real scenario in the future
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesExploration
  , mdesc   = "Don't let human interlopers defile your abstract secrets and flee unpunished!"
  }

boardgame = ModeKind  -- future work
  { msymbol = 'g'
  , mname   = "boardgame"
  , mfreq   = [("boardgame", 1)]
  , mroster = rosterBoardgame
  , mcaves  = cavesBoardgame
  , mdesc   = "Small room, no exits. Who will prevail?"
  }

screensaverSafari = safari
  { mname   = "auto-safari"
  , mfreq   = [("starting", 1), ("no confirms", 1)]
  , mroster = rosterSafari
      { rosterList = (head (rosterList rosterSafari))
                       -- changing leader by client needed, because of TFollow
                       {fleaderMode = LeaderAI $ AutoLeader False True}
                     : tail (rosterList rosterSafari)
      }
  }

screensaverRaid = raid
  { mname   = "auto-raid"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = rosterRaid
      { rosterList = (head (rosterList rosterRaid))
                       {fleaderMode = LeaderAI $ AutoLeader False False}
                     : tail (rosterList rosterRaid)
      }
  }

screensaverBrawl = brawl
  { mname   = "auto-brawl"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = rosterBrawl
      { rosterList = (head (rosterList rosterBrawl))
                       {fleaderMode = LeaderAI $ AutoLeader False False}
                     : tail (rosterList rosterBrawl)
      }
  }

screensaverAmbush = ambush
  { mname   = "auto-ambush"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = rosterAmbush
      { rosterList = (head (rosterList rosterAmbush))
                       {fleaderMode = LeaderAI $ AutoLeader False False}
                     : tail (rosterList rosterAmbush)
      }
  }


rosterRaid, rosterBrawl, rosterShootout, rosterEscape, rosterAmbush, rosterExploration, rosterSafari, rosterSafariSurvival, rosterBattle, rosterBattleSurvival, rosterDefense, rosterBoardgame :: Roster

rosterRaid = Roster
  { rosterList = [ playerHero { fhiCondPoly = hiRaid
                              , fentryLevel = -4
                              , finitialActors = [(1, "hero")] }
                 , playerAntiHero { fname = "Red Founder"
                                  , fhiCondPoly = hiRaid
                                  , fentryLevel = -4
                                  , finitialActors = [(1, "hero")] }
                 , playerAnimal { fentryLevel = -4  -- starting over escape
                                , finitialActors = [(2, "animal")] } ]
  , rosterEnemy = [ ("Explorer Party", "Animal Kingdom")
                  , ("Red Founder", "Animal Kingdom") ]
  , rosterAlly = [] }

rosterBrawl = Roster
  { rosterList = [ playerHero { fcanEscape = False
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -3 }
                 , playerAntiHero { fname = "Indigo Research"
                                  , fcanEscape = False
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = -3 }
                 , playerHorror ]
  , rosterEnemy = [ ("Explorer Party", "Indigo Research")
                  , ("Explorer Party", "Horror Den")
                  , ("Indigo Research", "Horror Den") ]
  , rosterAlly = [] }

-- Exactly one scout gets a sight boost, to help the aggressor, because he uses
-- the scout for initial attack, while camper (on big enough maps)
-- can't guess where the attack would come and so can't position his single
-- scout to counter the stealthy advance.
rosterShootout = Roster
  { rosterList = [ playerHero { fcanEscape = False
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -5
                              , finitialActors =
                                  [(1, "scout hero"), (2, "ranger hero")] }
                 , playerAntiHero { fname = "Indigo Research"
                                  , fcanEscape = False
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = -5
                                  , finitialActors =
                                      [(1, "scout hero"), (2, "ranger hero")] }
                 , playerHorror {fentryLevel = -5} ]
  , rosterEnemy = [ ("Explorer Party", "Indigo Research")
                  , ("Explorer Party", "Horror Den")
                  , ("Indigo Research", "Horror Den") ]
  , rosterAlly = [] }

rosterEscape = Roster
  { rosterList = [ playerHero { fhiCondPoly = hiEscapist
                              , fentryLevel = -8
                              , finitialActors =
                                  [ (1, "scout hero")
                                  , (2, "escapist hero") ] }
                 , playerAntiHero { fname = "Blue Hijacker"  -- start on escape
                                  , fcanEscape = False
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = -8
                                  , finitialActors =
                                      [ (1, "scout hero")
                                      , (7, "ambusher hero") ] }
                 , playerHorror {fentryLevel = -8} ]
  , rosterEnemy = [ ("Explorer Party", "Blue Hijacker")
                  , ("Explorer Party", "Horror Den")
                  , ("Blue Hijacker", "Horror Den") ]
  , rosterAlly = [] }

rosterAmbush = Roster
  { rosterList = [ playerHero { fcanEscape = False
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -10
                              , finitialActors =
                                  [ (1, "scout hero")
                                  , (5, "ambusher hero") ] }
                 , playerAntiHero { fname = "Blue Hijacker"
                                  , fcanEscape = False
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = -10
                                  , finitialActors =
                                      [ (1, "scout hero")
                                      , (5, "ambusher hero") ] }
                 , playerHorror {fentryLevel = -10} ]
  , rosterEnemy = [ ("Explorer Party", "Blue Hijacker")
                  , ("Explorer Party", "Horror Den")
                  , ("Blue Hijacker", "Horror Den") ]
  , rosterAlly = [] }

rosterExploration = Roster
  { rosterList = [ playerHero
                 , playerMonster
                 , playerAnimal ]
  , rosterEnemy = [ ("Explorer Party", "Monster Hive")
                  , ("Explorer Party", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

playerMonsterTourist, playerHunamConvict, playerAnimalMagnificent, playerAnimalExquisite :: Player Dice

playerMonsterTourist =
  playerAntiMonster { fname = "Monster Tourist Office"
                    , fcanEscape = True
                    , fneverEmpty = True  -- no spawning
                      -- Follow-the-guide, as tourists do.
                    , ftactic = TFollow
                    , fentryLevel = -4
                    , finitialActors = [(15, "monster")]
                    , fleaderMode =
                        LeaderUI $ AutoLeader False False }

playerHunamConvict =
  playerCivilian { fname = "Hunam Convict Pack"
                 , fentryLevel = -4 }

playerAnimalMagnificent =
  playerAnimal { fname = "Animal Magnificent Specimen Variety"
               , fneverEmpty = True
               , fentryLevel = -7
               , finitialActors = [(10, "mobile animal")]
               , fleaderMode =  -- False to move away from stairs
                   LeaderAI $ AutoLeader True False }

playerAnimalExquisite =
  playerAnimal { fname = "Animal Exquisite Herds and Packs"
               , fneverEmpty = True
               , fentryLevel = -10
               , finitialActors = [(30, "mobile animal")] }

rosterSafari = Roster
  { rosterList = [ playerMonsterTourist
                 , playerHunamConvict
                 , playerAnimalMagnificent
                 , playerAnimalExquisite  -- start on escape
                 ]
  , rosterEnemy = [ ("Monster Tourist Office", "Hunam Convict Pack")
                  , ( "Monster Tourist Office"
                    , "Animal Magnificent Specimen Variety" )
                  , ( "Monster Tourist Office"
                    , "Animal Exquisite Herds and Packs" ) ]
  , rosterAlly = [ ( "Animal Magnificent Specimen Variety"
                   , "Animal Exquisite Herds and Packs" )
                 , ( "Animal Magnificent Specimen Variety"
                   , "Hunam Convict Pack" )
                 , ( "Hunam Convict Pack"
                   , "Animal Exquisite Herds and Packs" ) ] }

rosterSafariSurvival = rosterSafari
  { rosterList = [ playerMonsterTourist
                     { fleaderMode = LeaderAI $ AutoLeader True True
                     , fhasUI = False }
                 , playerHunamConvict
                 , playerAnimalMagnificent
                     { fleaderMode = LeaderUI $ AutoLeader True False
                     , fhasUI = True }
                 , playerAnimalExquisite
                 ] }

rosterBattle = Roster
  { rosterList = [ playerHero { fcanEscape = False
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -5
                              , finitialActors = [(5, "soldier hero")] }
                 , playerMonster { fentryLevel = -5
                                 , finitialActors = [(35, "mobile monster")]
                                 , fneverEmpty = True }
                 , playerAnimal { fentryLevel = -5
                                , finitialActors = [(30, "mobile animal")]
                                , fneverEmpty = True } ]
  , rosterEnemy = [ ("Explorer Party", "Monster Hive")
                  , ("Explorer Party", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ playerHero { fcanEscape = False
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -5
                              , finitialActors = [(5, "soldier hero")]
                              , fleaderMode = LeaderAI $ AutoLeader False False
                              , fhasUI = False }
                 , playerMonster { fentryLevel = -5
                                 , finitialActors = [(35, "mobile monster")]
                                 , fneverEmpty = True }
                 , playerAnimal { fentryLevel = -5
                                , finitialActors = [(30, "mobile animal")]
                                , fneverEmpty = True
                                , fhasUI = True } ] }

rosterDefense = rosterExploration
  { rosterList = [ playerAntiHero
                 , playerAntiMonster
                 , playerAnimal ] }

rosterBoardgame = Roster
  { rosterList = [ playerHero { fname = "Blue"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -3
                              , finitialActors = [(6, "hero")] }
                 , playerAntiHero { fname = "Red"
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = -3
                                  , finitialActors = [(6, "hero")] }
                 , playerHorror ]
  , rosterEnemy = [ ("Blue", "Red")
                  , ("Blue", "Horror Den")
                  , ("Red", "Horror Den") ]
  , rosterAlly = [] }

cavesRaid, cavesBrawl, cavesShootout, cavesEscape, cavesAmbush, cavesExploration, cavesSafari, cavesBattle, cavesBoardgame :: Caves

cavesRaid = IM.fromList [(-4, "caveRaid")]

cavesBrawl = IM.fromList [(-3, "caveBrawl")]

cavesShootout = IM.fromList [(-5, "caveShootout")]

cavesEscape = IM.fromList [(-8, "caveEscape")]

cavesAmbush = IM.fromList [(-10, "caveAmbush")]

cavesExploration = IM.fromList $
  [ (-1, "shallow random 1")
  , (-2, "caveRogue")
  , (-3, "caveEmpty") ]
  ++ zip [-4, -5..(-9)] (repeat "default random")
  ++ [(-10, "caveNoise")]

cavesSafari = IM.fromList [ (-4, "caveSafari1")
                          , (-7, "caveSafari2")
                          , (-10, "caveSafari3") ]

cavesBattle = IM.fromList [(-5, "caveBattle")]

cavesBoardgame = IM.fromList [(-3, "caveBoardgame")]
