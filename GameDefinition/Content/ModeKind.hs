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
      [raid, brawl, ambush, battle, exploration, battleSurvival, safari, safariSurvival, defense, boardgame, screensaverSafari, screensaverRaid, screensaverBrawl, screensaverAmbush]
  }
raid,        brawl, ambush, battle, exploration, battleSurvival, safari, safariSurvival, defense, boardgame, screensaverSafari, screensaverRaid, screensaverBrawl, screensaverAmbush :: ModeKind

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
  , mdesc   = "Your engineering team disagreed over a drink with some gentlemen scientists about premises of a relative completeness theorem and there's only one way to settle that."
  }

ambush = ModeKind
  { msymbol = 'm'
  , mname   = "ambush"
  , mfreq   = [("ambush", 1), ("campaign scenario", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mdesc   = "Surprising, striking ideas and fast execution are what makes or breaks a creative team!"
  }

battle = ModeKind
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [("battle", 1), ("campaign scenario", 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked against those that unleash the horrors of abstraction."
  }

exploration = ModeKind
  { msymbol = 'c'
  , mname   = "exploration"
  , mfreq   = [("exploration", 1), ("campaign scenario", 1)]
  , mroster = rosterExploration
  , mcaves  = cavesExploration
  , mdesc   = "Don't let wanton curiosity, greed and the creeping abstraction madness keep you down there in the darkness for too long!"
  }

battleSurvival = ModeKind
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [("battle survival", 1)]
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  }

safari = ModeKind
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the bottommost level. (VR recording recovered from monster nest debris)"
  }

safariSurvival = ModeKind
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

defense = ModeKind
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesExploration
  , mdesc   = "Don't let human interlopers defile your abstract secrets and flee unpunished!"
  }

boardgame = ModeKind
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


rosterRaid, rosterBrawl, rosterAmbush, rosterBattle, rosterExploration, rosterBattleSurvival, rosterSafari, rosterSafariSurvival, rosterDefense, rosterBoardgame :: Roster

rosterRaid = Roster
  { rosterList = [ playerHero { fhiCondPoly = hiRaid
                              , fentryLevel = -4
                              , finitialActors = 1 }
                 , playerAntiHero { fname = "Red Founder"
                                  , fhiCondPoly = hiRaid
                                  , fentryLevel = -4
                                  , finitialActors = 1 }
                 , playerAnimal { fentryLevel = -4
                                , finitialActors = 2 } ]
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

rosterAmbush = Roster
  { rosterList = [ playerSniper { fcanEscape = False
                                , fhiCondPoly = hiDweller
                                , fentryLevel = -5
                                , finitialActors = 4 }
                 , playerAntiSniper { fname = "Blue Hijacker"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiDweller
                                    , fentryLevel = -5
                                    , finitialActors = 4 }
                 , playerHorror {fentryLevel = -5} ]
  , rosterEnemy = [ ("Explorer Party", "Blue Hijacker")
                  , ("Explorer Party", "Horror Den")
                  , ("Blue Hijacker", "Horror Den") ]
  , rosterAlly = [] }

rosterBattle = Roster
  { rosterList = [ playerSoldier { fcanEscape = False
                                 , fhiCondPoly = hiDweller
                                 , fentryLevel = -5
                                 , finitialActors = 5 }
                 , playerMobileMonster { fentryLevel = -5
                                       , finitialActors = 35
                                       , fneverEmpty = True }
                 , playerMobileAnimal { fentryLevel = -5
                                      , finitialActors = 30
                                      , fneverEmpty = True } ]
  , rosterEnemy = [ ("Explorer Party", "Monster Hive")
                  , ("Explorer Party", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterExploration = Roster
  { rosterList = [ playerHero
                 , playerMonster
                 , playerAnimal ]
  , rosterEnemy = [ ("Explorer Party", "Monster Hive")
                  , ("Explorer Party", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ playerSoldier { fcanEscape = False
                                 , fhiCondPoly = hiDweller
                                 , fentryLevel = -5
                                 , finitialActors = 5
                                 , fleaderMode =
                                     LeaderAI $ AutoLeader False False
                                 , fhasUI = False }
                 , playerMobileMonster { fentryLevel = -5
                                       , finitialActors = 35
                                       , fneverEmpty = True }
                 , playerMobileAnimal { fentryLevel = -5
                                      , finitialActors = 30
                                      , fneverEmpty = True
                                      , fhasUI = True } ] }

playerMonsterTourist, playerHunamConvict, playerAnimalMagnificent, playerAnimalExquisite :: Player Dice

playerMonsterTourist =
  playerAntiMonster { fname = "Monster Tourist Office"
                    , fcanEscape = True
                    , fneverEmpty = True  -- no spawning
                      -- Follow-the-guide, as tourists do.
                    , ftactic = TFollow
                    , fentryLevel = -4
                    , finitialActors = 15
                    , fleaderMode =
                        LeaderUI $ AutoLeader False False }

playerHunamConvict =
  playerCivilian { fname = "Hunam Convict Pack"
                 , fentryLevel = -4 }

playerAnimalMagnificent =
  playerMobileAnimal { fname = "Animal Magnificent Specimen Variety"
                     , fneverEmpty = True
                     , fentryLevel = -7
                     , finitialActors = 10
                     , fleaderMode =  -- False to move away from stairs
                         LeaderAI $ AutoLeader True False }

playerAnimalExquisite =
  playerMobileAnimal { fname = "Animal Exquisite Herds and Packs"
                     , fneverEmpty = True
                     , fentryLevel = -10
                     , finitialActors = 30 }

rosterSafari = Roster
  { rosterList = [ playerMonsterTourist
                 , playerHunamConvict
                 , playerAnimalMagnificent
                 , playerAnimalExquisite
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

rosterDefense = rosterExploration
  { rosterList = [ playerAntiHero
                 , playerAntiMonster
                 , playerAnimal ] }

rosterBoardgame = Roster
  { rosterList = [ playerHero { fname = "Blue"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -3
                              , finitialActors = 6 }
                 , playerAntiHero { fname = "Red"
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = -3
                                  , finitialActors = 6 }
                 , playerHorror ]
  , rosterEnemy = [ ("Blue", "Red")
                  , ("Blue", "Horror Den")
                  , ("Red", "Horror Den") ]
  , rosterAlly = [] }

cavesRaid, cavesBrawl, cavesAmbush, cavesBattle, cavesExploration, cavesSafari, cavesBoardgame :: Caves

cavesRaid = IM.fromList [(-4, "caveRogueLit")]

cavesBrawl = IM.fromList [(-3, "caveBrawl")]

cavesAmbush = IM.fromList [(-5, "caveAmbush")]

cavesBattle = IM.fromList [(-5, "caveBattle")]

cavesExploration = IM.fromList $
  [ (-1, "shallow random 1")
  , (-2, "caveRogue")
  , (-3, "caveEmpty") ]
  ++ zip [-4, -5..(-9)] (repeat "default random")
  ++ [(-10, "caveNoise")]

cavesSafari = IM.fromList [ (-4, "caveSafari1")
                          , (-7, "caveSafari2")
                          , (-10, "caveSafari3") ]

cavesBoardgame = IM.fromList [(-3, "caveBoardgame")]
