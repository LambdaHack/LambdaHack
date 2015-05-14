-- | Game mode definitions.
module Content.ModeKind ( cdefs ) where

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
  , content =
      [campaign, raid, skirmish, ambush, battle, battleSurvival, safari, safariSurvival, pvp, coop, defense, screensaver, boardgame]
  }
campaign,        raid, skirmish, ambush, battle, battleSurvival, safari, safariSurvival, pvp, coop, defense, screensaver, boardgame :: ModeKind

campaign = ModeKind
  { msymbol = 'c'
  , mname   = "campaign"
  , mfreq   = [("campaign", 1)]
  , mroster = rosterCampaign
  , mcaves  = cavesCampaign
  , mdesc   = "Don't let wanton curiosity, greed and the creeping abstraction madness keep you down there in the darkness for too long!"
  }

raid = ModeKind
  { msymbol = 'r'
  , mname   = "raid"
  , mfreq   = [("raid", 1)]
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mdesc   = "An incredibly advanced typing machine worth 100 gold is buried at the other end of this maze. Be the first to claim it and fund a research team that will make typing accurate and dependable forever."
  }

skirmish = ModeKind
  { msymbol = 'k'
  , mname   = "skirmish"
  , mfreq   = [("skirmish", 1)]
  , mroster = rosterSkirmish
  , mcaves  = cavesSkirmish
  , mdesc   = "Your type theory research teams disagreed about the premises of a relative completeness theorem and there's only one way to settle that."
  }

ambush = ModeKind
  { msymbol = 'm'
  , mname   = "ambush"
  , mfreq   = [("ambush", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mdesc   = "Surprising, striking ideas and fast execution are what makes or breaks a creative team!"
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

safari = ModeKind
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent (exit at the bottommost level)."
  }

safariSurvival = ModeKind
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

pvp = ModeKind
  { msymbol = 'v'
  , mname   = "PvP"
  , mfreq   = [("PvP", 1)]
  , mroster = rosterPvP
  , mcaves  = cavesSkirmish
  , mdesc   = "(Not usable right now.) This is a fight to the death between two human-controlled teams."
  }

coop = ModeKind
  { msymbol = 'o'
  , mname   = "Coop"
  , mfreq   = [("Coop", 1)]
  , mroster = rosterCoop
  , mcaves  = cavesCampaign
  , mdesc   = "(This mode is intended solely for automated testing.)"
  }

defense = ModeKind
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesCampaign
  , mdesc   = "Don't let the humans defile your abstract secrets and flee, like the vulgar, literal, base scoundrels that they are!"
  }

screensaver = safari
  { mname   = "safari screensaver"
  , mfreq   = [("screensaver", 1), ("starting", 1)]
  , mroster = rosterSafari
      { rosterList = (head (rosterList rosterSafari))
                       -- changing leader by client needed, because of TFollow
                       {fleaderMode = LeaderAI $ AutoLeader True False}
                     : tail (rosterList rosterSafari)
      }
  }

boardgame = ModeKind
  { msymbol = 'g'
  , mname   = "boardgame"
  , mfreq   = [("boardgame", 1)]
  , mroster = rosterBoardgame
  , mcaves  = cavesBoardgame
  , mdesc   = "Small room, no exits. Who will prevail?"
  }


rosterCampaign, rosterRaid, rosterSkirmish, rosterAmbush, rosterBattle, rosterBattleSurvival, rosterSafari, rosterSafariSurvival, rosterPvP, rosterCoop, rosterDefense, rosterBoardgame:: Roster

rosterCampaign = Roster
  { rosterList = [ playerHero
                 , playerMonster
                 , playerAnimal ]
  , rosterEnemy = [ ("Adventurer Party", "Monster Hive")
                  , ("Adventurer Party", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterRaid = Roster
  { rosterList = [ playerHero { fname = "White Recursive"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -4
                              , finitialActors = 1 }
                 , playerAntiHero { fname = "Red Iterative"
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = -4
                                  , finitialActors = 1 }
                 , playerAnimal { fentryLevel = -4
                                , finitialActors = 2 } ]
  , rosterEnemy = [ ("White Recursive", "Animal Kingdom")
                  , ("Red Iterative", "Animal Kingdom") ]
  , rosterAlly = [] }

rosterSkirmish = Roster
  { rosterList = [ playerHero { fname = "White Haskell"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -3 }
                 , playerAntiHero { fname = "Purple Agda"
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = -3 }
                 , playerHorror ]
  , rosterEnemy = [ ("White Haskell", "Purple Agda")
                  , ("White Haskell", "Horror Den")
                  , ("Purple Agda", "Horror Den") ]
  , rosterAlly = [] }

rosterAmbush = Roster
  { rosterList = [ playerSniper { fname = "Yellow Idris"
                                , fhiCondPoly = hiDweller
                                , fentryLevel = -5
                                , finitialActors = 4 }
                 , playerAntiSniper { fname = "Blue Epigram"
                                    , fhiCondPoly = hiDweller
                                    , fentryLevel = -5
                                    , finitialActors = 4 }
                 , playerHorror {fentryLevel = -5} ]
  , rosterEnemy = [ ("Yellow Idris", "Blue Epigram")
                  , ("Yellow Idris", "Horror Den")
                  , ("Blue Epigram", "Horror Den") ]
  , rosterAlly = [] }

rosterBattle = Roster
  { rosterList = [ playerSoldier { fhiCondPoly = hiDweller
                                 , fentryLevel = -5
                                 , finitialActors = 5 }
                 , playerMobileMonster { fentryLevel = -5
                                       , finitialActors = 35
                                       , fneverEmpty = True }
                 , playerMobileAnimal { fentryLevel = -5
                                      , finitialActors = 30
                                      , fneverEmpty = True } ]
  , rosterEnemy = [ ("Armed Adventurer Party", "Monster Hive")
                  , ("Armed Adventurer Party", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ playerSoldier { fhiCondPoly = hiDweller
                                 , fentryLevel = -5
                                 , finitialActors = 5
                                 , fleaderMode =
                                     LeaderAI $ AutoLeader True False
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
                     , fleaderMode =  -- move away from stairs
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
                    , "Animal Magnificent Specimen Variety")
                  , ( "Monster Tourist Office"
                    , "Animal Exquisite Herds and Packs") ]
  , rosterAlly = [ ( "Animal Magnificent Specimen Variety"
                   , "Animal Exquisite Herds and Packs" )
                 , ( "Animal Magnificent Specimen Variety"
                   , "Hunam Convict Pack" )
                 , ( "Hunam Convict Pack"
                   , "Animal Exquisite Herds and Packs" ) ] }

rosterSafariSurvival = rosterSafari
  { rosterList = [ playerMonsterTourist
                     { fleaderMode = LeaderAI $ AutoLeader True False
                     , fhasUI = False }
                 , playerHunamConvict
                 , playerAnimalMagnificent
                     { fleaderMode = LeaderUI $ AutoLeader False False
                     , fhasUI = True }
                 , playerAnimalExquisite
                 ] }

rosterPvP = Roster
  { rosterList = [ playerHero { fname = "Red"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -3 }
                 , playerHero { fname = "Blue"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -3 }
                 , playerHorror ]
  , rosterEnemy = [ ("Red", "Blue")
                  , ("Red", "Horror Den")
                  , ("Blue", "Horror Den") ]
  , rosterAlly = [] }

rosterCoop = Roster
  { rosterList = [ playerAntiHero { fname = "Coral" }
                 , playerAntiHero { fname = "Amber"
                                  , fleaderMode = LeaderNull }
                 , playerAnimal { fhasUI = True }
                 , playerAnimal
                 , playerMonster
                 , playerMonster { fname = "Leaderless Monster Hive"
                                 , fleaderMode = LeaderNull } ]
  , rosterEnemy = [ ("Coral", "Monster Hive")
                  , ("Amber", "Monster Hive") ]
  , rosterAlly = [ ("Coral", "Amber") ] }

rosterDefense = rosterCampaign
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

cavesCampaign, cavesRaid, cavesSkirmish, cavesAmbush, cavesBattle, cavesSafari, cavesBoardgame :: Caves

cavesCampaign = IM.fromList
                $ [ (-1, ("shallow random 1", Just True))
                  , (-2, ("caveRogue", Nothing))
                  , (-3, ("caveEmpty", Nothing)) ]
                  ++ zip [-4, -5..(-9)] (repeat ("campaign random", Nothing))
                  ++ [(-10, ("caveNoise", Nothing))]

cavesRaid = IM.fromList [(-4, ("caveRogueLit", Just True))]

cavesSkirmish = IM.fromList [(-3, ("caveSkirmish", Nothing))]

cavesAmbush = IM.fromList [(-5, ("caveAmbush", Nothing))]

cavesBattle = IM.fromList [(-5, ("caveBattle", Nothing))]

cavesSafari = IM.fromList [ (-4, ("caveSafari1", Nothing))
                          , (-7, ("caveSafari2", Nothing))
                          , (-10, ("caveSafari3", Just False)) ]

cavesBoardgame = IM.fromList [(-3, ("caveBoardgame", Nothing))]
