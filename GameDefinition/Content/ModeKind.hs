-- | The type of kinds of game modes for LambdaHack.
module Content.ModeKind ( cdefs ) where

import qualified Data.IntMap.Strict as IM

import Content.ModeKindPlayer
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.ModeKind

cdefs :: ContentDef ModeKind
cdefs = ContentDef
  { getSymbol = msymbol
  , getName = mname
  , getFreq = mfreq
  , validate = validateModeKind
  , content =
      [campaign, duel, skirmish, ambush, battle, safari, pvp, coop, defense]
  }
campaign,        duel, skirmish, ambush, battle, safari, pvp, coop, defense :: ModeKind

campaign = ModeKind
  { msymbol = 'a'
  , mname   = "campaign"
  , mfreq   = [("campaign", 1)]
  , mroster = rosterCampaign
  , mcaves  = cavesCampaign
  , mdesc   = "Don't let wanton curiosity, greed and the creeping abstraction madness keep you down there in the darkness for too long!"
  }

duel = ModeKind
  { msymbol = 'u'
  , mname   = "duel"
  , mfreq   = [("duel", 1)]
  , mroster = rosterDuel
  , mcaves  = cavesSkirmish
  , mdesc   = "You disagreed about the premises of a relative completeness theorem and there's only one way to settle that."
  }

skirmish = ModeKind
  { msymbol = 'k'
  , mname   = "skirmish"
  , mfreq   = [("skirmish", 1)]
  , mroster = rosterSkirmish
  , mcaves  = cavesSkirmish
  , mdesc   = "The scoring system of a programming contest fails to determine the winning team and participants take matters into their own hands."
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

safari = ModeKind
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent (exit at the bottommost level)."
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


rosterCampaign, rosterDuel, rosterSkirmish, rosterAmbush, rosterBattle, rosterSafari, rosterPvP, rosterCoop, rosterDefense :: Roster

rosterCampaign = Roster
  { rosterList = [ playerHero
                 , playerMonster
                 , playerAnimal ]
  , rosterEnemy = [ ("Adventurer Party", "Monster Hive")
                  , ("Adventurer Party", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterDuel = Roster
  { rosterList = [ playerHero { fname = "White Recursive"
                              , finitialActors = 1 }
                 , playerAntiHero { fname = "Red Iterative"
                                  , finitialActors = 1 }
                 , playerHorror ]
  , rosterEnemy = [ ("White Recursive", "Red Iterative")
                  , ("White Recursive", "Horror Den")
                  , ("Red Iterative", "Horror Den") ]
  , rosterAlly = [] }

rosterSkirmish = rosterDuel
  { rosterList = [ playerHero {fname = "White Haskell"}
                 , playerAntiHero {fname = "Purple Agda"}
                 , playerHorror ]
  , rosterEnemy = [ ("White Haskell", "Purple Agda")
                  , ("White Haskell", "Horror Den")
                  , ("Purple Agda", "Horror Den") ] }

rosterAmbush = rosterDuel
  { rosterList = [ playerHero {fname = "Yellow Idris"}
                 , playerAntiHero {fname = "Blue Epigram"}
                 , playerHorror ]
  , rosterEnemy = [ ("Yellow Idris", "Blue Epigram")
                  , ("Yellow Idris", "Horror Den")
                  , ("Blue Epigram", "Horror Den") ] }

rosterBattle = Roster
  { rosterList = [ playerHero {finitialActors = 5}
                 , playerMonster { finitialActors = 15
                                 , fneverEmpty = True }
                 , playerAnimal { finitialActors = 10
                                , fneverEmpty = True } ]
  , rosterEnemy = [ ("Adventurer Party", "Monster Hive")
                  , ("Adventurer Party", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }

rosterSafari = Roster
  { rosterList = [ playerAntiMonster { fname = "Monster Tourist Office"
                                     , fcanEscape = True
                                     , fneverEmpty = True
                                     , fentryLevel = -4
                                     , finitialActors = 10
                                     , fhasLeader = LeaderMode False False }
                 , playerCivilian { fname = "Hunam Convict Pack"
                                  , fentryLevel = -4 }
                 , playerAnimal { fname =
                                    "Animal Magnificent Specimen Variety"
                                , fneverEmpty = True
                                , fentryLevel = -7
                                , finitialActors = 7 }
                 , playerAnimal { fname =
                                    "Animal Exquisite Herds and Packs"
                                , fneverEmpty = True
                                , fentryLevel = -10
                                , finitialActors = 20 } ]
  , rosterEnemy = [ ("Monster Tourist Office", "Hunam Convict Pack")
                  , ("Monster Tourist Office",
                     "Animal Magnificent Specimen Variety")
                  , ("Monster Tourist Office",
                     "Animal Exquisite Herds and Packs") ]
  , rosterAlly = [( "Animal Magnificent Specimen Variety"
                  , "Animal Exquisite Herds and Packs" )] }

rosterPvP = Roster
  { rosterList = [ playerHero {fname = "Red"}
                 , playerHero {fname = "Blue"}
                 , playerHorror ]
  , rosterEnemy = [ ("Red", "Blue")
                  , ("Red", "Horror Den")
                  , ("Blue", "Horror Den") ]
  , rosterAlly = [] }

rosterCoop = Roster
  { rosterList = [ playerAntiHero { fname = "Coral" }
                 , playerAntiHero { fname = "Amber"
                                  , fhasLeader = LeaderNull }
                 , playerAntiHero { fname = "Green" }
                 , playerAnimal { fhasUI = True }
                 , playerMonster
                 , playerMonster { fname = "Leaderless Monster Hive"
                                 , fhasLeader = LeaderNull } ]
  , rosterEnemy = [ ("Coral", "Monster Hive")
                  , ("Amber", "Monster Hive")
                  , ("Animal Kingdom", "Leaderless Monster Hive") ]
  , rosterAlly = [ ("Coral", "Amber")
                 , ("Coral", "Green")
                 , ("Amber", "Green")
                 , ("Green", "Animal Kingdom")
                 , ("Green", "Monster Hive")
                 , ("Green", "Leaderless Monster Hive") ] }

rosterDefense = Roster
  { rosterList = [ playerAntiMonster { finitialActors = 1
                                     , fhasLeader = LeaderMode True False }
                 , playerAntiHero { fname = "Yellow"
                                  , finitialActors = 10 }
                 , playerAnimal ]
  , rosterEnemy = [ ("Yellow", "Monster Hive")
                  , ("Yellow", "Animal Kingdom") ]
  , rosterAlly = [("Monster Hive", "Animal Kingdom")] }


cavesCampaign, cavesSkirmish, cavesAmbush, cavesBattle, cavesSafari :: Caves

cavesCampaign = IM.fromList $ [ (-1, ("caveRogue", Just True))
                              , (-2, ("caveRogue", Nothing))
                              , (-3, ("caveEmpty", Nothing)) ]
                              ++ zip [-4, -5..(-9)] (repeat ("dng", Nothing))
                              ++ [(-10, ("caveNoise", Nothing))]

cavesSkirmish = IM.fromList [(-3, ("caveSkirmish", Nothing))]

cavesAmbush = IM.fromList [(-5, ("caveAmbush", Nothing))]

cavesBattle = IM.fromList [(-3, ("caveBattle", Nothing))]

cavesSafari = IM.fromList [ (-4, ("caveSafari1", Nothing))
                          , (-7, ("caveSafari2", Nothing))
                          , (-10, ("caveSafari3", Just False)) ]
