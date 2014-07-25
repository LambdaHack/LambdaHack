-- | The type of kinds of game modes for LambdaHack.
module Content.ModeKind ( cdefs ) where

import qualified Data.IntMap.Strict as IM

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
  { msymbol  = 'a'
  , mname    = "campaign"
  , mfreq    = [("campaign", 1)]
  , mplayers = playersCampaign
  , mcaves   = cavesCampaign
  , mdesc    = "Don't let wanton curiosity, greed and the creeping abstraction madness keep you down there in the darkness for too long!"
  }

duel = ModeKind
  { msymbol  = 'u'
  , mname    = "duel"
  , mfreq    = [("duel", 1)]
  , mplayers = playersDuel
  , mcaves   = cavesSkirmish
  , mdesc    = "You disagreed about the premises of a relative completeness theorem and there's only one way to settle that."
  }

skirmish = ModeKind
  { msymbol  = 'k'
  , mname    = "skirmish"
  , mfreq    = [("skirmish", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesSkirmish
  , mdesc    = "The scoring system of a programming contest fails to determine the winning team and participants take matters into their own hands."
  }

ambush = ModeKind
  { msymbol  = 'm'
  , mname    = "ambush"
  , mfreq    = [("ambush", 1)]
  , mplayers = playersAmbush
  , mcaves   = cavesAmbush
  , mdesc    = "Surprising, striking ideas and fast execution are what makes or breaks a creative team!"
  }

battle = ModeKind
  { msymbol  = 'b'
  , mname    = "battle"
  , mfreq    = [("battle", 1)]
  , mplayers = playersBattle
  , mcaves   = cavesBattle
  , mdesc    = "Odds are stacked against those that unleash the horrors of abstraction."
  }

safari = ModeKind
  { msymbol  = 'f'
  , mname    = "safari"
  , mfreq    = [("safari", 1)]
  , mplayers = playersSafari
  , mcaves   = cavesSafari
  , mdesc    = "In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent (exit at the bottommost level)."
  }

pvp = ModeKind
  { msymbol  = 'v'
  , mname    = "PvP"
  , mfreq    = [("PvP", 1)]
  , mplayers = playersPvP
  , mcaves   = cavesSkirmish
  , mdesc    = "(Not usable right now.) This is a fight to the death between two human-controlled teams."
  }

coop = ModeKind
  { msymbol  = 'o'
  , mname    = "Coop"
  , mfreq    = [("Coop", 1)]
  , mplayers = playersCoop
  , mcaves   = cavesCampaign
  , mdesc    = "(This mode is intended solely for automated testing.)"
  }

defense = ModeKind
  { msymbol  = 'e'
  , mname    = "defense"
  , mfreq    = [("defense", 1)]
  , mplayers = playersDefense
  , mcaves   = cavesCampaign
  , mdesc    = "Don't let the humans defile your abstract secrets and flee, like the vulgar, literal, base scoundrels that they are!"
  }


playersCampaign, playersDuel, playersSkirmish, playersAmbush, playersBattle, playersSafari, playersPvP, playersCoop, playersDefense :: Players

playersCampaign = Players
  { playersList = [ playerHero
                  , playerMonster
                  , playerAnimal ]
  , playersEnemy = [ ("Adventurer Party", "Monster Hive")
                   , ("Adventurer Party", "Animal Kingdom") ]
  , playersAlly = [("Monster Hive", "Animal Kingdom")] }

playersDuel = Players
  { playersList = [ playerHero { playerName = "White Recursive"
                               , playerInitial = 1 }
                  , playerAntiHero { playerName = "Red Iterative"
                                   , playerInitial = 1 }
                  , playerHorror ]
  , playersEnemy = [ ("White Recursive", "Red Iterative")
                   , ("White Recursive", "Horror Den")
                   , ("Red Iterative", "Horror Den") ]
  , playersAlly = [] }

playersSkirmish = playersDuel
  { playersList = [ playerHero {playerName = "White Haskell"}
                  , playerAntiHero {playerName = "Purple Agda"}
                  , playerHorror ]
  , playersEnemy = [ ("White Haskell", "Purple Agda")
                   , ("White Haskell", "Horror Den")
                   , ("Purple Agda", "Horror Den") ] }

playersAmbush = playersDuel
  { playersList = [ playerHero {playerName = "Yellow Idris"}
                  , playerAntiHero {playerName = "Blue Epigram"}
                  , playerHorror ]
  , playersEnemy = [ ("Yellow Idris", "Blue Epigram")
                   , ("Yellow Idris", "Horror Den")
                   , ("Blue Epigram", "Horror Den") ] }

playersBattle = Players
  { playersList = [ playerHero {playerInitial = 5}
                  , playerMonster { playerInitial = 15
                                  , playerIsSpawn = False }
                  , playerAnimal { playerInitial = 10
                                 , playerIsSpawn = False } ]
  , playersEnemy = [ ("Adventurer Party", "Monster Hive")
                   , ("Adventurer Party", "Animal Kingdom") ]
  , playersAlly = [("Monster Hive", "Animal Kingdom")] }

playersSafari = Players
  { playersList = [ playerMonster { playerName = "Monster Tourist Office"
                                  , playerIsSpawn = False
                                  , playerEntry = -4
                                  , playerInitial = 10
                                  , playerAI = False
                                  , playerUI = True }
                  , playerCivilian { playerName = "Hunam Convict Pack"
                                   , playerEntry = -4 }
                  , playerAnimal { playerName =
                                     "Animal Magnificent Specimen Variety"
                                 , playerIsSpawn = False
                                 , playerEntry = -7
                                 , playerInitial = 7 }
                  , playerAnimal { playerName =
                                     "Animal Exquisite Herds and Packs"
                                 , playerIsSpawn = False
                                 , playerEntry = -10
                                 , playerInitial = 20 } ]
  , playersEnemy = [ ("Monster Tourist Office", "Hunam Convict Pack")
                   , ("Monster Tourist Office",
                      "Animal Magnificent Specimen Variety")
                   , ("Monster Tourist Office",
                      "Animal Exquisite Herds and Packs") ]
  , playersAlly = [( "Animal Magnificent Specimen Variety"
                   , "Animal Exquisite Herds and Packs" )] }

playersPvP = Players
  { playersList = [ playerHero {playerName = "Red"}
                  , playerHero {playerName = "Blue"}
                  , playerHorror ]
  , playersEnemy = [ ("Red", "Blue")
                   , ("Red", "Horror Den")
                   , ("Blue", "Horror Den") ]
  , playersAlly = [] }

playersCoop = Players
  { playersList = [ playerAntiHero { playerName = "Coral" }
                  , playerAntiHero { playerName = "Amber"
                                   , playerLeader = False }
                  , playerAntiHero { playerName = "Green" }
                  , playerAnimal { playerUI = True }
                  , playerMonster
                  , playerMonster { playerName = "Leaderless Monster Hive"
                                  , playerLeader = False } ]
  , playersEnemy = [ ("Coral", "Monster Hive")
                   , ("Amber", "Monster Hive")
                   , ("Animal Kingdom", "Leaderless Monster Hive") ]
  , playersAlly = [ ("Coral", "Amber")
                  , ("Coral", "Green")
                  , ("Amber", "Green")
                  , ("Green", "Animal Kingdom")
                  , ("Green", "Monster Hive")
                  , ("Green", "Leaderless Monster Hive") ] }

playersDefense = Players
  { playersList = [ playerMonster { playerInitial = 1
                                  , playerAI = False
                                  , playerUI = True }
                  , playerAntiHero { playerName = "Yellow"
                                   , playerInitial = 10 }
                  , playerAnimal ]
  , playersEnemy = [ ("Yellow", "Monster Hive")
                   , ("Yellow", "Animal Kingdom") ]
  , playersAlly = [("Monster Hive", "Animal Kingdom")] }

playerHero, playerAntiHero, playerCivilian, playerMonster, playerAnimal, playerHorror :: Player

playerHero = Player
  { playerName = "Adventurer Party"
  , playerFaction = "hero"
  , playerIsSpawn = False
  , playerIsHero = True
  , playerEntry = -1
  , playerInitial = 3
  , playerLeader = True
  , playerAI = False
  , playerUI = True
  }

playerAntiHero = playerHero
  { playerAI = True
  , playerUI = False
  }

playerCivilian = Player
  { playerName = "Civilian Crowd"
  , playerFaction = "civilian"
  , playerIsSpawn = False
  , playerIsHero = False
  , playerEntry = -1
  , playerInitial = 3
  , playerLeader = False  -- unorganized
  , playerAI = True
  , playerUI = False
  }

playerMonster = Player
  { playerName = "Monster Hive"
  , playerFaction = "monster"
  , playerIsSpawn = True
  , playerIsHero = False
  , playerEntry = -3
  , playerInitial = 5
  , playerLeader = True
  , playerAI = True
  , playerUI = False
  }

playerAnimal = Player
  { playerName = "Animal Kingdom"
  , playerFaction = "animal"
  , playerIsSpawn = True
  , playerIsHero = False
  , playerEntry = -2
  , playerInitial = 3
  , playerLeader = False
  , playerAI = True
  , playerUI = False
  }

playerHorror = Player
  { playerName = "Horror Den"
  , playerFaction = "horror"
  , playerIsSpawn = False
  , playerIsHero = False
  , playerEntry = -1
  , playerInitial = 0
  , playerLeader = False
  , playerAI = True
  , playerUI = False
  }


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
