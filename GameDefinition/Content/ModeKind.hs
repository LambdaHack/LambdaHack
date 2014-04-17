-- | The type of kinds of game modes for LambdaHack.
module Content.ModeKind ( cdefs ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.ModeKind

cdefs :: ContentDef ModeKind
cdefs = ContentDef
  { getSymbol = msymbol
  , getName = mname
  , getFreq = mfreq
  , validate = validateModeKind
  , content =
      [campaign, skirmish, ambush, battle, pvp, coop, defense]
  }
campaign,        skirmish, ambush, battle, pvp, coop, defense :: ModeKind

campaign = ModeKind
  { msymbol  = 'a'
  , mname    = "campaign"
  , mfreq    = [("campaign", 1)]
  , mplayers = playersCampaign
  , mcaves   = cavesCampaign
  }

skirmish = ModeKind
  { msymbol  = 'k'
  , mname    = "skirmish"
  , mfreq    = [("skirmish", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesSkirmish
  }

ambush = ModeKind
  { msymbol  = 'm'
  , mname    = "ambush"
  , mfreq    = [("ambush", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesAmbush
  }

battle = ModeKind
  { msymbol  = 'b'
  , mname    = "battle"
  , mfreq    = [("battle", 1)]
  , mplayers = playersBattle
  , mcaves   = cavesBattle
  }

pvp = ModeKind
  { msymbol  = 'v'
  , mname    = "PvP"
  , mfreq    = [("PvP", 1)]
  , mplayers = playersPvP
  , mcaves   = cavesSkirmish
  }

coop = ModeKind
  { msymbol  = 'o'
  , mname    = "Coop"
  , mfreq    = [("Coop", 1)]
  , mplayers = playersCoop
  , mcaves   = cavesCampaign
  }

defense = ModeKind
  { msymbol  = 'e'
  , mname    = "defense"
  , mfreq    = [("defense", 1)]
  , mplayers = playersDefense
  , mcaves   = cavesCampaign
  }


playersCampaign, playersSkirmish, playersBattle, playersPvP, playersCoop, playersDefense :: Players

playersCampaign = Players
  { playersList = [ playerHero
                  , playerMonster
                  , playerAnimal ]
  , playersEnemy = [ ("Adventurer Party", "Monster Hive")
                   , ("Adventurer Party", "Animal Kingdom") ]
  , playersAlly = [("Monster Hive", "Animal Kingdom")] }

playersSkirmish = Players
  { playersList = [ playerHero {playerName = "White"}
                  , playerAntiHero {playerName = "Purple"}
                  , playerHorror ]
  , playersEnemy = [ ("White", "Purple")
                   , ("White", "Horror Den")
                   , ("Purple", "Horror Den") ]
  , playersAlly = [] }

playersBattle = Players
  { playersList = [ playerHero {playerInitial = 5}
                  , playerMonster { playerInitial = 20
                                  , playerSpawn = 0 }
                  , playerAnimal { playerInitial = 10
                                 , playerSpawn = 0 } ]
  , playersEnemy = [ ("Adventurer Party", "Monster Hive")
                   , ("Adventurer Party", "Animal Kingdom") ]
  , playersAlly = [("Monster Hive", "Animal Kingdom")] }

playersPvP = Players
  { playersList = [ playerHero {playerName = "Red"}
                  , playerHero {playerName = "Blue"}
                  , playerHorror ]
  , playersEnemy = [ ("Red", "Blue")
                   , ("Red", "Horror Den")
                   , ("Blue", "Horror Den") ]
  , playersAlly = [] }

playersCoop = Players
  { playersList = [ playerHero { playerName = "Coral" }
                  , playerHero { playerName = "Amber" }
                  , playerAntiHero { playerName = "Green" }
                  , playerAntiHero { playerName = "Yellow" }
                  , playerAntiHero { playerName = "Cyan" }
                  , playerAntiHero { playerName = "Red" }
                  , playerAntiHero { playerName = "Blue" }
                  , playerMonster { playerUI = True }
                  , playerMonster { playerName = "Neutral Monster Hive" } ]
  , playersEnemy = [ ("Coral", "Monster Hive")
                   , ("Amber", "Monster Hive")
                   , ("Green", "Monster Hive")
                   , ("Yellow", "Monster Hive")
                   , ("Cyan", "Monster Hive")
                   , ("Red", "Monster Hive")
                   , ("Blue", "Monster Hive") ]
  , playersAlly = [ ("Coral", "Amber")
                  , ("Green", "Yellow")
                  , ("Green", "Cyan")
                  , ("Yellow", "Cyan") ] }

playersDefense = Players
  { playersList = [ playerMonster { playerInitial = 1
                                  , playerAiLeader = False
                                  , playerUI = True }
                  , playerAntiHero { playerName = "Yellow"
                                   , playerInitial = 10 }
                  , playerAnimal ]
  , playersEnemy = [ ("Yellow", "Monster Hive")
                   , ("Yellow", "Animal Kingdom") ]
  , playersAlly = [("Monster Hive", "Animal Kingdom")] }

playerHero, playerAntiHero, playerMonster, playerAnimal, playerHorror :: Player

playerHero = Player
  { playerName = "Adventurer Party"
  , playerFaction = "hero"
  , playerSpawn = 0
  , playerEntry = toEnum (-1)
  , playerInitial = 3
  , playerAiLeader = False
  , playerUI = True
  }

playerAntiHero = playerHero
  { playerAiLeader = True
  , playerUI = False
  }

playerMonster = Player
  { playerName = "Monster Hive"
  , playerFaction = "monster"
  , playerSpawn = 66
  , playerEntry = toEnum (-3)
  , playerInitial = 5
  , playerAiLeader = True
  , playerUI = False
  }

playerAnimal = Player
  { playerName = "Animal Kingdom"
  , playerFaction = "animal"
  , playerSpawn = 33
  , playerEntry = toEnum (-2)
  , playerInitial = 3
  , playerAiLeader = True
  , playerUI = False
  }

playerHorror = Player
  { playerName = "Horror Den"
  , playerFaction = "horror"
  , playerSpawn = 0
  , playerEntry = toEnum (-1)
  , playerInitial = 0
  , playerAiLeader = True
  , playerUI = False
  }


cavesCampaign, cavesSkirmish, cavesAmbush, cavesBattle :: Caves

cavesCampaign = EM.fromList [ (toEnum (-1), ("caveRogue", Just True))
                            , (toEnum (-2), ("caveRogue", Nothing))
                            , (toEnum (-3), ("caveEmpty", Nothing))
                            , (toEnum (-10), ("caveNoise", Nothing))]

cavesSkirmish = EM.fromList [(toEnum (-3), ("caveSkirmish", Nothing))]

cavesAmbush = EM.fromList [(toEnum (-5), ("caveAmbush", Nothing))]

cavesBattle = EM.fromList [(toEnum (-3), ("caveBattle", Nothing))]
