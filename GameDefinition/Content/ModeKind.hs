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
      [campaign, skirmish, battle, pvp, coop, defense, testCampaign, testSkirmish, testBattle, testPvP, testCoop, testDefense, peekCampaign, peekSkirmish]
  }
campaign,        skirmish, battle, pvp, coop, defense, testCampaign, testSkirmish, testBattle, testPvP, testCoop, testDefense, peekCampaign, peekSkirmish :: ModeKind

campaign = ModeKind
  { msymbol  = 'r'
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
  , mcaves   = cavesCombat
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
  , mcaves   = cavesCombat
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

testCampaign = ModeKind
  { msymbol  = 't'
  , mname    = "testCampaign"
  , mfreq    = [("testCampaign", 1)]
  , mplayers = playersTestCampaign
  , mcaves   = cavesCampaign
  }

testSkirmish = ModeKind
  { msymbol  = 't'
  , mname    = "testSkirmish"
  , mfreq    = [("testSkirmish", 1)]
  , mplayers = playersTestSkirmish
  , mcaves   = cavesCombat
  }

testBattle = ModeKind
  { msymbol  = 't'
  , mname    = "testBattle"
  , mfreq    = [("testBattle", 1)]
  , mplayers = playersTestBattle
  , mcaves   = cavesBattle
  }

testPvP = ModeKind
  { msymbol  = 't'
  , mname    = "testPvP"
  , mfreq    = [("testPvP", 1)]
  , mplayers = playersTestPvP
  , mcaves   = cavesCombat
  }

testCoop = ModeKind
  { msymbol  = 't'
  , mname    = "testCoop"
  , mfreq    = [("testCoop", 1)]
  , mplayers = playersTestCoop
  , mcaves   = cavesCampaign
  }

testDefense = ModeKind
  { msymbol  = 't'
  , mname    = "testDefense"
  , mfreq    = [("testDefense", 1)]
  , mplayers = playersTestDefense
  , mcaves   = cavesCampaign
  }

peekCampaign = ModeKind
  { msymbol  = 'p'
  , mname    = "peekCampaign"
  , mfreq    = [("peekCampaign", 1)]
  , mplayers = playersPeekCampaign
  , mcaves   = cavesCampaign
  }

peekSkirmish = ModeKind
  { msymbol  = 'p'
  , mname    = "peekSkirmish"
  , mfreq    = [("peekSkirmish", 1)]
  , mplayers = playersPeekSkirmish
  , mcaves   = cavesCombat
  }


playersCampaign, playersSkirmish, playersBattle, playersPvP, playersCoop, playersDefense, playersTestCampaign, playersTestSkirmish, playersTestBattle, playersTestPvP, playersTestCoop, playersTestDefense, playersPeekCampaign, playersPeekSkirmish :: Players

playersCampaign = Players
  { playersList = [ playerHero {playerInitial = 1}
                  , playerMonster ]
  , playersEnemy = [("Adventurer Party", "Monster Hive")]
  , playersAlly = [] }

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
                  , playerMonster { playerInitial = 30
                                  , playerSpawn = 0 } ]
  , playersEnemy = [("Adventurer Party", "Monster Hive")]
  , playersAlly = [] }

playersPvP = Players
  { playersList = [ playerHero {playerName = "Red"}
                  , playerHero {playerName = "Blue"}
                  , playerHorror ]
  , playersEnemy = [ ("Red", "Blue")
                   , ("Red", "Horror Den")
                   , ("Blue", "Horror Den") ]
  , playersAlly = [] }

playersCoop = Players
  { playersList = [ playerHero { playerName = "Coral"
                               , playerInitial = 1 }
                  , playerHero { playerName = "Amber"
                               , playerInitial = 1 }
                  , playerMonster ]
  , playersEnemy = [ ("Coral", "Monster Hive")
                   , ("Amber", "Monster Hive") ]
  , playersAlly = [("Coral", "Amber")] }

playersDefense = Players
  { playersList = [ playerMonster { playerInitial = 1
                                  , playerAiLeader = False
                                  , playerUI = True }
                  , playerAntiHero {playerName = "Green"}
                  , playerAntiHero {playerName = "Yellow"}
                  , playerAntiHero {playerName = "Cyan"} ]
  , playersEnemy = [ ("Green", "Monster Hive")
                   , ("Yellow", "Monster Hive")
                   , ("Cyan", "Monster Hive") ]
  , playersAlly = [ ("Green", "Yellow")
                  , ("Green", "Cyan")
                  , ("Yellow", "Cyan") ] }

playersTestCampaign = playersCampaign
  { playersList = [ playerHero { playerInitial = 5
                               , playerAiLeader = True }
                  , playerMonster ] }

playersTestSkirmish = playersSkirmish
  { playersList = [ playerHero { playerName = "White"
                               , playerAiLeader = True }
                  , playerAntiHero { playerName = "Purple" }
                  , playerHorror ] }

playersTestBattle = playersBattle
  { playersList = [ playerHero { playerInitial = 5
                               , playerAiLeader = True }
                  , playerMonster { playerInitial = 30
                                  , playerSpawn = 0 } ] }

playersTestPvP = playersPvP
  { playersList = [ playerHero { playerName = "Red"
                               , playerAiLeader = True }
                  , playerHero { playerName = "Blue"
                               , playerAiLeader = True }
                  , playerHorror ] }

playersTestCoop = playersCoop
  { playersList = [ playerHero { playerName = "Coral"
                               , playerAiLeader = True }
                  , playerHero { playerName = "Amber"
                               , playerAiLeader = True }
                  , playerMonster ] }

playersTestDefense = playersDefense
  { playersList = [ playerMonster { playerInitial = 1
                                  , playerUI = True }
                  , playerAntiHero {playerName = "Green"}
                  , playerAntiHero {playerName = "Yellow"}
                  , playerAntiHero {playerName = "Cyan"} ] }

playersPeekCampaign = playersCampaign
  { playersList = [ playerHero {playerInitial = 1}
                  , playerMonster {playerUI = True} ] }

playersPeekSkirmish = playersSkirmish
  { playersList = [ playerHero {playerName = "White"}
                  , playerAntiHero { playerName = "Purple"
                                   , playerUI = True }
                  , playerHorror ] }


playerHero, playerAntiHero, playerMonster, playerHorror :: Player

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
  , playerSpawn = 50
  , playerEntry = toEnum (-3)
  , playerInitial = 5
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


cavesCampaign, cavesCombat, cavesBattle :: Caves

cavesCampaign = EM.fromList [ (toEnum (-1), ("caveRogue", Just True))
                            , (toEnum (-2), ("caveRogue", Nothing))
                            , (toEnum (-3), ("caveEmpty", Nothing))
                            , (toEnum (-10), ("caveNoise", Nothing))]

cavesCombat = EM.fromList [(toEnum (-3), ("caveCombat", Nothing))]

cavesBattle = EM.fromList [(toEnum (-3), ("caveBattle", Nothing))]
