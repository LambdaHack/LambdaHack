{-# LANGUAGE OverloadedStrings #-}
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
  , validate = mvalidate
  , content =
      [campaign, skirmish, pvp, coop, defense]
  }
campaign,        skirmish, pvp, coop, defense :: ModeKind

campaign = ModeKind
  { msymbol  = 'r'  -- matches the keypress (with C-)
  , mname    = "campaign"
  , mfreq    = [("campaign", 1)]
  , mplayers = playersCampaign
  , mcaves   = cavesCampaign
  }

skirmish = ModeKind
  { msymbol  = 's'
  , mname    = "skirmish"
  , mfreq    = [("skirmish", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesCombat
  }

pvp = ModeKind
  { msymbol  = 'p'
  , mname    = "PvP"
  , mfreq    = [("PvP", 1)]
  , mplayers = playersPvP
  , mcaves   = cavesCombat
  }

coop = ModeKind
  { msymbol  = 'c'
  , mname    = "Coop"
  , mfreq    = [("Coop", 1)]
  , mplayers = playersCoop
  , mcaves   = cavesCampaign
  }

defense = ModeKind
  { msymbol  = 'd'
  , mname    = "defense"
  , mfreq    = [("defense", 1)]
  , mplayers = playersDefense
  , mcaves   = cavesDefense
  }

playersCampaign, playersSkirmish, playersPvP, playersCoop, playersDefense :: Players

playersCampaign = Players
  { playersHuman = [Player {playerName = "Adventuring Party", playerKind = "hero", playerInitial = 1, playerEntry = toEnum 1}]
  , playersComputer = [Player {playerName = "Monster Hive", playerKind = "monster", playerInitial = 5, playerEntry = toEnum 3}]
  , playersEnemy = [("Adventuring Party", "Monster Hive")]
  , playersAlly = [] }

playersSkirmish = Players
  { playersHuman = [Player {playerName = "White", playerKind = "hero", playerInitial = 3, playerEntry = toEnum 1}]
  , playersComputer = [Player {playerName = "Green", playerKind = "hero", playerInitial = 3, playerEntry = toEnum 1}, Player {playerName = "Horror Den", playerKind = "horror", playerInitial = 0, playerEntry = toEnum 1}]
  , playersEnemy = [ ("White", "Green")
                   , ("White", "Horror Den")
                   , ("Green", "Horror Den") ]
  , playersAlly = [] }

playersPvP = Players
  { playersHuman = [Player {playerName = "Red", playerKind = "hero", playerInitial = 3, playerEntry = toEnum 1}, Player {playerName = "Blue", playerKind = "hero", playerInitial = 3, playerEntry = toEnum 1}]
  , playersComputer = [Player {playerName = "Horror Den", playerKind = "horror", playerInitial = 0, playerEntry = toEnum 1}]
  , playersEnemy = [ ("Red", "Blue")
                   , ("Red", "Horror Den")
                   , ("Blue", "Horror Den") ]
  , playersAlly = [] }

playersCoop = Players
  { playersHuman = [Player {playerName = "Coral", playerKind = "hero", playerInitial = 1, playerEntry = toEnum 1}, Player {playerName = "Amber", playerKind = "hero", playerInitial = 1, playerEntry = toEnum 1}]
  , playersComputer = [Player {playerName = "Monster Hive", playerKind = "monster", playerInitial = 5, playerEntry = toEnum 3}]
  , playersEnemy = [ ("Coral", "Monster Hive")
                   , ("Amber", "Monster Hive") ]
  , playersAlly = [("Coral", "Amber")] }

playersDefense = Players
  { playersHuman = [Player {playerName = "Monster Hive", playerKind = "monster", playerInitial = 1, playerEntry = toEnum 1}]
  , playersComputer = [Player {playerName = "Green", playerKind = "hero", playerInitial = 1, playerEntry = toEnum 1}, Player {playerName = "Yellow", playerKind = "hero", playerInitial = 2, playerEntry = toEnum 1}, Player {playerName = "Cyan", playerKind = "hero", playerInitial = 3, playerEntry = toEnum 1}]
  , playersEnemy = [ ("Green", "Monster Hive")
                   , ("Yellow", "Monster Hive")
                   , ("Cyan", "Monster Hive") ]
  , playersAlly = [ ("Green", "Yellow")
                  , ("Green", "Cyan")
                  , ("Yellow", "Cyan") ] }

cavesCampaign, cavesCombat, cavesDefense :: Caves

-- Cave "dng" means a random choice from caves that can randomly appear;
-- this is the default and the lack of the Escape feature is the default.

cavesCampaign = EM.fromList [ (toEnum 1, ("caveRogue", True))
                            , (toEnum 2, ("caveRogue", False))
                            , (toEnum 3, ("caveEmpty", False))
                            , (toEnum 10, ("caveNoise", False))]

cavesCombat = EM.fromList [(toEnum 3, ("caveCombat", False))]

cavesDefense = EM.fromList [ (toEnum 1, ("dng", False))
                           , (toEnum 5, ("caveEmpty", True))]
