{-# LANGUAGE OverloadedStrings #-}
-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.)
-- for LambdaHack.
module Content.FactionKind ( cdefs ) where

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.FactionKind

cdefs :: ContentDef FactionKind
cdefs = ContentDef
  { getSymbol = fsymbol
  , getName = fname
  , getFreq = ffreq
  , validate = fvalidate
  , content =
      [hero, monster]
  }
hero,        monster :: FactionKind

hero = FactionKind
  { fsymbol   = '@'
  , fname     = "hero"
  , ffreq     = [("hero", 1), ("playable", 50)]
  , fAiLeader = "fullAbility"
  , fAiMember = "meleeAdjacent"
  , fspawn    = 0
  , fentry    = toEnum 1
  }

monster = FactionKind
  { fsymbol   = 'm'
  , fname     = "monster"
  , ffreq     = [("monster", 1), ("playable", 50)]
  , fAiLeader = "fullAbility"
  , fAiMember = "fullAbility"
  , fspawn    = 50
  , fentry    = toEnum 3  -- doesn't matter
  }
