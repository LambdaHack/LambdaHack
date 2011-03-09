module Actor where

data Actor = AHero Int     -- ^ hero serial number
           | AMonster Int  -- ^ offset in monster list
           | APlayer       -- ^ currently player-controlled hero
  deriving (Show, Eq)
