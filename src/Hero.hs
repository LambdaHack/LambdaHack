module Hero where

import Geometry
import qualified Config
import Movable

-- TODO: move more hero functionis here; perhaps addHero from Dungeon, etc.

-- | Hit points of the hero. Experimentally balanced for multiple heroes.
heroHP :: Config.CP -> Int
heroHP config =
  let b = Config.get config "heroes" "baseHp"
      k = Config.get config "heroes" "extraHeroes"
  in  k + b `div` (k + 1)

-- | Initial hero.
defaultHero :: Char -> String -> Loc -> Int -> Movable
defaultHero symbol name ploc hp =
  Movable (Hero symbol name) hp hp Nothing TCursor ploc [] 'a' 10 0
