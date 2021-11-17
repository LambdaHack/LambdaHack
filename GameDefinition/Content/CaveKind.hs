-- | Definitions of of cave kinds. Every level in the game is an instantiated
-- cave kind.
module Content.CaveKind
  ( -- * Group name patterns
    pattern CAVE_ROGUE, pattern CAVE_ARENA, pattern CAVE_SMOKING, pattern CAVE_LABORATORY, pattern CAVE_NOISE, pattern CAVE_MINE, pattern CAVE_EMPTY, pattern CAVE_SHALLOW_ROGUE, pattern CAVE_OUTERMOST, pattern CAVE_RAID, pattern CAVE_BRAWL, pattern CAVE_SHOOTOUT, pattern CAVE_HUNT, pattern CAVE_ESCAPE, pattern CAVE_ZOO, pattern CAVE_AMBUSH, pattern CAVE_BATTLE, pattern CAVE_SAFARI_1, pattern CAVE_SAFARI_2, pattern CAVE_SAFARI_3
  , groupNamesSingleton, groupNames
  , -- * Content
    content
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Ratio

import           Content.ItemKind hiding
  (content, groupNames, groupNamesSingleton)
import           Content.ItemKindActor
import           Content.PlaceKind hiding
  (content, groupNames, groupNamesSingleton)
import           Content.TileKind hiding
  (content, groupNames, groupNamesSingleton)
import           Game.LambdaHack.Content.CaveKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.TileKind
import           Game.LambdaHack.Core.Dice
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

-- * Group name patterns

groupNamesSingleton :: [GroupName CaveKind]
groupNamesSingleton = []

groupNames :: [GroupName CaveKind]
groupNames =
       [CAVE_ROGUE, CAVE_ARENA, CAVE_SMOKING, CAVE_LABORATORY, CAVE_NOISE, CAVE_MINE, CAVE_EMPTY, CAVE_SHALLOW_ROGUE, CAVE_OUTERMOST, CAVE_RAID, CAVE_BRAWL, CAVE_SHOOTOUT, CAVE_HUNT, CAVE_ESCAPE, CAVE_ZOO, CAVE_AMBUSH, CAVE_BATTLE, CAVE_SAFARI_1, CAVE_SAFARI_2, CAVE_SAFARI_3]

pattern CAVE_ROGUE, CAVE_ARENA, CAVE_SMOKING, CAVE_LABORATORY, CAVE_NOISE, CAVE_MINE, CAVE_EMPTY, CAVE_SHALLOW_ROGUE, CAVE_OUTERMOST, CAVE_RAID, CAVE_BRAWL, CAVE_SHOOTOUT, CAVE_HUNT, CAVE_ESCAPE, CAVE_ZOO, CAVE_AMBUSH, CAVE_BATTLE, CAVE_SAFARI_1, CAVE_SAFARI_2, CAVE_SAFARI_3 :: GroupName CaveKind

pattern CAVE_ROGUE = GroupName "caveRogue"
pattern CAVE_ARENA = GroupName "caveArena"
pattern CAVE_SMOKING = GroupName "caveSmoking"
pattern CAVE_LABORATORY = GroupName "caveLaboratory"
pattern CAVE_NOISE = GroupName "caveNoise"
pattern CAVE_MINE = GroupName "caveMine"
pattern CAVE_EMPTY = GroupName "caveEmpty"
pattern CAVE_SHALLOW_ROGUE = GroupName "caveShallowRogue"
pattern CAVE_OUTERMOST = GroupName "caveOutermost"
pattern CAVE_RAID = GroupName "caveRaid"
pattern CAVE_BRAWL = GroupName "caveBrawl"
pattern CAVE_SHOOTOUT = GroupName "caveShootout"
pattern CAVE_HUNT = GroupName "caveHunt"
pattern CAVE_ESCAPE = GroupName "caveEscape"
pattern CAVE_ZOO = GroupName "caveZoo"
pattern CAVE_AMBUSH = GroupName "caveAmbush"
pattern CAVE_BATTLE = GroupName "caveBattle"
pattern CAVE_SAFARI_1 = GroupName "caveSafari1"
pattern CAVE_SAFARI_2 = GroupName "caveSafari2"
pattern CAVE_SAFARI_3 = GroupName "caveSafari3"

-- * Content

content :: [CaveKind]
content =
  [rogue, arena, smoking, laboratory, noise, mine, empty, outermost, shallowRogue, raid, brawl, shootout, hunt, escape, zoo, ambush, battle, safari1, safari2, safari3]

rogue,    arena, smoking, laboratory, noise, mine, empty, outermost, shallowRogue, raid, brawl, shootout, hunt, escape, zoo, ambush, battle, safari1, safari2, safari3 :: CaveKind

-- * Underground caves; most of mediocre height and size

rogue = CaveKind
  { cname         = "A maze of twisty passages"
  , cfreq         = [(DEFAULT_RANDOM, 100), (CAVE_ROGUE, 1)]
  , cXminSize     = 80
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 4 + 10) 6
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5
  , cmaxPlaceSize = DiceXY 16 40
  , cdarkOdds     = 1 `d` 54 + 1 `dL` 20
      -- most rooms lit, to compensate for dark corridors
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%2
  , cmaxVoid      = 1%6
  , cdoorChance   = 3%4
  , copenChance   = 1%5
  , chidden       = 7
  , cactorCoeff   = 70  -- the maze requires time to explore
  , cactorFreq    = [(MONSTER, 60), (ANIMAL, 40)]
  , citemNum      = 6 `d` 5 + 10 - 10 `dL` 1
      -- deep down quality over quantity; generally quite random,
      -- for interesting replays at the cost of unreliable balance
  , citemFreq     = [(IK.COMMON_ITEM, 40), (IK.TREASURE, 60)]
  , cplaceFreq    = [(ROGUE, 1)]
  , cpassable     = False
  , clabyrinth    = False
  , cdefTile      = FILLER_WALL
  , cdarkCorTile  = FLOOR_CORRIDOR_DARK
  , clitCorTile   = FLOOR_CORRIDOR_LIT
  , cwallTile     = FILLER_WALL
  , ccornerTile   = FILLER_WALL
  , cfenceTileN   = S_BASIC_OUTER_FENCE
  , cfenceTileE   = S_BASIC_OUTER_FENCE
  , cfenceTileS   = S_BASIC_OUTER_FENCE
  , cfenceTileW   = S_BASIC_OUTER_FENCE
  , cfenceApart   = False
  , cminStairDist = 20
  , cmaxStairsNum = 1 + 1 `d` 2
  , cescapeFreq   = []
  , cstairFreq    = [ (WALLED_STAIRCASE, 50), (OPEN_STAIRCASE, 50)
                    , (TINY_STAIRCASE, 1) ]
  , cstairAllowed = []
  , cskip         = []
  , cinitSleep    = InitSleepPermitted
  , cdesc         = "Winding tunnels stretch into the dark."
  }  -- no lit corridors cave alternative, since both lit # and . look bad here
arena = rogue
  { cname         = "Dusty underground library"
  , cfreq         = [(DEFAULT_RANDOM, 60), (CAVE_ARENA, 1)]
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (3 `d` 3 + 17) (1 `d` 3 + 4)
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 6
  , cmaxPlaceSize = DiceXY 16 12
  , cdarkOdds     = 49 + 1 `d` 10  -- almost all rooms dark (1 in 10 lit)
  -- Light is not too deadly, because not many obstructions and so
  -- foes visible from far away and few foes have ranged combat
  -- at shallow depth.
  , cnightOdds    = 0  -- always day
  , cauxConnects  = 1
  , cmaxVoid      = 1%8
  , chidden       = 0
  , cactorCoeff   = 70  -- small open level, don't rush the player
  , cactorFreq    = [(MONSTER, 30), (ANIMAL, 70)]
  , citemNum      = 4 `d` 5  -- few rooms
  , citemFreq     = [ (IK.COMMON_ITEM, 20), (IK.TREASURE, 40)
                    , (IK.ANY_SCROLL, 40) ]
  , cplaceFreq    = [(ARENA, 1)]
  , cpassable     = True
  , cdefTile      = ARENA_SET_LIT
  , cdarkCorTile  = TRAIL_LIT  -- let trails give off light
  , clitCorTile   = TRAIL_LIT  -- may be rolled different than the above
  , cminStairDist = 15
  , cmaxStairsNum = 1 `d` 2
  , cstairFreq    = [ (WALLED_STAIRCASE, 20), (CLOSED_STAIRCASE, 80)
                    , (TINY_STAIRCASE, 1) ]
  , cinitSleep    = InitSleepAlways
  , cdesc         = "The shelves groan with dusty books and tattered scrolls. Subtle snoring can be heard from a distance."
  }
smoking = arena
  { cname         = "Smoking rooms"
  , cfreq         = [(CAVE_SMOKING, 1)]
  , cdarkOdds     = 41 + 1 `d` 10  -- almost all rooms lit (1 in 10 dark)
  -- Trails provide enough light for fun stealth.
  , cnightOdds    = 51  -- always night
  , citemNum      = 6 `d` 5  -- rare, so make it exciting
  , citemFreq     = [(IK.COMMON_ITEM, 20), (IK.TREASURE, 40), (IK.ANY_GLASS, 40)]
  , cdefTile      = ARENA_SET_DARK
  , cdesc         = "Velvet couches exude the strong smell of tobacco."
  }
laboratory = rogue
  { cname         = "Burnt laboratory"
  , cfreq         = [(CAVE_LABORATORY, 1)]
  , cXminSize     = 60
  , cYminSize     = 21
  , ccellSize     = DiceXY (1 `d` 2 + 5) 6
  , cminPlaceSize = DiceXY 7 5
  , cmaxPlaceSize = DiceXY 10 40
  , cnightOdds    = 0  -- always day so that the corridor smoke is lit
  , cauxConnects  = 1%5
  , cmaxVoid      = 1%10
  , cdoorChance   = 1
  , copenChance   = 1%2
  , cactorFreq    = [(MONSTER, 30), (ANIMAL, 70)]
  , citemNum      = 6 `d` 5  -- reward difficulty
  , citemFreq     = [ (IK.COMMON_ITEM, 20), (IK.TREASURE, 40)
                    , (IK.EXPLOSIVE, 40) ]
  , cplaceFreq    = [(LABORATORY, 1)]
  , cdarkCorTile  = LAB_TRAIL_LIT  -- let lab smoke give off light always
  , clitCorTile   = LAB_TRAIL_LIT
  , cmaxStairsNum = 2
  , cstairFreq    = [ (WALLED_STAIRCASE, 50), (OPEN_STAIRCASE, 50)
                    , (TINY_STAIRCASE, 1) ]
  , cdesc         = "Shattered glassware and the sharp scent of spilt chemicals show that something terrible happened here."
  }
noise = rogue
  { cname         = "Leaky burrowed sediment"
  , cfreq         = [(DEFAULT_RANDOM, 30), (CAVE_NOISE, 1)]
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (3 `d` 5 + 12) 6
  , cminPlaceSize = DiceXY 8 5
  , cmaxPlaceSize = DiceXY 20 20
  , cdarkOdds     = 51
  -- Light is deadly, because nowhere to hide and pillars enable spawning
  -- very close to heroes.
  , cnightOdds    = 0  -- harder variant, but looks cheerful
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%100
  , cdoorChance   = 1  -- to avoid lit quasi-door tiles
  , chidden       = 0
  , cactorCoeff   = 100  -- the maze requires time to explore; also, small
  , cactorFreq    = [(MONSTER, 80), (ANIMAL, 20)]
  , citemNum      = 6 `d` 5  -- an incentive to explore the labyrinth
  , cpassable     = True
  , cplaceFreq    = [(NOISE, 1)]
  , clabyrinth    = True
  , cdefTile      = NOISE_SET_LIT
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cdarkCorTile  = DAMP_FLOOR_DARK
  , clitCorTile   = DAMP_FLOOR_LIT
  , cminStairDist = 15
  , cstairFreq    = [ (CLOSED_STAIRCASE, 50), (OPEN_STAIRCASE, 50)
                    , (TINY_STAIRCASE, 1) ]
  , cinitSleep    = InitSleepBanned
  , cdesc         = "Soon, these passages will be swallowed up by the mud."
  }
mine = noise
  { cname         = "Frozen derelict mine"
  , cfreq         = [(CAVE_MINE, 1)]
  , cnightOdds    = 51  -- easier variant, but looks sinister
  , citemNum      = 10 `d` 4  -- an incentive to explore the final labyrinth
  , citemFreq     = [(IK.COMMON_ITEM, 20), (GEM, 20)]
                      -- can't be "valuable" or template items generated
  , cplaceFreq    = [(NOISE, 1), (MINE, 99)]
  , clabyrinth    = True
  , cdefTile      = POWER_SET_DARK
  , cstairFreq    = [ (GATED_CLOSED_STAIRCASE, 50)
                    , (GATED_OPEN_STAIRCASE, 50)
                    , (GATED_TINY_STAIRCASE, 1) ]
  , cinitSleep    = InitSleepBanned
  , cdesc         = "Pillars of shining ice create a frozen labyrinth."
  }
empty = rogue
  { cname         = "Tall cavern"
  , cfreq         = [(CAVE_EMPTY, 1)]
  , ccellSize     = DiceXY (2 `d` 2 + 11) (1 `d` 2 + 8)
  , cminPlaceSize = DiceXY 13 11
  , cmaxPlaceSize = DiceXY 37 31  -- favour large rooms
  , cdarkOdds     = 1 `d` 100 + 1 `dL` 100
  , cnightOdds    = 0  -- always day
  , cauxConnects  = 3%2
  , cmaxVoid      = 0  -- too few rooms to have void and fog common anyway
  , cdoorChance   = 0
  , copenChance   = 0
  , chidden       = 0
  , cactorCoeff   = 8
  , cactorFreq    = [(ANIMAL, 10), (IMMOBILE_ANIMAL, 90)]
      -- The healing geysers on lvl 3 act like HP resets. Needed to avoid
      -- cascading failure, if the particular starting conditions were
      -- very hard. Items are not reset, even if they are bad, which provides
      -- enough of a continuity. Gyesers on lvl 3 are not OP and can't be
      -- abused, because they spawn less and less often and also HP doesn't
      -- effectively accumulate over max.
  , citemNum      = 4 `d` 5  -- few rooms and geysers are the boon
  , cplaceFreq    = [(EMPTY, 1)]
  , cpassable     = True
  , cdefTile      = EMPTY_SET_LIT
  , cdarkCorTile  = FLOOR_ARENA_DARK
  , clitCorTile   = FLOOR_ARENA_LIT
  , cminStairDist = 30
  , cmaxStairsNum = 1
  , cstairFreq    = [ (WALLED_STAIRCASE, 20), (CLOSED_STAIRCASE, 80)
                    , (TINY_STAIRCASE, 1) ]
  , cdesc         = "Swirls of warm fog fill the air, the hiss of geysers sounding all around."
  }
outermost = shallowRogue
  { cname         = "Cave entrance"
  , cfreq         = [(CAVE_OUTERMOST, 100)]
  , cXminSize     = 40
  , cYminSize     = 21
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cactorCoeff   = 100  -- already animals start there; also, pity on the noob
  , cactorFreq    = filter ((/= MONSTER) . fst) $ cactorFreq rogue
  , citemNum      = 6 `d` 5  -- lure them in with loot
  , citemFreq     = filter ((/= IK.TREASURE) . fst) $ citemFreq rogue
  , cminStairDist = 10
  , cmaxStairsNum = 1
  , cescapeFreq   = [(INDOOR_ESCAPE_UP, 1)]
  , cdesc         = "This close to the surface, the sunlight still illuminates the dungeon."
  }
shallowRogue = rogue
  { cfreq         = [(CAVE_SHALLOW_ROGUE, 100)]
  , cXminSize     = 60
  , cYminSize     = 21
  , cmaxStairsNum = 1  -- ensure heroes meet initial monsters and their loot
  , cdesc         = "The snorts and grunts of savage beasts can be clearly heard."
  }

-- * Overground "caves"; no story-wise limits wrt height and size

raid = rogue
  { cname         = "Typing den"
  , cfreq         = [(CAVE_RAID, 1)]
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 4 + 6) 6
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5
  , cmaxPlaceSize = DiceXY 16 20
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cmaxVoid      = 1%10
  , cdoorChance   = 1  -- make sure enemies not seen on turn 1
  , copenChance   = 0  -- make sure enemies not seen on turn 1
  , cactorCoeff   = 300  -- deep level with no kit, so slow spawning
  , cactorFreq    = [(ANIMAL, 100)]
  , citemNum      = 6 `d` 6  -- just one level, hard enemies, treasure
  , citemFreq     = [ (IK.COMMON_ITEM, 100), (IK.S_CURRENCY, 500)
                    , (STARTING_WEAPON, 100) ]
  , cmaxStairsNum = 0
  , cescapeFreq   = [(INDOOR_ESCAPE_UP, 1)]
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Mold spreads across the walls and scuttling sounds can be heard in the distance."
  }
brawl = rogue  -- many random solid tiles, to break LOS, since it's a day
               -- and this scenario is not focused on ranged combat;
               -- also, sanctuaries against missiles in shadow under trees
  { cname         = "Sunny woodland"
  , cfreq         = [(CAVE_BRAWL, 1)]
  , cXminSize     = 60
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 5 + 5) 6
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkOdds     = 51
  , cnightOdds    = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 4 `d` 6
  , citemFreq     = [ (IK.COMMON_ITEM, 50), (STARTING_WEAPON, 100)
                    , (STARTING_ARMOR, 100) ]
  , cplaceFreq    = [(BRAWL, 1)]
  , cpassable     = True
  , cdefTile      = BRAWL_SET_LIT
  , cdarkCorTile  = DIRT_LIT
  , clitCorTile   = DIRT_LIT
  , cstairFreq    = []
  , cfenceTileN   = OUTDOOR_OUTER_FENCE
  , cfenceTileE   = OUTDOOR_OUTER_FENCE
  , cfenceTileS   = OUTDOOR_OUTER_FENCE
  , cfenceTileW   = OUTDOOR_OUTER_FENCE
  , cmaxStairsNum = 0
  , cdesc         = "Sunlight falls through the trees and dapples on the ground."
  }
shootout = rogue  -- a scenario with strong missiles;
                  -- few solid tiles, but only translucent tiles or walkable
                  -- opaque tiles, to make scouting and sniping more interesting
                  -- and to avoid obstructing view too much, since this
                  -- scenario is about ranged combat at long range
  { cname         = "Misty meadow"
  , cfreq         = [(CAVE_SHOOTOUT, 1)]
  , ccellSize     = DiceXY (1 `d` 2 + 6) 6
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 4 4
  , cdarkOdds     = 51
  , cnightOdds    = 0
  , cauxConnects  = 1%10
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 16
                      -- less items in inventory, more to be picked up,
                      -- to reward explorer and aggressor and punish camper
  , citemFreq     = [ (IK.COMMON_ITEM, 30)
                    , (ANY_ARROW, 400), (HARPOON, 300), (IK.EXPLOSIVE, 50) ]
                      -- Many consumable buffs are needed in symmetric maps
                      -- so that aggressor prepares them in advance and camper
                      -- needs to waste initial turns to buff for the defence.
  , cplaceFreq    = [(SHOOTOUT, 1)]
  , cpassable     = True
  , cdefTile      = SHOOTOUT_SET_LIT
  , cdarkCorTile  = DIRT_LIT
  , clitCorTile   = DIRT_LIT
  , cstairFreq    = []
  , cfenceTileN   = OUTDOOR_OUTER_FENCE
  , cfenceTileE   = OUTDOOR_OUTER_FENCE
  , cfenceTileS   = OUTDOOR_OUTER_FENCE
  , cfenceTileW   = OUTDOOR_OUTER_FENCE
  , cmaxStairsNum = 0
  , cdesc         = "The warmth has released fog and the wind brooms it away."
  }
hunt = rogue  -- a scenario with strong missiles for ranged and shade for melee
  { cname         = "Afternoon swamp"
  , cfreq         = [(CAVE_HUNT, 1)]
  , ccellSize     = DiceXY (1 `d` 2 + 6) 6
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 4 4
  , cdarkOdds     = 51
  , cnightOdds    = 0
  , cauxConnects  = 1%10
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 0
  , cactorCoeff   = 400  -- spawn slowly
  , cactorFreq    = [(INSECT, 100)]
  , citemNum      = 5 `d` 10
  , citemFreq     = [ (IK.COMMON_ITEM, 30)
                    , (ANY_ARROW, 400), (HARPOON, 300), (IK.EXPLOSIVE, 50) ]
  , cplaceFreq    = [(BRAWL, 50), (SHOOTOUT, 100)]
  , cpassable     = True
  , cdefTile      = SHOOTOUT_SET_LIT
  , cdarkCorTile  = DIRT_LIT
  , clitCorTile   = DIRT_LIT
  , cstairFreq    = []
  , cfenceTileN   = OUTDOOR_OUTER_FENCE
  , cfenceTileE   = OUTDOOR_OUTER_FENCE
  , cfenceTileS   = OUTDOOR_OUTER_FENCE
  , cfenceTileW   = OUTDOOR_OUTER_FENCE
  , cmaxStairsNum = 0
  , cdesc         = "Tired after the day's heat, the insects gather strength in their hiding places."
  }
escape = rogue  -- a scenario with weak missiles, because heroes don't depend
                -- on them; dark, so solid obstacles are to hide from missiles,
                -- not view; obstacles are not lit, to frustrate the AI;
                -- lots of small lights to cross, to have some risks
  { cname         = "Metropolitan park at dusk"  -- "night" didn't fit
  , cfreq         = [(CAVE_ESCAPE, 1)]
  , ccellSize     = DiceXY (1 `d` 3 + 7) 6
  , cminPlaceSize = DiceXY 5 3
  , cmaxPlaceSize = DiceXY 9 9  -- bias towards larger lamp areas
  , cdarkOdds     = 51  -- rooms always dark so that fence not visible from afar
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 2  -- many lit trails, so easy to aim
  , cmaxVoid      = 1%100
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 6 `d` 8
  , citemFreq     = [ (IK.COMMON_ITEM, 30), (GEM, 500)
                    , (WEAK_ARROW, 500), (HARPOON, 400)
                    , (IK.EXPLOSIVE, 100) ]
  , cplaceFreq    = [(ESCAPE, 1)]
  , cpassable     = True
  , cdefTile      = ESCAPE_SET_DARK  -- unlike in ambush, tiles not burning yet
  , cdarkCorTile  = SAFE_TRAIL_LIT  -- let trails give off light
  , clitCorTile   = SAFE_TRAIL_LIT
  , cfenceTileN   = OUTDOOR_OUTER_FENCE
  , cfenceTileE   = OUTDOOR_OUTER_FENCE
  , cfenceTileS   = OUTDOOR_OUTER_FENCE
  , cfenceTileW   = OUTDOOR_OUTER_FENCE
  , cmaxStairsNum = 0
  , cescapeFreq   = [(OUTDOOR_ESCAPE_DOWN, 1)]
  , cstairFreq    = []
  , cskip         = []
  , cdesc         = "The darkening greyness is settling into silence."
  }
zoo = rogue  -- few lights and many solids, to help the less numerous heroes
  { cname         = "Menagerie in flames"
  , cfreq         = [(CAVE_ZOO, 1)]
  , ccellSize     = DiceXY (1 `d` 3 + 7) 6
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 12 5
  , cdarkOdds     = 51  -- rooms always dark so that fence not visible from afar
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 7%10
  , copenChance   = 9%10
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 7 `d` 8
  , citemFreq     = [ (IK.COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 1000)
                    , (STARTING_WEAPON, 1000) ]
  , cplaceFreq    = [(ZOO, 1)]
  , cpassable     = True
  , cdefTile      = ZOO_SET_DARK
  , cdarkCorTile  = SAFE_TRAIL_LIT  -- let trails give off light
  , clitCorTile   = SAFE_TRAIL_LIT
  , cstairFreq    = []
  , cfenceTileN   = OUTDOOR_OUTER_FENCE
  , cfenceTileE   = OUTDOOR_OUTER_FENCE
  , cfenceTileS   = OUTDOOR_OUTER_FENCE
  , cfenceTileW   = OUTDOOR_OUTER_FENCE
  , cmaxStairsNum = 0
  , cdesc         = "The night is filled with animal calls."
  }
ambush = rogue  -- a scenario with strong missiles;
                -- dark, so solid obstacles are to hide from missiles,
                -- not view, and they are all lit, because stopped missiles
                -- are frustrating, while a few LOS-only obstacles are not lit;
                -- few small lights to cross, giving a chance to snipe;
                -- crucial difference wrt shootout and hunt is that trajectories
                -- of missiles are usually not seen, so enemy can't be guessed;
                -- camping doesn't pay off, because enemies can sneak and only
                -- active scouting, throwing flares and shooting discovers them
  { cname         = "Burning metropolitan park"
  , cfreq         = [(CAVE_AMBUSH, 1)]
  , ccellSize     = DiceXY (1 `d` 4 + 7) 6
  , cminPlaceSize = DiceXY 5 3
  , cmaxPlaceSize = DiceXY 9 9  -- bias towards larger lamp areas
  , cdarkOdds     = 51  -- rooms always dark so that fence not visible from afar
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%10  -- few lit trails, so hard to aim
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 8
  , citemFreq     = [ (IK.COMMON_ITEM, 30)
                    , (ANY_ARROW, 400), (HARPOON, 300), (IK.EXPLOSIVE, 50) ]
  , cplaceFreq    = [(AMBUSH, 1)]
  , cpassable     = True
  , cdefTile      = AMBUSH_SET_DARK
  , cdarkCorTile  = TRAIL_LIT  -- let trails give off light
  , clitCorTile   = TRAIL_LIT
  , cstairFreq    = []
  , cfenceTileN   = OUTDOOR_OUTER_FENCE
  , cfenceTileE   = OUTDOOR_OUTER_FENCE
  , cfenceTileS   = OUTDOOR_OUTER_FENCE
  , cfenceTileW   = OUTDOOR_OUTER_FENCE
  , cmaxStairsNum = 0
  , cdesc         = "Fires have reached into the city, glowing in darkness."
  }

-- * Other caves; testing, Easter egg, future work

battle = rogue  -- few lights and many solids, to help the less numerous heroes
  { cname         = "Old battle ground"
  , cfreq         = [(CAVE_BATTLE, 1)]
  , ccellSize     = DiceXY (5 `d` 3 + 11) 5  -- cfenceApart results in 2 rows
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 9 7
  , cdarkOdds     = 0
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 2%10
  , copenChance   = 9%10
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 8
  , citemFreq     = [(IK.COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 200)]
  , cplaceFreq    = [(BATTLE, 50), (ROGUE, 50)]
  , cpassable     = True
  , cdefTile      = BATTLE_SET_DARK
  , cdarkCorTile  = SAFE_TRAIL_LIT  -- let trails give off light
  , clitCorTile   = SAFE_TRAIL_LIT
  , cfenceTileN   = OUTDOOR_OUTER_FENCE
  , cfenceTileE   = OUTDOOR_OUTER_FENCE
  , cfenceTileS   = OUTDOOR_OUTER_FENCE
  , cfenceTileW   = OUTDOOR_OUTER_FENCE
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cmaxStairsNum = 0
  , cstairFreq    = []
  , cdesc         = "Eroded walls, rusted weapons and unidentifiable bones cruch underfoot all alike."
  }
safari1 = brawl
  { cname         = "Hunam habitat"
  , cfreq         = [(CAVE_SAFARI_1, 1)]
  , cminPlaceSize = DiceXY 5 3
  , cmaxStairsNum = 1
  , cstairFreq    = [ (OUTDOOR_WALLED_STAIRCASE, 20)
                    , (OUTDOOR_CLOSED_STAIRCASE, 80)
                    , (OUTDOOR_TINY_STAIRCASE, 1) ]
  , cskip         = [0]
  , cdesc         = "\"Act 1. Hunams scavenge in a forest in their usual disgusting way.\""
  }
safari2 = escape  -- lamps instead of trees, but ok, it's only a simulation
  { cname         = "Deep into the jungle"
  , cfreq         = [(CAVE_SAFARI_2, 1)]
  , cmaxStairsNum = 1
  , cescapeFreq   = []
  , cstairFreq    = [ (OUTDOOR_WALLED_STAIRCASE, 20)
                    , (OUTDOOR_CLOSED_STAIRCASE, 80)
                    , (OUTDOOR_TINY_STAIRCASE, 1) ]
  , cskip         = [0]
  , cdesc         = "\"Act 2. In the dark pure heart of the jungle noble animals roam freely.\""
  }
safari3 = zoo  -- glass rooms, but ok, it's only a simulation
  { cname         = "Jungle in flames"
  , cfreq         = [(CAVE_SAFARI_3, 1)]
  , cminPlaceSize = DiceXY 5 4
  , cescapeFreq   = [(OUTDOOR_ESCAPE_DOWN, 1)]
  , cmaxStairsNum = 1
  , cstairFreq    = [ (OUTDOOR_WALLED_STAIRCASE, 20)
                    , (OUTDOOR_CLOSED_STAIRCASE, 80)
                    , (OUTDOOR_TINY_STAIRCASE, 1) ]
  , cdesc         = "\"Act 3. Jealous hunams set jungle on fire and flee.\""
  }
