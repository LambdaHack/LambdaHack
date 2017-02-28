-- | Terrain tile definitions.
module Content.TileKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.TileKind

cdefs :: ContentDef TileKind
cdefs = ContentDef
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validateSingle = validateSingleTileKind
  , validateAll = validateAllTileKind
  , content = contentFromList $
      [unknown, wall, hardRock, pillar, pillarIce, pulpit, pillarCache, lampPost, signboardUnread, signboardRead, bush, bushDark, bushBurnt, bushBurning, tree, treeDark, treeBurnt, treeBurning, wallV, wallGlassV, wallGlassVSpice, wallSuspectV, wallObscuredDefacedV, wallObscuredFrescoedV, doorTrappedV, doorClosedV, doorOpenV, wallH, wallGlassH, wallGlassHSpice, wallSuspectH, wallObscuredH, doorTrappedH, doorClosedH, doorOpenH, stairsUp, stairsTaintedUp, stairsOutdoorUp, stairsDown, stairsTaintedDown, stairsOutdoorDown, escapeUp, escapeDown, escapeOutdoorDown, rubble, floorCorridorLit, floorArenaLit, floorNoiseLit, floorDirtLit, floorDirtSpiceLit, floorArenaShade, floorActorLit, floorItemLit, floorActorItemLit, floorRedLit, floorBlueLit, floorGreenLit, floorBrownLit, floorFog, floorFogDark, floorSmoke, floorSmokeDark]
      ++ map makeDark ldarkable
      ++ map makeDarkColor ldarkColorable
  }
unknown,        wall, hardRock, pillar, pillarIce, pulpit, pillarCache, lampPost, signboardUnread, signboardRead, bush, bushDark, bushBurnt, bushBurning, tree, treeDark, treeBurnt, treeBurning, wallV, wallGlassV, wallGlassVSpice, wallSuspectV, wallObscuredDefacedV, wallObscuredFrescoedV, doorTrappedV, doorClosedV, doorOpenV, wallH, wallGlassH, wallGlassHSpice, wallSuspectH, wallObscuredH, doorTrappedH, doorClosedH, doorOpenH, stairsUp, stairsTaintedUp, stairsOutdoorUp, stairsDown, stairsTaintedDown, stairsOutdoorDown, escapeUp, escapeDown, escapeOutdoorDown, rubble, floorCorridorLit, floorArenaLit, floorNoiseLit, floorDirtLit, floorDirtSpiceLit, floorArenaShade, floorActorLit, floorItemLit, floorActorItemLit, floorRedLit, floorBlueLit, floorGreenLit, floorBrownLit, floorFog, floorFogDark, floorSmoke, floorSmokeDark :: TileKind

ldarkable :: [TileKind]
ldarkable = [wallV, wallSuspectV, wallObscuredDefacedV, wallObscuredFrescoedV, doorClosedV, doorTrappedV, doorOpenV, wallH, wallSuspectH, wallObscuredH, doorClosedH, doorTrappedH, doorOpenH, rubble, floorCorridorLit]

ldarkColorable :: [TileKind]
ldarkColorable = [floorArenaLit, floorNoiseLit, floorDirtLit, floorActorLit, floorItemLit, floorActorItemLit]

-- Note that for AI hints and UI comfort, most multiple-use @Embed@ tiles
-- should have a variant, which after first use transforms into a different
-- colour tile without @ChangeTo@ and similar (which then AI no longer touches).
-- If a tile is supposed to be repeatedly activated by AI (e.g., cache),
-- it should keep @ChangeTo@ for the whole time.

unknown = TileKind  -- needs to have index 0 and alter 1
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 1
  , tfeature = [Dark, Indistinct]
  }
wall = TileKind
  { tsymbol  = ' '
  , tname    = "bedrock"
  , tfreq    = [("fillerWall", 1), ("legendLit", 100), ("legendDark", 100)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [Dark, Indistinct]
      -- Bedrock being dark is bad for AI (forces it to backtrack to explore
      -- bedrock at corridor turns) and induces human micromanagement
      -- if there can be corridors joined diagonally (humans have to check
      -- with the xhair if the dark space is bedrock or unexplored).
      -- Lit bedrock would be even worse for humans, because it's harder
      -- to guess which tiles are unknown and which can be explored bedrock.
      -- The setup of Allure is ideal, with lit bedrock that is easily
      -- distinguished from an unknown tile. However, LH follows the NetHack,
      -- not the Angband, visual tradition, so we can't improve the situation,
      -- unless we turn to subtle shades of black or non-ASCII glyphs,
      -- but that is yet different aesthetics and it's inconsistent
      -- with console frontends.
  }
hardRock = TileKind
  { tsymbol  = ' '
  , tname    = "impenetrable bedrock"
  , tfreq    = [("basic outer fence", 1), ("noise fence", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = maxBound
  , tfeature = [Dark, Impenetrable, Indistinct]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "rock"
  , tfreq    = [ ("cachable", 50), ("stair terminal", 100)
               , ("legendLit", 100), ("legendDark", 100)
               , ("noiseSet", 70), ("brawlSet", 50), ("shootoutSet", 10)
               , ("battleSet", 250) ]
  , tcolor   = BrCyan  -- not BrWhite, to tell from heroes
  , tcolor2  = Cyan
  , talter   = 100
  , tfeature = [Indistinct]
  }
pillarIce = TileKind
  { tsymbol  = 'O'
  , tname    = "ice"
  , tfreq    = [("noiseSet", 30), ("ice", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [Clear, Embed "frost", OpenTo "damp stone floor", Indistinct]
  }
pulpit = TileKind
  { tsymbol  = 'O'
  , tname    = "pulpit"
  , tfreq    = [("pulpit", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [Clear, Embed "pulpit", Indistinct]
  }
pillarCache = TileKind
  { tsymbol  = 'O'
  , tname    = "cache"
  , tfreq    = [("cachable", 50), ("stair terminal", 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 5
  , tfeature = [ Embed "terrain cache", Embed "terrain cache trap"
               , ChangeTo "cachable", Indistinct ]
  }
lampPost = TileKind
  { tsymbol  = 'O'
  , tname    = "lamp post"
  , tfreq    = [("lampPostOver_O", 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 100
  , tfeature = [Indistinct]
  }
signboardUnread = TileKind  -- client only, indicates never used by this faction
  { tsymbol  = 'O'
  , tname    = "signboard"
  , tfreq    = [("signboard unread", 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [ Embed "signboard", ConsideredByAI
               , RevealAs "dummy, treated as suspect", Indistinct ]
  }
signboardRead = TileKind  -- after first use revealed to be this one
  { tsymbol  = 'O'
  , tname    = "signboard"
  , tfreq    = [("signboard", 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [Embed "signboard", HideAs "signboard unread", Indistinct]
  }
bush = TileKind
  { tsymbol  = '&'
  , tname    = "bush"
  , tfreq    = [ ("lit bush", 1), ("shootoutSet", 30)
               , ("bushClumpOver_f_Lit", 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 10
  , tfeature = [Clear]
  }
bushDark = bush
  { tfreq    = [("escapeSet", 30)]
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature bush
  }
bushBurnt = bush
  { tname    = "burnt bush"
  , tfreq    = [("battleSet", 30), ("bush with fire", 70)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature bush
  }
bushBurning = bush
  { tname    = "burning bush"
  , tfreq    = [("ambushSet", 30), ("bush with fire", 30)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "small fire" : ChangeTo "bush with fire" : tfeature bush
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [("brawlSet", 140), ("treeShadeOver_O_Lit", 1)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 50
  , tfeature = []
  }
treeDark = tree
  { tfreq    = [("escapeSet", 30)]
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature tree
  }
treeBurnt = tree
  { tname    = "burnt tree"
  , tfreq    = [("tree with fire", 30)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature tree
  }
treeBurning = tree
  { tname    = "burning tree"
  , tfreq    = [("ambushSet", 30), ("tree with fire", 70)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "big fire" : ChangeTo "tree with fire" : tfeature tree
      -- dousing off the tree will have more sense when it periodically
      -- explodes, hitting and lighting up the team and so betraying it
  }
wallV = TileKind
  { tsymbol  = '|'
  , tname    = "granite wall"
  , tfreq    = [("legendLit", 100), ("rectWindowsOver_!_Lit", 90)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [ BuildAs "suspect vertical wall Lit"
               , Indistinct ]
  }
wallGlassV = TileKind
  { tsymbol  = '|'
  , tname    = "polished crystal wall"
  , tfreq    = [("wallGlassV", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [ BuildAs "suspect vertical wall Lit"
               , Clear ]
  }
wallGlassVSpice = wallGlassV
  { tfreq    = [("rectWindowsOver_!_Lit", 10)]
  , tfeature = Spice : tfeature wallGlassV
  }
wallSuspectV = TileKind  -- only on client
  { tsymbol  = '|'
  , tname    = "suspect painted wall"
  , tfreq    = [("suspect vertical wall Lit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [ RevealAs "trapped vertical door Lit"
               , ObscureAs "obscured vertical wall Lit"
               , Indistinct ]
  }
wallObscuredDefacedV = TileKind
  { tsymbol  = '|'
  , tname    = "defaced wall"
  , tfreq    = [("obscured vertical wall Lit", 90)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "obscene pictograms"
               , HideAs "suspect vertical wall Lit"
               , Indistinct
               ]
  }
wallObscuredFrescoedV = TileKind
  { tsymbol  = '|'
  , tname    = "frescoed wall"
  , tfreq    = [("obscured vertical wall Lit", 10)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "subtle fresco"
               , HideAs "suspect vertical wall Lit"
               , Indistinct
               ]
  }
doorTrappedV = TileKind
  { tsymbol  = '+'
  , tname    = "trapped door"
  , tfreq    = [("trapped vertical door Lit", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2
  , tfeature = [ Embed "doorway trap"
               , OpenTo "open vertical door Lit"
               , HideAs "suspect vertical wall Lit"
               , Indistinct
               ]
  }
doorClosedV = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("closed vertical door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo "open vertical door Lit", Indistinct]  -- never hidden
  }
doorOpenV = TileKind
  { tsymbol  = '-'
  , tname    = "open door"
  , tfreq    = [("open vertical door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo "closed vertical door Lit"
               ]
  }
wallH = TileKind
  { tsymbol  = '-'
  , tname    = "granite wall"
  , tfreq    = [("legendLit", 100), ("rectWindowsOver_=_Lit", 90)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [ BuildAs "suspect horizontal wall Lit"
               , Indistinct ]
  }
wallGlassH = TileKind
  { tsymbol  = '-'
  , tname    = "polished crystal wall"
  , tfreq    = [("wallGlassH", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [ BuildAs "suspect horizontal wall Lit"
               , Clear ]
  }
wallGlassHSpice = wallGlassH
  { tfreq    = [("rectWindowsOver_=_Lit", 10)]
  , tfeature = Spice : tfeature wallGlassH
  }
wallSuspectH = TileKind  -- only on client
  { tsymbol  = '-'
  , tname    = "suspect uneven wall"
  , tfreq    = [("suspect horizontal wall Lit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [ RevealAs "trapped horizontal door Lit"
               , ObscureAs "obscured horizontal wall Lit"
               , Indistinct ]
  }
wallObscuredH = TileKind
  { tsymbol  = '-'
  , tname    = "scratched wall"
  , tfreq    = [("obscured horizontal wall Lit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "scratch on wall"
               , HideAs "suspect horizontal wall Lit"
               , Indistinct
               ]
  }
doorTrappedH = TileKind
  { tsymbol  = '+'
  , tname    = "trapped door"
  , tfreq    = [("trapped horizontal door Lit", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2
  , tfeature = [ Embed "doorway trap"
               , OpenTo "open horizontal door Lit"
               , HideAs "suspect horizontal wall Lit"
               , Indistinct
               ]
  }
doorClosedH = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("closed horizontal door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo "open horizontal door Lit", Indistinct]  -- never hidden
  }
doorOpenH = TileKind
  { tsymbol  = '|'
  , tname    = "open door"
  , tfreq    = [("open horizontal door Lit", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo "closed horizontal door Lit"
               ]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("staircase up", 9), ("ordinary staircase up", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Embed "staircase up", ConsideredByAI]
  }
stairsTaintedUp = TileKind
  { tsymbol  = '<'
  , tname    = "tainted staircase up"
  , tfreq    = [("staircase up", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed "staircase up", Embed "staircase trap up"
               , ChangeTo "ordinary staircase up" ]
  }
stairsOutdoorUp = stairsUp
  { tname    = "signpost pointing backward"
  , tfreq    = [("staircase outdoor up", 1)]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("staircase down", 9), ("ordinary staircase down", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Embed "staircase down", ConsideredByAI]
  }
stairsTaintedDown = TileKind
  { tsymbol  = '>'
  , tname    = "tainted staircase down"
  , tfreq    = [("staircase down", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed "staircase down", Embed "staircase trap down"
               , ChangeTo "ordinary staircase down" ]
  }

stairsOutdoorDown = stairsDown
  { tname    = "signpost pointing forward"
  , tfreq    = [("staircase outdoor down", 1)]
  }
escapeUp = TileKind
  { tsymbol  = '<'
  , tname    = "exit hatch up"
  , tfreq    = [("legendLit", 1), ("legendDark", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed "escape", ConsideredByAI]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("legendLit", 1), ("legendDark", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed "escape", ConsideredByAI]
  }
escapeOutdoorDown = escapeDown
  { tname    = "exit back to town"
  , tfreq    = [("escape outdoor down", 1)]
  }
rubble = TileKind
  { tsymbol  = ';'
  , tname    = "rubble"
  , tfreq    = [("floorCorridorLit", 1), ("stair terminal", 1)]
      -- ("rubbleOrNotLit", 70)
      -- until we can sync change of tile and activation, it always takes 1 turn
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [OpenTo "rubbleOrNotLit", Embed "rubble"]
  }
floorCorridorLit = TileKind
  { tsymbol  = '#'
  , tname    = "corridor"
  , tfreq    = [("floorCorridorLit", 99), ("rubbleOrNotLit", 30)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = maxBound
  , tfeature = [Walkable, Clear, Indistinct]
  }
floorArenaLit = floorCorridorLit
  { tsymbol  = floorSymbol
  , tname    = "stone floor"
  , tfreq    = [("floorArenaLit", 1), ("arenaSet", 1), ("emptySet", 97)]
  }
floorNoiseLit = floorArenaLit
  { tname    = "damp stone floor"
  , tfreq    = [("noiseSet", 60), ("damp stone floor", 1)]
  }
floorDirtLit = floorArenaLit
  { tname    = "dirt"
  , tfreq    = [ ("battleSet", 1000), ("brawlSet", 1000), ("shootoutSet", 1000)
               , ("ambushSet", 1000), ("escapeSet", 1000) ]
  }
floorDirtSpiceLit = floorDirtLit
  { tfreq    = [ ("treeShadeOver_s_Lit", 1), ("fogClumpOver_f_Lit", 1)
               , ("smokeClumpOver_f_Lit", 1), ("bushClumpOver_f_Lit", 1) ]
  , tfeature = Spice : tfeature floorDirtLit
  }
floorActorLit = floorArenaLit
  { tfreq    = [("floorActorLit", 1)]
  , tfeature = OftenActor : tfeature floorArenaLit
  }
floorItemLit = floorArenaLit
  { tfreq    = []
  , tfeature = OftenItem : tfeature floorArenaLit
  }
floorActorItemLit = floorItemLit
  { tfreq    = [("legendLit", 100)]  -- no OftenItem in legendDark
  , tfeature = OftenActor : tfeature floorItemLit
  }
floorArenaShade = floorActorLit
  { tname    = "shaded ground"
  , tfreq    = [("shaded ground", 1), ("treeShadeOver_s_Lit", 2)]
  , tcolor2  = BrBlack
  , tfeature = Dark : NoItem : tfeature floorActorLit  -- no OftenItem
  }
floorRedLit = floorCorridorLit
  { tsymbol  = floorSymbol
  , tname    = "brick pavement"
  , tfreq    = [("trailLit", 30), ("trailChessLit", 30)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Trail : tfeature floorCorridorLit  -- no Indistinct
  }
floorBlueLit = floorRedLit
  { tname    = "cobblestone path"
  , tfreq    = [("trailLit", 100), ("trailChessLit", 70)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreenLit = floorRedLit
  { tname    = "mossy stone path"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrownLit = floorRedLit
  { tname    = "rotting mahogany deck"
  , tfreq    = [("trailLit", 10)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
floorFog = TileKind
  { tsymbol  = '#'
  , tname    = "faint fog"
  , tfreq    = [ ("lit fog", 1), ("emptySet", 3), ("shootoutSet", 20)
               , ("fogClumpOver_f_Lit", 2) ]
      -- lit fog is OK for shootout, because LOS is mutual, as opposed
      -- to dark fog, and so camper has little advantage, especially
      -- on big maps, where he doesn't know on which side of fog patch to hide
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = maxBound
  , tfeature = [Walkable, NoItem, Indistinct]
  }
floorFogDark = floorFog
  { tname    = "thick fog"
  , tfreq    = [("noiseSet", 10), ("escapeSet", 60)]
  , tfeature = Dark : tfeature floorFog
  }
floorSmoke = TileKind
  { tsymbol  = '#'
  , tname    = "billowing smoke"
  , tfreq    = [ ("lit smoke", 1), ("ambushSet", 30), ("battleSet", 5)
               , ("labTrailLit", 1), ("stair terminal", 2)
               , ("smokeClumpOver_f_Lit", 1) ]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = maxBound
  , tfeature = [Walkable, NoItem, Indistinct]  -- not dark, embers
  }
floorSmokeDark = floorSmoke
  { tname    = "lingering smoke"
  , tfreq    = [("ambushSet", 30)]
  , tfeature = Dark : tfeature floorSmoke
  }

makeDark :: TileKind -> TileKind
makeDark k = let darkText :: GroupName TileKind -> GroupName TileKind
                 darkText t = maybe t (toGroupName . (<> "Dark"))
                              $ T.stripSuffix "Lit" $ tshow t
                 darkFrequency = map (first darkText) $ tfreq k
                 darkFeat (OpenTo t) = Just $ OpenTo $ darkText t
                 darkFeat (CloseTo t) = Just $ CloseTo $ darkText t
                 darkFeat (ChangeTo t) = Just $ ChangeTo $ darkText t
                 darkFeat (HideAs t) = Just $ HideAs $ darkText t
                 darkFeat (BuildAs t) = Just $ BuildAs $ darkText t
                 darkFeat (RevealAs t) = Just $ RevealAs $ darkText t
                 darkFeat (ObscureAs t) = Just $ ObscureAs $ darkText t
                 darkFeat OftenItem = Nothing  -- items not common in the dark
                 darkFeat feat = Just feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) {tcolor2 = BrBlack}
