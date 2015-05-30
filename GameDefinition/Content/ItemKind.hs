-- | Item and treasure definitions.
module Content.ItemKind ( cdefs ) where

import qualified Data.EnumMap.Strict as EM
import Data.List

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindOrgan
import Content.ItemKindTemporary
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

cdefs :: ContentDef ItemKind
cdefs = ContentDef
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validateSingle = validateSingleItemKind
  , validateAll = validateAllItemKind
  , content = items ++ organs ++ blasts ++ actors ++ temporaries
  }

items :: [ItemKind]
items =
  [dart, dart200, paralizingProj, harpoon, net, jumpingPole, sharpeningTool, seeingItem, light1, light2, light3, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, sightSharpening, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, gem4, currency]

dart,    dart200, paralizingProj, harpoon, net, jumpingPole, sharpeningTool, seeingItem, light1, light2, light3, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, sightSharpening, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, gem4, currency :: ItemKind

necklace, ring, potion, flask, scroll, wand, gem :: ItemKind  -- generic templates

-- * Item group symbols, partially from Nethack

symbolProjectile, _symbolLauncher, symbolLight, symbolTool, symbolGem, symbolGold, symbolNecklace, symbolRing, symbolPotion, symbolFlask, symbolScroll, symbolTorsoArmor, symbolMiscArmor, _symbolClothes, symbolShield, symbolPolearm, symbolEdged, symbolHafted, symbolWand, _symbolStaff, _symbolFood :: Char

symbolProjectile = '|'
_symbolLauncher  = '}'
symbolLight      = '('
symbolTool       = '('
symbolGem        = '*'
symbolGold       = '$'
symbolNecklace   = '"'
symbolRing       = '='
symbolPotion     = '!'  -- concoction, bottle, jar, vial, canister
symbolFlask      = '!'
symbolScroll     = '?'  -- book, note, tablet, remote
symbolTorsoArmor = '['
symbolMiscArmor  = '['
_symbolClothes   = '('
symbolShield     = '['
symbolPolearm    = ')'
symbolEdged      = ')'
symbolHafted     = ')'
symbolWand       = '/'  -- magical rod, transmitter, pistol, rifle
_symbolStaff     = '_'  -- scanner
_symbolFood      = ','  -- too easy to miss?

-- * Thrown weapons

dart = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "dart"
  , ifreq    = [("useful", 100), ("any arrow", 100)]
  , iflavour = zipPlain [Cyan]
  , icount   = 4 * d 3
  , irarity  = [(1, 10), (10, 20)]
  , iverbHit = "nick"
  , iweight  = 50
  , iaspects = [AddHurtRanged (d 3 + dl 6 |*| 20)]
  , ieffects = [Hurt (2 * d 1)]
  , ifeature = [Identified]
  , idesc    = "Little, but sharp and sturdy."  -- "Much inferior to arrows though, especially given the contravariance problems."  --- funny, but destroy the suspension of disbelief; this is supposed to be a Lovecraftian horror and any hilarity must ensue from the failures in making it so and not from actively trying to be funny; also, mundane objects are not supposed to be scary or transcendental; the scare is in horrors from the abstract dimension visiting our ordinary reality; without the contrast there's no horror and no wonder, so also the magical items must be contrasted with ordinary XIX century and antique items
  , ikit     = []
  }
dart200 = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "fine dart"
  , ifreq    = [("useful", 100), ("any arrow", 50)]  -- TODO: until arrows added
  , iflavour = zipPlain [BrRed]
  , icount   = 4 * d 3
  , irarity  = [(1, 20), (10, 10)]
  , iverbHit = "prick"
  , iweight  = 50
  , iaspects = [AddHurtRanged (d 3 + dl 6 |*| 20)]
  , ieffects = [Hurt (1 * d 1)]
  , ifeature = [toVelocity 200, Identified]
  , idesc    = "Finely balanced for throws of great speed."
  , ikit     = []
  }

-- * Exotic thrown weapons

paralizingProj = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "bolas set"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = dl 4
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "entangle"
  , iweight  = 500
  , iaspects = []
  , ieffects = [Hurt (2 * d 1), Paralyze (5 + d 5), DropBestWeapon]
  , ifeature = [Identified]
  , idesc    = "Wood balls tied with hemp rope. The target enemy is tripped and bound to drop the main weapon, while fighting for balance."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = dl 5
  , irarity  = [(10, 10)]
  , iverbHit = "hook"
  , iweight  = 4000
  , iaspects = [AddHurtRanged (d 2 + dl 5 |*| 20)]
  , ieffects = [Hurt (4 * d 1), PullActor (ThrowMod 200 50)]
  , ifeature = [Identified]
  , idesc    = "The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
net = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "net"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = dl 3
  , irarity  = [(3, 5), (10, 4)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , iaspects = []
  , ieffects = [ toOrganGameTurn "slow 10" (3 + d 3)
               , DropItem CEqp "torso armor" False ]
  , ifeature = [Identified]
  , idesc    = "A wide net with weights along the edges. Entangles armor and restricts movement."
  , ikit     = []
  }

-- * Assorted tools

jumpingPole = ItemKind
  { isymbol  = symbolTool
  , iname    = "jumping pole"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 2)]
  , iverbHit = "prod"
  , iweight  = 10000
  , iaspects = [Timeout $ d 2 + 2 - dl 2 |*| 10]
  , ieffects = [Recharging (toOrganActorTurn "fast 20" 1)]
  , ifeature = [Durable, Applicable, Identified]
  , idesc    = "Makes you vulnerable at take-off, but then you are free like a bird."
  , ikit     = []
  }
sharpeningTool = ItemKind
  { isymbol  = symbolTool
  , iname    = "whetstone"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(10, 10)]
  , iverbHit = "smack"
  , iweight  = 400
  , iaspects = [AddHurtMelee $ d 10 |*| 3]
  , ieffects = []
  , ifeature = [EqpSlot EqpSlotAddHurtMelee "", Identified]
  , idesc    = "A portable sharpening stone that lets you fix your weapons between or even during fights, without the need to set up camp, fish out tools and assemble a proper sharpening workshop."
  , ikit     = []
  }
seeingItem = ItemKind
  { isymbol  = '%'
  , iname    = "pupil"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "gaze at"
  , iweight  = 100
  , iaspects = [ AddSight 10, AddMaxCalm 60, AddLight 2
               , Periodic, Timeout $ 1 + d 2 ]
  , ieffects = [ Recharging (toOrganNone "poisoned")
               , Recharging (Summon [("mobile monster", 1)] 1) ]
  , ifeature = [Identified]
  , idesc    = "A slimy, dilated green pupil torn out from some giant eye. Clear and focused, as if still alive."
  , ikit     = []
  }

-- * Lights

light1 = ItemKind
  { isymbol  = symbolLight
  , iname    = "wooden torch"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = d 2
  , irarity  = [(1, 10)]
  , iverbHit = "scorch"
  , iweight  = 1200
  , iaspects = [ AddLight 3       -- not only flashes, but also sparks
               , AddSight (-2) ]  -- unused by AI due to the mixed blessing
  , ieffects = [Burn 2]
  , ifeature = [EqpSlot EqpSlotAddLight "", Identified]
  , idesc    = "A smoking, heavy wooden torch, burning in an unsteady glow."
  , ikit     = []
  }
light2 = ItemKind
  { isymbol  = symbolLight
  , iname    = "oil lamp"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(6, 7)]
  , iverbHit = "burn"
  , iweight  = 1000
  , iaspects = [AddLight 3, AddSight (-1)]
  , ieffects = [Burn 3, Paralyze 3, OnSmash (Explode "burning oil 3")]
  , ifeature = [ toVelocity 70  -- hard not to spill the oil while throwing
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "A clay lamp filled with plant oil feeding a tiny wick."
  , ikit     = []
  }
light3 = ItemKind
  { isymbol  = symbolLight
  , iname    = "brass lantern"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(10, 5)]
  , iverbHit = "burn"
  , iweight  = 2400
  , iaspects = [AddLight 4, AddSight (-1)]
  , ieffects = [Burn 4, Paralyze 4, OnSmash (Explode "burning oil 4")]
  , ifeature = [ toVelocity 70  -- hard to throw so that it opens and burns
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "Very bright and very heavy brass lantern."
  , ikit     = []
  }

-- * Periodic jewelry

gorget = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "Old Gorget"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(4, 3), (10, 3)]  -- weak, shallow
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = [ Unique
               , Periodic
               , Timeout $ 1 + d 2
               , AddArmorMelee $ 2 + d 3
               , AddArmorRanged $ 2 + d 3 ]
  , ieffects = [Recharging (RefillCalm 1)]
  , ifeature = [ Durable, Precious, EqpSlot EqpSlotPeriodic ""
               , Identified, toVelocity 50 ]  -- not dense enough
  , idesc    = "Highly ornamental, cold, large, steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
  , ikit     = []
  }
necklace = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(10, 2)]
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = [Periodic]
  , ieffects = []
  , ifeature = [ Precious, EqpSlot EqpSlotPeriodic ""
               , toVelocity 50 ]  -- not dense enough
  , idesc    = "Menacing Greek symbols shimmer with increasing speeds along a chain of fine encrusted links. After a tense build-up, a prismatic arc shoots towards the ground and the iridescence subdues, becomes ordered and resembles a harmless ornament again, for a time."
  , ikit     = []
  }
necklace1 = necklace
  { ifreq    = [("treasure", 100)]
  , iaspects = [Unique, Timeout $ d 3 + 4 - dl 3 |*| 10]
               ++ iaspects necklace
  , ieffects = [NoEffect "of Aromata", Recharging (RefillHP 1)]
  , ifeature = Durable : ifeature necklace
  , idesc    = "A cord of freshly dried herbs and healing berries."
  }
necklace2 = necklace
  { ifreq    = [("treasure", 100)]  -- just too nasty to call it useful
  , irarity  = [(1, 1)]
  , iaspects = (Timeout $ d 3 + 3 - dl 3 |*| 10) : iaspects necklace
  , ieffects = [ Recharging Impress
               , Recharging (DropItem COrgan "temporary conditions" True)
               , Recharging (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , Recharging (Explode "waste") ]
  }
necklace3 = necklace
  { iaspects = (Timeout $ d 3 + 3 - dl 3 |*| 10) : iaspects necklace
  , ieffects = [Recharging (Paralyze $ 5 + d 5 + dl 5)]
  }
necklace4 = necklace
  { iaspects = (Timeout $ d 4 + 4 - dl 4 |*| 2) : iaspects necklace
  , ieffects = [Recharging (Teleport $ d 2 * 3)]
  }
necklace5 = necklace
  { iaspects = (Timeout $ d 3 + 4 - dl 3 |*| 10) : iaspects necklace
  , ieffects = [Recharging (Teleport $ 14 + d 3 * 3)]
  }
necklace6 = necklace
  { iaspects = (Timeout $ d 4 |*| 10) : iaspects necklace
  , ieffects = [Recharging (PushActor (ThrowMod 100 50))]
  }
necklace7 = necklace  -- TODO: teach AI to wear only for fight
  { ifreq    = [("treasure", 100)]
  , iaspects = [ Unique, AddMaxHP $ 10 + d 10
               , AddArmorMelee 20, AddArmorRanged 20
               , Timeout $ d 2 + 5 - dl 3 ]
               ++ iaspects necklace
  , ieffects = [ NoEffect "of Overdrive"
               , Recharging (InsertMove $ 1 + d 2)
               , Recharging (RefillHP (-1))
               , Recharging (RefillCalm (-1)) ]
  , ifeature = Durable : ifeature necklace
  }
necklace8 = necklace
  { iaspects = (Timeout $ d 3 + 3 - dl 3 |*| 5) : iaspects necklace
  , ieffects = [Recharging $ Explode "spark"]
  }
necklace9 = necklace
  { iaspects = (Timeout $ d 3 + 3 - dl 3 |*| 5) : iaspects necklace
  , ieffects = [Recharging $ Explode "fragrance"]
  }

-- * Non-periodic jewelry

sightSharpening = ItemKind
  { isymbol  = symbolRing
  , iname    = "Sharp Monocle"
  , ifreq    = [("treasure", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(7, 3), (10, 3)]  -- medium weak, medium shallow
  , iverbHit = "rap"
  , iweight  = 50
  , iaspects = [Unique, AddSight $ 1 + d 2, AddHurtMelee $ d 2 |*| 3]
  , ieffects = []
  , ifeature = [ Precious, Identified, Durable
               , EqpSlot EqpSlotAddSight "" ]
  , idesc    = "Let's you better focus your weaker eye."
  , ikit     = []
  }
-- Don't add standard effects to rings, because they go in and out
-- of eqp and so activating them would require UI tedium: looking for
-- them in eqp and inv or even activating a wrong item via letter by mistake.
ring = ItemKind
  { isymbol  = symbolRing
  , iname    = "ring"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 3)]
  , iverbHit = "knock"
  , iweight  = 15
  , iaspects = []
  , ieffects = [Explode "blast 20"]
  , ifeature = [Precious, Identified]
  , idesc    = "It looks like an ordinary object, but it's in fact a generator of exceptional effects: adding to some of your natural abilities and subtracting from others. You'd profit enormously if you could find a way to multiply such generators."
  , ikit     = []
  }
ring1 = ring
  { irarity  = [(10, 2)]
  , iaspects = [AddSpeed $ 1 + d 2, AddMaxHP $ dl 7 - 7 - d 7]
  , ieffects = [Explode "distortion"]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddSpeed ""]
  }
ring2 = ring
  { irarity  = [(10, 5)]
  , iaspects = [AddMaxHP $ 10 + dl 10, AddMaxCalm $ dl 5 - 20 - d 5]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddMaxHP ""]
  }
ring3 = ring
  { irarity  = [(10, 5)]
  , iaspects = [AddMaxCalm $ 29 + dl 10]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddMaxCalm ""]
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring4 = ring
  { irarity  = [(3, 3), (10, 5)]
  , iaspects = [AddHurtMelee $ d 5 + dl 5 |*| 3, AddMaxHP $ dl 3 - 5 - d 3]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddHurtMelee ""]
  }
ring5 = ring  -- by the time it's found, probably no space in eqp
  { irarity  = [(5, 0), (10, 2)]
  , iaspects = [AddLight $ d 2]
  , ieffects = [Explode "distortion"]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddLight ""]
  , idesc    = "A sturdy ring with a large, shining stone."
  }
ring6 = ring
  { ifreq    = [("treasure", 100)]
  , irarity  = [(10, 2)]
  , iaspects = [ Unique, AddSpeed $ 3 + d 4
               , AddMaxCalm $ - 20 - d 20, AddMaxHP $ - 20 - d 20 ]
  , ieffects = [NoEffect "of Rush"]  -- no explosion, because Durable
  , ifeature = ifeature ring ++ [Durable, EqpSlot EqpSlotAddSpeed ""]
  }
ring7 = ring
  { ifreq    = [("useful", 100), ("ring of opportunity sniper", 1) ]
  , irarity  = [(1, 1)]
  , iaspects = [AddSkills $ EM.fromList [(AbProject, 8)]]
  , ieffects = [ NoEffect "of opportunity sniper"
               , Explode "distortion" ]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot (EqpSlotAddSkills AbProject) ""]
  }
ring8 = ring
  { ifreq    = [("useful", 1), ("ring of opportunity grenadier", 1) ]
  , irarity  = [(1, 1)]
  , iaspects = [AddSkills $ EM.fromList [(AbProject, 11)]]
  , ieffects = [ NoEffect "of opportunity grenadier"
               , Explode "distortion" ]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot (EqpSlotAddSkills AbProject) ""]
  }

-- * Ordinary exploding consumables, often intended to be thrown

potion = ItemKind
  { isymbol  = symbolPotion
  , iname    = "potion"
  , ifreq    = [("useful", 100)]
  , iflavour = zipLiquid brightCol ++ zipPlain brightCol ++ zipFancy brightCol
  , icount   = 1
  , irarity  = [(1, 12), (10, 9)]
  , iverbHit = "splash"
  , iweight  = 200
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 50  -- oily, bad grip
               , Applicable, Fragile ]
  , idesc    = "A vial of bright, frothing concoction."  -- purely natural; no maths, no magic
  , ikit     = []
  }
potion1 = potion
  { ieffects = [ NoEffect "of rose water", Impress, RefillCalm (-3)
               , OnSmash ApplyPerfume, OnSmash (Explode "fragrance") ]
  }
potion2 = potion
  { ifreq    = [("treasure", 100)]
  , irarity  = [(6, 10), (10, 10)]
  , iaspects = [Unique]
  , ieffects = [ NoEffect "of Attraction", Impress, OverfillCalm (-20)
               , OnSmash (Explode "pheromone") ]
  }
potion3 = potion
  { irarity  = [(1, 10)]
  , ieffects = [ RefillHP 5, DropItem COrgan "poisoned" True
               , OnSmash (Explode "healing mist") ]
  }
potion4 = potion
  { irarity  = [(10, 10)]
  , ieffects = [ RefillHP 10, DropItem COrgan "poisoned" True
               , OnSmash (Explode "healing mist 2") ]
  }
potion5 = potion
  { ieffects = [ OneOf [ OverfillHP 10, OverfillHP 5, Burn 5
                       , toOrganActorTurn "strengthened" (20 + d 5) ]
               , OnSmash (OneOf [ Explode "healing mist"
                                , Explode "wounding mist"
                                , Explode "fragrance"
                                , Explode "smelly droplet"
                                , Explode "blast 10" ]) ]
  }
potion6 = potion
  { irarity  = [(3, 3), (10, 6)]
  , ieffects = [ Impress
               , OneOf [ OverfillCalm (-60)
                       , OverfillHP 20, OverfillHP 10, Burn 10
                       , toOrganActorTurn "fast 20" (20 + d 5) ]
               , OnSmash (OneOf [ Explode "healing mist 2"
                                , Explode "calming mist"
                                , Explode "distressing odor"
                                , Explode "eye drop"
                                , Explode "blast 20" ]) ]
  }
potion7 = potion
  { irarity  = [(1, 15), (10, 5)]
  , ieffects = [ DropItem COrgan "poisoned" True
               , OnSmash (Explode "antidote mist") ]
  }
potion8 = potion
  { irarity  = [(1, 5), (10, 15)]
  , ieffects = [ DropItem COrgan "temporary conditions" True
               , OnSmash (Explode "blast 10") ]
  }
potion9 = potion
  { ifreq    = [("treasure", 100)]
  , irarity  = [(10, 5)]
  , iaspects = [Unique]
  , ieffects = [ NoEffect "of Love", OverfillHP 60
               , Impress, OverfillCalm (-60)
               , OnSmash (Explode "healing mist 2")
               , OnSmash (Explode "pheromone") ]
  }

-- * Exploding consumables with temporary aspects, can be thrown
-- TODO: dip projectiles in those
-- TODO: add flavour and realism as in, e.g., "flask of whiskey",
-- which is more flavourful and believable than "flask of strength"

flask = ItemKind
  { isymbol  = symbolFlask
  , iname    = "flask"
  , ifreq    = [("useful", 100), ("flask", 100)]
  , iflavour = zipLiquid darkCol ++ zipPlain darkCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(1, 9), (10, 6)]
  , iverbHit = "splash"
  , iweight  = 500
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 50  -- oily, bad grip
               , Applicable, Fragile ]
  , idesc    = "A flask of oily liquid of a suspect color."
  , ikit     = []
  }
flask1 = flask
  { irarity  = [(10, 5)]
  , ieffects = [ NoEffect "of strength brew"
               , toOrganActorTurn "strengthened" (20 + d 5)
               , toOrganNone "regenerating"
               , OnSmash (Explode "strength mist") ]
  }
flask2 = flask
  { ieffects = [ NoEffect "of weakness brew"
               , toOrganGameTurn "weakened" (20 + d 5)
               , OnSmash (Explode "weakness mist") ]
  }
flask3 = flask
  { ieffects = [ NoEffect "of protecting balm"
               , toOrganActorTurn "protected" (20 + d 5)
               , OnSmash (Explode "protecting balm") ]
  }
flask4 = flask
  { ieffects = [ NoEffect "of PhD defense questions"
               , toOrganGameTurn "defenseless" (20 + d 5)
               , OnSmash (Explode "PhD defense question") ]
  }
flask5 = flask
  { irarity  = [(10, 5)]
  , ieffects = [ NoEffect "of haste brew"
               , toOrganActorTurn "fast 20" (20 + d 5)
               , OnSmash (Explode "haste spray") ]
  }
flask6 = flask
  { ieffects = [ NoEffect "of lethargy brew"
               , toOrganGameTurn "slow 10" (20 + d 5)
               , toOrganNone "regenerating"
               , RefillCalm 3
               , OnSmash (Explode "slowness spray") ]
  }
flask7 = flask  -- sight can be reduced from Calm, drunk, etc.
  { irarity  = [(10, 7)]
  , ieffects = [ NoEffect "of eye drops"
               , toOrganActorTurn "far-sighted" (20 + d 5)
               , OnSmash (Explode "blast 10") ]
  }
flask8 = flask
  { irarity  = [(10, 3)]
  , ieffects = [ NoEffect "of smelly concoction"
               , toOrganActorTurn "keen-smelling" (20 + d 5)
               , OnSmash (Explode "blast 10") ]
  }
flask9 = flask
  { ieffects = [ NoEffect "of bait cocktail"
               , toOrganActorTurn "drunk" (5 + d 5)
               , OnSmash (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , OnSmash (Explode "waste") ]
  }
flask10 = flask
  { ieffects = [ NoEffect "of whiskey"
               , toOrganActorTurn "drunk" (20 + d 5)
               , Impress, Burn 2, RefillHP 4
               , OnSmash (Explode "whiskey spray") ]
  }
flask11 = flask
  { irarity  = [(1, 20), (10, 10)]
  , ieffects = [ NoEffect "of regeneration brew"
               , toOrganNone "regenerating"
               , OnSmash (Explode "healing mist") ]
  }
flask12 = flask  -- but not flask of Calm depletion, since Calm reduced often
  { ieffects = [ NoEffect "of poison"
               , toOrganNone "poisoned"
               , OnSmash (Explode "wounding mist") ]
  }
flask13 = flask
  { irarity  = [(10, 5)]
  , ieffects = [ NoEffect "of slow resistance"
               , toOrganNone "slow resistant"
               , OnSmash (Explode "anti-slow mist") ]
  }
flask14 = flask
  { irarity  = [(10, 5)]
  , ieffects = [ NoEffect "of poison resistance"
               , toOrganNone "poison resistant"
               , OnSmash (Explode "antidote mist") ]
  }

-- * Non-exploding consumables, not specifically designed for throwing

scroll = ItemKind
  { isymbol  = symbolScroll
  , iname    = "scroll"
  , ifreq    = [("useful", 100), ("any scroll", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain darkCol  -- arcane and old
  , icount   = 1
  , irarity  = [(1, 15), (10, 12)]
  , iverbHit = "thump"
  , iweight  = 50
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 25  -- bad shape, even rolled up
               , Applicable ]
  , idesc    = "Scraps of haphazardly scribbled mysteries from beyond. Is this equation an alchemical recipe? Is this diagram an extradimensional map? Is this formula a secret call sign?"
  , ikit     = []
  }
scroll1 = scroll
  { ifreq    = [("treasure", 100)]
  , irarity  = [(5, 10), (10, 10)]  -- mixed blessing, so available early
  , iaspects = [Unique]
  , ieffects = [ NoEffect "of Reckless Beacon"
               , CallFriend 1, Summon standardSummon (2 + d 2) ]
  }
scroll2 = scroll
  { irarity  = []
  , ieffects = []
  }
scroll3 = scroll
  { irarity  = [(1, 5), (10, 3)]
  , ieffects = [Ascend (-1)]
  }
scroll4 = scroll
  { ieffects = [OneOf [ Teleport 5, RefillCalm 5, RefillCalm (-5)
                      , InsertMove 5, Paralyze 10 ]]
  }
scroll5 = scroll
  { irarity  = [(10, 15)]
  , ieffects = [ Impress
               , OneOf [ Teleport 20, Ascend (-1), Ascend 1
                       , Summon standardSummon 2, CallFriend 1
                       , RefillCalm 5, OverfillCalm (-60)
                       , CreateItem CGround "useful" TimerNone ] ]
  }
scroll6 = scroll
  { ieffects = [Teleport 5]
  }
scroll7 = scroll
  { ieffects = [Teleport 20]
  }
scroll8 = scroll
  { irarity  = [(10, 3)]
  , ieffects = [InsertMove $ 1 + d 2 + dl 2]
  }
scroll9 = scroll  -- TODO: remove Calm when server can tell if anything IDed
  { irarity  = [(1, 15), (10, 10)]
  , ieffects = [ NoEffect "of scientific explanation"
               , Identify, OverfillCalm 3 ]
  }
scroll10 = scroll  -- TODO: firecracker only if an item really polymorphed?
                   -- But currently server can't tell.
  { irarity  = [(10, 10)]
  , ieffects = [ NoEffect "transfiguration"
               , PolyItem, Explode "firecracker 7" ]
  }
scroll11 = scroll
  { ifreq    = [("treasure", 100)]
  , irarity  = [(6, 10), (10, 10)]
  , iaspects = [Unique]
  , ieffects = [NoEffect "of Prisoner Release", CallFriend 1]
  }

standardSummon :: Freqs ItemKind
standardSummon = [("mobile monster", 30), ("mobile animal", 70)]

-- * Armor

armorLeather = ItemKind
  { isymbol  = symbolTorsoArmor
  , iname    = "leather armor"
  , ifreq    = [("useful", 100), ("torso armor", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 9), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 7000
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ 1 + d 2 + dl 2 |*| 5
               , AddArmorRanged $ 1 + d 2 + dl 2 |*| 5 ]
  , ieffects = []
  , ifeature = [ toVelocity 30  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "A stiff jacket formed from leather boiled in bee wax. Smells much better than the rest of your garment."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "mail armor"
  , iflavour = zipPlain [Cyan]
  , irarity  = [(6, 9), (10, 3)]
  , iweight  = 12000
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ 2 + d 2 + dl 3 |*| 5
               , AddArmorRanged $ 2 + d 2 + dl 3 |*| 5 ]
  , idesc    = "A long shirt woven from iron rings. Discourages foes from attacking your torso, making it harder for them to land a blow."
  }
gloveFencing = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "leather gauntlet"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]
  , iverbHit = "flap"
  , iweight  = 100
  , iaspects = [ AddHurtMelee $ (d 2 + dl 10) |*| 3
               , AddArmorRanged $ d 2 |*| 5 ]
  , ieffects = []
  , ifeature = [ toVelocity 30  -- flaps and flutters
               , Durable, EqpSlot EqpSlotAddArmorRanged "", Identified ]
  , idesc    = "A fencing glove from rough leather ensuring a good grip. Also quite effective in deflecting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "steel gauntlet"
  , iflavour = zipPlain [BrCyan]
  , irarity  = [(1, 9), (10, 3)]
  , iweight  = 300
  , iaspects = [ AddArmorMelee $ 1 + dl 2 |*| 5
               , AddArmorRanged $ 1 + dl 2 |*| 5 ]
  , idesc    = "Long leather gauntlet covered in overlapping steel plates."
  }
gloveJousting = gloveFencing
  { iname    = "Tournament Gauntlet"
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 3), (10, 3)]
  , iweight  = 500
  , iaspects = [ Unique
               , AddHurtMelee $ dl 4 - 6 |*| 3
               , AddArmorMelee $ 2 + dl 2 |*| 5
               , AddArmorRanged $ 2 + dl 2 |*| 5 ]
  , idesc    = "Rigid, steel, jousting handgear. If only you had a lance. And a horse."
  }

-- * Shields

-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
buckler = ItemKind
  { isymbol  = symbolShield
  , iname    = "buckler"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(4, 6)]
  , iverbHit = "bash"
  , iweight  = 2000
  , iaspects = [ AddArmorMelee 40
               , AddHurtMelee (-30)
               , Timeout $ d 3 + 3 - dl 3 |*| 2 ]
  , ieffects = [ Hurt (1 * d 1)  -- to display xdy everywhre in Hurt
               , Recharging (PushActor (ThrowMod 200 50)) ]
  , ifeature = [ toVelocity 40  -- unwieldy to throw
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Heavy and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too small to intercept projectiles with."
  , ikit     = []
  }
shield = buckler
  { iname    = "shield"
  , irarity  = [(8, 3)]
  , iflavour = zipPlain [Green]
  , iweight  = 3000
  , iaspects = [ AddArmorMelee 80
               , AddHurtMelee (-70)
               , Timeout $ d 6 + 6 - dl 6 |*| 2 ]
  , ieffects = [Hurt (1 * d 1), Recharging (PushActor (ThrowMod 400 50))]
  , ifeature = [ toVelocity 30  -- unwieldy to throw
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Large and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  }

-- * Weapons

dagger = ItemKind
  { isymbol  = symbolEdged
  , iname    = "dagger"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1, 20)]
  , iverbHit = "stab"
  , iweight  = 1000
  , iaspects = [ AddHurtMelee $ d 3 + dl 3 |*| 3
               , AddArmorMelee $ d 2 |*| 5
               , AddHurtRanged (-60) ]  -- as powerful as a dart
  , ieffects = [Hurt (6 * d 1)]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "A short dagger for thrusting and parrying blows. Does not penetrate deeply, but is hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = dagger
  { iname    = "Double Dagger"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(1, 2), (10, 4)]
  -- The timeout has to be small, so that the player can count on the effect
  -- occuring consistently in any longer fight. Otherwise, the effect will be
  -- absent in some important fights, leading to the feeling of bad luck,
  -- but will manifest sometimes in fights where it doesn't matter,
  -- leading to the feeling of wasted power.
  -- If the effect is very powerful and so the timeout has to be significant,
  -- let's make it really large, for the effect to occur only once in a fight:
  -- as soon as the item is equipped, or just on the first strike.
  , iaspects = [Unique, Timeout $ d 3 + 4 - dl 3 |*| 2]
  , ieffects = ieffects dagger
               ++ [Recharging DropBestWeapon, Recharging $ RefillCalm (-3)]
  , idesc    = "A double dagger that a focused fencer can use to catch and twist an opponent's blade occasionally."
  }
hammer = ItemKind
  { isymbol  = symbolHafted
  , iname    = "war hammer"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(5, 15)]
  , iverbHit = "club"
  , iweight  = 1500
  , iaspects = [ AddHurtMelee $ d 2 + dl 2 |*| 3
               , AddHurtRanged (-80) ]  -- as powerful as a dart
  , ieffects = [Hurt (8 * d 1)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the sharp tip costs
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "It may not cause grave wounds, but neither does it glance off nor ricochet. Great sidearm for opportunistic blows against armored foes."
  , ikit     = []
  }
hammerParalyze = hammer
  { iname    = "Concussion Hammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 2), (10, 4)]
  , iaspects = [Unique, Timeout $ d 2 + 3 - dl 2 |*| 2]
  , ieffects = ieffects hammer ++ [Recharging $ Paralyze 5]
  }
hammerSpark = hammer
  { iname    = "Grand Smithhammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 2), (10, 4)]
  , iaspects = [Unique, Timeout $ d 4 + 4 - dl 4 |*| 2]
  , ieffects = ieffects hammer ++ [Recharging $ Explode "spark"]
  }
sword = ItemKind
  { isymbol  = symbolEdged
  , iname    = "sword"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(4, 1), (5, 15)]
  , iverbHit = "slash"
  , iweight  = 2000
  , iaspects = []
  , ieffects = [Hurt (10 * d 1)]
  , ifeature = [ toVelocity 5  -- ensuring it hits with the tip costs speed
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "Difficult to master; deadly when used effectively. The steel is particularly hard and keen, but rusts quickly without regular maintenance."
  , ikit     = []
  }
swordImpress = sword
  { iname    = "Master's Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 4)]
  , iaspects = [Unique, Timeout $ d 4 + 5 - dl 4 |*| 2]
  , ieffects = ieffects sword ++ [Recharging Impress]
  , idesc    = "A particularly well-balance blade, lending itself to impressive shows of fencing skill."
  }
swordNullify = sword
  { iname    = "Gutting Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 4)]
  , iaspects = [Unique, Timeout $ d 4 + 5 - dl 4 |*| 2]
  , ieffects = ieffects sword
               ++ [ Recharging $ DropItem COrgan "temporary conditions" True
                  , Recharging $ RefillHP (-2) ]
  , idesc    = "Cold, thin blade that pierces deeply and sends its victim into abrupt, sobering shock."
  }
halberd = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "war scythe"
  , ifreq    = [("useful", 100), ("starting weapon", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(7, 1), (10, 10)]
  , iverbHit = "impale"
  , iweight  = 3000
  , iaspects = [AddArmorMelee $ 1 + dl 3 |*| 5]
  , ieffects = [Hurt (12 * d 1)]
  , ifeature = [ toVelocity 5  -- not balanced
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "An improvised but deadly weapon made of a blade from a scythe attached to a long pole."
  , ikit     = []
  }
halberdPushActor = halberd
  { iname    = "Swiss Halberd"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(7, 1), (10, 4)]
  , iaspects = [Unique, Timeout $ d 5 + 5 - dl 5 |*| 2]
  , ieffects = ieffects halberd ++ [Recharging (PushActor (ThrowMod 400 25))]
  , idesc    = "A versatile polearm, with great reach and leverage. Foes are held at a distance."
  }

-- * Wands

wand = ItemKind
  { isymbol  = symbolWand
  , iname    = "wand"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , irarity  = []  -- TODO: add charges, etc.
  , iverbHit = "club"
  , iweight  = 300
  , iaspects = [AddLight 1, AddSpeed (-1)]  -- pulsing with power, distracts
  , ieffects = []
  , ifeature = [ toVelocity 125  -- magic
               , Applicable, Durable ]
  , idesc    = "Buzzing with dazzling light that shines even through appendages that handle it."  -- TODO: add math flavour
  , ikit     = []
  }
wand1 = wand
  { ieffects = []  -- TODO: emit a cone of sound shrapnel that makes enemy cover his ears and so drop '|' and '{'
  }
wand2 = wand
  { ieffects = []
  }

-- * Treasure

gem = ItemKind
  { isymbol  = symbolGem
  , iname    = "gem"
  , ifreq    = [("treasure", 100), ("gem", 100)]
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = []
  , iverbHit = "tap"
  , iweight  = 50
  , iaspects = [AddLight 1, AddSpeed (-1)]
                 -- reflects strongly, distracts; so it glows in the dark,
                 -- is visible on dark floor, but not too tempting to wear
  , ieffects = []
  , ifeature = [Precious]
  , idesc    = "Useless, and still worth around 100 gold each. Would gems of thought and pearls of artful design be valued that much in our age of Science and Progress!"
  , ikit     = []
  }
gem1 = gem
  { irarity  = [(2, 0), (10, 12)]
  }
gem2 = gem
  { irarity  = [(4, 0), (10, 14)]
  }
gem3 = gem
  { irarity  = [(6, 0), (10, 16)]
  }
gem4 = gem
  { iname    = "elixir"
  , iflavour = zipPlain [BrYellow]
  , irarity  = [(1, 40), (10, 40)]
  , iaspects = []
  , ieffects = [NoEffect "of youth", OverfillCalm 5, OverfillHP 15]
  , ifeature = [Identified, Applicable, Precious]  -- TODO: only heal humans
  , idesc    = "A crystal vial of amber liquid, supposedly granting eternal youth and fetching 100 gold per piece. The main effect seems to be mild euphoria, but it admittedly heals minor ailments rather well."
  }
currency = ItemKind
  { isymbol  = symbolGold
  , iname    = "gold piece"
  , ifreq    = [("treasure", 100), ("currency", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + d 20 + dl 20
  , irarity  = [(1, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 31
  , iaspects = []
  , ieffects = []
  , ifeature = [Identified, Precious]
  , idesc    = "Reliably valuable in every civilized plane of existence."
  , ikit     = []
  }
