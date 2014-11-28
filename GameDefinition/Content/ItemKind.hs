-- | Item and treasure definitions.
module Content.ItemKind ( cdefs ) where

import Data.List

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindOrgan
import Content.ItemKindTemporary
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
  [dart, dart200, bolas, harpoon, net, jumpingPole, whetstone, woodenTorch, oilLamp, brassLantern, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, monocle, ring1, ring2, ring3, ring4, ring5, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, currency]

dart,    dart200, bolas, harpoon, net, jumpingPole, whetstone, woodenTorch, oilLamp, brassLantern, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, monocle, ring1, ring2, ring3, ring4, ring5, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, currency :: ItemKind

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
_symbolFood      = ','

-- * Thrown weapons

dart = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "dart"
  , ifreq    = [("useful", 100), ("any arrow", 100)]
  , iflavour = zipPlain [Cyan]
  , icount   = 3 * d 3
  , irarity  = [(1, 20), (10, 10)]
  , iverbHit = "prick"
  , iweight  = 50
  , iaspects = [AddHurtRanged ((d 6 + dl 6) |*| 10)]
  , ieffects = [Hurt (3 * d 1)]
  , ifeature = [Identified]
  , idesc    = "Little, but sharp and sturdy."  -- "Much inferior to arrows though, especially given the contravariance problems."  --- funny, but destroy the suspension of disbelief; this is supposed to be a Lovecraftian horror and any hilarity must ensue from the failures in making it so and not from actively trying to be funny; also, mundane objects are not supposed to be scary or transcendental; the scare is in horrors from the abstract dimension visiting our ordinary reality; without the contrast there's no horror and no wonder, so also the magical items must be contrasted with ordinary XIX century and antique items
  , ikit     = []
  }
dart200 = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "fine dart"
  , ifreq    = [("useful", 100), ("any arrow", 50)]  -- TODO: until arrows added
  , iflavour = zipPlain [BrRed]
  , icount   = 3 * d 3
  , irarity  = [(4, 20), (10, 10)]
  , iverbHit = "prick"
  , iweight  = 50
  , iaspects = [AddHurtRanged ((d 6 + dl 6) |*| 10)]
  , ieffects = [Hurt (2 * d 1)]
  , ifeature = [toVelocity 200, Identified]
  , idesc    = "Finely balanced for throws of great speed."
  , ikit     = []
  }

-- * Exotic thrown weapons

bolas = ItemKind
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
  , ifeature = []
  , idesc    = "Wood balls tied with hemp rope. The target enemy is tripped and bound to drop its weapon, while recovering balance."
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
  , iaspects = [AddHurtRanged ((d 2 + 2 * dl 5) |*| 10)]
  , ieffects = [Hurt (4 * d 1), PullActor (ThrowMod 200 50)]
  , ifeature = []
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
  , ifeature = []
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
  , irarity  = [(1, 2), (10, 1)]
  , iverbHit = "prod"
  , iweight  = 10000
  , iaspects = [Timeout $ (d 2 + 2 - dl 2) |*| 10]
  , ieffects = [Recharging (toOrganActorTurn "fast 20" 1)]
  , ifeature = [Durable, Applicable, Identified]
  , idesc    = "Makes you vulnerable at take-off, but then you are free like a bird."
  , ikit     = []
  }
whetstone = ItemKind
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

-- * Lights

woodenTorch = ItemKind
  { isymbol  = symbolLight
  , iname    = "wooden torch"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 8), (3, 6)]
  , iverbHit = "scorch"
  , iweight  = 1200
  , iaspects = [ AddLight 3
               , AddSight (-2) ]  -- not only flashes, but also sparks
  , ieffects = [Burn 3]
  , ifeature = [EqpSlot EqpSlotAddLight "", Identified]
  , idesc    = "A smoking, heavy wooden torch, burning in an unsteady fire."
  , ikit     = []
  }
oilLamp = ItemKind
  { isymbol  = symbolLight
  , iname    = "oil lamp"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "burn"
  , iweight  = 1000
  , iaspects = [AddLight 3, AddSight (-1)]
  , ieffects = [Burn 3, Paralyze 3, OnSmash (Explode "burning oil 3")]
  , ifeature = [ toVelocity 70  -- hard not to spill the oil while throwing
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "A clay lamp filled with plant oil feeding a tiny wick."
  , ikit     = []
  }
brassLantern = ItemKind
  { isymbol  = symbolLight
  , iname    = "brass lantern"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(10, 4)]
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
  , iname    = "gorget"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(4, 1), (10, 2)]
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = [ Periodic
               , Timeout $ (d 3 + 3 - dl 3) |*| 10
               , AddArmorMelee 1
               , AddArmorRanged 1 ]
  , ieffects = [Recharging (RefillCalm 1)]
  , ifeature = [ Durable, Precious, EqpSlot EqpSlotPeriodic "", Identified
               , toVelocity 50 ]  -- not dense enough
  , idesc    = "Highly ornamental, cold, large, steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
  , ikit     = []
  }
necklace = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(10, 3)]
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
  { iaspects = (Timeout $ (d 3 + 4 - dl 3) |*| 10) : iaspects necklace
  , ieffects = [Recharging (RefillHP 1)]
  , idesc    = "A cord of dried herbs and healing berries."
  }
necklace2 = necklace
  { irarity  = [(2, 0), (10, 1)]
  , iaspects = (Timeout $ (d 3 + 3 - dl 3) |*| 10) : iaspects necklace
  , ieffects = [ Recharging (Impress)
               , Recharging (DropItem COrgan "temporary conditions" True)
               , Recharging (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , Recharging (Explode "waste") ]
  }
necklace3 = necklace
  { iaspects = (Timeout $ (d 3 + 3 - dl 3) |*| 10) : iaspects necklace
  , ieffects = [Recharging (Paralyze $ 5 + d 5 + dl 5)]
  }
necklace4 = necklace
  { iaspects = (Timeout $ (d 4 + 4 - dl 4) |*| 2) : iaspects necklace
  , ieffects = [Recharging (Teleport $ d 3 |*| 3)]
  }
necklace5 = necklace
  { iaspects = (Timeout $ (d 3 + 4 - dl 3) |*| 10) : iaspects necklace
  , ieffects = [Recharging (Teleport $ 12 + d 3 |*| 3)]
  }
necklace6 = necklace
  { iaspects = (Timeout $ d 4 |*| 10) : iaspects necklace
  , ieffects = [Recharging (PushActor (ThrowMod 100 50))]
  }
necklace7 = necklace  -- TODO: teach AI to wear only for fight
  { irarity  = [(4, 0), (10, 2)]
  , iaspects = [AddSpeed $ d 2, Timeout $ (d 3 + 3 + dl 3) |*| 2]
               ++ iaspects necklace
  , ieffects = [Recharging (RefillHP (-1))]
  }

-- * Non-periodic jewelry

monocle = ItemKind
  { isymbol  = symbolRing
  , iname    = "monocle"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(5, 0), (10, 1)]
  , iverbHit = "rap"
  , iweight  = 50
  , iaspects = [AddSight $ d 2, AddHurtMelee $ d 2 |*| 3]
  , ieffects = []
  , ifeature = [Precious, Identified, Durable, EqpSlot EqpSlotAddSight ""]
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
  , ieffects = [Explode "blast 2"]
  , ifeature = [Precious, Identified]
  , idesc    = "It looks like an ordinary object, but it's in fact a generator of exceptional effects: adding to some of your natural abilities and subtracting from others. You'd profit enormously if you could find a way to multiply such generators."
  , ikit     = []
  }
ring1 = ring
  { irarity  = [(10, 2)]
  , iaspects = [AddSpeed $ d 2, AddMaxHP $ dl 3 - 5 - d 3]
  , ieffects = [Explode "distortion"]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddSpeed ""]
  }
ring2 = ring
  { iaspects = [AddMaxHP $ 3 + dl 5, AddMaxCalm $ dl 6 - 15 - d 6]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddMaxHP ""]
  }
ring3 = ring
  { iaspects = [AddMaxCalm $ 10 + dl 10]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddMaxCalm ""]
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring4 = ring
  { irarity  = [(3, 6), (10, 6)]
  , iaspects = [AddHurtMelee $ (d 5 + dl 5) |*| 3, AddMaxHP $ dl 3 - 4 - d 2]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddHurtMelee ""]
  }
ring5 = ring  -- by the time it's found, probably no space in eqp
  { irarity  = [(5, 0)]
  , iaspects = [AddLight $ d 2]
  , ieffects = [Explode "distortion"]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddLight ""]
  , idesc    = "A sturdy ring with a large, shining stone."
  }

-- * Exploding consumables, often intended to be thrown

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
  { ieffects = [ NoEffect "of rose water", Impress
               , OnSmash (ApplyPerfume), OnSmash (Explode "fragrance") ]
  }
potion2 = potion
  { ifreq    = [("useful", 10)]  -- extremely rare
  , irarity  = [(1, 1)]
  , ieffects = [ NoEffect "of attraction", OnSmash (Explode "pheromone")]
  }
potion3 = potion
  { irarity  = [(1, 5), (10, 5)]
  , ieffects = [RefillHP 5, OnSmash (Explode "healing mist")]
  }
potion4 = potion
  { irarity  = [(10, 5)]
  , ieffects = [RefillHP 10, OnSmash (Explode "healing mist 2")]
  }
potion5 = potion
  { ieffects = [ OneOf [Impress, DropBestWeapon, RefillHP 5, Burn 3]
               , OnSmash (OneOf [ Explode "healing mist"
                                , Explode "wounding mist"
                                , Explode "fragrance"
                                , Explode "blast 10" ]) ]
  }
potion6 = potion
  { irarity  = [(3, 3), (10, 6)]
  , ieffects = [ OneOf [ Dominate, DropBestWeapon, RefillHP 20, Burn 9
                       , InsertMove 4 ]
               , OnSmash (OneOf [ Explode "healing mist 2"
                                , Explode "healing mist 2"
                                , Explode "pheromone"
                                , Explode "distortion"  -- outlier, OK
                                , Explode "blast 20" ]) ]
  }
potion7 = potion  -- used only as initial equipment; count betrays identity
  { ifreq    = [("useful", 30), ("potion of glue", 1)]
  , icount   = 1 + d 2
  , irarity  = [(1, 1)]
  , ieffects = [ NoEffect "of glue", Paralyze (5 + d 5)
               , OnSmash (Explode "glue")]
  , ifeature = [Identified]
  }
potion8 = potion
  { ieffects = [ DropItem COrgan "poisoned" True, RefillHP 1
               , OnSmash (Explode "antidote mist") ]
  }
potion9 = potion
  { ieffects = [ DropItem COrgan "temporary conditions" True, RefillHP 2
               , OnSmash (Explode "blast 10") ]
  }

-- * Exploding consumables, with temporary aspects
-- TODO: dip projectiles in those
-- TODO: add flavour and realism as in, e.g., "flask of whiskey",
-- which is more flavourful and believable than "flask of strength"

flask = ItemKind
  { isymbol  = symbolFlask
  , iname    = "flask"
  , ifreq    = [("useful", 100)]
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
  { ieffects = [ NoEffect "of red paint"
               , toOrganGameTurn "painted red" (20 + d 5)
               , OnSmash (Explode "red paint") ]
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
               , OnSmash (Explode "slowness spray") ]
  }
flask7 = flask  -- sight can be reduced from Calm, drunk, etc.
  { irarity  = [(10, 7)]
  , ieffects = [ NoEffect "of eye drops"
               , toOrganActorTurn "far-sighted" (20 + d 5)
               , OnSmash (Explode "eye drop") ]
  }
flask8 = flask
  { irarity  = [(10, 3)]
  , ieffects = [ NoEffect "of smelly concoction"
               , toOrganActorTurn "keen-smelling" (20 + d 5)
               , OnSmash (Explode "smelly droplet") ]
  }
flask9 = flask
  { ieffects = [ NoEffect "of bait cocktail", toOrganActorTurn "drunk" (5 + d 5)
               , OnSmash (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , OnSmash (Explode "waste") ]
  }
flask10 = flask
  { ieffects = [ NoEffect "of whiskey", toOrganActorTurn "drunk" (20 + d 5)
               , Burn 2, RefillHP 4, OnSmash (Explode "whiskey spray") ]
  }
flask11 = flask
  { irarity  = [(1, 20), (10, 6)]
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
flask14 = flask  -- but not flask of Calm depletion, since Calm reduced often
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
  { irarity  = [(1, 2), (10, 3)]
  , ieffects = [CallFriend 1]
  }
scroll2 = scroll
  { irarity  = [(1, 7), (10, 5)]
  , ieffects = [NoEffect "of fireworks", Explode "firecracker 7"]
  }
scroll3 = scroll
  { irarity  = [(1, 5), (10, 3)]
  , ieffects = [Ascend (-1)]
  }
scroll4 = scroll
  { ieffects = [OneOf [ Teleport $ d 3 |*| 3, RefillCalm 10, RefillCalm (-10)
                      , InsertMove 3, Paralyze 10, Identify CGround ]]
  }
scroll5 = scroll
  { irarity  = [(3, 3), (10, 6)]
  , ieffects = [OneOf [ Summon standardSummon $ d 2
                      , CallFriend 1, Ascend (-1), Ascend 1
                      , RefillCalm 30, RefillCalm (-30)
                      , CreateItem CGround "useful" TimerNone
                      , PolyItem CGround ]]
               -- TODO: ask player: Escape 1
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
scroll9 = scroll
  { irarity  = [(1, 15)]
  , ieffects = [Identify CGround]  -- TODO: ask player: AskPlayer cstore eff?
  }
scroll10 = scroll
  { irarity  = [(10, 10)]
  , ieffects = [PolyItem CGround]
  }

standardSummon :: Freqs ItemKind
standardSummon = [("monster", 30), ("mobile animal", 70)]

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
               , AddArmorMelee $ (d 2 + dl 3) |*| 5
               , AddArmorRanged $ (d 2 + dl 3) |*| 5 ]
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
               , AddArmorMelee $ (1 + d 2 + dl 4) |*| 5
               , AddArmorRanged $ (1 + d 2 + dl 4) |*| 5 ]
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
  , iaspects = [ AddHurtMelee $ (d 2 + dl 10) * 3
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
  , iaspects = [ AddArmorMelee $ (1 + dl 2) |*| 5
               , AddArmorRanged $ (1 + dl 2) |*| 5 ]
  , idesc    = "Long leather gauntlet covered in overlapping steel plates."
  }
gloveJousting = gloveFencing
  { iname    = "jousting gauntlet"
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 3), (10, 3)]
  , iweight  = 500
  , iaspects = [ AddHurtMelee $ (dl 4 - 6) |*| 3
               , AddArmorMelee $ (2 + dl 2) |*| 5
               , AddArmorRanged $ (2 + dl 2) |*| 5 ]
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
               , Timeout $ (d 3 + 3 - dl 3) |*| 2 ]
  , ieffects = []  -- [Recharging (PushActor (ThrowMod 200 50))]
  , ifeature = [ toVelocity 30  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Heavy and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too small to intercept projectiles with."
  , ikit     = []
  }
shield = buckler
  { iname    = "shield"
  , irarity  = [(7, 5)]
  , iflavour = zipPlain [Green]
  , iweight  = 3000
  , iaspects = [ AddArmorMelee 80
               , AddHurtMelee (-70)
               , Timeout $ (d 3 + 6 - dl 3) |*| 2 ]
  , ieffects = []  -- [Recharging (PushActor (ThrowMod 400 50))]
  , ifeature = [ toVelocity 20  -- unwieldy to throw and blunt
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
  , irarity  = [(1, 12), (10, 4)]
  , iverbHit = "stab"
  , iweight  = 1000
  , iaspects = [AddHurtMelee $ (d 3 + dl 3) |*| 3, AddArmorMelee $ d 2 |*| 5]
  , ieffects = [Hurt (6 * d 1)]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "A short dagger for thrusting and parrying blows. Does not penetrate deeply, but is hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = dagger
  { ifreq    = [("useful", 30)]
  , irarity  = [(1, 1), (10, 2)]
  -- The timeout has to be small, so that the player can count on the effect
  -- occuring consistently in any longer fight. Otherwise, the effect will be
  -- absent in some important fights, leading to the feeling of bad luck,
  -- but will manifest sometimes in fights where it doesn't matter,
  -- leading to the feeling of wasted power.
  -- If the effect is very powerful and so the timeout has to be significant,
  -- let's make it really large, for the effect to occur only once in a fight:
  -- as soon as the item is equipped, or just on the first strike.
  , iaspects = iaspects dagger ++ [Timeout $ (d 3 + 4 - dl 3) |*| 2]
  , ieffects = ieffects dagger ++ [Recharging DropBestWeapon]
  , idesc    = "A double dagger that a focused fencer can use to catch and twist an opponent's blade occasionally."
  }
hammer = ItemKind
  { isymbol  = symbolHafted
  , iname    = "war hammer"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(4, 12), (10, 2)]
  , iverbHit = "club"
  , iweight  = 1500
  , iaspects = [AddHurtMelee $ (d 2 + dl 2) |*| 3]
  , ieffects = [Hurt (8 * d 1)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the sharp tip costs
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "It may not cause grave wounds, but neither does it glance off nor ricochet. Great sidearm for opportunistic blows against armored foes."
  , ikit     = []
  }
hammerParalyze = hammer
  { ifreq    = [("useful", 30)]
  , irarity  = [(4, 1), (10, 2)]
  , iaspects = iaspects hammer ++ [Timeout $ (d 2 + 3 - dl 2) |*| 2]
  , ieffects = ieffects hammer ++ [Recharging $ Paralyze 5]
  }
hammerSpark = hammer
  { iname    = "smithhammer"
  , ifreq    = [("useful", 30)]
  , irarity  = [(4, 1), (10, 2)]
  , iaspects = iaspects hammer ++ [Timeout $ (d 4 + 4 - dl 4) |*| 2]
  , ieffects = ieffects hammer ++ [Recharging $ Explode "spark"]
  }
sword = ItemKind
  { isymbol  = symbolEdged
  , iname    = "sword"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(3, 1), (6, 16), (10, 8)]
  , iverbHit = "slash"
  , iweight  = 2000
  , iaspects = []
  , ieffects = [Hurt (10 * d 1)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the tip costs speed
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "Difficult to master; deadly when used effectively. The steel is particularly hard and keen, but rusts quickly without regular maintenance."
  , ikit     = []
  }
swordImpress = sword
  { ifreq    = [("useful", 30)]
  , irarity  = [(3, 1), (10, 2)]
  , iaspects = iaspects sword ++ [Timeout $ (d 4 + 5 - dl 4) |*| 2]
  , ieffects = ieffects sword ++ [Recharging Impress]
  , idesc    = "A particularly well-balance blade, lending itself to impressive shows of fencing skill."
  }
swordNullify = sword
  { ifreq    = [("useful", 30)]
  , irarity  = [(3, 0), (10, 1)]
  , iaspects = iaspects sword ++ [Timeout $ (d 4 + 5 - dl 4) |*| 2]
  , ieffects = ieffects sword
               ++ [Recharging $ DropItem COrgan "temporary conditions" True]
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
  , iaspects = [AddArmorMelee $ (1 + dl 3) |*| 5]
  , ieffects = [Hurt (12 * d 1)]
  , ifeature = [ toVelocity 20  -- not balanced
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "An improvised but deadly weapon made of a blade from a scythe attached to a long pole."
  , ikit     = []
  }
halberdPushActor = halberd
  { iname    = "halberd"
  , ifreq    = [("useful", 30)]
  , irarity  = [(7, 1), (10, 2)]
  , iaspects = iaspects halberd ++ [Timeout $ (d 5 + 5 - dl 5) |*| 2]
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
  , ifreq    = [("treasure", 100)]
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = []
  , iverbHit = "tap"
  , iweight  = 50
  , iaspects = [AddLight 1, AddSpeed (-1)]  -- reflects strongly, distracts
  , ieffects = []
  , ifeature = [Precious]
  , idesc    = "Useless, and still worth around 100 gold each. Would gems of thought and pearls of artful design be valued that much in our age of Science and Progress!"
  , ikit     = []
  }
gem1 = gem
  { irarity  = [(2, 0), (10, 10)]
  }
gem2 = gem
  { irarity  = [(4, 0), (10, 15)]
  }
gem3 = gem
  { irarity  = [(6, 0), (10, 20)]
  }
currency = ItemKind
  { isymbol  = symbolGold
  , iname    = "gold piece"
  , ifreq    = [("treasure", 100), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + d 20 + dl 20
  , irarity  = [(1, 0), (2, 15), (5, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 31
  , iaspects = []
  , ieffects = []
  , ifeature = [Identified, Precious]
  , idesc    = "Reliably valuable in every civilized plane of existence."
  , ikit     = []
  }
