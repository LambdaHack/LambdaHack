-- | Item definitions.
module Content.ItemKind
  ( content, items, otherItemContent
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindEmbed
import Content.ItemKindOrgan
import Content.ItemKindTemporary
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.ItemAspect (Aspect (..), EqpSlot (..))
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

content :: [ItemKind]
content = items ++ otherItemContent

otherItemContent :: [ItemKind]
otherItemContent = embeds ++ actors ++ organs ++ blasts ++ temporaries

items :: [ItemKind]
items =
  [sandstoneRock, dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, light1, light2, light3, blanket, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, flask18, flask19, flask20, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklaceTemplate, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, shield2, shield3, dagger, daggerDropBestWeapon, hammer, hammer2, hammer3, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberd2, halberd3, halberdPushActor, wandTemplate, wand1, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency]

sandstoneRock,    dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, light1, light2, light3, blanket, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, flask18, flask19, flask20, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklaceTemplate, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, shield2, shield3, dagger, daggerDropBestWeapon, hammer, hammer2, hammer3, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberd2, halberd3, halberdPushActor, wandTemplate, wand1, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency :: ItemKind

-- Keep the dice rolls and sides in aspects small so that not too many
-- distinct items are generated (for display in item lore and for narrative
-- impact ("oh, I found the more powerful of the two variants of the item!",
-- instead of "hmm, I found one of the countless variants, a decent one").
-- In particular, for unique items, unless they inherit aspects from
-- a standard item, permit only a couple possible variants.
-- This is especially important if an item kind has mulitple random aspects.
-- Instead multiply dice results, e.g., (1 `d` 3) * 5 instead of 1 `d` 15.
--
-- Beware of non-periodic non-weapon durable items with beneficial effects
-- and low timeout -- AI will starve applying such an item incessantly.

-- * Item group symbols, partially from Nethack

symbolProjectile, _symbolLauncher, symbolLight, symbolTool, symbolSpecial, symbolGold, symbolNecklace, symbolRing, symbolPotion, symbolFlask, symbolScroll, symbolTorsoArmor, symbolMiscArmor, _symbolClothes, symbolShield, symbolPolearm, symbolEdged, symbolHafted, symbolWand, _symbolStaff, symbolFood :: Char

symbolProjectile = '|'
_symbolLauncher  = '}'
symbolLight      = '('
symbolTool       = '('
symbolSpecial    = '*'  -- don't overuse, because it clashes with projectiles
symbolGold       = '$'  -- also gems
symbolNecklace   = '"'
symbolRing       = '='
symbolPotion     = '!'  -- concoction, bottle, jar, vial, canister
symbolFlask      = '!'
symbolScroll     = '?'  -- book, note, tablet, remote, chip, card
symbolTorsoArmor = '['
symbolMiscArmor  = '['
_symbolClothes   = '['
symbolShield     = ']'
symbolPolearm    = ')'
symbolEdged      = ')'
symbolHafted     = ')'
symbolWand       = '/'  -- magical rod, transmitter, pistol, rifle, instrument
_symbolStaff     = '_'  -- scanner
symbolFood       = ','  -- also body part; distinct from floor: not middle dot

-- * Thrown weapons

sandstoneRock = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sandstone rock"
  , ifreq    = [("sandstone rock", 1), ("weak arrow", 10)]
  , iflavour = zipPlain [Green]
  , icount   = 1 `d` 2
  , irarity  = [(1, 50), (10, 1)]
  , iverbHit = "hit"
  , iweight  = 300
  , idamage  = 1 `d` 1
  , iaspects = [AddHurtMelee $ -16 * 5]
  , ieffects = []
  , ifeature = [toVelocity 70, Fragile]  -- not dense, irregular
  , idesc    = "A lump of brittle sandstone rock."
  , ikit     = []
  }
dart = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "dart"
  , ifreq    = [("common item", 100), ("any arrow", 50), ("weak arrow", 50)]
  , iflavour = zipPlain [BrRed]
  , icount   = 4 `dL` 5
  , irarity  = [(1, 20), (10, 10)]
  , iverbHit = "prick"
  , iweight  = 40
  , idamage  = 1 `d` 1
  , iaspects = [AddHurtMelee $ (-15 + 1 `d` 2 + 1 `dL` 3) * 5]
                 -- only leather-piercing
  , ieffects = []
  , ifeature = []
  , idesc    = "A sharp delicate dart with fins."
  , ikit     = []
  }
spike = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "spike"
  , ifreq    = [("common item", 100), ("any arrow", 50), ("weak arrow", 50)]
  , iflavour = zipPlain [Cyan]
  , icount   = 4 `dL` 5
  , irarity  = [(1, 10), (10, 15)]
  , iverbHit = "nick"
  , iweight  = 150
  , idamage  = 2 `d` 1
  , iaspects = [AddHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5]
                 -- heavy vs armor
  , ieffects = [ Explode "single spark"  -- when hitting enemy
               , OnSmash (Explode "single spark") ]  -- at wall hit
      -- this results in a wordy item synopsis, but it's OK, the spark really
      -- is useful in some situations, not just a flavour
  , ifeature = [MinorEffects, toVelocity 70]  -- hitting with tip costs speed
  , idesc    = "A cruel long nail with small head."  -- "Much inferior to arrows though, especially given the contravariance problems."  -- funny, but destroy the suspension of disbelief; this is supposed to be a Lovecraftian horror and any hilarity must ensue from the failures in making it so and not from actively trying to be funny; also, mundane objects are not supposed to be scary or transcendental; the scare is in horrors from the abstract dimension visiting our ordinary reality; without the contrast there's no horror and no wonder, so also the magical items must be contrasted with ordinary XIX century and antique items
  , ikit     = []
  }
spike2 = spike
  { ifreq    = [("common item", 2), ("any arrow", 1), ("weak arrow", 1)]
  , iweight  = 200
  , idamage  = 4 `d` 1
  -- , idesc    = ""
  }
slingStone = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sling stone"
  , ifreq    = [("common item", 5), ("any arrow", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 3 `dL` 4
  , irarity  = [(1, 1), (10, 20)]
  , iverbHit = "hit"
  , iweight  = 200
  , idamage  = 1 `d` 1
  , iaspects = [AddHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5]
                 -- heavy vs armor
  , ieffects = [ Explode "single spark"  -- when hitting enemy
               , OnSmash (Explode "single spark") ]  -- at wall hit
  , ifeature = [MinorEffects, toVelocity 150]
  , idesc    = "A round stone, carefully sized and smoothed to fit the pouch of a standard string and cloth sling."
  , ikit     = []
  }
slingBullet = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sling bullet"
  , ifreq    = [("common item", 5), ("any arrow", 100)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 6 `dL` 4
  , irarity  = [(1, 1), (10, 15)]
  , iverbHit = "hit"
  , iweight  = 28
  , idamage  = 1 `d` 1
  , iaspects = [AddHurtMelee $ (-17 + 1 `d` 2 + 1 `dL` 3) * 5]
                 -- not armor-piercing
  , ieffects = []
  , ifeature = [toVelocity 200]
  , idesc    = "Small almond-shaped leaden projectile that weighs more than the sling used to tie the bag. It doesn't drop out of the sling's pouch when swung and doesn't snag when released."
  , ikit     = []
  }

-- * Exotic thrown weapons

-- Identified, because shape (and name) says it all. Detailed stats id by use.
paralizingProj = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "bolas set"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1 `dL` 4
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "entangle"
  , iweight  = 500
  , idamage  = 1 `d` 1
  , iaspects = [AddHurtMelee $ -14 * 5]
  , ieffects = [Paralyze 15, DropBestWeapon]
  , ifeature = []
  , idesc    = "Wood balls tied with hemp rope. The target enemy is tripped and bound to drop the main weapon, while fighting for balance."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [("common item", 100), ("harpoon", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dL` 5
  , irarity  = [(10, 10)]
  , iverbHit = "hook"
  , iweight  = 750
  , idamage  = 5 `d` 1
  , iaspects = [AddHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5]
  , ieffects = [PullActor (ThrowMod 200 50)]
  , ifeature = []
  , idesc    = "The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
harpoon2 = harpoon
  { ifreq    = [("common item", 2), ("harpoon", 2)]
  , iweight  = 1000
  , idamage  = 10 `d` 1
  -- , idesc    = ""
  }
net = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "net"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1 `dL` 3
  , irarity  = [(3, 5), (10, 4)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , idamage  = 2 `d` 1
  , iaspects = [AddHurtMelee $ -14 * 5]
  , ieffects = [ toOrganBad "slowed" (3 + 1 `d` 3)
               , DropItem maxBound 1 CEqp "torso armor"
               , SendFlying (ThrowMod 100 50) ]  -- make the drop painful
      -- only one of each kind is dropped, because no rubbish in this group
  , ifeature = []
  , idesc    = "A wide net with weights along the edges. Entangles armor and restricts movement."
  , ikit     = []
  }

-- * Lights

light1 = ItemKind
  { isymbol  = symbolLight
  , iname    = "wooden torch"
  , ifreq    = [ ("common item", 100), ("light source", 100)
               , ("wooden torch", 1) ]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dL` 4
  , irarity  = [(1, 15)]
  , iverbHit = "scorch"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddShine 3       -- not only flashes, but also sparks,
               , AddSight (-2) ]  -- so unused by AI due to the mixed blessing
  , ieffects = [Burn 1]
  , ifeature = [Lobable, Equipable, EqpSlot EqpSlotLightSource]
                 -- not Fragile; reusable flare
  , idesc    = "A heavy smoking wooden torch, improvised using a cloth soaked in tar, burning in an unsteady glow."
  , ikit     = []
  }
light2 = ItemKind
  { isymbol  = symbolLight
  , iname    = "oil lamp"
  , ifreq    = [("common item", 100), ("light source", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1 `dL` 2
  , irarity  = [(6, 8)]
  , iverbHit = "burn"
  , iweight  = 1500
  , idamage  = 1 `d` 1
  , iaspects = [AddShine 3, AddSight (-1)]
  , ieffects = [Burn 1, Paralyze 6, OnSmash (Explode "burning oil 2")]
  , ifeature = [Lobable, Fragile, Equipable, EqpSlot EqpSlotLightSource ]
  , idesc    = "A clay lamp filled with plant oil feeding a tiny wick."
  , ikit     = []
  }
light3 = ItemKind
  { isymbol  = symbolLight
  , iname    = "brass lantern"
  , ifreq    = [("common item", 100), ("light source", 100)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(10, 6)]
  , iverbHit = "burn"
  , iweight  = 3000
  , idamage  = 4 `d` 1
  , iaspects = [AddShine 4, AddSight (-1)]
  , ieffects = [Burn 1, Paralyze 8, OnSmash (Explode "burning oil 4")]
  , ifeature = [Lobable, Fragile, Equipable, EqpSlot EqpSlotLightSource]
  , idesc    = "Very bright and very heavy brass lantern."
  , ikit     = []
  }
blanket = ItemKind
  { isymbol  = symbolLight
  , iname    = "wool blanket"
  , ifreq    = [("common item", 100), ("light source", 100), ("blanket", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 3)]
  , iverbHit = "swoosh"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddShine (-10)  -- douses torch, lamp and lantern in one action
               , AddArmorMelee 1, AddMaxCalm 2 ]
  , ieffects = []
  , ifeature = [Lobable, Equipable]  -- not Fragile; reusable douse
  , idesc    = "Warm, comforting, and concealing, woven from soft wool."
  , ikit     = []
  }

-- * Exploding consumables, often intended to be thrown.

-- Not identified, because they are perfect for the id-by-use fun,
-- due to effects. They are fragile and upon hitting the ground explode
-- for effects roughly corresponding to their normal effects.
-- Whether to hit with them or explode them close to the tartget
-- is intended to be an interesting tactical decision.
--
-- Flasks are often not natural; maths, magic, distillery.
-- In reality, they just cover all conditions, which in turn matches
-- all aspects.
--
-- There is no flask nor temporary organ of Calm depletion,
-- because Calm reduced often via combat, etc..

flaskTemplate = ItemKind
  { isymbol  = symbolFlask
  , iname    = "flask"
  , ifreq    = [("flask unknown", 1)]
  , iflavour = zipLiquid darkCol ++ zipPlain darkCol ++ zipFancy darkCol
               ++ zipLiquid brightCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 7), (10, 3)]
  , iverbHit = "splash"
  , iweight  = 500
  , idamage  = 0
  , iaspects = []
  , ieffects = []
  , ifeature = [ HideAs "flask unknown", Lobable, Fragile
               , toVelocity 50 ]  -- oily, bad grip
  , idesc    = "A flask of oily liquid of a suspect color. Something seems to be moving inside."
  , ikit     = []
  }
flask1 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 5)]
  , ieffects = [ toOrganGood "strengthened" (20 + 1 `d` 5)
               , toOrganNoTimer "regenerating"
               , OnSmash (Explode "dense shower") ]
  , ifeature = [ELabel "of strength renewal brew"] ++ ifeature flaskTemplate
  }
flask2 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , ieffects = [ toOrganBad "weakened" (20 + 1 `d` 5)
               , OnSmash (Explode "sparse shower") ]
  , ifeature = [ELabel "of weakness brew"] ++ ifeature flaskTemplate
  }
flask3 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , ieffects = [ toOrganGood "protected from melee" (20 + 1 `d` 5)
               , OnSmash (Explode "melee protective balm") ]
  , ifeature = [ELabel "of melee protective balm"] ++ ifeature flaskTemplate
  }
flask4 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , ieffects = [ toOrganGood "protected from ranged" (20 + 1 `d` 5)
               , OnSmash (Explode "ranged protective balm") ]
  , ifeature = [ELabel "of ranged protective balm"] ++ ifeature flaskTemplate
  }
flask5 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , ieffects = [ toOrganBad "defenseless" (20 + 1 `d` 5)
               , Impress
               , Detect DetectExit 20
               , OnSmash (Explode "PhD defense question") ]
  , ifeature = [ELabel "of PhD defense questions"] ++ ifeature flaskTemplate
  }
flask6 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 7)]
  , ieffects = [ toOrganGood "resolute" (200 + 1 `d` 50)
                   -- long, for scouting and has to recharge
               , RefillCalm 60  -- not to make it a drawback, via @calmEnough@
               , OnSmash (Explode "resolution dust") ]
  , ifeature = [ELabel "of resolution"] ++ ifeature flaskTemplate
  }
flask7 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , icount   = 1  -- too poweful en masse
  , ieffects = [ toOrganGood "hasted" (20 + 1 `d` 5)
               , OnSmash (Explode "haste spray") ]
  , ifeature = [ELabel "of haste brew"] ++ ifeature flaskTemplate
  }
flask8 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(1, 12), (10, 2)]
  , ieffects = [ toOrganBad "slowed" (20 + 1 `d` 5)
               , toOrganNoTimer "regenerating", toOrganNoTimer "regenerating"  -- x2
               , RefillCalm 5
               , OnSmash (Explode "slowness mist")
               , OnSmash (Explode "youth sprinkle") ]
  , ifeature = [ELabel "of lethargy brew"] ++ ifeature flaskTemplate
  }
flask9 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , ieffects = [ toOrganGood "far-sighted" (40 + 1 `d` 10)
               , OnSmash (Explode "eye drop") ]
  , ifeature = [ELabel "of eye drops"] ++ ifeature flaskTemplate
  }
flask10 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 2)]  -- not very useful right now
  , ieffects = [ toOrganGood "keen-smelling" (40 + 1 `d` 10)
               , Detect DetectActor 10
               , OnSmash (Explode "smelly droplet") ]
  , ifeature = [ELabel "of smelly concoction"] ++ ifeature flaskTemplate
  }
flask11 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 2)]  -- not very useful right now
  , ieffects = [ toOrganGood "shiny-eyed" (40 + 1 `d` 10)
               , OnSmash (Explode "eye shine") ]
  , ifeature = [ELabel "of cat tears"] ++ ifeature flaskTemplate
  }
flask12 = flaskTemplate
  { iname    = "bottle"
  , ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , icount   = 1 `d` 3  -- the only one sometimes giving away its identity
  , ieffects = [ toOrganGood "drunk" (20 + 1 `d` 5)
               , Burn 1, RefillHP 3
               , OnSmash (Explode "whiskey spray") ]
  , ifeature = [ELabel "of whiskey"] ++ ifeature flaskTemplate
  }
flask13 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , icount   = 1
  , ieffects = [ toOrganGood "drunk" (20 + 1 `d` 5)
               , Burn 1, RefillHP 3
               , Summon "mobile animal" 1
               , OnSmash (Summon "mobile animal" 1)
               , OnSmash Impress
               , OnSmash (Explode "waste") ]
   , ifeature = [ELabel "of bait cocktail"] ++ ifeature flaskTemplate
 }
-- The player has full control over throwing the flask at his party,
-- so he can milk the explosion, so it has to be much weaker, so a weak
-- healing effect is enough. OTOH, throwing a harmful flask at many enemies
-- at once is not easy to arrange, so these explostions can stay powerful.
flask14 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(1, 2), (10, 10)]
  , ieffects = [ toOrganNoTimer "regenerating", toOrganNoTimer "regenerating"  -- x2
               , OnSmash (Explode "youth sprinkle") ]
  , ifeature = [ELabel "of regeneration brew"] ++ ifeature flaskTemplate
 }
flask15 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , ieffects = [ toOrganNoTimer "poisoned", toOrganNoTimer "poisoned"  -- x2
               , OnSmash (Explode "poison cloud") ]
  , ifeature = [ELabel "of poison"] ++ ifeature flaskTemplate
  }
flask16 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , ieffects = [ toOrganNoTimer "poisoned"
               , OnSmash (Explode "poison cloud") ]
  , ifeature = [ELabel "of weak poison"] ++ ifeature flaskTemplate
  }
flask17 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 3)]
  , ieffects = [ toOrganNoTimer "slow resistant"
               , OnSmash (Explode "anti-slow mist") ]
  , ifeature = [ELabel "of slow resistance"] ++ ifeature flaskTemplate
  }
flask18 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 3)]
  , ieffects = [ toOrganNoTimer "poison resistant"
               , OnSmash (Explode "antidote mist") ]
  , ifeature = [ELabel "of poison resistance"] ++ ifeature flaskTemplate
  }
flask19 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , ieffects = [ toOrganBad "blind" (40 + 1 `d` 10)
               , OnSmash (Explode "iron filing") ]
  , ifeature = [ELabel "of blindness"] ++ ifeature flaskTemplate
  }
flask20 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , ieffects = [ toOrganNoTimer "poisoned"
               , toOrganBad "weakened" (20 + 1 `d` 5)
               , toOrganBad "defenseless" (20 + 1 `d` 5)
               , OnSmash (Explode "glass hail") ]  -- enough glass to cause that
  , ifeature = [ELabel "of calamity"] ++ ifeature flaskTemplate
  }

-- Potions are often natural. Appear deeper than most flasks. Various
-- configurations of effects. A different class of effects is on scrolls
-- and mechanical items. Some are shared.

potionTemplate = ItemKind
  { isymbol  = symbolPotion
  , iname    = "potion"
  , ifreq    = [("potion unknown", 1)]
  , iflavour = zipLiquid brightCol ++ zipPlain brightCol ++ zipFancy brightCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "splash"
  , iweight  = 200
  , idamage  = 0
  , iaspects = []
  , ieffects = []
  , ifeature = [ HideAs "potion unknown", Lobable, Fragile
               , toVelocity 50 ]  -- oily, bad grip
  , idesc    = "A vial of bright, frothing concoction. The best that nature has to offer."
  , ikit     = []
  }
potion1 = potionTemplate
  { iname    = "vial"
  , ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , icount   = 3 `dL` 1  -- very useful, despite appearances
  , ieffects = [ Impress, RefillCalm (-5)
               , OnSmash ApplyPerfume, OnSmash (Explode "fragrance") ]
  , ifeature = [ELabel "of rose water"] ++ ifeature potionTemplate
  }
potion2 = potionTemplate
  { ifreq    = [("treasure", 100)]
  , icount   = 1
  , irarity  = [(5, 8), (10, 8)]
  , ieffects = [ Impress, RefillCalm (-20)
               , OnSmash (Explode "pheromone") ]
  , ifeature = [Unique, ELabel "of Attraction"]
               ++ [ Lobable, Fragile  -- identified
                  , toVelocity 50 ]
  -- , idesc    = ""
  }
potion3 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , ieffects = [ RefillHP 5, DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "healing mist") ]
  }
potion4 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , irarity  = [(1, 6), (10, 9)]
  , ieffects = [ RefillHP 10, DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "healing mist 2") ]
  }
potion5 = potionTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , icount   = 3 `dL` 1  -- always as many as possible on this level
                         -- without giving away potion identity
  , irarity  = [(1, 10)]
  , ieffects = [ OneOf [ RefillHP 10, RefillHP 5, Burn 5
                       , DropItem 1 maxBound COrgan "poisoned"
                       , toOrganGood "strengthened" (20 + 1 `d` 5) ]
               , OnSmash (OneOf [ Explode "dense shower"
                                , Explode "sparse shower"
                                , Explode "melee protective balm"
                                , Explode "ranged protective balm"
                                , Explode "PhD defense question" ]) ]
  }
potion6 = potionTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(10, 8)]
  , ieffects = [ Impress
               , OneOf [ RefillCalm (-60)
                       , RefillHP 20, RefillHP 10, Burn 10
                       , DropItem 1 maxBound COrgan "poisoned"
                       , toOrganGood "hasted" (20 + 1 `d` 5) ]
               , OnSmash (OneOf [ Explode "healing mist 2"
                                , Explode "wounding mist"
                                , Explode "distressing odor"
                                , Explode "haste spray"
                                , Explode "slowness mist"
                                , Explode "fragrance"
                                , Explode "violent flash" ]) ]
  }
potion7 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , ieffects = [ DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "antidote mist") ]
  }
potion8 = potionTemplate
  { iname    = "ampoule"  -- probably filled with nitroglycerine, but let's
                          -- not mix fantasy with too much technical jargon
  , ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , icount   = 3 `dL` 1
  , ieffects = [ DropItem 1 maxBound COrgan "condition"
               , OnSmash (Explode "violent concussion") ]
      -- not fragmentation nor glass hail, because not enough glass
  }
potion9 = potionTemplate
  { ifreq    = [("treasure", 100)]
  , icount   = 1
  , irarity  = [(10, 5)]
  , ieffects = [ RefillHP 60, Impress, RefillCalm (-60)
               , OnSmash (Explode "healing mist 2")
               , OnSmash (Explode "pheromone") ]
  , ifeature = [Unique, ELabel "of Love"]
               ++ [ Lobable, Fragile  -- identified
                  , toVelocity 50 ]
  -- , idesc    = ""
  }

-- * Explosives, with the only effect being @Explode@

fragmentationBomb = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "clay pot"
      -- clay pot filled with black powder; fragmentation comes from the clay
      -- shards, so it's not obvious if it's a weapon or just storage method;
      -- deflagration, not detonation, so large mass and hard container
      -- required not to burn harmlessly; improvised short fuze
  , ifreq    = [("common item", 100), ("explosive", 200)]
  , iflavour = zipPlain [Red]
  , icount   = 1 `dL` 4  -- many, because not very intricate
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 3000  -- low velocity due to weight
  , idamage  = 1 `d` 1  -- heavy and hard
  , iaspects = []
  , ieffects = [ Explode "focused fragmentation"
               , OnSmash (Explode "violent fragmentation") ]
  , ifeature = [ELabel "of black powder", Lobable, Fragile]
  , idesc    = "The practical application of science."
      -- given that we now have several kinds of explosives, tell something
      -- related to 'fragmentation', e.g., mention flying shards
  , ikit     = []
  }
concussionBomb = fragmentationBomb
  { iname    = "satchel"
      -- slightly stabilized nitroglycerine in a soft satchel, hence
      -- no fragmentation, but huge shock wave despite small size and lack
      -- of strong container to build up pressure; indoors help the shock wave;
      -- unstable enough that no fuze required
  , iflavour = zipPlain [Magenta]
  , iverbHit = "flap"
  , iweight  = 400
  , idamage  = 0
  , ieffects = [ Explode "focused concussion"
               , OnSmash (Explode "violent concussion") ]
  , ifeature = [ ELabel "of mining charges", Lobable, Fragile
               , toVelocity 70 ]  -- flappy and so slow
  , idesc    = ""
  }
-- Not flashbang, because powerful bang without fragmentation is harder
-- to manufacture (requires an oxidizer and steel canister with holes)
-- and because we don't model hearing adequately yet. The bang would also
-- paralyze and/or lower the movement skill (out of balance due to ear trauma).
flashBomb = fragmentationBomb
  { iname    = "magnesium ribbon"  -- filled with magnesium flash powder
  , iflavour = zipPlain [BrWhite]
  , iverbHit = "flash"
  , iweight  = 400
  , idamage  = 0
  , ieffects = [Explode "focused flash", OnSmash (Explode "violent flash")]
  , ifeature = [Lobable, Fragile, toVelocity 70]  -- bad shape for throwing
  , idesc    = ""
  }
firecrackerBomb = fragmentationBomb
  { iname = "roll"  -- not fireworks, as they require outdoors
  , iflavour = zipPlain [BrMagenta]
  , irarity  = [(1, 5), (5, 5)]  -- a toy, if deadly
  , iverbHit = "crack"  -- a pun, matches the verb from "ItemKindBlast"
  , iweight  = 1000
  , idamage  = 0
  , ieffects = [Explode "firecracker", OnSmash (Explode "firecracker")]
  , ifeature = [Lobable, Fragile]
  , idesc = "String and paper, concealing a deadly surprise."
  }

-- * Non-exploding consumables, not specifically designed for throwing

scrollTemplate = ItemKind
  { isymbol  = symbolScroll
  , iname    = "scroll"
  , ifreq    = [("scroll unknown", 1)]
  , iflavour = zipFancy stdCol ++ zipPlain darkCol  -- arcane and old
  , icount   = 1 `dL` 3
  , irarity  = [(1, 14), (10, 7)]
  , iverbHit = "thump"
  , iweight  = 50
  , idamage  = 0
  , iaspects = []
  , ieffects = []
  , ifeature = [ HideAs "scroll unknown"
               , toVelocity 30 ]  -- bad shape, even rolled up
  , idesc    = "Scraps of haphazardly scribbled mysteries from beyond. Is this equation an alchemical recipe? Is this diagram an extradimensional map? Is this formula a secret call sign?"
  , ikit     = []
  }
scroll1 = scrollTemplate
  { ifreq    = [("treasure", 100)]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]  -- mixed blessing, so available early, often
  , ieffects = [Summon "hero" 1, Summon "mobile animal" (2 + 1 `d` 2)]
  , ifeature = [Unique, ELabel "of Reckless Beacon"] ++ ifeature scrollTemplate
  , idesc    = "The bright flame and sweet-smelling smoke of this heavily infused scroll should attract natural creatures inhabiting the area, including human survivors, if any."
  }
scroll2 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , irarity  = [(1, 2)]  -- mixed blessing
  , ieffects = [Detect DetectItem 20, Teleport 20, RefillCalm (-100)]
  , ifeature = [ELabel "of greed"] ++ ifeature scrollTemplate
  }
scroll3 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , irarity  = [(1, 4), (10, 2)]
  , ieffects = [Ascend False]
  }
scroll4 = scrollTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 14)]
  , ieffects = [OneOf [ Teleport 5, Paralyze 10, InsertMove 10
                      , Detect DetectEmbed 12, Detect DetectItem 20 ]]
  }
scroll5 = scrollTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(10, 11)]
  , ieffects = [ Impress
               , OneOf [ Teleport 20, Ascend False, Ascend True
                       , Summon "hero" 1, Summon "mobile animal" $ 1 `d` 2
                       , Detect DetectAll 40, RefillCalm (-100)
                       , CreateItem CGround "common item" timerNone ] ]
  }
scroll6 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , ieffects = [Teleport 5]
  }
scroll7 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , ieffects = [Teleport 20]
  }
scroll8 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , icount   = 1  -- too poweful en masse
  , irarity  = [(10, 4)]
  , ieffects = [InsertMove $ 1 + 1 `d` 2 + 1 `dL` 2]
  }
scroll9 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 14)]  -- uncommon deep down, where all is known
  , ieffects = [Composite [Identify, RefillCalm 10]]
  , ifeature = [ELabel "of scientific explanation"] ++ ifeature scrollTemplate
  , idesc    = "The most pressing existential concerns are met with a deeply satisfying scientific answer."
  }
scroll10 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , irarity  = [(10, 20)]  -- at endgame a crucial item may be missing
  , ieffects = [Composite [PolyItem, Explode "firecracker"]]
  , ifeature = [ELabel "of transfiguration"] ++ ifeature scrollTemplate
  }
scroll11 = scrollTemplate
  { ifreq    = [("treasure", 100)]
  , icount   = 1
  , irarity  = [(5, 8), (10, 8)]
  , ieffects = [Summon "hero" 1]
  , ifeature = [Unique, ELabel "of Rescue Proclamation"]
               ++ ifeature scrollTemplate
  , idesc    = "A survivor is found that enjoys, apparently, complete physiological integrity. If we so wish, we can pronounce him rescued and let him join our team."
  }
scroll12 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , irarity  = [(1, 9), (10, 4)]
  , ieffects = [Detect DetectHidden 20]
  }
scroll13 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , ieffects = [Detect DetectActor 20]
  , ifeature = [ELabel "of acute hearing"] ++ ifeature scrollTemplate
  }

-- * Assorted tools

jumpingPole = ItemKind
  { isymbol  = symbolWand
  , iname    = "jumping pole"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 3)]
  , iverbHit = "prod"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [Timeout $ (2 + 1 `d` 2 - 1 `dL` 2) * 5]
  , ieffects = [Recharging (toOrganGood "hasted" 1)]
                 -- safe for AI, because it speeds up, so when AI applies it
                 -- again and again, it gets its time back and is not stuck;
                 -- in total, the explorations speed is unchanged,
                 -- but it's useful when fleeing in the dark to make distance
                 -- and when initiating combat, so it's OK that AI uses it
  , ifeature = [Durable]
  , idesc    = "Makes you vulnerable at take-off, but then you are free like a bird."
  , ikit     = []
  }
sharpeningTool = ItemKind
  { isymbol  = symbolTool
  , iname    = "whetstone"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(10, 10)]
  , iverbHit = "smack"
  , iweight  = 400
  , idamage  = 0
  , iaspects = [AddHurtMelee $ (1 `dL` 5) * 5]
  , ieffects = []
  , ifeature = [Equipable, EqpSlot EqpSlotAddHurtMelee]
  , idesc    = "A portable sharpening stone that lets you fix your weapons between or even during fights, without the need to set up camp, fish out tools and assemble a proper sharpening workshop."
  , ikit     = []
  }
seeingItem = ItemKind
  { isymbol  = symbolFood
  , iname    = "pupil"
  , ifreq    = [("common item", 30)]  -- spooky and wierd, so rare
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "gaze at"
  , iweight  = 100
  , idamage  = 0
  , iaspects = [ AddSight 10, AddMaxCalm 30, AddShine 2
               , Timeout $ 1 + 1 `d` 2 ]
  , ieffects = [ Recharging (toOrganNoTimer "poisoned")
               , Recharging (Summon "mobile monster" 1) ]
  , ifeature = [Periodic]
  , idesc    = "A slimy, dilated green pupil torn out from some giant eye. Clear and focused, as if still alive."
  , ikit     = []
  }
motionScanner = ItemKind
  { isymbol  = symbolTool
  , iname    = "draft detector"
  , ifreq    = [("common item", 100), ("add nocto 1", 20)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "jingle"
  , iweight  = 300
  , idamage  = 0
  , iaspects = [ AddNocto 1
               , AddArmorMelee (-10 + 1 `dL` 5)
               , AddArmorRanged (-10 + 1 `dL` 5) ]
  , ieffects = []
  , ifeature = [Equipable, EqpSlot EqpSlotMiscBonus]
  , idesc    = "A silk flag with a bell for detecting sudden draft changes. May indicate a nearby corridor crossing or a fast enemy approaching in the dark. Is also very noisy."
  , ikit     = []
  }

-- * Periodic jewelry

gorget = necklaceTemplate
  { iname    = "Old Gorget"
  , ifreq    = [("common item", 25), ("treasure", 25)]
  , iflavour = zipFancy [BrCyan]  -- looks exactly the same as on of necklaces,
                                  -- but it's OK, it's an artifact
  , irarity  = [(4, 3), (10, 3)]  -- weak, shallow
  , iaspects = [ Timeout $ (1 `d` 2) * 2
               , AddArmorMelee 3
               , AddArmorRanged 2 ]
  , ieffects = [Recharging (RefillCalm 1)]
  , ifeature = [Unique, Durable, EqpSlot EqpSlotMiscBonus]
               ++ ifeature necklaceTemplate
  , idesc    = "Highly ornamental, cold, large, steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
  }
-- Not idenfified, because id by use, e.g., via periodic activations. Fun.
necklaceTemplate = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [("necklace unknown", 1)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(10, 2)]
  , iverbHit = "whip"
  , iweight  = 30
  , idamage  = 0
  , iaspects = [Timeout 1]  -- fake, but won't be displayed
  , ieffects = []
  , ifeature = [ Periodic, HideAs "necklace unknown", Precious, Equipable
               , toVelocity 50 ]  -- not dense enough
  , idesc    = "Menacing Greek symbols shimmer with increasing speeds along a chain of fine encrusted links. After a tense build-up, a prismatic arc shoots towards the ground and the iridescence subdues, becomes ordered and resembles a harmless ornament again, for a time."
  , ikit     = []
  }
necklace1 = necklaceTemplate
  { ifreq    = [("treasure", 100), ("any jewelry", 100)]
  , iaspects = [Timeout $ (1 `d` 2) * 20]
  , ieffects = [Recharging (RefillHP 1)] ++ ieffects necklaceTemplate
  , ifeature = [Unique, ELabel "of Aromata", Durable, EqpSlot EqpSlotMiscBonus]
               ++ ifeature necklaceTemplate
  , idesc    = "A cord of freshly dried herbs and healing berries."
  }
necklace2 = necklaceTemplate
  { ifreq    = [("treasure", 100), ("any jewelry", 100)]
      -- too nasty to call it just a "common item"
  , iaspects = [Timeout 30]
  , ieffects = [ Recharging (Summon "mobile animal" $ 1 `d` 2)
               , Recharging (Explode "waste")
               , Recharging Impress
               , Recharging (DropItem 1 maxBound COrgan "condition") ]
               ++ ieffects necklaceTemplate
  , ifeature = [Unique, ELabel "of Live Bait", Durable]
               ++ ifeature necklaceTemplate
  -- , idesc    = ""
  }
necklace3 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = [Timeout $ (1 `d` 2) * 20]
  , ieffects = [ Recharging (Detect DetectActor 10)
               , Recharging (RefillCalm (-20)) ]
               ++ ieffects necklaceTemplate
  , ifeature = [ELabel "of fearful listening"] ++ ifeature necklaceTemplate
  }
necklace4 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = [Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 2]
  , ieffects = [Recharging (Teleport $ 3 `d` 2)]
               ++ ieffects necklaceTemplate
  }
necklace5 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = [Timeout $ (7 - 1 `dL` 5) * 10]
  , ieffects = [ Recharging (Teleport $ 14 + 3 `d` 3)
               , Recharging (Detect DetectExit 20)
               , Recharging (RefillHP (-2)) ]  -- prevent micromanagement
               ++ ieffects necklaceTemplate
  , ifeature = [ELabel "of escape"] ++ ifeature necklaceTemplate
  }
necklace6 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = [Timeout $ 1 + (1 `d` 3) * 2]
  , ieffects = [Recharging (PushActor (ThrowMod 100 50))]  -- 1 step, slow
                  -- the @50@ is only for the case of very light actor, etc.
               ++ ieffects necklaceTemplate
  }
necklace7 = necklaceTemplate
  { ifreq    = [("treasure", 100), ("any jewelry", 100)]
  , iaspects = [AddMaxHP 15, AddArmorMelee 20, AddArmorRanged 10, Timeout 4]
  , ieffects = [ Recharging (InsertMove $ 1 `d` 3)  -- unpredictable
               , Recharging (RefillCalm (-1))  -- fake "hears something" :)
               , Recharging (RefillHP (-1)) ]
               ++ ieffects necklaceTemplate
  , ifeature = [Unique, ELabel "of Overdrive", Durable, EqpSlot EqpSlotAddSpeed]
               ++ ifeature necklaceTemplate
  -- , idesc    = ""
  }
necklace8 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = [Timeout $ (1 + 1 `d` 3) * 5]
  , ieffects = [Recharging $ Explode "spark"]
               ++ ieffects necklaceTemplate
  }
necklace9 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = [Timeout $ (1 + 1 `d` 3) * 5]
  , ieffects = [Recharging $ Explode "fragrance"]
               ++ ieffects necklaceTemplate
  }

-- * Non-periodic jewelry

imageItensifier = ItemKind
  { isymbol  = symbolRing
  , iname    = "light cone"
  , ifreq    = [("treasure", 100), ("add nocto 1", 80)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "bang"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [AddNocto 1, AddSight (-1), AddArmorMelee $ (1 `dL` 3) * 3]
  , ieffects = []
  , ifeature = [Precious, Durable, Equipable, EqpSlot EqpSlotMiscBonus]
  , idesc    = "Contraption of lenses and mirrors on a polished brass headband for capturing and strengthening light in dark environment. Hampers vision in daylight. Stackable."
  , ikit     = []
  }
sightSharpening = ringTemplate  -- small and round, so mistaken for a ring
  { iname    = "sharp monocle"
  , ifreq    = [("treasure", 10), ("add sight", 1)]
      -- it's has to be very rare, because it's powerful and not unique,
      -- and also because it looks exactly as one of necklaces, so it would
      -- be misleading when seen on the map
  , irarity  = [(7, 1), (10, 5)]  -- low @ifreq@
  , iweight  = 50  -- heavier that it looks, due to glass
  , iaspects = [AddSight $ 1 + 1 `d` 2, AddHurtMelee $ (1 `d` 2) * 3]
  , ifeature = [EqpSlot EqpSlotAddSight] ++ ifeature ringTemplate
  , idesc    = "Lets you better focus your weaker eye."
  }
-- Don't add standard effects to rings, because they go in and out
-- of eqp and so activating them would require UI tedium: looking for
-- them in eqp and inv or even activating a wrong item by mistake.
--
-- By general mechanisms, due to not having effects that could identify
-- them by observing the effect, rings are identified on pickup.
-- That's unlike necklaces, which provide the fun of id-by-use, because they
-- have effects and when the effects are triggered, they get identified.
ringTemplate = ItemKind
  { isymbol  = symbolRing
  , iname    = "ring"
  , ifreq    = [("ring unknown", 1)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 1)]  -- the default very low
  , iverbHit = "knock"
  , iweight  = 15
  , idamage  = 0
  , iaspects = []
  , ieffects = []
  , ifeature = [HideAs "ring unknown", Precious, Equipable]
  , idesc    = "It looks like an ordinary object, but it's in fact a generator of exceptional effects: adding to some of your natural qualities and subtracting from others."
  , ikit     = []
  }
ring1 = ringTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , irarity  = [(10, 4)]
  , iaspects = [AddSpeed $ 1 `d` 3, AddMaxHP (-15)]
  , ieffects = [OnSmash (Explode "distortion")]  -- high power
  , ifeature = [EqpSlot EqpSlotAddSpeed] ++ ifeature ringTemplate
  }
ring2 = ringTemplate
  { ifreq    = [("treasure", 100), ("any jewelry", 100)]
  , irarity  = [(10, 2)]
  , iaspects = [AddSpeed $ (1 `d` 2) * 3, AddMaxCalm (-40), AddMaxHP (-20)]
  , ieffects = [OnSmash (Explode "distortion")]  -- high power
  , ifeature = [Unique, ELabel "of Rush", Durable, EqpSlot EqpSlotAddSpeed]
               ++ ifeature ringTemplate
  -- , idesc    = ""
  }
ring3 = ringTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , irarity  = [(10, 11)]
  , iaspects = [ AddMaxHP $ 10 + (1 `dL` 5) * 2
               , AddMaxCalm $ -20 + (1 `dL` 5) * 2 ]
  , ifeature = [EqpSlot EqpSlotAddMaxHP] ++ ifeature ringTemplate
  }
ring4 = ringTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , irarity  = [(5, 1), (10, 14)]  -- needed after other rings drop Calm
  , iaspects = [AddMaxCalm $ 25 + (1 `dL` 4) * 5]
  , ifeature = [EqpSlot EqpSlotMiscBonus] ++ ifeature ringTemplate
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring5 = ringTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , irarity  = [(3, 3), (10, 6)]
  , iaspects = [ AddHurtMelee $ (2 + 1 `d` 2 + (1 `dL` 2) * 2 ) * 3
               , AddMaxHP $ (-2 - (1 `d` 2) + (1 `dL` 2) * 2) * 3 ]  -- !!!
  , ifeature = [EqpSlot EqpSlotAddHurtMelee] ++ ifeature ringTemplate
  }
ring6 = ringTemplate  -- by the time it's found, probably no space in eqp
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , irarity  = [(5, 0), (10, 4)]
  , iaspects = [AddShine $ 1 `d` 2]
  , ifeature = [EqpSlot EqpSlotLightSource] ++ ifeature ringTemplate
  , idesc    = "A sturdy ring with a large, shining stone."
  }
ring7 = ringTemplate
  { ifreq    = [("common item", 10), ("ring of opportunity sniper", 1) ]
  , irarity  = [(10, 5)]  -- low @ifreq@
  , iaspects = [AddAbility AbProject 8]
  , ieffects = [OnSmash (Explode "distortion")]  -- high power
  , ifeature = [ELabel "of opportunity sniper", EqpSlot EqpSlotAbProject]
               ++ ifeature ringTemplate
  }
ring8 = ringTemplate
  { ifreq    = [("common item", 1), ("ring of opportunity grenadier", 1) ]
  , irarity  = [(1, 1)]
  , iaspects = [AddAbility AbProject 11]
  , ieffects = [OnSmash (Explode "distortion")]  -- high power
  , ifeature = [ELabel "of opportunity grenadier", EqpSlot EqpSlotAbProject]
               ++ ifeature ringTemplate
  }

-- * Armor

armorLeather = ItemKind
  { isymbol  = symbolTorsoArmor
  , iname    = "leather armor"
  , ifreq    = [("common item", 100), ("torso armor", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 9), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 7000
  , idamage  = 0
  , iaspects = [ AddHurtMelee (-2)
               , AddArmorMelee $ (2 + 1 `dL` 4) * 5
               , AddArmorRanged $ (1 + 1 `dL` 2) * 3 ]
  , ieffects = []
  , ifeature = [Durable, Equipable, EqpSlot EqpSlotAddArmorMelee]
  , idesc    = "A stiff jacket formed from leather boiled in bee wax, padded linen and horse hair. Protects from anything that is not too sharp. Smells much better than the rest of your garment."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "mail armor"
  , ifreq    = [("common item", 100), ("torso armor", 1), ("armor ranged", 50) ]
  , iflavour = zipPlain [Cyan]
  , irarity  = [(6, 9), (10, 3)]
  , iweight  = 12000
  , idamage  = 0
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ (2 + 1 `dL` 4) * 5
               , AddArmorRanged $ (4 + 1 `dL` 2) * 3 ]
  , ieffects = []
  , ifeature = [Durable, Equipable, EqpSlot EqpSlotAddArmorRanged]
  , idesc    = "A long shirt woven from iron rings that are hard to pierce through. Discourages foes from attacking your torso, making it harder for them to hit you."
  }
gloveFencing = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "leather glove"
  , ifreq    = [("common item", 100), ("misc armor", 1), ("armor ranged", 50)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]
  , iverbHit = "flap"
  , iweight  = 100
  , idamage  = 1 `d` 1
  , iaspects = [ AddHurtMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddArmorRanged $ (1 `dL` 2) * 3 ]
  , ieffects = []
  , ifeature = [ toVelocity 50  -- flaps and flutters
               , Durable, Equipable, EqpSlot EqpSlotAddHurtMelee ]
  , idesc    = "A fencing glove from rough leather ensuring a good grip. Also quite effective in deflecting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "steel gauntlet"
  , ifreq    = [("common item", 100), ("misc armor", 1)]
  , iflavour = zipPlain [BrCyan]
  , irarity  = [(1, 9), (10, 3)]
  , iweight  = 300
  , idamage  = 2 `d` 1
  , iaspects = [AddArmorMelee $ (1 + 1 `dL` 4) * 5]
  , ifeature = [ toVelocity 50  -- flaps and flutters
               , Durable, Equipable, EqpSlot EqpSlotAddArmorMelee ]
  , idesc    = "Long leather gauntlet covered in overlapping steel plates."
  }
gloveJousting = gloveFencing
  { iname    = "Tournament Gauntlet"
  , ifreq    = [("common item", 100), ("misc armor", 1)]
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 3), (10, 3)]
  , iweight  = 3000
  , idamage  = 3 `d` 1
  , iaspects = [ AddHurtMelee $ (-7 + 1 `dL` 5) * 3
               , AddArmorMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 5
               , AddArmorRanged $ (1 + 1 `dL` 2) * 3 ]
                 -- very random on purpose and can even be good on occasion
  , ifeature = [ toVelocity 50  -- flaps and flutters
               , Unique, Durable, Equipable, EqpSlot EqpSlotAddArmorMelee ]
  , idesc    = "Rigid, steel, jousting handgear. If only you had a lance. And a horse to carry it all."
  }

-- * Shields

-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
-- Note that AI will pick them up but never wear and will use them at most
-- as a way to push itself (but they won't recharge, not being in eqp).
-- Being @Meleeable@ they will not be use as weapons either.
-- This is OK, using shields smartly is totally beyond AI.
buckler = ItemKind
  { isymbol  = symbolShield
  , iname    = "buckler"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(4, 5)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = 2 `d` 1
  , iaspects = [ AddArmorMelee 40  -- not enough to compensate; won't be in eqp
               , AddHurtMelee (-30)  -- too harmful; won't be wielded as weapon
               , Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 2 ]
  , ieffects = [Recharging (PushActor (ThrowMod 100 50))]  -- 1 step, slow
  , ifeature = [ toVelocity 50  -- unwieldy to throw
               , MinorEffects, Durable, Meleeable
               , EqpSlot EqpSlotAddArmorMelee ]
  , idesc    = "Heavy and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too small to intercept projectiles with."
  , ikit     = []
  }
shield = buckler
  { iname    = "shield"
  , irarity  = [(8, 4)]  -- the stronger variants add to total probability
  , iflavour = zipPlain [Green]
  , iweight  = 4000
  , idamage  = 4 `d` 1
  , iaspects = [ AddArmorMelee 80  -- not enough to compensate; won't be in eqp
               , AddHurtMelee (-70)  -- too harmful; won't be wielded as weapon
               , Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 4 ]
  , ieffects = [Recharging (PushActor (ThrowMod 400 25))]  -- 1 step, fast
  , ifeature = [ toVelocity 50  -- unwieldy to throw
               , MinorEffects, Durable, Meleeable
               , EqpSlot EqpSlotAddArmorMelee ]
  , idesc    = "Large and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  }
shield2 = shield
  { ifreq    = [("common item", 3 * 3)]  -- very low base rarity
  , iweight  = 5000
  , idamage  = 8 `d` 1
  -- , idesc    = "" e.g., "this kind has a spike protruding from the center"
  }
shield3 = shield
  { ifreq    = [("common item", 1 * 3)]  -- very low base rarity
  , iweight  = 6000
  , idamage  = 12 `d` 1
  -- , idesc    = ""
  }

-- * Weapons

dagger = ItemKind
  { isymbol  = symbolEdged
  , iname    = "dagger"
  , ifreq    = [("common item", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1, 40), (3, 1)]
  , iverbHit = "stab"
  , iweight  = 800
  , idamage  = 6 `d` 1
  , iaspects = [ AddHurtMelee $ (-1 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddArmorMelee $ (1 `d` 2) * 5 ]
                   -- very common, so don't make too random
  , ieffects = []
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Meleeable, EqpSlot EqpSlotWeapon ]
  , idesc    = "A short dagger for thrusting and parrying blows. Does not penetrate deeply, but is hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = dagger
  { iname    = "Double Dagger"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(1, 5), (10, 3)]
  -- Here timeout has to be small, if the player is to count on the effect
  -- occuring consistently in any longer fight. Otherwise, the effect will be
  -- absent in some important fights, leading to the feeling of bad luck,
  -- but will manifest sometimes in fights where it doesn't matter,
  -- leading to the feeling of wasted power.
  -- If the effect is very powerful and so the timeout has to be significant,
  -- let's make it really large, for the effect to occur only once in a fight:
  -- as soon as the item is equipped, or just on the first strike.
  -- Here the timeout is either very small or very large, randomly.
  -- In the latter case the weapon is best swapped for a stronger one
  -- later on in the game, but provides some variety at the start.
  , iaspects = iaspects dagger ++ [Timeout $ (1 `d` 2) * 20 - 16]
  , ieffects = ieffects dagger
               ++ [ Recharging DropBestWeapon, Recharging $ RefillCalm (-3) ]
  , ifeature = [Unique] ++ ifeature dagger
  , idesc    = "A double dagger that a focused fencer can use to catch and twist away an opponent's blade occasionally."
  }
hammer = ItemKind
  { isymbol  = symbolHafted
  , iname    = "war hammer"
  , ifreq    = [ ("common item", 100), ("starting weapon", 100)
               , ("hammer unknown", 1) ]
  , iflavour = zipFancy [BrMagenta]  -- avoid "pink"
  , icount   = 1
  , irarity  = [(5, 20), (8, 1)]
  , iverbHit = "club"
  , iweight  = 1600
  , idamage  = 8 `d` 1  -- we are lying about the dice here, but the dungeon
                        -- is too small and the extra-dice hammers too rare
                        -- to subdivide this identification class by dice
  , iaspects = [AddHurtMelee $ (-1 + 1 `d` 2 + 1 `dL` 2) * 3]
  , ieffects = []
  , ifeature = [ HideAs "hammer unknown"
               , toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Meleeable, EqpSlot EqpSlotWeapon ]
  , idesc    = "It may not cause extensive wounds, but neither does it harmlessly glance off heavy armour as blades and polearms tend to. There are so many shapes and types, some looking more like tools than weapons, that at a glance you can't tell what a particular specimen does."
  , ikit     = []
  }
hammer2 = hammer
  { ifreq    = [("common item", 3), ("starting weapon", 1)]
  , iweight  = 2000
  , idamage  = 12 `d` 1
  , idesc    = "Upon closer inspection, this hammer turns out particularly deadly, with one thick and sturdy and two long and sharp points."
  }
hammer3 = hammer
  { ifreq    = [("common item", 1)]
  , iweight  = 2400
  , idamage  = 16 `d` 1
  , idesc    = "This hammer sports a long metal handle that increases durability and momentum of the sharpened head's swing."
  }
hammerParalyze = hammer
  { iname    = "Concussion Hammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , idamage  = 8 `d` 1
  , iaspects = iaspects hammer ++ [Timeout 7]
  , ieffects = ieffects hammer ++ [Recharging $ Paralyze 10]
  , ifeature = [Unique] ++ ifeature hammer
  , idesc    = "A huge shapeless lump of steel on a long pole. Nobody remains standing after this hammer connects."
  }
hammerSpark = hammer
  { iname    = "Grand Smithhammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , idamage  = 12 `d` 1
  , iaspects = iaspects hammer ++ [AddShine 3, Timeout 10]
  , ieffects = ieffects hammer ++ [Recharging $ Explode "spark"]
      -- we can't use a focused explosion, because it would harm the hammer
      -- wielder as well, unlike this one
  , ifeature = [Unique] ++ ifeature hammer
  , idesc    = "Smiths of old wielded this hammer and its sparks christened many a potent blade."
  }
sword = ItemKind
  { isymbol  = symbolEdged
  , iname    = "sword"
  , ifreq    = [("common item", 100), ("starting weapon", 10)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(4, 1), (5, 15)]
  , iverbHit = "slash"
  , iweight  = 2000
  , idamage  = 10 `d` 1
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Meleeable, EqpSlot EqpSlotWeapon ]
  , idesc    = "Difficult to master; deadly when used effectively. The steel is particularly hard and keen, but rusts quickly without regular maintenance."
  , ikit     = []
  }
swordImpress = sword
  { iname    = "Master's Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , iaspects = [Timeout $ (1 `d` 2) * 40 - 30]
  , ieffects = ieffects sword ++ [Recharging Impress]
  , ifeature = [Unique] ++ ifeature sword
  , idesc    = "A particularly well-balance blade, lending itself to impressive shows of fencing skill."
  }
swordNullify = sword
  { iname    = "Gutting Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , iaspects = [Timeout 10]
  , ieffects = ieffects sword
               ++ [ Recharging
                    $ DropItem 1 maxBound COrgan "condition"
                  , Recharging $ RefillCalm (-10) ]
  , ifeature = [Unique] ++ ifeature sword
  , idesc    = "Cold, thin blade that pierces deeply and sends its victim into abrupt, sobering shock."
  }
halberd = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "war scythe"
  , ifreq    = [("common item", 100), ("starting weapon", 20)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(7, 1), (9, 15)]
  , iverbHit = "impale"
  , iweight  = 3000
  , idamage  = 12 `d` 1
  , iaspects = [ AddHurtMelee (-20)  -- useless against armor at game start
               , AddArmorMelee $ (1 `dL` 4) * 5 ]
  , ieffects = []
  , ifeature = [ toVelocity 20  -- not balanced
               , Durable, Meleeable, EqpSlot EqpSlotWeapon ]
  , idesc    = "An improvised but deadly weapon made of a blade from a scythe attached to a long pole."
  , ikit     = []
  }
halberd2 = halberd
  { ifreq    = [("common item", 3 * 2), ("starting weapon", 1)]
  , iweight  = 4000
  , idamage  = 18 `d` 1
  -- , idesc    = ""
  }
halberd3 = halberd
  { ifreq    = [("common item", 1 * 2)]  -- compensating for low base rarity
  , iweight  = 5000
  , idamage  = 24 `d` 1
  -- , idesc    = ""
  }
halberdPushActor = halberd
  { iname    = "Swiss Halberd"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(8, 1), (10, 15)]
  , idamage  = 12 `d` 1
  , iaspects = iaspects halberd ++ [Timeout $ (1 `d` 2) * 10]
  , ieffects = ieffects halberd
               ++ [Recharging (PushActor (ThrowMod 400 25))]  -- 1 step
  , ifeature = [Unique] ++ ifeature halberd
  , idesc    = "A versatile polearm, with great reach and leverage. Foes are held at a distance."
  }

-- * Wands

wandTemplate = ItemKind
  { isymbol  = symbolWand
  , iname    = "wand"
  , ifreq    = [("wand unknown", 1)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , irarity  = []
  , iverbHit = "club"
  , iweight  = 300
  , idamage  = 0
  , iaspects = [AddShine 1, AddSpeed (-1)]  -- pulsing with power, distracts
  , ieffects = []
  , ifeature = [ HideAs "wand unknown", Durable
               , toVelocity 125 ]  -- magic
  , idesc    = "Buzzing with dazzling light that shines even through appendages that handle it."  -- will have math flavour
  , ikit     = []
  }
wand1 = wandTemplate
  { ifreq    = []
  , ieffects = []  -- will be: emit a cone of sound shrapnel that makes enemy cover his ears and so drop '|' and '{'
  }

-- * Treasure

gemTemplate = ItemKind
  { isymbol  = symbolGold
  , iname    = "gem"
  , ifreq    = [("gem unknown", 1), ("valuable", 100)]
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = [(3, 0), (10, 24)]
  , iverbHit = "tap"
  , iweight  = 50
  , idamage  = 0
  , iaspects = []
  , ieffects = []
  , ifeature = [HideAs "gem unknown", Precious]
  , idesc    = "Useless, and still worth around 100 gold each. Would gems of thought and pearls of artful design be valued that much in our age of Science and Progress!"
  , ikit     = []
  }
gem1 = gemTemplate
  { ifreq    = [ ("treasure", 100), ("gem", 100), ("any jewelry", 100)
               , ("valuable", 100) ]
  , irarity  = [(3, 0), (10, 24)]
  , iaspects = [AddShine 1, AddSpeed (-1)]
                 -- reflects strongly, distracts; so it glows in the dark,
                 -- is visible on dark floor, but not too tempting to wear
  , ieffects = [RefillCalm (-1)]  -- minor effect to ensure no id-on-pickup
  }
gem2 = gem1
  { ifreq    = [ ("treasure", 100), ("gem", 100), ("any jewelry", 100)
               , ("valuable", 100) ]
  , irarity  = [(5, 0), (10, 28)]
  }
gem3 = gem1
  { ifreq    = [ ("treasure", 100), ("gem", 100), ("any jewelry", 100)
               , ("valuable", 100) ]
  , irarity  = [(7, 0), (10, 32)]
  }
gem4 = gem1
  { ifreq    = [ ("treasure", 100), ("gem", 100), ("any jewelry", 100)
               , ("valuable", 100) ]
  , irarity  = [(9, 0), (10, 100)]
  }
gem5 = gem1
  { isymbol  = symbolSpecial
  , iname    = "elixir"
  , ifreq    = [ ("treasure", 100), ("gem", 25), ("any jewelry", 25)
               , ("valuable", 100) ]
  , iflavour = zipPlain [BrYellow]
  , irarity  = [(1, 40), (10, 40)]
  , ieffects = [RefillCalm 10, RefillHP 40]
  , ifeature = [ELabel "of youth", Precious]  -- not hidden
  , idesc    = "A crystal vial of amber liquid, supposedly granting eternal youth and fetching 100 gold per piece. The main effect seems to be mild euphoria, but it admittedly heals minor ailments rather well."
  }
currencyTemplate = ItemKind
  { isymbol  = symbolGold
  , iname    = "gold piece"
  , ifreq    = [("currency unknown", 1), ("valuable", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + 1 `d` 20 + 1 `dL` 20
  , irarity  = [(1, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 31
  , idamage  = 0
  , iaspects = []
  , ieffects = []
  , ifeature = [HideAs "currency unknown", Precious]
  , idesc    = "Reliably valuable in every civilized plane of existence."
  , ikit     = []
  }
currency = currencyTemplate
  { ifreq    = [("treasure", 100), ("currency", 100), ("valuable", 1)]
  , iaspects = [AddShine 1, AddSpeed (-1)]
  , ieffects = [RefillCalm (-1)]
  }
