-- | Item definitions.
module Content.ItemKind
  ( cdefs, items, otherItemContent
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
  , content = contentFromList $ items ++ otherItemContent
  }

otherItemContent :: [ItemKind]
otherItemContent = embeds ++ actors ++ organs ++ blasts ++ temporaries

items :: [ItemKind]
items =
  [sandstoneRock, dart, spike, slingStone, slingBullet, paralizingProj, harpoon, net, light1, light2, light3, blanket, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, flask18, flask19, flask20, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, imageItensifier, sightSharpening, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, gem4, gem5, currency, explosive, firecracker]

sandstoneRock,    dart, spike, slingStone, slingBullet, paralizingProj, harpoon, net, light1, light2, light3, blanket, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, flask18, flask19, flask20, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, imageItensifier, sightSharpening, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, gem4, gem5, currency, explosive, firecracker :: ItemKind

necklace, ring, potion, flask, scroll, wand, gem :: ItemKind  -- generic templates

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
symbolWand       = '/'  -- magical rod, transmitter, pistol, rifle
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
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee $ -16 * 5]
  , ieffects = []
  , ifeature = [toVelocity 70, Fragile, Identified]  -- not dense, irregular
  , idesc    = "A lump of brittle sandstone rock."
  , ikit     = []
  }
dart = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "dart"
  , ifreq    = [("useful", 100), ("any arrow", 50), ("weak arrow", 50)]
  , iflavour = zipPlain [BrRed]
  , icount   = 4 `d` 3
  , irarity  = [(1, 20), (10, 10)]
  , iverbHit = "prick"
  , iweight  = 40
  , idamage  = [(98, 1 `d` 1), (2, 2 `d` 1)]
  , iaspects = [AddHurtMelee $ (-14 + 1 `d` 2 + 1 `dl` 4) * 5]
                 -- only leather-piercing
  , ieffects = []
  , ifeature = [Identified]
  , idesc    = "A sharp delicate dart with fins."
  , ikit     = []
  }
spike = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "spike"
  , ifreq    = [("useful", 100), ("any arrow", 50), ("weak arrow", 50)]
  , iflavour = zipPlain [Cyan]
  , icount   = 4 `d` 3
  , irarity  = [(1, 10), (10, 20)]
  , iverbHit = "nick"
  , iweight  = 150
  , idamage  = [(98, 2 `d` 1), (2, 4 `d` 1)]
  , iaspects = [AddHurtMelee $ (-10 + 1 `d` 2 + 1 `dl` 4) * 5]
                 -- heavy vs armor
  , ieffects = [ Explode "single spark"  -- when hitting enemy
               , OnSmash (Explode "single spark") ]  -- at wall hit
  , ifeature = [toVelocity 70, Identified]  -- hitting with tip costs speed
  , idesc    = "A cruel long nail with small head."  -- "Much inferior to arrows though, especially given the contravariance problems."  --- funny, but destroy the suspension of disbelief; this is supposed to be a Lovecraftian horror and any hilarity must ensue from the failures in making it so and not from actively trying to be funny; also, mundane objects are not supposed to be scary or transcendental; the scare is in horrors from the abstract dimension visiting our ordinary reality; without the contrast there's no horror and no wonder, so also the magical items must be contrasted with ordinary XIX century and antique items
  , ikit     = []
  }
slingStone = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sling stone"
  , ifreq    = [("useful", 5), ("any arrow", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 3 `d` 3
  , irarity  = [(1, 1), (10, 20)]
  , iverbHit = "hit"
  , iweight  = 200
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee $ (-10 + 1 `d` 2 + 1 `dl` 4) * 5]
                 -- heavy vs armor
  , ieffects = [ Explode "single spark"  -- when hitting enemy
               , OnSmash (Explode "single spark") ]  -- at wall hit
  , ifeature = [toVelocity 150, Identified]
  , idesc    = "A round stone, carefully sized and smoothed to fit the pouch of a standard string and cloth sling."
  , ikit     = []
  }
slingBullet = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sling bullet"
  , ifreq    = [("useful", 5), ("any arrow", 100)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 6 `d` 3
  , irarity  = [(1, 1), (10, 15)]
  , iverbHit = "hit"
  , iweight  = 28
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee $ (-17 + 1 `d` 2 + 1 `dl` 4) * 5]
                 -- not armor-piercing
  , ieffects = []
  , ifeature = [toVelocity 200, Identified]
  , idesc    = "Small almond-shaped leaden projectile that weighs more than the sling used to tie the bag. It doesn't drop out of the sling's pouch when swung and doesn't snag when released."
  , ikit     = []
  }

-- * Exotic thrown weapons

-- Identified, because shape (and name) says it all. Detailed stats id by use.
paralizingProj = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "bolas set"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1 `dl` 4
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "entangle"
  , iweight  = 500
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee $ -14 * 5]
  , ieffects = [Paralyze 15, DropBestWeapon]
  , ifeature = [Identified]
  , idesc    = "Wood balls tied with hemp rope. The target enemy is tripped and bound to drop the main weapon, while fighting for balance."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [("useful", 100), ("harpoon", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dl` 5
  , irarity  = [(10, 10)]
  , iverbHit = "hook"
  , iweight  = 750
  , idamage  = [(99, 5 `d` 1), (1, 10 `d` 1)]
  , iaspects = [AddHurtMelee $ (-10 + 1 `d` 2 + 1 `dl` 4) * 5]
  , ieffects = [PullActor (ThrowMod 200 50)]
  , ifeature = [Identified]
  , idesc    = "The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
net = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "net"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1 `dl` 3
  , irarity  = [(3, 5), (10, 4)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , idamage  = toDmg $ 2 `d` 1
  , iaspects = [AddHurtMelee $ -14 * 5]
  , ieffects = [ toOrganGameTurn "slowed" (3 + 1 `d` 3)
               , DropItem maxBound 1 CEqp "torso armor" ]
  , ifeature = [Identified]
  , idesc    = "A wide net with weights along the edges. Entangles armor and restricts movement."
  , ikit     = []
  }

-- * Lights

light1 = ItemKind
  { isymbol  = symbolLight
  , iname    = "wooden torch"
  , ifreq    = [("useful", 100), ("light source", 100), ("wooden torch", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `d` 2
  , irarity  = [(1, 15)]
  , iverbHit = "scorch"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddShine 3       -- not only flashes, but also sparks,
               , AddSight (-2) ]  -- so unused by AI due to the mixed blessing
  , ieffects = [Burn 1, EqpSlot EqpSlotLightSource]
  , ifeature = [Lobable, Identified, Equipable]  -- not Fragile; reusable flare
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
  , iweight  = 1500
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddShine 3, AddSight (-1)]
  , ieffects = [ Burn 1, Paralyze 6, OnSmash (Explode "burning oil 2")
               , EqpSlot EqpSlotLightSource ]
  , ifeature = [Lobable, Fragile, Identified, Equipable]
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
  , iweight  = 3000
  , idamage  = toDmg $ 4 `d` 1
  , iaspects = [AddShine 4, AddSight (-1)]
  , ieffects = [ Burn 1, Paralyze 8, OnSmash (Explode "burning oil 4")
               , EqpSlot EqpSlotLightSource ]
  , ifeature = [Lobable, Fragile, Identified, Equipable]
  , idesc    = "Very bright and very heavy brass lantern."
  , ikit     = []
  }
blanket = ItemKind
  { isymbol  = symbolLight
  , iname    = "wool blanket"
  , ifreq    = [("useful", 100), ("light source", 100), ("blanket", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "swoosh"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddShine (-10)  -- douses torch, lamp and lantern in one action
               , AddArmorMelee 1, AddMaxCalm 2 ]
  , ieffects = []
  , ifeature = [Lobable, Identified, Equipable]  -- not Fragile; reusable douse
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
-- In reality, they just cover all temporary effects, which in turn matches
-- all aspects.
--
-- No flask nor temporary organ of Calm depletion, since Calm reduced often.

-- All the explosive items are currently disabled (empty rarity),
-- so that we can brainstorm them without breaking game balance.
explosive = ItemKind
  { isymbol = symbolFlask
      -- if it's, in fact, a flask of laboratory-produced highly explosive
      -- liquid, symbolFlask makes sense; it also suggests it can be lobbed,
      -- which is very handy for an explosive thrown weapon;
      -- but symbolProjectile makes sense, too, if it's essentially a small
      -- grenade, specifically designed to be thrown, or symbolTool
      -- if it's a heavy mining explosive
  , iname = "explosive"  -- we will need something more specific, even it it's
                         -- only a generic template item
  , ifreq = [("useful", 100)]
  , iflavour = zipLiquid [BrRed]
      -- this ensures the object will be bright red on the screen
      -- and will be described as "red-speckled"; we are also free
      -- to manually set both the colour and the description
  , icount = 1
  , irarity = []  -- [(1, 7), (10, 4)] means it's quite common (7) first level
                  -- and less common (4) on the deepest level of the dungeon,
                  -- linearly interpolating in-between
  , iverbHit = "blast"
  , iweight = 500  -- flavour and affects the speed when thrown;
                   -- 250g and below is full speed
  , idamage = toDmg 0
  , iaspects = []
  , ieffects = [Explode "blast 20", OnSmash (Explode "blast 20")]
      -- this is tricky: @OnSmash@ causes the explosion when the item
      -- hits a tile (including floor), so a well aimed item can successfully
      -- ungulf many enemies (or a single hidden or fast-moving enemy)
      -- in an explosion; however the lone @Explode "blast 20"@
      -- activates only when an actor is hit (or applies the item)
      -- and makes the actor the centre of the explosion and so keeps him
      -- unharmed (from the initial explosion; fireckrackers cause chain
      -- explosion so the secondary blasts can harm him);
      -- this is why "oil lamp", which is the closest to a grenade
      -- we currently have, doesn't have a lone Exlode effect,
      -- but instead causes a couple of unplesant effects directly:
      -- slight burning, before the fire is doused, and paralysis
      -- from slipping on the oil and slippery grip of items,
      -- until the oil can be wiped out; it also causes a tiny impact damage;
      -- all this suggests that we should define "blastI" item
      -- that not only blasts outward, but also inward, by first creating
      -- particles that move 1 (or 2?) tle away outward and then starting
      -- a standard "blast" explosion in each (firecracker does that,
      -- but with many, not just 2 levels of explosions)
  , ifeature = [Lobable, Fragile, Identified, toVelocity 50]
      -- not Applicable, because won't be applied, only thrown;
      -- Fragile, because we want it to explode at target tile, even if
      -- it hits no actor nor wall when it gets there;
      -- Identified, because the player recognizes it from the description
      -- already (unless we design granades that all look alike, but some
      -- explode with blast, others with fireckracker, etc. and the player
      -- needs to experiment to learn to recognize them);
      -- @toVelocity 50@ means will have 50% of the speed of an ideally shaped
      -- item of the same weight
  , idesc = "The practical application of science."
  , ikit = []
  }

firecracker = explosive
  { iname = "firecracker bag/stick?"
     -- remove the revealing name if we go the "10 kinds of grenades" route,
     -- but make sure it's not too similar to the current "20 kinds of flasks
     -- and potions that also explode" fun
  , iverbHit = "crack"  -- a pun, matches the verb from ItemKindBlast.hs
  , ieffects = [Explode "firecracker 5", OnSmash (Explode "firecracker 5")]
  , idesc = "String and paper, concealing a deadly surprise."
  }

flask = ItemKind
  { isymbol  = symbolFlask
  , iname    = "flask"
  , ifreq    = [("useful", 100), ("flask", 100), ("any vial", 100)]
  , iflavour = zipLiquid darkCol ++ zipPlain darkCol ++ zipFancy darkCol
               ++ zipLiquid brightCol
  , icount   = 1
  , irarity  = [(1, 7), (10, 4)]
  , iverbHit = "splash"
  , iweight  = 500
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Applicable, Lobable, Fragile, toVelocity 50]  -- oily, bad grip
  , idesc    = "A flask of oily liquid of a suspect color. Something seems to be moving inside."
  , ikit     = []
  }
flask1 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of strength renewal brew"
               , toOrganActorTurn "strengthened" (20 + 1 `d` 5)
               , toOrganNone "regenerating"
               , OnSmash (Explode "dense shower") ]
  }
flask2 = flask
  { ieffects = [ ELabel "of weakness brew"
               , toOrganGameTurn "weakened" (20 + 1 `d` 5)
               , OnSmash (Explode "sparse shower") ]
  }
flask3 = flask
  { ieffects = [ ELabel "of melee protective balm"
               , toOrganActorTurn "protected from melee" (20 + 1 `d` 5)
               , OnSmash (Explode "melee protective balm") ]
  }
flask4 = flask
  { ieffects = [ ELabel "of ranged protective balm"
               , toOrganActorTurn "protected from ranged" (20 + 1 `d` 5)
               , OnSmash (Explode "ranged protective balm") ]
  }
flask5 = flask
  { ieffects = [ ELabel "of PhD defense questions"
               , toOrganGameTurn "defenseless" (20 + 1 `d` 5)
               , Impress
               , DetectExit 20
               , OnSmash (Explode "PhD defense question") ]
  }
flask6 = flask
  { irarity  = [(10, 9)]
  , ieffects = [ ELabel "of resolution"
               , toOrganActorTurn "resolute" (200 + 1 `d` 50)
                   -- long, for scouting and has to recharge
               , RefillCalm 60  -- not to make it a drawback, via @calmEnough@
               , OnSmash (Explode "resolution dust") ]
  }
flask7 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of haste brew"
               , toOrganActorTurn "hasted" (20 + 1 `d` 5)
               , OnSmash (Explode "blast 20")
               , OnSmash (Explode "haste spray") ]
  }
flask8 = flask
  { irarity  = [(1, 14), (10, 4)]
  , ieffects = [ ELabel "of lethargy brew"
               , toOrganGameTurn "slowed" (20 + 1 `d` 5)
               , toOrganNone "regenerating", toOrganNone "regenerating"  -- x2
               , RefillCalm 5
               , OnSmash (Explode "slowness mist") ]
  }
flask9 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of eye drops"
               , toOrganActorTurn "far-sighted" (40 + 1 `d` 10)
               , OnSmash (Explode "eye drop") ]
  }
flask10 = flask
  { irarity  = [(10, 2)]
  , ieffects = [ ELabel "of smelly concoction"
               , toOrganActorTurn "keen-smelling" (40 + 1 `d` 10)
               , DetectActor 10
               , OnSmash (Explode "smelly droplet") ]
  }
flask11 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of cat tears"
               , toOrganActorTurn "shiny-eyed" (40 + 1 `d` 10)
               , OnSmash (Explode "eye shine") ]
  }
flask12 = flask
  { irarity  = [(1, 14), (10, 10)]
  , ieffects = [ ELabel "of whiskey"
               , toOrganActorTurn "drunk" (20 + 1 `d` 5)
               , Burn 1, RefillHP 3
               , OnSmash (Explode "whiskey spray") ]
  }
flask13 = flask
  { ieffects = [ ELabel "of bait cocktail"
               , toOrganActorTurn "drunk" (20 + 1 `d` 5)
               , Burn 1, RefillHP 3
               , Summon "mobile animal" 1
               , OnSmash (Summon "mobile animal" 1)
               , OnSmash Impress
               , OnSmash (Explode "waste") ]
  }
-- The player has full control over throwing the flask at his party,
-- so he can milk the explosion, so it has to be much weaker, so a weak
-- healing effect is enough. OTOH, throwing a harmful flask at many enemies
-- at once is not easy to arrange, so these explostions can stay powerful.
flask14 = flask
  { irarity  = [(1, 4), (10, 14)]
  , ieffects = [ ELabel "of regeneration brew"
               , toOrganNone "regenerating", toOrganNone "regenerating"  -- x2
               , OnSmash (Explode "healing mist") ]
  }
flask15 = flask
  { ieffects = [ ELabel "of poison"
               , toOrganNone "poisoned", toOrganNone "poisoned"  -- x2
               , OnSmash (Explode "poison cloud") ]
  }
flask16 = flask
  { irarity  = [(1, 14), (10, 4)]
  , ieffects = [ ELabel "of weak poison"
               , toOrganNone "poisoned"
               , OnSmash (Explode "poison cloud") ]
  }
flask17 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of slow resistance"
               , toOrganNone "slow resistant"
               , OnSmash (Explode "blast 10")
               , OnSmash (Explode "anti-slow mist") ]
  }
flask18 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of poison resistance"
               , toOrganNone "poison resistant"
               , OnSmash (Explode "antidote mist") ]
  }
flask19 = flask
  { ieffects = [ ELabel "of blindness"
               , toOrganGameTurn "blind" (40 + 1 `d` 10)
               , OnSmash (Explode "iron filing") ]
  }
flask20 = flask
  { ieffects = [ ELabel "of calamity"
               , toOrganNone "poisoned"
               , toOrganGameTurn "weakened" (20 + 1 `d` 5)
               , toOrganGameTurn "defenseless" (20 + 1 `d` 5)
               , OnSmash (Explode "poison cloud") ]
  }

-- Potions are often natura. Various configurations of effects.
-- A different class of effects is on scrolls and/or mechanical items.
-- Some are shared.

potion = ItemKind
  { isymbol  = symbolPotion
  , iname    = "potion"
  , ifreq    = [("useful", 100), ("potion", 100), ("any vial", 100)]
  , iflavour = zipLiquid brightCol ++ zipPlain brightCol ++ zipFancy brightCol
  , icount   = 1
  , irarity  = [(1, 10), (10, 7)]
  , iverbHit = "splash"
  , iweight  = 200
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Applicable, Lobable, Fragile, toVelocity 50]  -- oily, bad grip
  , idesc    = "A vial of bright, frothing concoction. The best that nature has to offer."
  , ikit     = []
  }
potion1 = potion
  { ieffects = [ ELabel "of rose water", Impress, RefillCalm (-5)
               , OnSmash ApplyPerfume, OnSmash (Explode "fragrance") ]
  }
potion2 = potion
  { ifreq    = [("treasure", 100)]
  , irarity  = [(6, 9), (10, 9)]
  , ieffects = [ Unique, ELabel "of Attraction", Impress, RefillCalm (-20)
               , OnSmash (Explode "pheromone") ]
  -- , idesc    = ""
  }
potion3 = potion
  { ieffects = [ RefillHP 5, DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "healing mist") ]
  }
potion4 = potion
  { irarity  = [(1, 7), (10, 10)]
  , ieffects = [ RefillHP 10, DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "healing mist 2") ]
  }
potion5 = potion  -- needs to be common to show at least a portion of effects
  { irarity  = [(1, 30), (10, 15)]
  , ieffects = [ OneOf [ RefillHP 10, RefillHP 5, Burn 5
                       , DropItem 1 maxBound COrgan "poisoned"
                       , toOrganActorTurn "strengthened" (20 + 1 `d` 5) ]
               , OnSmash (OneOf [ Explode "dense shower"
                                , Explode "sparse shower"
                                , Explode "melee protective balm"
                                , Explode "ranged protective balm"
                                , Explode "PhD defense question"
                                , Explode "blast 10" ]) ]
  }
potion6 = potion  -- needs to be common to show at least a portion of effects
  { irarity  = [(1, 5), (10, 20)]
  , ieffects = [ Impress
               , OneOf [ RefillCalm (-60)
                       , RefillHP 20, RefillHP 10, Burn 10
                       , DropItem 1 maxBound COrgan "poisoned"
                       , toOrganActorTurn "hasted" (20 + 1 `d` 5) ]
               , OnSmash (OneOf [ Explode "healing mist 2"
                                , Explode "wounding mist"
                                , Explode "distressing odor"
                                , Explode "haste spray"
                                , Explode "slowness mist"
                                , Explode "fragrance"
                                , Explode "blast 20" ]) ]
  }
potion7 = potion
  { irarity  = [(1, 11), (10, 4)]
  , ieffects = [ DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "antidote mist") ]
  }
potion8 = potion
  { irarity  = [(1, 9)]
  , ieffects = [ DropItem 1 maxBound COrgan "temporary condition"
               , OnSmash (Explode "blast 10") ]
  }
potion9 = potion
  { irarity  = [(10, 9)]
  , ieffects = [ DropItem maxBound maxBound COrgan "temporary condition"
               , OnSmash (Explode "blast 20") ]
  }
potion10 = potion
  { ifreq    = [("treasure", 100)]
  , irarity  = [(10, 4)]
  , ieffects = [ Unique, ELabel "of Love", RefillHP 60
               , Impress, RefillCalm (-60)
               , OnSmash (Explode "healing mist 2")
               , OnSmash (Explode "pheromone") ]
  -- , idesc    = ""
  }

-- * Non-exploding consumables, not specifically designed for throwing

scroll = ItemKind
  { isymbol  = symbolScroll
  , iname    = "scroll"
  , ifreq    = [("useful", 100), ("any scroll", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain darkCol  -- arcane and old
  , icount   = 1
  , irarity  = [(1, 14), (10, 11)]
  , iverbHit = "thump"
  , iweight  = 50
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 30  -- bad shape, even rolled up
               , Applicable ]
  , idesc    = "Scraps of haphazardly scribbled mysteries from beyond. Is this equation an alchemical recipe? Is this diagram an extradimensional map? Is this formula a secret call sign?"
  , ikit     = []
  }
scroll1 = scroll
  { ifreq    = [("treasure", 100)]
  , irarity  = [(5, 9), (10, 9)]  -- mixed blessing, so available early
  , ieffects = [ Unique, ELabel "of Reckless Beacon"
               , Summon "hero" 1, Summon "mobile animal" (2 + 1 `d` 2) ]
  , idesc    = "The bright flame and sweet-smelling smoke of this heavily infused scroll should attract natural creatures inhabiting the area, including human survivors, if any."
  }
scroll2 = scroll
  { irarity  = [(1, 2)]
  , ieffects = [ ELabel "of greed", DetectItem 20, Teleport 20
               , RefillCalm (-100) ]
  }
scroll3 = scroll
  { irarity  = [(1, 4), (10, 2)]
  , ieffects = [Ascend False]
  }
scroll4 = scroll  -- needs to be common to show at least a portion of effects
  { irarity  = [(1, 40), (10, 20)]
  , ieffects = [OneOf [ Teleport 5, RefillCalm 5, InsertMove 5
                      , DetectActor 20, DetectItem 20 ]]
  }
scroll5 = scroll  -- needs to be common to show at least a portion of effects
  { irarity  = [(10, 30)]
  , ieffects = [ Impress
               , OneOf [ Teleport 20, Ascend False, Ascend True
                       , Summon "hero" 1, Summon "mobile animal" 2
                       , Detect 40, RefillCalm (-100)
                       , CreateItem CGround "useful" TimerNone ] ]
  }
scroll6 = scroll
  { ieffects = [Teleport 5]
  }
scroll7 = scroll
  { ieffects = [Teleport 20]
  }
scroll8 = scroll
  { irarity  = [(10, 2)]
  , ieffects = [InsertMove $ 1 + 1 `d` 2 + 1 `dl` 2]
  }
scroll9 = scroll
  { irarity  = [(1, 30)]
  , ieffects = [ ELabel "of scientific explanation"
               , Composite [Identify, RefillCalm 10] ]
      -- your most pressing existential concerns are answered scientifitically,
      -- hence the calming effect
  }
scroll10 = scroll
  { irarity  = [(10, 20)]
  , ieffects = [ ELabel "transfiguration"
               , Composite [PolyItem, Explode "firecracker 7"] ]
  }
scroll11 = scroll
  { ifreq    = [("treasure", 100)]
  , irarity  = [(6, 9), (10, 9)]
  , ieffects = [Unique, ELabel "of Rescue Proclamation", Summon "hero" 1]
  , idesc    = "A survivor is found that enjoys, apparently, complete physiological integrity. If we so wish, we can pronounce him rescued and let him join our team."
  }
scroll12 = scroll
  { irarity  = [(1, 9), (10, 4)]
  , ieffects = [DetectHidden 20]
  }
scroll13 = scroll
  { ieffects = [ELabel "of acute hearing", DetectActor 20]
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
  , idamage  = toDmg 0
  , iaspects = [Timeout $ (1 `d` 2 + 2 - 1 `dl` 2) * 10]
  , ieffects = [Recharging (toOrganActorTurn "hasted" 1)]
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
  , idamage  = toDmg 0
  , iaspects = [AddHurtMelee $ (1 `d` 10) * 3]
  , ieffects = [EqpSlot EqpSlotAddHurtMelee]
  , ifeature = [Identified, Equipable]
  , idesc    = "A portable sharpening stone that lets you fix your weapons between or even during fights, without the need to set up camp, fish out tools and assemble a proper sharpening workshop."
  , ikit     = []
  }
seeingItem = ItemKind
  { isymbol  = symbolFood
  , iname    = "pupil"
  , ifreq    = [("useful", 30)]  -- spooky and wierd, so rare
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "gaze at"
  , iweight  = 100
  , idamage  = toDmg 0
  , iaspects = [ AddSight 10, AddMaxCalm 30, AddShine 2
               , Timeout $ 1 + 1 `d` 2 ]
  , ieffects = [ Periodic
               , Recharging (toOrganNone "poisoned")
               , Recharging (Summon "mobile monster" 1) ]
  , ifeature = [Identified]
  , idesc    = "A slimy, dilated green pupil torn out from some giant eye. Clear and focused, as if still alive."
  , ikit     = []
  }
motionScanner = ItemKind
  { isymbol  = symbolTool
  , iname    = "draft detector"
  , ifreq    = [("useful", 100), ("add nocto 1", 20)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(6, 2), (10, 2)]
  , iverbHit = "jingle"
  , iweight  = 300
  , idamage  = toDmg 0
  , iaspects = [ AddNocto 1
               , AddArmorMelee (1 `dl` 5 - 10), AddArmorRanged (1 `dl` 5 - 10) ]
  , ieffects = [EqpSlot EqpSlotMiscBonus]
  , ifeature = [Identified, Equipable]
  , idesc    = "A silk flag with a bell for detecting sudden draft changes. May indicate a nearby corridor crossing or a fast enemy approaching in the dark. Is also very noisy."
  , ikit     = []
  }

-- * Periodic jewelry

gorget = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "Old Gorget"
  , ifreq    = [("useful", 25), ("treasure", 25)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(4, 3), (10, 3)]  -- weak, shallow
  , iverbHit = "whip"
  , iweight  = 30
  , idamage  = toDmg 0
  , iaspects = [ Timeout $ 1 + 1 `d` 2
               , AddArmorMelee $ 2 + 1 `d` 3
               , AddArmorRanged $ 1 `d` 3 ]
  , ieffects = [ Unique, Periodic
               , Recharging (RefillCalm 1), EqpSlot EqpSlotMiscBonus ]
  , ifeature = [Durable, Precious, Identified, Equipable]
  , idesc    = "Highly ornamental, cold, large, steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
  , ikit     = []
  }
-- Not idenfified, because the id by use, e.g., via periodic activations. Fun.
necklace = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(10, 2)]
  , iverbHit = "whip"
  , iweight  = 30
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Periodic]
  , ifeature = [Precious, toVelocity 50, Equipable]  -- not dense enough
  , idesc    = "Menacing Greek symbols shimmer with increasing speeds along a chain of fine encrusted links. After a tense build-up, a prismatic arc shoots towards the ground and the iridescence subdues, becomes ordered and resembles a harmless ornament again, for a time."
  , ikit     = []
  }
necklace1 = necklace
  { ifreq    = [("treasure", 100)]
  , iaspects = [Timeout $ (1 `d` 3 + 4 - 1 `dl` 3) * 10]
  , ieffects = [ Unique, ELabel "of Aromata", EqpSlot EqpSlotMiscBonus
               , Recharging (RefillHP 1) ]
               ++ ieffects necklace
  , ifeature = Durable : ifeature necklace
  , idesc    = "A cord of freshly dried herbs and healing berries."
  }
necklace2 = necklace
  { ifreq    = [("treasure", 100)]  -- just too nasty to call it useful
  , irarity  = [(1, 1)]
  , iaspects = [Timeout $ (1 `d` 3 + 3 - 1 `dl` 3) * 10]
  , ieffects = [ Recharging (Summon "mobile animal" $ 1 + 1 `dl` 2)
               , Recharging (Explode "waste")
               , Recharging Impress
               , Recharging (DropItem 1 maxBound COrgan "temporary condition") ]
               ++ ieffects necklace
  }
necklace3 = necklace
  { iaspects = [Timeout $ (1 `d` 3 + 4 - 1 `dl` 3) * 5]
  , ieffects = [ ELabel "of fearful listening"
               , Recharging (DetectActor 10)
               , Recharging (RefillCalm (-20)) ]
               ++ ieffects necklace
  }
necklace4 = necklace
  { iaspects = [Timeout $ (1 `d` 4 + 4 - 1 `dl` 4) * 2]
  , ieffects = [Recharging (Teleport $ 3 `d` 2)]
               ++ ieffects necklace
  }
necklace5 = necklace
  { iaspects = [Timeout $ (1 `d` 3 + 4 - 1 `dl` 3) * 10]
  , ieffects = [ ELabel "of escape"
               , Recharging (Teleport $ 14 + 3 `d` 3)
               , Recharging (DetectExit 20)
               , Recharging (RefillHP (-2)) ]  -- prevent micromanagement
               ++ ieffects necklace
  }
necklace6 = necklace
  { iaspects = [Timeout $ (1 `d` 3 + 1) * 2]
  , ieffects = [Recharging (PushActor (ThrowMod 100 50))]
               ++ ieffects necklace
  }
necklace7 = necklace
  { ifreq    = [("treasure", 100)]
  , iaspects = [ AddMaxHP $ 10 + 1 `d` 10
               , AddArmorMelee 20, AddArmorRanged 10
               , Timeout $ 1 `d` 2 + 5 - 1 `dl` 3 ]
  , ieffects = [ Unique, ELabel "of Overdrive", EqpSlot EqpSlotAddSpeed
               , Recharging (InsertMove $ 1 + 1 `d` 2)
               , Recharging (RefillHP (-1))
               , Recharging (RefillCalm (-1)) ]  -- fake "hears something" :)
               ++ ieffects necklace
  , ifeature = Durable : ifeature necklace
  -- , idesc    = ""
  }
necklace8 = necklace
  { iaspects = [Timeout $ (1 `d` 3 + 3 - 1 `dl` 3) * 5]
  , ieffects = [Recharging $ Explode "spark"]
               ++ ieffects necklace
  }
necklace9 = necklace
  { iaspects = [Timeout $ (1 `d` 3 + 3 - 1 `dl` 3) * 5]
  , ieffects = [Recharging $ Explode "fragrance"]
               ++ ieffects necklace
  }

-- * Non-periodic jewelry

imageItensifier = ItemKind
  { isymbol  = symbolRing
  , iname    = "light cone"
  , ifreq    = [("treasure", 100), ("add nocto 1", 80)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 1
  , irarity  = [(7, 2), (10, 2)]
  , iverbHit = "bang"
  , iweight  = 500
  , idamage  = toDmg 0
  , iaspects = [AddNocto 1, AddSight (-1), AddArmorMelee $ (1 + 1 `dl` 3) * 3]
  , ieffects = [EqpSlot EqpSlotMiscBonus]
  , ifeature = [Precious, Identified, Durable, Equipable]
  , idesc    = "Contraption of lenses and mirrors on a polished brass headband for capturing and strengthening light in dark environment. Hampers vision in daylight. Stackable."
  , ikit     = []
  }
sightSharpening = ItemKind
  { isymbol  = symbolRing
  , iname    = "sharp monocle"
  , ifreq    = [("treasure", 10), ("add sight", 1)]  -- not unique, so very rare
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(7, 1), (10, 5)]
  , iverbHit = "rap"
  , iweight  = 50
  , idamage  = toDmg 0
  , iaspects = [AddSight $ 1 + 1 `d` 2, AddHurtMelee $ (1 `d` 2) * 3]
  , ieffects = [EqpSlot EqpSlotAddSight]
  , ifeature = [Precious, Identified, Durable, Equipable]
  , idesc    = "Let's you better focus your weaker eye."
  , ikit     = []
  }
-- Don't add standard effects to rings, because they go in and out
-- of eqp and so activating them would require UI tedium: looking for
-- them in eqp and inv or even activating a wrong item by mistake.
--
-- However, rings have the explosion effect.
-- They explode on use (and throw), for the fun of hitting everything
-- around without the risk of being hit. In case of teleportation explosion
-- this can also be used to immediately teleport close friends, as opposed
-- to throwing the ring, which takes time.
--
-- Rings should have @Identified@, so that they fully identify upon picking up.
-- Effects of many of them are seen in character sheet, so it would be silly
-- not to identify them. Necklaces provide the fun of id-by-use, because they
-- have effects and when they are triggered, they id.
ring = ItemKind
  { isymbol  = symbolRing
  , iname    = "ring"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 3)]
  , iverbHit = "knock"
  , iweight  = 15
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Precious, Identified, Equipable]
  , idesc    = "It looks like an ordinary object, but it's in fact a generator of exceptional effects: adding to some of your natural abilities and subtracting from others. You'd profit enormously if you could find a way to multiply such generators."
  , ikit     = []
  }
ring1 = ring
  { irarity  = [(10, 2)]
  , iaspects = [AddSpeed $ 1 + 1 `d` 2, AddMaxHP $ 1 `dl` 7 - 7 - 1 `d` 7]
  , ieffects = [ Explode "distortion"  -- strong magic
               , EqpSlot EqpSlotAddSpeed ]
  }
ring2 = ring
  { irarity  = [(10, 8)]
  , iaspects = [AddMaxHP $ 10 + 1 `dl` 10, AddMaxCalm $ 1 `dl` 5 - 20 - 1 `d` 5]
  , ieffects = [Explode "blast 20", EqpSlot EqpSlotAddMaxHP]
  }
ring3 = ring
  { irarity  = [(10, 3)]
  , iaspects = [AddMaxCalm $ 29 + 1 `dl` 10]
  , ieffects = [Explode "blast 20", EqpSlot EqpSlotMiscBonus]
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring4 = ring
  { irarity  = [(3, 3), (10, 5)]
  , iaspects = [ AddHurtMelee $ (1 `d` 5 + 1 `dl` 5) * 3
               , AddMaxHP $ 1 `dl` 3 - 5 - 1 `d` 3 ]
  , ieffects = [Explode "blast 20", EqpSlot EqpSlotAddHurtMelee]
  }
ring5 = ring  -- by the time it's found, probably no space in eqp
  { irarity  = [(5, 0), (10, 2)]
  , iaspects = [AddShine $ 1 `d` 2]
  , ieffects = [ Explode "distortion"  -- strong magic
               , EqpSlot EqpSlotLightSource ]
  , idesc    = "A sturdy ring with a large, shining stone."
  }
ring6 = ring
  { ifreq    = [("treasure", 100)]
  , irarity  = [(10, 2)]
  , iaspects = [ AddSpeed $ 3 + 1 `d` 4
               , AddMaxCalm $ - 20 - 1 `d` 20, AddMaxHP $ - 20 - 1 `d` 20 ]
  , ieffects = [ Unique, ELabel "of Rush"  -- no explosion, because Durable
               , EqpSlot EqpSlotAddSpeed ]
  , ifeature = Durable : ifeature ring
  -- , idesc    = ""
  }
ring7 = ring
  { ifreq    = [("useful", 10), ("ring of opportunity sniper", 1) ]
  , irarity  = [(10, 5)]
  , iaspects = [AddAbility AbProject 8]
  , ieffects = [ ELabel "of opportunity sniper"
               , Explode "distortion"  -- strong magic
               , EqpSlot EqpSlotAbProject ]
  }
ring8 = ring
  { ifreq    = [("useful", 1), ("ring of opportunity grenadier", 1) ]
  , irarity  = [(1, 1)]
  , iaspects = [AddAbility AbProject 11]
  , ieffects = [ ELabel "of opportunity grenadier"
               , Explode "distortion"  -- strong magic
               , EqpSlot EqpSlotAbProject ]
  }

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
  , idamage  = toDmg 0
  , iaspects = [ AddHurtMelee (-2)
               , AddArmorMelee $ (1 + 1 `d` 2 + 1 `dl` 2) * 5
               , AddArmorRanged $ (1 `dl` 3) * 3 ]
  , ieffects = [EqpSlot EqpSlotAddArmorMelee]
  , ifeature = [Durable, Identified, Equipable]
  , idesc    = "A stiff jacket formed from leather boiled in bee wax, padded linen and horse hair. Protects from anything that is not too sharp. Smells much better than the rest of your garment."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "mail armor"
  , ifreq    = [("useful", 100), ("torso armor", 1), ("armor ranged", 50) ]
  , iflavour = zipPlain [Cyan]
  , irarity  = [(6, 9), (10, 3)]
  , iweight  = 12000
  , idamage  = toDmg 0
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ (1 + 1 `d` 2 + 1 `dl` 2) * 5
               , AddArmorRanged $ (2 + 1 `d` 2 + 1 `dl` 3) * 3 ]
  , ieffects = [EqpSlot EqpSlotAddArmorRanged]
  , ifeature = [Durable, Identified, Equipable]
  , idesc    = "A long shirt woven from iron rings that are hard to pierce through. Discourages foes from attacking your torso, making it harder for them to hit you."
  }
gloveFencing = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "leather glove"
  , ifreq    = [("useful", 100), ("armor ranged", 50)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]
  , iverbHit = "flap"
  , iweight  = 100
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [ AddHurtMelee $ (1 `d` 2 + 1 `dl` 7) * 3
               , AddArmorRanged $ (1 `dl` 2) * 3 ]
  , ieffects = [EqpSlot EqpSlotAddHurtMelee]
  , ifeature = [ toVelocity 50  -- flaps and flutters
               , Durable, Identified, Equipable ]
  , idesc    = "A fencing glove from rough leather ensuring a good grip. Also quite effective in deflecting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "steel gauntlet"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrCyan]
  , irarity  = [(1, 9), (10, 3)]
  , iweight  = 300
  , idamage  = toDmg $ 2 `d` 1
  , iaspects = [ AddArmorMelee $ (2 + 1 `dl` 2) * 5
               , AddArmorRanged $ (1 `dl` 1) * 3 ]
  , ieffects = [EqpSlot EqpSlotAddArmorMelee]
  , idesc    = "Long leather gauntlet covered in overlapping steel plates."
  }
gloveJousting = gloveFencing
  { iname    = "Tournament Gauntlet"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 3), (10, 3)]
  , iweight  = 1000
  , idamage  = toDmg $ 3 `d` 1
  , iaspects = [ AddHurtMelee $ (1 `dl` 4 - 6) * 3
               , AddArmorMelee $ (2 + 1 `d` 2 + 1 `dl` 2) * 5
               , AddArmorRanged $ (1 `dl` 2) * 3 ]
  , ieffects = [Unique, EqpSlot EqpSlotAddArmorMelee]
  , idesc    = "Rigid, steel, jousting handgear. If only you had a lance. And a horse."
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
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(4, 6)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = [(96, 2 `d` 1), (3, 4 `d` 1), (1, 8 `d` 1)]
  , iaspects = [ AddArmorMelee 40  -- not enough to compensate; won't be in eqp
               , AddHurtMelee (-30)  -- too harmful; won't be wielded as weapon
               , Timeout $ (1 `d` 3 + 3 - 1 `dl` 3) * 2 ]
  , ieffects = [ Recharging (PushActor (ThrowMod 200 50))
               , EqpSlot EqpSlotAddArmorMelee ]
  , ifeature = [ toVelocity 50  -- unwieldy to throw
               , Durable, Identified, Meleeable ]
  , idesc    = "Heavy and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too small to intercept projectiles with."
  , ikit     = []
  }
shield = buckler
  { iname    = "shield"
  , irarity  = [(8, 3)]
  , iflavour = zipPlain [Green]
  , iweight  = 3000
  , idamage  = [(96, 4 `d` 1), (3, 8 `d` 1), (1, 16 `d` 1)]
  , iaspects = [ AddArmorMelee 80  -- not enough to compensate; won't be in eqp
               , AddHurtMelee (-70)  -- too harmful; won't be wielded as weapon
               , Timeout $ (1 `d` 6 + 6 - 1 `dl` 6) * 2 ]
  , ieffects = [ Recharging (PushActor (ThrowMod 400 50))
               , EqpSlot EqpSlotAddArmorMelee ]
  , ifeature = [ toVelocity 50  -- unwieldy to throw
               , Durable, Identified, Meleeable ]
  , idesc    = "Large and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  }

-- * Weapons

dagger = ItemKind
  { isymbol  = symbolEdged
  , iname    = "dagger"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1, 50), (3, 1)]
  , iverbHit = "stab"
  , iweight  = 800
  , idamage  = toDmg $ 6 `d` 1
  , iaspects = [ AddHurtMelee $ (1 `d` 3 + 1 `dl` 3) * 3
               , AddArmorMelee $ (1 `d` 2) * 5 ]
  , ieffects = [EqpSlot EqpSlotWeapon]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Identified, Meleeable ]
  , idesc    = "A short dagger for thrusting and parrying blows. Does not penetrate deeply, but is hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = dagger
  { iname    = "Double Dagger"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(1, 1), (10, 4)]
  -- The timeout has to be small, so that the player can count on the effect
  -- occuring consistently in any longer fight. Otherwise, the effect will be
  -- absent in some important fights, leading to the feeling of bad luck,
  -- but will manifest sometimes in fights where it doesn't matter,
  -- leading to the feeling of wasted power.
  -- If the effect is very powerful and so the timeout has to be significant,
  -- let's make it really large, for the effect to occur only once in a fight:
  -- as soon as the item is equipped, or just on the first strike.
  , iaspects = iaspects dagger ++ [Timeout $ (1 `d` 3 + 4 - 1 `dl` 3) * 2]
  , ieffects = ieffects dagger
               ++ [ Unique
                  , Recharging DropBestWeapon, Recharging $ RefillCalm (-3) ]
  , idesc    = "A double dagger that a focused fencer can use to catch and twist an opponent's blade occasionally."
  }
hammer = ItemKind
  { isymbol  = symbolHafted
  , iname    = "war hammer"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipFancy [BrMagenta]  -- avoid "pink"
  , icount   = 1
  , irarity  = [(5, 20), (8, 1)]
  , iverbHit = "club"
  , iweight  = 1600
  , idamage  = [(96, 8 `d` 1), (3, 12 `d` 1), (1, 16 `d` 1)]
  , iaspects = [AddHurtMelee $ (1 `d` 2 + 1 `dl` 2) * 3]
  , ieffects = [EqpSlot EqpSlotWeapon]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Identified, Meleeable ]
  , idesc    = "It may not cause grave wounds, but neither does it glance off nor ricochet. Great sidearm for opportunistic blows against armored foes."
  , ikit     = []
  }
hammerParalyze = hammer
  { iname    = "Concussion Hammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , idamage  = toDmg $ 8 `d` 1
  , iaspects = iaspects hammer ++ [Timeout $ (1 `d` 2 + 3 - 1 `dl` 2) * 2]
  , ieffects = ieffects hammer ++ [Unique, Recharging $ Paralyze 10]
  -- , idesc    = ""
  }
hammerSpark = hammer
  { iname    = "Grand Smithhammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , idamage  = toDmg $ 12 `d` 1
  , iaspects = iaspects hammer ++ [AddShine 3, Timeout $ (1 `d` 4 + 4 - 1 `dl` 4) * 2]
  , ieffects = ieffects hammer ++ [Unique, Recharging $ Explode "spark"]
  -- , idesc    = ""
  }
sword = ItemKind
  { isymbol  = symbolEdged
  , iname    = "sword"
  , ifreq    = [("useful", 100), ("starting weapon", 10)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(4, 1), (5, 15)]
  , iverbHit = "slash"
  , iweight  = 2000
  , idamage  = toDmg $ 10 `d` 1
  , iaspects = []
  , ieffects = [EqpSlot EqpSlotWeapon]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Identified, Meleeable ]
  , idesc    = "Difficult to master; deadly when used effectively. The steel is particularly hard and keen, but rusts quickly without regular maintenance."
  , ikit     = []
  }
swordImpress = sword
  { iname    = "Master's Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , iaspects = [Timeout $ (1 `d` 4 + 5 - 1 `dl` 4) * 2]
  , ieffects = ieffects sword ++ [Unique, Recharging Impress]
  , idesc    = "A particularly well-balance blade, lending itself to impressive shows of fencing skill."
  }
swordNullify = sword
  { iname    = "Gutting Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , iaspects = [Timeout $ (1 `d` 4 + 5 - 1 `dl` 4) * 2]
  , ieffects = ieffects sword
               ++ [ Unique
                  , Recharging $ DropItem 1 maxBound COrgan
                                          "temporary condition"
                  , Recharging $ RefillCalm (-10) ]
  , idesc    = "Cold, thin blade that pierces deeply and sends its victim into abrupt, sobering shock."
  }
halberd = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "war scythe"
  , ifreq    = [("useful", 100), ("starting weapon", 20)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(8, 1), (9, 40)]
  , iverbHit = "impale"
  , iweight  = 3000
  , idamage  = [(96, 12 `d` 1), (3, 18 `d` 1), (1, 24 `d` 1)]
  , iaspects = [ AddHurtMelee (-20)  -- just benign enough to be used
               , AddArmorMelee $ (1 + 1 `dl` 3) * 5 ]
  , ieffects = [EqpSlot EqpSlotWeapon]
  , ifeature = [ toVelocity 20  -- not balanced
               , Durable, Identified, Meleeable ]
  , idesc    = "An improvised but deadly weapon made of a blade from a scythe attached to a long pole."
  , ikit     = []
  }
halberdPushActor = halberd
  { iname    = "Swiss Halberd"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(8, 1), (9, 20)]
  , idamage  = toDmg $ 12 `d` 1
  , iaspects = iaspects halberd ++ [Timeout $ (1 `d` 5 + 5 - 1 `dl` 5) * 2]
  , ieffects = ieffects halberd
               ++ [Unique, Recharging (PushActor (ThrowMod 400 25))]
  , idesc    = "A versatile polearm, with great reach and leverage. Foes are held at a distance."
  }

-- * Wands

wand = ItemKind
  { isymbol  = symbolWand
  , iname    = "wand"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , irarity  = []
  , iverbHit = "club"
  , iweight  = 300
  , idamage  = toDmg 0
  , iaspects = [AddShine 1, AddSpeed (-1)]  -- pulsing with power, distracts
  , ieffects = []
  , ifeature = [ toVelocity 125  -- magic
               , Applicable, Durable ]
  , idesc    = "Buzzing with dazzling light that shines even through appendages that handle it."  -- will have math flavour
  , ikit     = []
  }
wand1 = wand
  { ieffects = []  -- will be: emit a cone of sound shrapnel that makes enemy cover his ears and so drop '|' and '{'
  }
wand2 = wand
  { ieffects = []
  }

-- * Treasure

gem = ItemKind
  { isymbol  = symbolGold
  , iname    = "gem"
  , ifreq    = [("treasure", 100), ("gem", 100)]
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = []
  , iverbHit = "tap"
  , iweight  = 50
  , idamage  = toDmg 0
  , iaspects = [AddShine 1, AddSpeed (-1)]
                 -- reflects strongly, distracts; so it glows in the dark,
                 -- is visible on dark floor, but not too tempting to wear
  , ieffects = [RefillCalm (-1)]  -- minor effect to ensure no id-on-pickup
  , ifeature = [Precious]  -- no @Identified@, so kind not known
  , idesc    = "Useless, and still worth around 100 gold each. Would gems of thought and pearls of artful design be valued that much in our age of Science and Progress!"
  , ikit     = []
  }
gem1 = gem
  { irarity  = [(3, 0), (10, 24)]
  }
gem2 = gem
  { irarity  = [(5, 0), (10, 28)]
  }
gem3 = gem
  { irarity  = [(7, 0), (10, 32)]
  }
gem4 = gem
  { irarity  = [(9, 0), (10, 100)]
  }
gem5 = gem
  { isymbol  = symbolSpecial
  , iname    = "elixir"
  , iflavour = zipPlain [BrYellow]
  , irarity  = [(1, 40), (10, 40)]
  , iaspects = []
  , ieffects = [ELabel "of youth", RefillCalm 5, RefillHP 15]
  , ifeature = [Identified, Applicable, Precious]
  , idesc    = "A crystal vial of amber liquid, supposedly granting eternal youth and fetching 100 gold per piece. The main effect seems to be mild euphoria, but it admittedly heals minor ailments rather well."
  }
currency = ItemKind
  { isymbol  = symbolGold
  , iname    = "gold piece"
  , ifreq    = [("treasure", 100), ("currency", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + 1 `d` 20 + 1 `dl` 20
  , irarity  = [(1, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 31
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Identified, Precious]
  , idesc    = "Reliably valuable in every civilized plane of existence."
  , ikit     = []
  }
