-- | Weapon and treasure definitions.
module Content.ItemKind ( cdefs ) where

import Data.List

import Content.ItemKindActor
import Content.ItemKindOrgan
import Content.ItemKindShrapnel
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Content.ItemKind

cdefs :: ContentDef ItemKind
cdefs = ContentDef
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validate = validateItemKind
  , content = items ++ organs ++ shrapnels ++ actors
  }

items :: [ItemKind]
items =
  [bolas, brassLantern, dart, dart100, gem1, gem2, gem3, currency, harpoon, jumpingPole, monocle, necklace1, necklace2, necklace3, net, oilLamp, potion1, potion2, potion3, potion4, ring1, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, shield, dagger, hammer, sword, halberd, wand1, wand2, woodenTorch]

bolas,    brassLantern, dart, dart100, gem1, gem2, gem3, currency, harpoon, jumpingPole, monocle, necklace1, necklace2, necklace3, net, oilLamp, potion1, potion2, potion3, potion4, ring1, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, shield, dagger, hammer, sword, halberd, wand1, wand2, woodenTorch :: ItemKind

gem, necklace, potion, ring, scroll, wand :: ItemKind  -- generic templates

-- * Thrown weapons

dart = ItemKind
  { isymbol  = '|'
  , iname    = "dart"
  , ifreq    = [("useful", 20), ("any arrow", 100)]
  , iflavour = zipPlain [Cyan]
  , icount   = 3 * d 3
  , irarity  = [(1, 20)]
  , iverbHit = "prick"
  , iweight  = 50
  , iaspects = [AddHurtRanged ((d 6 + dl 6) * 10)]
  , ieffects = [Hurt (3 * d 1)]
  , ifeature = []
  , idesc    = "Little, but sharp and sturdy."
  , ikit     = []
  }
dart100 = ItemKind
  { isymbol  = '|'
  , iname    = "fine dart"
  , ifreq    = [("useful", 20), ("any arrow", 50)]  -- TODO: until arrows added
  , iflavour = zipPlain [BrRed]
  , icount   = 3 * d 3
  , irarity  = [(4, 20)]
  , iverbHit = "prick"
  , iweight  = 50
  , iaspects = [AddHurtRanged ((d 6 + dl 6) * 10)]
  , ieffects = [Hurt (2 * d 1)]
  , ifeature = [toVelocity 200]
  , idesc    = "Finely balanced for throws of great speed."
  , ikit     = []
  }

-- * Exotic thrown weapons

bolas = ItemKind
  { isymbol  = '|'
  , iname    = "bolas set"
  , ifreq    = [("useful", 5)]
  , iflavour = zipPlain [BrYellow]
  , icount   = dl 4
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "entangle"
  , iweight  = 500
  , iaspects = []
  , ieffects = [Hurt (2 * d 1), Paralyze (5 + d 5), ActivateEqp '!']
  , ifeature = []
  , idesc    = "Wood balls tied with hemp rope for tripping, entangling and bringing down crashing."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = '|'
  , iname    = "harpoon"
  , ifreq    = [("useful", 15)]
  , iflavour = zipPlain [Brown]
  , icount   = dl 5
  , irarity  = [(5, 5), (10, 20)]
  , iverbHit = "hook"
  , iweight  = 4000
  , iaspects = [AddHurtRanged ((d 2 + 2 * dl 5) * 10)]
  , ieffects = [Hurt (4 * d 1), PullActor (ThrowMod 200 50)]
  , ifeature = []
  , idesc    = "The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
net = ItemKind
  { isymbol  = '|'
  , iname    = "net"
  , ifreq    = [("useful", 5)]
  , iflavour = zipPlain [White]
  , icount   = dl 3
  , irarity  = [(3, 5), (10, 4)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , iaspects = []
  , ieffects = [ Paralyze (5 + d 5)
               , DropBestWeapon, DropEqp ']' False ]
  , ifeature = []
  , idesc    = "A wide net with weights along the edges. Entangles weapon and armor alike."  -- TODO: shield instead of armor if a separate symbol for shields
  , ikit     = []
  }

-- * Lights

woodenTorch = ItemKind
  { isymbol  = '('
  , iname    = "wooden torch"
  , ifreq    = [("useful", 8)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 10)]
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
  { isymbol  = '('
  , iname    = "oil lamp"
  , ifreq    = [("useful", 4)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 4), (10, 4)]
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
  { isymbol  = '('
  , iname    = "brass lantern"
  , ifreq    = [("useful", 2)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(10, 3)]
  , iverbHit = "burn"
  , iweight  = 2400
  , iaspects = [AddLight 4, AddSight (-1)]
  , ieffects = [Burn 4, Paralyze 4, OnSmash (Explode "burning oil 4")]
  , ifeature = [ toVelocity 70  -- hard to throw so that it opens and burns
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "Very bright and very heavy brass lantern."
  , ikit     = []
  }

-- * Treasure

gem = ItemKind
  { isymbol  = '*'
  , iname    = "gem"
  , ifreq    = [("treasure", 20)]  -- x3, but rare on shallow levels
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = []
  , iverbHit = "tap"
  , iweight  = 50
  , iaspects = [AddLight 1]  -- just reflects strongly
  , ieffects = []
  , ifeature = [ Durable  -- prevent destruction by evil monsters
               , Precious ]
  , idesc    = "Precious, though useless. Worth around 100 gold."
  , ikit     = []
  }
gem1 = gem
  { irarity  = [(2, 0), (10, 10)]
  }
gem2 = gem
  { irarity  = [(5, 0), (10, 10)]
  }
gem3 = gem
  { irarity  = [(8, 0), (10, 10)]
  }
currency = ItemKind
  { isymbol  = '$'
  , iname    = "gold piece"
  , ifreq    = [("treasure", 20), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + d 20 + dl 20
  , irarity  = [(1, 0), (5, 20), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 31
  , iaspects = []
  , ieffects = []
  , ifeature = [Durable, Identified, Precious]
  , idesc    = "Reliably valuable in every civilized place."
  , ikit     = []
  }

jumpingPole = ItemKind
  { isymbol  = '-'
  , iname    = "jumping pole"
  , ifreq    = [("useful", 3)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 4), (10, 2)]
  , iverbHit = "prod"
  , iweight  = 10000
  , iaspects = []
  , ieffects = [InsertMove 2]  -- TODO: implement with timed speed instead
                               -- and then make Durable, freq 2, and just trade
                               -- taken turn now for a free turn later
  , ifeature = [Applicable, Identified]
  , idesc    = "Makes you vulnerable at take-off, but then you are free like a bird."
  , ikit     = []
  }

-- * Periodic jewelry

necklace = ItemKind  -- TODO: when more items, perhaps make all Periodic necklaces
  { isymbol  = '"'
  , iname    = "necklace"
  , ifreq    = [("useful", 3)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , irarity  = [(4, 1), (10, 3)]
  , icount   = 1
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = []
  , ieffects = []
  , ifeature = [ Precious, EqpSlot EqpSlotPeriodic ""
               , toVelocity 50 ]  -- not dense enough
  , idesc    = "Tingling, rattling chain of flat encrusted links."
  , ikit     = []
  }
necklace1 = necklace
  { iaspects = [Periodic $ d 2 + dl 2]
  , ieffects = [RefillHP 1]
  , idesc    = "A cord of dried herbs and healing berries."
  }
necklace2 = necklace
  { irarity  = [(2, 0), (10, 1)]
  , iaspects = [Periodic $ d 4 + dl 2]
  , ieffects = [Summon $ 1 + dl 2]  -- TODO: + explosion
  }
necklace3 = necklace
  { iaspects = [Periodic $ d 4 + dl 2]
  , ieffects = [Paralyze $ 5 + d 5 + dl 5, RefillCalm 50]
  }

-- * Non-periodic jewelry

monocle = ItemKind
  { isymbol  = '='
  , iname    = "monocle"
  , ifreq    = [("useful", 1)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(6, 0), (10, 1)]
  , iverbHit = "rap"
  , iweight  = 50
  , iaspects = [AddSight $ dl 3]
  , ieffects = []
  , ifeature = [Durable, EqpSlot EqpSlotAddSight "", Identified]
  , idesc    = "Let's you better focus your weaker eye."
  , ikit     = []
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = [("useful", 3)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(6, 1), (10, 3)]
  , iverbHit = "knock"
  , iweight  = 15
  , iaspects = []
  , ieffects = []
  , ifeature = [Precious]
  , idesc    = "A sturdy ring with a strangely shining eye."
  , ikit     = []
  }
ring1 = ring
  { irarity  = [(4, 1), (10, 2)]
  , iaspects = [Periodic $ d 4 + dl 2]
  , ieffects = [RefillCalm 1]
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with letters that meant a lot to somebody."
  }
ring2 = ring
  { iaspects = [Periodic $ 2 * d 10 + dl 10]
  , ieffects = [Teleport $ 2 + d 3]
  }
ring3 = ring
  { iaspects = [Periodic $ d 4 + dl 2]
  , ieffects = [Teleport $ 10 + d 10]
  }
ring4 = ring
  { iaspects = [Periodic $ 2 * d 5 + dl 5]
  , ieffects = [PushActor (ThrowMod 100 50)]
  }
ring5 = ring
  {  irarity  = [(4, 0), (10, 2)]
  , iaspects = [Periodic $ 2 * d 10 + dl 20]
  , ieffects = [InsertMove 1, RefillHP (-1)]
  , ifeature = [Durable]  -- evil players would throw before death, to destroy
      -- TODO: teach AI to wear only for fight; prevent players from meleeing
      -- allies with that (Durable)
  }

-- * Exploding consumables

potion = ItemKind
  { isymbol  = '!'
  , iname    = "potion"
  , ifreq    = [("useful", 10)]
  , iflavour = zipPlain stdCol ++ zipFancy brightCol
  , icount   = 1
  , irarity  = [(1, 10), (10, 8)]
  , iverbHit = "splash"
  , iweight  = 200
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 50  -- oily, bad grip
               , Applicable, Fragile ]
  , idesc    = "A flask of bubbly, slightly oily liquid of a suspect color."
  , ikit     = []
  }
potion1 = potion
  { ieffects = [ApplyPerfume, Impress, OnSmash (Explode "fragrance")]
  }
potion2 = potion
  { ieffects = [RefillHP 5, OnSmash (Explode "healing mist")]
  }
potion3 = potion  -- TODO: a bit boring
  { irarity  = [(1, 5), (10, 1)]
  , ieffects = [RefillHP (-5), OnSmash (Explode "wounding mist")]
  }
potion4 = potion
  { ieffects = [ Explode "explosion blast 10"
               , PushActor (ThrowMod 200 75)
               , OnSmash (Explode "explosion blast 10") ]
  }

-- * Non-exploding consumables

scroll = ItemKind
  { isymbol  = '?'
  , iname    = "scroll"
  , ifreq    = [("useful", 4), ("any scroll", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain darkCol  -- arcane and old
  , icount   = 1
  , irarity  = [(1, 10), (10, 8)]
  , iverbHit = "thump"
  , iweight  = 50
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 25  -- bad shape, even rolled up
               , Applicable ]
  , idesc    = "A haphazardly scribbled piece of parchment. May contain directions or a secret call sign."
  , ikit     = []
  }
scroll1 = scroll
  { irarity  = [(10, 2)]
  , ieffects = [CallFriend 1]
  }
scroll2 = scroll
  { ieffects = [Summon $ 1 + dl 2]
  }
scroll3 = scroll
  { ieffects = [Ascend (-1)]
  }
scroll4 = scroll
  { irarity  = [(10, 1)]
  , ieffects = [Dominate]
  }
scroll5 = scroll
  { ieffects = [Teleport $ 2 + d 5]
  }
scroll6 = scroll
  { irarity  = [(10, 2)]
  , ieffects = [DropBestWeapon, Teleport $ 15 + d 10]
  }
scroll7 = scroll
  { irarity  = [(10, 1)]
  , ieffects = [InsertMove (d 2 + dl 2)]
  }

-- * Armor

-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
shield = ItemKind
  { isymbol  = ']'
  , iname    = "shield"
  , ifreq    = [("useful", 5)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(7, 7)]
  , iverbHit = "bash"
  , iweight  = 3000
  , iaspects = [AddArmorMelee 50, AddHurtMelee (-40)]
  , ieffects = []
  , ifeature = [ toVelocity 20  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Large and unwieldy. Absorbs a precentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  , ikit     = []
  }

-- * Weapons

dagger = ItemKind
  { isymbol  = ')'
  , iname    = "dagger"
  , ifreq    = [("useful", 20)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1, 20), (10, 4)]
  , iverbHit = "stab"
  , iweight  = 1000
  , iaspects = [AddHurtMelee $ 2 * (d 2 + 2 * dl 5), AddArmorMelee $ d 4 + dl 4]
  , ieffects = [Hurt (4 * d 1)]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "A short dagger for thrusting and parrying blows. Does not penetrate deeply, but is hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
hammer = ItemKind
  { isymbol  = ')'
  , iname    = "war hammer"
  , ifreq    = [("useful", 10)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(3, 10), (10, 2)]
  , iverbHit = "club"
  , iweight  = 1500
  , iaspects = [AddHurtMelee $ d 2 + 2 * dl 5]
  , ieffects = [Hurt (6 * d 1)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the sharp tip costs
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "It may not cause grave wounds, but neither does it glance off nor ricochet. Great sidearm for opportinistic blows against armored foes."
  , ikit     = []
  }
sword = ItemKind
  { isymbol  = ')'
  , iname    = "sword"
  , ifreq    = [("useful", 20)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(3, 1), (6, 20), (10, 10)]
  , iverbHit = "slash"
  , iweight  = 2000
  , iaspects = []
  , ieffects = [Hurt (9 * d 1)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the tip costs speed
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "Hard to master, but chops limbs when used effectively."
  , ikit     = []
  }
halberd = ItemKind
  { isymbol  = ')'
  , iname    = "halberd"
  , ifreq    = [("useful", 10)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(7, 1), (10, 10)]
  , iverbHit = "impale"
  , iweight  = 3000
  , iaspects = [AddArmorMelee $ 2 * (d 4 + dl 4)]
  , ieffects = [Hurt (12 * d 1)]
  , ifeature = [ toVelocity 20  -- not balanced
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "Versatile, with great reach and leverage. Foes are held at a distance."
  , ikit     = []
  }

-- * Wands

wand = ItemKind
  { isymbol  = '/'
  , iname    = "wand"
  , ifreq    = [("useful", 2)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , irarity  = []  -- TODO: add charges, etc.
  , iverbHit = "club"
  , iweight  = 300
  , iaspects = [AddLight 1]
  , ieffects = []
  , ifeature = [ toVelocity 125  -- magic
               , Applicable, Durable ]
  , idesc    = "Buzzing with dazzling light that shines even through appendages that handle it."
  , ikit     = []
  }
wand1 = wand
  { ieffects = [NoEffect]  -- TODO: emit a cone of sound shrapnel that makes enemy cover his ears and so drop '|' and '{'
  }
wand2 = wand
  { ieffects = [NoEffect]
  }
