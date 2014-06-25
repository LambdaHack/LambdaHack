-- | Weapon and treasure definitions.
module Content.ItemKind ( cdefs ) where

import Data.List

import Content.ItemKindActor
import Content.ItemKindBodyPart
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
  , content = items ++ bodyParts ++ shrapnels ++ actors
  }

items :: [ItemKind]
items =
  [bolas, brassLantern, dart, dart100, gem1, gem2, gem3, currency, harpoon, jumpingPole, monocle, necklace1, necklace2, necklace3, net, oilLamp, potion1, potion2, potion3, potion4, ring1, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, shield, sword, wand1, wand2, woodenTorch]

bolas,    brassLantern, dart, dart100, gem1, gem2, gem3, currency, harpoon, jumpingPole, monocle, necklace1, necklace2, necklace3, net, oilLamp, potion1, potion2, potion3, potion4, ring1, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, shield, sword, wand1, wand2, woodenTorch :: ItemKind

gem, necklace, potion, ring, scroll, wand :: ItemKind  -- generic templates

bolas = ItemKind
  { isymbol  = '|'
  , iname    = "a set of bolas"
  , ifreq    = [("useful", 5)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1 + dl 3
  , iverbApply   = "tie"
  , iverbProject = "swirl"
  , iweight  = 500
  , iaspects = []
  , ieffects = [Hurt (2 * d 1) 0, Paralyze (5 + d 5), ActivateEqp '!']
  , ifeature = []
  , idesc    = "Wood balls tied with hemp rope for tripping, entangling and bringing down crashing."
  , ikit     = []
  }
brassLantern = ItemKind
  { isymbol  = '('
  , iname    = "brass lantern"
  , ifreq    = [("useful", 2)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , iverbApply   = "douse"
  , iverbProject = "heave"
  , iweight  = 2400
  , iaspects = [AddLight 4, AddSight (-1)]
  , ieffects = [Burn 4, Paralyze 4, OnSmash (Explode "burning oil 4")]
  , ifeature = [ toVelocity 70  -- hard to throw so that it opens and burns
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "Very bright and very heavy brass lantern."
  , ikit     = []
  }
dart = ItemKind
  { isymbol  = '|'
  , iname    = "dart"
  , ifreq    = [("useful", 20), ("any arrow", 100), ("fallback item", 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 3 * d 3
  , iverbApply   = "snap"
  , iverbProject = "hurl"
  , iweight  = 50
  , iaspects = []
  , ieffects = [Hurt (2 * d 1) (d 3 + dl 3)]
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
  , iverbApply   = "snap"
  , iverbProject = "hurl"
  , iweight  = 50
  , iaspects = []
  , ieffects = [Hurt (d 1) (d 2 + dl 2)]
  , ifeature = [toVelocity 200]
  , idesc    = "Finely balanced for throws of great speed."
  , ikit     = []
  }
gem = ItemKind
  { isymbol  = '*'
  , iname    = "gem"
  , ifreq    = [("treasure", 20)]  -- x3, but rare on shallow levels
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 0
  , iverbApply   = "crush"
  , iverbProject = "toss"
  , iweight  = 50
  , iaspects = [AddLight 1]  -- just reflects strongly
  , ieffects = []
  , ifeature = [ Durable  -- prevent destruction by evil monsters
               , Precious ]
  , idesc    = "Precious, though useless. Worth around 100 gold."
  , ikit     = []
  }
gem1 = gem
  { icount   = dl 1  -- appears on max depth
  }
gem2 = gem
  { icount   = dl 2  -- appears halfway
  }
gem3 = gem
  { icount   = dl 3  -- appears early
  }
currency = ItemKind
  { isymbol  = '$'
  , iname    = "gold piece"
  , ifreq    = [("treasure", 20), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 * dl 10  -- appears on lvl 2
  , iverbApply   = "grind"
  , iverbProject = "toss"
  , iweight  = 31
  , iaspects = []
  , ieffects = []
  , ifeature = [Durable, Identified, Precious]
  , idesc    = "Reliably valuable in every civilized place."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = '|'
  , iname    = "harpoon"
  , ifreq    = [("useful", 15)]
  , iflavour = zipPlain [Brown]
  , icount   = 1 + dl 3
  , iverbApply   = "break up"
  , iverbProject = "hurl"
  , iweight  = 4000
  , iaspects = []
  , ieffects = [Hurt (3 * d 1) (d 2 + 2 * dl 2), PullActor (ThrowMod 200 50)]
  , ifeature = []
  , idesc    = "The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
jumpingPole = ItemKind
  { isymbol  = '-'
  , iname    = "jumping pole"
  , ifreq    = [("useful", 3)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , iverbApply   = "break up"
  , iverbProject = "extend"
  , iweight  = 10000
  , iaspects = []
  , ieffects = [InsertMove 2]  -- TODO: implement with timed speed instead
                               -- and then make Durable, freq 2, and just trade
                               -- taken turn now for a free turn later
  , ifeature = [Applicable, Identified]
  , idesc    = "Makes you vulnerable at take-off, but then you are free like a bird."
  , ikit     = []
  }
monocle = ItemKind
  { isymbol  = '['  -- TODO: a hack, we need a symbol for non-armor gear
  , iname    = "monocle"
  , ifreq    = [("useful", 1)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , iverbApply   = "focus"
  , iverbProject = "toss"
  , iweight  = 50
  , iaspects = [AddSight $ 1 + dl 3]
  , ieffects = []
  , ifeature = [Durable, EqpSlot EqpSlotAddSight "", Identified]
  , idesc    = "Let's you better focus your weaker eye."
  , ikit     = []
  }
necklace = ItemKind  -- TODO: when more items, perhaps make all Periodic necklaces
  { isymbol  = '"'
  , iname    = "necklace"
  , ifreq    = [("useful", 3)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , iverbApply   = "tear down"
  , iverbProject = "cast"
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
  { ifreq    = [("useful", 1)]
  , iaspects = [Periodic $ d 4 + dl 2]
  , ieffects = [Summon $ 1 + dl 2]  -- TODO: + explosion
  }
necklace3 = necklace
  { ifreq    = [("useful", 1)]
  , iaspects = [Periodic $ d 4 + dl 2]
  , ieffects = [Paralyze $ 5 + d 5 + dl 5, RefillCalm 50]
  }
net = ItemKind
  { isymbol  = '|'
  , iname    = "net"
  , ifreq    = [("useful", 5)]
  , iflavour = zipPlain [White]
  , icount   = 1 + dl 2
  , iverbApply   = "entangle"
  , iverbProject = "spread"
  , iweight  = 1000
  , iaspects = []
  , ieffects = [ Hurt (d 1) 0, Paralyze (5 + d 5)
               , DropBestWeapon, DropEqp ']' False ]
  , ifeature = []
  , idesc    = "A wide net with weights along the edges. Entangles weapon and armor alike."  -- shield instead of armor if a separate symbol for shields
  , ikit     = []
  }
oilLamp = ItemKind
  { isymbol  = '('
  , iname    = "oil lamp"
  , ifreq    = [("useful", 4)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , iverbApply   = "douse"
  , iverbProject = "lob"
  , iweight  = 1000
  , iaspects = [AddLight 3, AddSight (-1)]
  , ieffects = [Burn 3, Paralyze 3, OnSmash (Explode "burning oil 3")]
  , ifeature = [ toVelocity 70  -- hard not to spill the oil while throwing
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "A clay lamp filled with plant oil feeding a tiny wick."
  , ikit     = []
  }
potion = ItemKind
  { isymbol  = '!'
  , iname    = "potion"
  , ifreq    = [("useful", 10)]
  , iflavour = zipPlain stdCol ++ zipFancy brightCol
  , icount   = 1
  , iverbApply   = "gulp down"
  , iverbProject = "lob"
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
  { ifreq    = [("useful", 5)]
  , ieffects = [RefillHP (-5), OnSmash (Explode "wounding mist")]
  }
potion4 = potion
  { ieffects = [ PushActor (ThrowMod 200 75)
               , Explode "explosion blast 10"
               , OnSmash (Explode "explosion blast 10") ]
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = [("useful", 3)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , iverbApply   = "squeeze down"
  , iverbProject = "toss"
  , iweight  = 15
  , iaspects = []
  , ieffects = []
  , ifeature = [Precious]
  , idesc    = "A sturdy ring with a strangely shining eye."
  , ikit     = []
  }
ring1 = ring
  { ifreq    = [("useful", 1)]
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
  { ifreq    = [("useful", 1)]
  , iaspects = [Periodic $ 2 * d 10 + dl 20]
  , ieffects = [InsertMove 1, RefillHP (-1)]  -- TODO: allow fractions for smooth?
  , ifeature = [Durable]  -- evil players would throw before death, to destroy
      -- TODO: teach AI to wear only for fight; prevent players from meleeing
      -- allies with that (Durable, perhaps require Hurt for melee)
  }
scroll = ItemKind
  { isymbol  = '?'
  , iname    = "scroll"
  , ifreq    = [("useful", 4), ("any scroll", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain darkCol  -- arcane and old
  , icount   = 1
  , iverbApply   = "decipher"
  , iverbProject = "lob"
  , iweight  = 50
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 25  -- bad shape, even rolled up
               , Applicable ]
  , idesc    = "A haphazardly scribbled piece of parchment. May contain directions or a secret call sign."
  , ikit     = []
  }
scroll1 = scroll
  { ifreq    = [("useful", 2)]
  , ieffects = [CallFriend 1]
  }
scroll2 = scroll
  { ieffects = [Summon $ 1 + dl 2]
  }
scroll3 = scroll
  { ieffects = [Ascend (-1)]
  }
scroll4 = scroll
  { ifreq    = [("useful", 1)]
  , ieffects = [Dominate]
  }
scroll5 = scroll
  { ifreq    = [("useful", 5)]
  , ieffects = [Teleport $ 2 + d 5]
  }
scroll6 = scroll
  { ifreq    = [("useful", 2)]
  , ieffects = [DropBestWeapon, Teleport $ 15 + d 10]
  }
scroll7 = scroll
  { ifreq    = [("useful", 1)]
  , ieffects = [InsertMove (d 2 + dl 2)]
  }
-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
shield = ItemKind
  { isymbol  = ']'
  , iname    = "shield"
  , ifreq    = [("useful", 5)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , iverbApply   = "bash"
  , iverbProject = "push"
  , iweight  = 3000
  , iaspects = [AddArmorMelee 50, AddHurtMelee (-40)]
  , ieffects = []
  , ifeature = [ toVelocity 20  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Large and unwieldy. Absorbs the precentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with. WIP (absolute value of total bonus capped at 75%)"
  , ikit     = []
  }
sword = ItemKind
  { isymbol  = ')'
  , iname    = "sword"
  , ifreq    = [("useful", 40)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , iverbApply   = "hit"
  , iverbProject = "heave"
  , iweight  = 2000
  , iaspects = []
  , ieffects = [Hurt (5 * d 1) (d 2 + 4 * dl 2)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the tip costs speed
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "A standard heavy weapon. Does not penetrate very effectively, but hard to block."
  , ikit     = []
  }
wand = ItemKind
  { isymbol  = '/'
  , iname    = "wand"
  , ifreq    = []  -- TODO: add charges, etc.  -- [("useful", 2)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , iverbApply   = "snap"
  , iverbProject = "zap"
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
woodenTorch = ItemKind
  { isymbol  = '('
  , iname    = "wooden torch"
  , ifreq    = [("useful", 8)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , iverbApply   = "douse"
  , iverbProject = "fling"
  , iweight  = 1200
  , iaspects = [ AddLight 3
               , AddSight (-2) ]  -- not only flashes, but also sparks
  , ieffects = [Burn 3]
  , ifeature = [EqpSlot EqpSlotAddLight "", Identified]
  , idesc    = "A smoking, heavy wooden torch, burning in an unsteady fire."
  , ikit     = []
  }
