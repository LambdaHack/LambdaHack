-- | Item and treasure definitions.
module Content.ItemKind ( cdefs ) where

import Data.List

import Content.ItemKindActor
import Content.ItemKindOrgan
import Content.ItemKindShrapnel
import Content.ItemKindTempAspect
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
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
  , content = items ++ organs ++ shrapnels ++ actors ++ tempAspects
  }

items :: [ItemKind]
items =
  [bolas, brassLantern, buckler, dart, dart200, gem1, gem2, gem3, gloveFencing, gloveGauntlet, gloveJousting, currency, gorget, harpoon, jumpingPole, monocle, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, net, oilLamp, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, ring1, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, halberd, halberdPushActor, wand1, wand2, woodenTorch, armorLeather, armorMail, whetstone]

bolas,    brassLantern, buckler, dart, dart200, gem1, gem2, gem3, gloveFencing, gloveGauntlet, gloveJousting, currency, gorget, harpoon, jumpingPole, monocle, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, net, oilLamp, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, ring1, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, halberd, halberdPushActor, wand1, wand2, woodenTorch, armorLeather, armorMail, whetstone :: ItemKind

gem, necklace, potion, ring, scroll, wand :: ItemKind  -- generic templates

-- * Thrown weapons

dart = ItemKind
  { isymbol  = '|'
  , iname    = "dart"
  , ifreq    = [("useful", 100), ("any arrow", 100)]
  , iflavour = zipPlain [Cyan]
  , icount   = 3 * d 3
  , irarity  = [(1, 20), (10, 10)]
  , iverbHit = "prick"
  , iweight  = 50
  , iaspects = [AddHurtRanged ((d 6 + dl 6) |*| 10)]
  , ieffects = [Hurt (3 * d 1)]
  , ifeature = []
  , idesc    = "Little, but sharp and sturdy."  -- "Much inferior to arrows though, especially given the contravariance problems."  --- funny, but destroy the suspension of disbelief; this is supposed to be a Lovecraftian horror and any hilarity must ensue from the failures in making it so and not from actively trying to be funny; also, mundane objects are not supposed to be scary or transcendental; the scare is in horrors from the abstract dimension visiting our ordinary reality; without the contrast there's no horror and no wonder, so also the magical items must be contrasted with ordinary XIX century and antique items
  , ikit     = []
  }
dart200 = ItemKind
  { isymbol  = '|'
  , iname    = "fine dart"
  , ifreq    = [("useful", 100), ("any arrow", 50)]  -- TODO: until arrows added
  , iflavour = zipPlain [BrRed]
  , icount   = 3 * d 3
  , irarity  = [(4, 20), (10, 10)]
  , iverbHit = "prick"
  , iweight  = 50
  , iaspects = [AddHurtRanged ((d 6 + dl 6) |*| 10)]
  , ieffects = [Hurt (2 * d 1)]
  , ifeature = [toVelocity 200]
  , idesc    = "Finely balanced for throws of great speed."
  , ikit     = []
  }

-- * Exotic thrown weapons

bolas = ItemKind
  { isymbol  = '|'
  , iname    = "bolas set"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = dl 4
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "entangle"
  , iweight  = 500
  , iaspects = []
  , ieffects = [Hurt (2 * d 1), Paralyze (5 + d 5), ActivateInv '!']
  , ifeature = []
  , idesc    = "Wood balls tied with hemp rope for tripping, entangling and bringing down crashing."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = '|'
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
  { isymbol  = '|'
  , iname    = "net"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = dl 3
  , irarity  = [(3, 5), (10, 4)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , iaspects = []
  , ieffects = [ Paralyze (5 + d 5)
               , DropBestWeapon, DropEqp ']' False ]
  , ifeature = []
  , idesc    = "A wide net with weights along the edges. Entangles weapon and armor alike."
  , ikit     = []
  }

-- * Lights

woodenTorch = ItemKind
  { isymbol  = '('
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
  { isymbol  = '('
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
  { isymbol  = '('
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

-- * Treasure

gem = ItemKind
  { isymbol  = '*'
  , iname    = "gem"
  , ifreq    = [("treasure", 100)]  -- x3, but rare on shallow levels
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = []
  , iverbHit = "tap"
  , iweight  = 50
  , iaspects = [AddLight 1, AddSpeed (-1)]  -- reflects strongly, distracts
  , ieffects = []
  , ifeature = [ Durable  -- prevent destruction by evil monsters
               , Precious ]
  , idesc    = "Useless, and still worth around 100 gold each. Would gems of thought and pearls of artful design be valued that much in our age of Science and Progress!"
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
  , ifreq    = [("treasure", 100), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + d 20 + dl 20
  , irarity  = [(1, 0), (2, 15), (5, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 31
  , iaspects = []
  , ieffects = []
  , ifeature = [Durable, Identified, Precious]
  , idesc    = "Reliably valuable in every civilized plane of existence."
  , ikit     = []
  }

-- * Periodic jewelry

gorget = ItemKind
  { isymbol  = '"'
  , iname    = "gorget"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy [BrCyan]
  , irarity  = [(4, 1), (10, 2)]
  , icount   = 1
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = [ Periodic
               , Timeout $ (d 3 + 3 - dl 3) |*| 10
               , AddArmorMelee 1
               , AddArmorRanged 1 ]
  , ieffects = [ Recharging (RefillCalm 1)
               , RefillHP (-1) ]  -- extra pain without periodic
  , ifeature = [ Durable, Precious, EqpSlot EqpSlotPeriodic "", Identified
               , toVelocity 50 ]  -- not dense enough
  , idesc    = "Highly ornamental, cold, large, steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
  , ikit     = []
  }
necklace = ItemKind
  { isymbol  = '"'
  , iname    = "necklace"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , irarity  = [(10, 3)]
  , icount   = 1
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = [Periodic]
  , ieffects = []
  , ifeature = [ Durable, Precious, EqpSlot EqpSlotPeriodic ""
               , toVelocity 50 ]  -- not dense enough
  , idesc    = "Menacing Greek symbols shimmer with increasing speeds along a chain of fine encrusted links. After a tense build-up, a prismatic arc shoots towards the ground and the iridescence subdues, becomes ordered and resembles a harmless ornament again, for a time."
  , ikit     = []
  }
necklace1 = necklace
  { iaspects = (Timeout $ (d 3 + 4 - dl 3) |*| 10) : iaspects necklace
  , ieffects = [ Recharging (RefillHP 1)
               , Burn 1 ]  -- only beneficial if activation is periodic
  , idesc    = "A cord of dried herbs and healing berries."
  }
necklace2 = necklace
  { irarity  = [(2, 0), (10, 1)]
  , iaspects = (Timeout $ (d 3 + 3 - dl 3) |*| 10) : iaspects necklace
  , ieffects = [ Recharging (Impress)
               , Recharging (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , Recharging (Explode "waste") ]
  }
necklace3 = necklace
  { iaspects = (Timeout $ (d 3 + 3 - dl 3) |*| 10) : iaspects necklace
  , ieffects = [ Recharging (Paralyze $ 5 + d 5 + dl 5)
               , Recharging (RefillCalm 999)
               , Paralyze $ 10 + d 10 + dl 10  -- extra pain without periodic
               , OnSmash (Explode "explosion blast 2") ]
  , ifeature = Fragile : ifeature necklace  -- too powerful projection
  }
necklace4 = necklace
  { iaspects = (Timeout $ (d 4 + 4 - dl 4) |*| 2) : iaspects necklace
  , ieffects = [ Recharging (Teleport $ d 3 |*| 3)
               , RefillHP (-2)  -- price to pay if activation not periodic
               , OnSmash (Explode "explosion blast 2") ]
  , ifeature = Fragile : ifeature necklace  -- too powerful projection
  }
necklace5 = necklace
  { iaspects = (Timeout $ (d 3 + 4 - dl 3) |*| 10) : iaspects necklace
  , ieffects = [ Recharging (Teleport $ 12 + d 3 |*| 3)
               , RefillHP (-3)  -- price to pay if activation not periodic
               , OnSmash (Explode "explosion blast 2") ]
  , ifeature = Fragile : ifeature necklace  -- too powerful projection
  }
necklace6 = necklace
  { iaspects = (Timeout $ d 4 |*| 10) : iaspects necklace
  , ieffects = [ Recharging (PushActor (ThrowMod 100 50))
               , RefillHP (-1)  -- price to pay if activation not periodic
               , OnSmash (Explode "explosion blast 2") ]
  , ifeature = Fragile : ifeature necklace  -- quite powerful projection
  }
necklace7 = necklace  -- TODO: teach AI to wear only for fight
  { irarity  = [(4, 0), (10, 2)]
  , iaspects = (Timeout $ (d 3 + 3 - dl 3) |*| 2) : iaspects necklace
  , ieffects = [ Recharging (InsertMove 1)
               , Recharging (RefillHP (-1)) ]
  }

-- * Non-periodic jewelry

monocle = ItemKind
  { isymbol  = '='
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
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 3)]
  , iverbHit = "knock"
  , iweight  = 15
  , iaspects = []
  , ieffects = []
  , ifeature = [Precious, Identified]
  , idesc    = "It looks like an ordinary object, but it's in fact a generator of exceptional effects: adding to some of your natural abilities and subtracting from others. You'd profit enormously if you could find a way to multiply such generators..."  -- TODO: merge rings: do not add effects though, because it would make the ring too powerful (only one eqp slot taken); define correct, but not overpowered multiplication, if possible
  , ikit     = []
  }
ring1 = ring
  { irarity  = [(2, 0), (10, 2)]
  , iaspects = [AddSpeed $ d 2, AddMaxHP $ dl 3 - 5 - d 3]
  , ifeature = ifeature ring ++ [Durable, EqpSlot EqpSlotAddSpeed ""]
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
ring4 = ring  -- TODO: move to level-ups and to timed effects
  { irarity  = [(3, 12), (10, 12)]
  , iaspects = [AddHurtMelee $ (d 5 + dl 5) |*| 3, AddMaxHP $ dl 3 - 4 - d 2]
  , ifeature = ifeature ring ++ [Durable, EqpSlot EqpSlotAddHurtMelee ""]
  }
ring5 = ring  -- by the time it's found, probably no space in eqp
  { irarity  = [(5, 0)]
  , iaspects = [AddLight $ d 2]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddLight ""]
  , idesc    = "A sturdy ring with a large, shining stone."
  }

-- * Exploding consumables, often intended to be thrown

potion = ItemKind
  { isymbol  = '!'
  , iname    = "potion"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain stdCol ++ zipFancy brightCol
  , icount   = 1
  , irarity  = [(1, 15), (10, 12)]
  , iverbHit = "splash"
  , iweight  = 200
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 50  -- oily, bad grip
               , Applicable, Fragile ]
  , idesc    = "A flask of bubbly, slightly oily liquid of a suspect color."  -- purely natural; no maths, no magic  -- TODO: move distortion to a special flask item or when some precious magical item is destroyed (jewelry?)?
  , ikit     = []
  }
potion1 = potion
  { ieffects = [ NoEffect "of rose water", Impress
               , OnSmash (ApplyPerfume), OnSmash (Explode "fragrance") ]
  }
potion2 = potion
  { ifreq    = [("useful", 1)]  -- extremely rare
  , irarity  = [(1, 1)]
  , ieffects = [ NoEffect "of musky concoction", Impress, DropBestWeapon
               , OnSmash (Explode "pheromone")]
  }
potion3 = potion
  { ieffects = [RefillHP 5, OnSmash (Explode "healing mist")]
  }
potion4 = potion  -- TODO: a bit boring
  { irarity  = [(1, 7)]
  , ieffects = [RefillHP (-5), OnSmash (Explode "wounding mist")]
  }
potion5 = potion
  { ieffects = [ Explode "explosion blast 10", Impress
               , PushActor (ThrowMod 200 75)
               , OnSmash (Explode "explosion blast 10") ]
  }
potion6 = potion
  { irarity  = [(10, 2)]
  , ieffects = [ NoEffect "of distortion", Impress
               , OnSmash (Explode "distortion")]
  }
potion7 = potion
  { ieffects = [ NoEffect "of bait cocktail", CreateOrgan (5 + d 5) "drunk"
               , OnSmash (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , OnSmash (Explode "waste") ]
  }
potion8 = potion
  { ieffects = [ OneOf [ Impress, DropBestWeapon, RefillHP 5, Burn 3
                       , CreateOrgan (7 + d 3) "drunk" ]
               , OnSmash (OneOf [ Explode "healing mist"
                                , Explode "wounding mist"
                                , Explode "fragrance"
                                , Explode "explosion blast 10"
                                , Explode "whiskey spray" ]) ]
  }
potion9 = potion
  { irarity  = [(3, 3), (10, 6)]
  , ieffects = [ OneOf [ Dominate, DropBestWeapon, RefillHP 20, Burn 9
                       , InsertMove 2, CreateOrgan (4 + d 3) "fast 20" ]
               , OnSmash (OneOf [ Explode "healing mist 2"
                                , Explode "healing mist 2"
                                , Explode "pheromone"
                                , Explode "distortion"
                                , Explode "explosion blast 20" ]) ]
  }
potion10 = potion  -- used only as initial equipmnt; count betray identity
  { ifreq    = [("useful", 100), ("potion of glue", 1)]
  , irarity  = [(1, 1)]
  , icount   = 1 + d 2
  , ieffects = [ NoEffect "of glue", Paralyze (5 + d 5)
               , OnSmash (Explode "glue")]
  , ifeature = [Identified]
  }
potion11 = potion
  { irarity  = [(10, 5)]
  , ieffects = [RefillHP 10, OnSmash (Explode "healing mist 2")]
  }
potion12 = potion
  { irarity  = [(1, 500), (10, 500)]
  , ieffects = [ NoEffect "whiskey", CreateOrgan (20 + d 5) "drunk"
               , Burn 3, RefillHP 4, OnSmash (Explode "whiskey spray") ]
  }

-- * Non-exploding consumables, not specifically designed for throwing

scroll = ItemKind
  { isymbol  = '?'
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
  { ieffects = [ OneOf [ Teleport $ d 3 |*| 3, RefillCalm 10, RefillCalm (-10)
                       , InsertMove 4, Paralyze 10, Identify CGround ] ]
  }
scroll5 = scroll
  { irarity  = [(3, 3), (10, 6)]
  , ieffects = [ OneOf [ Summon standardSummon $ d 2
                       , CallFriend 1, Ascend (-1), Ascend 1
                       , RefillCalm 30, RefillCalm (-30), CreateItem $ d 2
                       , PolyItem CGround ] ]
               -- TODO: ask player: Escape 1
  }
scroll6 = scroll
  { ieffects = [Teleport $ d 3 |*| 3]
  }
scroll7 = scroll
  { irarity  = [(10, 3)]
  , ieffects = [InsertMove $ d 2 + dl 2]
  }
scroll8 = scroll
  { irarity  = [(3, 8), (10, 4)]
  , ieffects = [Identify CGround]  -- TODO: ask player: AskPlayer cstore eff?
  }
scroll9 = scroll
  { irarity  = [(10, 10)]
  , ieffects = [PolyItem CGround]
  }

standardSummon :: Freqs
standardSummon = [("monster", 30), ("mobile animal", 70)]

-- * Armor

armorLeather = ItemKind
  { isymbol  = '['
  , iname    = "leather armor"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(3, 6), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 7000
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ (1 + dl 3) |*| 5
               , AddArmorRanged $ (1 + dl 3) |*| 5 ]
  , ieffects = []
  , ifeature = [ toVelocity 30  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "A stiff jacket formed from leather boiled in bee wax. Smells much better than the rest of your garment."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "mail armor"
  , iflavour = zipPlain [Cyan]
  , irarity  = [(6, 6), (10, 6)]
  , iweight  = 12000
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ (2 + dl 4) |*| 5
               , AddArmorRanged $ (2 + dl 4) |*| 5 ]
  , idesc    = "A long shirt woven from iron rings. Discourages foes from attacking your torso, making it harder for them to land a blow."
  }
gloveFencing = ItemKind
  { isymbol  = '['
  , iname    = "leather gauntlet"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 8), (10, 8)]
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
  , irarity  = [(6, 12)]
  , iflavour = zipPlain [BrCyan]
  , iweight  = 300
  , iaspects = [ AddArmorMelee $ (1 + dl 2) |*| 5
               , AddArmorRanged $ (1 + dl 2) |*| 5 ]
  , idesc    = "Long leather gauntlet covered in overlapping steel plates."
  }
gloveJousting = gloveFencing
  { iname    = "jousting gauntlet"
  , irarity  = [(6, 6)]
  , iflavour = zipFancy [BrRed]
  , iweight  = 500
  , iaspects = [ AddHurtMelee $ (dl 4 - 6) |*| 3
               , AddArmorMelee $ (2 + dl 2) |*| 5
               , AddArmorRanged $ (2 + dl 2) |*| 5 ]
  , idesc    = "Rigid, steel, jousting handgear. If only you had a lance. And a horse."
  }
-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
buckler = ItemKind
  { isymbol  = '['
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
  , ieffects = [Recharging (PushActor (ThrowMod 200 50))]
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
  , ieffects = [Recharging (PushActor (ThrowMod 400 50))]
  , ifeature = [ toVelocity 20  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Large and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  }

-- * Weapons

dagger = ItemKind
  { isymbol  = ')'
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
  { ifreq    = [("useful", 100)]
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
  { isymbol  = ')'
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
  { ifreq    = [("useful", 100)]
  , irarity  = [(4, 1), (10, 2)]
  , iaspects = iaspects hammer ++ [Timeout $ (d 2 + 3 - dl 2) |*| 2]
  , ieffects = ieffects hammer ++ [Recharging $ Paralyze 5]
  }
hammerSpark = hammer
  { iname    = "smithhammer"
  , ifreq    = [("useful", 100)]
  , irarity  = [(4, 1), (10, 2)]
  , iaspects = iaspects hammer ++ [Timeout $ (d 4 + 4 - dl 4) |*| 2]
  , ieffects = ieffects hammer ++ [Recharging $ Explode "spark"]
  }
sword = ItemKind
  { isymbol  = ')'
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
  { ifreq    = [("useful", 100)]
  , irarity  = [(3, 1), (10, 2)]
  , iaspects = iaspects sword ++ [Timeout $ (d 4 + 5 - dl 4) |*| 2]
  , ieffects = ieffects sword ++ [Recharging Impress]
  , idesc    = "A particularly well-balance blade, lending itself to impressive shows of fencing skill."
  }
halberd = ItemKind
  { isymbol  = ')'
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
  , ifreq    = [("useful", 100)]
  , irarity  = [(7, 1), (10, 2)]
  , iaspects = iaspects halberd ++ [Timeout $ (d 5 + 5 - dl 5) |*| 2]
  , ieffects = ieffects halberd ++ [Recharging (PushActor (ThrowMod 400 25))]
  , idesc    = "A versatile polearm, with great reach and leverage. Foes are held at a distance."
  }

-- * Wands

wand = ItemKind
  { isymbol  = '/'
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

-- * Assorted tools

jumpingPole = ItemKind
  { isymbol  = '('
  , iname    = "jumping pole"
  , ifreq    = [("useful", 100)]
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
whetstone = ItemKind
  { isymbol  = '~'
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
