-- | Item definitions.
module Content.ItemKind
  ( -- * Group name patterns
    pattern HARPOON, pattern EDIBLE_PLANT, pattern RING_OF_OPPORTUNITY_GRENADIER, pattern ARMOR_LOOSE, pattern CLOTHING_MISC, pattern CHIC_GEAR
  , groupNamesSingleton, groupNames
  , -- * Content
    content, items, otherItemContent
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindEmbed
import Content.ItemKindOrgan
import Content.ItemKindTemporary
import Content.RuleKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

groupNamesSingleton :: [GroupName ItemKind]
groupNamesSingleton =
       [S_FRAGRANCE, S_SINGLE_SPARK, S_SPARK]
    ++ [FLASK_UNKNOWN, POTION_UNKNOWN, EDIBLE_PLANT_UNKNOWN, SCROLL_UNKNOWN, NECKLACE_UNKNOWN, RING_UNKNOWN, HAMMER_UNKNOWN, GEM_UNKNOWN, CURRENCY_UNKNOWN]
    ++ actorsGNSingleton ++ organsGNSingleton
    ++ blastsGNSingleton ++ temporariesGNSingleton

pattern FLASK_UNKNOWN, POTION_UNKNOWN, EDIBLE_PLANT_UNKNOWN, SCROLL_UNKNOWN, NECKLACE_UNKNOWN, RING_UNKNOWN, HAMMER_UNKNOWN, GEM_UNKNOWN, CURRENCY_UNKNOWN :: GroupName ItemKind

groupNames :: [GroupName ItemKind]
groupNames =
       [TREASURE, ANY_SCROLL, ANY_GLASS, ANY_POTION, ANY_FLASK, EXPLOSIVE, ANY_JEWELRY, VALUABLE, UNREPORTED_INVENTORY]
    ++ [HARPOON, EDIBLE_PLANT, RING_OF_OPPORTUNITY_GRENADIER, ARMOR_LOOSE, CLOTHING_MISC, CHIC_GEAR]
    ++ embedsGN ++ actorsGN ++ organsGN ++ blastsGN

pattern HARPOON, EDIBLE_PLANT, RING_OF_OPPORTUNITY_GRENADIER, ARMOR_LOOSE, CLOTHING_MISC, CHIC_GEAR :: GroupName ItemKind

-- The @UNKNOWN@ patterns don't need to be exported. Used internally.
-- They also represent singleton groups.
pattern FLASK_UNKNOWN = GroupName "flask unknown"
pattern POTION_UNKNOWN = GroupName "potion unknown"
pattern EDIBLE_PLANT_UNKNOWN = GroupName "edible plant unknown"
pattern SCROLL_UNKNOWN = GroupName "scroll unknown"
pattern NECKLACE_UNKNOWN = GroupName "necklace unknown"
pattern RING_UNKNOWN = GroupName "ring unknown"
pattern HAMMER_UNKNOWN = GroupName "hammer unknown"
pattern GEM_UNKNOWN = GroupName "gem unknown"
pattern CURRENCY_UNKNOWN = GroupName "currency unknown"

pattern HARPOON = GroupName "harpoon"
pattern EDIBLE_PLANT = GroupName "edible plant"
pattern RING_OF_OPPORTUNITY_GRENADIER = GroupName "ring of grenadier"
pattern ARMOR_LOOSE = GroupName "loose armor"
pattern CLOTHING_MISC = GroupName "miscellaneous clothing"
pattern CHIC_GEAR = GroupName "chic gear"

-- * Content

content :: [ItemKind]
content = items ++ otherItemContent

otherItemContent :: [ItemKind]
otherItemContent = embeds ++ actors ++ organs ++ blasts ++ temporaries

items :: [ItemKind]
items =
  [sandstoneRock, dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, potion13, potion14, potion15, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, ediblePlantTemplate, ediblePlant1, ediblePlant2, ediblePlant3, ediblePlant4, ediblePlant5, ediblePlant6, ediblePlant7, light1, light2, light3, blanket, gorget, necklaceTemplate, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, necklace10, motionScanner, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, meleeEnhancement, gloveFencing, gloveGauntlet, gloveJousting, hatUshanka, capReinforced, helmArmored, smokingJacket, buckler, shield, shield2, shield3, hammerTemplate, hammer1, hammer2, hammer3, hammerParalyze, hammerSpark, knife, daggerDropBestWeapon, sword, swordImpress, swordNullify, halberd, halberd2, halberd3, halberdPushActor, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency, jumpingPole, seeingItem]

sandstoneRock,    dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, potion13, potion14, potion15, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, ediblePlantTemplate, ediblePlant1, ediblePlant2, ediblePlant3, ediblePlant4, ediblePlant5, ediblePlant6, ediblePlant7, light1, light2, light3, blanket, gorget, necklaceTemplate, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, necklace10, motionScanner, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, meleeEnhancement, gloveFencing, gloveGauntlet, gloveJousting, hatUshanka, capReinforced, helmArmored, smokingJacket, buckler, shield, shield2, shield3, hammerTemplate, hammer1, hammer2, hammer3, hammerParalyze, hammerSpark, knife, daggerDropBestWeapon, sword, swordImpress, swordNullify, halberd, halberd2, halberd3, halberdPushActor, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency, jumpingPole, seeingItem :: ItemKind

-- Keep the dice rolls and sides in aspects small so that not too many
-- distinct items are generated (for display in item lore and for narrative
-- impact ("oh, I found the more powerful of the two variants of the item!",
-- instead of "hmm, I found one of the countless variants, a decent one").
-- In particular, for unique items, unless they inherit aspects from
-- a standard item, permit only a couple possible variants.
-- This is especially important if an item kind has multiple random aspects.
-- Instead multiply dice results, e.g., (1 `d` 3) * 5 instead of 1 `d` 15.
--
-- Beware of non-periodic non-weapon durable items with beneficial effects
-- and low timeout -- AI will starve applying such an item incessantly.

-- * Item group symbols, partially from Nethack

symbolProjectile, _symbolLauncher, symbolLight, symbolTool, symbolSpecial, symbolGold, symbolNecklace, symbolRing, symbolPotion, symbolFlask, symbolScroll, symbolTorsoArmor, symbolMiscArmor, symbolClothes, symbolShield, symbolPolearm, symbolEdged, symbolHafted, symbolWand, _symbolStaff, symbolFood :: Char

symbolProjectile = rsymbolProjectile standardRules
_symbolLauncher  = toContentSymbol '}'
symbolLight      = rsymbolLight standardRules
symbolTool       = rsymbolTool standardRules
symbolSpecial    = rsymbolSpecial standardRules
symbolGold       = rsymbolGold standardRules
symbolNecklace   = rsymbolNecklace standardRules
symbolRing       = rsymbolRing standardRules
symbolPotion     = rsymbolPotion standardRules
symbolFlask      = rsymbolFlask standardRules
symbolScroll     = rsymbolScroll standardRules
symbolTorsoArmor = rsymbolTorsoArmor standardRules
symbolMiscArmor  = rsymbolMiscArmor standardRules
symbolClothes    = rsymbolClothes standardRules
symbolShield     = rsymbolShield standardRules
symbolPolearm    = rsymbolPolearm standardRules
symbolEdged      = rsymbolEdged standardRules
symbolHafted     = rsymbolHafted standardRules
symbolWand       = rsymbolWand standardRules
_symbolStaff     = toContentSymbol '_'  -- scanner
symbolFood       = rsymbolFood standardRules

-- ** Thrown weapons

sandstoneRock = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sandstone rock"
  , ifreq    = [ (S_SANDSTONE_ROCK, 1)
               , (UNREPORTED_INVENTORY, 1) ]  -- too weak to spam
  , iflavour = zipPlain [Green]
  , icount   = 1 + 1 `d` 2  -- > 1, to let AI ignore sole pieces
  , irarity  = [(1, 50), (10, 1)]
  , iverbHit = "hit"
  , iweight  = 300
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ -16 * 5
               , SetFlag Fragile
               , toVelocity 70 ] -- not dense, irregular
  , ieffects = []
  , idesc    = "A lump of brittle sandstone rock."
  , ikit     = []
  }
dart = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "dart"
  , ifreq    = [(COMMON_ITEM, 100), (ANY_ARROW, 50), (WEAK_ARROW, 50)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1 + 4 `dL` 5
  , irarity  = [(1, 15), (10, 5)]
  , iverbHit = "prick"
  , iweight  = 40
  , idamage  = 1 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ (-15 + 1 `d` 2 + 1 `dL` 3) * 5]
                 -- only good against leather
  , ieffects = []
  , idesc    = "A sharp delicate dart with fins."
  , ikit     = []
  }
spike = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "spike"
  , ifreq    = [(COMMON_ITEM, 100), (ANY_ARROW, 50), (WEAK_ARROW, 50)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1 + 4 `dL` 5
  , irarity  = [(1, 10), (10, 8)]
  , iverbHit = "nick"
  , iweight  = 150
  , idamage  = 2 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- heavy vs armor
               , SetFlag MinorEffects
               , toVelocity 70 ]  -- hitting with tip costs speed
  , ieffects = [ Explode S_SINGLE_SPARK  -- when hitting enemy
               , OnSmash (Explode S_SINGLE_SPARK) ]  -- at wall hit
      -- this results in a wordy item synopsis, but it's OK, the spark really
      -- is useful in some situations, not just a flavour
  , idesc    = "A cruel long nail with small head."  -- "Much inferior to arrows though, especially given the contravariance problems."  -- funny, but destroy the suspension of disbelief; this is supposed to be a Lovecraftian horror and any hilarity must ensue from the failures in making it so and not from actively trying to be funny; also, mundane objects are not supposed to be scary or transcendental; the scare is in horrors from the abstract dimension visiting our ordinary reality; without the contrast there's no horror and no wonder, so also the magical items must be contrasted with ordinary XIX century and antique items
  , ikit     = []
  }
spike2 = spike
  { ifreq    = [(COMMON_ITEM, 2), (ANY_ARROW, 1), (WEAK_ARROW, 1)]
  , icount   = 6 `dL` 5
  , iverbHit = "penetrate"
  , iweight  = 200
  , idamage = 4 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
               , SetFlag MinorEffects
               , Odds (10 * 1 `dL` 10) [] [toVelocity 70] ]
                   -- at deep levels sometimes even don't limit velocity
  , idesc    = "A jagged skewer of rusty metal."
  }
slingStone = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sling stone"
  , ifreq    = [(COMMON_ITEM, 5), (ANY_ARROW, 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1 + 3 `dL` 4
  , irarity  = [(1, 1), (10, 20)]
  , iverbHit = "batter"
  , iweight  = 200
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- heavy, to bludgeon through armor
               , SetFlag MinorEffects
               , toVelocity 150 ]
  , ieffects = [ Explode S_SINGLE_SPARK  -- when hitting enemy
               , OnSmash (Explode S_SINGLE_SPARK) ]  -- at wall hit
  , idesc    = "A round stone, carefully sized and smoothed to fit the pouch of a standard string and cloth sling."
  , ikit     = []
  }
slingBullet = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "sling bullet"
  , ifreq    = [(COMMON_ITEM, 5), (ANY_ARROW, 100)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1 + 6 `dL` 4
  , irarity  = [(1, 1), (10, 15)]
  , iverbHit = "slug"
  , iweight  = 28
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-17 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- not too good against armor
               , ToThrow $ ThrowMod 200 100 2 ]  -- piercing
  , ieffects = []
  , idesc    = "Small almond-shaped leaden projectile that weighs more than the sling used to tie the bag. It doesn't drop out of the sling's pouch when swung and doesn't snag when released. Known to pierce through flesh, at least at maximum speed."  -- we lie, it doesn't slow down in our model; but it stops piercing alright
  , ikit     = []
  }

-- ** Exotic thrown weapons

-- Identified, because shape (and name) says it all. Detailed aspects id by use.
-- This is an extremely large value for @Paralyze@. Normally for such values
-- we should instead use condition that disables (almost) all stats,
-- except @SkWait@, so that the player can switch leader and not be
-- helpless nor experience instadeath (unless his party is 1-person
-- or the actor is isolated, but that's usually player's fault).
paralizingProj = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "bolas set"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1 `dL` 4
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "entangle"
  , iweight  = 500
  , idamage  = 1 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ -14 * 5]
  , ieffects = [Paralyze 15, Discharge 1 100]
  , idesc    = "Wood balls tied with hemp rope. The foe is unlikely to use its main weapon while fighting for balance."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [(COMMON_ITEM, 100), (HARPOON, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dL` 5
  , irarity  = [(10, 10)]
  , iverbHit = "hook"
  , iweight  = 750
  , idamage  = 5 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5]
  , ieffects = [ PullActor (ThrowMod 200 50 1)  -- 1 step, fast
               , Yell ]  -- yell, because brutal
  , idesc    = "The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
harpoon2 = harpoon
  { iname    = "whaling harpoon"
  , ifreq    = [(COMMON_ITEM, 5), (HARPOON, 2)]
  , icount   = 2 `dL` 5
  , iweight  = 1000
  , idamage  = 10 `d` 1
  , idesc    = "With a brittle, barbed head and thick cord, this ancient weapon is designed for formidable prey."
  }
net = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "net"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1 `dL` 3
  , irarity  = [(5, 5), (10, 7)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , idamage  = 2 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ -14 * 5]
  , ieffects = [ toOrganBad S_SLOWED (3 + 1 `d` 3)
               , DropItem maxBound 1 CEqp ARMOR_LOOSE
                   -- only one of each kind is dropped, because no rubbish
                   -- in this group and so no risk of exploit
               , SendFlying (ThrowMod 100 50 1) ]  -- 1 step; painful
  , idesc    = "A wide net with weights along the edges. Entangles armor and restricts movement."
  , ikit     = []
  }

-- ** Explosives, with the only effect being @Explode@

fragmentationBomb = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "clay pot"
      -- clay pot filled with black powder; fragmentation comes from the clay
      -- shards, so it's not obvious if it's a weapon or just storage method;
      -- deflagration, not detonation, so large mass and hard container
      -- required not to burn harmlessly; improvised short fuze
  , ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 200)]
  , iflavour = zipPlain [Red]
  , icount   = 1 `dL` 5  -- many, because not very intricate
  , irarity  = [(5, 8), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 3000  -- low velocity due to weight
  , idamage  = 0  -- heavy and hard, but let's not confuse with blast damage
  , iaspects = [ ELabel "of black powder"
               , SetFlag Lobable, SetFlag Fragile ]
  , ieffects = [ Explode S_FOCUSED_FRAGMENTATION
               , OnSmash (Explode S_VIOLENT_FRAGMENTATION) ]
  , idesc    = "The practical application of science."
  , ikit     = []
  }
concussionBomb = fragmentationBomb
  { iname    = "satchel"
      -- slightly stabilized nitroglycerine in a soft satchel, hence
      -- no fragmentation, but huge shock wave despite small size and lack of
      -- strong container to build up pressure (hence only mild hearing loss);
      -- indoors helps the shock wave; unstable enough that no fuze required
  , iflavour = zipPlain [Magenta]
  , iverbHit = "flap"
  , iweight  = 400
  , iaspects = [ ELabel "of mining charges"
               , SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]  -- flappy and so slow
  , ieffects = [ Explode S_FOCUSED_CONCUSSION
               , OnSmash (Explode S_VIOLENT_CONCUSSION) ]
  , idesc    = "Avoid sudden movements."
  }
-- Not flashbang, because powerful bang without fragmentation is harder
-- to manufacture (requires an oxidizer and steel canister with holes).
-- The bang would also paralyze and/or lower the movement skill
-- (out of balance due to ear trauma).
flashBomb = fragmentationBomb
  { iname    = "magnesium ribbon"  -- filled with magnesium flash powder
  , iflavour = zipPlain [BrYellow]  -- avoid @BrWhite@; looks wrong in dark
  , iverbHit = "flash"
  , iweight  = 400
  , iaspects = [ SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]  -- bad shape for throwing
  , ieffects = [Explode S_FOCUSED_FLASH, OnSmash (Explode S_VIOLENT_FLASH)]
  , idesc    = "For dramatic entrances and urgent exits."
  }
firecrackerBomb = fragmentationBomb
  { iname = "roll"  -- not fireworks, as they require outdoors
  , iflavour = zipPlain [BrMagenta]
  , irarity  = [(1, 5), (5, 6)]  -- a toy, if deadly
  , iverbHit = "crack"  -- a pun, matches the verb from "ItemKindBlast"
  , iweight  = 1000
  , iaspects = [SetFlag Lobable, SetFlag Fragile]
  , ieffects = [Explode S_FIRECRACKER, OnSmash (Explode S_FIRECRACKER)]
  , idesc    = "String and paper, concealing a deadly surprise."
  }

-- ** Exploding consumables.

-- Not identified, because they are perfect for the id-by-use fun,
-- due to effects. They are fragile and upon hitting the ground explode
-- for effects roughly corresponding to their normal effects.
-- Whether to hit with them or explode them close to the target
-- is intended to be an interesting tactical decision.

-- Flasks are intended to be thrown. They are often not natural: maths, magic,
-- distillery. In fact, they cover all temporary conditions, except those
-- for stats resistance and regeneration. They never heal, directly
-- nor indirectly (regen), so may be thrown without the risk of wasting
-- precious HP.
--
-- There is no flask nor condition that only does Calm or max Calm depletion,
-- because Calm reduced often via combat, etc.

flaskTemplate = ItemKind
  { isymbol  = symbolFlask
  , iname    = "flask"
  , ifreq    = [(FLASK_UNKNOWN, 1)]
  , iflavour = zipGlassPlain darkCol ++ zipGlassFancy darkCol
               ++ zipLiquid darkCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 7), (10, 3)]
  , iverbHit = "splash"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ PresentAs FLASK_UNKNOWN, SetFlag Lobable, SetFlag Fragile
               , toVelocity 60 ]  -- oily, rather bad grip
  , ieffects = []
  , idesc    = "A flask of oily liquid of a suspect color. Something seems to be moving inside. Double dose causes twice longer effect. Triple dose is not advisable, since the active substance is never without unhealty side-efects and often dissolved in large volumes of alcohol."
  , ikit     = []
  }
flask1 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , icount   = 1 `dL` 5
  , irarity  = [(10, 10)]
  , iaspects = ELabel "of strength renewal brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_STRENGTHENED (20 + 1 `d` 5)
               , OnSmash (Explode S_DENSE_SHOWER) ]
  }
flask2 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , iaspects = ELabel "of weakness brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganBad S_WEAKENED (20 + 1 `d` 5)
               , OnSmash (Explode S_SPARSE_SHOWER) ]
  }
flask3 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , iaspects = ELabel "of melee protective balm"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_PROTECTED_FROM_MELEE (20 + 1 `d` 5)
               , OnSmash (Explode S_MELEE_PROTECTIVE_BALM) ]
  }
flask4 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , iaspects = ELabel "of ranged protective balm"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_PROTECTED_FROM_RANGED (20 + 1 `d` 5)
               , OnSmash (Explode S_RANGE_PROTECTIVE_BALM) ]
  }
flask5 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , iaspects = ELabel "of PhD defense questions"
               : iaspects flaskTemplate
  , ieffects = [ toOrganBad S_DEFENSELESS (20 + 1 `d` 5)
               , Impress
               , Detect DetectExit 20
               , OnSmash (Explode S_DEFENSELESSNESS_RUNOUT) ]
  }
flask6 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , irarity  = [(1, 1)]  -- not every playthrough needs one
  , iaspects = ELabel "of resolution"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_RESOLUTE (500 + 1 `d` 200)  -- long, for scouting
               , RefillCalm 60  -- not to make it a drawback, via @calmEnough@
               , OnSmash (Explode S_RESOLUTION_DUST) ]
  }
flask7 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , icount   = 1 `d` 2  -- too powerful en masse
  , iaspects = ELabel "of haste brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_HASTED (20 + 1 `d` 5)
               , OnSmash (Explode S_HASTE_SPRAY) ]
  }
flask8 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , iaspects = ELabel "of eye drops"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_FAR_SIGHTED (40 + 1 `d` 10)
               , OnSmash (Explode S_EYE_DROP) ]
  }
flask9 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , irarity  = [(10, 2)]  -- not very useful right now
  , iaspects = ELabel "of smelly concoction"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_KEEN_SMELLING (40 + 1 `d` 10)
               , Detect DetectActor 10  -- make it at least slightly useful
               , OnSmash (Explode S_SMELLY_DROPLET) ]
  }
flask10 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , irarity  = [(10, 2)]  -- not very useful right now
  , iaspects = ELabel "of cat tears"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_SHINY_EYED (40 + 1 `d` 10)
               , OnSmash (Explode S_EYE_SHINE) ]
  }
flask11 = flaskTemplate
  { iname    = "bottle"
  , ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , icount   = 1 `d` 3  -- the only one sometimes giving away its identity
  , iaspects = ELabel "of whiskey"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_DRUNK (20 + 1 `d` 5)
               , Burn 10, RefillHP 10, Yell
               , OnSmash (Explode S_WHISKEY_SPRAY) ]
  }
flask12 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , iaspects = ELabel "of bait cocktail"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_DRUNK (20 + 1 `d` 5)
               , Burn 1, RefillHP 3  -- risky exploit possible, good
               , Summon MOBILE_ANIMAL 1
               , OnSmash (Summon MOBILE_ANIMAL 1)
               , OnSmash Impress  -- mildly useful when thrown
               , OnSmash (Explode S_WASTE) ]
  }
flask13 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , iaspects = ELabel "of poison"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer S_POISONED, toOrganNoTimer S_POISONED  -- x2
               , OnSmash (Explode S_POISON_CLOUD) ]
  }
flask14 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , iaspects = ELabel "of calamity"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer S_POISONED
               , toOrganBad S_WEAKENED (20 + 1 `d` 5)
               , toOrganBad S_DEFENSELESS (20 + 1 `d` 5)
               , OnSmash (Explode S_GLASS_HAIL) ]  -- enough glass to cause that
  }

-- Vials are often not intended to be thrown. They usually natural,
-- including natural stat boosts. They also include the only healing
-- consumables in the game, apart of elixirs and, to a limited extent, fruits.
-- They appear deeper than most flasks. Various configurations of effects.
-- A different class of effects is on scrolls and mechanical items.
-- Some are shared.

potionTemplate = ItemKind
  { isymbol  = symbolPotion
  , iname    = "potion"
  , ifreq    = [(POTION_UNKNOWN, 1)]
  , iflavour = zipLiquid brightCol ++ zipPlain brightCol ++ zipFancy brightCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "splash"
  , iweight  = 200
  , idamage  = 0
  , iaspects = [ PresentAs POTION_UNKNOWN, SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- oily, small momentum due to small size
  , ieffects = []
  , idesc    = "A vial of bright, frothing concoction. The best medicine that nature has to offer for wounds, ailments and mood swings."
  , ikit     = []
  }
potion1 = potionTemplate
  { iname    = "vial"
  , ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , icount   = 3 `dL` 1  -- very useful, despite appearances
  , iaspects = ELabel "of rose water"
               : iaspects potionTemplate
  , ieffects = [ Impress, toOrganGood S_ROSE_SMELLING (80 + 1 `d` 20)
               , OnSmash ApplyPerfume, OnSmash (Explode S_FRAGRANCE) ]
  }
potion2 = potionTemplate
  { iname    = "the Potion"
  , ifreq    = [(TREASURE, 100), (ANY_GLASS, 100)]
  , icount   = 1
  , irarity  = [(5, 8), (10, 8)]
  , iaspects = [ SetFlag Unique, ELabel "of Attraction"
               , SetFlag Precious, SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- identified
  , ieffects = [ Dominate
               , toOrganGood S_HASTED (20 + 1 `d` 5)
               , OnSmash (Explode S_PHEROMONE)
               , OnSmash (Explode S_HASTE_SPRAY) ]
  , idesc    = "The liquid fizzes with energy."
  }
potion3 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , ieffects = [ RefillHP 5, DropItem 1 maxBound COrgan S_POISONED
               , OnSmash (Explode S_HEALING_MIST) ]
  }
potion4 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(1, 6), (10, 10)]
  , ieffects = [ RefillHP 10
               , DropItem maxBound maxBound COrgan CONDITION
               , OnSmash (Explode S_HEALING_MIST_2) ]
  }
potion5 = potionTemplate
  { iname    = "ampoule"  -- probably filled with nitroglycerine, but let's
                          -- not mix fantasy with too much technical jargon
  , ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , icount   = 3 `dL` 1
  , ieffects = [ DropItem 1 maxBound COrgan CONDITION
               , OnSmash (Explode S_VIOLENT_CONCUSSION) ]
      -- not fragmentation nor glass hail, because not enough glass
  }
potion6 = potionTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , icount   = 3 `dL` 1  -- always as many as possible on this level
                         -- without giving away potion identity
  , irarity  = [(1, 12)]
  , ieffects = [ OneOf [ RefillHP 10, RefillHP 5, Burn 5
                       , DropItem 1 maxBound COrgan S_POISONED
                       , toOrganGood S_STRENGTHENED (20 + 1 `d` 5) ]
               , OnSmash (OneOf [ Explode S_DENSE_SHOWER
                                , Explode S_SPARSE_SHOWER
                                , Explode S_MELEE_PROTECTIVE_BALM
                                , Explode S_RANGE_PROTECTIVE_BALM
                                , Explode S_DEFENSELESSNESS_RUNOUT ]) ]
  }
potion7 = potionTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(10, 10)]
  , ieffects = [ Impress
               , OneOf [ RefillHP 20, RefillHP 10, Burn 10
                       , DropItem 1 maxBound COrgan S_POISONED
                       , toOrganGood S_HASTED (20 + 1 `d` 5)
                       , toOrganBad S_IMPATIENT (2 + 1 `d` 2) ]
               , OnSmash (OneOf [ Explode S_HEALING_MIST_2
                                , Explode S_WOUNDING_MIST
                                , Explode S_DISTRESSING_ODOR
                                , Explode $ blastNoStatOf S_IMPATIENT
                                , Explode S_HASTE_SPRAY
                                , Explode S_SLOWNESS_MIST
                                , Explode S_FRAGRANCE
                                , Explode S_VIOLENT_FLASH ]) ]
  }
potion8 = potionTemplate
  { iname    = "the Potion"
  , ifreq    = [(TREASURE, 100), (ANY_GLASS, 100)]
  , icount   = 1
  , irarity  = [(10, 5)]
  , iaspects = [ SetFlag Unique, ELabel "of Love"
               , SetFlag Precious, SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- identified
  , ieffects = [ RefillHP 60, RefillCalm (-60)
               , toOrganGood S_ROSE_SMELLING (80 + 1 `d` 20)
               , OnSmash (Explode S_HEALING_MIST_2)
               , OnSmash (Explode S_DISTRESSING_ODOR) ]
  , idesc    = "Perplexing swirls of intense, compelling colour."
  }
potion9 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 5)]
  , iaspects = ELabel "of grenadier focus"
               : iaspects potionTemplate
  , ieffects = [ toOrganGood S_MORE_PROJECTING (40 + 1 `d` 10)
               , toOrganBad S_PACIFIED (5 + 1 `d` 3)
                   -- the malus has to be weak, or would be too good
                   -- when thrown at foes
               , OnSmash (Explode $ blastBonusStatOf S_MORE_PROJECTING)
               , OnSmash (Explode $ blastNoStatOf S_PACIFIED) ]
  , idesc    = "Thick, sluggish fluid with violently-bursting bubbles."
  }
potion10 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of frenzy"
               : iaspects potionTemplate
  , ieffects = [ Yell
               , toOrganGood S_STRENGTHENED (20 + 1 `d` 5)
               , toOrganBad S_RETAINING (5 + 1 `d` 3)
               , toOrganBad S_FRENZIED (40 + 1 `d` 10)
               , OnSmash (Explode S_DENSE_SHOWER)
               , OnSmash (Explode $ blastNoStatOf S_RETAINING)    -- more
               , OnSmash (Explode $ blastNoStatOf S_RETAINING) ]  -- explosion
  }
potion11 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of panic"
               : iaspects potionTemplate
  , ieffects = [ RefillCalm (-30)
               , toOrganGood S_HASTED (20 + 1 `d` 5)
               , toOrganBad S_WEAKENED (20 + 1 `d` 5)
               , toOrganBad S_WITHHOLDING (10 + 1 `d` 5)
               , OnSmash (Explode S_HASTE_SPRAY)
               , OnSmash (Explode S_SPARSE_SHOWER)
               , OnSmash (Explode $ blastNoStatOf S_WITHHOLDING) ]
  }
potion12 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of quicksilver"
               : iaspects potionTemplate
  , ieffects = [ toOrganGood S_HASTED (20 + 1 `d` 5)
               , toOrganBad S_BLIND (10 + 1 `d` 5)
               , toOrganBad S_IMMOBILE (5 + 1 `d` 5)
               , OnSmash (Explode S_HASTE_SPRAY)
               , OnSmash (Explode S_IRON_FILING)
               , OnSmash (Explode $ blastNoStatOf S_IMMOBILE) ]
  }
potion13 = potionTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , irarity  = [(10, 4)]
  , iaspects = ELabel "of slow resistance"
               : iaspects potionTemplate
  , ieffects = [ toOrganNoTimer S_SLOW_RESISTANT
               , OnSmash (Explode S_ANTI_SLOW_MIST) ]
  }
potion14 = potionTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , irarity  = [(10, 4)]
  , iaspects = ELabel "of poison resistance"
               : iaspects potionTemplate
  , ieffects = [ toOrganNoTimer S_POISON_RESISTANT
               , OnSmash (Explode S_ANTIDOTE_MIST) ]
  }
-- The player has full control over throwing the flask at his party,
-- so he can milk the explosion, so it has to be much weaker, so a weak
-- healing effect is enough. OTOH, throwing a harmful flask at many enemies
-- at once is not easy to arrange, so these explosions can stay powerful.
potion15 = potionTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (ANY_FLASK, 100), (EXPLOSIVE, 100)
               , (ANY_GLASS, 100) ]
  , irarity  = [(1, 2), (10, 12)]
  , iaspects = ELabel "of regeneration brew"
               : iaspects potionTemplate
  , ieffects = [ toOrganGood S_ROSE_SMELLING (80 + 1 `d` 20)
               , toOrganNoTimer S_REGENERATING
               , toOrganNoTimer S_REGENERATING  -- x2
               , OnSmash (Explode S_YOUTH_SPRINKLE) ]
  }

-- ** Non-exploding consumables, not specifically designed for throwing

-- Readable or otherwise communicating consumables require high apply skill
-- to be consumed.

scrollTemplate = ItemKind
  { isymbol  = symbolScroll
  , iname    = "scroll"
  , ifreq    = [(SCROLL_UNKNOWN, 1)]
  , iflavour = zipFancy stdCol ++ zipPlain stdCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 14), (10, 7)]
  , iverbHit = "thump"
  , iweight  = 50
  , idamage  = 0
  , iaspects = [ PresentAs SCROLL_UNKNOWN
               , toVelocity 30 ]  -- bad shape, even rolled up
  , ieffects = []
  , idesc    = "Scraps of haphazardly scribbled mysteries from beyond. Is this equation an alchemical recipe? Is this diagram an extradimensional map? Is this formula a secret call sign?"
  , ikit     = []
  }
scroll1 = scrollTemplate
  { iname    = "the Scroll"
  , ifreq    = [(TREASURE, 100), (ANY_SCROLL, 100)]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]  -- mixed blessing, so found early for a unique
  , iaspects = [SetFlag Unique, ELabel "of Reckless Beacon"]
               ++ iaspects scrollTemplate
  , ieffects = [Summon HERO 1, Summon MOBILE_ANIMAL (2 + 1 `d` 2)]
  , idesc    = "The bright flame and sweet-smelling smoke of this heavily infused scroll should attract natural creatures inhabiting the area, including human survivors, if any."
  }
scroll2 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(1, 6), (10, 2)]
  , ieffects = [Ascend False]
  }
scroll3 = scrollTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 14)]
  , ieffects = [OneOf [ Teleport 5, Paralyze 10, InsertMove 30
                      , Detect DetectEmbed 12, Detect DetectHidden 20 ]]
  }
scroll4 = scrollTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(10, 14)]
  , ieffects = [ Impress
               , OneOf [ Teleport 20, Ascend False, Ascend True
                       , Summon HERO 1, Summon MOBILE_ANIMAL $ 1 `d` 2
                       , Detect DetectLoot 20  -- the most useful of detections
                       , CreateItem Nothing CGround COMMON_ITEM timerNone ] ]
  }
scroll5 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(1, 6)]  -- powerful, but low counts at the depths it appears on
  , ieffects = [InsertMove $ 20 + 1 `dL` 20]
  }
scroll6 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 11)]
  , ieffects = [PullActor (ThrowMod 800 75 1)]  -- 6 steps, 1.5 turns
  }
scroll7 = scrollTemplate
  { iname    = "the Scroll"
  , ifreq    = [(TREASURE, 100), (ANY_SCROLL, 100)]
  , icount   = 1
  , irarity  = [(10, 12)]
  , iaspects = [SetFlag Unique, ELabel "of Rescue Proclamation"]
               ++ iaspects scrollTemplate
  , ieffects = [Summon HERO 1]
  , idesc    = "A survivor of past exploration missions is found that enjoys, apparently, complete physiological integrity. We can pronounce him a comrade in arms and let him join our party."
  }
scroll8 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 4)]  -- powerful, even if not ideal; scares newbies
  , ieffects = [Detect DetectAll 20]
  }
scroll9 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , iaspects = ELabel "of cue interpretation"
               : iaspects scrollTemplate
  , ieffects = [Detect DetectActor 20]
  }
scroll10 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 20)]  -- uncommon deep down, where all is known
  , iaspects = ELabel "of scientific explanation"
               : iaspects scrollTemplate
  , ieffects = [Identify `AndEffect` RefillCalm 10]
  , idesc    = "The most pressing existential concerns are met with a deeply satisfying scientific answer."
  }
scroll11 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 20)]  -- at gameover a crucial item may be missing
  , iaspects = ELabel "of transmutation"
               : iaspects scrollTemplate
  , ieffects = [PolyItem `AndEffect` Explode S_FIRECRACKER]
  }
scroll12 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 15)]
  , iaspects = ELabel "of transfiguration"
               : iaspects scrollTemplate
  , ieffects = [RerollItem]
  }
scroll13 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 15)]
  , iaspects = ELabel "of similarity"
               : iaspects scrollTemplate
  , ieffects = [DupItem]
  }

-- Foods require only minimal apply skill to consume. Many animals can eat them.

ediblePlantTemplate = ItemKind
  { isymbol  = symbolFood
  , iname    = "edible plant"
  , ifreq    = [(EDIBLE_PLANT_UNKNOWN, 1)]
  , iflavour = zipFancy stdCol
  , icount   = 1 `dL` 5
  , irarity  = [(1, 12), (10, 6)]  -- let's feed the animals
  , iverbHit = "thump"
  , iweight  = 50
  , idamage  = 0
  , iaspects = [ PresentAs EDIBLE_PLANT_UNKNOWN
               , toVelocity 30 ]  -- low density, often falling apart
  , ieffects = []
  , idesc    = "Withered but fragrant bits of a colorful plant. Taste tolerably and break down easily, but only eating may reveal the full effects."
  , ikit     = []
  }
ediblePlant1 = ediblePlantTemplate
  { iname    = "overripe berry"
  , ifreq    = [(COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [RefillHP 1, toOrganBad S_IMMOBILE (5 + 1 `d` 5)]
  }
ediblePlant2 = ediblePlantTemplate
  { iname    = "frayed fungus"
  , ifreq    = [(COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [toOrganNoTimer S_POISONED]
  }
ediblePlant3 = ediblePlantTemplate
  { iname    = "thick leaf"
  , ifreq    = [(COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [DropItem 1 maxBound COrgan S_POISONED]
  }
ediblePlant4 = ediblePlantTemplate
  { iname    = "shrunk fruit"
  , ifreq    = [(COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [toOrganBad S_BLIND (10 + 1 `d` 10)]
  }
ediblePlant5 = ediblePlantTemplate
  { iname    = "fragrant herb"
  , ifreq    = [(COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , icount   = 1 `dL` 9
  , irarity  = [(1, 12), (10, 5)]
  , iaspects = ELabel "of lethargy"
               : iaspects ediblePlantTemplate
  , ieffects = [ toOrganBad S_SLOWED (20 + 1 `d` 5)
               , toOrganNoTimer S_REGENERATING
               , toOrganNoTimer S_REGENERATING  -- x2
               , RefillCalm 5 ]
  }
ediblePlant6 = ediblePlantTemplate
  { iname    = "dull flower"
  , ifreq    = [(COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [PutToSleep]
  }
ediblePlant7 = ediblePlantTemplate
  { iname    = "spicy bark"
  , ifreq    = [(COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [InsertMove 20, toOrganBad S_FRENZIED (40 + 1 `d` 10)]
  }

-- ** Lights

light1 = ItemKind
  { isymbol  = symbolLight
  , iname    = "wooden torch"
  , ifreq    = [ (COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 100)
               , (S_WOODEN_TORCH, 1) ]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dL` 4
  , irarity  = [(1, 40), (4, 1)]
  , iverbHit = "scorch"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine 3, AddSkill SkSight (-2)
                   -- not only flashes, but also sparks,
                   -- so unused by AI due to the mixed blessing
               , SetFlag Lobable, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
                   -- not Fragile; reusable flare
  , ieffects = [Burn 1]
  , idesc    = "A heavy smoking wooden torch, improvised using a cloth soaked in tar, burning in an unsteady glow."
  , ikit     = []
  }
light2 = ItemKind
  { isymbol  = symbolLight
  , iname    = "oil lamp"
  , ifreq    = [(COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1 `dL` 2
  , irarity  = [(4, 10)]
  , iverbHit = "burn"
  , iweight  = 1500
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkShine 3, AddSkill SkSight (-1)
               , SetFlag Lobable, SetFlag Fragile, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
  , ieffects = [ Burn 1
               , toOrganBad S_PACIFIED (2 + 1 `d` 2)
               , OnSmash (Explode S_BURNING_OIL_2) ]
  , idesc    = "A clay lamp filled with plant oil feeding a tiny wick."
  , ikit     = []
  }
light3 = ItemKind
  { isymbol  = symbolLight
  , iname    = "brass lantern"
  , ifreq    = [(COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(10, 6)]
  , iverbHit = "burn"
  , iweight  = 3000
  , idamage  = 2 `d` 1
  , iaspects = [ AddSkill SkShine 4, AddSkill SkSight (-1)
               , SetFlag Lobable, SetFlag Fragile, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
  , ieffects = [ Burn 1
               , toOrganBad S_PACIFIED (4 + 1 `d` 2)
               , OnSmash (Explode S_BURNING_OIL_4) ]
  , idesc    = "Very bright and very heavy brass lantern."
  , ikit     = []
  }
blanket = ItemKind
  { isymbol  = symbolLight
  , iname    = "wool blanket"
  , ifreq    = [ (COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 100)
               , (FIREPROOF_CLOTH, 1) ]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]  -- not every playthrough needs one
  , iverbHit = "swoosh"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine (-10)
               , AddSkill SkArmorMelee 2, AddSkill SkMaxCalm 5
               , SetFlag Lobable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee ]
                  -- not Fragile; reusable douse implement;
                   -- douses torch, lamp and lantern in one action,
                   -- both in equipment and when thrown at the floor
  , ieffects = []
  , idesc    = "Warm, comforting, and concealing, woven from soft wool."
  , ikit     = []
  }

-- ** Periodic jewelry

-- Morally these are the aspects, but we also need to add a fake @Timeout@,
-- to let clients know that the not identified item is periodic jewelry.
iaspects_necklaceTemplate :: [Aspect]
iaspects_necklaceTemplate =
  [ PresentAs NECKLACE_UNKNOWN
  , SetFlag Periodic, SetFlag Precious, SetFlag Equipable
  , toVelocity 50 ]  -- not dense enough
gorget = necklaceTemplate
  { iname    = "Old Gorget"
  , ifreq    = [(COMMON_ITEM, 25), (TREASURE, 25)]
  , iflavour = zipFancy [BrCyan]  -- looks exactly the same as one of necklaces,
                                  -- but it's OK, it's an artifact
  , iaspects = [ SetFlag Unique
               , Timeout $ 5 - 1 `dL` 4
                   -- the dL dice need to be in negative positions
                   -- for negative stats, such as @Timeout@, so that
                   -- the @RerollItem@ effect makes the item better, not worse
               , AddSkill SkArmorMelee 3, AddSkill SkArmorRanged 2
               , AddSkill SkHearing 3
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [RefillCalm 1]
  , idesc    = "Highly ornamental, cold, large steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old worn engraving reassures the wearer."
  }
-- Not identified, because id by use, e.g., via periodic activations. Fun.
necklaceTemplate = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [(NECKLACE_UNKNOWN, 1)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(4, 3), (10, 6)]
  , iverbHit = "whip"
  , iweight  = 30
  , idamage  = 0
  , iaspects = Timeout 1000000
                 -- fake, needed to display "charging"; the timeout itself
                 -- won't be displayed thanks to periodic; as a side-effect,
                 -- it can't be activated until identified, which is better
                 -- than letting the player try to activate before the real
                 -- cooldown is over and waste turn
               : iaspects_necklaceTemplate
  , ieffects = []
  , idesc    = "Menacing Greek symbols shimmer with increasing speed along a chain of fine encrusted links. After a tense build-up, a prismatic arc shoots towards the ground and the iridescence subdues, becomes ordered and resembles a harmless ornament again, for a time."
  , ikit     = []
  }
necklace1 = necklaceTemplate
  { iname    = "the Necklace"
  , ifreq    = [(TREASURE, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 3)]
  , iaspects = [ SetFlag Unique, ELabel "of Aromata"
               , Timeout $ (4 - 1 `dL` 3) * 10
                   -- priceless, so worth the long wait and Calm drain
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ RefillCalm (-5)
               , When (TriggeredBy ActivationPeriodic) $ RefillHP 1 ]
  , idesc    = "A cord of freshly dried herbs and healing berries."
  }
necklace2 = necklaceTemplate
  { iname    = "the Necklace"
  , ifreq    = [(TREASURE, 100), (ANY_JEWELRY, 100)]
      -- too nasty to call it just a COMMON_ITEM
  , irarity  = [(10, 3)]
  , iaspects = [ SetFlag Unique, ELabel "of Live Bait"
               , Timeout 30
               , AddSkill SkOdor 2
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ DropItem 1 1 COrgan CONDITION  -- mildly useful when applied
               , When (TriggeredBy ActivationPeriodic) $ SeqEffect
                   [ Impress
                   , Summon MOBILE_ANIMAL $ 1 `dL` 2
                   , Explode S_WASTE ] ]
  , idesc    = "A cord hung with lumps of decaying meat. It's better not to think about the source."
  }
necklace3 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ ELabel "of fearful listening"
               , Timeout 40
                   -- has to be larger than Calm drain or item not removable;
                   -- equal is not enough if enemies drained Calm already
               , AddSkill SkHearing 6 ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Detect DetectActor 20  -- can be applied; destroys the item
               , When (TriggeredBy ActivationPeriodic) $ RefillCalm (-30) ]
  }
necklace4 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ ELabel "of escape"
               , Timeout $ (7 - 1 `dL` 5) * 10 ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Teleport $ 14 + 3 `d` 3  -- can be applied; destroys the item
               , Detect DetectExit 20
               , Yell ]  -- drawback when used for quick exploring
  , idesc    = "A supple chain that slips through your fingers."
  }
necklace5 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ ELabel "of greed"
               , Timeout ((2 + 1 `d` 3) * 10) ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Detect DetectLoot 20
               , toOrganBad S_PARSIMONIOUS (5 + 1 `d` 3)  -- hard to flee
               , When (TriggeredBy ActivationPeriodic) $ Teleport 40 ]  -- risky
  }
necklace6 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = Timeout ((3 + 1 `d` 3 - 1 `dL` 3) * 2)
               : iaspects_necklaceTemplate
  , ieffects = [Teleport $ 3 `d` 2]
  }
necklace7 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = Timeout (1 + (1 `d` 3) * 2)
               : iaspects_necklaceTemplate
  , ieffects = [PushActor (ThrowMod 100 50 1)]  -- 1 step, slow
                  -- the @50@ is only for the case of very light actor, etc.
  }
necklace8 = necklaceTemplate
  { iname    = "the Necklace"
  , ifreq    = [(TREASURE, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 1)]  -- different gameplay for the actor that wears it
  , iaspects = [ SetFlag Unique, ELabel "of Overdrive"
               , Timeout 4
               , AddSkill SkMaxHP 25  -- give incentive to cope with impatience
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ InsertMove $ 9 + 1 `d` 11  -- unpredictable
               , toOrganBad S_IMPATIENT 4]
                 -- The same duration as timeout, to avoid spurious messages
                 -- as well as unlimited accumulation of the duration.
  , idesc    = "A string of beads in various colours, with no discernable pattern."
  }
necklace9 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(4, 3)]  -- entirely optional
  , iaspects = Timeout ((1 + 1 `d` 3) * 5)
               : iaspects_necklaceTemplate
  , ieffects = [Explode S_SPARK]
  }
necklace10 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = Timeout ((1 + 1 `d` 3) * 5)
               : iaspects_necklaceTemplate
  , ieffects = [Explode S_FRAGRANCE]
  }
motionScanner = necklaceTemplate
  { iname    = "draft detector"
  , ifreq    = [(COMMON_ITEM, 100), (ADD_NOCTO_1, 20)]
  , irarity  = [(5, 2)]
  , iverbHit = "jingle"
  , iweight  = 300  -- almost gives it away
  , iaspects = [ Timeout $ 4 + 1 `dL` 6
               , AddSkill SkNocto 1
               , AddSkill SkArmorMelee (-20 + (1 `dL` 3) * 5)
               , EqpSlot EqpSlotMiscBonus ]
               ++ iaspects_necklaceTemplate
  , ieffects = [Explode S_PING_PLASH]
  , idesc    = "A silk flag with a bell for detecting sudden draft changes. May indicate a nearby corridor crossing or a fast enemy approaching in the dark. The bell is very noisy and casts light reflection flashes."
  }

-- ** Non-periodic jewelry

imageItensifier = ItemKind
  { isymbol  = symbolRing
  , iname    = "light cone"
  , ifreq    = [(TREASURE, 100), (ADD_NOCTO_1, 80)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "bang"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ AddSkill SkNocto 1, AddSkill SkSight (-1)
               , AddSkill SkArmorMelee $ (-1 + 1 `dL` 6) * 3
               , SetFlag Precious, SetFlag Equipable
               , EqpSlot EqpSlotMiscBonus ]
  , ieffects = []
  , idesc    = "Contraption of lenses and mirrors on a polished brass headband for capturing and strengthening light in dark environment. Hampers vision in daylight. Stackable."
  , ikit     = []
  }
sightSharpening = ringTemplate  -- small and round, so mistaken for a ring
  { iname    = "sharp monocle"
  , ifreq    = [(TREASURE, 20), (ADD_SIGHT, 1)]
      -- it's has to be very rare, because it's powerful and not unique,
      -- and also because it looks exactly as one of necklaces, so it would
      -- be misleading when seen on the map
  , irarity  = [(7, 1), (10, 12)]  -- low @ifreq@
  , iweight  = 50  -- heavier that it looks, due to glass
  , iaspects = [ AddSkill SkSight $ 1 + 1 `dL` 2
               , AddSkill SkHurtMelee $ (-1 + 1 `d` 3) * 3
               , EqpSlot EqpSlotSight ]
               ++ iaspects ringTemplate
  , idesc    = "Lets you better focus your weaker eye."
  }
-- Don't add standard effects to rings, because they go in and out
-- of eqp and so activating them would require UI tedium: looking for
-- them in eqp and stash or even activating a wrong item by mistake.
--
-- By general mechanisms, due to not having effects that could identify
-- them by observing the effect, rings are identified on pickup.
-- That's unlike necklaces, which provide the fun of id-by-use, because they
-- have effects and when the effects are triggered, they get identified.
ringTemplate = ItemKind
  { isymbol  = symbolRing
  , iname    = "ring"
  , ifreq    = [(RING_UNKNOWN, 1)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 2)]  -- the default very low
  , iverbHit = "knock"
  , iweight  = 15
  , idamage  = 0
  , iaspects = [PresentAs RING_UNKNOWN, SetFlag Precious, SetFlag Equipable]
  , ieffects = []
  , idesc    = "It looks like an ordinary object, but it's in fact a generator of exceptional effects: adding to some of your natural qualities and subtracting from others."
  , ikit     = []
  }
ring1 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(8, 4)]
  , iaspects = [ AddSkill SkSpeed $ 1 `dL` 3
               , AddSkill SkMaxHP (-10)
               , EqpSlot EqpSlotSpeed ]
               ++ iaspects ringTemplate
  }
ring2 = ringTemplate
  { iname    = "the Ring"
  , ifreq    = [(TREASURE, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ SetFlag Unique, ELabel "of Rush"
               , AddSkill SkSpeed $ (1 + 1 `dL` 2) * 2
               , AddSkill SkMaxHP (-20)
               , AddSkill SkMaxCalm (-40)
               , SetFlag Durable, EqpSlot EqpSlotSpeed ]
               ++ iaspects ringTemplate
  , idesc    = "Roughly-shaped metal with shallow scratches marking it."
  }
ring3 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(3, 4), (10, 8)]
  , iaspects = [ AddSkill SkHurtMelee $ (2 + 1 `d` 2 + (1 `dL` 2) * 2 ) * 3
               , AddSkill SkMaxHP $ (-3 + 1 `dL` 3) * 10
               , EqpSlot EqpSlotHurtMelee ]
               ++ iaspects ringTemplate
  }
ring4 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 8)]
  , iaspects = [ AddSkill SkMaxHP $ 5 + (1 `d` 2 + 1 `dL` 2) * 5
               , AddSkill SkMaxCalm $ -30 + (1 `dL` 3) * 5
               , EqpSlot EqpSlotMaxHP ]
               ++ iaspects ringTemplate
  }
ring5 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(5, 1), (10, 9)]  -- needed after other rings drop Calm
  , iaspects = [ AddSkill SkMaxCalm $ 20 + (1 `dL` 4) * 5
               , AddSkill SkHearing 6
               , EqpSlot EqpSlotMiscBonus ]
               ++ iaspects ringTemplate
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring6 = ringTemplate  -- weak skill per eqp slot, so can be without drawbacks
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 3)]
  , iaspects = [ AddSkill SkShine 1
               , EqpSlot EqpSlotShine ]
               ++ iaspects ringTemplate
  , idesc    = "A sturdy ring with a large, shining stone."
  }
ring7 = ringTemplate
  { ifreq    = [(RING_OF_OPPORTUNITY_SNIPER, 1) ]  -- only for scenarios
  , irarity  = [(1, 1)]
  , iaspects = [ ELabel "of opportunity sniper"
               , AddSkill SkProject 8
               , EqpSlot EqpSlotProject ]
               ++ iaspects ringTemplate
  }
ring8 = ringTemplate
  { ifreq    = [(RING_OF_OPPORTUNITY_GRENADIER, 1) ]  -- only for scenarios
  , irarity  = [(1, 1)]
  , iaspects = [ ELabel "of opportunity grenadier"
               , AddSkill SkProject 11
               , EqpSlot EqpSlotProject ]
               ++ iaspects ringTemplate
  }

-- ** Armor

armorLeather = ItemKind
  { isymbol  = symbolTorsoArmor
  , iname    = "leather armor"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_LOOSE, 1), (STARTING_ARMOR, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 9), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 7000
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee (-2)
               , AddSkill SkArmorMelee $ (2 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (1 + 1 `dL` 2) * 3
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee ]
  , ieffects = []
  , idesc    = "A stiff jacket formed from leather boiled in bee wax, padded linen and horse hair. Protects from anything that is not too sharp. Smells much better than the rest of your garment."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "ring armor"
  , ifreq    = [ (COMMON_ITEM, 100), (ARMOR_LOOSE, 1), (ARMOR_RANGED, 50)
               , (STARTING_ARMOR, 50) ]
  , iflavour = zipPlain [Cyan]
  , irarity  = [(6, 9), (10, 3)]
  , iweight  = 12000
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee (-3)
               , AddSkill SkArmorMelee $ (2 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (4 + 1 `dL` 2) * 3
               , AddSkill SkOdor 2
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorRanged ]
  , ieffects = []
  , idesc    = "A long shirt with tiny iron rings sewn into it. Discourages foes from attacking your torso, especially with ranged weapons, which can't pierce the rings nor aim between them. The stiff fabric is hard to wash, though."
  }
meleeEnhancement = ItemKind
  { isymbol  = symbolTool
  , iname    = "whetstone"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(10, 10)]
  , iverbHit = "smack"
  , iweight  = 400
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee $ (1 `dL` 7) * 5
               , AddSkill SkArmorMelee 2
               , SetFlag Equipable, EqpSlot EqpSlotHurtMelee ]
  , ieffects = []
  , idesc    = "A portable sharpening stone for keeping your weapons keen and true, without the need to set up camp, fish out tools and assemble a proper sharpening workshop. Provides an extra polish to amor, as well."
  , ikit     = []
  }
gloveFencing = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "leather glove"
  , ifreq    = [ (COMMON_ITEM, 100), (ARMOR_MISC, 1), (ARMOR_RANGED, 50)
               , (STARTING_ARMOR, 50) ]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]
  , iverbHit = "flap"
  , iweight  = 100
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddSkill SkArmorRanged $ (1 `dL` 2) * 3
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotHurtMelee
               , toVelocity 50 ]  -- flaps and flutters
  , ieffects = []
  , idesc    = "A fencing glove from rough leather ensuring a good grip. Also quite effective in averting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "steel gauntlet"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1), (STARTING_ARMOR, 50)]
  , iflavour = zipPlain [BrCyan]
  , irarity  = [(1, 9), (10, 3)]
  , iweight  = 300
  , idamage  = 2 `d` 1
  , iaspects = [ AddSkill SkArmorMelee $ (1 + 1 `dL` 4) * 5
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- flaps and flutters
  , idesc    = "Long leather gauntlet covered in overlapping steel plates."
  }
gloveJousting = gloveFencing
  { iname    = "Tournament Gauntlet"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1)]
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 3), (10, 3)]
  , iverbHit = "rasp"
  , iweight  = 3000
  , idamage  = 3 `d` 1
  , iaspects = [ SetFlag Unique
               , AddSkill SkHurtMelee $ (-7 + 1 `dL` 5) * 3
               , AddSkill SkArmorMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 5
               , AddSkill SkArmorRanged $ (1 + 1 `dL` 2) * 3
                 -- very random on purpose and can even be good on occasion
                 -- or when ItemRerolled enough times
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- flaps and flutters
  , idesc    = "Rigid, steel jousting handgear. If only you had a lance. And a horse to carry it all."
  }
hatUshanka = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "ushanka hat"
  , ifreq    = [ (COMMON_ITEM, 100), (ARMOR_MISC, 1), (CLOTHING_MISC, 1)
               , (STARTING_ARMOR, 50) ]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 6), (10, 1)]
  , iverbHit = "tickle"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ Timeout $ (2 + 1 `d` 2) * 3
               , AddSkill SkArmorMelee 5, AddSkill SkHearing (-10)
               , SetFlag Periodic, SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- flaps and flutters
  , ieffects = [RefillCalm 1]
  , idesc    = "Soft and warm fur. It keeps your ears warm."
  , ikit     = []
  }
capReinforced = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "leather cap"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1), (STARTING_ARMOR, 50)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(6, 9), (10, 3)]
  , iverbHit = "cut"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee $ (1 `d` 2) * 5
               , AddSkill SkProject 1
                   -- the brim shields against blinding by light sources, etc.
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotProject ]
  , ieffects = []
  , idesc    = "Boiled leather with a wide brim. It might soften a blow."
  , ikit     = []
  }
helmArmored = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "bucket helm"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1), (STARTING_ARMOR, 50)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(6, 9), (10, 3)]
  , iverbHit = "bounce"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee $ (1 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (2 + 1 `dL` 2) * 3  -- headshot
               , AddSkill SkHearing (-7), AddSkill SkSight (-1)
               , AddSkill SkSmell (-5)
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorRanged ]
  , ieffects = []
  , idesc    = "Blocks out everything, including your senses."
  , ikit     = []
  }
smokingJacket = ItemKind
  { isymbol  = symbolClothes
  , iname    = "smoking jacket"
  , ifreq    = [(COMMON_ITEM, 100), (CLOTHING_MISC, 1), (CHIC_GEAR, 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(1, 9), (10, 3)]
  , iverbHit = "stroke"
  , iweight  = 5000
  , idamage  = 0
  , iaspects = [ Timeout $ (1 `d` 2) * 3
               , AddSkill SkSpeed 2
               , AddSkill SkOdor 2
               , SetFlag Periodic, SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotSpeed ]
  , ieffects = [RefillCalm 1]
  , idesc    = "Wearing this velvet jacket, anyone would look dashing."
  , ikit     = []
  }
-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
-- Their biggest power is pushing enemies, which however reduces
-- to 1 extra damage point if no clear space behind enemy.
-- So they require keen tactical management.
-- Note that AI will pick them up but never wear and will use them at most
-- as a way to push itself. Despite being @Meleeable@, they will not be used
-- as weapons either. This is OK, using shields smartly is totally beyond AI.
buckler = ItemKind
  { isymbol  = symbolShield
  , iname    = "buckler"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_LOOSE, 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(4, 5)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = 0  -- safe to be used on self
  , iaspects = [ Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 2
               , AddSkill SkArmorMelee 40
                   -- not enough to compensate; won't be in eqp
               , AddSkill SkHurtMelee (-30)
                   -- too harmful; won't be wielded as weapon
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotArmorMelee ]
  , ieffects = [PushActor (ThrowMod 200 50 1)]  -- 1 step, fast
  , idesc    = "Heavy and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too small to intercept projectiles with. May serve as a counterweight to suddenly push forth."
  , ikit     = []
  }
shield = buckler
  { iname    = "shield"
  , irarity  = [(8, 4)]  -- the stronger variants add to total probability
  , iflavour = zipPlain [Green]
  , iweight  = 4000
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 4
               , AddSkill SkArmorMelee 80
                   -- not enough to compensate; won't be in eqp
               , AddSkill SkHurtMelee (-70)
                   -- too harmful; won't be wielded as weapon
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- unwieldy to throw
  , ieffects = [PushActor (ThrowMod 400 50 1)]  -- 2 steps, fast
  , idesc    = "Large and unwieldy. Absorbs a percentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with. Useful to push foes out of the way."
  }
shield2 = shield
  { ifreq    = [(COMMON_ITEM, 3 * 3)]  -- very low base rarity
  , iweight  = 5000
  , idamage  = 8 `d` 1
  , idesc    = "A relic of long-past wars, heavy and with a central spike."
  }
shield3 = shield2
  { ifreq    = [(COMMON_ITEM, 1 * 3)]  -- very low base rarity
  , iweight  = 6000
  , idamage  = 12 `d` 1
  }

-- ** Weapons

knife = ItemKind
  { isymbol  = symbolEdged
  , iname    = "dagger"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 200)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(2, 45), (4, 1)]
  , iverbHit = "cut"
  , iweight  = 800
  , idamage  = 6 `d` 1
  , iaspects = [ Timeout 2
               , AddSkill SkHurtMelee $ (-1 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddSkill SkArmorMelee $ (1 `d` 2) * 5
                   -- very common, so don't make too random
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "A short dagger for thrusting and parrying blows. Does not penetrate deeply, but is quick to move and hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = knife
  { iname    = "The Double Dagger"
  , ifreq    = [(TREASURE, 20)]
  , irarity  = [(1, 3), (10, 3)]
  , iaspects = SetFlag Unique
               : iaspects knife
  , ieffects = [Discharge 1 50, Yell]  -- powerful and low timeout, but noisy
                                       -- and no effect if no weapons charged
  , idesc    = "A double dagger that a focused fencer can use to catch and twist away an opponent's blade."
  }
hammerTemplate = ItemKind
  { isymbol  = symbolHafted
  , iname    = "war hammer"
  , ifreq    = [(HAMMER_UNKNOWN, 1)]
  , iflavour = zipFancy [BrMagenta]  -- avoid "pink"
  , icount   = 1
  , irarity  = [(5, 20), (8, 1)]
  , iverbHit = "club"
  , iweight  = 1600
  , idamage  = 8 `d` 1  -- we are lying about the dice here, but the dungeon
                        -- is too small and the extra-dice hammers too rare
                        -- to subdivide this identification class by dice
  , iaspects = [ PresentAs HAMMER_UNKNOWN
               , SetFlag Durable, SetFlag Meleeable
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "It may not cause extensive wounds, but neither does it harmlessly glance off heavy armour as blades and polearms tend to. There are so many shapes and types, some looking more like tools than weapons, that at a glance you can't tell what a particular specimen does. It's obvious, though, that any of them requires some time to recover after a swing."  -- if it's really the average kind, the weak kind, the description stays; if not, it's replaced with one of the descriptions below at identification time
  , ikit     = []
  }
hammer1 = hammerTemplate
  { ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 70)]
  , iaspects = [Timeout 5, EqpSlot EqpSlotWeaponBig]
               ++ iaspects hammerTemplate
  }
hammer2 = hammerTemplate
  { ifreq    = [(COMMON_ITEM, 20), (STARTING_WEAPON, 7)]
  , iverbHit = "gouge"
  , iaspects = [Timeout 3, EqpSlot EqpSlotWeaponFast]
               ++ iaspects hammerTemplate
  , idesc    = "Upon closer inspection, this hammer turns out particularly handy and well balanced, with one thick and sturdy and two long and sharp points compensating the modest size."
  }
hammer3 = hammerTemplate
  { ifreq    = [(COMMON_ITEM, 3), (STARTING_WEAPON, 1)]
  , iverbHit = "puncture"
  , iweight  = 2400  -- weight gives it away
  , idamage  = 12 `d` 1
  , iaspects = [ Timeout 12  -- balance, or @DupItem@ would break the game
               , EqpSlot EqpSlotWeaponBig]
               ++ delete (PresentAs HAMMER_UNKNOWN) (iaspects hammerTemplate)
  , idesc    = "This hammer sports a long metal handle that increases the momentum of the sharpened head's swing, at the cost of long recovery."
  }
hammerParalyze = hammerTemplate
  { iname    = "The Brute Hammer"
  , ifreq    = [(TREASURE, 20)]
  , irarity  = [(5, 1), (8, 6)]
  , iaspects = [ SetFlag Unique
               , Timeout 5
               , EqpSlot EqpSlotWeaponBig ]
               ++ iaspects hammerTemplate
  , ieffects = [Paralyze 10]
  , idesc    = "A huge shapeless lump of meteorite iron alloy on a sturdy pole. Nobody remains standing when this head connects."
  }
hammerSpark = hammerTemplate
  { iname    = "The Grand Smithhammer"
  , ifreq    = [(TREASURE, 20)]
  , irarity  = [(5, 1), (8, 6)]
  , iweight  = 2400  -- weight gives it away
  , idamage  = 12 `d` 1
  , iaspects = [ SetFlag Unique
               , Timeout 10
               , EqpSlot EqpSlotWeaponBig
               , AddSkill SkShine 3]
               ++ delete (PresentAs HAMMER_UNKNOWN) (iaspects hammerTemplate)
  , ieffects = [Explode S_SPARK]
      -- we can't use a focused explosion, because it would harm the hammer
      -- wielder as well, unlike this one
  , idesc    = "Smiths of old wielded this heavy hammer and its sparks christened many a potent blade."
  }
sword = ItemKind
  { isymbol  = symbolEdged
  , iname    = "sword"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 30)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(4, 1), (6, 20)]
  , iverbHit = "slash"
  , iweight  = 2000
  , idamage  = 10 `d` 1
  , iaspects = [ Timeout 7
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "Difficult to master; deadly when used effectively. The steel is particularly hard and keen, but rusts quickly without regular maintenance."
  , ikit     = []
  }
swordImpress = sword
  { iname    = "The Master's Sword"
  , ifreq    = [(TREASURE, 20)]
  , irarity  = [(5, 1), (8, 6)]
  , iaspects = SetFlag Unique
               : iaspects sword
  , ieffects = [Impress]
  , idesc    = "A particularly well-balance blade, lending itself to impressive shows of fencing skill."
  }
swordNullify = sword
  { iname    = "The Gutting Sword"
  , ifreq    = [(TREASURE, 20)]
  , iverbHit = "pierce"
  , irarity  = [(5, 1), (8, 6)]
  , iaspects = [SetFlag Unique, Timeout 3, EqpSlot EqpSlotWeaponFast]
               ++ (iaspects sword \\ [Timeout 7, EqpSlot EqpSlotWeaponBig])
  , ieffects = [ DropItem 1 maxBound COrgan CONDITION
               , RefillCalm (-10)
               , Yell ]
  , idesc    = "Cold, thin blade that pierces deeply and sends its victim into abrupt, sobering shock."
  }
halberd = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "war scythe"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 20)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 1), (8, 15)]
  , iverbHit = "impale"
  , iweight  = 3000
  , idamage  = 12 `d` 1
  , iaspects = [ Timeout 10
               , AddSkill SkHurtMelee $ (-5 + 1 `dL` 3) * 5
                   -- useless against armor at game start
               , AddSkill SkArmorMelee 20
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 20 ]  -- not balanced
  , ieffects = []
  , idesc    = "An improvised weapon made of scythe's blade attached to a long pole. Not often one succeeds in making enough space to swing it freely, but even when stuck between terrain obstacles it blocks approaches effectively and makes using other weapons difficult, both by friends and foes."
  , ikit     = []
  }
halberd2 = halberd
  { iname    = "halberd"
  , ifreq    = [(COMMON_ITEM, 3 * 2), (STARTING_WEAPON, 1)]
  , iweight  = 4000
  , iaspects = AddSkill SkHurtMelee ((-6 + 1 `dL` 4) * 10)
                 -- balance, or @DupItem@ would break the game;
                 -- together with @RerollItem@, it's allowed to, though
               : (iaspects halberd
                  \\ [AddSkill SkHurtMelee $ (-6 + 1 `dL` 4) * 5])
  , idamage  = 18 `d` 1
  , idesc    = "A long haft with a sharp blade. Designed and refined for war."
  }
halberd3 = halberd2
  { iname    = "bardiche"
  , ifreq    = [(COMMON_ITEM, 1 * 2)]  -- compensating for low base rarity
  , iverbHit = "carve"
  , iweight  = 5000
  , idamage  = 24 `d` 1
  , idesc    = "The reach of a spear but the edge of an axe."
  }
halberdPushActor = halberd
  { iname    = "The Swiss Halberd"
  , ifreq    = [(TREASURE, 20)]
  , irarity  = [(7, 0), (9, 15)]
  , iaspects = SetFlag Unique
               : iaspects halberd
  , ieffects = [PushActor (ThrowMod 200 100 1)]  -- 2 steps, slow
  , idesc    = "A versatile polearm, with great reach and leverage. Foes are held at a distance."
  }

-- ** Treasure

gemTemplate = ItemKind
  { isymbol  = symbolGold
  , iname    = "gem"
  , ifreq    = [(GEM_UNKNOWN, 1), (VALUABLE, 100)]
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = [(3, 0), (10, 24)]
  , iverbHit = "tap"
  , iweight  = 50
  , idamage  = 0
  , iaspects = [PresentAs GEM_UNKNOWN, SetFlag Precious]
  , ieffects = []
  , idesc    = "Useless, and still worth around 100 gold each. Would gems of thought and pearls of artful design be valued that much in our age of Science and Progress!"
  , ikit     = []
  }
gem1 = gemTemplate
  { ifreq    = [ (TREASURE, 100), (GEM, 100), (ANY_JEWELRY, 10)
               , (VALUABLE, 100) ]
  , irarity  = [(3, 0), (6, 12), (10, 8)]
  , iaspects = [AddSkill SkShine 1, AddSkill SkSpeed (-1)]
                 -- reflects strongly, distracts; so it glows in the dark,
                 -- is visible on dark floor, but not too tempting to wear
               ++ iaspects gemTemplate
  }
gem2 = gem1
  { ifreq    = [ (TREASURE, 150), (GEM, 100), (ANY_JEWELRY, 10)
               , (VALUABLE, 100) ]
  , irarity  = [(5, 0), (7, 25), (10, 8)]
  }
gem3 = gem1
  { ifreq    = [ (TREASURE, 150), (GEM, 100), (ANY_JEWELRY, 10)
               , (VALUABLE, 100) ]
  , irarity  = [(7, 0), (8, 20), (10, 8)]
  }
gem4 = gem1
  { ifreq    = [ (TREASURE, 150), (GEM, 100), (ANY_JEWELRY, 30)
               , (VALUABLE, 100) ]
  , irarity  = [(9, 0), (10, 70)]
  }
gem5 = gem1
  { isymbol  = symbolSpecial
  , iname    = "elixir"
  , ifreq    = [ (TREASURE, 100), (GEM, 25), (ANY_JEWELRY, 10)
               , (VALUABLE, 100) ]
  , iflavour = zipPlain [BrYellow]
  , irarity  = [(1, 40), (10, 10)]
  , iaspects = [ ELabel "of youth", SetFlag Precious  -- not hidden
               , AddSkill SkOdor (-1) ]
  , ieffects = [RefillCalm 10, RefillHP 40]
  , idesc    = "A crystal vial of amber liquid, supposedly granting eternal youth and fetching 100 gold per piece. The main effect seems to be mild euphoria, but it admittedly smells good and heals minor ailments rather well."
  }
currencyTemplate = ItemKind
  { isymbol  = symbolGold
  , iname    = "gold piece"
  , ifreq    = [(CURRENCY_UNKNOWN, 1), (VALUABLE, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + 1 `d` 20 + 1 `dL` 20
  , irarity  = [(1, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 31
  , idamage  = 0
  , iaspects = [PresentAs CURRENCY_UNKNOWN, SetFlag Precious]
  , ieffects = []
  , idesc    = "Reliably valuable in every civilized plane of existence."
  , ikit     = []
  }
currency = currencyTemplate
  { ifreq    = [(TREASURE, 100), (S_CURRENCY, 100), (VALUABLE, 1)]
  , iaspects = [AddSkill SkShine 1, AddSkill SkSpeed (-1)]
               ++ iaspects currencyTemplate
  }

-- ** Tools to be actively used, but not worn

jumpingPole = ItemKind
  { isymbol  = symbolWand
  , iname    = "jumping pole"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 3)]
  , iverbHit = "prod"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [ Timeout $ (2 + 1 `d` 2 - 1 `dL` 2) * 5
               , SetFlag Durable ]
  , ieffects = [toOrganGood S_HASTED 1]
                 -- This works and doesn't cause AI loops. @InsertMove@
                 -- would produce an activation that doesn't change game state.
                 -- Hasting for an absolute number of turns would cause
                 -- an explosion of time when several poles are accumulated.
                 -- Here it speeds AI up for exactly the turn spent activating,
                 -- so when AI applies it repeatedly, it gets its time back and
                 -- is not stuck. In total, the exploration speed is unchanged,
                 -- but it's useful when fleeing in the dark to make distance
                 -- and when initiating combat, so it's OK that AI uses it.
                 -- Timeout is rather high, because for factions with leaders
                 -- some time is often gained, so this could be useful
                 -- even during melee, which would be tiresome to employ.
  , idesc    = "Makes you vulnerable at take-off, but then you are free like a bird."
  , ikit     = []
  }
seeingItem = ItemKind
  { isymbol  = symbolFood
  , iname    = "giant pupil"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 2)]
  , iverbHit = "gaze at"
  , iweight  = 100
  , idamage  = 0
  , iaspects = [ Timeout 3
               , AddSkill SkSight 10  -- a spyglass for quick wields
               , AddSkill SkMaxCalm 30  -- to diminish clipping sight by Calm
               , AddSkill SkShine 2  -- to lit corridors when flying
               , SetFlag Periodic ]
  , ieffects = [ Detect DetectActor 20  -- rare enough
               , When (TriggeredBy ActivationPeriodic) $ SeqEffect
                   [ toOrganNoTimer S_POISONED  -- really can't be worn
                   , Summon MOBILE_MONSTER 1 ] ]
  , idesc    = "A slimy, dilated green pupil torn out from some giant eye. Clear and focused, as if still alive."
  , ikit     = []
  }
