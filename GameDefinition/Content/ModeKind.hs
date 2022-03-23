-- | Definitions of game mode kinds.
module Content.ModeKind
  ( -- * Group name patterns
    groupNamesSingleton, groupNames
  , -- * Content
    content
#ifdef EXPOSE_INTERNAL
  -- * Group name patterns
  , pattern RAID, pattern BRAWL, pattern LONG, pattern CRAWL, pattern FOGGY, pattern SHOOTOUT, pattern PERILOUS, pattern HUNT, pattern NIGHT, pattern FLIGHT, pattern BURNING, pattern ZOO, pattern RANGED, pattern AMBUSH, pattern SAFARI, pattern DIG, pattern SEE, pattern SHORT, pattern CRAWL_EMPTY, pattern CRAWL_SURVIVAL, pattern SAFARI_SURVIVAL, pattern BATTLE, pattern BATTLE_DEFENSE, pattern BATTLE_SURVIVAL, pattern DEFENSE, pattern DEFENSE_EMPTY
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Content.CaveKind (CaveKind, pattern DEFAULT_RANDOM)
import Game.LambdaHack.Content.FactionKind (Outcome (..))
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.DefsInternal (GroupName (..))

import Content.CaveKind hiding (content, groupNames, groupNamesSingleton)
import Content.FactionKind hiding (content, groupNames, groupNamesSingleton)
import Content.ItemKindActor

-- * Group name patterns

groupNamesSingleton :: [GroupName ModeKind]
groupNamesSingleton =
       [RAID, BRAWL, LONG, CRAWL, FOGGY, SHOOTOUT, PERILOUS, HUNT, NIGHT, FLIGHT, BURNING, ZOO, RANGED, AMBUSH, SAFARI, DIG, SEE, SHORT, CRAWL_EMPTY, CRAWL_SURVIVAL, SAFARI_SURVIVAL, BATTLE, BATTLE_DEFENSE, BATTLE_SURVIVAL, DEFENSE, DEFENSE_EMPTY]

pattern RAID, BRAWL, LONG, CRAWL, FOGGY, SHOOTOUT, PERILOUS, HUNT, NIGHT, FLIGHT, BURNING, ZOO, RANGED, AMBUSH, SAFARI, DIG, SEE, SHORT, CRAWL_EMPTY, CRAWL_SURVIVAL, SAFARI_SURVIVAL, BATTLE, BATTLE_DEFENSE, BATTLE_SURVIVAL, DEFENSE, DEFENSE_EMPTY :: GroupName ModeKind

groupNames :: [GroupName ModeKind]
groupNames = []

pattern RAID = GroupName "raid"
pattern BRAWL = GroupName "brawl"
pattern LONG = GroupName "long crawl"
pattern CRAWL = GroupName "crawl"
pattern FOGGY = GroupName "foggy shootout"
pattern SHOOTOUT = GroupName "shootout"
pattern PERILOUS = GroupName "perilous hunt"
pattern HUNT = GroupName "hunt"
pattern NIGHT = GroupName "night flight"
pattern FLIGHT = GroupName "flight"
pattern BURNING = GroupName "burning zoo"
pattern ZOO = GroupName "zoo"
pattern RANGED = GroupName "ranged ambush"
pattern AMBUSH = GroupName "ambush"
pattern SAFARI = GroupName "safari"
pattern DIG = GroupName "dig"
pattern SEE = GroupName "see"
pattern SHORT = GroupName "short"
pattern CRAWL_EMPTY = GroupName "crawlEmpty"  -- only the first word matters
pattern CRAWL_SURVIVAL = GroupName "crawlSurvival"
pattern SAFARI_SURVIVAL = GroupName "safariSurvival"
pattern BATTLE = GroupName "battle"
pattern BATTLE_DEFENSE = GroupName "battleDefense"
pattern BATTLE_SURVIVAL = GroupName "battleSurvival"
pattern DEFENSE = GroupName "defense"
pattern DEFENSE_EMPTY = GroupName "defenseEmpty"

-- * Content

content :: [ModeKind]
content =
  [raid, brawl, crawl, shootout, hunt, flight, zoo, ambush, safari, dig, see, short, crawlEmpty, crawlSurvival, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverCrawl, screensaverShootout, screensaverHunt, screensaverFlight, screensaverZoo, screensaverAmbush, screensaverSafari]

raid,    brawl, crawl, shootout, hunt, flight, zoo, ambush, safari, dig, see, short, crawlEmpty, crawlSurvival, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverCrawl, screensaverShootout, screensaverHunt, screensaverFlight, screensaverZoo, screensaverAmbush, screensaverSafari :: ModeKind

-- What other symmetric (two only-one-moves factions) and asymmetric vs crowd
-- scenarios make sense (e.g., are good for a tutorial or for standalone
-- extreme fun or are impossible as part of a crawl)?
-- sparse melee at night: no, shade ambush in brawl is enough
-- dense melee: no, keeping big party together is a chore and big enemy
--   party is less fun than huge enemy party
-- crowd melee in daylight: no, possible in crawl and at night is more fun
-- sparse ranged at night: no, less fun than dense and if no reaction fire,
--   just a camp fest or firing blindly
-- dense ranged in daylight: no, less fun than at night with flares
-- crowd ranged: no, fish in a barrel, less predictable and more fun inside
--   crawl, even without reaction fire

raid = ModeKind
  { mname   = "raid (tutorial, 1)"
  , mfreq   = [(RAID, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = True
  , mattract = False
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mendMsg = [ (Killed, "This expedition has gone wrong. However, scientific mind does not despair, but analyzes and corrects. Did you perchance awake one animal too many? Did you remember to try using all consumables at your disposal for your immediate survival? Did you choose a challenge with difficulty level within your means? Answer honestly, ponder wisely, experiment methodically.")
              , (Defeated, "Regrettably, the other team snatched the grant, while you were busy contemplating natural phenomena. Science is a competitive sport, as sad as it sounds. It's not enough to make a discovery, you have to get there first.")
              , (Escape, "You've got hold of the machine! Think of the hours of fun taking it apart and putting it back together again! That's a great first step on your quest to solve the typing problems of the world.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Two heroes vs. Spawned enemies"
      , "* Gather gold"
      , "* Find a way out and escape ASAP"
      ]
  , mdesc   = "An incredibly advanced typing machine worth 100 gold is buried at the exit of this maze. Be the first to find it and fund a research team that makes typing accurate and dependable forever."
  , mreason = "In addition to initiating the (loose) game plot, this adventure provides an introductory tutorial. Relax, explore, gather loot, find the way out and escape. With some luck, you won't even need to fight anything."
  , mhint   = "You can't use gathered items in your next encounters, so trigger any consumables at will. Feel free to scout with only one of the heroes and keep the other one immobile, e.g., standing guard over the squad's shared inventory stash. If in grave danger, retreat with the scout to join forces with the guard. The more gold collected and the faster the victory, the higher your score in this encounter."
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { mname   = "brawl (tutorial, 2)"
  , mfreq   = [(BRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = True
  , mattract = False
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mendMsg = [ (Killed, "The inquisitive scholars turned out to be envious of our deep insight to the point of outright violence. It would still not result in such a defeat and recanting of our thesis if we figured out to use terrain to protect us from missiles or even completely hide our presence. It would also help if we honourably kept our ground together to the end, at the same time preventing the overwhelming enemy forces from brutishly ganging up on our modest-sized, though valiant, research team.")
              , (Conquer, "That's settled: local compactness *is* necessary for relative completeness, given the assumptions.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Three heroes vs. Three human enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "Your research team disagrees over a drink with some gentlemen scientists about premises of a relative completeness theorem and there's only one way to settle that."
      -- Not enough space with square fonts and also this is more of a hint than a flavour: Remember to keep your party together when opponents are spotted, or they might be tempted to silence solitary disputants one by one and so win the altercation.
  , mreason = "In addition to advancing game plot, this encounter trains melee, squad formation and stealth. The battle is symmetric in goals (incapacitate all enemies) and in squad capabilities (only the pointman moves, others either melee or wait)."
  , mhint   =  T.intercalate "\n"
    [ "Run a short distance with Shift or LMB, switch the pointman with Tab, repeat. In open terrain, if you keep distance between teammates, this resembles the leap frog infantry tactics. For best effects, end each sprint behind a cover or concealment."
    , "Observe and mimic the enemies. If you can't see an enemy that apparently can see you, in reversed circumstances you would have the same advantage. Savour the relative fairness --- you won't find any in the main crawl adventure that follows."
    , "If you get beaten repeatedly, try using all consumables you find. Ponder the hints from the defeat message, in particular the one about keeping your party together once the opponents are spotted. However, if you want to discover a winning tactics on your own, make sure to ignore any such tips until you succeed."
    ]
  }

crawl = ModeKind
  { mname   = "long crawl (main)"
  , mfreq   = [(LONG, 1), (CRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mendMsg = [ (Killed, killedMsg)
              , (Escape, "It's better to live to tell the tale than to choke on more than one can swallow. There was no more exquisite cultural artifacts and glorious scientific wonders in these forbidding tunnels anyway. Or were there?") ]
  , mrules  = T.intercalate "\n"
      [ "* Many levels"
      , "* Three heroes vs. Spawned enemies"
      , "* Gather gold, gems and elixirs"
      , "* Find a way out and escape ASAP"
      ]
  , mdesc   = "Enjoy the peaceful seclusion of these cold austere tunnels, but don't let wanton curiosity, greed and the ever-creeping abstraction madness keep you down there for too long. If you find survivors (whole or perturbed or segmented) of the past scientific missions, exercise extreme caution and engage or ignore at your discretion."
  , mreason = "This is the main, longest and most replayable scenario of the game."
  , mhint   = "If you keep dying, attempt the subsequent adventures as a breather (perhaps at lowered difficulty). They fill the gaps in the plot and teach particular skills that may come in handy and help you discover new tactics of your own or come up with a strategy for staving off the attrition. Also experimenting with the initial adventures may answer some questions you didn't have when you attempted them originally."
  }
 where
   killedMsg = T.intercalate "\n"
     [ "To think that followers of science and agents of enlightenment would earn death as their reward! Where did we err in our ways? Perhaps nature should not have been disturbed so brashly and the fell beasts woken up from their slumber so eagerly?"
     , "Perhaps the gathered items should have been used for scientific experiments on the spot rather than hoarded as if of base covetousness? Or perhaps the challenge, chosen freely but without the foreknowledge of the grisly difficulty, was insurmountable and forlorn from the start, despite the enormous power of educated reason at out disposal?"
     ]

-- The trajectory tip is important because of tactics of scout looking from
-- behind a bush and others hiding in mist. If no suitable bushes,
-- fire once and flee into mist or behind cover. Then whomever is out of LOS
-- range or inside mist can shoot at the last seen enemy locations,
-- adjusting aim according to sounds and incoming missile trajectories.
-- If the scout can't find bushes or glass building to set a lookout,
-- the other team members are more spotters and guardians than snipers
-- and that's their only role, so a small party makes sense.
shootout = ModeKind  -- sparse ranged in daylight
  { mname   = "foggy shootout (3)"
  , mfreq   = [(FOGGY, 1), (SHOOTOUT, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mendMsg = [ (Killed, killedMsg)
              , (Conquer, "That was a good fight, with scientifically accurate application of missiles, cover and concealment. Not even skilled logicians can routinely deduce enemy position from the physical trajectory of their projectiles nor by firing without line of sight and interpreting auditory cues. However, while this steep hurdle is overcome, the dispute is not over yet.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Three heroes vs. Three human enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "Whose arguments are most striking and whose ideas fly fastest? Let's scatter up, attack the problems from different angles and find out."
  , mreason = "This adventure teaches the ranged combat skill in the simplified setup of fully symmetric battle."
  , mhint   = "Try to come up with the best squad formation for this tactical challenge. Don't despair if you run out of ammo, because if you aim truly, enemy has few hit points left at this point. In turn, when trying to avoid enemy projectiles, you can display the trajectory of any soaring entity by pointing it with the crosshair in aiming mode."
  }
 where
   killedMsg = T.intercalate "\n"
     [ "This is a disgrace. What have we missed in our theoretic models of this fight? Did we miss a human lookout placed in a covered but unobstructed spot that lets the rest of the squad snipe from concealment or from a safe distance?"
     , "Barring that, would we end up in a better shape even if we all hid and only fired blindly? We'd listen to impact sounds and wait vigilantly for incoming enemy missiles in order to register their trajectories and derive hints of enemy location. Apparently, ranged combat requires a change of pace and better planning than our previous simple but efficient calculations accustomed us to."
     ]

hunt = ModeKind  -- melee vs ranged with reaction fire in daylight
  { mname   = "perilous hunt (4)"
  , mfreq   = [(PERILOUS, 1), (HUNT, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterHunt
  , mcaves  = cavesHunt
  , mendMsg = [ (Killed, "Leaving concealment might have not been rational enough, leaving cover is hard to justify on a scientific basis and wandering off on an area of a heated dispute is foolhardy. All this is doubly regrettable, given that our cold-hearted opponents supported their weak arguments with inexplicably effective telegraphy and triangulation equipment. And we so deserve a complete intellectual victory, if only we strove to lower the difficulty of this altercation instead of raising it.")
      -- this is in the middle of the scenario list and the mission is not tricky, so a subtle reminder about lowering difficulty, in case the player struggles
              , (Conquer, "We chased them off and proved our argument, like we knew that we would. It feels efficient to stick together and prevail. We taught them a lesson in rationality, despite their superior scientific equipment. Scientific truth prevails over brute force.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Seven heroes vs. Seven human enemies capable of concurrent attacks"
      , "* Minimize losses"
      , "* Incapacitate all human enemies ASAP"
      ]
  , mdesc   = "Who is the hunter and who is the prey? The only criterion is last man standing when the chase for truth ends."
  , mreason = "This adventure is quite a tactical challenge, because enemies are allowed to fling their ammo simultaneously at your team, which has no such ability."
  , mhint   = "Try not to outshoot the enemy, but to instead focus more on melee tactics. A useful concept here is communication overhead. Any team member that is not waiting and spotting for everybody, but acts, e.g., melees or moves or manages items, slows down all other team members by roughly 10%, because they need to keep track of his actions. Therefore, if other heroes melee, consider carefully if it makes sense to come to their aid, slowing them while you move, or if it's better to stay put and monitor the perimeter. This is true for all factions and all actors on each level separately, except the pointman of each faction, if it has one."  -- this also eliminates lag in big battles and helps the player to focus on combat and not get distracted by distant team members frantically trying to reach the battleground in time
  }

flight = ModeKind  -- asymmetric ranged and stealth race at night
  { mname   = "night flight (5)"
  , mfreq   = [(NIGHT, 1), (FLIGHT, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterFlight
  , mcaves  = cavesFlight
  , mendMsg = [ (Killed, killedMsg)
              , (Conquer, "It was enough to reach the escape area marked by yellow '>' symbol. Spilling that much blood was risky. unnecessary and alerted the authorities. Having said that --- impressive indeed.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Three heroes vs. Seven human enemies capable of concurrent attacks"
      , "* Minimize losses"
      , "* Gather gems"
      , "* Find a way out and escape ASAP"
      ]
  , mdesc   = "Dwelling into dark matters is dangerous, so avoid the crowd of firebrand disputants, catch any gems of thought, find a way out and bring back a larger team to shed new light on the field."
  , mreason = "The focus of this installment is on stealthy exploration under the threat of numerically superior enemy."
  , mhint   = ""
  }
 where
   killedMsg = T.intercalate "\n"
     [ "Somebody must have tipped the enemies of free inquiry off. However, us walking along a lit trail, yelling, could have been a contributing factor. Also, it's worth noting that the torches prepared for this assault are best used as thrown makeshift flares."
     , "On the other hand, equipping a lit torch makes one visible in the dark, regrettably but not quite unexpectedly to a scientific mind. Lastly, the goal of this foray was to definitely disengage from the fruitless dispute, via a way out marked by a yellow '>' sign, and to gather treasure that would support our future research. Not to harass every nearby scientific truth denier, as much as they do deserve it."
     ]

zoo = ModeKind  -- asymmetric crowd melee at night
  { mname   = "burning zoo (6)"
  , mfreq   = [(BURNING, 1), (ZOO, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mendMsg = [ (Killed, "Against such an onslaught, only clever positioning, use of terrain and patient vigilance gives any chance of survival.")
              , (Conquer, "That was a grim harvest. Science demands sacrifices.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Five heroes vs. Many enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "The heat of the dispute reaches the nearby Wonders of Science and Nature exhibition, igniting greenery, nets and cages. Crazed animals must be dissuaded from ruining precious scientific equipment and setting back the otherwise fruitful exchange of ideas."
  , mreason = "This is a crowd control exercise, at night, with a raging fire."
  , mhint   = "Note that communication overhead, as explained in perilous hunt adventure hints, makes it impossible for any faction to hit your heroes by more than 10 normal speed actors each turn. However, this is still too much, so position is everything."
  }

-- The tactic is to sneak in the dark, highlight enemy with thrown torches
-- (and douse thrown enemy torches with blankets) and only if this fails,
-- actually scout using extended noctovision.
-- With reaction fire, larger team is more fun.
--
-- For now, while we have no shooters with timeout, massive ranged battles
-- without reaction fire don't make sense, because then usually only one hero
-- shoots (and often also scouts) and others just gather ammo.
ambush = ModeKind  -- dense ranged with reaction fire vs melee at night
  { mname   = "ranged ambush (7)"
  , mfreq   = [(RANGED, 1), (AMBUSH, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mendMsg = [ (Killed, killedMsg)
              , (Conquer, "The new instant telegraphy equipment enabling simultaneous ranged attacks with indirect triangulation and aiming proved effective beyond expectation. Your ideas are safe, your research programme on track, your chartered company ready to launch and introduce progress and science into every household of the nation.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Three heroes with concurrent attacks vs. Unidentified foes"
      , "* Minimize losses"
      , "* Assert control of the situation ASAP"
      ]
  , mdesc   = "Prevent hijacking of your ideas at all cost! Be stealthy, be observant, be aggressive. Fast execution is what makes or breaks a creative team."
  , mreason = "In this adventure, finally, your heroes are able to all use ranged attacks at once, given enough ammunition."
  , mhint   = "Beware of friendly fire, particularly from explosives. But you need no more hints. Go fulfill your destiny! For Science!"
  }
 where
   killedMsg = T.intercalate "\n"
     [ "You turned out to be the prey, this time, not the hunter. In fact, you are not even in the hunters' league. When fighting against such odds, passively waiting for enemy to spring a trap is to no avail, because a professional team can sneak in darkness and ambush the ambushers."
     , "Granted, good positioning is crucial, so that each squad member can overwatch the battlefield and fire opportunistically, using the recently recovered instant telegraphy equipment. However, there is no hope without active scouting, throwing lit objects and probing suspect areas with missiles while paying attention to sounds. And that may still not be enough."
     ]

safari = ModeKind  -- Easter egg available only via screensaver
  { mname   = "safari"
  , mfreq   = [(SAFARI, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mendMsg = []
  , mrules  = T.intercalate "\n"
      [ "* Three levels"
      , "* Many teammates capable of concurrent action vs. Many enemies"
      , "* Minimize losses"
      , "* Find a way out and escape ASAP"
      ]
  , mdesc   = "\"In this enactment you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the bottommost level.\" This is a drama script recovered from a monster nest debris."
  , mreason = "This is an Easter egg. The default squad doctrine is that all team members follow the pointman, but it can be changed from the settings submenu of the main menu."
  , mhint   = ""
  }

-- * Testing modes

dig = ModeKind
  { mname   = "dig"
  , mfreq   = [(DIG, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesDig
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Delve deeper!"
  , mreason = ""
  , mhint   = ""
  }

see = ModeKind
  { mname   = "see"
  , mfreq   = [(SEE, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesSee
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "See all!"
  , mreason = ""
  , mhint   = ""
  }

short = ModeKind
  { mname   = "short"
  , mfreq   = [(SHORT, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesShort
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "See all short scenarios!"
  , mreason = ""
  , mhint   = ""
  }

crawlEmpty = ModeKind
  { mname   = "crawl empty"
  , mfreq   = [(CRAWL_EMPTY, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Enjoy the extra legroom."
  , mreason = ""
  , mhint   = ""
  }

crawlSurvival = ModeKind
  { mname   = "crawl survival"
  , mfreq   = [(CRAWL_SURVIVAL, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlSurvival
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Lure the human intruders deeper and deeper."
  , mreason = ""
  , mhint   = ""
  }

safariSurvival = ModeKind
  { mname   = "safari survival"
  , mfreq   = [(SAFARI_SURVIVAL, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "In this enactment you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  , mreason = ""
  , mhint   = ""
  }

battle = ModeKind
  { mname   = "battle"
  , mfreq   = [(BATTLE, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked against those that unleash the horrors of abstraction."
  , mreason = ""
  , mhint   = ""
  }

battleDefense = ModeKind
  { mname   = "battle defense"
  , mfreq   = [(BATTLE_DEFENSE, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterBattleDefense
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  , mreason = ""
  , mhint   = ""
  }

battleSurvival = ModeKind
  { mname   = "battle survival"
  , mfreq   = [(BATTLE_SURVIVAL, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked for those that ally with the strongest."
  , mreason = ""
  , mhint   = ""
  }

defense = ModeKind  -- perhaps a real scenario in the future
  { mname   = "defense"
  , mfreq   = [(DEFENSE, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterDefense
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Don't let human interlopers defile your abstract secrets and flee unpunished!"
  , mreason = "This is an initial sketch of the reversed crawl game mode. Play on high difficulty to avoid guaranteed victories against the pitiful humans."
  , mhint   = ""
  }

defenseEmpty = ModeKind
  { mname   = "defense empty"
  , mfreq   = [(DEFENSE_EMPTY, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterDefenseEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Lord over empty halls."
  , mreason = ""
  , mhint   = ""
  }

-- * Screensaver modes

screensaverRaid = raid
  { mname   = "auto-raid (1)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

screensaverBrawl = brawl
  { mname   = "auto-brawl (2)"
  , mfreq   = []
  , mattract = True
  }

screensaverCrawl = crawl
  { mname   = "auto-crawl (long)"
  , mfreq   = []
  , mattract = True
  }

screensaverShootout = shootout
  { mname   = "auto-shootout (3)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

screensaverHunt = hunt
  { mname   = "auto-hunt (4)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

screensaverFlight = flight
  { mname   = "auto-flight (5)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

screensaverZoo = zoo
  { mname   = "auto-zoo (6)"
  , mfreq   = []
  , mattract = True
  }

screensaverAmbush = ambush
  { mname   = "auto-ambush (7)"
  , mfreq   = []
  , mattract = True
  }

screensaverSafari = safari
  { mname   = "auto-safari"
  , mfreq   = [(INSERT_COIN, 1)]
  , mattract = True
  }

rosterRaid, rosterBrawl, rosterCrawl, rosterShootout, rosterHunt, rosterFlight, rosterZoo, rosterAmbush, rosterSafari, rosterCrawlEmpty, rosterCrawlSurvival, rosterSafariSurvival, rosterBattle, rosterBattleDefense, rosterBattleSurvival, rosterDefense, rosterDefenseEmpty :: Roster

rosterRaid =
  [ ( ANIMAL_REPRESENTATIVE  -- starting over escape
    , [(-2, 2, ANIMAL)] )
  , ( EXPLORER_SHORT
    , [(-2, 2, HERO)] )
  , ( COMPETITOR_SHORT
    , [(-2, 1, HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]  -- for summoned monsters

rosterBrawl =
  [ ( EXPLORER_NO_ESCAPE
    , [(-2, 3, BRAWLER_HERO)] )
  , ( COMPETITOR_NO_ESCAPE
    , [(-2, 3, BRAWLER_HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]

rosterCrawl =
  [ ( ANIMAL_REPRESENTATIVE  -- starting over escape
    , -- Fun from the start to avoid empty initial level:
      [ (-1, 1 + 1 `d` 2, ANIMAL)
      -- Huge battle at the end:
      , (-10, 100, MOBILE_ANIMAL) ] )
  , ( EXPLORER_REPRESENTATIVE
        -- start on stairs so that stash is handy
    , [(-1, 3, HERO)] )
  , ( MONSTER_REPRESENTATIVE
    , [(-4, 1, SCOUT_MONSTER), (-4, 3, MONSTER)] ) ]

-- Exactly one scout gets a sight boost, to help the aggressor, because he uses
-- the scout for initial attack, while camper (on big enough maps)
-- can't guess where the attack would come and so can't position his single
-- scout to counter the stealthy advance.
rosterShootout =
  [ ( EXPLORER_NO_ESCAPE
    , [(-5, 2, RANGER_HERO), (-5, 1, SCOUT_HERO)] )
  , ( COMPETITOR_NO_ESCAPE
    , [(-5, 2, RANGER_HERO), (-5, 1, SCOUT_HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]

rosterHunt =
  [ ( EXPLORER_NO_ESCAPE
    , [(-6, 7, SOLDIER_HERO)] )
  , ( COMPETITOR_NO_ESCAPE
    , [(-6, 6, AMBUSHER_HERO), (-6, 1, SCOUT_HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]

rosterFlight =
  [ ( COMPETITOR_NO_ESCAPE  -- start on escape
    , [(-7, 6, AMBUSHER_HERO), (-7, 1, SCOUT_HERO)] )
  , ( EXPLORER_MEDIUM
    , [(-7, 2, ESCAPIST_HERO), (-7, 1, SCOUT_HERO)] )
      -- second on the list to let foes occupy the escape
  , (HORROR_REPRESENTATIVE, []) ]

rosterZoo =
  [ ( EXPLORER_TRAPPED
    , [(-8, 5, SOLDIER_HERO)] )
  , ( ANIMAL_CAPTIVE
    , [(-8, 100, MOBILE_ANIMAL)] )
  , (HORROR_REPRESENTATIVE, []) ]  -- for summoned monsters

rosterAmbush =
  [ ( EXPLORER_NO_ESCAPE
    , [(-9, 5, AMBUSHER_HERO), (-9, 1, SCOUT_HERO)] )
  , ( COMPETITOR_NO_ESCAPE
    , [(-9, 12, SOLDIER_HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]

-- No horrors faction needed, because spawned heroes land in civilian faction.
rosterSafari =
  [ ( MONSTER_TOURIST
    , [(-4, 15, MONSTER)] )
  , ( CONVICT_REPRESENTATIVE
    , [(-4, 2, CIVILIAN)] )
  , ( ANIMAL_MAGNIFICENT
    , [(-7, 15, MOBILE_ANIMAL)] )
  , ( ANIMAL_EXQUISITE  -- start on escape
    , [(-10, 20, MOBILE_ANIMAL)] ) ]

rosterCrawlEmpty =
  [ ( EXPLORER_PACIFIST
    , [(-1, 1, HERO)] )
  , (HORROR_PACIFIST, []) ]
      -- for spawned and summoned monsters

rosterCrawlSurvival =
  [ ( EXPLORER_AUTOMATED
    , [(-1, 3, HERO)] )
  , ( MONSTER_REPRESENTATIVE
    , [(-4, 1, SCOUT_MONSTER), (-4, 3, MONSTER)] )
  , ( ANIMAL_NARRATING
    , [(-5, 10, ANIMAL)] ) ]  -- explore unopposed for some time

rosterSafariSurvival =
  [ ( MONSTER_TOURIST_PASSIVE
    , [(-4, 15, MONSTER)] )
  , ( CONVICT_REPRESENTATIVE
    , [(-4, 3, CIVILIAN)] )
  , ( ANIMAL_MAGNIFICENT_NARRATING
    , [(-7, 20, MOBILE_ANIMAL)] )
  , ( ANIMAL_EXQUISITE
    , [(-10, 30, MOBILE_ANIMAL)] ) ]

rosterBattle =
  [ ( EXPLORER_TRAPPED
    , [(-5, 5, SOLDIER_HERO)] )
  , ( MONSTER_CAPTIVE
    , [(-5, 35, MOBILE_MONSTER)] )
  , ( ANIMAL_CAPTIVE
    , [(-5, 30, MOBILE_ANIMAL)] ) ]

rosterBattleDefense =
  [ ( EXPLORER_AUTOMATED_TRAPPED
    , [(-5, 5, SOLDIER_HERO)] )
  , ( MONSTER_CAPTIVE_NARRATING
    , [(-5, 35, MOBILE_MONSTER)] )
  , ( ANIMAL_CAPTIVE
    , [(-5, 30, MOBILE_ANIMAL)] ) ]

rosterBattleSurvival =
  [ ( EXPLORER_AUTOMATED_TRAPPED
    , [(-5, 5, SOLDIER_HERO)] )
  , ( MONSTER_CAPTIVE
    , [(-5, 35, MOBILE_MONSTER)] )
  , ( ANIMAL_CAPTIVE_NARRATING
    , [(-5, 30, MOBILE_ANIMAL)] ) ]

rosterDefense =
  [ ( EXPLORER_AUTOMATED
    , [(-1, 3, HERO)] )
  , ( MONSTER_ANTI
    , [(-4, 1, SCOUT_MONSTER), (-4, 3, MONSTER)] )
  , ( ANIMAL_REPRESENTATIVE
    , [ (-1, 1 + 1 `d` 2, ANIMAL)
      , (-10, 100, MOBILE_ANIMAL) ] ) ]

rosterDefenseEmpty =
  [ ( MONSTER_ANTI_PACIFIST
    , [(-4, 1, SCOUT_MONSTER)] )
  , (HORROR_PACIFIST, []) ]
      -- for spawned and summoned animals

cavesRaid, cavesBrawl, cavesCrawl, cavesShootout, cavesHunt, cavesFlight, cavesZoo, cavesAmbush, cavesSafari, cavesDig, cavesSee, cavesShort, cavesCrawlEmpty, cavesBattle :: Caves

cavesRaid = [([-2], [CAVE_RAID])]

cavesBrawl = [([-2], [CAVE_BRAWL])]

listCrawl :: [([Int], [GroupName CaveKind])]
listCrawl =
  [ ([-1], [CAVE_OUTERMOST])
  , ([-2], [CAVE_SHALLOW_ROGUE])
  , ([-3], [CAVE_EMPTY])
  , ([-4, -5, -6], [DEFAULT_RANDOM, CAVE_ROGUE, CAVE_ARENA])
  , ([-7, -8], [CAVE_ROGUE, CAVE_SMOKING])
  , ([-9], [CAVE_LABORATORY])
  , ([-10], [CAVE_MINE]) ]

cavesCrawl = listCrawl

cavesShootout = [([-5], [CAVE_SHOOTOUT])]

cavesHunt = [([-6], [CAVE_HUNT])]

cavesFlight = [([-7], [CAVE_FLIGHT])]

cavesZoo = [([-8], [CAVE_ZOO])]

cavesAmbush = [([-9], [CAVE_AMBUSH])]

cavesSafari = [ ([-4], [CAVE_SAFARI_1])
              , ([-7], [CAVE_SAFARI_2])
              , ([-10], [CAVE_SAFARI_3]) ]

cavesDig = concat $ zipWith (map . renumberCaves)
                            [0, -10 ..]
                            (replicate 100 listCrawl)

renumberCaves :: Int -> ([Int], [GroupName CaveKind])
              -> ([Int], [GroupName CaveKind])
renumberCaves offset (ns, l) = (map (+ offset) ns, l)

cavesSee = let numberCaves n c = ([n], [c])
           in zipWith numberCaves [-1, -2 ..]
              $ concatMap (replicate 10) allCaves

cavesShort = let numberCaves n c = ([n], [c])
             in zipWith numberCaves [-1, -2 ..]
                $ concatMap (replicate 100) $ take 7 allCaves

allCaves :: [GroupName CaveKind]
allCaves =
  [ CAVE_RAID, CAVE_BRAWL, CAVE_SHOOTOUT, CAVE_HUNT, CAVE_FLIGHT, CAVE_ZOO
  , CAVE_AMBUSH
  , CAVE_ROGUE, CAVE_LABORATORY, CAVE_EMPTY, CAVE_ARENA, CAVE_SMOKING
  , CAVE_NOISE, CAVE_MINE ]

cavesCrawlEmpty = cavesCrawl

cavesBattle = [([-5], [CAVE_BATTLE])]
