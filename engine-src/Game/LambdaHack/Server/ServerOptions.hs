-- | Server and client game state types and operations.
module Game.LambdaHack.Server.ServerOptions
  ( ServerOptions(..), RNGs(..), defServerOptions
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified System.Random.SplitMix32 as SM

import Game.LambdaHack.Client (ClientOptions (..), defClientOptions)
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Content.ModeKind

-- | Options that affect the behaviour of the server (including game rules).
data ServerOptions = ServerOptions
  { sknowMap         :: Bool
  , sknowEvents      :: Bool
  , sknowItems       :: Bool
  , sniff            :: Bool
  , sallClear        :: Bool
  , sboostRandomItem :: Bool
  , sgameMode        :: Maybe (GroupName ModeKind)
  , sautomateAll     :: Bool
  , skeepAutomated   :: Bool
  , sdungeonRng      :: Maybe SM.SMGen
  , smainRng         :: Maybe SM.SMGen
  , snewGameSer      :: Bool
  , scurChalSer      :: Challenge
  , sdumpInitRngs    :: Bool
  , ssavePrefixSer   :: String
  , sdbgMsgSer       :: Bool
  , sshowItemSamples :: Bool
  , sclientOptions   :: ClientOptions
      -- The client debug inside server debug only holds the client commandline
      -- options and is never updated with config options, etc.
  }
  deriving Show

data RNGs = RNGs
  { dungeonRandomGenerator  :: Maybe SM.SMGen
  , startingRandomGenerator :: Maybe SM.SMGen
  }

instance Show RNGs where
  show RNGs{..} =
    let args = [ maybe "" (\gen -> "--setDungeonRng \"" ++ show gen ++ "\"")
                       dungeonRandomGenerator
               , maybe "" (\gen -> "--setMainRng \"" ++ show gen ++ "\"")
                       startingRandomGenerator ]
    in unwords args

instance Binary ServerOptions where
  put ServerOptions{..} = do
    put sknowMap
    put sknowEvents
    put sknowItems
    put sniff
    put sallClear
    put sboostRandomItem
    put sgameMode
    put sautomateAll
    put skeepAutomated
    put scurChalSer
    put ssavePrefixSer
    put sdbgMsgSer
    put sshowItemSamples
    put sclientOptions
  get = do
    sknowMap <- get
    sknowEvents <- get
    sknowItems <- get
    sniff <- get
    sallClear <- get
    sboostRandomItem <- get
    sgameMode <- get
    sautomateAll <- get
    skeepAutomated <- get
    scurChalSer <- get
    ssavePrefixSer <- get
    sdbgMsgSer <- get
    sshowItemSamples <- get
    sclientOptions <- get
    let sdungeonRng = Nothing
        smainRng = Nothing
        snewGameSer = False
        sdumpInitRngs = False
    return $! ServerOptions{..}

instance Binary RNGs where
  put RNGs{..} = do
    put (show dungeonRandomGenerator)
    put (show startingRandomGenerator)
  get = do
    dg <- get
    sg <- get
    let dungeonRandomGenerator = read dg
        startingRandomGenerator = read sg
    return $! RNGs{..}

-- | Default value of server options.
defServerOptions :: ServerOptions
defServerOptions = ServerOptions
  { sknowMap = False
  , sknowEvents = False
  , sknowItems = False
  , sniff = False
  , sallClear = False
  , sboostRandomItem = False
  , sgameMode = Nothing
  , sautomateAll = False
  , skeepAutomated = False
  , sdungeonRng = Nothing
  , smainRng = Nothing
  , snewGameSer = False
  , scurChalSer = defaultChallenge
  , sdumpInitRngs = False
  , ssavePrefixSer = ""
  , sdbgMsgSer = False
  , sshowItemSamples = False
  , sclientOptions = defClientOptions
  }
