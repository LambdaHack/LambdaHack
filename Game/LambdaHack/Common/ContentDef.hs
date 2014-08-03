-- | A game requires the engine provided by the library, perhaps customized,
-- and game content, defined completely afresh for the particular game.
-- The general type of the content is @ContentDef@ and it has instances
-- for all content kinds, such as items kinds
-- (@Game.LambdaHack.Content.ItemKind@).
-- The possible kinds are fixed in the library and all defined in the same
-- directory. On the other hand, game content, that is all elements
-- of @ContentDef@ instances, are defined in a directory
-- of the game code proper, with names corresponding to their kinds.
module Game.LambdaHack.Common.ContentDef (ContentDef(..)) where

import Data.Text (Text)

import Game.LambdaHack.Common.Misc

-- | The general type of a particular game content, e.g., item kinds.
data ContentDef a = ContentDef
  { getSymbol      :: a -> Char    -- ^ symbol, e.g., to print on the map
  , getName        :: a -> Text    -- ^ name, e.g., to show to the player
  , getFreq        :: a -> Freqs   -- ^ frequency within groups
  , validateSingle :: a -> [Text]
      -- ^ validate a content item and list all offences
  , validateAll    :: [a] -> [Text]
      -- ^ validate the whole defined content of this type and list all offences
  , content        :: [a]          -- ^ all the defined content of this type
  }
