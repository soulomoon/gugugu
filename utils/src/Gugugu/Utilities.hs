{-|
Utilities
 -}
module Gugugu.Utilities
  (
  -- * Utilities to create source file
    module Gugugu.Utilities.SourceUtils

  -- * Utilities for command line options
  , module Gugugu.Utilities.Command

  -- * Others
  , unsafeQuote
  , showText
  , indexed
  ) where

import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Gugugu.Utilities.Command
import           Gugugu.Utilities.SourceUtils


-- | Quote the text with quote mark, without escaping.
unsafeQuote :: Text -> Text
unsafeQuote = let q = '\"' in T.cons q . flip T.snoc q

-- | Like 'show' but returns 'Text' instead of 'String'
showText :: Show a => a -> Text
showText = T.pack . show

-- | Return a new list with indices of elements.
indexed :: [a] -> [(Int, a)]
indexed = zip [0..]
