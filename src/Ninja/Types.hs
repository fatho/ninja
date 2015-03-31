module Ninja.Types where

-- | Simple error return type.
data Result = Success | Failure String deriving (Eq, Ord, Show, Read)
