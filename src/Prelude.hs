module Prelude
       ( module ClassyPrelude
       , module Prelude) where

import           ClassyPrelude

dropMaybeThisIsNeverEmptyIKnowIt :: Maybe t -> t
dropMaybeThisIsNeverEmptyIKnowIt (Just a) = a

traceConcat
  :: Show a
  => String
  -> a
  -> a
traceConcat msg a =
  trace (msg <> " " <> show a) a

