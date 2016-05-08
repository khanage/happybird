module Data.Pipes where

import qualified Data.Dequeue as DQ
import           Prelude

newtype Pipes = Pipes { unPipes :: [(Float, Float)] }
              deriving (Eq, Show)
