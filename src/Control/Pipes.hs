module Control.Pipes where

import           Control.Lens
import           Data.Pipes
import           Graphics.Gloss (Picture)
import           Prelude
import           System.Random  (StdGen)

newtype Delay = Delay {unDelay :: Float}
                deriving (Show, Eq)

generatePipes
  :: Delay
  -> StdGen
  -> Pipes
generatePipes delay randomGen =
  Pipes [(100, -200), (400, -300)]
