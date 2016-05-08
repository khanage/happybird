module Control.Pipes where

import           Control.Lens
import           Data.List      (unfoldr)
import           Data.Pipes
import           Graphics.Gloss (Picture)
import           Prelude
import           System.Random  (Random, RandomGen, StdGen, randomR)

newtype Delay = Delay {unDelay :: Float}
                deriving (Show, Eq)

generatePipes
  :: Delay
  -> StdGen
  -> Pipes
generatePipes delay randomGen =
  let
    (heights, newGen) =
      randomRSI (-200 :: Float, -500) randomGen 7
  in Pipes newGen $ zip [100::Float,300..] heights

randomRSI
  :: (Random a, RandomGen g)
  => (a, a)
  -> g
  -> Int
  -> ([a], g)
randomRSI =
  step []
  where
    step :: (Random a, RandomGen g)
      => [a] -> (a, a) -> g -> Int -> ([a], g)
    step acc range gen i
      | i <= 0 =
        (acc, gen)
      | i > 0 =
        let (a, newGen) = randomR range gen
        in step (a:acc) range newGen (pred i)
