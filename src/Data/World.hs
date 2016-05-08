{-# LANGUAGE TemplateHaskell #-}
module Data.World where

import           Control.Lens   (makeLenses)
import qualified Data.Dequeue   as DQ
import           Graphics.Gloss (Picture)
import           Linear.V2      (V2 (..))
import           Prelude
import Data.Pipes                 

data World = World
   { _birdPos                 :: V2 Float
   , _birdVelocity            :: V2 Float

   , _screenBounds            :: (Float, Float)
   , _gravity                 :: V2 Float
   , _jumpValue               :: Float

   , _gameOver                :: Bool

   , _birdImages              :: DQ.BankersDequeue Picture
   , _birdImageLastCycled     :: Float
   , _birdImageCycleThreshold :: Float

   , _horizontalSpeed         :: Float

   , _pipeLocations           :: Pipes
   } deriving (Eq, Show)

makeLenses ''World
