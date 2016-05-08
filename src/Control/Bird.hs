module Control.Bird where

import           Control.Lens
import qualified Data.Dequeue   as DQ
import           Data.Images    (birdImage1, birdImage2, birdImage3)
import           Data.World
import           Graphics.Gloss (Picture)
import           Prelude

imagesToCycleForBird
  :: DQ.BankersDequeue Picture
imagesToCycleForBird = foldl' DQ.pushFront DQ.empty [birdImage1, birdImage2, birdImage3]

currentBirdImage
  :: World
  -> Picture
currentBirdImage =
  view $ birdImages . to DQ.first . to dropMaybeThisIsNeverEmptyIKnowIt

cycleImage
  :: World
  -> World
cycleImage world =
  let Just (currentImage, q) = DQ.popFront $ world ^. birdImages
      updatedImages = DQ.pushBack q currentImage
  in world & birdImages .~ updatedImages
