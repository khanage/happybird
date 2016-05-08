{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Bird
import           Control.Lens
import           Control.Pipes       (Delay (..), generatePipes)
import           Control.Render
import           Control.Update
import           Data.Pipes          (Pipes)
import           Data.World
import           Graphics.Gloss
import           Graphics.Gloss.Game hiding (play)
import           Linear.V2           (V2 (..), _x, _y)
import           Prelude             hiding (Down)
import           System.Random       (getStdGen)

-- Game methods
handleEvent
  :: Event
  -> World
  -> World
handleEvent event world@World{..} =
  case event of
    EventKey (SpecialKey KeySpace) Down _ _ ->
      world & birdVelocity . _y .~ (world ^. jumpValue)
    _ ->
      world

main
  :: IO ()
main = do
  randomGen <- getStdGen

  let
    screenSize = (574, 800)
    windowDisplay = InWindow "Happy Bird!" screenSize (10, 10)
    background = white
    stepsPerSecond = 60
    cycleBirdImageEvery = 0.3

    initWorld = World
      { _birdPos = V2 0 80
      , _birdVelocity = V2 0 110

      , _screenBounds = bimap fromIntegral fromIntegral screenSize
      , _gravity = V2 0 (-2.5)
      , _jumpValue = 150

      , _gameOver = False
      , _birdImages = imagesToCycleForBird
      , _birdImageCycleThreshold = cycleBirdImageEvery
      , _birdImageLastCycled = 0

      , _pipeLocations = generatePipes (Delay 100) randomGen

      , _horizontalSpeed = 50
      }

    run = play windowDisplay background stepsPerSecond initWorld

  run drawAllElements handleEvent updateWorld
