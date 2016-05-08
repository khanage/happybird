{-# LANGUAGE MultiWayIf #-}
module Control.Update where

import           Control.Bird
import           Control.Lens
import           Data.Pipes
import           Data.World
import           Linear.V2
import           Prelude

updateWorld
  :: Float
  -> World
  -> World
updateWorld step world =
    if world^.gameOver
    then world
    else world
       & gravitateBird step
       & animateBird step
       & updatePipes step
       & tidyAndSpawnPipes

gravitateBird
  :: Float
  -> World
  -> World
gravitateBird step world =
  let
    (_, screenY) = world ^. screenBounds

    position = world ^. birdPos
    velocity = (*) (pure step) $ world ^. birdVelocity

    updatedPosition = position + velocity
  in
    world
  & birdVelocity +~ (world ^. gravity)
  & if | updatedPosition ^. _y < negate (screenY/2) ->
           birdVelocity._y %~ (* negate 1)
       | updatedPosition ^. _y > screenY/2 ->
           gameOver .~ True
       | otherwise ->
           birdPos .~ updatedPosition

animateBird
  :: Float
  -> World
  -> World
animateBird step world =
  let
    lastCycledBird = --traceConcat "Last cycled: " $
       world ^. birdImageLastCycled

    shouldCycleBird =
      lastCycledBird > world ^. birdImageCycleThreshold
  in world & birdImageLastCycled +~ step
           & if shouldCycleBird
             then cycleImage . (birdImageLastCycled -~ (world^.birdImageCycleThreshold))
             else id

updatePipes
  :: Float
  -> World
  -> World
updatePipes step world =
  let
    updatePipe
      :: (Float, Float)
      -> (Float, Float)
    updatePipe (x, y) =
      (x - (world ^. horizontalSpeed) * step, y)
  in
    world & pipeLocations . pPipes %~ fmap updatePipe

tidyAndSpawnPipes
   :: World
   -> World
tidyAndSpawnPipes world =
  let
    offScreen
      :: (Float, Float)
      -> Bool
    offScreen (x, _) =
      x > negate 100

    -- go :: (Maybe step, [step]) -> step -> (Maybe step, [step])
    -- go = undefined

    spawnNeccesaryPipes
      :: [(Float, Float)]
      -> [(Float, Float)]
    spawnNeccesaryPipes =
      id
  in world -- & pipeLocations %~ spawnNeccesaryPipes . filter offScreen

