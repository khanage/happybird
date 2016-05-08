module Control.Render ( drawAllElements )
       where

import           Control.Bird
import           Control.Lens
import           Data.Images    (backgroundImage, groundImage, lowerPipeImage,
                                 upperPipeImage)
import           Data.World
import           Graphics.Gloss (Picture, pictures, scale, translate)
import           Linear.V2
import           Prelude
import Data.Pipes
  
newtype ElementRender = ElementRender { unElementRender :: World -> Picture }

drawBackground
  :: ElementRender
drawBackground =
  ElementRender $ draw backgroundImage 0 0

drawGround
  :: ElementRender
drawGround =
  ElementRender $ draw groundImage 0 (-400)

drawPipes
  :: ElementRender
drawPipes = ElementRender $ \world ->
  pictures . concat $ uncurry (pipes world) <$> world ^. pipeLocations . to unPipes
  where
     pipes world x y =
        [ draw lowerPipeImage x y world
        , draw upperPipeImage x (y + 750) world]

draw
  :: Picture
  -> Float
  -> Float
  -> World
  -> Picture
draw image x y world =
  scalingFor world image `renderAt` V2 x y

drawBird
  :: ElementRender
drawBird = ElementRender $ \world ->
  let
    birdImage = currentBirdImage world
    position = world ^. birdPos
  in scalingFor world $ birdImage `renderAt` position

renderAll
  :: [ElementRender]
  -> World
  -> [Picture]
renderAll renderers world =
  fmap (($ world) . unElementRender) renderers

drawAllElements
  :: World
  -> Picture
drawAllElements =
  pictures . renderAll
      [ drawBackground
      , drawGround
      , drawPipes
      , drawBird
      ]

renderAt p (V2 x y) = translate x y p

scalingFor
  :: World
  -> Picture
  -> Picture
scalingFor _ =
  scale 2 2 -- Just hardcode this for now
