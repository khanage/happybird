module Data.Images
       ( birdImage1
       , birdImage2
       , birdImage3

       , backgroundImage

       , groundImage

       , lowerPipeImage
       , upperPipeImage) where

import           Codec.Picture        (DynamicImage, generateImage, pixelAt,
                                       readImage)
import           Codec.Picture.Types  (dynamicPixelMap)
import           Graphics.Gloss
import           Graphics.Gloss.Juicy (fromDynamicImage)
import           Prelude
import           System.IO.Unsafe     (unsafePerformIO)

birdImage1, birdImage2, birdImage3 :: Picture
birdImage1 = loadBirdImage 522 172
birdImage2 = loadBirdImage 522 124
birdImage3 = loadBirdImage 441 243

loadBirdImage
  :: Int
  -> Int
  -> Picture
loadBirdImage x y =
  loadFromSprite (x, y) (42, 35)

groundImage
  :: Picture
groundImage =
  loadFromSprite (290, 0) (311, 111)

upperPipeImage
  :: Picture
upperPipeImage =
  loadFromSprite (600, 0) (58, 271)

lowerPipeImage
  :: Picture
lowerPipeImage =
  loadFromSprite (655, 0) (58, 240)

backgroundImage
  :: Picture
backgroundImage =
  loadFromSprite (0, 0) (287, 511)

{-# NOINLINE sprite #-}
sprite
  :: DynamicImage
sprite =
  let imagePath = "resources/sprite.png"
  in unsafePerformIO $ do
    eitherErrorOrImage <- readImage imagePath
    case eitherErrorOrImage of
      Left err          -> fail $ "Failed to load the image " <> imagePath <> ": " <> err
      Right loadedImage -> pure loadedImage

loadFromSprite
  :: (Int, Int)
  -> (Int, Int)
  -> Picture
loadFromSprite (startX, startY) (sizeX, sizeY) =
  let Just img = fromDynamicImage $ cropImage sprite (startX, startY) (sizeX, sizeY)
  in img

cropImage
  :: DynamicImage
  -> (Int, Int)
  -> (Int, Int)
  -> DynamicImage
cropImage baseImage (startX, startY) (width, height) =
  let
    generatePixel img x2 y2 =
      pixelAt img (x2 + startX) (y2 + startY)
    createCroppedImage imageWhenceToCrop =
      generateImage (generatePixel imageWhenceToCrop) width height
  in
    dynamicPixelMap createCroppedImage baseImage
