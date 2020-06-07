module Ellipses where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics

simpleEllipsePic :: Float -> Picture
simpleEllipsePic n = map greenEllipse [0, pi/n .. (n-1)*pi/n]
  where
    centre :: Point
    centre = Point 400 400

    greenEllipse :: Float -> PictureObject
    greenEllipse angle = Ellipse centre 250 70 angle (colourFor angle) Solid SolidFill

    colourFor angle = let x = round (255 * angle / pi)
                      in Colour (255 - x) 128 x 84



writeToFile pic
  = writePng "output.png" 
         (drawPicture 3 pic)

