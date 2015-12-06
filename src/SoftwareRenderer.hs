{-# LANGUAGE MultiParamTypeClasses #-}
module SoftwareRenderer where
import Point
import Circle
import Rectangle
import Renderer
import Shape
import Data.Matrix
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB

data SoftwareBuffer = SoftwareBuffer { w,h :: Integer, frame :: Point -> Colour Double}

swBufferMatrix :: SoftwareBuffer -> Matrix (Colour Double)
swBufferMatrix (SoftwareBuffer w h f)= matrix (fromInteger w) (fromInteger h) matrixGen where
    matrixGen :: (Int,Int) -> Colour Double
    matrixGen (x,y) = f (pointCreate (toInteger x) (toInteger y))

instance RenderObject SoftwareBuffer Rectangle where
    render (SoftwareBuffer w h frame) trans rect = (SoftwareBuffer w h newRect) where
        newRect :: Point -> Colour Double
        newRect pos
            | contains rect ((getMiddle rect) + (trans $ relativPos rect pos)) = over (getColour rect) (frame pos)
            | otherwise = frame pos

instance RenderObject SoftwareBuffer Circle where
    render (SoftwareBuffer w h frame) trans circle = (SoftwareBuffer w h newCircle) where
        newCircle :: Point -> Colour Double
        newCircle pos
            | contains circle ((getMiddle circle) + (trans $ relativPos circle pos)) = over (getColour circle) (frame pos)
            | otherwise = frame pos

instance Renderer SoftwareBuffer where
    emptyFrame w h = (SoftwareBuffer w h a) where
        a :: Point -> Colour Double
        a p = white