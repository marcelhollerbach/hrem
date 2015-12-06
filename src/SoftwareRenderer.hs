module SoftwareRenderer(SoftwareBuffer, swBufferPixel, swBufferMatrix) where
import Point
import Circle
import Rectangle
import Renderer
import Data.Matrix
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB

data SoftwareBuffer = SoftwareBuffer { w,h :: Integer, f :: Point -> Colour Double}

swBufferPixel :: SoftwareBuffer -> Integer -> Integer -> Colour Double
swBufferPixel (SoftwareBuffer w h f) x y = f (pointCreate x y)

swBufferMatrix :: SoftwareBuffer -> Matrix (Colour Double)
swBufferMatrix (SoftwareBuffer w h f)= matrix (fromInteger w) (fromInteger h) matrixGen where
    matrixGen :: (Int,Int) -> Colour Double
    matrixGen (x,y) = f (pointCreate (toInteger x) (toInteger y))

renderRect :: (Point -> (Colour Double)) -> Rectangle -> Point -> Colour Double
renderRect f r = newRect where
    newRect :: Point -> Colour Double
    newRect p
        | rectContains r p = over (rectColour r) (f p)
        | otherwise = f p

renderCircle :: (Point -> (Colour Double)) -> Circle -> Point -> Colour Double
renderCircle f c = newCircle where
    newCircle :: Point -> Colour Double
    newCircle p
        | circleContains c p = over (circleColour c) (f p)
        | otherwise = f p

instance Renderer SoftwareBuffer where
    emptyFrame w h = (SoftwareBuffer w h a) where
        a :: Point -> Colour Double
        a p = white
    renderFramePrimitiv (SoftwareBuffer w h f) p = case p of
        (Rect r) -> (SoftwareBuffer w h (renderRect f r))
        (Circle c) -> (SoftwareBuffer w h (renderCircle f c))