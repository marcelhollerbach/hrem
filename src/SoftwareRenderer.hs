module SoftwareRenderer where
import Point
import Circle
import Rectangle
import qualified Renderer as R
import Shape
import Data.Matrix
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB

data SoftwareBuffer = SoftwareBuffer { w,h :: Integer, frame :: Point -> Colour Double}

swBufferPixel :: SoftwareBuffer -> Integer -> Integer -> Colour Double
swBufferPixel (SoftwareBuffer w h f) x y = f (pointCreate x y)

swBufferMatrix :: SoftwareBuffer -> Matrix (Colour Double)
swBufferMatrix (SoftwareBuffer w h f)= matrix (fromInteger w) (fromInteger h) matrixGen where
    matrixGen :: (Int,Int) -> Colour Double
    matrixGen (x,y) = f (pointCreate (toInteger x) (toInteger y))

renderRect :: (Point -> (Colour Double)) -> (Point -> Point) -> Rectangle -> Point -> Colour Double
renderRect frame trans rect = newRect where
    newRect :: Point -> Colour Double
    newRect pos
        | contains rect ((getMiddle rect) + (trans $ relativPos rect pos)) = over (getColour rect) (frame pos)
        | otherwise = frame pos

renderCircle :: (Point -> (Colour Double)) -> (Point -> Point) -> Circle -> Point -> Colour Double
renderCircle frame trans circle = newCircle where
    newCircle :: Point -> Colour Double
    newCircle pos
        | contains circle ((getMiddle circle) + (trans $ relativPos circle pos)) = over (getColour circle) (frame pos)
        | otherwise = frame pos

instance R.Renderer SoftwareBuffer where
    emptyFrame w h = (SoftwareBuffer w h a) where
        a :: Point -> Colour Double
        a p = white
    renderFramePrimitiv (SoftwareBuffer w h frame) (R.Primitiv transform shape) =
        case shape of
            (R.Rect rect) -> (SoftwareBuffer w h (renderRect frame transform rect))
            (R.Circle circle) -> (SoftwareBuffer w h (renderCircle frame transform circle))