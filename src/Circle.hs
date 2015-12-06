module Circle where
import Point
import Shape
import Data.Colour

data Circle = Circle { middle :: Point, radius :: Integer, color :: AlphaColour Double}

instance Shape Circle where
    getMiddle (Circle middle _ _) = middle
    getColour (Circle _ _ c) = c
    contains (Circle middle radius _) p = pointDistance(abs(middle - p)) <= fromInteger radius

circleCreate :: Point -> Integer -> AlphaColour Double-> Circle
circleCreate p r c = (Circle p r c)

circleCreate2 :: Integer -> Integer -> Integer -> AlphaColour Double -> Circle
circleCreate2 x y r c = (Circle (pointCreate x y) r c)

circleColour :: Circle -> AlphaColour Double
circleColour (Circle p r c) = c