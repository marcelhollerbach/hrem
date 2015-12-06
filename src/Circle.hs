module Circle(Circle, circleCreate2, circleCreate, circleContains, circleColour) where
import Point
import Data.Colour

data Circle = Circle { middle :: Point, radius :: Integer, color :: AlphaColour Double}

circleCreate :: Point -> Integer -> AlphaColour Double-> Circle
circleCreate p r c = (Circle p r c)

circleCreate2 :: Integer -> Integer -> Integer -> AlphaColour Double -> Circle
circleCreate2 x y r c = (Circle (pointCreate x y) r c)

circleColour :: Circle -> AlphaColour Double
circleColour (Circle p r c) = c

circleContains :: Circle -> Point -> Bool
circleContains (Circle middle radius color) p = pointDistance(abs(middle - p)) <= fromInteger radius