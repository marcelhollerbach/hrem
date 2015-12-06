module Rectangle where
import Data.Colour
import Point
import Shape

--
data Rectangle = Rectangle { start, dimension :: Point, c :: AlphaColour Double }

instance Eq Rectangle where
    (==) (Rectangle p1 d1 c1)(Rectangle p2 d2 c2)
        | p1 == p2 && d1 == d2 && c1 == c2 = True
        | otherwise = False

instance Shape Rectangle where
    getMiddle (Rectangle (Point x y) dim _) = pointCreate (div x 2) (div y 2)
    getColour (Rectangle _ _ c) = c
    contains (Rectangle start dim _) p = (p >= start && p <= dim + start)

rectCreate :: Point -> Point -> AlphaColour Double -> Rectangle
rectCreate s d c = (Rectangle s d c)

rectCreate2 :: Integer -> Integer -> Integer -> Integer -> AlphaColour Double -> Rectangle
rectCreate2 x y w h c = rectCreate (pointCreate x y) (pointCreate w h) c