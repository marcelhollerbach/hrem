module Rectangle (Rectangle (Rectangle), rectCreate, rectCreate2, rectContains, rectColour) where
import Data.Colour
import Point

--
data Rectangle = Rectangle { start, dimension :: Point, c :: AlphaColour Double }

instance Eq Rectangle where
    (==) (Rectangle p1 d1 c1)(Rectangle p2 d2 c2)
        | p1 == p2 && d1 == d2 && c1 == c2 = True
        | otherwise = False

rectCreate :: Point -> Point -> AlphaColour Double -> Rectangle
rectCreate s d c = (Rectangle s d c)

rectCreate2 :: Integer -> Integer -> Integer -> Integer -> AlphaColour Double -> Rectangle
rectCreate2 x y w h c = rectCreate (pointCreate x y) (pointCreate w h) c

rectContains :: Rectangle -> Point -> Bool
rectContains (Rectangle start dim c) p = (p >= start && p <= dim + start)

rectColour :: Rectangle -> AlphaColour Double
rectColour (Rectangle p d c) = c