module Point(Point, pointDistance, pointCreate) where

--
data Point = Point {x, y :: Integer} deriving (Show)

pointCreate:: Integer -> Integer -> Point
pointCreate x y = (Point x y)

pointDistance :: Point -> Double
pointDistance (Point x y) = sqrt ((fromIntegral x^2) + (fromIntegral y^2))

instance Eq Point where
    (==) (Point x1 y1) (Point x2 y2)
        | x1 == x2 && y1 == y2 = True
        | otherwise = False

instance Ord Point where
    (<) (Point x1 y1) (Point x2 y2) = x1 < x2 && y1 < y2
    (<=) (Point x1 y1) (Point x2 y2) = x1 <= x2 && y1 <= y2
    (>) (Point x1 y1) (Point x2 y2) = x1 > x2 && y1 > y2
    (>=) (Point x1 y1) (Point x2 y2) = x1 >= x2 && y1 >= y2

instance Num Point where
    (+) (Point x1 y1) (Point x2 y2) = (Point (x1+x2) (y1+y2))
    (*) (Point x1 y1) (Point x2 y2) = (Point (x1*x2) (y1*y2))
    (-) (Point x1 y1) (Point x2 y2) = (Point (x1-x2) (y1-y2))
    abs (Point x y) = (Point (abs(x)) (abs(y)))
    signum (Point x y) = (Point (signum(x)) (signum(y)))
    fromInteger x = (Point x 0)
