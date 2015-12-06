module Shape where
import Point
import Data.Colour

-- abstraktion on operations on a shape
class Shape f where
    -- returns the given coords relative to the middle of the given shape
    relativPos :: f -> Point -> Point
    relativPos shape pos = pos - getMiddle shape
    -- get the middle of the passed shape
    getMiddle :: f -> Point
    -- get Colour of the shape
    getColour :: f -> AlphaColour Double
    -- check if the shape contains the given point
    contains :: f -> Point -> Bool