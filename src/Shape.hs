module Shape where
import Point
import Data.Colour

class Shape f where
    relativPos :: f -> Point -> Point
    relativPos shape pos = pos - getMiddle shape
    getMiddle :: f -> Point
    getColour :: f -> AlphaColour Double
    contains :: f -> Point -> Bool