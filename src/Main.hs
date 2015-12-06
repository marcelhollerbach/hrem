module Main where
import Circle
import Point
import Rectangle
import Shape
import Renderer as R
import qualified SoftwareRenderer as S
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Matrix
import Codec.Picture

-- a clean buffer
frame :: S.SoftwareBuffer;
frame = emptyFrame 1000 1000

simpleTransform :: Point -> Point
simpleTransform (Point x y) = pointCreate (x * 2) y

-- debugging list with a bunch of primitives
debuglist :: [Primitiv]
debuglist = (map noTransformShape [R.Rect (rectCreate2  0  0 200 200 (withOpacity black 100)),
                                   R.Rect (rectCreate2 200 200 200 200 (opaque red)),
                                   R.Rect (rectCreate2 400 400 200 200 (opaque yellow))]) ++
            [Primitiv simpleTransform (R.Circle (circleCreate2 400 400 200 (withOpacity blue 50)))]

-- converts a Double to a pixel8 vaue
convert :: Double -> Pixel8
convert d = fromInteger $ round d

-- converts SRGB with values between 0 and 1 to PixelRGB 8 with values between 0 and 256
pixelConvert :: RGB Double -> PixelRGB8
pixelConvert (RGB r g b)= PixelRGB8 (convert (r* 255))
                                    (convert (g* 255))
                                    (convert (b* 255))

outputMatrix = S.swBufferMatrix $ renderFramePrimitives frame debuglist

main = writePng "/tmp/lucker.png" $ generateImage converter 1000 1000 where
   converter x y = pixelConvert $ toSRGB $ getElem (x+1) (y+1) outputMatrix