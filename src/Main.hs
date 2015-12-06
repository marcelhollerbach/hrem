module Main where
import Circle
import Point
import Rectangle
import Renderer
import SoftwareRenderer
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Matrix
import Codec.Picture

frame :: SoftwareBuffer;
frame = emptyFrame 1000 1000

debuglist :: [Primitiv]

debuglist = [Rect (rectCreate2  0  0 200 200 (opaque black)),
             Rect (rectCreate2 200 200 200 200 (opaque red)),
             Rect (rectCreate2 400 400 200 200 (opaque green)),
             Circle (circleCreate2 400 400 200 (opaque blue))]

maxn :: Double
maxn = 255

c :: Double -> Pixel8
c d = fromInteger $ round d

-- converts SRGB with values between 0 and 1 to PixelRGB 8 with values between 0 and 256
pixelConvert :: RGB Double -> PixelRGB8
pixelConvert (RGB r g b)= PixelRGB8 (c (r* maxn)) (c (g* maxn)) (c (b* maxn))

outputMatrix = swBufferMatrix $ renderFramePrimitives frame debuglist
main = writePng "/tmp/lucker.png" $ generateImage converter 1000 1000 where
    converter x y = pixelConvert $ toSRGB $ getElem (x+1) (y+1) outputMatrix