module Renderer where
import Rectangle
import Circle
import Point

noTransform :: Point -> Point
noTransform p = p

noTransformShape :: Shape -> Primitiv
noTransformShape s = Primitiv noTransform s

-- a graphical primitiv
data Shape = Rect Rectangle | Circle Circle

-- graphical primitiv
data Primitiv = Primitiv {coordTransform :: Point -> Point, shape :: Shape}

-- Set of function which needs to be set by a renderer
class Renderer f where
    -- returns a empty frame with no primitives
    emptyFrame :: Integer -> Integer -> f

    -- renders a single primitiv onto the given frame, returning the new
    renderFramePrimitiv :: f -> Primitiv -> f

    --renders a list of primitives onto a the given frame
    renderFramePrimitives :: f -> [Primitiv] -> f
    renderFramePrimitives frame p = foldl renderFramePrimitiv frame p