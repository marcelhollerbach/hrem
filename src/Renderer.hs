{-# LANGUAGE MultiParamTypeClasses #-}
module Renderer where
import Shape
import Point

noTransform :: Point -> Point
noTransform p = p

noTransformShape :: Shape a => a -> Primitiv a
noTransformShape s = Primitiv noTransform s

-- graphical primitiv
data Shape a => Primitiv a = Primitiv {coordTransform :: Point -> Point, shape :: a}

-- set of objects which are making a object renderable
class (Renderer frame, Shape shape) => RenderObject frame shape where
    render :: Shape shape => frame -> (Point -> Point) -> shape -> frame

-- Set of function which needs to be set by a renderer
class Renderer f where
    -- returns a empty frame with no primitives
    emptyFrame :: Integer -> Integer -> f

    -- renders a single primitiv onto the given frame, returning the new
    renderFramePrimitiv :: RenderObject f a => f -> Primitiv a -> f
    renderFramePrimitiv frame (Primitiv trans shape) = render frame trans shape

    --renders a list of primitives onto a the given frame
    renderFramePrimitives :: RenderObject f a => f -> [Primitiv a] -> f
    renderFramePrimitives frame p = foldl renderFramePrimitiv frame p