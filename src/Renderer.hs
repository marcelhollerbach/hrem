module Renderer(Renderer, emptyFrame, renderFramePrimitiv, renderFramePrimitives, Primitiv(Rect, Circle)) where
import Rectangle
import Circle

-- graphical primitiv
data Primitiv = Rect Rectangle | Circle Circle
class Renderer f where
    emptyFrame :: Integer -> Integer -> f
    renderFramePrimitiv :: f -> Primitiv -> f
    renderFramePrimitives :: f -> [Primitiv] -> f
    renderFramePrimitives frame p = foldl renderFramePrimitiv frame p