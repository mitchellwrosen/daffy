module Daffy.Svg exposing (Transform, transform, translate, translateY)

import Svg exposing (Attribute)
import Svg.Attributes


{-| An SVG transformation.
-}
type Transform
    = Translate Float Float


render : Transform -> String
render x =
    case x of
        Translate x y ->
            "translate(" ++ toString x ++ " " ++ toString y ++ ")"


{-| Render a list of 'Transform' as an SVG "transform" attribute.
-}
transform : List Transform -> Attribute msg
transform =
    Svg.Attributes.transform << List.foldl (\x s -> render x ++ " " ++ s) ""


{-| `translate(x y)`
-}
translate : Float -> Float -> Transform
translate =
    Translate


{-| `translate(0 y)`
-}
translateY : Float -> Transform
translateY =
    Translate 0
