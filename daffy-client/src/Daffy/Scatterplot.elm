module Daffy.Scatterplot exposing (Config, svg)

import Daffy.Extent as Extent exposing (Extent)
import Daffy.List.Extra as List exposing (when)
import Daffy.Svg
import Lazy exposing (Lazy, force, lazy)
import List.Extra as List exposing (last)
import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))
import Maybe.Extra as Maybe exposing (unwrap)
import Svg exposing (Svg)
import Svg.Attributes
import Visualization.Axis as Axis
import Visualization.Scale as Scale exposing (ContinuousScale)
import Visualization.Shape as Shape


type alias Config a =
    { width : Float
    , height : Float
    , margin :
        { bottom : Float
        , left : Float
        , right : Float
        , top : Float
        }
    , extent : Extent -> Extent
    , line : Bool
    , getX : a -> Float
    , getY : a -> Float
    , getR : a -> Int
    }


svg : Config a -> Nonempty a -> Svg msg
svg { width, height, margin, extent, line, getX, getY, getR } xs =
    let
        { xmin, xmax, ymin, ymax } =
            extent <|
                Extent.bufferY 0.05 <|
                    Extent.extent { getX = getX, getY = getY } xs

        xscale : ContinuousScale
        xscale =
            Scale.linear
                ( xmin, xmax )
                ( 0, width - margin.left - margin.right )

        yscale : ContinuousScale
        yscale =
            Scale.linear
                ( ymin, ymax )
                ( height - margin.bottom - margin.top, 0 )

        xaxis : Svg msg
        xaxis =
            Axis.axis
                { orientation = Axis.Bottom
                , ticks = Nothing
                , tickFormat = Nothing
                , tickCount = 10
                , tickSizeInner = 6
                , tickSizeOuter = 6
                , tickPadding = 3
                }
                xscale

        yaxis : Svg msg
        yaxis =
            Axis.axis
                { orientation = Axis.Left
                , ticks = Nothing
                , tickFormat = Nothing
                , tickCount = 10
                , tickSizeInner = 6
                , tickSizeOuter = 6
                , tickPadding = 3
                }
                yscale

        points : List ( Float, Float, Int )
        points =
            xs
                |> Nonempty.toList
                |> List.map
                    (\x ->
                        ( Scale.convert xscale (getX x)
                        , Scale.convert yscale (getY x)
                        , getR x
                        )
                    )

        circles : List (Svg msg)
        circles =
            List.map
                (\( x, y, r ) ->
                    Svg.circle
                        [ Svg.Attributes.cx <| toString x
                        , Svg.Attributes.cy <| toString y
                        , Svg.Attributes.r <| toString r
                        ]
                        []
                )
                points

        path : Lazy (Svg msg)
        path =
            lazy
                (\() ->
                    Svg.path
                        [ Svg.Attributes.d
                            (Shape.line Shape.monotoneInXCurve
                                (List.map (\( x, y, _ ) -> Just ( x, y )) points)
                            )
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.strokeWidth "1px"
                        , Svg.Attributes.stroke "gray"
                        ]
                        []
                )
    in
        Svg.svg
            [ Svg.Attributes.width <| toString width
            , Svg.Attributes.height <| toString height
            ]
            [ Svg.g
                [ Daffy.Svg.transform
                    [ Daffy.Svg.translate margin.left margin.top ]
                ]
                (Svg.g
                    [ Daffy.Svg.transform
                        [ Daffy.Svg.translateY <|
                            height
                                - margin.bottom
                                - margin.top
                        ]
                    ]
                    [ xaxis ]
                    :: yaxis
                    :: List.when line (force path)
                    ++ circles
                )
            ]
