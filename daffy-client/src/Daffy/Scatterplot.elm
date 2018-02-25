module Daffy.Scatterplot exposing (Config, svg)

import Daffy.Svg
import List.Extra as List exposing (last)
import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))
import Maybe.Extra as Maybe exposing (unwrap)
import Svg exposing (Svg)
import Svg.Attributes
import Visualization.Axis as VAxis
import Visualization.Scale as VScale exposing (ContinuousScale)


type alias Config a =
    { width : Float
    , height : Float
    , margin :
        { bottom : Float
        , left : Float
        , right : Float
        , top : Float
        }
    , getX : a -> Float
    , getY : a -> Float
    , getR : a -> Int
    }


svg : Config a -> Nonempty a -> Svg msg
svg { width, height, margin, getX, getY, getR } xs =
    let
        { xmin, xmax, ymin, ymax } =
            extent getX getY xs

        xscale : ContinuousScale
        xscale =
            VScale.linear
                ( xmin, xmax )
                ( 0, width - margin.left - margin.right )

        yscale : ContinuousScale
        yscale =
            VScale.linear
                ( ymin, ymax )
                ( height - margin.bottom - margin.top, 0 )

        xaxis : Svg msg
        xaxis =
            VAxis.axis
                { orientation = VAxis.Bottom
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
            VAxis.axis
                { orientation = VAxis.Left
                , ticks = Nothing
                , tickFormat = Nothing
                , tickCount = 10
                , tickSizeInner = 6
                , tickSizeOuter = 6
                , tickPadding = 3
                }
                yscale

        draw : a -> Svg msg
        draw x =
            Svg.g []
                [ Svg.circle
                    [ (Svg.Attributes.cx << toString << VScale.convert xscale << getX) x
                    , (Svg.Attributes.cy << toString << VScale.convert yscale << getY) x
                    , (Svg.Attributes.r << toString << getR) x
                    ]
                    []
                ]
    in
        Svg.svg
            [ Svg.Attributes.width <| toString width
            , Svg.Attributes.height <| toString height
            ]
            [ Svg.g
                [ Daffy.Svg.transform
                    [ Daffy.Svg.translate margin.left margin.top ]
                ]
                [ Svg.g
                    [ Daffy.Svg.transform
                        [ Daffy.Svg.translateY <|
                            height
                                - margin.bottom
                                - margin.top
                        ]
                    ]
                    [ xaxis ]
                , Svg.g [] [ yaxis ]
                , Svg.g [] (List.map draw <| Nonempty.toList xs)
                ]
            ]


type alias Extent =
    { xmin : Float
    , xmax : Float
    , ymin : Float
    , ymax : Float
    }


extent : (a -> Float) -> (a -> Float) -> Nonempty a -> Extent
extent getX getY (Nonempty v0 vs) =
    let
        x0 : Float
        x0 =
            getX v0

        y0 : Float
        y0 =
            getY v0

        step : a -> Extent -> Extent
        step v { xmin, xmax, ymin, ymax } =
            let
                x : Float
                x =
                    getX v

                y : Float
                y =
                    getY v
            in
                { xmin = min xmin x
                , xmax = max xmax x
                , ymin = min ymin y
                , ymax = max ymax y
                }
    in
        List.foldl step
            { xmin = x0, xmax = x0, ymin = y0, ymax = y0 }
            vs
