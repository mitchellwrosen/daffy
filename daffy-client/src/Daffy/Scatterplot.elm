module Daffy.Scatterplot exposing (Config, svg)

import Daffy.Extent as Extent exposing (Extent)
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
    , extent : Extent -> Extent
    , getX : a -> Float
    , getY : a -> Float
    , getR : a -> Int
    }


svg : Config a -> Nonempty a -> Svg msg
svg { width, height, margin, extent, getX, getY, getR } xs =
    let
        { xmin, xmax, ymin, ymax } =
            extent
              <| Extent.bufferY 0.05
              <| Extent.extent { getX = getX, getY = getY } xs

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
