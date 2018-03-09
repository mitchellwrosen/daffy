module Daffy.Scatterplot exposing (Config, svg)

import Daffy.Extent as Extent exposing (Extent)
import Daffy.List.Extra as List exposing (when)
import Daffy.Svg as Svg exposing (transform, translate, translateY)
import Lazy exposing (Lazy, force, lazy)
import List.Extra as List exposing (last)
import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))
import Maybe.Extra as Maybe exposing (unwrap)
import Svg exposing (Svg)
import Svg.Attributes
import Visualization.Axis as Axis
import Visualization.Scale as Scale exposing (ContinuousScale)
import Visualization.Shape as Shape


--       5px
--       ||
--       60px                                           20px
--       |  |                                           | |
--       +-------------------------------------------------+
--  18px{|Title                                            |
--   8px{| -|                                         .    |
--       |  |                                              |
--       | -|                                    . .       |
--       |  |                                 .            |
--       | -|                     .       .                |
--       |  |                        .                     |
--       | -|                                              |
--       |  |            .  . .                            |
--       | -|    .                                         |
--       |  |         .                                    |
--       | -| .                                            |
--       |  |___________________________________________   |
--  25px{|    ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '    |
--       +-------------------------------------------------+


{-| Distance from the bottom pixel of the title to the top of the window
-}
titleTopMargin =
    18


{-| Distance from the leftmost pixel of the title to the left of the window
-}
titleLeftMargin =
    5


{-| Distance from the top pixel of the Y-axis to the top of the window
-}
graphTopMargin =
    26


{-| Distance from the X-axis to the bottom of the window
-}
graphBottomMargin =
    25


{-| Distance from the Y-axis to the left of the window
-}
graphLeftMargin =
    60


{-| Distance from the rightmost pixel of the X-axis to the right of the window
-}
graphRightMargin =
    20


type alias Config a =
    { title : String
    , width : Float
    , height : Float

    -- Modify the default extent, which ranges from xmin to xmax and ymin to
    -- ymax with a 5% buffer.
    , extent : Extent -> Extent

    -- Draw a line between the dots?
    , line : Bool
    , getX : a -> Float
    , getY : a -> Float
    , getR : a -> Int
    , showX : Float -> String
    , showY : Float -> String
    }


svg : Config a -> Nonempty a -> Svg msg
svg config xs =
    let
        { xmin, xmax, ymin, ymax } =
            config.extent <|
                Extent.bufferY 0.05 <|
                    Extent.extent { getX = config.getX, getY = config.getY } xs

        xscale : ContinuousScale
        xscale =
            Scale.linear
                ( xmin, xmax )
                ( 0, config.width - graphLeftMargin - graphRightMargin )

        yscale : ContinuousScale
        yscale =
            Scale.linear
                ( ymin, ymax )
                ( config.height - graphTopMargin - graphBottomMargin, 0 )

        xaxis : Svg msg
        xaxis =
            Svg.g
                [ Svg.transform
                    [ Svg.translateY <|
                        config.height
                            - graphTopMargin
                            - graphBottomMargin
                    ]
                ]
                [ Axis.axis
                    { orientation = Axis.Bottom
                    , ticks = Nothing
                    , tickFormat = Just config.showX
                    , tickCount = 10
                    , tickSizeInner = 6
                    , tickSizeOuter = 6
                    , tickPadding = 3
                    }
                    xscale
                ]

        yaxis : Svg msg
        yaxis =
            Axis.axis
                { orientation = Axis.Left
                , ticks = Nothing
                , tickFormat = Just config.showY
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
                        ( Scale.convert xscale (config.getX x)
                        , Scale.convert yscale (config.getY x)
                        , config.getR x
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
                                (List.map
                                    (\( x, y, _ ) -> Just ( x, y ))
                                    points
                                )
                            )
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.strokeWidth "1px"
                        , Svg.Attributes.stroke "gray"
                        ]
                        []
                )
    in
        Svg.svg
            [ Svg.Attributes.width <| toString config.width
            , Svg.Attributes.height <| toString config.height
            ]
            [ Svg.text_
                [ Svg.transform
                    [ Svg.translate titleLeftMargin titleTopMargin ]
                ]
                [ Svg.text config.title ]
            , Svg.g
                [ Svg.transform
                    [ Svg.translate graphLeftMargin graphTopMargin ]
                ]
                (List.concat
                    [ [ xaxis ]
                    , [ yaxis ]
                    , List.when config.line (force path)
                    , circles
                    ]
                )
            ]
