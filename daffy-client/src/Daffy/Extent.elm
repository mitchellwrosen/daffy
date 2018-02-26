module Daffy.Extent exposing (..)

import List.Nonempty exposing (Nonempty(Nonempty))


type alias Extent =
    { xmin : Float
    , xmax : Float
    , ymin : Float
    , ymax : Float
    }


extent : { getX : a -> Float, getY : a -> Float } -> Nonempty a -> Extent
extent { getX, getY } (Nonempty v0 vs) =
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



-- Buffer the Y-dimensions of an `Extent` by the given percentage of the
-- difference. For example, if `ymin = 0` and `


bufferY : Float -> Extent -> Extent
bufferY p { xmin, xmax, ymin, ymax } =
    let
        yrange : Float
        yrange =
            ymax - ymin
    in
        { xmin = xmin
        , xmax = xmax
        , ymin = ymin - p * yrange
        , ymax = ymax + p * yrange
        }
