module Daffy.ElapsedTimeGCStats exposing (ElapsedTimeGCStats, make)

import Daffy.List.Extra exposing (groupBy)
import Daffy.Types exposing (GCStats)
import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))


{-| GCStats, bucketed by total elapsed time.
-}
type alias ElapsedTimeGCStats =
    { bytesAllocated : Int
    , bytesCopied : Int
    , liveBytes : Int
    , time : Float -- Total elapsed time
    , pageFaults : Int
    , totalPageFaults : Int
    , generation : Int
    , count : Int -- How many GCStats are in this bucket
    }


make : List GCStats -> List ElapsedTimeGCStats
make =
    groupBy coincident >> List.concatMap group


coincident : GCStats -> GCStats -> Bool
coincident x y =
    x.totalTime.elapsed == y.totalTime.elapsed


group : Nonempty GCStats -> List ElapsedTimeGCStats
group =
    Nonempty.toList
        >> List.sortBy .generation
        >> groupBy (\a b -> a.generation == b.generation)
        >> List.map
            (\(Nonempty x xs) ->
                let
                    ys =
                        x :: xs
                in
                    { bytesAllocated = List.sum (List.map .bytesAllocated ys)
                    , bytesCopied = List.sum (List.map .bytesCopied ys)
                    , liveBytes = List.sum (List.map .liveBytes ys)
                    , time = x.totalTime.elapsed
                    , pageFaults = List.sum (List.map .pageFaults ys)
                    , totalPageFaults = List.sum (List.map .totalPageFaults ys)
                    , generation = x.generation
                    , count = List.length ys
                    }
            )
