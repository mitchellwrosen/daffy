module Daffy.RunSpec exposing (..)

import Daffy.List.Extra as List
import Daffy.Proto.RunReq exposing (RunReq)


type alias RunSpec =
    { command : String
    , nurserySize : String -- -A
    , nurseryChunks : String -- -n
    , largeObjectSize : String -- -AL
    , oldGenMinSize : String -- -O
    , oldGenFactor : String -- -F
    , compaction : Bool -- -c?
    , stats : Bool -- -S?
    , prof : Bool -- -p?
    , eventlog : Bool -- -l?
    }


{-| List the GHC options a program must have been compile with in order to
satisfy this `RunSpec`.
-}
ghcFlags : RunSpec -> List String
ghcFlags spec =
    List.concat
        [ List.when spec.eventlog "-eventlog"
        , List.when spec.prof "-prof"
        , List.when
            (List.or
                [ not (String.isEmpty spec.nurserySize)
                , not (String.isEmpty spec.nurseryChunks)
                , not (String.isEmpty spec.largeObjectSize)
                , not (String.isEmpty spec.oldGenMinSize)
                , not (String.isEmpty spec.oldGenFactor)
                , spec.compaction
                , spec.stats
                , spec.prof
                , spec.eventlog
                ]
            )
            "-rtsopts"
        ]


{-| List the RTS flags associated with this `RunSpec` (for pedagogical purposes,
not for the server, because some flags, e.g. `-l`, need to be passed
separately).
-}
rtsFlags : RunSpec -> List String
rtsFlags spec =
    List.concat
        [ List.unless
            (String.isEmpty spec.nurserySize)
            ("-A" ++ spec.nurserySize)
        , List.unless
            (String.isEmpty spec.largeObjectSize)
            ("-AL" ++ spec.largeObjectSize)
        , List.when spec.compaction "-c"
        , List.unless
            (String.isEmpty spec.oldGenFactor)
            ("-F" ++ spec.oldGenFactor)
        , List.when spec.eventlog "-l"
        , List.unless
            (String.isEmpty spec.nurseryChunks)
            ("-n" ++ spec.nurseryChunks)
        , List.unless
            (String.isEmpty spec.oldGenMinSize)
            ("-o" ++ spec.oldGenMinSize)
        , List.when spec.prof "-pa"
        , List.when spec.stats "-S"
        ]


preview : RunSpec -> String
preview spec =
    if String.isEmpty spec.command then
        ""
    else
        case rtsFlags spec of
            [] ->
                spec.command

            flags ->
                String.join " " <| spec.command :: "+RTS" :: flags


{-| Make a `RunReq` with a `RunSpec`
-}
toReq : RunSpec -> RunReq
toReq spec =
    let
        flags : List String
        flags =
            List.concat
                [ List.unless
                    (String.isEmpty spec.nurserySize)
                    ("-A" ++ spec.nurserySize)
                , List.unless
                    (String.isEmpty spec.largeObjectSize)
                    ("-AL" ++ spec.largeObjectSize)
                , List.when spec.compaction "-c"
                , List.unless
                    (String.isEmpty spec.oldGenFactor)
                    ("-F" ++ spec.oldGenFactor)
                , List.unless
                    (String.isEmpty spec.nurseryChunks)
                    ("-n" ++ spec.nurseryChunks)
                , List.unless
                    (String.isEmpty spec.oldGenMinSize)
                    ("-o" ++ spec.oldGenMinSize)
                ]
    in
        { command =
            if List.isEmpty flags then
                spec.command
            else
                String.join " " <| spec.command :: "+RTS" :: flags ++ ["-RTS"]
        , stats = spec.stats
        , prof = spec.prof
        , eventlog = spec.eventlog
        }
