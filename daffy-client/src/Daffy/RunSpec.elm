module Daffy.RunSpec exposing (..)


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


ghcFlags : RunSpec -> List String
ghcFlags spec =
    List.concat
        [ if spec.eventlog then
            [ "-eventlog" ]
          else
            []
        , if spec.prof then
            [ "-prof" ]
          else
            []
        , if
            not (String.isEmpty spec.nurserySize)
                || not (String.isEmpty spec.nurseryChunks)
                || not (String.isEmpty spec.largeObjectSize)
                || not (String.isEmpty spec.oldGenMinSize)
                || not (String.isEmpty spec.oldGenFactor)
                || spec.compaction
                || spec.stats
                || spec.prof
                || spec.eventlog
          then
            [ "-rtsopts" ]
          else
            [ ]
        ]


rtsFlags : RunSpec -> List String
rtsFlags spec =
    List.concat
        [ if String.isEmpty spec.nurserySize then
            []
          else
            [ "-A" ++ spec.nurserySize ]
        , if String.isEmpty spec.largeObjectSize then
            []
          else
            [ "-AL" ++ spec.largeObjectSize ]
        , if spec.compaction then
            [ "-c" ]
          else
            []
        , if String.isEmpty spec.oldGenFactor then
            []
          else
            [ "-F" ++ spec.oldGenFactor ]
        , if String.isEmpty spec.nurseryChunks then
            []
          else
            [ "-n" ++ spec.nurseryChunks ]
        , if String.isEmpty spec.oldGenMinSize then
            []
          else
            [ "-o" ++ spec.oldGenMinSize ]
        , if spec.eventlog then
            [ "-l" ]
          else
            []
        , if spec.prof then
            [ "-pa" ]
          else
            []
        , if spec.stats then
            [ "-S" ]
          else
            []
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
