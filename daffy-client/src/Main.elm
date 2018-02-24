module Main exposing (..)

import Daffy.Lenses exposing (..)
import Daffy.RunSpec exposing (RunSpec)
import Daffy.Types exposing (..)

import Array exposing (Array)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode exposing (..)
import Json.Encode
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))
import Maybe.Extra as Maybe
import Step exposing (Step)
import Svg exposing (Svg)
import Svg.Attributes
import Visualization.Axis as VAxis
import Visualization.Path as VPath
import Visualization.Scale as VScale exposing (ContinuousScale)
import WebSocket


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = Step.asUpdateFunction update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = Initial ProgramData
    | RunningProgram ProgramData ProgramOutput
    | MsgParseError String
    | ExploringRun ProgramData ProgramRun


type alias ProgramData =
    { runSpec : RunSpec
    , runs : Array ProgramRun
    }


type alias ProgramOutput =
    { output : Array Output
    , stats : Maybe Stats
    , flamegraphs : List SvgPath
    }


type alias Output =
    { line : String
    , stdout : Bool
    }


type alias SvgPath =
    String


type alias ProgramRun =
    { output : Array Output
    , exitCode : Int
    , flamegraphs : List SvgPath
    , stats : Maybe Stats
    }


type Msg
    = TypeCommand String
    | TypeNurserySize String
    | TypeNurseryChunks String
    | TypeLargeObjectSize String
    | TypeOldGenMinSize String
    | TypeOldGenFactor String
    | ToggleCompaction Bool
    | ToggleStats Bool
    | ToggleProf Bool
    | ToggleEventlog Bool
    | RunCommand
    | RunningProgramMsg RunningProgramMsg
    | ExploreRun Index
    | StartNewRun


type alias Index =
    Int


type RunExplorationMsg
    = TypeFilter String
    | DeleteFilter
    | ConfirmFilter


type RunningProgramMsg
    = OutputMsg Output
    | RunStats Stats
    | ExitedWith Int
    | RunMsgParseErr String
    | FlamegraphMsg SvgPath


init : Model
init =
    Initial
        { runSpec =
            { command = ""
            , nurserySize = ""
            , nurseryChunks = ""
            , largeObjectSize = ""
            , oldGenMinSize = ""
            , oldGenFactor = ""
            , compaction = False
            , stats = True
            , prof = False
            , eventlog = False
            }
        , runs = Array.empty
        }


update : Msg -> Model -> Step Model Msg Never
update msg model =
    case ( model, msg ) of
        ( Initial model_, TypeCommand s ) ->
            model_
                |> set (runSpecL << commandL) s
                |> Initial
                |> Step.to

        ( Initial model_, TypeNurserySize s ) ->
            model_
                |> set (runSpecL << nurserySizeL) s
                |> Initial
                |> Step.to

        ( Initial model_, TypeNurseryChunks s ) ->
            model_
                |> set (runSpecL << nurseryChunksL) s
                |> Initial
                |> Step.to

        ( Initial model_, TypeLargeObjectSize s ) ->
            model_
                |> set (runSpecL << largeObjectSizeL) s
                |> Initial
                |> Step.to

        ( Initial model_, TypeOldGenMinSize s ) ->
            model_
                |> set (runSpecL << oldGenMinSizeL) s
                |> Initial
                |> Step.to

        ( Initial model_, TypeOldGenFactor s ) ->
            model_
                |> set (runSpecL << oldGenFactorL) s
                |> Initial
                |> Step.to

        ( Initial model_, ToggleCompaction b ) ->
            model_
                |> set (runSpecL << compactionL) b
                |> Initial
                |> Step.to

        ( Initial model_, ToggleStats b ) ->
            model_
                |> set (runSpecL << statsL) b
                |> Initial
                |> Step.to

        ( Initial model_, ToggleProf b ) ->
            model_
                |> set (runSpecL << profL) b
                |> Initial
                |> Step.to

        ( Initial model_, ToggleEventlog b ) ->
            model_
                |> set (runSpecL << eventlogL) b
                |> Initial
                |> Step.to

        ( Initial model_, ExploreRun index ) ->
            case Array.get index model_.runs of
                Just programRun ->
                    Step.to (ExploringRun model_ programRun)

                Nothing ->
                    Step.noop

        ( Initial model_, RunCommand ) ->
            Step.to (RunningProgram model_ { output = Array.empty, stats = Nothing, flamegraphs = [] })
                |> Step.withCmd
                    ([ ( "command", Json.Encode.string model_.runSpec.command )
                     , ( "stats", Json.Encode.bool model_.runSpec.stats )
                     , ( "prof", Json.Encode.bool model_.runSpec.prof )
                     , ( "eventlog", Json.Encode.bool model_.runSpec.eventlog )
                     ]
                        |> Json.Encode.object
                        |> Json.Encode.encode 0
                        |> WebSocket.send "ws://localhost:8080"
                    )

        ( RunningProgram model runningProgram, RunningProgramMsg programRunMsg ) ->
            stepRunningProgram programRunMsg runningProgram
                |> Step.map (\x -> RunningProgram model x)
                |> Step.mapMsg RunningProgramMsg
                |> Step.onExit
                    (\result ->
                        Step.to <|
                            case result of
                                Err parseErr ->
                                    MsgParseError parseErr

                                Ok run ->
                                    ExploringRun model run
                    )

        ( ExploringRun data programRun, StartNewRun ) ->
            Step.to (Initial { data | runs = Array.push programRun data.runs })

        _ ->
            Step.noop


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        RunningProgram runningProgram _ ->
            Sub.map RunningProgramMsg <|
                WebSocket.listen "ws://localhost:8080"
                    (\raw ->
                        case Json.Decode.decodeString decodeRunMsg raw of
                            Ok msg ->
                                msg

                            Err string ->
                                RunMsgParseErr string
                    )

        _ ->
            WebSocket.keepAlive "ws://localhost:8080"


decodeRunMsg : Decoder RunningProgramMsg
decodeRunMsg =
    let
        payload =
            field "payload"
    in
        field "type" string
            |> Json.Decode.andThen
                (\type_ ->
                    case type_ of
                        "output" ->
                            payload decodeOutput
                                |> Json.Decode.map OutputMsg

                        "event" ->
                            payload (fail "gotta implement event")

                        "exitcode" ->
                            payload int
                                |> Json.Decode.map ExitedWith

                        "stats" ->
                            payload decodeStats
                                |> Json.Decode.map RunStats

                        "flamegraph" ->
                            payload string
                                |> Json.Decode.map FlamegraphMsg

                        _ ->
                            Json.Decode.fail "couldn't parse message from server"
                )


decodeOutput : Decoder Output
decodeOutput =
    map2 Output (field "line" string) (field "stdout" bool)


type alias ParseErr =
    String


stepRunningProgram : RunningProgramMsg -> ProgramOutput -> Step ProgramOutput msg (Result String ProgramRun)
stepRunningProgram programRunMsg ({ output, stats, flamegraphs } as programOutput) =
    case programRunMsg of
        OutputMsg line ->
            Step.to { programOutput | output = Array.push line output }

        ExitedWith code ->
            Step.exit (Ok { output = output, stats = stats, exitCode = code, flamegraphs = flamegraphs })

        RunStats stats ->
            Step.to { programOutput | stats = Just stats }

        RunMsgParseErr err ->
            Step.exit (Err err)

        FlamegraphMsg flamegraph ->
            Step.to { programOutput | flamegraphs = flamegraph :: flamegraphs }


view : Model -> Html Msg
view model =
    let
        viewProgramOutput programData { output } =
            div [ class "command-form" ]
                (span [ class "ps1" ] [ text <| "$ " ++ programData.runSpec.command ]
                    :: (output
                            |> Array.toList
                            |> List.map (p [] << List.singleton << text << .line)
                       )
                )
    in
        div [ class "container" ] <|
            [ h1 [ class "heading" ] [ text "🔥 daffy 🔥" ] ]
                ++ case model of
                    Initial model_ ->
                        [ Html.form [ class "command-form", Html.Events.onSubmit RunCommand ]
                            [ div [ class "form-group prompt-group" ]
                                [ span [ class "ps1" ] [ text "$" ]
                                , input
                                    [ type_ "text"
                                    , Html.Attributes.autofocus True
                                    , Html.Attributes.value model_.runSpec.command
                                    , Html.Events.onInput TypeCommand
                                    ]
                                    []
                                ]
                            , viewPreview model_
                            , div [ class "form-pair" ]
                                [ div [ class "form-group" ]
                                    [ label
                                        []
                                        [ text "Nursery size" ]
                                    , input
                                        [ class "tiny"
                                        , type_ "text"
                                        , Html.Attributes.placeholder "1m"
                                        , Html.Attributes.value model_.runSpec.nurserySize
                                        , Html.Events.onInput TypeNurserySize
                                        ]
                                        []
                                    , span
                                        [ class "inline-label" ]
                                        [ text "split into" ]
                                    , input
                                        [ class "tiny"
                                        , type_ "text"
                                        , Html.Attributes.value model_.runSpec.nurseryChunks
                                        , Html.Events.onInput TypeNurseryChunks
                                        ]
                                        []
                                    , span [ class "inline-label" ]
                                        [ text "chunks" ]
                                    ]
                                , div [ class "form-group" ]
                                    [ label
                                        []
                                        [ text "Large object size" ]
                                    , input
                                        [ type_ "text"
                                        , Html.Attributes.placeholder <|
                                            if String.isEmpty model_.runSpec.nurserySize then
                                                "1m"
                                            else
                                                model_.runSpec.nurserySize
                                        , Html.Attributes.value model_.runSpec.largeObjectSize
                                        , Html.Events.onInput TypeLargeObjectSize
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "form-pair" ]
                                [ div [ class "form-group" ]
                                    [ label
                                        []
                                        [ text "Minimum old generation size" ]
                                    , input
                                        [ type_ "text"
                                        , Html.Attributes.placeholder "1m"
                                        , Html.Attributes.value model_.runSpec.oldGenMinSize
                                        , Html.Events.onInput TypeOldGenMinSize
                                        ]
                                        []
                                    ]
                                , div [ class "form-group" ]
                                    [ label
                                        []
                                        [ text "Old generation factor" ]
                                    , input
                                        [ type_ "text"
                                        , Html.Attributes.placeholder "2"
                                        , Html.Attributes.value model_.runSpec.oldGenFactor
                                        , Html.Events.onInput TypeOldGenFactor
                                        ]
                                        []
                                    ]
                                ]
                            , div [ class "form-group" ]
                                [ span
                                    [ class "inline-label" ]
                                    [ text "Collect oldest generation by" ]
                                , fieldset []
                                    ((radio "Copying" (model_.runSpec.compaction == False) (ToggleCompaction False))
                                        ++ (radio "Compacting" (model_.runSpec.compaction == True) (ToggleCompaction True))
                                    )
                                ]
                            , div [ class "form-group" ]
                                [ label
                                    [ class "inline-label" ]
                                    [ input
                                        [ type_ "checkbox"
                                        , Html.Events.onCheck ToggleStats
                                        , Html.Attributes.checked model_.runSpec.stats
                                        ]
                                        []
                                    , text "stats"
                                    ]
                                , label
                                    [ class "inline-label" ]
                                    [ input
                                        [ type_ "checkbox"
                                        , Html.Events.onCheck ToggleProf
                                        , Html.Attributes.checked model_.runSpec.prof
                                        ]
                                        []
                                    , text "profile"
                                    ]
                                , label
                                    [ class "inline-label" ]
                                    [ input
                                        [ type_ "checkbox"
                                        , Html.Events.onCheck ToggleEventlog
                                        , Html.Attributes.checked model_.runSpec.eventlog
                                        ]
                                        []
                                    , text "eventlog"
                                    ]
                                ]
                            , div [ class "form-group" ]
                                [ input
                                    [ class "btn"
                                    , type_ "submit"
                                    , Html.Attributes.value "Run"
                                    ]
                                    []
                                ]
                            ]
                        , viewFlagsRequired model_
                        ]

                    RunningProgram programData programOutput ->
                        [ viewProgramOutput programData programOutput ]

                    MsgParseError parseError ->
                        [ div [] [ text <| "error parsing messages from daffy: " ++ parseError ] ]

                    ExploringRun programData programRun ->
                        [ button [ class "btn btn-back", type_ "button", Html.Events.onClick StartNewRun ] [ text "Back" ]
                        , viewProgramOutput programData programRun
                        ]
                            ++ List.map (\path -> object [ class "flame-svg", Html.Attributes.attribute "data" path ] []) programRun.flamegraphs
                            ++ List.filterMap (Maybe.map (.garbageCollections >> viewStats)) [ programRun.stats ]
                            ++ List.filterMap (Maybe.map (.garbageCollections >> timeBucketGCs >> renderBytesAllocatedSVG)) [ programRun.stats ]
                            ++ List.filterMap (Maybe.map (.garbageCollections >> timeBucketGCs >> renderBytesCopiedSVG)) [ programRun.stats ]
                            ++ List.filterMap (Maybe.map (.garbageCollections >> timeBucketGCs >> renderLiveBytesSVG)) [ programRun.stats ]


radio : String -> Bool -> a -> List (Html a)
radio value isChecked msg =
    let
        id =
            "radio-" ++ value
    in
        [ input
            [ type_ "radio"
            , checked isChecked
            , Html.Events.onClick msg
            , Html.Attributes.id id
            ]
            []
        , label [ Html.Attributes.for id ]
            [ text value
            ]
        ]


viewPreview : ProgramData -> Html a
viewPreview data =
    div [ class "command-preview" ]
        [ span [ class "ps1" ] [ text "$" ]
        , text <| Daffy.RunSpec.preview data.runSpec
        ]


viewFlagsRequired : ProgramData -> Html a
viewFlagsRequired data =
    let
        flags : List String
        flags =
            Daffy.RunSpec.ghcFlags data.runSpec
    in
        if List.isEmpty flags then
            text ""
        else
            div [ class "flags-required" ]
                [ span [ class "flags-required__preface" ]
                    [ text "Note: your program must be compiled with: " ]
                , span [ class "flags-required__flags" ]
                    [ text <| String.join " " flags ]
                ]



-- Bucket garbage collections by timestamp (since the -S output is only accurate
-- to the millisecond or so). Don't put garbage collections from the same
-- generation in the same bucket.


timeBucketGCs : List GCStats -> List { totalTimeElapsed : Float, averageBytesAllocated : Float, averageBytesCopied : Float, averageLiveBytes : Float, generation : Int }
timeBucketGCs =
    groupBy (\prev curr -> curr.totalTime.elapsed == prev.totalTime.elapsed)
        >> List.concatMap
            (\gcsAtTime ->
                gcsAtTime
                    |> Nonempty.toList
                    |> List.sortBy .generation
                    |> groupBy (\a b -> a.generation == b.generation)
                    |> List.map
                        (\(Nonempty { generation, bytesAllocated, bytesCopied, totalTime, liveBytes } gcs) ->
                            let
                                len =
                                    toFloat (List.length gcs)
                            in
                                { totalTimeElapsed = totalTime.elapsed
                                , averageBytesAllocated =
                                    toFloat (bytesAllocated + List.sum (List.map .bytesAllocated gcs))
                                        / (1 + len)
                                , averageBytesCopied =
                                    toFloat (bytesCopied + List.sum (List.map .bytesCopied gcs))
                                        / (1 + len)
                                , averageLiveBytes =
                                    toFloat (liveBytes + List.sum (List.map .liveBytes gcs))
                                        / (1 + len)
                                , generation = generation
                                }
                        )
            )


viewStats : List GCStats -> Html msg
viewStats garbageCollections =
    let
        timeBucketedGCs : List { totalTimeElapsed : Float, averageBytesAllocated : Float, averageBytesCopied : Float, averageLiveBytes : Float, generation : Int }
        timeBucketedGCs =
            timeBucketGCs garbageCollections
    in
        div []
            [ LineChart.viewCustom (chartConfig .totalTimeElapsed .averageLiveBytes)
                [ LineChart.line (Color.rgb 255 99 71)
                    Dots.none
                    "Last Run"
                    timeBucketedGCs
                ]
            , div [] [ text <| "Length: " ++ toString (List.length timeBucketedGCs) ]
            ]


groupBy : (a -> a -> Bool) -> List a -> List (Nonempty a)
groupBy sameGroup =
    List.foldr
        (\x ys ->
            case ys of
                (Nonempty y ys_) :: groups ->
                    if sameGroup x y then
                        Nonempty x (y :: ys_) :: groups
                    else
                        Nonempty x [] :: ys

                [] ->
                    [ Nonempty x [] ]
        )
        []


chartConfig : (data -> Float) -> (data -> Float) -> LineChart.Config data msg
chartConfig x y =
    { x = Axis.default 1000 "Time Elapsed" x
    , y = Axis.default 600 "Bytes" y
    , container = Container.responsive "line-chart-1"
    , interpolation = Interpolation.stepped
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = Dots.custom (Dots.full 0)
    }


margin : { bottom : number, left : number1, right : number2, top : number3 }
margin =
    { top = 20, right = 40, bottom = 20, left = 40 }


( width, height ) =
    ( 800 - margin.left - margin.right, 500 - margin.top - margin.bottom )


renderBytesAllocatedSVG : List { r | averageBytesAllocated : Float, totalTimeElapsed : Float, generation : Int } -> Svg msg
renderBytesAllocatedSVG stats =
    let
        xscale : ContinuousScale
        xscale =
            let
                xmax : Float
                xmax =
                    stats
                        |> List.last
                        |> Maybe.unwrap 0 .totalTimeElapsed
            in
                VScale.linear ( 0, xmax ) ( 0, width )

        yscale : ContinuousScale
        yscale =
            let
                ymin : Float
                ymin =
                    stats
                        |> List.map .averageBytesAllocated
                        |> List.minimum
                        |> Maybe.withDefault 0

                ymax : Float
                ymax =
                    stats
                        |> List.map .averageBytesAllocated
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
                VScale.linear ( ymin, ymax ) ( height, 0 )

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

        point stats =
            Svg.g
                []
                [ Svg.circle
                    [ Svg.Attributes.cx <|
                        toString <|
                            VScale.convert xscale (stats.totalTimeElapsed)
                    , Svg.Attributes.cy <|
                        toString <|
                            VScale.convert yscale (stats.averageBytesAllocated)
                    , Svg.Attributes.r <|
                        toString <|
                            2
                                * (stats.generation + 1)
                    ]
                    []
                ]
    in
        Svg.svg
            [ Svg.Attributes.width (toString (width + margin.left + margin.right))
            , Svg.Attributes.height (toString (height + margin.top + margin.bottom))
            ]
            [ Svg.g
                [ transformTranslate ( margin.left, margin.top ) ]
                [ Svg.g [ transformTranslate ( 0, height ) ] [ xaxis ]
                , Svg.g [] [ yaxis ]
                , Svg.g [] (List.map point stats)
                ]
            ]


renderBytesCopiedSVG : List { r | averageBytesCopied : Float, totalTimeElapsed : Float, generation : Int } -> Svg msg
renderBytesCopiedSVG stats =
    let
        xscale : ContinuousScale
        xscale =
            let
                xmax : Float
                xmax =
                    stats
                        |> List.last
                        |> Maybe.unwrap 0 .totalTimeElapsed
            in
                VScale.linear ( 0, xmax ) ( 0, width )

        yscale : ContinuousScale
        yscale =
            let
                ymin : Float
                ymin =
                    stats
                        |> List.map .averageBytesCopied
                        |> List.minimum
                        |> Maybe.withDefault 0

                ymax : Float
                ymax =
                    stats
                        |> List.map .averageBytesCopied
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
                VScale.linear ( ymin, ymax ) ( height, 0 )

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

        point stats =
            Svg.g
                []
                [ Svg.circle
                    [ Svg.Attributes.cx <|
                        toString <|
                            VScale.convert xscale (stats.totalTimeElapsed)
                    , Svg.Attributes.cy <|
                        toString <|
                            VScale.convert yscale (stats.averageBytesCopied)
                    , Svg.Attributes.r <|
                        toString <|
                            2
                                * (stats.generation + 1)
                    ]
                    []
                ]
    in
        Svg.svg
            [ Svg.Attributes.width (toString (width + margin.left + margin.right))
            , Svg.Attributes.height (toString (height + margin.top + margin.bottom))
            ]
            [ Svg.g
                [ transformTranslate ( margin.left, margin.top ) ]
                [ Svg.g [ transformTranslate ( 0, height ) ] [ xaxis ]
                , Svg.g [] [ yaxis ]
                , Svg.g [] (List.map point stats)
                ]
            ]


renderLiveBytesSVG : List { r | averageLiveBytes : Float, totalTimeElapsed : Float, generation : Int } -> Svg msg
renderLiveBytesSVG stats =
    let
        xscale : ContinuousScale
        xscale =
            let
                xmax : Float
                xmax =
                    stats
                        |> List.last
                        |> Maybe.unwrap 0 .totalTimeElapsed
            in
                VScale.linear ( 0, xmax ) ( 0, width )

        yscale : ContinuousScale
        yscale =
            let
                ymin : Float
                ymin =
                    stats
                        |> List.map .averageLiveBytes
                        |> List.minimum
                        |> Maybe.withDefault 0

                ymax : Float
                ymax =
                    stats
                        |> List.map .averageLiveBytes
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
                VScale.linear ( ymin, ymax ) ( height, 0 )

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

        point stats =
            Svg.g
                []
                [ Svg.circle
                    [ Svg.Attributes.cx <|
                        toString <|
                            VScale.convert xscale (stats.totalTimeElapsed)
                    , Svg.Attributes.cy <|
                        toString <|
                            VScale.convert yscale (stats.averageLiveBytes)
                    , Svg.Attributes.r <|
                        toString <|
                            2
                                * (stats.generation + 1)
                    ]
                    []
                ]
    in
        Svg.svg
            [ Svg.Attributes.width (toString (width + margin.left + margin.right))
            , Svg.Attributes.height (toString (height + margin.top + margin.bottom))
            ]
            [ Svg.g
                [ transformTranslate ( margin.left, margin.top ) ]
                [ Svg.g [ transformTranslate ( 0, height ) ] [ xaxis ]
                , Svg.g [] [ yaxis ]
                , Svg.g [] (List.map point stats)
                ]
            ]


transformTranslate : ( a, b ) -> Svg.Attribute msg
transformTranslate ( x, y ) =
    Svg.Attributes.transform <|
        String.concat
            [ "translate("
            , toString x
            , ","
            , toString y
            , ")"
            ]
