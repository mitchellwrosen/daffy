module Main exposing (..)

import Array exposing (Array)
import Color
import DaffyTypes exposing (..)
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
        , runs = Array.empty
        }


update : Msg -> Model -> Step Model Msg Never
update msg model =
    case ( model, msg ) of
        ( Initial model_, TypeCommand s ) ->
            Step.to (Initial { model_ | command = s })

        ( Initial model_, TypeNurserySize s ) ->
            Step.to (Initial { model_ | nurserySize = s })

        ( Initial model_, TypeNurseryChunks s ) ->
            Step.to (Initial { model_ | nurseryChunks = s })

        ( Initial model_, TypeLargeObjectSize s ) ->
            Step.to (Initial { model_ | largeObjectSize = s })

        ( Initial model_, TypeOldGenMinSize s ) ->
            Step.to (Initial { model_ | oldGenMinSize = s })

        ( Initial model_, TypeOldGenFactor s ) ->
            Step.to (Initial { model_ | oldGenFactor = s })

        ( Initial model_, ToggleCompaction b ) ->
            Step.to (Initial { model_ | compaction = b })

        ( Initial model_, ToggleStats b ) ->
            Step.to (Initial { model_ | stats = b })

        ( Initial model_, ToggleProf b ) ->
            Step.to (Initial { model_ | prof = b })

        ( Initial model_, ToggleEventlog b ) ->
            Step.to (Initial { model_ | eventlog = b })

        ( Initial model_, ExploreRun index ) ->
            case Array.get index model_.runs of
                Just programRun ->
                    Step.to (ExploringRun model_ programRun)

                Nothing ->
                    Step.noop

        ( Initial model_, RunCommand ) ->
            Step.to (RunningProgram model_ { output = Array.empty, stats = Nothing, flamegraphs = [] })
                |> Step.withCmd
                    ([ ( "command", Json.Encode.string model_.command )
                     , ( "stats", Json.Encode.bool model_.stats )
                     , ( "prof", Json.Encode.bool model_.prof )
                     , ( "eventlog", Json.Encode.bool model_.eventlog )
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
                (span [ class "ps1" ] [ text <| "$ " ++ programData.command ]
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
                                    , Html.Attributes.value model_.command
                                    , Html.Events.onInput TypeCommand
                                    ]
                                    []
                                ]
                            , div [ class "form-group prompt-group" ]
                                [ span
                                    [ class "ps1" ]
                                    [ text "Nursery size:" ]
                                , input
                                    [ type_ "text"
                                    , Html.Attributes.placeholder "1m"
                                    , Html.Attributes.value model_.nurserySize
                                    , Html.Events.onInput TypeNurserySize
                                    ]
                                    []
                                , span
                                    [ class "ps1" ]
                                    [ text "split into" ]
                                , input
                                    [ type_ "text"
                                    , Html.Attributes.value model_.nurseryChunks
                                    , Html.Events.onInput TypeNurseryChunks
                                    ]
                                    []
                                , span [ class "ps1" ] [ text "chunks" ]
                                ]
                            , div [ class "form-group prompt-group" ]
                                [ span
                                    [ class "ps1" ]
                                    [ text "Large object size:" ]
                                , input
                                    [ type_ "text"
                                    , Html.Attributes.placeholder <|
                                        if String.isEmpty model_.nurserySize then
                                            "1m"
                                        else
                                            model_.nurserySize
                                    , Html.Attributes.value model_.largeObjectSize
                                    , Html.Events.onInput TypeLargeObjectSize
                                    ]
                                    []
                                ]
                            , div [ class "form-group prompt-group" ]
                                [ span
                                    [ class "ps1" ]
                                    [ text "Minimum old generation size:" ]
                                , input
                                    [ type_ "text"
                                    , Html.Attributes.placeholder "1m"
                                    , Html.Attributes.value model_.oldGenMinSize
                                    , Html.Events.onInput TypeOldGenMinSize
                                    ]
                                    []
                                ]
                            , div [ class "form-group prompt-group" ]
                                [ span
                                    [ class "ps1" ]
                                    [ text "Old generation factor:" ]
                                , input
                                    [ type_ "text"
                                    , Html.Attributes.placeholder "2"
                                    , Html.Attributes.value model_.oldGenFactor
                                    , Html.Events.onInput TypeOldGenFactor
                                    ]
                                    []
                                ]
                            , div [ class "form-group prompt-group" ]
                                [ span
                                    [ class "ps1" ]
                                    [ text "Collect oldest generation by:" ]
                                , fieldset []
                                    [ radio "Copying" (model_.compaction == False) (ToggleCompaction False)
                                    , radio "Compacting" (model_.compaction == True) (ToggleCompaction True)
                                    ]
                                ]
                            , viewPreview model_
                            , div [ class "form-group" ]
                                [ input
                                    [ class "btn"
                                    , type_ "submit"
                                    , Html.Attributes.value "Run"
                                    ]
                                    []
                                , label
                                    []
                                    [ input
                                        [ type_ "checkbox"
                                        , Html.Events.onCheck ToggleStats
                                        , Html.Attributes.checked model_.stats
                                        ]
                                        []
                                    , text "stats"
                                    ]
                                , label
                                    []
                                    [ input
                                        [ type_ "checkbox"
                                        , Html.Events.onCheck ToggleProf
                                        , Html.Attributes.checked model_.prof
                                        ]
                                        []
                                    , text "profile"
                                    ]
                                , label
                                    []
                                    [ input
                                        [ type_ "checkbox"
                                        , Html.Events.onCheck ToggleEventlog
                                        , Html.Attributes.checked model_.eventlog
                                        ]
                                        []
                                    , text "eventlog"
                                    ]
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


radio : String -> Bool -> a -> Html a
radio value isChecked msg =
    label []
        [ input
            [ type_ "radio"
            , checked isChecked
            , Html.Events.onClick msg
            ]
            []
        , text value
        ]


viewPreview : ProgramData -> Html a
viewPreview data =
    case data.command of
        "" ->
            text ""

        _ ->
            let
                flags =
                    List.concat
                        [ if String.isEmpty data.nurserySize then
                            []
                          else
                            [ "-A" ++ data.nurserySize ]
                        , if String.isEmpty data.largeObjectSize then
                            []
                          else
                            [ "-AL" ++ data.largeObjectSize ]
                        , if data.compaction then
                            [ "-c" ]
                          else
                            []
                        , if String.isEmpty data.oldGenFactor then
                            []
                          else
                            [ "-F" ++ data.oldGenFactor ]
                        , if String.isEmpty data.nurseryChunks then
                            []
                          else
                            [ "-n" ++ data.nurseryChunks ]
                        , if String.isEmpty data.oldGenMinSize then
                            []
                          else
                            [ "-o" ++ data.oldGenMinSize ]
                        , if data.eventlog then
                            [ "-l" ]
                          else
                            []
                        , if data.prof then
                            [ "-pa" ]
                          else
                            []
                        , if data.stats then
                            [ "-S" ]
                          else
                            []
                        ]
            in
                if List.isEmpty flags then
                    text data.command
                else
                    text <|
                        String.join " " <|
                            data.command
                                :: "+RTS"
                                :: flags


viewFlagsRequired : ProgramData -> Html a
viewFlagsRequired data =
    let
        flags : List String
        flags =
            List.concat
                [ if data.eventlog then
                    [ "-eventlog" ]
                  else
                    []
                , if data.prof then
                    [ "-prof" ]
                  else
                    []
                , if
                    String.isEmpty data.nurserySize
                        && String.isEmpty data.nurseryChunks
                        && String.isEmpty data.largeObjectSize
                        && String.isEmpty data.oldGenMinSize
                        && String.isEmpty data.oldGenFactor
                        && not data.compaction
                  then
                    []
                  else
                    [ "-rtsopts" ]
                ]
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


viewStats : List GCStats -> Html msg
viewStats garbageCollections =
    let
        timeBucketedGCs : List { totalTimeElapsed : Float, averageLiveBytes : Float, generation : Int }
        timeBucketedGCs =
            garbageCollections
                |> groupBy (\prev curr -> curr.totalTime.elapsed == prev.totalTime.elapsed)
                |> List.concatMap
                    (\gcsAtTime ->
                        gcsAtTime
                            |> Nonempty.toList
                            |> List.sortBy .generation
                            |> groupBy (\a b -> a.generation == b.generation)
                            |> List.map
                                (\(Nonempty { generation, totalTime, liveBytes } gcs) ->
                                    { totalTimeElapsed = totalTime.elapsed
                                    , averageLiveBytes =
                                        toFloat (liveBytes + List.sum (List.map .liveBytes gcs))
                                            / (1 + (toFloat <| List.length gcs))
                                    , generation = generation
                                    }
                                )
                    )
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


renderLiveBytesSVG : List { averageLiveBytes : Float, totalTimeElapsed : Float, generation : Int } -> Svg msg
renderLiveBytesSVG stats =
    let
        ( getx, gety ) =
            ( .totalTimeElapsed, .averageLiveBytes )

        xscale : ContinuousScale
        xscale =
            let
                xmax : Float
                xmax =
                    stats
                        |> List.last
                        |> Maybe.unwrap 0 getx
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

        -- addPoint : _ -> VPath.Path -> VPath.Path
        addPoint stats =
            VPath.moveTo (VScale.convert xscale (getx stats)) (VScale.convert yscale (gety stats))
                >> VPath.lineTo (VScale.convert xscale (getx stats)) (VScale.convert yscale (gety stats + 1))
    in
        Svg.svg
            [ Svg.Attributes.width (toString (width + margin.left + margin.right))
            , Svg.Attributes.height (toString (height + margin.top + margin.bottom))
            ]
            [ Svg.g
                [ transformTranslate ( margin.left, margin.top ) ]
                [ Svg.g [ transformTranslate ( 0, height ) ] [ xaxis ]
                , Svg.g [] [ yaxis ]
                , Svg.path
                    [ Svg.Attributes.d
                        (stats
                            |> List.foldl addPoint VPath.begin
                            |> VPath.close
                            |> VPath.toAttrString
                        )
                    , Svg.Attributes.stroke "blue"
                    , Svg.Attributes.strokeWidth "50"
                    ]
                    []
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
