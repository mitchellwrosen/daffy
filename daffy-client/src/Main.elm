module Main exposing (..)

import Daffy.ElapsedTimeGCStats exposing (ElapsedTimeGCStats)
import Daffy.Html exposing (checkbox, radio, textInput)
import Daffy.List.Extra as List
import Daffy.Scatterplot
import Daffy.Setters exposing (..)
import Daffy.RunSpec exposing (RunSpec)
import Daffy.Types exposing (..)
import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_)
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
import List.Nonempty as Nonempty exposing (Nonempty)
import Maybe.Extra as Maybe
import Step exposing (Step)
import Svg exposing (Svg)
import Svg.Attributes
import Visualization.Axis as VAxis
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


type alias Model =
    { runSpec : RunSpec, runs : List ProgramRun }


type ProgramRun
    = RunningProgram RunSpec ProgramOutput
    | MsgParseError RunSpec ProgramOutput String
    | ExploringRun RunSpec ProgramOutput { exitCode : Int }


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


type Msg
    = ConfigureRun ConfigureRunMsg
    | RunCommand
    | RunningProgramMsg RunningProgramMsg


type ConfigureRunMsg
    = TypeCommand String
    | TypeNurserySize String
    | TypeNurseryChunks String
    | TypeLargeObjectSize String
    | TypeOldGenMinSize String
    | TypeOldGenFactor String
    | ToggleCompaction
    | ToggleStats
    | ToggleProf
    | ToggleEventlog


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
    , runs = []
    }


update : Msg -> Model -> Step Model Msg Never
update msg model =
    case msg of
        ConfigureRun runMsg ->
            Step.to { model | runSpec = configureRun runMsg model.runSpec }

        RunningProgramMsg runningProgramMsg ->
            case model.runs of
                (RunningProgram runSpec programOutput) :: moreRuns ->
                    stepRunningProgram runningProgramMsg programOutput
                        |> Step.map (RunningProgram runSpec)
                        |> Step.onExit
                            (\o ->
                                Step.to <|
                                    case o of
                                        Ok exitCode ->
                                            ExploringRun runSpec programOutput exitCode

                                        Err parseErr ->
                                            MsgParseError runSpec programOutput parseErr
                            )
                        |> Step.map (\x -> { model | runs = x :: moreRuns })

                _ ->
                    Step.noop

        RunCommand ->
            model
                |> over runsS
                    (List.cons
                        (RunningProgram
                            model.runSpec
                            { output = Array.empty
                            , stats = Nothing
                            , flamegraphs = []
                            }
                        )
                    )
                |> Step.to
                |> Step.withCmd
                    ([ ( "command", Json.Encode.string model.runSpec.command )
                     , ( "stats", Json.Encode.bool model.runSpec.stats )
                     , ( "prof", Json.Encode.bool model.runSpec.prof )
                     , ( "eventlog", Json.Encode.bool model.runSpec.eventlog )
                     ]
                        |> Json.Encode.object
                        |> Json.Encode.encode 0
                        |> WebSocket.send "ws://localhost:8080"
                    )


configureRun : ConfigureRunMsg -> RunSpec -> RunSpec
configureRun runMsg =
    case runMsg of
        TypeCommand s ->
            set commandS s

        TypeNurserySize s ->
            set nurserySizeS s

        TypeNurseryChunks s ->
            set nurseryChunksS s

        TypeLargeObjectSize s ->
            set largeObjectSizeS s

        TypeOldGenMinSize s ->
            set oldGenMinSizeS s

        TypeOldGenFactor s ->
            set oldGenFactorS s

        ToggleCompaction ->
            over compactionS not

        ToggleStats ->
            over statsS not

        ToggleProf ->
            over profS not

        ToggleEventlog ->
            over eventlogS not


subscriptions : Model -> Sub Msg
subscriptions model =
    case List.head model.runs of
        Just (RunningProgram runningProgram _) ->
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


stepRunningProgram : RunningProgramMsg -> ProgramOutput -> Step ProgramOutput msg (Result ParseErr { exitCode : Int })
stepRunningProgram programRunMsg programOutput =
    case programRunMsg of
        OutputMsg line ->
            Step.to { programOutput | output = Array.push line programOutput.output }

        ExitedWith code ->
            Step.exit (Ok { exitCode = code })

        RunStats stats ->
            Step.to { programOutput | stats = Just stats }

        RunMsgParseErr err ->
            Step.exit (Err err)

        FlamegraphMsg flamegraph ->
            Step.to { programOutput | flamegraphs = flamegraph :: programOutput.flamegraphs }


view : Model -> Html Msg
view ({ runSpec } as model) =
    div [ class "container" ] <|
        List.concat
            [ [ h1 [ class "heading" ] [ text "🔥 daffy 🔥" ] ]
            , [ Html.form [ class "command-form", Html.Events.onSubmit RunCommand ]
                    << List.map (Html.map ConfigureRun)
                <|
                    [ div [ class "form-group prompt-group" ]
                        [ span [ class "ps1" ] [ text "$" ]
                        , textInput runSpec.command
                            TypeCommand
                            [ Html.Attributes.autofocus True ]
                        ]
                    , viewPreview runSpec
                    , div [ class "form-pair" ]
                        [ div [ class "form-group" ]
                            [ label
                                []
                                [ text "Nursery size" ]
                            , textInput
                                runSpec.nurserySize
                                TypeNurserySize
                                [ class "tiny"
                                , Html.Attributes.placeholder "1m"
                                ]
                            , span
                                [ class "inline-label" ]
                                [ text "split into" ]
                            , textInput
                                runSpec.nurseryChunks
                                TypeNurseryChunks
                                [ class "tiny" ]
                            , span [ class "inline-label" ]
                                [ text "chunks" ]
                            ]
                        , div [ class "form-group" ]
                            [ label
                                []
                                [ text "Large object size" ]
                            , textInput
                                runSpec.largeObjectSize
                                TypeLargeObjectSize
                                [ Html.Attributes.placeholder <|
                                    if String.isEmpty runSpec.nurserySize then
                                        "1m"
                                    else
                                        runSpec.nurserySize
                                ]
                            ]
                        ]
                    , div [ class "form-pair" ]
                        [ div [ class "form-group" ]
                            [ label
                                []
                                [ text "Minimum old generation size" ]
                            , textInput
                                runSpec.oldGenMinSize
                                TypeOldGenMinSize
                                [ Html.Attributes.placeholder "1m" ]
                            ]
                        , div [ class "form-group" ]
                            [ label
                                []
                                [ text "Old generation factor" ]
                            , textInput
                                runSpec.oldGenFactor
                                TypeOldGenFactor
                                [ Html.Attributes.placeholder "2" ]
                            ]
                        ]
                    , div [ class "form-group" ]
                        [ span
                            [ class "inline-label" ]
                            [ text "Collect oldest generation by" ]
                        , fieldset [] <|
                            List.concat
                                [ radio "Copying"
                                    (runSpec.compaction == False)
                                    ToggleCompaction
                                , radio "Compacting"
                                    (runSpec.compaction == True)
                                    ToggleCompaction
                                ]
                        ]
                    , div [ class "form-group" ]
                        [ label
                            [ class "inline-label" ]
                            (checkbox "stats" runSpec.stats ToggleStats [])
                        , label
                            [ class "inline-label" ]
                            (checkbox "profile" runSpec.prof ToggleProf [])
                        , label
                            [ class "inline-label" ]
                            (checkbox "eventlog" runSpec.eventlog ToggleEventlog [])
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
              , viewFlagsRequired runSpec
              ]
            , case List.head model.runs of
                Just (RunningProgram programData programOutput) ->
                    [ viewOutput programOutput ]

                Just (MsgParseError _ _ parseError) ->
                    [ div [] [ text <| "error parsing messages from daffy: " ++ parseError ] ]

                Just (ExploringRun runSpec programOutput exitCode) ->
                    [ viewOutput programOutput ]
                        ++ List.map (\path -> object [ class "flame-svg", Html.Attributes.attribute "data" path ] [])
                            programOutput.flamegraphs
                        ++ Maybe.unwrap [] (List.singleton << viewStats) programOutput.stats

                _ ->
                    []
            ]


viewPreview : RunSpec -> Html a
viewPreview runSpec =
    div [ class "command-preview" ]
        [ span [ class "ps1" ] [ text "$" ]
        , text <| Daffy.RunSpec.preview runSpec
        ]


viewFlagsRequired : RunSpec -> Html a
viewFlagsRequired spec =
    case Daffy.RunSpec.ghcFlags spec of
        [] ->
            text ""

        flags ->
            div [ class "flags-required" ]
                [ span [ class "flags-required__preface" ]
                    [ text "Note: your program must be compiled with: " ]
                , span [ class "flags-required__flags" ]
                    [ text <| String.join " " flags ]
                ]


viewOutput : { r | output : Array Output } -> Html a
viewOutput { output } =
    div [ class "ps1" ]
        (output
            |> Array.toList
            |> List.map (p [] << List.singleton << text << .line)
        )


viewStats : Stats -> Html msg
viewStats stats =
    case Nonempty.fromList (Daffy.ElapsedTimeGCStats.make stats.garbageCollections) of
        Nothing ->
            text ""

        Just elapsedTimeGCs ->
            div []
                [ viewBytesAllocatedSvg elapsedTimeGCs
                , viewBytesCopiedSvg elapsedTimeGCs
                , viewLiveBytesSvg elapsedTimeGCs
                , viewNumGCsSvg elapsedTimeGCs

                --   LineChart.viewCustom (chartConfig .totalTimeElapsed .averageLiveBytes)
                --     [ LineChart.line (Color.rgb 255 99 71)
                --         Dots.none
                --         "Last Run"
                --         elapsedTimeGCs
                --     ]
                -- , div [] [ text <| "Length: " ++ toString (List.length timeBucketedGCs) ]
                ]



-- chartConfig : (data -> Float) -> (data -> Float) -> LineChart.Config data msg
-- chartConfig x y =
--     { x = Axis.default 1000 "Time Elapsed" x
--     , y = Axis.default 600 "Bytes" y
--     , container = Container.responsive "line-chart-1"
--     , interpolation = Interpolation.stepped
--     , intersection = Intersection.default
--     , legends = Legends.default
--     , events = Events.default
--     , junk = Junk.default
--     , grid = Grid.default
--     , area = Area.default
--     , line = Line.default
--     , dots = Dots.custom (Dots.full 0)
--     }


width =
    800


height =
    500


margin =
    { bottom = 25
    , top = 10
    , left = 60
    , right = 20
    }


viewBytesAllocatedSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewBytesAllocatedSvg =
    Daffy.Scatterplot.svg
        { width = width
        , height = height
        , margin = margin
        , extent = set xminS 0 >> over yminS (max 0)
        , getX = .time
        , getY = \x -> toFloat (x.bytesAllocated // x.count)
        , getR = \x -> x.generation + 2
        }


viewBytesCopiedSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewBytesCopiedSvg =
    Daffy.Scatterplot.svg
        { width = width
        , height = height
        , margin = margin
        , extent = set xminS 0 >> over yminS (max 0)
        , getX = .time
        , getY = \x -> toFloat (x.bytesCopied // x.count)
        , getR = \x -> x.generation + 2
        }


viewLiveBytesSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewLiveBytesSvg =
    Daffy.Scatterplot.svg
        { width = width
        , height = height
        , margin = margin
        , extent = set xminS 0 >> over yminS (max 0)
        , getX = .time
        , getY = \x -> toFloat (x.liveBytes // x.count)
        , getR = \x -> x.generation + 2
        }


viewNumGCsSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewNumGCsSvg =
    Daffy.Scatterplot.svg
        { width = width
        , height = height
        , margin = margin
        , extent = set xminS 0 >> over yminS (max 0)
        , getX = .time
        , getY = toFloat << .count
        , getR = \x -> x.generation + 2
        }
