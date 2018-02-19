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
import LineChart.Dots as Dots exposing (Shape)
import LineChart.Axis as Axis
import LineChart.Container as Container
import LineChart.Interpolation as Interpolation
import LineChart.Legends as Legends
import LineChart.Events as Events
import LineChart.Junk as Junk
import LineChart.Grid as Grid
import LineChart.Area as Area
import LineChart.Line as Line
import LineChart.Dots as Dots
import LineChart.Axis.Intersection as Intersection
import List.Extra as List
import Maybe.Extra as Maybe
import Step exposing (Step)
import Svg exposing (Svg)
import Svg.Attributes
import Visualization.Axis as VAxis
import Visualization.Scale as VScale exposing (ContinuousScale)
import WebSocket
import List.Nonempty as Nonempty exposing (Nonempty(Nonempty))


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
    , stats : Bool
    , prof : Bool
    , eventlog : Bool
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
    Initial { command = "", stats = True, prof = False, eventlog = False, runs = Array.empty }


update : Msg -> Model -> Step Model Msg Never
update msg model =
    case ( model, msg ) of
        ( Initial model_, TypeCommand s ) ->
            Step.to (Initial { model_ | command = s })

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
            [ h1 [ class "heading" ] [ text "🔥 daffy 🔥" ]
            ]
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
                            , div [ class "form-group" ]
                                [ input [ class "btn", type_ "submit", Html.Attributes.value "Run" ] []
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
                            ++ [ Maybe.map viewStats programRun.stats
                                    |> Maybe.withDefault (text "")
                               ]


viewStats : Stats -> Html msg
viewStats stats =
    let
        timeBucketedGCs : List { totalTimeElapsed : Float, averageLiveBytes : Float }
        timeBucketedGCs =
            stats.garbageCollections
                |> groupBy (\prev curr -> curr.totalTime.elapsed == prev.totalTime.elapsed)
                |> List.map
                    (\(Nonempty firstGC groupedGCs) ->
                        { totalTimeElapsed = firstGC.totalTime.elapsed
                        , averageLiveBytes =
                            toFloat (firstGC.liveBytes + List.sum (List.map .liveBytes groupedGCs))
                                / (1 + (toFloat <| List.length groupedGCs))
                        }
                    )
    in
        div []
            [ LineChart.viewCustom (chartConfig .totalTimeElapsed .averageLiveBytes)
                [ LineChart.line (Color.rgb 255 99 71)
                    Dots.none
                    "Last Run"
                    timeBucketedGCs
                ]
            , renderLiveBytesSVG timeBucketedGCs
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


buttonStyle : String
buttonStyle =
    "border shadow rounded px-2 py-1"


gcTableConfig : List (Column GCStats)
gcTableConfig =
    [ Column (.bytesAllocated >> toString) "Bytes Allocated"
    , Column (.bytesCopied >> toString) "Bytes Copied"
    , Column (.liveBytes >> toString) "Live Bytes"
    , Column (.time >> toString) "Time Spent"
    , Column (.totalTime >> toString) "Total Time"
    , Column (.pageFaults >> toString) "Page Faults"
    , Column (.totalPageFaults >> toString) "Total Page Faults"
    , Column (.generation >> toString) "Generation"
    ]


type alias Column a =
    { renderColumn : a -> String, name : String }


textFilled : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> Html msg
textFilled node str =
    node [] [ text str ]


apply : a -> (a -> b) -> b
apply =
    flip identity


viewTable : List (Column a) -> List a -> Html msg
viewTable columns elements =
    table [ class "my-4" ]
        [ columns
            |> List.map (.name >> textFilled th)
            |> tr []
            |> List.singleton
            |> thead []
        , elements
            |> List.map
                (\element ->
                    columns
                        |> List.map (.renderColumn >> apply element >> textFilled td)
                        |> tr []
                )
            |> tbody []
        ]


margin : { bottom : number, left : number1, right : number2, top : number3 }
margin =
    { top = 20, right = 40, bottom = 20, left = 40 }


( width, height ) =
    ( 800 - margin.left - margin.right, 500 - margin.top - margin.bottom )


renderLiveBytesSVG : List { averageLiveBytes : Float, totalTimeElapsed : Float } -> Svg msg
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

        -- point : GCStats -> Svg msg
        point stats =
            Svg.g
                []
                [ Svg.circle
                    [ Svg.Attributes.cx <|
                        toString <|
                            VScale.convert xscale (getx stats)
                    , Svg.Attributes.cy <|
                        toString <|
                            VScale.convert yscale (gety stats)
                    , Svg.Attributes.r "1"
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
