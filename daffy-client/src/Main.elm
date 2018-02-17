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
import Step exposing (Step)
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
    , stats : Bool
    , prof : Bool
    , eventlog : Bool
    , runs : Array ProgramRun
    }


type alias ProgramOutput =
    { output : List Output
    , stats : Maybe Stats
    , ticks : Maybe SvgPath
    , bytes : Maybe SvgPath
    }


type alias Output =
    { line : String
    , stdout : Bool
    }


type alias SvgPath =
    String


type alias ProgramRun =
    { output : List Output
    , exitCode : Int
    , ticks : Maybe SvgPath
    , bytes : Maybe SvgPath
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
    | GotTicks SvgPath
    | GotBytes SvgPath


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
            Step.to (RunningProgram model_ { output = [], stats = Nothing, ticks = Nothing, bytes = Nothing })
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
            Debug.crash "Invalid message"


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

                        "ticks-flamegraph" ->
                            payload string
                                |> Json.Decode.map GotTicks

                        "bytes-flamegraph" ->
                            payload string
                                |> Json.Decode.map GotBytes

                        _ ->
                            Json.Decode.fail "couldn't parse message from server"
                )


decodeOutput : Decoder Output
decodeOutput =
    map2 Output (field "line" string) (field "stdout" bool)


type alias ParseErr =
    String



-- type Step model msg output =
--     To model (Cmd msg)
--     | Noop
--     | Output output
-- stepRunningProgram : RunningProgramMsg -> ProgramOutput -> Step ProgramOutput msg (Result ParseErr ProgramRun)


stepRunningProgram : RunningProgramMsg -> ProgramOutput -> Step ProgramOutput msg (Result String ProgramRun)
stepRunningProgram programRunMsg ({ output, stats, ticks, bytes } as programOutput) =
    case programRunMsg of
        OutputMsg line ->
            Step.to { programOutput | output = line :: output }

        ExitedWith code ->
            Step.exit (Ok { output = output, stats = stats, exitCode = code, ticks = ticks, bytes = bytes })

        RunStats stats ->
            Step.to { programOutput | stats = Just stats }

        RunMsgParseErr err ->
            Step.exit (Err err)

        GotTicks ticks ->
            Step.to { programOutput | ticks = Just ticks }

        GotBytes bytes ->
            Step.to { programOutput | bytes = Just bytes }


view : Model -> Html Msg
view model =
    let
        viewProgramOutput programData { output } =
            div [ class "command-form" ]
                [ span [ class "ps1" ] [ text <| "$ " ++ programData.command ]
                , p [] [ text <| String.join "\n" (List.reverse (List.map .line output)) ]
                ]
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
                        [ viewProgramOutput programData programOutput
                        ]

                    MsgParseError parseError ->
                        [ div [] [ text <| "error parsing messages from daffy: " ++ parseError ] ]

                    ExploringRun programData programRun ->
                        [ button [ class "btn btn-back", type_ "button", Html.Events.onClick StartNewRun ] [ text "Back" ]
                        , viewProgramOutput programData programRun
                        , Maybe.map (\path -> object [ class "flame-svg", Html.Attributes.attribute "data" path ] []) programRun.ticks
                            |> Maybe.withDefault (text "")
                        , Maybe.map
                            (\path -> object [ class "flame-svg", Html.Attributes.attribute "data" path ] [])
                            programRun.bytes
                            |> Maybe.withDefault (text "")
                        , case programRun.stats of
                            Just stats ->
                                div []
                                    [ LineChart.viewCustom (chartConfig (toFloat << .liveBytes))
                                        [ LineChart.line (Color.rgb 255 99 71)
                                            Dots.none
                                            "Last Run"
                                            stats.garbageCollections
                                        ]
                                    , LineChart.viewCustom (chartConfig (toFloat << .bytesCopied))
                                        [ LineChart.line (Color.rgb 255 99 71)
                                            Dots.none
                                            "Last Run"
                                            stats.garbageCollections
                                        ]
                                    , div [] [ text <| "Length: " ++ toString (List.length stats.garbageCollections) ]
                                    ]

                            Nothing ->
                                text ""
                        ]


chartConfig : (GCStats -> Float) -> LineChart.Config GCStats msg
chartConfig y =
    { x = Axis.default 1000 "Time Elapsed" (.totalTime >> .elapsed)
    , y = Axis.default 600 "Bytes" y
    , container = Container.responsive "line-chart-1"
    , interpolation = Interpolation.default
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
