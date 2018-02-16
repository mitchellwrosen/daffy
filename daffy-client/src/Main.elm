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
    , runs : Array ProgramRun
    }


type alias ProgramOutput =
    { stderr : List String
    , stdout : List String
    , stats : Maybe Stats
    , ticks : Maybe SvgPath
    , bytes : Maybe SvgPath
    }


type alias SvgPath =
    String


type alias ProgramRun =
    { stdout : List String
    , stderr : List String
    , exitCode : Int
    , ticks : Maybe SvgPath
    , bytes : Maybe SvgPath
    , stats : Maybe Stats
    }


type Msg
    = TypeCommand String
    | ToggleStats Bool
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
    = StdOutLine String
    | StdErrLine String
    | RunStats Stats
    | ExitedWith Int
    | RunMsgParseErr String
    | GotTicks SvgPath
    | GotBytes SvgPath


init : Model
init =
    Initial { command = "", stats = True, runs = Array.empty }


update : Msg -> Model -> Step Model Msg Never
update msg model =
    case ( model, msg ) of
        ( Initial model_, TypeCommand s ) ->
            Step.to (Initial { model_ | command = s })

        ( Initial model_, ToggleStats b ) ->
            Step.to (Initial { model_ | stats = b })

        ( Initial model_, ExploreRun index ) ->
            case Array.get index model_.runs of
                Just programRun ->
                    Step.to (ExploringRun model_ programRun)

                Nothing ->
                    Step.noop

        ( Initial model_, RunCommand ) ->
            Step.to (RunningProgram model_ { stderr = [], stdout = [], stats = Nothing, ticks = Nothing, bytes = Nothing })
                |> Step.withCmd
                    ([ ( "command", Json.Encode.string model_.command )
                     , ( "stats", Json.Encode.bool model_.stats )
                     , ( "eventlog", Json.Encode.bool False )
                     , ( "prof", Json.Encode.bool True )
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
                        "stdout" ->
                            payload string
                                |> Json.Decode.map StdOutLine

                        "stderr" ->
                            payload string
                                |> Json.Decode.map StdErrLine

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


type alias ParseErr =
    String



-- type Step model msg output =
--     To model (Cmd msg)
--     | Noop
--     | Output output
-- stepRunningProgram : RunningProgramMsg -> ProgramOutput -> Step ProgramOutput msg (Result ParseErr ProgramRun)


stepRunningProgram : RunningProgramMsg -> ProgramOutput -> Step ProgramOutput msg (Result String ProgramRun)
stepRunningProgram programRunMsg ({ stdout, stderr, stats, ticks, bytes } as programOutput) =
    case programRunMsg of
        StdOutLine line ->
            Step.to { programOutput | stdout = line :: stdout }

        StdErrLine line ->
            Step.to { programOutput | stderr = line :: stderr }

        ExitedWith code ->
            Step.exit (Ok { stdout = stdout, stderr = stderr, stats = stats, exitCode = code, ticks = ticks, bytes = bytes })

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
        viewProgramOutput programData { stdout, stderr } =
            let
                whatMatters =
                    if (List.length stderr) > 0 then
                        stderr
                    else
                        stdout
            in
                div [ class "command-form" ]
                    [ span [ class "ps1" ] [ text <| "$ " ++ programData.command ]
                    , p [] [ text <| String.join "\n" (List.reverse whatMatters) ]
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



-- Geting Standart Out And Error and exit -> onExit waitingForStats -> Done
{-
   "type" = "stdout" | "stderr" | "exitcode" | "stats"
   "payload" = (stdout|stderr -> String) ("exitcode" -> Int) (stats -> Maybe Stats)

-}


testStats : Stats
testStats =
    { garbageCollections =
        -- each GC creates an entry
        [ { bytesAllocated = 0
          , bytesCopied = 0
          , liveBytes = 0
          , time = { user = 0.0, elapsed = 0.0 }
          , totalTime = { user = 0.0, elapsed = 0.0 }
          , pageFaults = 0
          , totalPageFaults = 0
          , generation = 0 -- might want to filter by generation
          }
        , { bytesAllocated = 0
          , bytesCopied = 0
          , liveBytes = 0
          , time = { user = 0.0, elapsed = 0.0 }
          , totalTime = { user = 0.0, elapsed = 0.0 }
          , pageFaults = 0
          , totalPageFaults = 0
          , generation = 1
          }
        ]
    , generationSummaries =
        [ { parallel = 0
          , averagePauseTime = 0.0
          , maxPauseTime = 0.0
          }
        ]
    , parallelGarbageCollection =
        Just
            { parallel = 0.0
            , serial = 0.0
            , perfect = 0.0
            }
    , tasks =
        Just
            { tasks = 0
            , bound = 0
            , peakWorkers = 0
            , totalWorkers = 0
            }
    , sparks =
        Just
            { sparks = 0
            , converted = 0
            , overflowed = 0
            , dud = 0
            , garbageCollected = 0
            , fizzled = 0
            }
    , runtimeInitTime = { user = 0.0, elapsed = 0.0 }
    , mutatorTime = { user = 0.0, elapsed = 0.0 }
    , garbageCollectionTime = { user = 0.0, elapsed = 0.0 }
    , retainerProfilingTime = Just { user = 0.0, elapsed = 0.0 }
    , otherProfilingTime = Just { user = 0.0, elapsed = 0.0 }
    , runtimeShutdownTime = { user = 0.0, elapsed = 0.0 }
    , totalTime = { user = 0.0, elapsed = 0.0 }
    , percentGarbageCollectionTime = Just { user = 0.0, elapsed = 0.0 }
    }
