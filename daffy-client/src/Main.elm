module Main exposing (..)

import DaffyTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import HtmlParser.Util exposing (toVirtualDomSvg)
import Json.Decode exposing (..)
import Json.Encode
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
    = Initial { command : String, stats : Bool }
    | RunningProgram RunningProgram
    | ProgramFailed ProgramOutput { exitCode : Int }
    | ExploringRun ProgramRun


type alias ProgramRun =
    { stdout : List String
    , stderr : List String
    , exitCode : Int
    , stats : Stats
    }


type Msg
    = TypeCommand String
    | ToggleStats Bool
    | RunCommand
    | ProgramRunMsg ProgramRunMsg


type RunExplorationMsg
    = TypeFilter String
    | DeleteFilter
    | ConfirmFilter


type RunningProgram
    = StreamingOutput ProgramOutput
    | WaitingForStats ProgramOutput


type alias ProgramOutput =
    { stderr : List String, stdout : List String }


type ProgramRunMsg
    = StdOutLine String
    | StdErrLine String
    | RunStats Stats
    | ExitedWith Int
    | RunMsgParseErr String


init : Model
init =
    Initial { command = "", stats = False }


update : Msg -> Model -> Step Model Msg Never
update msg model =
    case ( model, msg ) of
        ( Initial model_, TypeCommand s ) ->
            Step.to (Initial { model_ | command = s })

        ( Initial model_, ToggleStats b ) ->
            Step.to (Initial { model_ | stats = b })

        ( Initial model_, RunCommand ) ->
            Step.to (RunningProgram (StreamingOutput { stderr = [], stdout = [] }))
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

        ( RunningProgram runningProgram, ProgramRunMsg programRunMsg ) ->
            stepRunningProgram programRunMsg runningProgram
                |> Step.map RunningProgram
                |> Step.mapMsg ProgramRunMsg
                |> Step.onExit
                    (\result ->
                        Step.to <|
                            case result of
                                Err ( output, code ) ->
                                    ProgramFailed output code

                                Ok run ->
                                    ExploringRun run
                    )

        _ ->
            Debug.crash "bad message"


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initial _ ->
            Sub.none

        ProgramFailed _ _ ->
            Sub.none

        RunningProgram runningProgram ->
            Sub.map ProgramRunMsg <|
                WebSocket.listen "ws://localhost:8080"
                    (\raw ->
                        case Json.Decode.decodeString decodeRunMsg raw of
                            Ok msg ->
                                msg

                            Err string ->
                                RunMsgParseErr string
                    )

        ExploringRun programRun ->
            Sub.none


decodeRunMsg : Decoder ProgramRunMsg
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

                        _ ->
                            Json.Decode.fail "couldn't parse message from server"
                )


stepRunningProgram : ProgramRunMsg -> RunningProgram -> Step RunningProgram msg (Result ( ProgramOutput, { exitCode : Int } ) ProgramRun)
stepRunningProgram programRunMsg runningProgram =
    case ( runningProgram, programRunMsg ) of
        ( StreamingOutput data, StdOutLine line ) ->
            Step.to (StreamingOutput { data | stdout = line :: data.stdout })

        ( StreamingOutput data, StdErrLine line ) ->
            Step.to (StreamingOutput { data | stderr = line :: data.stderr })

        ( StreamingOutput data, ExitedWith code ) ->
            case code of
                0 ->
                    Step.to (WaitingForStats data)

                nonZero ->
                    Step.exit (Err ( data, { exitCode = code } ))

        ( WaitingForStats data, RunStats stats ) ->
            Step.exit (Ok { stdout = data.stdout, stderr = data.stderr, exitCode = 0, stats = stats })

        ( _, RunMsgParseErr err ) ->
            Debug.log "parse error" err
                |> \_ -> Step.noop

        _ ->
            Step.noop


view : Model -> Html Msg
view model =
    let
        viewProgramOutput { stdout, stderr } =
            div []
                [ div []
                    [ h1 [] [ text "Standard Out" ]
                    , p [] [ text <| String.join "\n" (List.reverse stdout) ]
                    ]
                , div []
                    [ h1 [] [ text "Standard Error" ]
                    , p [] [ text <| String.join "\n" (List.reverse stderr) ]
                    ]
                ]
    in
        div [ class "p-8" ] <|
            case model of
                Initial model_ ->
                    [ div []
                        [ span [ class "text-lg mr-3" ] [ text "Command to Run" ]
                        , input
                            [ class "border"
                            , type_ "text"
                            , Html.Attributes.value model_.command
                            , Html.Events.onInput TypeCommand
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
                            , text "Stats"
                            ]
                        ]
                    , div [] [ button [ class "border shadow rounded px-2 py-1", type_ "button", Html.Events.onClick RunCommand ] [ text "Run" ] ]
                    ]

                RunningProgram runningProgram ->
                    [ case runningProgram of
                        StreamingOutput programOutput ->
                            viewProgramOutput programOutput

                        WaitingForStats programOutput ->
                            viewProgramOutput programOutput
                    ]

                ProgramFailed programOutput { exitCode } ->
                    [ div [] [ text <| "progam failed with exit code " ++ toString exitCode ]
                    , viewProgramOutput programOutput
                    ]

                ExploringRun programRun ->
                    [ viewProgramOutput programRun
                    , div []
                        [ h1 [] [ text "Garbage Collections" ]
                        , viewTable gcTableConfig programRun.stats.garbageCollections
                        ]
                    ]


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
