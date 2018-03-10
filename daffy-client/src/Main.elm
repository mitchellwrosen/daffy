module Main exposing (..)

import Array exposing (Array)
import Daffy.Basics exposing (..)
import Daffy.ElapsedTimeGCStats exposing (ElapsedTimeGCStats)
import Daffy.Html exposing (checkbox, radio, textInput)
import Daffy.Proto.RunReq
import Daffy.RunSpec exposing (RunSpec)
import Daffy.Scatterplot
import Daffy.Setters exposing (..)
import Daffy.Types exposing (..)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_)
import Html.Events
import Json.Decode exposing (..)
import List.Extra
import List.Nonempty as Nonempty exposing (Nonempty)
import Maybe.Extra as Maybe
import Set exposing (Set)
import Step exposing (Step)
import Svg exposing (Svg)
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
    { runSpec : RunSpec
    , runs : List ProgramRun
    }


type alias ProgramRun =
    { spec : RunSpec
    , output : ProgramOutput
    , state : RunState
    }


type RunState
    = RunningProgram
    | MsgParseError String
    | ExploringRun { exitCode : Int, visibleGens : Set Int }


type alias ProgramOutput =
    { lines : Array Output
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
    | ToggleGen Int
    | SelectAllGens


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
                firstRun :: moreRuns ->
                    let
                        initExploration res =
                            case res of
                                Ok { exitCode } ->
                                    { firstRun
                                        | state =
                                            ExploringRun
                                                { exitCode = exitCode
                                                , visibleGens =
                                                    firstRun.output.stats
                                                        |> Maybe.map
                                                            (generations
                                                                >> Set.fromList
                                                            )
                                                        |> Maybe.withDefault Set.empty
                                                }
                                    }

                                Err parseErr ->
                                    { firstRun | state = MsgParseError parseErr }
                    in
                        Step.map (\x -> { model | runs = x :: moreRuns }) <|
                            case firstRun.state of
                                RunningProgram ->
                                    stepRunningProgram runningProgramMsg firstRun.output
                                        |> Step.map (\programOutput -> { firstRun | output = programOutput })
                                        |> Step.onExit (Step.to << initExploration)

                                _ ->
                                    Step.noop

                _ ->
                    Step.noop

        RunCommand ->
            { model
                | runSpec = model.runSpec
                , runs = { state = RunningProgram, spec = model.runSpec, output = { lines = Array.empty, stats = Nothing, flamegraphs = [] } } :: model.runs
            }
                |> Step.to
                |> Step.withCmd
                    (model.runSpec
                        |> Daffy.RunSpec.toReq
                        |> Daffy.Proto.RunReq.encode
                        |> WebSocket.send "ws://localhost:8080"
                    )

        ToggleGen gen ->
            model
                |> mapExploringRun
                    (\_ ({ visibleGens } as exploringRunData) ->
                        { exploringRunData
                            | visibleGens =
                                if Set.member gen visibleGens && (Set.size visibleGens > 1) then
                                    Set.remove gen visibleGens
                                else
                                    Set.insert gen visibleGens
                        }
                    )

        SelectAllGens ->
            model
                |> mapExploringRun
                    (\output erd ->
                        { erd
                            | visibleGens =
                                output.stats
                                    |> Maybe.map (generations >> Set.fromList)
                                    |> Maybe.withDefault Set.empty
                        }
                    )


generations : Stats -> List Int
generations { generationSummaries } =
    List.range 0 <| List.length generationSummaries - 1


maybeStep : Maybe a -> Step a msg o
maybeStep aMaybe =
    case aMaybe of
        Just a ->
            Step.to a

        Nothing ->
            Step.noop


mapExploringRun : (ProgramOutput -> { exitCode : Int, visibleGens : Set Int } -> { exitCode : Int, visibleGens : Set Int }) -> Model -> Step Model msg o
mapExploringRun f model =
    model.runs
        |> List.Extra.uncons
        |> Maybe.andThen
            (\( run, runs ) ->
                case run.state of
                    ExploringRun erd ->
                        Just { model | runs = { run | state = ExploringRun (f run.output erd) } :: runs }

                    _ ->
                        Nothing
            )
        |> maybeStep


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
    case List.head model.runs |> Maybe.map .state of
        Just RunningProgram ->
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
            Step.to { programOutput | lines = Array.push line programOutput.lines }

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
                Just { spec, output, state } ->
                    case state of
                        RunningProgram ->
                            [ viewOutput output.lines ]

                        MsgParseError parseError ->
                            [ div [] [ text <| "error parsing messages from daffy: " ++ parseError ] ]

                        ExploringRun { visibleGens, exitCode } ->
                            [ viewOutput output.lines ]
                                ++ List.map
                                    (\path ->
                                        object
                                            [ class "flame-svg", Html.Attributes.attribute "data" path ]
                                            []
                                    )
                                    output.flamegraphs
                                ++ Maybe.unwrap [] (List.singleton << viewStats visibleGens) output.stats

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


viewOutput : Array Output -> Html a
viewOutput lines =
    div [ class "program-output" ]
        (lines
            |> Array.toList
            |> List.map (p [] << List.singleton << text << .line)
        )


viewStats : Set Int -> Stats -> Html Msg
viewStats visibleGens stats =
    case
        Daffy.ElapsedTimeGCStats.make stats.garbageCollections
            |> List.filter (.generation >> \x -> Set.member x visibleGens)
            |> Nonempty.fromList
    of
        Nothing ->
            text ""

        Just elapsedTimeGCs ->
            div []
                [ generationPicker visibleGens stats
                , viewBytesAllocatedSvg elapsedTimeGCs
                , viewTotalBytesAllocatedSvg elapsedTimeGCs
                , viewBytesCopiedSvg elapsedTimeGCs
                , viewTotalBytesCopiedSvg elapsedTimeGCs
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


generationPicker : Set Int -> Stats -> Html Msg
generationPicker visibleGens stats =
    generations stats
        |> List.concatMap
            (\gen ->
                let
                    isChecked =
                        Set.member gen visibleGens
                in
                    checkbox ("Generation " ++ toString gen)
                        isChecked
                        (ToggleGen gen)
                        [ Html.Attributes.disabled (isChecked && (Set.size visibleGens == 1)) ]
            )
        |> div [ class "generation_picker" ]



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


width : number
width =
    1100


height : number
height =
    500


formatSeconds : Float -> String
formatSeconds =
    FormatNumber.format FormatNumber.Locales.usLocale


formatBytes : Float -> String
formatBytes x =
    let
        y =
            x / 1024
    in
        if y >= 1024 then
            toString (round (y / 1024)) ++ "M"
        else
            toString (round y) ++ "K"


viewBytesAllocatedSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewBytesAllocatedSvg =
    Daffy.Scatterplot.svg
        { title = "Bytes allocated during GC"
        , width = width
        , height = height
        , extent = set xminS 0 >> over yminS (max 0)
        , line = False
        , getX = .time
        , getY = \x -> toFloat x.bytesAllocated
        , getR = \x -> x.generation + 1
        , showX = formatSeconds
        , showY = formatBytes
        }


viewTotalBytesAllocatedSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewTotalBytesAllocatedSvg =
    Daffy.Scatterplot.svg
        { title = "Total bytes allocated during GC"
        , width = width
        , height = height
        , extent = set xminS 0 >> over yminS (max 0)
        , line = True
        , getX = .time
        , getY = \x -> toFloat x.bytesAllocated
        , getR = \x -> x.generation + 1
        , showX = formatSeconds
        , showY = formatBytes
        }
        << Nonempty.scanl1
            (\x y -> over bytesAllocatedS (add y.bytesAllocated) x)


viewBytesCopiedSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewBytesCopiedSvg =
    Daffy.Scatterplot.svg
        { title = "Bytes copied during GC"
        , width = width
        , height = height
        , extent = set xminS 0 >> over yminS (max 0)
        , line = False
        , getX = .time
        , getY = \x -> toFloat x.bytesCopied
        , getR = \x -> x.generation + 1
        , showX = formatSeconds
        , showY = formatBytes
        }


viewTotalBytesCopiedSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewTotalBytesCopiedSvg =
    Daffy.Scatterplot.svg
        { title = "Total bytes copied during GC"
        , width = width
        , height = height
        , extent = set xminS 0 >> over yminS (max 0)
        , line = True
        , getX = .time
        , getY = \x -> toFloat x.bytesCopied
        , getR = \x -> x.generation + 1
        , showX = formatSeconds
        , showY = formatBytes
        }
        << Nonempty.scanl1 (\x y -> over bytesCopiedS (add y.bytesCopied) x)


viewLiveBytesSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewLiveBytesSvg =
    Daffy.Scatterplot.svg
        { title = "Live bytes"
        , width = width
        , height = height
        , extent = set xminS 0 >> over yminS (max 0)
        , line = True
        , getX = .time
        , getY = \x -> toFloat (x.liveBytes // x.count)
        , getR = always 1
        , showX = formatSeconds
        , showY = formatBytes
        }


viewNumGCsSvg : Nonempty ElapsedTimeGCStats -> Svg msg
viewNumGCsSvg =
    Daffy.Scatterplot.svg
        { title = "Total number of GCs"
        , width = width
        , height = height
        , extent = set xminS 0 >> over yminS (max 0)
        , line = False
        , getX = .time
        , getY = toFloat << .count
        , getR = always 1
        , showX = formatSeconds
        , showY =
            FormatNumber.format { usLocale | decimals = 0 }
        }
        << Nonempty.scanl1 (\x y -> over countS (add y.count) x)
