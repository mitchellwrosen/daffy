module Daffy.Model exposing (..)

import Array exposing (Array)
import Daffy.Types exposing (..)
import Daffy.RunSpec exposing (RunSpec)
import Set exposing (Set)
import Step exposing (Step)
import Daffy.Proto.RunReq exposing (..)
import WebSocket
import List.Extra
import Daffy.Setters exposing (..)


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


type alias ParseErr =
    String


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
