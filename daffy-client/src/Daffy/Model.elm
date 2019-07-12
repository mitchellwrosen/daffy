module Daffy.Model exposing (..)

import Array exposing (Array)
import Daffy.Proto.RunReq exposing (..)
import Daffy.RunSpec exposing (RunSpec)
import Daffy.Setters exposing (..)
import Daffy.Types exposing (..)
import List.Extra
import Set exposing (Set)
import Step exposing (Step)
import WebSocket


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
    | ExploringRun { exitCode : Int } ChartState


type alias ChartState =
    { bytesAllocated : Set Int
    , totalBytesAllocated : Set Int
    , bytesCopied : Set Int
    , totalBytesCopied : Set Int
    , liveBytes : Set Int
    }


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
    | ToggleGen Chart Int
    | SelectAllGens Chart


type Chart
    = BytesAllocated
    | TotalBytesAllocated
    | BytesCopied
    | TotalBytesCopied
    | LiveBytes


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
                                                { exitCode = exitCode }
                                                (ChartState allGens allGens allGens allGens allGens)
                                    }

                                Err parseErr ->
                                    { firstRun | state = MsgParseError parseErr }

                        allGens =
                            generationSet firstRun.output
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

        ToggleGen chart gen ->
            let
                updateGens visibleGens =
                    if Set.member gen visibleGens && (Set.size visibleGens > 1) then
                        Set.remove gen visibleGens
                    else
                        Set.insert gen visibleGens
            in
                mapExploringRun
                    (\_ chartState -> mapChart chart updateGens chartState)
                    model

        SelectAllGens chart ->
            mapExploringRun
                (\output chartState -> mapChart chart (\_ -> generationSet output) chartState)
                model


mapChart : Chart -> (Set Int -> Set Int) -> ChartState -> ChartState
mapChart chart f chartState =
    case chart of
        LiveBytes ->
            over liveBytesS f chartState

        BytesAllocated ->
            over bytesAllocatedS f chartState

        TotalBytesAllocated ->
            over totalBytesAllocatedS f chartState

        BytesCopied ->
            over bytesCopiedS f chartState

        TotalBytesCopied ->
            over totalBytesCopiedS f chartState


{-| get a set wth the one entry per generation
-}
generationSet : ProgramOutput -> Set Int
generationSet output =
    case output.stats of
        Just stats ->
            generations stats
                |> Set.fromList

        Nothing ->
            Set.empty


generations : { b | generationSummaries : List a } -> List Int
generations stats =
    List.range 0 (List.length stats.generationSummaries - 1)


maybeStep : Maybe a -> Step a msg o
maybeStep aMaybe =
    case aMaybe of
        Just a ->
            Step.to a

        Nothing ->
            Step.noop


mapExploringRun : (ProgramOutput -> ChartState -> ChartState) -> Model -> Step Model msg o
mapExploringRun f model =
    model.runs
        |> List.Extra.uncons
        |> Maybe.andThen
            (\( run, runs ) ->
                case run.state of
                    ExploringRun ex chartState ->
                        Just { model | runs = { run | state = ExploringRun ex (f run.output chartState) } :: runs }

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
