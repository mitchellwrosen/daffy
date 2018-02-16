{-# language CPP #-}

import Daffy.Command (Command)
import Daffy.Exception
import Daffy.Stats (Stats)
import Daffy.Supervisor (Supervisor)
import Paths_daffy (getDataDir)

import qualified Daffy.Command as Command
#ifdef INOTIFY
import qualified Daffy.Eventlog as Eventlog
#endif
import qualified Daffy.Profile as Profile
import qualified Daffy.Stats as Stats
import qualified Daffy.Supervisor as Supervisor

import Data.Aeson (Value, (.=))
import Data.List (intersperse)
import Data.Reflection (Given, given, give)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status200, status404, statusCode)
import Network.Wai.Handler.WebSockets (websocketsOr)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)
import System.IO (IOMode(WriteMode))
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text (dropWhile)
import qualified Data.Text.IO as Text
import qualified GHC.RTS.Events as GHC
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSockets
import qualified Streaming.Prelude as Streaming

daffydir :: FilePath
daffydir =
  ".daffy/"

main :: IO ()
main = do
  createDirectoryIfMissing False daffydir
  give V2 main'

main' :: Given V => IO ()
main' = do
  v0 "Running on http://localhost:8080"
  Warp.runSettings settings
    (websocketsOr WebSockets.defaultConnectionOptions wsApp (log httpApp))
 where
  settings :: Warp.Settings
  settings =
    Warp.defaultSettings
      & Warp.setPort 8080
      & Warp.setHost "127.0.0.1"
      & Warp.setOnException
          (\_ ex ->
            when (Warp.defaultShouldDisplayException ex)
              (hPutStrLn stderr (displayException ex)))

-- Log requests at v1 or higher
log :: Given V => Wai.Application -> Wai.Application
log app request respond =
  app request respond'
 where
  respond' :: Wai.Response -> IO Wai.ResponseReceived
  respond' response =
    respond response
      <* v1 (show code ++ " " ++ method ++ " " ++ path)
   where
    code :: Int
    code =
      statusCode (Wai.responseStatus response)

  method :: [Char]
  method =
    Char8.unpack (Wai.requestMethod request)

  path :: [Char]
  path =
    Char8.unpack (Wai.rawPathInfo request)

-- The HTTP application serves a couple static files.
httpApp :: Given V => Wai.Application
httpApp request respond = do
  case Wai.requestMethod request of
    "GET" ->
      case Wai.rawPathInfo request of
        "/" -> do
          respond (htmlFile (data_dir ++ "/static/index.html"))
        "/daffy.js" ->
          respond (jsFile (data_dir ++ "/codegen/daffy.js"))
        path | ByteString.isSuffixOf ".svg" path ->
          respond (svgFile ("." ++ Char8.unpack path))
        _ ->
          respond notFound
    _ ->
      respond notFound

 where
  htmlFile :: FilePath -> Wai.Response
  htmlFile file =
    Wai.responseFile status200 [(hContentType, "text/html")] file Nothing

  jsFile :: FilePath -> Wai.Response
  jsFile file =
    Wai.responseFile status200 [(hContentType, "application/javascript")] file
      Nothing

  svgFile :: FilePath -> Wai.Response
  svgFile file =
    Wai.responseFile status200 [(hContentType, "image/svg+xml")] file Nothing

  notFound :: Wai.Response
  notFound =
    Wai.responseLBS status404 [] ""

-- WebSockets app: receive one command to run, send back its output, exit code,
-- etc, and then loop.
wsApp :: Given V => WebSockets.PendingConnection -> IO ()
wsApp pconn = do
  v1 (show (WebSockets.pendingRequest pconn))

  conn :: WebSockets.Connection <-
    WebSockets.acceptRequest pconn

  fix $ \loop -> do
    command :: Command <-
      recvCommand conn

    when (null (Command.command command))
      (throw (CommandParseException (Aeson.encode command) "empty string"))

    runManaged (runCommand conn command)

    loop

recvCommand :: Given V => WebSockets.Connection -> IO Command
recvCommand conn = do
  v2 "Waiting for command"

  blob :: LByteString <-
    WebSockets.receiveData conn

  case Aeson.eitherDecode blob of
    Left err ->
      throw (CommandParseException blob err)

    Right request ->
      pure request

runCommand :: Given V => WebSockets.Connection -> Command -> Managed ()
runCommand conn command = do
  now :: Int <-
    (round :: Double -> Int) . realToFrac <$> io getPOSIXTime

  -- "/foo/bar/baz --oink" -> "baz"
  let progname :: [Char]
      progname =
        takeFileName (head (words (Command.command command)))

  let workdir :: FilePath
      workdir =
        daffydir ++ progname ++ "/" ++ show now ++ "/"

  io (createDirectoryIfMissing True workdir)

  -- Use a supervisor to keep track of the threads we spawn, so we can be sure
  -- they're all done before sending the exit code (which signals there's no
  -- more data).
  supervisor :: Supervisor <-
    Supervisor.new

  let eventlog :: FilePath
      eventlog =
        progname ++ ".eventlog"

  let proffile :: FilePath
      proffile =
        workdir ++ progname ++ ".prof"

  let statsfile :: FilePath
      statsfile =
        workdir ++ progname ++ ".stats"

  when (Command.eventlog command)
    (v2 ("Writing eventlog to " ++ daffydir ++ "eventlog"))

  when (Command.prof command)
    (v2 ("Writing profile to " ++ proffile))

  when (Command.stats command)
    (v2 ("Writing stats to " ++ statsfile))

  -- If Linux, incrementally parse the eventlog as it's written to by GHC. Turns
  -- out this doesn't exactly stream the eventlog in real time, as it's only
  -- flushed every so often. I believe writing a custom "EventLogWriter"
  -- involves manually recompiling the runtime system. So, blocky-streaming will
  -- have to do, for now.
#ifdef INOTIFY
  when (Command.eventlog command) $ do
    -- Truncate the old eventlog (if any)
    io (withFile eventlog WriteMode (const (pure ())))

    Supervisor.spawn
      supervisor
      (runManaged (Streaming.mapM_ (sendEvent conn) (Eventlog.stream eventlog)))
#endif

  -- Spawn the process.
  let rendered :: [Char]
      rendered =
       Command.render (workdir ++ progname) statsfile command

  v1 ("Running: " ++ rendered)
  process :: Process () Handle Handle <-
    managed
      (withProcess
        (shell rendered
          & setStdout createPipe
          & setStderr createPipe))

  -- Asynchronously stream stdout and stderr.
  Supervisor.spawn supervisor
    (forever (ByteString.hGetLine (getStdout process) >>= sendStdout conn)
      `catchAny` \_ -> pure ())
  Supervisor.spawn supervisor
    (forever (ByteString.hGetLine (getStderr process) >>= sendStderr conn)
      `catchAny` \_ -> pure ())

  -- Wait for the process to terminate.
  code :: ExitCode <-
    waitExitCode process
  v1 (show code)

  -- Wait for background threads forwarding stdout, stderr, and possibly the
  -- eventlog, to finish sending data.
  Supervisor.wait supervisor

  -- We can't have GHC write to a custom eventlog destination, so just crudely
  -- move it after the process is done.
  when (Command.eventlog command)
    (io (void (tryAny (renameFile eventlog (workdir ++ eventlog)))))

  -- If we didn't stream the eventlog, send all of the events out now.
#ifndef INOTIFY
  when (Command.eventlog command) $ do
    io (tryAny (GHC.readEventLogFromFile (workdir ++ eventlog))) >>= \case
      Left _ ->
        v2 (daffydir ++ eventlog ++ " not found")
      Right (Left err) ->
        io (throw (EventlogParseException err (workdir ++ eventlog)))
      Right (Right (GHC.dat -> GHC.Data events)) ->
        forM_ events (sendEvent conn)
#endif

  -- Send a big hunk of stats.
  when (Command.stats command) $
    io (tryAny (Text.readFile statsfile)) >>= \case
      Left _ ->
        v2 (statsfile ++ " not found")
      Right bytes ->
        -- Drop a line, because when directing -S output to a file, the command
        -- invocation is written at the top.
        case Stats.parse (Text.dropWhile (/= '\n') bytes) of
          Left err ->
            io (throw (StatsParseException err statsfile))
          Right stats ->
            sendStats conn stats

  -- Send time and alloc flamegraph paths
  when (Command.prof command) $ do
    io (tryAny (LByteString.readFile proffile)) >>= \case
      Left ex ->
        v2 (proffile ++ " not found: " ++ show ex)
      Right bytes ->
        case Profile.parse bytes of
          Left err ->
            io (throw (ProfileParseException err proffile))
          Right prof -> do
            let ticks_entries :: [Text]
                ticks_entries =
                  Profile.ticksFlamegraph prof

            unless (null ticks_entries) $ do
              let tickssvg :: FilePath
                  tickssvg =
                    progname ++ "-ticks.svg"

              let ticksflamegraph :: [Char]
                  ticksflamegraph =
                    flamegraph ++ " --title '" ++ progname ++ " (ticks)' "
                      ++ "--countname ticks " ++ "--nametype 'Cost center:' "
                      ++ "--colors purple"

              v2
                ("Running: " ++ ticksflamegraph ++ " > " ++ workdir
                  ++ tickssvg)

              io (withFile (workdir ++ tickssvg) WriteMode $ \h ->
                runProcess_
                  (shell ticksflamegraph
                    & setStdin (byteStringInput (linesInput ticks_entries))
                    & setStdout (useHandleClose h)))

              sendFlamegraph conn "ticks-flamegraph" (workdir ++ tickssvg)

            let bytes_entries :: [Text]
                bytes_entries =
                  Profile.allocFlamegraph prof

            unless (null bytes_entries) $ do
              let bytessvg :: FilePath
                  bytessvg =
                    progname ++ "-bytes.svg"

              let bytesflamegraph :: [Char]
                  bytesflamegraph =
                    flamegraph ++ " --title '" ++ progname ++ " (bytes)' "
                      ++ "--countname bytes " ++ "--nametype 'Cost center:' "
                      ++ "--colors purple"

              v2
                ("Running: " ++ bytesflamegraph ++ " > " ++ workdir
                  ++ bytessvg)

              io (withFile (workdir ++ bytessvg) WriteMode $ \h ->
                runProcess_
                  (shell bytesflamegraph
                    & setStdin (byteStringInput (linesInput bytes_entries))
                    & setStdout (useHandleClose h)))

              sendFlamegraph conn "bytes-flamegraph" (workdir ++ bytessvg)

  sendExitCode conn code

linesInput :: [Text] -> LByteString
linesInput =
  LByteString.fromChunks . intersperse (Char8.singleton '\n') . map encodeUtf8

--------------------------------------------------------------------------------
-- Send blobby blobs

sendText :: (Given V, MonadIO m) => WebSockets.Connection -> LByteString -> m ()
sendText conn msg = do
  v3 ("SEND " ++ unpack (decodeUtf8 (LByteString.toStrict msg)))
  io (WebSockets.sendTextData conn msg)

sendStdout :: Given V => WebSockets.Connection -> ByteString -> IO ()
sendStdout conn line =
  sendText conn (Aeson.encode blob)
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= ("stdout" :: Text)
      , "payload" .= decodeUtf8 line
      ]

sendStderr :: Given V => WebSockets.Connection -> ByteString -> IO ()
sendStderr conn line =
  sendText conn (Aeson.encode blob)
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= ("stderr" :: Text)
      , "payload" .= decodeUtf8 line
      ]

sendEvent :: (Given V, MonadIO m) => WebSockets.Connection -> GHC.Event -> m ()
sendEvent conn event =
  sendText conn (Aeson.encode blob)
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= ("event" :: Text)
      , "payload" .= show event
      ]

sendStats :: (Given V, MonadIO m) => WebSockets.Connection -> Stats -> m ()
sendStats conn stats =
  sendText conn (Aeson.encode blob)
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= ("stats" :: Text)
      , "payload" .= stats
      ]

sendFlamegraph
  :: (Given V, MonadIO m) => WebSockets.Connection -> Text -> FilePath -> m ()
sendFlamegraph conn name path = do
  sendText conn (Aeson.encode blob)
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= name
      , "payload" .= path
      ]

sendExitCode
  :: (Given V, MonadIO m) => WebSockets.Connection -> ExitCode -> m ()
sendExitCode conn code =
  sendText conn (Aeson.encode blob)
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= ("exitcode" :: Text)
      , "payload" .=
          case code of
            ExitSuccess ->
              0
            ExitFailure n ->
              n
      ]

--------------------------------------------------------------------------------
-- Data files

data_dir :: FilePath
data_dir =
  unsafePerformIO $
    lookupEnv "DAFFY_DEV" >>= \case
      Nothing ->
        getDataDir
      Just _ ->
        pure "./daffy-server"
{-# NOINLINE data_dir #-}

flamegraph :: FilePath
flamegraph =
  "perl " ++ data_dir ++ "/submodules/FlameGraph/flamegraph.pl"

--------------------------------------------------------------------------------
-- Verbosity

-- V0: Messages to show no matter what
-- V1: Log some noteworthy actions, such as clients connecting
-- V2: Very verbosely log almost every little action the program is doing
-- V3: Also log all communication with the client
data V
  = V0
  | V1
  | V2
  | V3
  deriving (Eq, Ord)

v0, v1, v2, v3 :: (Given V, MonadIO m) => [Char] -> m ()
v0 = vv V0
v1 = vv V1
v2 = vv V2
v3 = vv V3

vv :: (Given V, MonadIO m) => V -> [Char] -> m ()
vv v =
  if given >= v
    then putStrLn
    else io . mempty
