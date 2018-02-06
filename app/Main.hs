{-# language CPP #-}

import Daffy.Command (Command)
import Daffy.Exception
import Daffy.Stats (Stats)
import Daffy.Supervisor (Supervisor)

import qualified Daffy.Command as Command
#ifdef INOTIFY
import qualified Daffy.Eventlog as Eventlog
#endif
import qualified Daffy.Stats as Stats
import qualified Daffy.Supervisor as Supervisor

import Data.Aeson (Value, (.=))
import Data.Reflection (Given, given, give)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status200, status404, statusCode)
import Network.Wai.Handler.WebSockets (websocketsOr)
import System.FilePath (takeFileName)
import System.IO (IOMode(WriteMode))
import System.Process.Typed

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text (dropWhile)
import qualified Data.Text.IO as Text
import qualified GHC.RTS.Events as GHC
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSockets
import qualified Streaming.Prelude as Streaming

main :: IO ()
main =
  give V1 main'

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

-- Logging middleware:
--
--   200 GET /
--
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
        "/" ->
          respond (htmlFile "static/index.html")
        "/daffy.js" ->
          respond (jsFile "static/daffy.js")
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
    Wai.responseFile status200 [(hContentType, "application/json")] file Nothing

  notFound :: Wai.Response
  notFound =
    Wai.responseLBS status404 [] ""

-- WebSockets app: receive one command to run, send back its output, exit code,
-- etc, and then tear down the connection.
wsApp :: Given V => WebSockets.PendingConnection -> IO ()
wsApp pconn = do
  conn :: WebSockets.Connection <-
    WebSockets.acceptRequest pconn

  command :: Command <-
    recvCommand conn

  when (null (Command.command command))
    (throw (CommandParseException (Aeson.encode command) "empty string"))

  runManaged (runCommand conn command)

  -- TODO: Graceful teardown

recvCommand :: Given V => WebSockets.Connection -> IO Command
recvCommand conn = do
  blob :: LByteString <-
    WebSockets.receiveData conn

  case Aeson.eitherDecode blob of
    Left err ->
      throw (CommandParseException blob err)

    Right request ->
      pure request

runCommand :: Given V => WebSockets.Connection -> Command -> Managed ()
runCommand conn command = do
  -- Use a supervisor to keep track of the threads we spawn, so we can be sure
  -- they're all done before sending the exit code (which signals there's no
  -- more data).
  supervisor :: Supervisor <-
    Supervisor.new

  -- "/foo/bar/baz --oink" -> "baz"
  let progname :: [Char]
      progname =
        takeFileName (head (words (Command.command command)))

  let eventlog :: FilePath
      eventlog =
        progname ++ ".eventlog"

  -- If Linux, incrementally parse the eventlog as it's written to by GHC. Turns
  -- out this doesn't exactly stream the eventlog in real time, as it's only
  -- flushed every so often. I believe writing a custom "EventLogWriter"
  -- involves manually recompiling the runtime system. So, blocky-streaming will
  -- have to do, for now.
#ifdef INOTIFY
  when (Command.eventlog command) $ do
    v1 ("Writing eventlog to " ++ eventlog)

    -- Truncate the old eventlog (if any)
    io (withFile eventlog WriteMode (const (pure ())))

    Supervisor.spawn
      supervisor
      (runManaged (Streaming.mapM_ (sendEvent conn) (Eventlog.parse eventlog)))
#endif

  let statsfile :: FilePath
      statsfile =
        progname ++ ".stats"

  when (Command.stats command)
    (v1 ("Writing stats to " ++ statsfile))

  -- Spawn the process.
  v1 ("Running: " ++ Command.render statsfile command)
  process :: Process () Handle Handle <-
    managed
      (withProcess
        (shell (Command.render statsfile command)
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

  -- If we didn't stream the eventlog, send all of the events out now.
#ifndef INOTIFY
  when (Command.eventlog command) $ do
    io (tryAny (GHC.readEventLogFromFile eventlog)) >>= \case
      Left _ ->
        pure ()
      Right (Left err) ->
        throw (EventlogParseException err)
      Right (Right (GHC.dat -> GHC.Data events)) ->
        forM_ events (sendEvent conn)
#endif

  -- Send a big hunk of stats.
  when (Command.stats command) $
    io (tryAny (Text.readFile statsfile)) >>= \case
      Left _ ->
        pure ()
      Right bytes ->
        -- Drop a line, because when directing -S output to a file, the command
        -- invocation is written at the top.
        case Stats.parse (Text.dropWhile (/= '\n') bytes) of
          Left err ->
            io (throw (StatsParseException bytes err))
          Right stats ->
            sendStats conn stats

  -- Send time and alloc flamegraphs
  when (Command.prof command) $ do
    let prof :: FilePath
        prof =
          progname ++ ".prof"

    let tickssvg :: FilePath
        tickssvg =
          progname ++ "-ticks.svg"

    v1 ("Writing ticks flamegraph to " ++ tickssvg)
    runProcess_
      (shell ("ghc-prof-flamegraph --ticks " ++ prof ++ " -o " ++ tickssvg)
        & setStdout closed)

    let bytessvg :: FilePath
        bytessvg =
          progname ++ "-bytes.svg"

    v1 ("Writing bytes flamegraph to " ++ bytessvg)
    runProcess_
      (shell ("ghc-prof-flamegraph --bytes " ++ prof ++ " -o " ++ bytessvg)
        & setStdout closed)

  sendExitCode conn code

--------------------------------------------------------------------------------
-- Send blobby blobs

sendStdout :: MonadIO m => WebSockets.Connection -> ByteString -> m ()
sendStdout conn line =
  io (WebSockets.sendTextData conn (Aeson.encode blob))
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= ("stdout" :: Text)
      , "payload" .= decodeUtf8 line
      ]

sendStderr :: WebSockets.Connection -> ByteString -> IO ()
sendStderr conn line =
  WebSockets.sendTextData conn (Aeson.encode blob)
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= ("stderr" :: Text)
      , "payload" .= decodeUtf8 line
      ]

sendEvent :: MonadIO m => WebSockets.Connection -> GHC.Event -> m ()
sendEvent conn event =
  io (WebSockets.sendTextData conn (Aeson.encode blob))
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= ("event" :: Text)
      , "payload" .= show event
      ]

sendExitCode :: MonadIO m => WebSockets.Connection -> ExitCode -> m ()
sendExitCode conn code =
  io (WebSockets.sendTextData conn (Aeson.encode blob))
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

sendStats :: MonadIO m => WebSockets.Connection -> Stats -> m ()
sendStats conn stats =
  io (WebSockets.sendTextData conn (Aeson.encode blob))
 where
  blob :: Value
  blob =
    Aeson.object
      [ "type" .= ("stats" :: Text)
      , "payload" .= stats
      ]

--------------------------------------------------------------------------------
-- Verbosity

data V
  = V0
  | V1
  deriving (Eq, Ord)

v0 :: MonadIO m => [Char] -> m ()
v0 =
  io . putStrLn

v1 :: (Given V, MonadIO m) => [Char] -> m ()
v1 =
  if given >= V1
    then io . putStrLn
    else io . mempty
