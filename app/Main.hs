{-# language CPP #-}

import Daffy.Command (Command)
import Daffy.Stats (Stats)
import Daffy.Supervisor (Supervisor)

import qualified Daffy.Command as Command
#ifdef INOTIFY
import qualified Daffy.Eventlog as Eventlog
#endif
import qualified Daffy.Stats as Stats
import qualified Daffy.Supervisor as Supervisor

import Data.Aeson (Value, (.=))
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai.Handler.WebSockets (websocketsOr)
import System.Directory (removeFile)
import System.FilePath (takeFileName)
import System.IO (IOMode(WriteMode))
import System.IO.Temp (emptySystemTempFile)
import System.Process.Typed

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text (dropWhile)
import qualified Data.Text.IO as Text
import qualified GHC.RTS.Events as GHC
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSockets
import qualified Streaming.Prelude as Streaming

main :: IO ()
main =
  Warp.run 8080 app

app :: Wai.Application
app =
  websocketsOr WebSockets.defaultConnectionOptions wsApp httpApp

httpApp :: Wai.Application
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
  htmlFile path =
    Wai.responseFile status200 [(hContentType, "text/html")] path Nothing

  jsFile :: FilePath -> Wai.Response
  jsFile path =
    Wai.responseFile status200 [(hContentType, "application/json")] path Nothing

  notFound :: Wai.Response
  notFound =
    Wai.responseLBS status404 [] ""

wsApp :: WebSockets.PendingConnection -> IO ()
wsApp pconn = do
  conn :: WebSockets.Connection <-
    WebSockets.acceptRequest pconn

  command :: Command <-
    recvCommand conn

  runManaged (runCommand conn command)

recvCommand :: WebSockets.Connection -> IO Command
recvCommand conn = do
  blob :: LByteString <-
    WebSockets.receiveData conn

  case Aeson.decode blob of
    Nothing -> do
      hPutStrLn stderr
        ("Could not decode: "
          ++ unpack (decodeUtf8 (LByteString.toStrict blob)))
      exitFailure

    Just request ->
      pure request

runCommand :: WebSockets.Connection -> Command -> Managed ()
runCommand conn command = do
  -- Use a supervisor to keep track of the threads we spawn, so we can be sure
  -- they're all done before sending the exit code (which signals there's no
  -- more data).
  supervisor :: Supervisor <-
    Supervisor.new

  -- "/foo/bar/baz --oink" -> "baz"
  let progname :: String
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
    -- Truncate the old eventlog (if any)
    io (withFile eventlog WriteMode (const (pure ())))

    Supervisor.spawn
      supervisor
      (runManaged (Streaming.mapM_ (sendEvent conn) (Eventlog.parse eventlog)))
#endif

  -- Temporary file to write runtime stats to, if requested.
  tempfile :: FilePath <-
    io (emptySystemTempFile "daffy")
  managed_ (flip finally (void (tryAny (removeFile tempfile))))

  -- Spawn the process.
  process :: Process () Handle Handle <-
    managed
      (withProcess
        (shell (Command.render tempfile command)
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

  -- Wait for background threads forwarding stdout, stderr, and possibly the
  -- eventlog, to finish sending data.
  Supervisor.wait supervisor

  -- If we didn't stream the eventlog, send all of the events out now.
#ifndef INOTIFY
  when (Command.eventlog command) $ do
    io (tryAny (GHC.readEventLogFromFile eventlog)) >>= \case
      Left _ ->
        pure ()
      Right (Left err) -> do
        io (hPutStrLn stderr ("Could not parse eventlog: " ++ err))
        io exitFailure
      Right (Right (GHC.dat -> GHC.Data events)) ->
        forM_ events (sendEvent conn)
#endif

  -- Send a big hunk of stats.
  when (Command.stats command) $
    io (tryAny (Text.readFile tempfile)) >>= \case
      Left _ ->
        pure ()
      Right bytes ->
        -- Drop a line, because when directing -S output to a file, the command
        -- invocation is written at the top.
        case Stats.parse (Text.dropWhile (/= '\n') bytes) of
          Left err -> io $ do
            hPutStrLn stderr ("Could not parse runtime statistics: " ++ err)
            exitFailure
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

    let bytessvg :: FilePath
        bytessvg =
          progname ++ "-bytes.svg"

    runProcess_
      (shell ("ghc-prof-flamegraph --ticks " ++ prof ++ " -o " ++ tickssvg)
        & setStdout closed)
    runProcess_
      (shell ("ghc-prof-flamegraph --bytes " ++ prof ++ " -o " ++ bytessvg)
        & setStdout closed)

  sendExitCode conn code

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
