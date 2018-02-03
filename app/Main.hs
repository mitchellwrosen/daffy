import Daffy.Command (Command)
import Daffy.Stats (Stats)
import Daffy.Supervisor (Supervisor)

import qualified Daffy.Command as Command
import qualified Daffy.Eventlog as Eventlog
import qualified Daffy.Stats as Stats
import qualified Daffy.Supervisor as Supervisor

import Data.Aeson (Value, (.=))
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
import qualified Network.WebSockets as WebSockets
import qualified Streaming.Prelude as Streaming

main :: IO ()
main =
  WebSockets.runServer "localhost" 8080 app

app :: WebSockets.PendingConnection -> IO ()
app pconn = do
  conn :: WebSockets.Connection <-
    WebSockets.acceptRequest pconn

  command :: Command <- do
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

  runManaged (runCommand conn command)

runCommand :: WebSockets.Connection -> Command -> Managed ()
runCommand conn command = do
  supervisor :: Supervisor <-
    Supervisor.new

  when (Command.eventlog command) $ do
    -- The location that (we assume) GHC will write the eventlog (if any).
    eventlog :: FilePath <-
      case words (Command.command command) of
        [] -> do
          io (hPutStrLn stderr "Empty command")
          io exitFailure
        prog : _ ->
          pure (takeFileName prog ++ ".eventlog")

    -- Truncate the old eventlog (if any)
    io (withFile eventlog WriteMode (const (pure ())))

    Supervisor.spawn
      supervisor
      (runManaged (Streaming.mapM_ (sendEvent conn) (Eventlog.parse eventlog)))

  tempfile :: FilePath <-
    io (emptySystemTempFile "daffy")
  managed_ (flip finally (void (tryAny (removeFile tempfile))))

  let config :: ProcessConfig () Handle Handle
      config =
        shell (Command.render tempfile command)
          & setStdout createPipe
          & setStderr createPipe

  process :: Process () Handle Handle <-
    managed (withProcess config)

  let worker
        :: (WebSockets.Connection -> ByteString -> IO ()) -> Handle -> IO ()
      worker send handle =
        forever (ByteString.hGetLine handle >>= send conn)

  Supervisor.spawn supervisor
    (worker sendStdout (getStdout process) `catchAny` \_ -> pure ())

  Supervisor.spawn supervisor
    (worker sendStderr (getStderr process) `catchAny` \_ -> pure ())

  code :: ExitCode <-
    waitExitCode process

  Supervisor.wait supervisor

  sendExitCode conn code

  when (Command.stats command) $
    io (tryAny (Text.readFile tempfile)) >>= \case
      Left _ ->
        pure ()
      Right bytes ->
        case Stats.parse (Text.dropWhile (/= '\n') bytes) of
          Left err -> do
            io
              (hPutStrLn stderr ("Could not parse runtime statistics: " ++ err))
            io exitFailure
          Right stats ->
            sendStats conn stats

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
