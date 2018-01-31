import Daffy.Command
import Daffy.Stats (Stats)

import qualified Daffy.Stats as Stats

import Data.Aeson (Value, (.=))
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Process.Typed

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text (dropWhile)
import qualified Data.Text.IO as Text
import qualified Network.WebSockets as WebSockets

main :: IO ()
main =
  WebSockets.runServer "localhost" 8080 app

app :: WebSockets.PendingConnection -> IO ()
app pconn = do
  conn :: WebSockets.Connection <-
    WebSockets.acceptRequest pconn

  cmd :: Command <- do
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

  runManaged $ do
    tempdir :: FilePath <-
      managed (withSystemTempDirectory "daffy")

    let config :: ProcessConfig () Handle Handle
        config =
          shell (renderCommand cmd)
            & setStdout createPipe
            & setStderr createPipe
            & setWorkingDir tempdir

    process :: Process () Handle Handle <-
      managed (withProcess config)

    var1 :: MVar () <-
      io newEmptyMVar
    var2 :: MVar () <-
      io newEmptyMVar

    let worker
          :: (WebSockets.Connection -> ByteString -> IO ()) -> Handle -> IO ()
        worker send handle =
          forever (ByteString.hGetLine handle >>= send conn)

    void . io . forkIO $ do
      worker sendStdout (getStdout process)
        `catchAny` \_ -> putMVar var1 ()

    void . io . forkIO $ do
      worker sendStderr (getStderr process)
        `catchAny` \_ -> putMVar var2 ()

    code :: ExitCode <-
      waitExitCode process

    io (takeMVar var1)
    io (takeMVar var2)

    sendExitCode conn code

    io (tryAny (Text.readFile (tempdir </> statsFile))) >>= \case
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

statsFile :: FilePath
statsFile =
  "stats"

renderCommand :: Command -> String
renderCommand Command{command, stats} =
  if stats
    then command ++ " +RTS -S" ++ statsFile
    else command

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
  io (WebSockets.sendTextData conn (Aeson.encode stats))
