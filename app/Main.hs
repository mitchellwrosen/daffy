module Main where

import Data.Aeson (FromJSON, (.=))
import System.Process.Typed

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Network.WebSockets as WebSockets

data Command = Command
  { command :: String
  } deriving (Generic)

instance FromJSON Command

main :: IO ()
main =
  WebSockets.runServer "localhost" 8080 app

app :: WebSockets.PendingConnection -> IO ()
app pconn = do
  conn :: WebSockets.Connection <-
    WebSockets.acceptRequest pconn

  Command cmd <- do
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

  let config :: ProcessConfig () Handle Handle
      config =
        shell cmd
          & setStdout createPipe
          & setStderr createPipe

  withProcess config $ \p -> do
    var1 :: MVar () <-
      newEmptyMVar
    var2 :: MVar () <-
      newEmptyMVar

    let worker :: (ByteString -> IO ()) -> Handle -> IO ()
        worker f handle =
          forever (ByteString.hGetLine handle >>= f)

    void . forkIO $ do
      let action :: ByteString -> IO ()
          action bytes =
            WebSockets.sendTextData
              conn
              (Aeson.encode
                (Aeson.object
                  [ "type" .= ("stdout" :: Text)
                  , "payload" .= decodeUtf8 bytes
                  ]))
      worker action (getStdout p)
        `catchAny` \_ -> putMVar var1 ()

    void . forkIO $ do
      let action :: ByteString -> IO ()
          action bytes =
            WebSockets.sendTextData
              conn
              (Aeson.encode
                (Aeson.object
                  [ "type" .= ("stderr" :: Text)
                  , "payload" .= decodeUtf8 bytes
                  ]))
      worker action (getStderr p)
        `catchAny` \_ -> putMVar var2 ()

    code :: ExitCode <-
      waitExitCode p

    takeMVar var1
    takeMVar var2

    WebSockets.sendTextData
      conn
      (Aeson.encode
        (Aeson.object
          [ "type" .= ("exitcode" :: Text)
          , "payload" .=
              case code of
                ExitSuccess -> 0
                ExitFailure n -> n
          ]))
